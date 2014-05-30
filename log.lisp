;; -*- lisp -*-

(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)

(define-condition missing-logger (error)
  ((name :accessor name :initarg :name :initform nil))
  (:report (lambda (c s)
             (format s "~&Error: no logger defined named :~S" (name c)))))

(define-condition log-argument-evaluation-error (error)
  ((form :accessor form :initarg :form :initform nil)
   (inner-error :accessor inner-error :initarg :inner-error :initform nil))
  (:report (lambda (c s)
             (format s "~&Error evaluting log argument: ~s, ~A" (form c) (inner-error c)))))

(defun missing-logger (name)
  (error (make-condition 'missing-logger :name name)))

(defun get-logger-var-name (name)
  (typecase name
    (null nil)
    (logger (get-logger-var-name (name name)))
    (symbol (symbol-munger:reintern #?"*${name}*" (symbol-package name)))))

(defun ensure-type (val type)
  (when (typep val type) val))

(defun get-logger (name)
  (typecase name
    (null nil)
    (keyword nil) ;; loggers have to have non-keyword names
    (symbol
     (ensure-type
      (or (handler-case (symbol-value (get-logger-var-name name))
            (unbound-variable (c) (declare (ignore c))))
          (destructuring-bind (name &optional level)
              (split-log-helper name)
            (when level (get-logger name))))
      'logger))
    (logger name)))

(defun require-logger (name)
  (or (get-logger name)
      (missing-logger name)))

(defun (setf get-logger) (new name)
  (setf (symbol-value (get-logger-var-name name))
        new))

(defun rem-logger (name &aux (var (get-logger-var-name name)))
  (makunbound var)
  ;; potential race condition
  (setf *logger-vars* (remove var *logger-vars*)))


;;;; ** Loggers

(define-condition generating-message ()
  ((message :accessor message :initarg :message :initform nil)))

(defmacro with-change-message ((message-place) &body body)
  `(progn
    (restart-bind
        ((change-message
           (lambda (new) (setf ,message-place new))
           :report-function (lambda (stream)
                              (write-string "Change the message to be created" stream))
           ))
      ,@body)
    ,message-place))

(defun logger-signal-handlers (logger condition)
  (loop for (typename fn) in (default-signal-bindings logger)
        when (typep condition typename)
        do (with-change-message ((message condition))
             (funcall fn condition)))
  (loop for p in (parents logger)
        do (logger-signal-handlers p condition)))


(defun maybe-signal-message (c message &aux (*message* message)
                                       (logger (logger message)))
  (when (signal-messages logger)
    ;; we change the one on the condition that is passing through all the handlers
    (with-change-message ((message c)) (signal c))
    (logger-signal-handlers logger c)
    ;; make sure that all changed messages are visible
    (setf message (message c)))
  message)

(defun maybe-signal-generating-message (message)
  (maybe-signal-message
   (make-condition 'generating-message :message message)
   message))

(define-condition logging-message ()
  ((message :accessor message :initarg :message :initform nil)
   (logger :accessor logger :initarg :logger :initform nil)))

(defun maybe-signal-logging-message (logger message)
  (maybe-signal-message
   (make-condition 'logging-message :message message :logger logger)
   message))

(define-condition appending-message ()
  ((message :accessor message :initarg :message :initform nil)
   (logger :accessor logger :initarg :logger :initform nil)
   (appender :accessor appender :initarg :appender :initform nil)))

(defun maybe-signal-appending-message (logger appender message)
  (maybe-signal-message
   (make-condition
    'appending-message
    :logger logger :message message :appender appender)
   message))

(defclass message ()
  ((name :accessor name :initarg :name :initform nil)
   (logger :accessor logger :initarg :logger :initform nil)
   (level :accessor level :initarg :level :initform nil)
   (format-control :accessor format-control :initarg :format-control :initform nil)
   (format-args :accessor format-args :initarg :format-args :initform nil)
   (data-plist :accessor data-plist :initarg :data-plist :initform nil)
   (arg-literals :accessor arg-literals :initarg :arg-literals :initform nil)
   (timestamp :accessor timestamp :initarg :timestamp
              :initform (local-time:now))))

(defun copy-messsage (m &rest plist)
  (let ((new (apply #'make-instance (class-of m)
                    (iter (for s in (closer-mop:class-slots (class-of m)))
                      (for sn = (closer-mop:slot-definition-name s))
                      (when (slot-boundp m sn)
                        (collect (symbol-munger:lisp->keyword sn))
                        (collect (slot-value m sn)))))))
    (push-m-plist plist new)
    new))

(defclass logger ()
  ((name :initarg :name :accessor name :initform nil)
   (signal-messages
    :accessor signal-messages :initarg :signal-messages
    :initform t)
   (default-signal-bindings
    :accessor default-signal-bindings
    :initarg :default-signal-bindings :initform nil)
   (parents
    :initform '()     :accessor parents :initarg :parents
    :documentation "The logger this logger inherits from.")
   (children
    :initform '()     :accessor children  :initarg :children
    :documentation "The logger which inherit from this logger.")
   (appenders
    :initform '()     :accessor appenders :initarg :appenders
    :documentation "A list of appender objects this logger sholud send messages to.")
   (level
    :initform nil :initarg :level :accessor level
    :type (or null integer)
    :documentation "This loggers level.")
   (compile-time-level
    :initform nil :initarg :compile-time-level :accessor compile-time-level
    :type (or null integer)
    :documentation "This loggers's compile time level. Any log expression below this level will macro-expand to NIL.")))

(defun split-log-helper (sym)
  (destructuring-bind (name &optional level)
      ;; a dot followed by lookahead: no more dots till the end of the string
      (cl-ppcre:split #?r"\.(?=[^\.]+$)" (string sym))
    (let ((level (log-level-name-of level)))
      (if level
          (list (symbol-munger:reintern name (or (symbol-package sym) *package*))
                level)
          (list sym)))))

(defun ensure-level-value (level)
  (typecase level
    (null 1)
    (integer level)
    (symbol
     (third (log-level-name-of level :raw? t )))
    (t (ensure-level-value (log-level level)))))

(defun log-level-name-of (level &key raw?)
  (etypecase level
    (null nil)
    ((or logger appender message)
     (log-level-name-of (log-level level)))
    ((or symbol string)
     (let* ((ln (string level))
            (proper-names
              (find ln *log-level-names*
                    :test (lambda (x y)
                            (or (string-equal (string x) (string (first y)))
                                (string-equal (string x) (string (second y))))))))
       (if raw?
           proper-names
           (first proper-names))))
    (integer
     (when (not (< -1 level (length *log-level-names*)))
       (error "~S is an invalid log level" level))
     (car (aref *log-level-names* level)))))

(defmethod print-object ((log logger) stream)
  (print-unreadable-object (log stream :type t :identity t)
    (format stream "~S ~a"
	    (if (slot-boundp log 'name)
		(name log)
		"#<NO NAME>")
	    (level log))))

(defmethod initialize-instance :after ((log logger) &key &allow-other-keys)
  ;; TODO: Do we want this error here, or should we allow names such that defining is easier
  (setf (parents log) (mapcar #'require-logger (ensure-list (parents log))))

  (ensure-list! (appenders log))
  ;; everybody with no parents who are not the root loger is automatically a
  ;; child of the root logger
  (unless (or (eql 'root-logger (name log))
              (parents log))
    (pushnew *root-logger* (parents log)))

  (dolist (anc (parents log))
    (pushnew log (children anc) :key #'name))

  ;; this should mostly apply to the *root-logger* now
  (unless (or (appenders log) (parents log))
    (ensure-debug-io-appender log)))

(defun make-message (logger level args
                     &key arg-literals data-plist
                     &aux
                     (singleton (only-one? args))
                     format-control?)
  (typecase singleton
    ((or message function) singleton)
    (t
     (ensure-list! args)
     (setf format-control? (first args))
     (maybe-signal-generating-message
      (make-instance
       'message
       :logger logger
       :level level
       :format-control (and (stringp format-control?) format-control?)
       :format-args (when (stringp format-control?)
                      (rest args))
       :data-plist  (append
                     (unless (stringp format-control?)
                       args)
                     data-plist)
       :arg-literals arg-literals)))))

(defun %safe-log-helper-arg (form)
  ;; handle arguments such that evaluating a log message
  ;; shouldnt result in an error at runtime (even if
  ;; that message was disabled at dev time)
  (alexandria:with-unique-names (err)
    `(let ( ,err )
      (restart-case
          (handler-case ,form
            (error (c)
              (setf ,err
                    (make-instance 'log-argument-evaluation-error
                                   :inner-error c
                                   :form ',form))
              (when *debugger-hook* (invoke-debugger c))
              ,err))
        (continue () ,err)))))

(defun %make-log-helper (logger message-level-name)
  "Creates macros like logger.debug to facilitate logging"  
  (with-macro-splicing
      (($logger-var (get-logger-var-name logger))
       ($message-level-name message-level-name)
       ($logger-macro-name
        (symbol-munger:reintern
         #?"${logger}.${ (without-earmuffs message-level-name) }"
         (symbol-package logger))))
    (defmacro $logger-macro-name (&rest @message-args)
      (with-debugging-or-error-printing ($logger-var)
        (when (compile-time-enabled-p $message-level-name $logger-var)
          (let ((@safe-message-args
                  (iter (for a in @message-args)
                    (collect (%safe-log-helper-arg a)))))
            (with-macro-splicing (@message-args @safe-message-args)
              ;; prevents evaluating message-args if we are not enabled
              (with-debugging-or-error-printing ($logger-var)
                (when (enabled-p $message-level-name $logger-var)
                  (do-logging $logger-var 
                    (make-message
                     $logger-var $message-level-name
                     (list @safe-message-args)
                     :arg-literals '(@message-args))))))))))))

(defmacro define-log-helpers (logger-name)
  `(progn
    ,@(iter (for (level-name level-value-name) in-vector *log-level-names*)
        (collect (%make-log-helper logger-name level-value-name)))))


(defmacro define-logger
    (name parents &key compile-time-level level appenders documentation force?
          &aux
          (var-type (if force? 'defparameter 'defvar))
          (var (get-logger-var-name name)))
  (declare (ignore documentation) (type symbol name))
  `(progn
    (,var-type ,var
     (make-instance 'logger
      :name ',name
      :level ,(or level (and (not parents) +debug+))
      :compile-time-level ,compile-time-level
      :appenders ,appenders
      :parents (copy-list ',parents)))
    (pushnew ',var *logger-vars*)
    (define-log-helpers ,name)
    ,(get-logger-var-name name)))

;;; Runtime levels
(defun enabled-p (object check-against)
  (>= (ensure-level-value object)
      (ensure-level-value check-against)))

(defgeneric log-level (log)
  (:method ((log logger))
    (or (level log)
        (if (parents log)
            (loop for parent in (parents log)
                  minimize (log-level parent))
            (error "Can't determine level for ~S" log))))
  (:method (it &aux (log (get-logger it)))
    (if log
        (log-level log)
        (level it))))

(defgeneric (setf log-level) (new log &optional recursive)
  (:documentation
   "Change the logger's level of to NEW-LEVEL. If RECUSIVE is T the
  setting is also applied to the sub logger of logger.")
  (:method (new-level log &optional (recursive t))
    ;; could be an appender, logger or logger-name
    (require-logger! log)
    (ensure-level-value! new-level)
    (setf (slot-value log 'level) new-level)
    ;; could be an appender
    (when recursive
      (dolist (child (children log))
        (setf (log-level child) new-level)))
    new-level))

(defun %logger-name-for-output (log &key (width *max-logger-name-length*))
  "Output the logger name such that it takes exactly :width characters
   and displays the right most :width characters if name is too long

   this simplifies our formatting"
  (let* ((name (string
                (etypecase log
                  ((or symbol string) log)
                  (logger (name log))
                  (message (name (logger log))))))
         (len (length name))
         (out (make-string width :initial-element #\space)))
    (replace out name
             :start1 (max 0 (- width len))
             :start2 (max 0 (- len width)))))

;;; Compile time levels
(defgeneric compile-time-enabled-p (message-level logger)
  (:method (message-level logger)
    (require-logger! logger)
    (>= (ensure-level-value message-level)
        (or (ensure-level-value (log.compile-time-level logger))
            +dribble+))))

(defgeneric log.compile-time-level (log)
  (:method ( log )
    (require-logger! log)
    (or (compile-time-level log)
        (loop for parent in (parents log)
              minimize (log.compile-time-level parent))
        +dribble+)))

(defgeneric (setf log.compile-time-level) (new log &optional recursive)  
  (:documentation
   "Change the compile time log level of logger to NEW-LEVEL. If RECUSIVE is T the
  setting is also applied to the sub loggers.")
  (:method (new-level log &optional (recursive t))
    (get-logger! log)
    (setf (slot-value log 'compile-time-level) new-level)
    (when recursive
      (dolist (child (children log))
        (setf (log.compile-time-level child) new-level)))
    new-level))



;;;; ** Handling Messages

(defun do-log (log level &rest args)
  (require-logger! log)
  (do-logging log (make-message log level args)))

(defgeneric do-append (logger appender message)
  (:method :around (log appender message)
    (declare (ignore message))
    (let ((*appender* appender))
      (with-debugging-or-error-printing
          (log :continue "Try next appender")
        (call-next-method))))
  (:method (log appender message)    
    (when (enabled-p message appender)
      (append-message
       appender
       ;; this will possibly change the message
       (maybe-signal-appending-message
        log appender message)))))

(defgeneric do-logging (logger message)
  (:documentation
   "Applys a message to the loggers appenders
   Message is either a string or a list. When it's a list and the first
    element is a string then it's processed as args to
    cl:format."  )
  (:method :around ( logger message)
    (declare (ignore logger message))
    ;; turn off line wrapping for the entire time while inside the loggers
    (let ((*logger* logger))
      (with-logging-io () (call-next-method))))
  (:method ( log (message message))
    (require-logger! log)
    ;; this is probably a duplicate check, because our helper macros check
    ;; before evaluating the message args, but good to be sure, so that
    ;; extensions and calls to do-logging behave as expected
    (unless (enabled-p message log) (return-from do-logging))
    ;; if we have any appenders send them the message
    (adwutils:spy-break :dologging log message )
    (setf message (maybe-signal-logging-message log message))
    (dolist (appender (appenders log))
      (do-append log appender message))
    (dolist (parent (parents log))
      (with-debugging-or-error-printing
          (log :continue "Try next appender")
        (do-logging parent message)))
    (values message log)))

(defun logger-name-from-helper (name)
  (first (split-log-helper name)))

(defun logger-level-from-helper (name)
  (second (split-log-helper name)))



