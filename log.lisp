;; -*- lisp -*-

(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +dribble+ 0)
  (defparameter +debug+   1)
  (defparameter +info+    2)
  (defparameter +warn+    3)
  (defparameter +error+   4)
  (defparameter +fatal+   5)
  (defparameter *max-logger-name-length* 12)

  (defparameter *log-level-names*
    #((dribble +dribble+ 0)
      (debug +debug+ 1)
      (info +info+ 2)
      (warn +warn+ 3)
      (error +error+ 4)
      (fatal +fatal+ 5))))

(define-condition missing-logger (error)
  ((name :accessor name :initarg :name :initform nil))
  (:report (lambda (c s)
             (format s "~&Error: no logger defined named :~S" (name c)))))

(defun missing-logger (name)
  (error (make-condition 'missing-logger :name name)))

(defun get-logger-var-name (name)
  (typecase name
    (null nil)
    (logger (get-logger-var-name (name name)))
    (symbol (symbol-munger:reintern #?"*${name}*" (symbol-package name)))))

(defun get-logger (name)
  (typecase name
    (null nil)
    (symbol
     (or (handler-case (symbol-value (get-logger-var-name name))
           (unbound-variable (c) (declare (ignore c))))
         (destructuring-bind (name &optional level)
             (split-log-helper name)
           (when level (get-logger name)))))
    (logger name)))

(defun require-logger (name)
  (or (get-logger name)
      (missing-logger name)))

(defun (setf get-logger) (new name)
  (setf (symbol-value (get-logger-var-name name))
        new))

(defun rem-logger (name)
  (setf (symbol-value (get-logger-var-name name)) nil))


;;;; ** Loggers

(define-condition generating-message ()
  ((message :accessor message :initarg :message :initform nil)))

(defun maybe-signal-generating-message (message)
  (when (signal-messages (logger message))
    (signal (make-condition 'generating-message :message message)))
  message)

(define-condition appending-message ()
  ((message :accessor message :initarg :message :initform nil)
   (logger :accessor logger :initarg :logger :initform nil)
   (appender :accessor appender :initarg :appender :initform nil)))

(defun maybe-signal-appending-message (logger appender message)
  (when (signal-messages logger)
    (signal (make-condition 'appending-message
                            :logger logger :message message :appender appender)))
  message )

(defclass message ()
  ((logger :accessor logger :initarg :logger :initform nil)
   (level :accessor level :initarg :level :initform nil)
   (format-control :accessor format-control :initarg :format-control :initform nil)
   (format-args :accessor format-args :initarg :format-args :initform nil)
   (data-plist :accessor data-plist :initarg :data-plist :initform nil)
   (arg-literals :accessor arg-literals :initarg :arg-literals :initform nil)
   (timestamp :accessor timestamp :initarg :timestamp
              :initform (local-time:now))))

(defclass logger ()
  ((name :initarg :name :accessor name :initform nil)
   (signal-messages :accessor signal-messages :initarg :signal-messages :initform t)
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
    (list (symbol-munger:reintern name (or (symbol-package sym) *package*))
          (log-level-name-of level))))

(defun ensure-level-value (level)
  (typecase level
    (null 1)
    (integer level)
    ((or message logger) (ensure-level-value (level level)))
    (symbol
     (third (log-level-name-of level :raw? t )))))

(defun log-level-name-of (level &key raw?)
  (etypecase level
    (null nil)
    (message (log-level-name-of (level level)))
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
  (setf (parents log) (mapcar #'require-logger (ensure-list (parents log))))
  (ensure-list! (appenders log))
  (dolist (anc (parents log))
    (pushnew log (children anc) :key #'name))
  (unless (or (appenders log) (parents log))
    (ensure-stderr-appender log)))

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

(defmacro log.level-helper (logger level-name message-args
                            &aux
                            (logger-var (get-logger-var-name logger))
                            (mform `(make-message ,logger-var
                                     ,level-name
                                     (list ,@message-args)
                                     :arg-literals '(,@message-args))))
  (when (compile-time-enabled-p logger level-name)
    (with-spliced-unique-name (args)
      (splice-in-local-symbol-values
       (list 'logger logger-var 'level level-name
             'message-args mform)     
       '(when (enabled-p logger level)
         (do-logging logger message-args))))))

(defun %make-log-helper (name level-name)
  "Creates macros like logger.debug to facilitate logging"
  (let* ((logger-macro-name
           (symbol-munger:reintern
            #?"${name}.${ (without-earmuffs level-name) }"
            (symbol-package name))))
    (splice-in-local-symbol-values
     (list 'name name 'level level-name)
     `(defmacro ,logger-macro-name (&rest message-args)
       `(log.level-helper name level ,message-args)))))

(defmacro define-logger
    (name parents &key compile-time-level level appenders documentation)
  (declare (ignore documentation) (type symbol name))
  `(progn
    (defparameter
        ,(get-logger-var-name name)
      (make-instance 'logger
                     :name ',name
                     :level ,(or level (and (not parents) +debug+))
                     :compile-time-level ,compile-time-level
                     :appenders ,appenders
                     :parents (copy-list ',parents)))
    ,@(iter (for (level-name level-value-name) in-vector *log-level-names*)
        (collect (%make-log-helper name level-value-name)))
    ,(get-logger-var-name name)))

;;; Runtime levels
(defgeneric enabled-p (log level)
  (:method ((l logger) level)
    (>= (ensure-level-value level)
        (ensure-level-value (log.level l)))))

(defgeneric log.level (log)
  (:method ( log )
    (require-logger! log)
    (or (level log)
        (if (parents log)
            (loop for parent in (parents log)
                  minimize (log.level parent))
            (error "Can't determine level for ~S" log)))))

(defgeneric (setf log.level) (new log &optional recursive)
  (:documentation
   "Change the logger's level of to NEW-LEVEL. If RECUSIVE is T the
  setting is also applied to the sub logger of logger.")
  (:method (new-level log &optional (recursive t))  
    (require-logger! log)
    (ensure-level-value! new-level)
    (setf (slot-value log 'level) new-level)
    (when recursive
      (dolist (child (children log))
        (setf (log.level child) new-level)))
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
(defgeneric compile-time-enabled-p (log level)
  (:method (log level)
    (require-logger! log)
    (>= (ensure-level-value level)
        (or (ensure-level-value (log.compile-time-level log))
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

(defgeneric do-logging (logger message)
  (:documentation
   "Applys a message to 
   Message is either a string or a list. When it's a list and the first
    element is a string then it's processed as args to
    cl:format."  )
  (:method :around ( logger message)
    (declare (ignore logger message))
    ;; turn off line wrapping for the entire time while inside the loggers
    (with-logging-io () (call-next-method)))
  (:method ( log message )
    (require-logger! log)
    ;; this is probably a duplicate check, because our helper macros check
    ;; before evaluating the message args, but good to be sure, so that
    ;; extensions and calls to do-logging behave as expected
    (unless (enabled-p log message) (return-from do-logging))
    ;; if we have any appenders send them the message
    (dolist (appender (appenders log))
      (with-debugging-or-error-printing
          (log :continue "Try next appender")
        (append-message log appender message)))
    (dolist (parent (parents log))
      (with-debugging-or-error-printing
          (log :continue "Try next appender")
        (do-logging parent message)))
    (values message log)))

(defun logger-name-from-helper (name)
  (first (split-dot-sym name)))

(defun logger-level-from-helper (name)
  (second (split-dot-sym name)))

(defun get-log-fn (logger &key (level +debug+))
  "Given a logger identifier name like 'adwolf-log.debug or 'adwolf-log find the logger
   associated with it and build a (lambda (message &rest args)) that can be
   funcalled to log to that logger.
   "
  (get-logger! logger)
  (etypecase logger
    (null nil)
    (logger
     (lambda (&rest args)
       (when (enabled-p logger level)
         (do-logging logger (make-message logger level args)))
       (values)))
    (function logger)))


