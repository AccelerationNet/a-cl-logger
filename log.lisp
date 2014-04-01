;; -*- lisp -*-

(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)

;;;; * A Trivial logging facility

;;;; A logger is a way to have the system generate a text message and
;;;; have that messaged saved somewhere for future review. Logging can
;;;; be used as a debugging mechanism or for just reporting on the
;;;; status of a system.

;;;; Logs are sent to a particular log category, each log category
;;;; sends the messages it receives to its handlers. A handler's job
;;;; is to take a message and write it somewhere. Log categories are
;;;; organized in a hierarchy and messages sent to a log category will
;;;; also be sent to that category's ancestors.

;;;; Each log category has a log level which is used to determine
;;;; whether are particular message should be processed or
;;;; not. Categories inherit their log level from their ancestors. If a
;;;; category has multiple fathers its log level is the min of the
;;;; levels of its fathers.

;;;; ** Log Levels

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +dribble+ 0)
  (defparameter +debug+   1)
  (defparameter +info+    2)
  (defparameter +warn+    3)
  (defparameter +error+   4)
  (defparameter +fatal+   5)
  (defparameter *max-category-name-length* 12)

  (defparameter *log-level-names*
    #((dribble +dribble+)
      (debug +debug+)
      (info +info+)
      (warn +warn+)
      (error +error+)
      (fatal +fatal+))))

(defun ensure-level-value (level)
  (typecase level
    (null 1)
    (integer level)
    (symbol (symbol-value level))))

(define-condition missing-logger (error)
  ((name :accessor name :initarg :name :initform nil))
  (:report (lambda (c s)
             (format s "~&Error: no logger defined named :~S" (name c)))))

(defun missing-logger (name)
  (error (make-condition 'missing-logger :name name)))

(defun get-logger-var-name (name)
  (typecase name
    (null nil)
    (log-category (get-logger-var-name (name name)))
    (symbol (symbol-munger:reintern #?"*${name}*" (symbol-package name)))))

(defun get-logger (name)
  (typecase name
    (null nil)
    (symbol (handler-case (symbol-value (get-logger-var-name name))
              (unbound-variable (c) (declare (ignore c)))))
    (log-category name)))

(defun require-logger (name)
  (or (get-logger name)
      (missing-logger name)))

(defun (setf get-logger) (new name)
  (setf (symbol-value (get-logger-var-name name))
        new))

(defun rem-logger (name)
  (setf (symbol-value (get-logger-var-name name)) nil))


(defun log-level-name-of (level)
  (when (not (< -1 level (length *log-level-names*)))
    (error "~S is an invalid log level" level))
  (car (aref *log-level-names* level)))

;;;; ** Log Categories

(defclass message ()
  ((format-control :accessor format-control :initarg :format-control :initform nil)
   (args :accessor args :initarg :args :initform nil)
   (args-plist :accessor args-plist :initarg :args-plist :initform nil)))

(defclass log-category ()
  ((ancestors
    :initform '()     :accessor ancestors :initarg :ancestors
    :documentation "The log categories this category inherits from.")
   (children
    :initform '()     :accessor children  :initarg :children
    :documentation "The log categories which inherit from this category.")
   (appenders
    :initform '()     :accessor appenders :initarg :appenders
    :documentation "A list of appender objects this category sholud send messages to.")
   (level
    :initform nil :initarg :level :accessor level
    :type (or null integer)
    :documentation "This category's log level.")
   (compile-time-level
    :initform +dribble+ :initarg :compile-time-level :accessor compile-time-level
    :type integer
    :documentation "This category's compile time log level. Any log expression below this level will macro-expand to NIL.")
   (name :initarg :name :accessor name :initform nil)))

(defmethod print-object ((category log-category) stream)
  (print-unreadable-object (category stream :type t :identity t)
    (format stream "~S ~a"
	    (if (slot-boundp category 'name)
		(name category)
		"#<NO NAME>")
	    (level category))))

(defmethod initialize-instance :after ((l log-category) &key &allow-other-keys)
  (setf (ancestors l) (mapcar #'require-logger (ensure-list (ancestors l))))
  (ensure-list! (appenders l))
  (dolist (anc (ancestors l))
    (pushnew l (children anc) :key #'name))
  (unless (appenders l)
    (setup-logger l)))

(defun make-message (args arg-names)
  (lambda (&aux (mc (first args)))
    (make-instance
     'message
     :format-control (and (stringp mc) mc)
     :args (if (stringp mc) (rest args) args)
     :args-plist
     (iter
       (for n in arg-names)
       (for v in args)
       (when (and (first-iteration-p) (stringp mc))
         (next-iteration))
       (appending `(,n ,v))))))

(defmacro log.level-helper (logger level-name message-args
                            &aux
                            (logger-var (get-logger-var-name logger))
                            (mform `(make-message (list ,@message-args)
                                     '(,@message-args))))
  (when (compile-time-enabled-p logger level-name)
    (with-spliced-unique-name (args)
      (splice-in-local-symbol-values
       (list 'logger logger-var 'level level-name
             'message-args mform)     
       '(when (enabled-p logger level)
         (handle logger message-args level))))))

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

(defmacro deflogger
    (name ancestors &key compile-time-level level appenders documentation)
  (declare (ignore documentation) (type symbol name))
  `(progn
    (defparameter
        ,(get-logger-var-name name)
      (make-instance 'log-category
                     :name ',name
                     :level ,(or level (and (not ancestors) +debug+))
                     :compile-time-level ,compile-time-level
                     :appenders ,appenders
                     :ancestors ,ancestors))
    ,@(iter (for (level-name level-value-name) in-vector *log-level-names*)
        (collect (%make-log-helper name level-value-name)))
    ,(get-logger-var-name name)))

;;; Runtime levels
(defgeneric enabled-p (log level)
  (:method ((cat log-category) level)
    (>= level (ensure-level-value (log.level cat)))))

(defgeneric log.level (log)
  (:method ( cat )
    (require-logger! cat)
    (or (level cat)
        (if (ancestors cat)
            (loop for ancestor in (ancestors cat)
                  minimize (log.level ancestor))
            (error "Can't determine level for ~S" cat)))))

(defgeneric (setf log.level) (new log &optional recursive)
  (:documentation
   "Change the log level of CAT to NEW-LEVEL. If RECUSIVE is T the
  setting is also applied to the sub categories of CAT.")
  (:method (new-level log &optional (recursive t))  
    (require-logger! log)
    (ensure-level-value! new-level)
    (setf (slot-value log 'level) new-level)
    (when recursive
      (dolist (child (children log))
        (setf (log.level child) new-level)))
    new-level))

(defun %category-name-for-output (name &key (width *max-category-name-length*))
  "Output the category name such that it takes exactly :width characters
   and displays the right most :width characters if name is too long

   this simplifies our formatting"
  (let* ((len (length name))
         (out (make-string width :initial-element #\space)))
    (replace out name
             :start1 (max 0 (- width len))
             :start2 (max 0 (- len width)))))

;;; Compile time levels
(defgeneric compile-time-enabled-p (log level)
  (:method (log level)
    (require-logger! log)
    (>= (ensure-level-value level)
        (or (ensure-level-value (log.compile-time-level log)) +dribble+))))

(defgeneric log.compile-time-level (log)
  (:method ( log )
    (require-logger! log)
    (or (compile-time-level log)
        (loop for ancestor in (ancestors log)
              minimize (log.compile-time-level ancestor))
        +dribble+)))

(defgeneric (setf log.compile-time-level) (new log &optional recursive)  
  (:documentation
   "Change the compile time log level of CAT to NEW-LEVEL. If RECUSIVE is T the
  setting is also applied to the sub categories of CAT.")
  (:method (new-level log &optional (recursive t))
    (get-logger! log)
    (setf (slot-value log 'compile-time-level) new-level)
    (when recursive
      (dolist (child (children log))
        (setf (log.compile-time-level child) new-level)))
    new-level))



;;;; ** Handling Messages

(defgeneric handle (category message level)
  (:documentation "Message is either a string or a list. When it's a list and the first element is a string then it's processed as args to cl:format.")
  (:method :around ( cat message level)
    ;; turn off line wrapping for the entire time while inside the loggers
    (with-logging-io () (call-next-method)))
  (:method ( log message level)
    (require-logger! log)
    (labels
        ((do-appenders (ac)
           ;; if we have any appenders send them the message
           (dolist (appender (appenders ac))
             (with-simple-restart (continue "Run the next log-appender")
               (handler-bind
                   ((error (lambda (c)
                             (ignore-errors
                              (format *error-output* "Error in log appender ~A:~%~A~%~S"
                                      appender c c))
                             (when *debugger-hook*
                               (invoke-debugger c))
                             (continue c))))
                 (append-message log appender message level))))

           ;; send the message to our ancestors	     
           (mapc #'do-appenders (ancestors ac))))
      (do-appenders log))))

(defun logger-name-from-helper (name)
  (first (split-dot-sym name)))

(defun logger-level-from-helper (name)
  (second (split-dot-sym name)))

(defun get-log-fn-for-log-name (log-name &optional (default-level '+debug+))
  "Given a log name like 'adwolf-log.debug or 'adwolf-log find the logger
   associated with it and build a (lambda (message &rest args)) that can be
   funcalled to log to that logger.
   "
  (etypecase log-name
    (null nil)
    (function log-name)
    (symbol
     (let* ((parts (split-dot-sym log-name))
            (name (first parts))
            (level-name (second parts))
            (level-sym (symbol-munger:reintern #?"+${level-name}+"))
            (level-sym (if (and (boundp level-sym) (integerp (symbol-value level-sym)))
                           level-sym default-level))
            (level (symbol-value level-sym))
            (logger (get-logger name)))
       (when logger
         (lambda (message &rest args)
           (when (enabled-p logger level)
             (handle logger (list* message args) level-sym))
           (values)))))))

(defun setup-logger (logger &key level file-name log-root (buffer-p t))
  "Reconfigures a logger such that it matches the setup specified

   This is sometimes necessary if your streams get messed up (eg: slime
   disconnect and reconnect)

   Always ensure there is a *error-output* stream logger
   and if a file-name is passed in a file-logger going to it"
  (require-logger! logger)
  (unless level (setf level (level logger)))
  (setf (appenders logger) nil)
  (unless (find "--quiet" sb-ext:*posix-argv* :test #'equal)
    (push (make-instance 'stream-log-appender :stream *error-output*)
          (appenders logger)))
  (when (and log-root file-name)
    (let ((log-path (make-pathname
                     :name file-name
                     :type "log"
                     :defaults log-root)))
      (push (make-instance 'file-log-appender
                           :log-file log-path
                           :buffer-p buffer-p)
            (appenders logger))))
  (setf (log.level logger) level))
