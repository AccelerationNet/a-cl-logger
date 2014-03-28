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

(defun get-logger-var-name (name)
  (symbol-munger:reintern #?"*${name}*" (symbol-package name)))

(defun get-logger (name)
  (typecase name
    (symbol (symbol-value (get-logger-var-name name)))
    (log-category name)))

(defun (setf get-logger) (new name)
  (setf (symbol-value (get-logger-var-name name))
        new))

(defun rem-logger (name)
  (setf (symbol-value (get-logger-var-name name)) nil))


(defun log-level-name-of (level)
  (when (not (< -1 level (length *log-level-names*)))
    (error "~S is an invalid log level" level))
  (car (aref *log-level-names* level)))

(defgeneric %print-message (log appender message stream)
  (:method (log appender message stream)
    (etypecase message
      (string (write-sequence message stream))
      (function (%print-message log appender (funcall message) stream))
      (list (if (stringp (first message))
                (apply #'format stream message)
                (apply #'format stream "~{~A ~}" message))))))

;;;; ** Log Categories

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
  (setf (ancestors l) (mapcar #'get-logger (ensure-list (ancestors l))))
  (ensure-list! (appenders l))
  (dolist (anc (ancestors l))
    (pushnew l (children anc) :key #'name)))

(defun %make-log-helper (name suffix level)
  "Creates macros like logger.debug to facilitate logging"
  (let* ((logger-macro-name
           (symbol-munger:reintern
            #?"${name}.${suffix}"
            (symbol-package name)))
         (logger (get-logger-var-name name)))
    ;; instead of nested quasi-quoting which I cant seem to get correct
    ;; lets replace the symbols with their values directly    
    (splice-in-local-symbol-values
     (list 'logger logger 'level level)
     `(defmacro ,logger-macro-name (message-control &rest message-args)
       (when (compile-time-enabled-p logger level)
         `(let ((args (list ,message-control ,@message-args)))
           (when (enabled-p logger level)
             (handle logger args level))))))))

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
        (collect (%make-log-helper name level-name level-value-name)))
    ,(get-logger-var-name name)))

;;; Runtime levels
(defgeneric enabled-p (log level)
  (:method ((cat log-category) level)
    (>= level (log.level cat))))

(defgeneric log.level (log)
  (:method ((cat-name symbol))
    (log.level (get-logger cat-name))) 
  (:method ((cat log-category))
    (or (level cat)
        (if (ancestors cat)
            (loop for ancestor in (ancestors cat)
                  minimize (log.level ancestor))
            (error "Can't determine level for ~S" cat)))))

(defgeneric (setf log.level) (new log &optional recursive)
  (:method (new-level log &optional (recursive t))
    "Change the log level of CAT to NEW-LEVEL. If RECUSIVE is T the
  setting is also applied to the sub categories of CAT."
    (setf log (get-logger log))
    (setf (slot-value log 'level) new-level)
    (when recursive
      (dolist (child (children log))
        (setf (log.level child) new-level)))
    new-level))

(defun %category-name-for-output (name &key (width *max-category-name-length*))
  "Output the category name such that it takes exactly :width characters
   this simplifies our formatting"
  (let* ((len (length name))
         (out (make-string width :initial-element #\space)))
    (replace out name
             :start1 (max 0 (- width len))
             :start2 (max 0 (- len width)))))

;;; Compile time levels
(defgeneric compile-time-enabled-p (log level)
  (:method (log level)
    (get-logger! log)
    (>= level (or (log.compile-time-level log) +dribble+))))

(defgeneric log.compile-time-level (log)
  (:method ( log )
    (get-logger! log)
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
    (get-logger! log)
    (labels ((do-appenders (ac)
               ;; if we have any appenders send them the message
               (dolist (appender (appenders ac))
                 (append-message log appender message level))

               ;; send the message to our ancestors	     
               (mapc #'do-appenders (ancestors ac))))
      (do-appenders log))))


(defgeneric append-message (category log-appender message level)
  (:method :around (category log-appender message level)
    (handler-bind
        ((error (lambda (c)  
                  (if *debugger-hook*
                      (invoke-debugger c)
                      (format *error-output* "ERROR Appending Message: ~A" c))
                  (return-from append-message))))
      (call-next-method))))

;;;; *** Stream log appender

(defclass appender () ())

(defclass stream-log-appender (appender)
  ((stream :initarg :stream :accessor log-stream)
   (date-format :initarg :date-format :initform :iso
    :documentation "Format to print dates. Format can be one of: (:iso :stamp :time)"))
  (:documentation "Human readable to the console logger."))

(defmacro with-stream-restarts ((s recall) &body body)
  `(restart-case
    (progn ,@body)
    (use-*debug-io* ()
     :report "Use the current value of *debug-io*"
     (setf (log-stream ,s) *debug-io*)
     ,recall)
    (use-*standard-output* ()
     :report "Use the current value of *standard-output*"
     (setf (log-stream ,s) *standard-output*)
     ,recall)
    (silence-logger ()
     :report "Ignore all future messages to this logger."
     (setf (log-stream ,s) (make-broadcast-stream)))))

(defmethod append-message ((category log-category)
                           (s stream-log-appender)
                           message level)
  (with-stream-restarts (s (append-message category s message level))
    (maybe-with-presentations ((log-stream s) str)
      (let* ((category-name (symbol-name (name category)))
             (level-name (typecase level
                           (symbol level)
                           (integer (log-level-name-of level)))))
        (multiple-value-bind (second minute hour day month year)
            (decode-universal-time (get-universal-time))
          (ecase (slot-value s 'date-format)
            (:iso (format str "~d-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0D"
                          year month day hour minute second))
            (:stamp (format str "~d~2,'0D~2,'0D ~2,'0D~2,'0D~2,'0D"
                            year month day hour minute second))
            (:time (format str "~2,'0D:~2,'0D:~2,'0D"
                           hour minute second))))
        (princ #\space str)
        (format str "~A/~7A "
                (%category-name-for-output category-name)
                level-name)
        (apply #'format str (ensure-list message))
        (terpri str)
        ))))

(defun make-stream-log-appender (&rest args &key (stream *debug-io*) &allow-other-keys)
  (apply #'make-instance 'stream-log-appender :stream stream args))

(defun logger-inspector-lookup-hook (form)
  (when (symbolp form)
    (let ((logger (or (ignore-errors (get-logger form))
                      (ignore-errors (get-logger (logger-name-from-helper form))))))
      (when logger
        (values logger t)))))

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

(defclass file-log-appender (stream-log-appender)
  ((log-file :initarg :log-file :accessor log-file
             :documentation "Name of the file to write log messages to.")
   (buffer-p :initarg :buffer-p :accessor buffer-p :initform t))
  (:default-initargs :date-format :iso))

(defun %open-log-file (ufla)
  (setf (log-stream ufla)
        (ignore-errors
          (let ((f (open (log-file ufla) :if-exists :append :if-does-not-exist :create
                         :direction :output
                         :external-format :utf-8)))
            (push (lambda () (force-output f) (close f)) sb-ext::*exit-hooks*)
            f))))

(defmethod (setf log-file) :after (val (ufla file-log-appender))
  (%open-log-file ufla))

(defmethod append-message ((category log-category)
                           (appender file-log-appender)
                           message level)
  (unless (and (slot-boundp appender 'stream)
               (log-stream appender))
    (%open-log-file appender))

  (restart-case (handler-case
                    (progn (call-next-method)
                           (unless (buffer-p appender)
                             (force-output (log-stream appender))))
                  (error () (invoke-restart 'open-log-file)))
    (open-log-file ()
      (%open-log-file appender)
      (ignore-errors (call-next-method)))))

(defun setup-logger (logger level &optional file-name log-root (buffer-p t))
  "Reconfigures a logger such that it matches the setup specified

   This is sometimes necessary if your streams get messed up (eg: slime
   disconnect and reconnect)

   Always ensure there is a *error-output* stream logger
   and if a file-name is passed in a file-logger going to it"
  (get-logger! logger)
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
