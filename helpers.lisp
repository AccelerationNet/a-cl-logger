(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)

;; Make sure we have a root logger after the whole logging framework is created.
;; the force/unless is needed because we needed to reference the var before define-logger :/
(unless *root-logger*
  (define-logger root-logger ()
    :level +info+
    :force? t))

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
       (apply #'do-log logger level args)))
    (function logger)))

(defun open-message-block (message)
  (when (format-control message)
    (setf (format-control message)
          #?"BEGIN ${(format-control message)}"))
  (push-m-plist (list :begin (timestamp message))
                message))

(defun close-message-block (open message)
  (when (format-control message)
    (setf (format-control message)
          #?"  END ${(format-control message)}"))  
  (push-m-plist
   (list :end  (timestamp message) 
         :begin (timestamp open)
         :duration (local-time:timestamp-difference
                         (timestamp message)
                         (timestamp open)))
   message))

(defmacro log-around ( logger-form &body body )
  (alexandria:with-unique-names (open message)
    `(let ( ,open )
      (progn
        (handler-bind
            ((generating-message
               (lambda (c)
                 (let ((,message (message c)))
                   (open-message-block ,message)
                   (setf ,open ,message)
                   ))))
          ,logger-form)
        (multiple-value-prog1 (progn ,@body)
          (handler-bind
              ((generating-message
                 (lambda (c) (close-message-block ,open (message c)))))
            ,logger-form))
        ))))

(defun make-log-path (root file)
  (make-pathname :name file :type "log" :defaults root))

(defun setup-logger (logger &key level file-name log-root (buffer-p t))
  "Reconfigures a logger such that it matches the setup specified

   This is sometimes necessary if your streams get messed up (eg: slime
   disconnect and reconnect)

   Always ensure there is a *error-output* stream logger
   and if a file-name is passed in a file-logger going to it"
  (require-logger! logger)
  (ensure-debug-io-appender logger)
  (when log-root
    (ensure-file-appender logger :directory log-root :name file-name :buffer-p buffer-p))
  (when level (setf (log-level logger) level)))

(defun push-into-message (&rest plist)
  (push-m-plist plist *message*))


(defmacro when-log-message-* (signal (&body handler-body) &body body)

  "A macro that allows appending data to the log message based on the dynamic
   context of the message as it is being generated.

   The data-builder-form will be executed inside a context where
   (push-into-message key value) is a function to put data into the message
   the first form is

   Inside of the handler body, a `change-message` restart is available

   Ex: attaching information about the current http-context to log messages
   originating from it.
  "
  `(handler-bind
    ((,signal (lambda (c) (declare (ignorable c))
                (with-debugging-or-error-printing (*logger*)
                  ,@handler-body))))
    ,@body))

(defmacro when-log-message-generated ((&body handler-body) &body body)
  `(when-log-message-* generating-message (,@handler-body) ,@body))

(defmacro when-log-message-logged ((&body handler-body) &body body)
  `(when-log-message-* logging-message (,@handler-body) ,@body))

(defmacro when-log-message-appended ((&body handler-body) &body body)
  `(when-log-message-* appending-message (,@handler-body) ,@body))

(defmacro with-appender ((logger appender) &body body)
  "Add an appender to logger for the duration of the scope"
  (alexandria:with-unique-names (log app)
    `(let ((,log ,logger) (,app ,appender))
      (when-log-message-logged
          ((when (eql *logger* ,log)
             (do-append *logger* ,app *message*)))
        ,@body))))

(defmacro with-logged-output-to-place ((logger place) &body body)
  (alexandria:with-unique-names (appender)
    `(let ((,appender (make-instance 'string-stream-appender)))
      (with-appender (,logger ,appender)
        (multiple-value-prog1 (progn ,@body)
          (setf ,place (get-output-stream-string (log-stream ,appender))))))))

(defmacro with-logged-output ((logger) &body body)
  (alexandria:with-unique-names (out)
    `(let ((,out))
      (with-logged-output-to-place (,logger ,out)
        ,@body)
      ,out)))

(defmacro log-errors ((logger &optional message) &body body)
  "like ignore-errors but logs instead"
  `(handler-case (progn ,@body)
    (error (c)
     (do-log ,logger +error+ :message ,message :error c)
     (values nil c))))

(defmacro log-serious-conditions ((logger &optional message) &body body)
  "like ignore-errors but logs instead"
  `(handler-case (progn ,@body)
    (serious-condition (c)
     (do-log ,logger +error+ :message ,message :error c)
     (values nil c))))


(defun close-all-files (&key (loggers *logger-vars*))
  (iter (for ln in loggers)
    (for log = (get-logger ln))
    (when log
      (iter (for a in (appenders log))
        (when (and (typep a 'file-log-appender)
                  (log-stream a))
          (close (log-stream a)))))))
