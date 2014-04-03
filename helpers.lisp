(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)

(defmacro with-appender ((logger appender) &body body)
  "Add an appender to logger for the duration of the scope"
  (alexandria:with-unique-names (log app)
    `(let ((,log ,logger)
           (,app ,appender))
      (unwind-protect
           (progn
             (push ,appender (appenders ,log))
             ,@body)
        (setf (appenders ,log)
              (remove ,app (appenders ,log)))))))

(defun open-message-block (message)
  (when (format-control message)
    (setf (format-control message)
          #?"BEGIN ${(format-control message)}"))
  (push-plist :begin (timestamp message) (data-plist message)))

(defun close-message-block (open message)
  (when (format-control message)
    (setf (format-control message)
          #?"  END ${(format-control message)}"))
  
  (push-plist :end  (timestamp message) (data-plist message))
  (push-plist :begin (timestamp open) (data-plist message))
  (push-plist :duration (local-time:timestamp-difference
                         (timestamp message)
                         (timestamp open))
              (data-plist message)))

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
        (prog1 (progn ,@body)
          (handler-bind
              ((generating-message
                 (lambda (c) (close-message-block ,open (message c)))))
            ,logger-form))
        ))))

(defun setup-logger (logger &key level file-name log-root (buffer-p t))
  "Reconfigures a logger such that it matches the setup specified

   This is sometimes necessary if your streams get messed up (eg: slime
   disconnect and reconnect)

   Always ensure there is a *error-output* stream logger
   and if a file-name is passed in a file-logger going to it"
  (require-logger! logger)
  (ensure-stderr-appender logger)
  (when (and log-root file-name)
    (let ((log-path (make-pathname
                     :name file-name
                     :type "log"
                     :defaults log-root)))
      (ensure-file-appender logger log-path :buffer-p buffer-p)))
  (when level (setf (log.level logger) level)))
