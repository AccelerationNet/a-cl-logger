;; -*- lisp -*-

(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)

(defclass appender ()
  ()
  (:documentation "The base of all log appenders (destinations)"))

(defclass stream-log-appender (appender)
  ((stream :initarg :stream :accessor log-stream)
   (date-format :initarg :date-format :initform :time
    :documentation "Format to print dates. Format can be one of: (:iso :stamp :time)"))
  (:documentation "Human readable to the console logger."))

(defclass stderr-log-appender (stream-log-appender)
  ()
  (:default-initargs :stream *error-output*))

(defclass debug-io-log-appender (stream-log-appender)
  ()
  (:default-initargs :stream *debug-io*))

(defclass file-log-appender (stream-log-appender)
  ((log-file :initarg :log-file :accessor log-file
             :documentation "Name of the file to write log messages to.")
   (buffer-p :initarg :buffer-p :accessor buffer-p :initform t))
  (:default-initargs :date-format :iso))

(defgeneric append-message (logger log-appender message)
  (:documentation
   "The method responsible for actually putting the logged information somewhere")
  (:method :before (logger log-appender message)
    (maybe-signal-appending-message
     logger log-appender message)))

(defgeneric print-message (log appender message stream)
  (:method ((log logger) (appender appender) message stream)
    (etypecase message
      (message
          (format stream "~A ~7A "
                  (%logger-name-for-output log)
                  (log-level-name-of message))
        (when (format-control message)
          (apply #'format stream (format-control message) (format-args message)))
        (format stream " ~{~A:~A~^, ~}" (data-plist message)))
      (function (print-message log appender (funcall message) stream)))))

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

(defmethod append-message ((logger logger)
                           (s stream-log-appender)
                           message)
  (with-stream-restarts (s (append-message logger s message))
    (maybe-with-presentations ((log-stream s) str)
      (let* ((format (slot-value s 'date-format)))
        (format-time :stream str :format format)
        (princ #\space str)
        (print-message logger s message str)
        (terpri str)
        ))))

(defun logger-inspector-lookup-hook (form)
  (when (symbolp form)
    (let ((logger (or (ignore-errors (get-logger form))
                      (ignore-errors (get-logger (logger-name-from-helper form))))))
      (when logger
        (values logger t)))))

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

(defmethod append-message ((logger logger)
                           (appender file-log-appender)
                           message)
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

(defun ensure-stream-appender (logger stream
                               &key (new-type 'stream-log-appender))
  (require-logger! logger)
  (iter (for app in (appenders logger))
    (when (and (typep app 'stream-log-appender)
               (equal (log-stream app) stream))
      (return-from ensure-stream-appender app)))
  (let ((new (make-instance new-type :stream stream)))
    (push new (appenders logger))
    new))

(defun ensure-stderr-appender (logger)
  (require-logger! logger)
  (ensure-stream-appender
   logger *error-output* :new-type 'stderr-log-appender))

(defun ensure-debug-io-appender (logger)
  (require-logger! logger)
  (ensure-stream-appender
   logger *debug-io* :new-type 'debug-io-log-appender))

(defun ensure-file-appender (logger path &key (buffer-p t))
  (require-logger! logger)
  (iter (for app in (appenders logger))
    (when (and (typep app 'file-log-appender)
               (equal (log-file app) path))
      ;; if we find a matching appender, 
      (setf (buffer-p app) buffer-p)
      (return-from ensure-file-appender app)))
  (let ((new (make-instance 'file-log-appender
                            :log-file path
                            :buffer-p buffer-p)))
    (push new (appenders logger))
    new))

