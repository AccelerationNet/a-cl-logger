;; -*- lisp -*-

(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)

(defclass formatter ()
  ()
  (:documentation "The base class of all message formatters"))

(defclass json-formatter (formatter)
  ()
  (:documentation "The base class of all message formatters"))

(defclass appender ()
  ((formatter :accessor formatter
                      :initarg :formatter :initform nil)
   (level :accessor level :initarg :level :initform nil))
  (:documentation "The base of all log appenders (destinations)"))

(defmethod initialize-instance :after ((a appender)
                                       &key &allow-other-keys
                                       &aux (f (formatter a)))
  (when (and f (symbolp f))
    (setf (formatter a) (make-instance f))))

(defclass stream-log-appender (appender)
  ((stream :initarg :stream :accessor log-stream :initform nil)
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

(defclass json-file-log-appender (file-log-appender)
  ()
  (:default-initargs
   :date-format :iso
   :formatter 'json-formatter))

(defgeneric append-message (log-appender message)
  (:documentation
   "The method responsible for actually putting the logged information somewhere"))

(defun %filter-plist (msg &optional exclude)
  (iter
    (with ex = exclude)
    (for (k v) on (data-plist msg) by #'cddr)
    (unless (or (member k ex) (member k seen))
      (collect k into rtn)
      (collect v into rtn))
    (collect k into seen)
    (finally (return rtn))))

(defgeneric format-message
    (appender formatter message stream)
  (:method ((appender appender) formatter (message message) stream)
    (format stream "~A ~A ~7A "
            (timestamp message)
            (%logger-name-for-output
             (or (name message)
                 (logger message)))
            (log-level-name-of message))
    (when (format-control message)
      (if (format-args message)
          (handler-case
              (apply #'format stream (format-control message) (format-args message))
            (error ()
              (write-sequence "ERROR-FORMATTING: " stream)
              (write-sequence (format-control message) stream)
              (princ (format-args message) stream)))
          (write-sequence (format-control message) stream)))
    (format stream " ~{~A:~A~^, ~}~%"
            (%filter-plist message)))
  
  (:method ((appender appender)
            (formatter json-formatter)
            (message message)
            stream
            &aux seen (log (logger message)))
    (let ((plist (data-plist message))
          (json::*json-output* stream))
      (flet ((js-val (key value
                       &aux (pval (getf plist key)))
               (when pval (push key seen))
               (json:encode-object-member
                key (or pval value))))
        (json:with-object ()
          (js-val :type (or (name message) (name log)))
          ;; this is a logstash thing, but seemed generally useful,
          ;; can be moved if its not
          (js-val
           :tags (list* (name log) (mapcar #'name (parents log))))
          (js-val :level (log-level-name-of message))
          (js-val :hostname #+sbcl (sb-unix:unix-gethostname))
          ;; NB: this naming is for the sake of logstash, if we need to we can
          ;; abstract it better
          (js-val :@timestamp (princ-to-string (timestamp message)))
          (when (format-control message)
            (js-val
             :message
             (if (format-args message)
                 (apply #'format nil
                        (format-control message)
                        (format-args message))
                 (format-control message))))
          (when (arg-literals message)
            (json:encode-object-member
             :arg-literals (arg-literals message)))
          (iter (for (k v) on (%filter-plist message seen)
                     by #'cddr)
            (as-json-o-val k v)))))))


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

(defmethod append-message ((s stream-log-appender) message)
  (with-stream-restarts (s (append-message s message))
    (maybe-with-presentations ((log-stream s) str)      
      (format-message s (formatter s) message str))))

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

(defmethod append-message ((appender file-log-appender) message)
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
                               &key (type 'stream-log-appender))
  (require-logger! logger)
  (alexandria:when-let
      (a (find-appender
          logger
          :type type
          :predicate (lambda (a) (equal (log-stream a) stream))))
    (return-from ensure-stream-appender a))
  (let ((new (make-instance type :stream stream)))
    (push new (appenders logger))
    new))

(defun ensure-stderr-appender (logger)
  (require-logger! logger)
  (ensure-stream-appender
   logger *error-output* :type 'stderr-log-appender))

(defun ensure-debug-io-appender (logger)
  (require-logger! logger)
  (ensure-stream-appender
   logger *debug-io* :type 'debug-io-log-appender))

(defun ensure-file-appender
    (logger
     &key (buffer-p t) name directory
     (path (make-log-path directory (or name (string (name logger))))))
  (require-logger! logger)
  (alexandria:when-let
      (a (find-appender
          logger
          :type 'file-log-appender
          :predicate (lambda (a) (equal (log-file a) path))))
    ;; if we find a matching appender, 
    (setf (buffer-p a) buffer-p)
    (return-from ensure-file-appender a))
  (let ((new (make-instance 'file-log-appender
                            :log-file path
                            :buffer-p buffer-p)))
    (push new (appenders logger))
    new))

(defun find-appender (logger &key type predicate)
  (require-logger! logger)
  (iter (for a in (appenders logger))
    (when (and (or (null type) (typep a type))
               (or (null predicate)
                   (funcall predicate a)))
      (return-from find-appender a)))
  (iter
    (for p in (parents logger))
    (for a = 
         (find-appender p
                        :type type
                        :predicate predicate))
    (when a (return-from find-appender a))))

