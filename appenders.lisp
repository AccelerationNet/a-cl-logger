;; -*- lisp -*-

(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)

(defclass formatter ()
  ()
  (:documentation "The base class of all message formatters"))

(defclass raw-formatter (formatter)
  ()
  (:documentation "format the message with no annotations"))

(defclass json ()
  ((json :accessor json :initarg :json :initform nil))
  (:documentation "A type to know when something is already encoded, so that
  we just write it out.  It is sometimes beneficial to pre-encode some bits of
  log data and this allows that"))

(defun alist-as-json (alist)
  "Create a pre-encoded json object from an alist"
  (make-instance 'json :json (json:encode-json-alist-to-string alist)))

(defun plist-as-json (&rest plist)
  "Create a pre-encoded json object from a plist"
  (make-instance 'json :json (json:encode-json-plist-to-string plist)))

(defclass json-formatter (formatter)
  ()
  (:documentation "The base class of all json formatters"))

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

(defclass string-stream-appender (stream-log-appender)
  ()
  (:documentation "a log that appends all messages into a string stream"))

(defmethod initialize-instance :after ((a string-stream-appender) &key &allow-other-keys)
  (setf (formatter a) (make-instance 'raw-formatter)        
        (log-stream a) (make-string-output-stream)))

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

(defmethod (setf log-level) (new-level (appender appender) &optional (recursive nil))
    (declare (ignore recursive))
    (ensure-level-value! new-level)
    (setf (slot-value appender 'level) new-level))

(defun %filter-plist (msg &optional exclude)
  (iter
    (with ex = exclude)
    (for (k v) on (data-plist msg) by #'cddr)
    (unless (or (member k ex) (member k seen))
      (collect k into rtn)
      (collect v into rtn))
    (collect k into seen)
    (finally (return rtn))))


(defgeneric format-value (v formatter)
  (:method (v formatter)
    (typecase v
      (symbol (prin1-to-string v))
      (list (mapcar (lambda (it) (format-value it formatter))  v))
      (json (json v))
      (t v)))
  (:method (v (f json-formatter))
    (typecase v
      ;; nil is a symbol we dont want to print
      (null (json:encode-json v))
      (symbol (json:encode-json (prin1-to-string v)))
      (list (as-json-array v))
      (json (write-sequence (json v) json:*json-output*))
      (t (json:encode-json v)))))

(defgeneric format-message
    (appender formatter message stream)
  (:method ((appender appender) formatter (message message) stream)
    (unless (typep formatter 'raw-formatter)
      (format stream "~A ~A ~7A "
              (timestamp message)
              (%logger-name-for-output
               (or (name message)
                   (logger message)))
              (log-level-name-of message)))
    (when (format-control message)
      (if (format-args message)
          (restart-case
              (handler-case
                  (apply #'format stream (format-control message) (format-args message))
                (error (c)
                  (write-sequence "ERROR-FORMATTING: \"" stream)
                  (write-sequence (format-control message) stream)
                  (write-char #\" stream)
                  (iter (for arg in (format-args message))
                    (write-sequence ", " stream)
                    (ignore-errors (princ arg stream)))
                  (when *debugger-hook* (invoke-debugger c))))
            (continue () ))
          (write-sequence (format-control message) stream)))
    (format stream " ~{~A:~A~^, ~}"
            (iter (for (k v) on (%filter-plist message) by #'cddr)
              (collect k)
              (collect (format-value v formatter))))
    (terpri stream))
  
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
             ;; lets not encode arg-literals as json because then they wont be
             ;; literal again
             :arg-literals (prin1-to-string (arg-literals message))))
          
          (iter (for (k v) on (%filter-plist message seen)
                     by #'cddr)
            (as-json-object-member k v formatter)))))))

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
     :report "Ignore all future messages to this appender."
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
            (push (lambda ()
                    (ignore-errors (force-output f))
                    (ignore-errors (close f)))
                  sb-ext::*exit-hooks*)
            f))))

(defmethod (setf log-file) :after (val (ufla file-log-appender))
  (%open-log-file ufla))

(defmethod append-message ((appender file-log-appender) message)
  (unless (and (slot-boundp appender 'stream)
               (log-stream appender))
    (%open-log-file appender))

  ;; if we are no longer pointing to the correct file
  ;; EG: the file has been rotated
  (unless (and (probe-file (log-file appender))
               (and (log-stream appender)
                    (log-file appender)
                    (equal (osicat-posix:stat-ino
                            (osicat-posix:fstat (log-stream appender)))
                           (osicat-posix:stat-ino
                            (osicat-posix:stat (log-file  appender)))
                           )))
    (when (log-stream appender)
      (close (log-stream appender)))
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

(defun find-appender (logger &key type predicate (recurse? t))
  (require-logger! logger)
  (iter (for a in (appenders logger))
    (when (and (or (null type) (typep a type))
               (or (null predicate)
                   (funcall predicate a)))
      (return-from find-appender (values a logger))))
  (when recurse?
    (iter
      (for p in (parents logger))
      (for a =
           (find-appender p
                          :type type
                          :predicate predicate))
      (when a (return-from find-appender (values a p))))))

(defun remove-appender (logger &key type predicate path (recurse? nil))
  (multiple-value-bind (appender appender-logger)
      (find-appender
       logger
       :type type
       :recurse? recurse?
       :predicate (or predicate
                      (when path (lambda (a) (equal (log-file a) path)))))
    (when appender
      (setf (appenders appender-logger)
            (remove appender (appenders appender-logger))))
    (values appender appender-logger)))

