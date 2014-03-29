;; -*- lisp -*-

(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)


(defclass logstash-appender (stream-log-appender)
  ())

(defmethod append-message ((log log-category)
                           (append-message logstash-appender)
                           message level)
  (%print-message category s message level))

(defmethod %print-message (log (appender logstash-appender) (message message) stream)
  (let ((json::*json-output* stream))
    (json:with-object ()
      (json:encode-object-member :type (name log))
      (json:encode-object-member
       :tags (list* (name log) (mapcar #'name (ancestors log))))
      (json:encode-object-member :file nil)
      (json:encode-object-member :host #+sbcl (sb-unix:unix-gethostname))
      (json:encode-object-member :raw_json_fields
        '("message" "host" "file" "tags" "@timestamp" "type"))
      (json:as-object-member (:message)
        (json:with-object ()
          (when (format-control message)
            (json:encode-object-member
             :formatted-message
             (apply #'format nil
                    (format-control message)
                    (args message))))
          (iter (for (k v) on (args-plist message) by #'cddr)
            (json:encode-object-member k v)))))))
