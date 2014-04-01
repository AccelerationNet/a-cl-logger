;; -*- lisp -*-

(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)

'(progn
  (deflogger testlog ())
  (push (make-instance 'logstash-appender) (appenders *testlog*)))

(defclass node-logstash-appender (stream-log-appender)
  ((log-stash-type :accessor log-stash-type :initarg :log-stash-type :initform nil)
   (log-stash-server
    :accessor log-stash-server :initarg :log-stash-server
    :initform nil)))

(defun zeromq-send-data (json &key server &aux connected)
  "Push a message to the logstash zmq"
  (zmq:with-context (ctx 1)
    (zmq:with-socket (socket ctx :push)      
      (unwind-protect
           (progn 
             (setf connected (zerop (zmq:connect socket server)))
             (zmq:with-msg-init-data (msg json) 
               (zmq:msg-send msg socket)))
        (when connected
          ;; Disconnecting causes all messages to not flow, for some reason
          ;; I believe we will blcok on with-socket to close until all messages
          ;; are sent which this seems to bypass?
          ;; (zmq:disconnect socket server)
          )))))

(defmethod append-message ((log logger)
                           (appender node-logstash-appender)
                           message)
  (let ((out (with-output-to-string (json)
               (print-message log appender message json))))
    (format t "~%SENDING ZMQ MSQ: ~A~%" out)
    (zeromq-send-data out :server (log-stash-server appender))
    ))


(defmethod print-message (log
                           (appender node-logstash-appender)
                           (message message)
                           stream)
  (let ((json::*json-output* stream))
    (json:with-object ()
      (json:encode-object-member :type (or
                                        (log-stash-type appender)
                                        (name log)))
      (json:encode-object-member
       :tags (list* (name log) (mapcar #'name (parents log))))
      (json:encode-object-member :file nil)
      (json:encode-object-member :level (log-level-name-of message))
      (json:encode-object-member :hostname #+sbcl (sb-unix:unix-gethostname))
      (json:encode-object-member :@timestamp (princ-to-string (timestamp message)))
      (cond
        ((format-control message)
         (json:encode-object-member
          :message
          (apply #'format nil
                 (format-control message)
                 (args message)))
         (iter
           (for k in (arg-literals message))
           (for v in (args message))
           (as-json-o-val k v)))
        (:plist-values
         (iter (for (k v) on (args message) by #'cddr)
           (as-json-o-val k v)))))))

