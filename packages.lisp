(in-package :cl-user)

(defpackage :a-cl-logger
  (:use :cl :iterate)
  (:nicknames :a-log)
  (:import-from #:alexandria #:ensure-list )
  (:export #:+dribble+ #:+debug+ #:+info+ #:+warn+ #:+error+ #:+fatal+
           #:*log-level-names*

           #:do-logging
           #:print-message
           #:append-message
           
           #:define-logger
           #:logger
           #:name
           #:appenders
           #:parents
           #:level
           #:children
           
           #:appender
           #:stream-log-appender
           #:stderr-log-appender
           #:file-log-appender
           #:ensure-stderr-appender
           #:ensure-file-appender

           #:node-logstash-appender
           #:ensure-node-logstash-appender

           #:message
           #:args
           #:format-control
           #:args-plist

           #:get-log-fn
           #:setup-logger

           #:get-logger
           #:get-logger!
           #:require-logger
           #:require-logger!

           #:with-appender
           #:log-around
           
           ))

