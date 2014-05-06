(in-package :cl-user)

(defpackage :a-cl-logger
  (:use :cl :iterate)
  (:nicknames :a-log)
  (:import-from #:alexandria #:ensure-list )
  (:shadow #:formatter)
  (:export #:+dribble+ #:+debug+ #:+info+ #:+warn+ #:+error+ #:+fatal+
           #:*log-level-names* #:log-level-name-of
           #:*message*
           #:*appender*

           #:do-log
           #:do-logging
           #:format-message
           #:append-message

           #:root-logger
           #:*root-logger*
           #:define-logger
           #:logger
           #:log-level
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
           #:ensure-debug-io-appender
           #:remove-appender
           #:make-log-path

           #:formatter
           #:json-formatter

           #:node-logstash-appender
           #:ensure-node-logstash-appender

           #:message
           #:format-args
           #:format-control
           #:data-plist

           #:get-log-fn
           #:setup-logger

           #:get-logger
           #:get-logger!
           #:require-logger
           #:require-logger!

           #:with-appender
           #:log-around
           #:when-log-message-generated
           #:when-log-message-appended
           #:push-into-message
           ))

