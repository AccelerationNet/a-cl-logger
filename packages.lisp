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
           #:make-message

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
           #:log-stream
           #:stream-log-appender
           #:string-stream-appender
           #:stderr-log-appender
           #:file-log-appender
           #:ensure-stderr-appender
           #:ensure-file-appender
           #:ensure-debug-io-appender
           #:find-appender
           #:debug-io-log-appender
           #:remove-appender
           #:make-log-path

           #:formatter
           #:raw-formatter
           #:json-formatter
           #:json
           #:alist-as-json
           #:plist-as-json

           #:node-logstash-appender
           #:ensure-node-logstash-appender
           #:without-logstashing
           #:with-logstashing-only
           #:with-concatenated-logstash-logs

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

           #:log-errors
           #:log-serious-conditions
           #:with-appender
           #:with-logged-output-to-place
           #:log-around
           #:when-log-message-generated
           #:when-log-message-appended
           #:push-into-message
           #:default-signal-bindings
           #:add-signal-handler
           #:generating-message
           #:appending-message
           #:logging-message
           
           ))

