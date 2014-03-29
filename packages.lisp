(in-package :cl-user)

(defpackage :a-cl-logger
  (:use :cl :iterate)
  (:nicknames :a-log)
  (:import-from #:alexandria #:ensure-list )
  (:export #:+dribble+ #:+debug+ #:+info+ #:+warn+ #:+error+ #:+fatal+
           #:*log-level-names*
           #:deflogger
           #:log-category
           #:appenders
           #:level
           #:children))

