(in-package :cl-user)

(defpackage :a-cl-logger
  (:use :cl :iterate)
  (:import-from #:alexandria #:ensure-list )
  (:export #:+dribble+ #:+debug+ #:+info+ #:+warn+ #:+error+ #:+fatal+
           #:*log-level-names*
           #:log-category
           #:appenders
           #:level
           #:children))

