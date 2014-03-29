(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :a-cl-logger.system)
    (defpackage :a-cl-logger.system
      (:use :common-lisp :asdf))))

(in-package :a-cl-logger.system)
 
(defsystem :a-cl-logger-logstash
  :description "A library refactoring of arnesi - this loads the "
  :author "Russ Tyndall <russ@acceleration.net>, Nathan Bird <nathan@acceleration.net>, Ryan Davis <ryan@acceleration.net>"
  :licence "BSD"
  :serial t
  :components
  ((:file "logstash"))
  :depends-on (:a-cl-logger :zeromq))
