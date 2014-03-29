(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :a-cl-logger.system)
    (defpackage :a-cl-logger.system
      (:use :common-lisp :asdf))))

(in-package :a-cl-logger.system)
 
(defsystem :a-cl-logger
  :description "A library refactoring of arnesi"
  :author "Russ Tyndall <russ@acceleration.net>, Nathan Bird <nathan@acceleration.net>, Ryan Davis <ryan@acceleration.net>"
  :licence "BSD"
  :serial t
  :components
  ((:file "packages")
   (:file "utils")
   (:file "log")
   (:file "appenders"))
  :depends-on (:iterate :symbol-munger :alexandria :cl-interpol))

(defsystem :a-cl-logger-tests
  :description "A library refactoring of arnesi"
  :author "Russ Tyndall <russ@acceleration.net>, Nathan Bird <nathan@acceleration.net>, Ryan Davis <ryan@acceleration.net>"
  :licence "BSD"
  :serial t
  :components
  ((:file "tests"))
  :depends-on (:lisp-unit2 :a-cl-logger))
