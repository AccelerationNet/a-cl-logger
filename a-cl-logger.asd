(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :a-cl-logger.system)
    (defpackage :a-cl-logger.system
      (:use :common-lisp :asdf))))

(in-package :a-cl-logger.system)
 
(defsystem :a-cl-logger
  :description "A logger that sends to multiple destinations in multiple formats. Based on arnesi logger"
  :author "Russ Tyndall <russ@acceleration.net>, Nathan Bird <nathan@acceleration.net>, Ryan Davis <ryan@acceleration.net>"
  :version "1.0.1"
  :licence "BSD"
  :serial t
  :components
  ((:file "packages")
   (:file "utils")
   (:file "log")
   (:file "appenders")
   (:file "helpers"))
  :depends-on (:iterate :symbol-munger :alexandria :cl-interpol :cl-json :local-time
                :cl-json :closer-mop :osicat))

(defsystem :a-cl-logger-tests
  :description "Tests for: a-cl-logger"
  :author "Russ Tyndall <russ@acceleration.net>, Nathan Bird <nathan@acceleration.net>, Ryan Davis <ryan@acceleration.net>"
  :licence "BSD"
  :serial t
  :components
  ((:file "tests"))
  :depends-on (:lisp-unit2 :a-cl-logger))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :a-cl-logger))))
  (asdf:load-system :a-cl-logger-tests)
  (let ((*package* (find-package :a-cl-logger)))
    (eval (read-from-string "(run-tests)"))))
