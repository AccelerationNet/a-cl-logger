(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)

(define-logger testlog ())
(setf (appenders *testlog*) nil)

(defun run-tests ()
  (lisp-unit2:run-tests
   :package :a-cl-logger
   :name :a-cl-logger
   :run-contexts #'lisp-unit2:with-summary-context))

(lisp-unit2:define-test basic-level-test (:tags '(levels))
  (iter (for (_0 _1 log-level) in-vector *log-level-names*)
    (setf (level *testlog*) log-level)
    (iter (for (level-name _ message-level) in-vector *log-level-names*)
      (if (>= message-level log-level)
          (lisp-unit2:assert-signal
           'logging-message
           (do-log *testlog* message-level "Test ~A" level-name))
          (lisp-unit2:assert-no-signal
           'logging-message
           (do-log *testlog* message-level "Test ~A" level-name))))))

(lisp-unit2:define-test helper-tests (:tags '(helpers))
  (setf (level *testlog*) +dribble+)
  (lisp-unit2:assert-signal
     'logging-message
     (handler-bind ((logging-message
                      (lambda (c) (lisp-unit2:assert-equal +error+ (level (message c))))))
       (testlog.error "Test ~A" :some-stuff)))  
    (lisp-unit2:assert-signal
     'logging-message
     (handler-bind ((logging-message
                      (lambda (c) (lisp-unit2:assert-equal +debug+ (level (message c))))))
       (testlog.debug "Test ~A" :some-stuff))))


(lisp-unit2:define-test with-appender-test (:tags '(helpers))
  (lisp-unit2:assert-false (appenders *testlog*))
  (with-appender (*testlog* (make-instance 'debug-io-log-appender :level +dribble+) )
    (lisp-unit2:assert-eql 1 (length (appenders *testlog*)))
    (lisp-unit2:assert-signal
     'appending-message
     (testlog.debug "Test")))
  (lisp-unit2:assert-eql 0 (length (appenders *testlog*)))
  )
