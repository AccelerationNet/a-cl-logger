(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)

(define-logger testlog ())
(setf (appenders *testlog*) nil)

(defun run-tests ()
  (let ((root-appenders (appenders *root-logger*)))
    (setf (appenders *root-logger*) nil)
    (unwind-protect    
         (lisp-unit2:run-tests
          :package :a-cl-logger
          :name :a-cl-logger
          :run-contexts #'lisp-unit2:with-summary-context)
      (setf (appenders *root-logger*) root-appenders))))

(lisp-unit2:define-test basic-level-test (:tags '(levels))
  (setf (level *testlog*) 0)
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
  (setf (level *testlog*) 0)
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
  (setf (level *testlog*) 0)
  (lisp-unit2:assert-eql 0 (length (appenders *testlog*)))
  (let ((app (make-instance 'debug-io-log-appender :level +dribble+))
        (gotit? nil))
    (when-log-message-appended
        ((when (and (eql *testlog* (logger *message*))
                    (eql *appender* app))
           (setf gotit? t)))      
      (with-appender (*testlog* app)
        (testlog.debug "Test")))
    (lisp-unit2:assert-true gotit?))
  (lisp-unit2:assert-eql 0 (length (appenders *testlog*))))
