(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun splice-in-local-symbol-values (plist forms
                                        &aux (head (first forms)))
    "Double quasi-quoting hurts my head so lets do it a bit different"
    (when forms
      (cons
       (typecase head
         (list (splice-in-local-symbol-values plist head))
         (symbol
          (or (iter (for (k v) on plist by #'cddr)
                (if (eql head k) (return v)))
              head))
         (t head))
       (splice-in-local-symbol-values plist (rest forms))))))

(defmacro with-spliced-unique-name ((&rest names) &body body)
  `(alexandria:with-unique-names (,@names)
    (splice-in-local-symbol-values
     (list ,@(iter (for n in names) (appending `(',n ,n))))
     ,@body)))

(defun without-earmuffs (symbol)
  (check-type symbol symbol)
  (let* ((n (symbol-name symbol))
         (l (length n)))
    (symbol-munger:reintern
     (subseq n 1 (- l 1))
     (symbol-package symbol))))

(defmacro define-mutator-macro (%name)
  "defines mutator macros for a function name
    eg: ensure-account-id =>
      (defmacro ensure-account-id! (&rest places) ... )
      which (setf place (name place) for each place)

   %name is just so we dont accidentally run into someone
   using name accidentally
  "
  (let* ((macro-name (intern #?"${%name}!")))
    (splice-in-local-symbol-values
     (list '%name %name)
     `(defmacro ,macro-name (&rest places)
       `(setf ,@(iter (for p in places)
                  (collect p)
                  (collect `(%name ,p))))))))

(defmacro define-mutator-macros (&rest names)
  "creates many mutator macros for names"
  `(progn
    ,@(iter (for n in names)
        (collect `(define-mutator-macro ,n)))))

(defmacro maybe-with-presentations
    ((output-stream var &rest stream-properties) &body body)
  "Buffer the output to var, printing as if to a swank dedicated
presentation stream, dumping to a freshline on the output-stream when
done."
  (let ((os-name (gensym "output-stream"))
        (msg-name (gensym "msg")))
    `(let* ((,os-name ,output-stream)
            (,msg-name
             (with-output-to-string (,var ,@stream-properties)               
               (if #+swank
                   (and (boundp 'swank::*dedicated-presentation-streams*)
                        (ignore-errors
                         (eql :dedicated (swank::slime-stream-p ,os-name))))
                   #-swank nil
                   (let ((swank::*dedicated-presentation-streams*
                           (cons ,var swank::*dedicated-presentation-streams*)))
                     ,@body)
                   (progn ,@body)))))
      ;; we want the following two lines to be close (more likely run
      ;; together without actually locking) so do the computation
      ;; above.
      (fresh-line ,os-name)
      (write-sequence ,msg-name ,os-name))))

(define-mutator-macros ensure-list get-logger require-logger ensure-level-value)

(defmacro with-logger-level (logger-name new-level &body body)
  "Set the level of the listed logger(s) to NEW-LEVEL and restore the original value in an unwind-protect."
  (cond
    ((null logger-name) `(progn ,@body))
    ((consp logger-name)
     `(with-logger-level ,(first logger-name) ,new-level
       `(with-logger-level ,(rest logger-name) ,new-level
         ,@body)))
    ((symbolp logger-name)
     (alexandria:with-unique-names (logger old-level)
       `(let* ((,logger (get-logger ',logger-name))
               (,old-level (level ,logger)))
         (setf (level ,logger) ,new-level)
         (unwind-protect
              (progn ,@body)
           (setf (level ,logger) ,old-level)))))
    (t (error "Don't know how to interpret ~S as a logger name" logger-name))))

(defmacro with-logging-io (() &body body)
  `(let ((*print-right-margin* most-positive-fixnum)
         (*print-readably* nil)
         (*print-length* 64)
         (*package* #+ecl (find-package "COMMON-LISP")
                    #-ecl #.(find-package "COMMON-LISP")))
    ,@body))

(defun format-time (&key stream (time (get-universal-time))
                    (format :iso))  
  (case format
    (:iso (format stream "~A" (local-time:now)))
    (t
     (multiple-value-bind (second minute hour day month year)
         (decode-universal-time time)
       (ecase format
         (:stamp (format stream "~d~2,'0D~2,'0D ~2,'0D~2,'0D~2,'0D"
                         year month day hour minute second))
         (:time (format stream "~2,'0D:~2,'0D:~2,'0D"
                        hour minute second)))))))

(defun as-json-o-val (k v)
  (json:encode-object-member
   (typecase k
     ((or symbol string) k)
     (t (princ-to-string k)))
   (princ-to-string v)))

(defun only-one? (thing)
  (typecase thing
    (atom thing)
    (list (if (= 1 (length thing))
              (first thing)
              nil))))

(defmacro with-debugging-or-error-printing
    ((logger
      &key (continue "Run the next one"))
     &body body)
  `(with-simple-restart (continue ,continue)
    (handler-bind
        ((error (lambda (c)
                  (ignore-errors
                   (format *error-output* "Error in logger ~A:~%~A~%~S"
                           ,logger c c))
                  (when *debugger-hook*
                    (invoke-debugger c))
                  (continue c)))))
    ,@body
    ))

(defmacro push-plist (key val place)
  `(progn (push ,val ,place)
    (push ,key ,place)))

(defun class-name-of (o)
  (typecase o
    (null nil)
    (symbol (when (ignore-errors (find-class o))
              o))
    (standard-class (class-name o))
    (standard-object (class-name (class-of o)))))
