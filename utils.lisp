(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)



(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %with-macro-splicing (plist forms
                               &aux (tbl (alexandria:plist-hash-table plist)))
    "Processor for forms using with-macro-splicing"
    (when forms
      (labels ((find-replacement (in)
                 (gethash in tbl))
               (rec (forms)
                 (iter (for form in forms)
                   (typecase form
                     (list (collect (rec form)))
                     (symbol
                      (let* ((list-splice? (char-equal #\@ (char (string form) 0))))
                        (alexandria:if-let (rep (find-replacement form))
                          (if list-splice?
                              (appending rep)
                              (collect rep))
                          (collect form))))
                     (t (collect form))))))
        (rec forms))))

  (defmacro with-macro-splicing ((&rest names) &body forms)
    "Double quasi-quoting hurts my head so lets do it a bit different

     Instead lets replace symbols in the expansion with values named by the
     same symbols in the expansion environment.

     Obviously care needs to be taken when processing forms passed by the
     user, but that is the name of the macro game.

     Variables starting with an @ (such as @body) are spliced as with
     `(progn ,@)
   
     eg:
     (let ((a 1))
       (with-macro-splicing (a) (+ a 2)))
     => (+ 1 2)
    "
    `(%with-macro-splicing
      (list ,@(iter (for n in names)
                (appending `(',n ,n))))
      ',@forms))

  (defmacro with-spliced-unique-name ((&rest names) &body body)
  `(alexandria:with-unique-names (,@names)
    (with-macro-splicing (,@names)
     ,@body))))

(defun without-earmuffs (symbol)
  (check-type symbol symbol)
  (let* ((n (symbol-name symbol))
         (l (length n)))
    (symbol-munger:reintern
     (subseq n 1 (- l 1))
     (symbol-package symbol))))

(defmacro define-mutator-macros (&rest names)
  "defines mutator macros for a function name
    eg: ensure-account-id =>
      (defmacro ensure-account-id! (&rest places) ... )
      which (setf place (name place) for each place)

   %name is just so we dont accidentally run into someone
   using name accidentally
  "  
  `(progn
    ,@(loop for %name in names
            for macro-name = (intern #?"${%name}!")
            collect                 
               (with-macro-splicing (%name macro-name)
                 (defmacro macro-name (&rest places)
                   (let ((@places (loop for p in places
                                        collect p
                                        collect `(%name ,p))))
                     (with-macro-splicing (@places)
                       (setf @places))))))))
#|
(defmacro define-mutator-macros (&rest names)
  "defines mutator macros for a function name
    eg: ensure-account-id =>
      (defmacro ensure-account-id! (&rest places) ... )
      which (setf place (name place) for each place)

   %name is just so we dont accidentally run into someone
   using name accidentally
  "  
  `(progn
    ,@(loop for %name in names
            for macro-name = (intern #?"${%name}!")
            for setf-form = ``(,',%name ,p)
            collect                             
               `(defmacro ,macro-name (&rest places)
                 (let ((setf-places (loop for p in places
                                          collect p
                                          collect ,setf-form)))
                   `(setf ,@setf-places))))))
|#

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

(define-mutator-macros
    ensure-list get-logger require-logger ensure-level-value ensure-message)

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

(defmacro push-m-plist (plist
                        place)
  (alexandria:with-unique-names (pl)
    `(let ((,pl (copy-list ,plist)))
      (setf (cdr (last ,pl)) (data-plist ,place))
      (setf (data-plist ,place) ,pl))))

(defun class-name-of (o)
  (typecase o
    (null nil)
    (symbol (when (ignore-errors (find-class o))
              o))
    (standard-class (class-name o))
    (standard-object (class-name (class-of o)))))

(defun ensure-message (it)
  (etypecase it
    (message it)
    (function (funcall it))))
