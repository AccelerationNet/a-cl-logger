(in-package :a-cl-logger)
(cl-interpol:enable-interpol-syntax)

(defvar *root-logger* nil
  "By default all loggers have the *root-logger* as a parent")
(defvar *logger-vars* ())
(defvar *message* nil)
(defvar *appender* nil)
(defvar *logger* nil)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +dribble+ 0)
  (defparameter +debug+   1)
  (defparameter +info+    2)
  (defparameter +warn+    3)
  (defparameter +error+   4)
  (defparameter +fatal+   5)
  (defparameter +muted+   6)
  (defparameter *max-logger-name-length* 12)

  (defparameter *log-level-names*
    #((dribble +dribble+ 0)
      (debug +debug+ 1)
      (info +info+ 2)
      (warn +warn+ 3)
      (error +error+ 4)
      (fatal +fatal+ 5)
      (muted +muted+ 6))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun replace-symbols-in-forms
      (plist forms
       &aux (tbl (alexandria:plist-hash-table plist)))
    " Processor for forms using with-macro-splicing "
    (labels ((find-replacement (in &aux (out (gethash in tbl)))
               (typecase out
                 (null)
                 (list (copy-list out))
                 (t out)))
             (rec (form)       
               (typecase form
                 (null)
                 (list
                  (let* (rtn)
                    ;; do head
                    (multiple-value-bind (v splice?)
                        (rec (car form))
                      (setf rtn (if splice? v (cons v nil))))
                    ;; do tail
                    (setf (cdr (last rtn)) (rec (cdr form)))
                    rtn))
                 (symbol
                  (let* ((list-splice? (char-equal #\@ (char (string form) 0))))
                    (alexandria:if-let (rep (find-replacement form))
                      (values rep list-splice?)
                      form)))
                 (t form))))
      (rec forms)))

  (defmacro with-macro-splicing ((&rest names) &body forms)
    "Double quasi-quoting hurts my head so lets do it a bit different

     Instead lets replace symbols in the expansion with values named by the
     same symbols in the expansion environment.

     Obviously care needs to be taken when processing forms passed by the
     user, but that is the name of the macro game.

     Variables starting with an @ (such as @body) are spliced as with
     `(progn ,@)

     In the interest of enhanced readability it is suggested that all template
     variables start with $ or @ so that they stand out
   
     eg:
     (let ((@b '(2 3))) (with-macro-splicing (($a 1) @b) (+ $a @b 4)))
     => (+ 1 2 3 4)
    "
    `(replace-symbols-in-forms
      (list ,@(iter (for n in names)
                (typecase n
                  (null)
                  (atom (appending `(',n ,n)))
                  (list (appending `(',(first n) ,(second n)))))
                ))
      ',@forms))

  (defmacro with-auto-unique-names ((&rest names) &body body)
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
    ,@(loop for $name in names
            for $macro-name = (intern #?"${$name}!")
            collect                 
               (with-macro-splicing ($name $macro-name)
                 (defmacro $macro-name (&rest places)
                   (let ((@places (loop for p in places
                                        collect p
                                        collect `($name ,p))))
                     (with-macro-splicing (@places)
                       (setf @places))))))))

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

;; def-forward-reference class
(unless (find-class 'json nil) (defclass json () ()))

;; TODO: these really seem like they should recurse
(defun as-json-array (list)
  (json:with-array ()
    (iter (for v in list)
      (json:as-array-member ()
        (typecase v
          (json (write-sequence (json v) json:*json-output*))
          (t (json:encode-json v)))))))

(defun me-or-ancestor (logger to-match)
  (or (eql logger to-match)
      (some #'(lambda (it) (me-or-ancestor it to-match))
            (parents logger))))

(defun as-json-object-member (k v formatter)
  (setf k (typecase k
            ((or symbol string) k)
            (t (princ-to-string k))))
  (json:as-object-member (k)
    (format-value v formatter)))

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
                  (or
                   (ignore-errors
                    (do-logging *root-logger*
                      (make-message *root-logger* +error+
                                    (list "Error in logger ~A:~%~A~%~S" ,logger c c))))
                   (ignore-errors
                    (format *error-output* "Error in logger ~A:~%~A~%~S"
                            ,logger c c)))
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
