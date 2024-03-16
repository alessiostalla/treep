(in-package :treep-impl)

(defconstant +symbol-repl+      (import-lisp-symbol 'repl +symbol-treep+))
(defconstant +symbol-repl-quit+ (import-lisp-symbol 'quit +symbol-repl+))

;;TODO expose to the REPL as just "quit"
(define-abstraction quit +symbol-repl-quit+)

(defun repl (&key (evaluator (make-instance 'simple-evaluator)) (printer (make-instance 'printer)) (symbol-space +symbol-repl+))
  (let ((*symbol-space* symbol-space) (*environment* (copy-environment)))
    (cl:loop
     (let ((ss *symbol-space*) (*symbol-space* +root-symbol+))
       (print-symbol ss))
     (princ "> ")
     (force-output)
     (restart-case
	 (let ((form (read-form *standard-input* *environment*)))
	   (when (typep form 'quit) (return *environment*))
	   (transform printer (transform evaluator form *environment*) *environment*)
	   (terpri))
       (ignore-and-continue () :report "Ignore the error.")
       (quit () :report "Quit the Treep REPL." (return *environment*))))))
