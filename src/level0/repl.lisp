(in-package :treep-impl)

(defvar +symbol-repl+      (intern "repl" +symbol-treep+))
(defvar +symbol-repl-quit+ (intern "quit" +symbol-repl+))

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
