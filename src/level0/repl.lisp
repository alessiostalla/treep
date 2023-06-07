(in-package :treep-impl)

(defconstant +symbol-repl+ (intern "repl" +symbol-treep+))
(defconstant +symbol-repl-quit+ (intern "quit" +symbol-repl+))

;;TODO expose to the REPL as just "quit"
(define-abstraction quit +symbol-repl-quit+)

(defun repl (&key (evaluator (make-instance 'simple-evaluator)))
  (cl:loop
   (format t "~A> " *symbol-space*)
   (force-output)
   (restart-case
       (let ((form (read-form *standard-input* *environment*)))
	 (when (typep form 'quit) (return))
	 (with-read-symbol-syntax ()
	   (print (transform evaluator form *environment*)))
	 (terpri))
     (ignore-and-continue () :report "Ignore the error.")
     (quit () :report "Quit the Treep REPL." (return)))))
