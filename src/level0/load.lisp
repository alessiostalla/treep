(in-package :treep-impl)

(defun load (stream &key (evaluator (make-instance 'simple-evaluator)) (intern-function #'intern))
  (let ((*symbol-space* *symbol-space*)
	(environment (copy-environment))) ;So we don't side-effect it
    (cl:loop
     (unless (peek-char t stream nil nil)
       (return))
     (transform evaluator (read-form stream environment) environment))
    environment))

(defun load-file (file &key (evaluator (make-instance 'simple-evaluator)) (intern-function #'intern))
  (with-open-file (f file)
    (load f :evaluator evaluator :intern-function intern-function)))
