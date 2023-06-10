(in-package :treep-impl)

(defun load (stream &key (evaluator (make-instance 'simple-evaluator)) (intern-function #'intern))
  (let ((*symbol-space* *symbol-space*)
	(environment (copy-environment)) ;So we don't side-effect it
	(result nil))
    (cl:loop
     (unless (peek-char t stream nil nil)
       (return))
     (multiple-value-setq (result environment) (transform evaluator (read-form stream environment) environment)))
    (values environment result)))

(defun load-file (file &key (evaluator (make-instance 'simple-evaluator)) (intern-function #'intern))
  (with-open-file (f file)
    (load f :evaluator evaluator :intern-function intern-function)))
