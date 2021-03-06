(in-package :treep-impl)

(define-condition premature-end-of-form (error) ())

(defun read-form (stream environment)
  (let ((ch (peek-char t stream)))
    (cond
      ((char= ch #\()
       (read-complex-form stream environment)) ;;TODO intern function...
      ((char= ch #\))
       (read-char stream)
       (error "Not inside a form"))
      ((char= ch #\[)
       (read-seq stream environment)) ;;TODO intern function...
      ((char= ch #\])
       (read-char stream)
       (error "Not inside a seq"))
      ((digit-char-p ch)
       (let ((*readtable* (copy-readtable)))
	 (set-macro-character #\] (constantly #\]))
	 (cl:read stream)))
;;      ((and (or (char= ch #\+) (char= ch #\-) (char= ch #\.)) 
      (t (read-symbol stream)))))

(defun read-seq (stream environment)
  (unless (char= (read-char stream) #\[)
    (error "Not a seq!"))
  (let ((result (fset:seq)))
    (cl:loop
     (if (char= (peek-char t stream) #\])
	 (progn (read-char stream)
		(return))
	 (setf result (fset:with-last result (read-form stream environment)))))
    result))

(defun read-complex-form (stream environment &optional (intern-function #'intern))
  (unless (char= (read-char stream) #\()
    (error "Not a complex form!"))
  (when (char= (peek-char t stream) #\))
    (error "Symbol required"))
  (let* ((class-name (read-symbol stream intern-function))
	 (class (or (meaning class-name +kind-class+ environment) (error "No class named ~A" class-name)))
	 (form (make-instance class)))
    (dolist (slot (remove-if (lambda (x) (eq 'parent (closer-mop:slot-definition-name x)))
			     (closer-mop:class-slots class)))
      (when (char= (peek-char t stream) #\))
	(return))
      (let ((subform (read-form stream environment))
	    (slot-name (closer-mop:slot-definition-name slot)))
	(setf (cl:slot-value form slot-name) subform)
	(init-subform form slot-name subform)))
    (unless (char= (peek-char t stream) #\))
      (error "No more subforms expected"))
    (read-char stream)
    form))

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
