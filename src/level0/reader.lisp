(in-package :treep-impl)

(define-condition premature-end-of-form (error) ())

(defun read-form (stream environment)
  (let ((ch (peek-char t stream)))
    (cond
      ((char= ch #\()
       (read-complex-form stream environment)) ;;TODO intern function...
      ((char= ch #\))
       (read-char stream)
       (error "Not inside a form")) ;;TODO more specific condition, better message
      ((digit-char-p ch)
       (let ((*readtable* (copy-readtable)))
	 (set-macro-character #\] (constantly #\]))
	 (cl:read stream)))
;;      ((and (or (char= ch #\+) (char= ch #\-) (char= ch #\.)) 
      (t (read-symbol stream)))))

(defgeneric read-form-instance (form stream environment))

(defmethod read-form-instance (form stream environment)
  (dolist (slot (remove-if (lambda (slot) (transient-slot? form slot))
			   (closer-mop:class-slots (class-of form))))
    (when (char= (peek-char t stream) #\))
      (return))
    (let ((subform (read-form stream environment))
	  (slot-name (closer-mop:slot-definition-name slot)))
      (setf (cl:slot-value form slot-name) subform)
      (init-subform form slot-name subform)))
  (unless (char= (peek-char t stream) #\))
    (error "No more subforms expected"))
  (read-char stream)
  form)

(defmethod read-form-instance ((form seq) stream environment)
  (let ((result (fset:seq)))
    (cl:loop
     (if (char= (peek-char t stream) #\))
	 (progn (read-char stream)
		(return))
	 (setf result (fset:with-last result (read-form stream environment)))))
    (setf (seq-elements form) result)
    form))

(defun read-complex-form (stream environment &optional (intern-function #'intern))
  (unless (char= (read-char stream) #\()
    (error "Not a complex form!"))
  (when (char= (peek-char t stream) #\))
    (error "Symbol required"))
  (let* ((class-name (read-symbol stream intern-function))
	 (class (or (meaning class-name +kind-class+ environment) (error "Abstraction ~A is unknown" class-name)))
	 (form (make-instance class)))
    (read-form-instance form stream environment)))
