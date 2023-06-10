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
       (cl:read stream))
;;      ((and (or (char= ch #\+) (char= ch #\-) (char= ch #\.)) 
      (t (read-symbol stream)))))

(defun whitespace? (ch)
  (or (char= ch #\Space) (char= ch #\Tab) (char= ch #\Newline)))

(defun consume-whitespace (stream)
  (cl:loop :while (whitespace? (peek-char t stream)) :do (read-char stream)))

(defgeneric read-form-instance (form stream environment))

(defmethod read-form-instance (form stream environment)
  (let ((slots (remove-if (lambda (slot) (transient-slot? form slot))
			  (closer-mop:class-slots (class-of form)))))
    (cl:loop
     :while slots
     :do (progn
	   (consume-whitespace stream)
	   (when (char= (peek-char t stream) #\))
	     (return))
	   (let* ((subform (read-form stream environment))
		  (slot (if (typep subform 'named-child)
			    (or (find (named-child-name subform) slots :key #'slot-name)
				(error "No slot named ~A in ~A" (named-child-name subform) (class-of form)))
			    (car slots)))
		  (slot-name (closer-mop:slot-definition-name slot))
		  (value (if (typep subform 'named-child) (named-child-value subform) subform)))
	     (setf (cl:slot-value form slot-name) value)
	     (init-subform form slot-name value)
	     (setf slots (remove slot slots))))))
  (consume-whitespace stream)
  (if (char= (peek-char t stream) #\))
      (read-char stream)
      (error "No more child nodes expected in ~A" form))
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
