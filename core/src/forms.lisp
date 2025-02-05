(in-package :treep)

(defclass form-class (clutter:standard-class-with-attributes)
  ((definition :initform nil :accessor form-definition)))
(defclass form-slot-definition (clutter:-with-attributes)
  ((kind :initarg :kind :initform :attribute :accessor slot-kind)))
(defclass direct-form-slot-definition (clutter::direct-slot-definition-with-attributes form-slot-definition) ())
(defclass effective-form-slot-definition (clutter::effective-slot-definition-with-attributes form-slot-definition) ())

(defmethod closer-mop:validate-superclass ((class form-class) (superclass closer-mop:standard-class))
  t)
(defmethod closer-mop:validate-superclass ((class closer-mop:standard-class) (superclass form-class))
  t)

(defmethod closer-mop:direct-slot-definition-class ((class form-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'direct-form-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class form-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'effective-form-slot-definition))

(defstruct container form slot)

(defclass form (clutter:-with-attributes)
  ((container :accessor form-container :initform nil :kind :internal))
  (:metaclass form-class))

(defclass namespace () ())

(defgeneric ns-lookup (name namespace))
(defgeneric ns-entries (namespace))

(defmethod ns-lookup (name (namespace namespace))
  (error "Don't know how to look up ~S in ~S" name namespace))

(defmethod ns-entries ((namespace namespace))
  (error "Namespace ~S does not support listing entries"))

(defclass simple-namespace (namespace)
  ((contents :initform (make-hash-table))))

(defmethod ns-lookup ((name string) (namespace simple-namespace))
  (gethash name (slot-value namespace 'contents)))

(defclass language (form)
  ((definitions :reader definitions :kind :namespace :initform (make-instance 'simple-namespace))))

(defclass slots-namespace (namespace)
  ((slots :initform (list))))

(defmethod ns-lookup ((name string) (namespace slots-namespace))
  (find name (slot-value namespace 'slots) :key #'car :test #'string=))

(defmethod ns-lookup ((name symbol) (namespace slots-namespace))
  (let* ((sn (symbol-name name))
	 (name (if (string= (string-upcase sn) sn)
		   (string-downcase sn)
		   sn)))
  (find name (slot-value namespace 'slots) :key #'car :test #'string=)))

(defclass form-definition (form)
  ((slots :accessor form-slots :kind :namespace :initform (make-instance 'slots-namespace))))

(defun compute-form-definition (class)
  (make-instance 'form-definition))

(defun lookup-form (object name &optional namespace)
  (let ((ns (cond
	      ((typep namespace 'closer-mop:slot-definition)
	       (if (and (typep namespace 'form-slot-definition) (eq (slot-kind namespace) :namespace))
		   namespace
		   (error "Not a namespace slot: ~A" namespace)))
	      ((and (typep namespace 'symbol) (not (null namespace)))
	       (or (find-if (lambda (slot)
			      (and
			       (typep slot 'form-slot-definition)
			       (eq (closer-mop:slot-definition-name slot) namespace)
			       (eq (slot-kind slot) :namespace)))
			    (closer-mop:class-slots (class-of object)))
		   (error "Not a namespace slot: ~A" namespace)))
	      ((null namespace)
	       (let ((candidates (remove-if-not (lambda (slot)
						  (and
						   (typep slot 'form-slot-definition)
						   (eq (slot-kind slot) :namespace)))
						(closer-mop:class-slots (class-of object)))))
		 (if (car candidates)
		     (if (cdr candidates)
			 (error "Multiple candidate namespaces in ~A: ~A" (class-of object) candidates)
			 (car candidates))
		     (error "No candidate namespaces in ~A" (class-of object)))))
	      (t (error "Not a namespace designator: ~S" namespace)))))
    (ns-lookup name (slot-value object (closer-mop:slot-definition-name ns)))))

(loop
   :for class :in '(form language)
   :do (let ((class (find-class class)))
	 (setf (form-definition class)
	       (compute-form-definition class))))
