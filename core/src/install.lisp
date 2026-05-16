(in-package :treep)

(defun install (object place &key (allow-redefinition t))
  (install-element object place allow-redefinition))

(defgeneric install-element (object place allow-redefinition))
(defmethod install-element (object place allow-redefinition)
  (declare (ignore allow-redefinition))
  (error "Unsupported: install ~S into ~S" object place))

(defmethod install-element ((l language) (p package) allow-redefinition)
  (dolist (c (concepts l))
    (install-element c p allow-redefinition)))

(defmethod install-element ((c concept-definition) (p package) allow-redefinition)
  (let ((impl (ensure-concept-implementation c)))
    (install-element impl p allow-redefinition)))

(defmethod install-element ((c concept) (p package) allow-redefinition)
  (let* ((class-name (class-name c))
	 (existing (find-symbol (symbol-name class-name) p)))
    (if (and existing (not (eq existing class-name)))
	(if allow-redefinition
	    (setf (class-name c) existing)
	    (error "A symbol named ~S already exists in ~S" (symbol-name class-name) p))
	(import class-name p))
    (closer-mop:ensure-class-using-class c (class-name c)
					 :metaclass (class-of c)
					 :direct-superclasses (closer-mop:class-direct-superclasses c))))
    
