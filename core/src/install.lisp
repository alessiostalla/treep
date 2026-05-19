(in-package :treep)

(defun install (object &key (into *system*) (allow-redefinition t))
  (if (listp object)
      (dolist (element object)
	(install-element element into allow-redefinition))
      (install-element object into allow-redefinition)))

(defgeneric install-element (object place allow-redefinition))
(defmethod install-element (object place allow-redefinition)
  (declare (ignore allow-redefinition))
  (error "Unsupported: install ~S into ~S" object place))

;; Install into a system

(defmethod install-element ((l language) (s system) allow-redefinition)
  (let ((existing (find-language (language-name l) (system-languages s))))
    (when existing
      (if allow-redefinition
	  (setf (system-languages s) (remove existing (system-languages s))) ;; TODO migrate roots?
	  (error "A language named ~S already exists and redefinition is not allowed" (language-name l)))))
  (push l (system-languages s)))

(defmethod install-element (e (s system) allow-redefinition)
  (pushnew e (system-roots s))) ;; TODO honor allow-redefinition when forms adhere to an equality protocol

;; Install into a Lisp package

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

(defun install-into-package (lang &optional (package *package*) (allow-redefinition t))
  (install lang :into package :allow-redefinition allow-redefinition))
