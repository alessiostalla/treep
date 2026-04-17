(in-package :treep)

(defclass concept (clutter:standard-class-with-attributes)
  ((definition :initform nil :accessor concept-definition)))
(defclass concept-slot-definition (clutter:-with-attributes)
  ((name :initarg :feature-name :initform nil :accessor feature-name)
   (kind :initarg :kind :initform :attribute :accessor feature-kind)
   (multiplicity :initarg :multiplicity :initform 1 :accessor feature-multiplicity)))
(defclass direct-concept-slot-definition (clutter::direct-slot-definition-with-attributes concept-slot-definition) ())
(defclass effective-concept-slot-definition (clutter::effective-slot-definition-with-attributes concept-slot-definition) ())

(defmethod closer-mop:validate-superclass ((class concept) (superclass closer-mop:standard-class))
  t)
(defmethod closer-mop:validate-superclass ((class closer-mop:standard-class) (superclass concept))
  t)

(defmethod closer-mop:direct-slot-definition-class ((class concept) &rest initargs)
  (declare (ignore initargs))
  (find-class 'direct-concept-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class concept) &rest initargs)
  (declare (ignore initargs))
  (find-class 'effective-concept-slot-definition))

(defmethod closer-mop:compute-effective-slot-definition ((class concept) name direct-slot-definitions)
  (declare (ignorable class name))
  (let ((result (call-next-method))
	(most-specific (car direct-slot-definitions)))
    (when (typep most-specific 'concept-slot-definition)
      (setf (feature-name result) (feature-name most-specific))
      (setf (feature-kind result) (feature-kind most-specific))
      (setf (feature-multiplicity result) (feature-multiplicity most-specific)))
    result))

(defstruct container form slot)

(defclass form (clutter:-with-attributes)
  ((container :accessor form-container :initform nil :kind :internal :feature-name "container"))
  (:metaclass concept))

(deftype reference-to (type &key (by 'string)) (list 'or by type))

(defclass language (form)
  ((name :initarg :name :accessor language-name :feature-name "name" :kind :attribute)
   (concepts :reader concepts :initform (make-hash-table :test #'equal) :kind :containment :feature-name "concepts")
   (used-languages :accessor used-languages :initarg :used-languages :initform nil :kind :internal :feature-name "used-languages"))
  (:metaclass concept))

(defclass concept-definition (form)
  ((name :initarg :name :accessor concept-name :feature-name "name" :kind :attribute)
   (implementation :reader concept-implementation :initarg :implementation :initform nil :kind :internal :feature-name "implementation"))
  (:metaclass concept))

(defvar *treep* (make-instance 'language :name "treep"))
(defvar *language* (make-instance 'language :name "default" :used-languages (list *treep*)))

(defmethod find-concept ((name string) (language language))
  (gethash name (concepts language)))

(defmethod (setf find-concept) ((concept concept-definition) (name string) (language language))
  (setf (gethash name (concepts language)) concept))
(defmethod (setf find-concept) ((concept concept) (name string) (language language))
  (setf (find-concept name language)
	(or (concept-definition concept) (error "Concept ~S doesn't have a definition" concept))))

(setf (concept-definition (find-class 'concept-definition)) (make-instance 'concept-definition :name "concept-definition" :implementation (find-class 'concept-definition)))
(setf (concept-definition (find-class 'language)) (make-instance 'concept-definition :name "language" :implementation (find-class 'language)))

(setf (find-concept "concept" *treep*) (find-class 'concept-definition))
(setf (find-concept "language" *treep*) (find-class 'language))

(defun lookup-concept (name &optional (language *language*))
  (if name
      (let ((language
	     (if (cadr name)
		 (or
		  (find (string-upcase (car name)) (used-languages language)
			:key (lambda (lang) (string-upcase (language-name lang)))
			 :test #'string=)
		  (error "No language named ~S found in ~S" (car name) language))
		 language)))
	(if (cddr name)
	    (error "Not a valid concept path: ~S" name)
	    (find-concept (or (cadr name) (car name)) language)))
      nil))

(defgeneric lookup-feature (name object)
  (:documentation "Looks up a feature by name"))

(defmethod lookup-feature (name object)
  (lookup-feature name (class-of object)))

(defmethod lookup-feature ((name list) (concept concept))
  (if (cdr name)
      (error "Qualified feature name not supported yet: ~S" name)
      (lookup-feature (car name) concept)))

(defmethod lookup-feature (name (class class))
  (declare (ignore name))
  (error "Not a concept: ~S" class))

(defmethod lookup-feature ((name string) (concept concept))
  (find name (reverse (remove-if-not (lambda (feature)
				       (and (typep feature 'concept-slot-definition)
					    (not (null (feature-name feature)))))
				     (closer-mop:class-slots concept)))
	:key #'feature-name
	:test #'string=))

(defun set-feature (form feature value)
  (let ((feature (typecase feature
		   (concept-slot-definition feature)
		   (symbol
		    (find feature
			  (closer-mop:class-slots (class-of form))
			  :key #'closer-mop:slot-definition-name))
		   ((or string list) (lookup-feature feature form))
		   (t (error "Not a valid feature designator: ~S" feature)))))
    (unless feature
      (error "Unknown feature ~S in ~S" feature form))
    ;; TODO check multiplicity
    (setf (slot-value form (closer-mop:slot-definition-name feature)) value)
    (when (eq (feature-kind feature) :containment)
      (labels ((set-parent (f)
		 (typecase f
		   (form (setf (form-container form) (make-container :form form :slot feature)))
		   (list (map nil #'set-parent f)))))
	(set-parent value)))
    value))
