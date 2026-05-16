(in-package :treep)

(defclass concept (clutter:standard-class-with-attributes)
  ((definition :initform nil :initarg :definition :accessor concept-definition)))
(defclass concept-slot-definition (clutter:-with-attributes)
  ((name :initarg :feature-name :initform nil :accessor feature-name)
   (kind :initarg :kind :initform :attribute :accessor feature-kind)
   (multiplicity :initarg :multiplicity :initform 1 :accessor feature-multiplicity)
   (definition :initarg :definition :reader feature-definition)))
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
      (setf (feature-multiplicity result) (feature-multiplicity most-specific))
      (when (slot-boundp most-specific 'definition)
	(setf (slot-value result 'definition) (feature-definition most-specific))))
    result))

(defstruct container form slot)

(defclass form (clutter:-with-attributes)
  ((container :accessor form-container :initform nil :kind :internal :feature-name "container"))
  (:metaclass concept))

(deftype reference-to (type &key (by 'string)) (list 'or by type))

(defclass language (form)
  ((name :initarg :name :accessor language-name :feature-name "name" :kind :attribute)
   (concepts :reader concepts :initform (list) :kind :containment :feature-name "concepts")
   (concepts-map :reader concepts-map :initform (make-hash-table :test #'equal) :kind :internal :feature-name "concepts-map")
   (used-languages :accessor used-languages :initarg :used-languages :initform nil :kind :attribute :feature-name "used-languages"))
  (:metaclass concept))

(defclass concept-definition (form)
  ((name :initarg :name :accessor concept-name :feature-name "name" :kind :attribute)
   (features :reader features :initform (list) :kind :containment :feature-name "features")
   ;; Used by the reader to instantiate the concept
   (implementation :accessor concept-implementation :initarg :implementation :initform nil :kind :internal :feature-name "implementation"))
  (:metaclass concept))

(defun ensure-concept-definition (concept)
  (or (concept-definition concept) (error "The definition of concept ~A in unknown" concept)))

(defmethod concept-name ((c concept))
  (concept-name (ensure-concept-definition c)))

(defclass feature (form)
  ((name :initarg :name :accessor feature-name :feature-name "name" :kind :attribute)
   (multiplicity :initarg :multiplicity :accessor feature-multiplicity :initform 1 :feature-name "multiplicity" :kind :attribute)
   (kind :initarg :kind :initform :attribute :accessor feature-kind :feature-name "kind" :kind :attribute))
  (:metaclass concept))

(defvar *treep* (make-instance 'language :name "treep"))
(defvar *language* (make-instance 'language :name "default" :used-languages (list *treep*)))

(defmethod find-concept ((name string) (language language))
  (gethash name (concepts-map language)))

;; TODO generate these 
(setf (concept-definition (find-class 'concept-definition)) (make-instance 'concept-definition :name "concept" :implementation (find-class 'concept-definition)))
(setf (concept-definition (find-class 'feature)) (make-instance 'concept-definition :name "feature" :implementation (find-class 'feature)))
(setf (concept-definition (find-class 'language)) (make-instance 'concept-definition :name "define-language" :implementation (find-class 'language)))

(defun add-concept (concept language)
  (when (symbolp concept)
    (setf concept (or (find-class concept) (error "~S does not name a concept" concept))))
  (setf concept (ensure-concept-definition concept))
  (push concept (slot-value language 'concepts))
  (setf (gethash (concept-name concept) (concepts-map language)) concept))

(add-concept 'concept-definition *treep*)
(add-concept 'feature *treep*)
(add-concept 'language *treep*)

(defun lookup-concept (name &optional (language *language*))
  (when (stringp name)
    (setf name (list name)))
  (if name
      (let ((language
	     (if (cadr name)
		 (or
		  (when (string= (string-upcase (language-name language)) (string-upcase (car name)))
		    language)
		  (find (string-upcase (car name)) (used-languages language)
			:key (lambda (lang) (string-upcase (language-name lang)))
			:test #'string=)
		  (error "No language named ~S found in ~S" (car name) language))
		 language)))
	(if (cddr name)
	    (error "Not a valid concept path: ~S" name)
	    (let ((concept-name (or (cadr name) (car name))))
	      (or
	       (find-concept concept-name language)
	       (dolist (language (used-languages language))
		 (let ((concept (find-concept concept-name language)))
		   (when concept
		     (return concept))))))))
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
  (let ((the-feature
	 (typecase feature
	   (concept-slot-definition feature)
	   (symbol
	    (find feature
		  (closer-mop:class-slots (class-of form))
		  :key #'closer-mop:slot-definition-name))
	   ((or string list) (lookup-feature feature form))
	   (t (error "Not a valid feature designator: ~S" feature)))))
    (unless the-feature
      (error "Unknown feature ~S in ~S" feature form))
    ;; TODO check multiplicity
    (setf (slot-value form (closer-mop:slot-definition-name the-feature)) value)
    (when (eq (feature-kind the-feature) :containment)
      (labels ((set-parent (f)
		 (typecase f
		   (form (setf (form-container form) (make-container :form form :slot the-feature)))
		   (list (map nil #'set-parent f)))))
	(set-parent value)))
    value))

(defun ensure-concept-implementation (concept)
  (or (concept-implementation concept)
      (setf (concept-implementation concept) (implement-concept concept))))

(defgeneric implement-concept (concept))
(defmethod implement-concept ((concept concept-definition))
  (let ((slots (mapcar (lambda (f) (implement-feature f concept))
		       (features concept))))
    (make-instance 'concept
		 :name (make-symbol (concept-name concept))
		 :definition concept
		 :direct-superclasses (list (find-class 'form)) ;; TODO inheritance
		 :direct-slots slots)
    ;; TODO accessors for features
    ))

(defgeneric implement-feature (feature concept))
(defmethod implement-feature ((feature feature) (concept concept-definition))
  (list
   :name (make-symbol (feature-name feature))
   :feature-name (feature-name feature)
   :class concept
   :definition feature
   :multiplicity (feature-multiplicity feature)
   :kind (feature-kind feature)))

(defmethod print-object ((object form) stream)
  (let ((concept (class-of object)))
    (if (typep concept 'concept)
	(print-unreadable-object (object stream :type nil :identity t)
	  (let ((def (concept-definition object)))
	    (if def
		(princ (concept-name object) stream)
		(princ (class-name concept) stream))))
	(call-next-method))))

(defmethod print-object ((object concept) stream)
  (let ((def (concept-definition object)))
    (if def
	(print-object def stream)
	(call-next-method))))

(defmethod print-object ((object concept-definition) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (princ "Concept " stream)
    (prin1 (concept-name object) stream)))
