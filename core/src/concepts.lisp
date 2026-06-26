(in-package :treep)

(defclass concept (clutter:standard-class-with-attributes)
  ((definition :initform nil :initarg :definition :accessor concept-definition)))

(defclass concept-slot-definition (clutter:-with-attributes)
  ((name :initarg :feature-name :initform nil :accessor feature-name)
   (kind :initarg :kind :initform :attribute :accessor feature-kind)
   (multiplicity :initarg :multiplicity :initform 1 :accessor feature-multiplicity)
   (computed :initarg :computed :initform nil :accessor feature-computed :documentation "If non-nil, it indicates a feature that is computed at runtime, and not saved to persistent storage")
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
      (setf (feature-computed result) (feature-computed most-specific))
      (setf (feature-multiplicity result) (feature-multiplicity most-specific))
      (when (slot-boundp most-specific 'definition)
	(setf (slot-value result 'definition) (feature-definition most-specific))))
    result))

(defstruct container form slot)

(defclass form (clutter:-with-attributes)
  ((container :accessor form-container :initform nil :kind :internal :feature-name "container"))
  (:metaclass concept))

(defclass named (form)
  ((name :initarg :name :accessor name-of :feature-name "name" :kind :attribute :type string))
  (:metaclass concept))

(defstruct name (key context))

(defgeneric qualified-name-of (form))

(defmethod qualified-name-of ((n named))
  (let* ((container (form-container n))
	 (container-form (when container (container-form container)))
	 (context (when container-form (ignore-errors (qualified-name container-form)))))
    (make-name :context context :key (name-of n))))

(defclass concept-definition (named)
  ((features :reader features :initarg :features :initform (list) :kind :containment :multiplicity (0) :feature-name "features")
   (superconcepts :reader concept-superconcepts  :initarg :superconcepts :initform (list) :kind :reference :multiplicity (0) :feature-name "superconcepts")
   ;; Used by the reader to instantiate the concept
   (implementation :accessor concept-implementation :initarg :implementation :initform nil :kind :internal :feature-name "implementation"))
  (:metaclass concept))

(defun ensure-concept-definition (concept)
  (or (concept-definition concept) (error "The definition of concept ~A in unknown" concept)))

(defmethod concept-name ((c concept))
  (name-of (ensure-concept-definition c)))

(defmethod concept-name ((c concept-definition))
  (name-of c))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun slot-descriptor->feature (descr)
    (declare (ignore descr))
    nil)) ;; TODO

(defmacro defconcept (name superclasses slots &rest options)
  (destructuring-bind (name &optional (concept-name (string-downcase (symbol-name name))))
      (if (listp name) name (list name))
    (let* ((language-option (find :language options :key #'car))
	   (options (remove language-option options)))
      `(progn
	 (defclass ,name ,(or superclasses '(form)) ,slots ,@options (:metaclass concept))
	 (setf (concept-definition (find-class ',name))
	       (make-instance 'concept-definition
			      :name ,concept-name
			      :features ,(remove nil (mapcar #'slot-descriptor->feature slots))
			      :implementation (find-class ',name)))
	 ,@(when language-option
	     `((add-concept ',name ,(cadr language-option))))))))

(defconcept (language "define-language") (named)
  ((concepts :reader concepts :initform (list) :kind :containment :multiplicity (0) :feature-name "concepts")
   (concepts-map :reader concepts-map :initform (make-hash-table :test #'equal) :kind :internal :feature-name "concepts-map")
   (used-languages :accessor used-languages :initarg :used-languages :initform nil :kind :attribute :feature-name "used-languages")))

(defun language-name (lang)
  (name-of lang))

(defvar *treep* (make-instance 'language :name "treep"))
(defvar *language* (make-instance 'language :name "default" :used-languages (list *treep*)))

(defun find-concept (name language)
  (or (gethash name (concepts-map language))
      (dolist (language (used-languages language))
	(let ((concept (find-concept name language)))
	  (when concept
	    (return concept))))))

(defun add-concept (concept language)
  (when (symbolp concept)
    (setf concept (or (find-class concept) (error "~S does not name a concept" concept))))
  (setf concept (ensure-concept-definition concept))
  (push concept (slot-value language 'concepts))
  (setf (gethash (concept-name concept) (concepts-map language)) concept)
  (setf (form-container concept) (make-container :form language :slot 'concepts))
  concept)

(defun concept-language (concept)
  (when (form-container concept)
    (container-form (form-container concept))))

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
	      (find-concept concept-name language))))
      nil))

;; Features

(defconcept multiplicity ()
  ((min :initarg :min :accessor multiplicity-min :feature-name "min" :kind :attribute :initform 0)
   (max :initarg :max :accessor multiplicity-max :feature-name "max" :kind :attribute :initform nil))
  (:language *treep*))
  
(defconcept feature (named)
  ((multiplicity :initarg :multiplicity :accessor feature-multiplicity :initform (make-instance 'multiplicity :min 1 :max 1) :feature-name "multiplicity" :kind :containment)
   (computed :initarg :computed :accessor feature-computed :initform nil :feature-name "computed" :kind :attribute)
   (slot-name :accessor feature-slot-name :initarg :slot-name :initform nil :kind :internal :feature-name "slot-name"))
  (:language *treep*))

(defmethod feature-name ((f feature))
  (name-of f))

(defconcept ref ()
  ((key :initarg :key :accessor ref-key :kind :attribute)
   (target :initarg :target :accessor ref-target :initform nil :kind :internal))
  (:language *treep*))

(defun resolve-ref (ref resolve-function)
  (let ((target (ref-target ref)))
    (unless target
      (setf target (funcall resolve-function ref))
      (setf (ref-target ref) target))
    target))

(defconcept attribute (feature) () (:language *treep*))
(defconcept containment (feature) () (:language *treep*))
(defconcept reference (feature) () (:language *treep*))

(defmethod feature-kind ((feature feature))
  (error "No kind defined for feature of type ~A: ~S (~S)" (class-of feature) (feature-name feature) feature))

(defmethod feature-kind ((feature attribute))
  :attribute)

(defmethod feature-kind ((feature containment))
  :containment)

(defmethod feature-kind ((feature reference))
  :reference)

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

(defun containment? (feature)
  (eq (feature-kind feature) :containment))

(defun reference? (feature)
  (eq (feature-kind feature) :reference))

(defun resolve-feature (feature form)
  (typecase feature
    (concept-slot-definition feature)
    (symbol
     (find feature
	   (closer-mop:class-slots (class-of form))
	   :key #'closer-mop:slot-definition-name))
    ((or string list) (lookup-feature feature form))
    (feature (lookup-feature (feature-name feature) form))
    (t (error "Not a valid feature designator: ~S" feature))))

(defun get-feature (form feature)
  (let ((the-feature (resolve-feature feature form)))
    (unless the-feature
      (error "Unknown feature ~S in ~S" feature form))
    ;; TODO handle unbound slot
    (values
     (slot-value form (closer-mop:slot-definition-name the-feature))
     t)))

(defun set-feature (form feature value)
  (let ((the-feature (resolve-feature feature form)))
    (unless the-feature
      (error "Unknown feature ~S in ~S" feature form))
    ;; TODO check multiplicity
    (setf (slot-value form (closer-mop:slot-definition-name the-feature)) value)
    (when (containment? the-feature)
      (labels ((set-parent (f)
		 (typecase f
		   (form (setf (form-container f) (make-container :form form :slot the-feature)))
		   (list (map nil #'set-parent f)))))
	(set-parent value)))
    value))

(defun ensure-concept-implementation (concept)
  (or (concept-implementation concept)
      (setf (concept-implementation concept) (implement-concept concept))))

(defgeneric implement-concept (concept))
(defmethod implement-concept ((concept concept-definition))
  (let ((superconcepts
	 (mapcar (lambda (s)
		   (ensure-concept-implementation
		    (resolve-ref s (lambda (ref)
				     (or
				      (lookup-concept (ref-key ref)
						      (or (concept-language concept)
							  (error "Cannot resolve superconcept ~S for ~S because the derived concept has no language"
								 (ref-key ref)
								 concept)))
				      (error "Unknown concept ~S" (ref-key ref)))))))
		 (concept-superconcepts concept))))
    (make-instance 'concept
		 :name (make-symbol (concept-name concept))
		 :definition concept
		 :direct-superclasses (or superconcepts (list (find-class 'form)))
		 :direct-slots (mapcar (lambda (f) (implement-feature f concept)) (features concept)))
    ;; TODO accessors for features
    ))

(defun find-feature-in-hierarchy (feature-name concepts)
  (let (seen (to-process (remove nil (mapcar #'ref-target concepts))))
    (do+ loop
	 (doplus::while to-process)
	 (for c (being (first to-process)))
	 (pop to-process)
	 (when (not (find c seen))
	   (dolist (f (features c))
	     (when (string= (feature-name f) feature-name)
	       (return-from loop f)))
	   (dolist (sc (concept-superconcepts c))
	     (let ((the-super-concept (ref-target sc)))
	       (when the-super-concept
		 (push the-super-concept to-process))))
	   (push c seen)))))

(defgeneric implement-feature (feature concept))
(defmethod implement-feature ((feature feature) (concept concept-definition))
  (list
   :name (let ((redefined-feature (find-feature-in-hierarchy (feature-name feature) (concept-superconcepts concept))))
	   (if redefined-feature
	       (feature-slot-name redefined-feature)
	       (setf (feature-slot-name feature) (make-symbol (feature-name feature)))))
   :feature-name (feature-name feature)
   :class concept
   :definition feature
   :multiplicity (cons (multiplicity-min (feature-multiplicity feature)) (multiplicity-max (feature-multiplicity feature)))
   :kind (feature-kind feature)))

(defun concept-of (form)
  (concept-definition (class-of form)))

;; Some useful concepts

(defconcept constant ()
  ((value :initarg :value :accessor constant-value :feature-name "value" :kind :attribute))
  (:language *treep*))

(defconcept binding (named)
  ((value :initarg :value :accessor binding-value :feature-name "value" :kind :containment))
  (:language *treep*))

;; Printing

(defmethod print-object ((object form) stream)
  (let ((concept (class-of object)))
    (if (typep concept 'concept)
	(print-unreadable-object (object stream :type nil :identity t)
	  (let ((def (concept-definition concept)))
	    (if def
		(princ (concept-name def) stream)
		(princ (class-name concept) stream))))
	(call-next-method))))

(defmethod print-object ((object concept) stream)
  (let ((def (concept-definition object)))
    (if def
	(print-object def stream)
	(call-next-method))))

(defmethod print-object ((object language) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (princ "Language " stream)
    (prin1 (language-name object) stream)))

(defmethod print-object ((object concept-definition) stream)
  (print-unreadable-object (object stream :type nil :identity t)
    (princ "Concept " stream)
    (prin1 (concept-name object) stream)))

;; Reflection

(setf (concept-definition (find-class 'concept-definition))
      (make-instance 'concept-definition
		     :name "concept"
		     :features (list
				(make-instance 'attribute :name "name")
				(make-instance 'containment :name "features" :multiplicity (make-instance 'multiplicity))
				(make-instance 'reference :name "superconcepts" :multiplicity (make-instance 'multiplicity)))
		     :implementation (find-class 'concept-definition)))

(add-concept 'concept-definition *treep*)
(add-concept 'language *treep*)
