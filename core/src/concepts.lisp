(in-package :treep)

(defclass concept (clutter:standard-class-with-attributes)
  ((definition :initform nil :accessor concept-definition)))
(defclass concept-slot-definition (clutter:-with-attributes)
  ((kind :initarg :kind :initform :attribute :accessor slot-kind)))
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

(defstruct container form slot)

(defclass form (clutter:-with-attributes)
  ((container :accessor form-container :initform nil :kind :internal))
  (:metaclass concept))

(deftype reference-to (type &key (by 'string)) (list 'or by type))

(defclass language (form)
  ((name :initarg :name :accessor language-name :initform (error "Language name is required"))
   (concepts :reader concepts :initform (make-hash-table :test #'equal))
   (used-languages :accessor used-languages :initarg :used-languages :initform nil))
  (:metaclass concept))

(defvar *treep* (make-instance 'language :name "treep"))
(defvar *language* (make-instance 'language :name "default" :used-languages (list *treep*)))

(defmethod find-concept ((name string) (language language))
  (gethash name (concepts language)))

(defmethod (setf find-concept) ((concept concept) (name string) (language language))
  (setf (gethash name (concepts language)) concept))

;; Note: we can't use (setf find-concept) because concept isn't itself a concept
(setf (gethash "concept" (concepts *treep*)) (find-class 'concept))
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
