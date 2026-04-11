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

(defclass language ()
  ((name :initarg :name :accessor language-name :initform (error "Language name is required"))
   (concepts :reader concepts :initform (make-hash-table))))

(defvar *language* (make-instance 'language :name "default"))

(defmethod find-concept ((name string) (language language))
  (gethash name (concepts language)))

(defun lookup-concept (name &optional (language *language*))
  (if name
      (if (cdr name)
	  (error "TODO: lookup of qualified name not supported")
	  (find-concept (car name) language))
      nil))
