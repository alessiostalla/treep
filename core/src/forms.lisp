(in-package :treep)

(defclass form-class (clutter:standard-class-with-attributes)
  ((definition :initform nil :accessor form-class-definition)))
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

(defclass form-definition (form)
  ((slots :initform nil :accessor form-slots)))
