(in-package :treep-impl)

(defconstant +kind-class+    (intern "class"    +symbol-treep+))
(defconstant +kind-function+ (intern "function" +symbol-treep+))
(defconstant +kind-variable+ (intern "variable" +symbol-treep+))

(defclass environment ()
  ((bindings :initform (fset:map) :initarg :bindings :accessor environment-bindings)))

(defun augment-environment (environment name kind meaning)
  (flet ((compute-meanings ()
	   (fset:with (or (fset:@ (environment-bindings environment) name)
			  (fset:map))
		      kind
		      meaning)))
    (make-instance 'environment :bindings
		   (fset:with (environment-bindings environment)
			      name
			      (compute-meanings)))))

(defclass form-class (closer-mop:standard-class) ())
(defclass form-slot-definition ()
  ((symbol :initarg :symbol :accessor slot-definition-symbol)))
(defclass direct-form-slot-definition (closer-mop:standard-direct-slot-definition form-slot-definition) ())
(defclass effective-form-slot-definition (closer-mop:standard-effective-slot-definition form-slot-definition) ())

(defmethod closer-mop:validate-superclass ((class form-class) (superclass standard-class))
  t)

(defmethod closer-mop:direct-slot-definition-class ((class form-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'direct-form-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class form-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'effective-form-slot-definition))

(defconstant +form-symbol+ (intern "form" +symbol-treep+))
(defconstant +form-parent-symbol+ (intern "parent" +form-symbol+))

(defclass form ()
  ((parent :accessor form-parent))
  (:metaclass form-class))

(defun slot (name class)
  (or (find-if (lambda (def) (eq name (closer-mop:slot-definition-name def)))
	       (closer-mop:class-slots (if (symbolp class) (find-class class) class)))
      (error "~A does not name a slot in ~A" name class)))

(closer-mop:finalize-inheritance (find-class 'form))
(setf (slot-definition-symbol (slot 'parent 'form)) +form-parent-symbol+)

(defgeneric transient-slot? (form slot))
(defmethod transient-slot? ((form form) slot)
  (or (eq slot 'parent)))
(defmethod transient-slot? ((form cl:class) slot)
  (transient-slot? (make-instance form) slot))
(defmethod transient-slot? ((form symbol) slot)
  (transient-slot? (make-instance form) slot))
(defmethod transient-slot? (form (slot closer-mop:slot-definition))
  (transient-slot? form (closer-mop:slot-definition-name slot)))

(defgeneric init-subform (parent name child))
(defmethod init-subform ((parent form) (name (eql 'parent)) (child form))
  nil)
(defmethod init-subform ((parent form) name child)
  nil)
(defmethod init-subform ((parent form) name (child form))
  (setf (form-parent child) parent)) ;;TODO check it doesn't already have a parent

(defmethod initialize-instance :after ((instance form) &key &allow-other-keys)
  (cl:loop
     :for slot :in (closer-mop:class-slots (class-of instance))
     :do (let ((name (closer-mop:slot-definition-name slot)))
	   (if (slot-boundp instance name)
	       (init-subform instance name (cl:slot-value instance name)))))) ;;TODO lists of children

(defgeneric transform (transformer form environment))

(defclass quote (form)
  ((form :initarg :form :reader quoted-form))
  (:metaclass form-class))

(defmethod transform (transformer (form quote) environment)
  (declare (ignore transformer))
  (values (quoted-form form) environment))

#|TODO move/redo
(defclass constant (form)
  ((value :initarg :value :reader constant-value)))

(defclass variable-access (form)
  ((variable-name :initarg :name :reader accessed-variable-name)))

(defclass variable-read (variable-access) ())

(defclass variable-write (variable-access)
  ((form :initarg :form :reader variable-write-form)))

(defclass function-argument (form)
  ((name :initarg :name :reader function-argument-name :type symbol)))

(defclass optional-function-argument (function-argument)
  ((default-value :initarg :default-value :initform nil :reader function-argument-default-value)))

(defclass rest-function-argument (function-argument) ())

(defclass function (form)
  ((lambda-list :initarg :lambda-list :initform (fset:seq) :reader function-lambda-list)
   (expression :initarg :expression :reader function-expression)))

(defclass variable (form)
  ((init-form :initarg :init-form :initform nil :reader variable-init-form)))

(defclass constant (form)
  ((value :initarg :value :reader constant-value)))

;; A macro is not like in Lisp, i.e., superficially like a function call.
;; It's a new form (class).
(defclass macro (form)
  ((expansion :initarg :expansion :reader macro-expansion)))

(defmethod transform (transformer (form macro) environment)
  (let ((expanded (funcall (macro-expansion form) form environment)))
    (transform transformer expanded environment)))

(defclass function-access (form)
  ((function-designator :initarg :function :reader accessed-function-designator)))

(defclass function-reference (function-access) ())

(defclass function-call (function-access)
  ((arguments :initarg :arguments :reader function-arguments)))

;;Environment and definitions
(defclass definition (form)
  ((name :initarg :name :reader definition-name)))

(defclass binding (form)
  ((definition :initarg :definition :reader binding-definition :type definition)
   (body :initarg :body :reader binding-body)))

(defclass define (form)
  ((definition :initarg :definition :reader define-definition :type definition)))

;;TODO Should this inherit from variable?
(defclass variable-definition (definition)
  ((init-form :initarg :init-form :initform nil :reader variable-definition-init-form)
   (mutable? :initarg :mutable :initform nil :reader mutable?)))

(defclass function-definition (definition)
  ((init-form :initarg :init-form :initform nil :reader function-definition-init-form)))
(defclass macro-definition (definition)
  ((slot-definitions :initarg :slot-definitions :reader macro-definition-slots)
   (expander-function :initarg :expander-function :reader macro-definition-expander-function)))

(defgeneric definition-kind (transformer definition))
(defmethod definition-kind (transformer (definition variable-definition))
  +kind-variable+)
(defmethod definition-kind (transformer (definition function-definition))
  +kind-function+)
|#

(defun initial-environment ()
  (make-instance 'environment))

(defvar *environment* (initial-environment))

(defun meaning (symbol kind &optional (environment *environment*))
  (let ((meanings (fset:@ (environment-bindings environment) symbol)))
    (when meanings
      (fset:@ meanings kind))))

(defun copy-environment (&optional (environment *environment*))
  (make-instance 'environment :bindings (environment-bindings environment)))

#|TODO
(defgeneric compute-new-environment-for-definition (transformer definition environment))

(defmethod compute-new-environment-for-definition (transformer (definition definition) environment)
  (augment-environment environment
		       (definition-name definition)
		       (definition-kind transformer definition)
		       (transform transformer definition environment)))

(defmethod transform (transformer (form binding) environment)
  (let ((def (binding-definition form)))
    (transform transformer (binding-body form) (compute-new-environment-for-definition transformer def environment))))

(defmethod transform (transformer (form define) environment)
  (let ((def (define-definition form)))
    (setf (environment-bindings environment)
	  (environment-bindings (compute-new-environment-for-definition transformer def environment)))))
|#

(defmacro define-abstraction (name symbol &optional (superclasses '(form)) slots)
  `(progn
     (defclass ,name ,superclasses ,slots (:metaclass form-class))
     (setf *environment* (augment-environment *environment* ,symbol +kind-class+ (cl:find-class ',name)))))

;;Basic forms
(defconstant +abstraction-definition+ (intern "abstraction-definition" +symbol-treep+))
(defconstant +seq+                    (intern "seq"                    +symbol-treep+))
(defconstant +slot-definition+        (intern "slot-definition"        +symbol-treep+))

(define-abstraction seq +seq+ (form)
  ((elements :initarg :elements :accessor seq-elements)))

(define-abstraction slot-definition +slot-definition+ (form)
  ((name :initarg :name :accessor slot-definition-name)))

(define-abstraction abstraction-definition +abstraction-definition+ (form)
  ((name :initarg :name :accessor form-definition-name)
   (direct-superclasses :initarg :direct-superclasses :accessor form-definition-direct-superclasses)
   (slots :initarg :slots :accessor form-definition-slots)))

#| TODO Common forms
(defclass conditional (form)
  ((condition :initarg :condition :reader conditional-if)
   (then :initarg :then :reader conditional-then :type form :initform nil)
   (else :initarg :else :reader conditional-else :type form :initform nil)))

(defclass loop (form)
  ((name :initarg :name :reader loop-name :initform nil)
   (body :initarg :body :reader loop-body)))

(defclass loop-break (form)
  ((loop-name :initarg :loop-name :reader loop-name :initform nil)
   (return-form :initarg :return :reader return-form :initform nil)))
|#
