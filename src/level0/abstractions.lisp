(in-package :treep-impl)

(defmacro define-abstraction (name symbol &optional superclasses slots)
  `(progn
     (defclass ,name ,(or superclasses '(form)) ,slots (:metaclass form-class))
     (setf *environment* (augment-environment *environment* ,symbol +kind-class+ (cl:find-class ',name)))
     (setf (get ',name 'symbol) ,symbol)
     ',name))

;;Basic forms
(defconstant +symbol-abstraction+           (intern "abstraction" +symbol-treep+))
(defconstant +symbol-abstraction-slot+      (intern "slot"        +symbol-abstraction+))
(defconstant +symbol-abstraction-slot-name+ (intern "name"        +symbol-abstraction-slot+))
(defconstant +symbol-namespace+             (intern "namespace"   +symbol-treep+))
(defconstant +symbol-quote+                 (intern "quote"       +symbol-treep+))
(defconstant +symbol-seq+                   (intern "seq"         +symbol-treep+))
(defconstant +symbol-seq-elements+          (intern "elements"    +symbol-seq+))

(define-abstraction quote +symbol-quote+ ()
  ((form :initarg :form :reader quoted-form)))

(defmethod transform (transformer (form quote) environment)
  (declare (ignore transformer))
  (values (quoted-form form) environment))

(define-abstraction seq +symbol-seq+ ()
  ((elements :initarg :elements :accessor seq-elements :initform (fset:seq) :slot-name +symbol-seq-elements+)))

(define-abstraction slot-definition +symbol-abstraction-slot+ ()
  ((name :initarg :name :accessor slot-definition-name :slot-name +symbol-abstraction-slot-name+)))

(define-abstraction abstraction-definition +symbol-abstraction+ ()
  ((name :initarg :name :accessor abstraction-definition-name)
   (direct-superclasses :initarg :direct-superclasses :accessor abstraction-definition-direct-superclasses :initform (make-instance 'seq))
   (slots :initarg :slots :accessor abstraction-definition-slots :initform (make-instance 'seq))))

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
