(in-package :treep)

(defun transform-not-applicable (tr n)
  (declare (ignore tr n))
  nil)

(defun transform-not-implemented (tr n)
  (declare (ignore n))
  (error "Transform not implemented: ~A" tr))

(defconcept transform (named)
  ((applies-to :initarg :applies-to :initform nil :feature-name "applies-to" :reader transform-applies-to :kind :reference :multiplicity (0))
   (applicable :initarg :applicable :feature-name "applicable" :initform #'transform-not-applicable :reader transform-applicable :kind :internal :computed t)
   (function :initarg :function :initform #'transform-not-implemented :feature-name "function" :accessor transform-function :kind :internal :computed t))
  (:language *treep*))

(defun transform-applicable? (tr n)
  (funcall (transform-applicable tr) tr n))

(defun apply-transform-combination (transform node)
  (do+ (for tr (in (combination-transforms)))
       (when (transform-applicable? tr node)
	 (setf node (funcall (transform-function tr) tr node)))) ;; Note: for the moment, we only support generating exactly one node
  node)

(defconcept transform-combination (transform)
  ((transforms :initarg :transforms :initform nil :feature-name "transform" :accessor combination-transforms :kind :containment)
   (function :initarg :function :initform #'apply-transform-combination :feature-name "function" :accessor transform-function :kind :internal))
  (:language *treep*))

(defvar *transforms* nil "Known transforms")

(defmacro push-replace (obj place &key key (test #'eql))
  (let ((obj-var (gensym "OBJECT")))
  `(let ((,obj-var ,obj))
     (setf ,place (remove ,obj-var ,place ,@(when key `(:key ,key)) :test ,test))
     (push ,obj-var ,place))))

(defun publish-transform (tr)
  (push-replace tr *transforms* :key #'qualified-name-of :test #'equal))
