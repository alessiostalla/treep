(in-package :treep)

(defgeneric evaluate (form environment)
  (:documentation "Evaluates a Treep form to produce a value"))

(defmethod evaluate ((form form) env)
  "Tree nodes with no defined evaluation rule are self-evaluating, in Lisp fashion."
  (declare (ignore env))
  form)

(let ((eval-transform (make-instance 'transform :name "eval"
				     :applicable (lambda (tr n) (declare (ignore tr n)) t)
				     :function (lambda (tr n) (declare (ignore tr)) (evaluate n nil))))) ;; TODO extend the transform protocol to allow passing arguments
  (setf (slot-value eval-transform 'container) (make-container :form *treep*))
  (publish-transform eval-transform))
