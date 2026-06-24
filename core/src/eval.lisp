(in-package :treep)

(defgeneric evaluate (form environment)
  (:documentation "Evaluates a Treep form to produce a value"))

(defmethod evaluate ((form form) env)
  "Tree nodes with no defined evaluation rule are self-evaluating, in Lisp fashion."
  (declare (ignore env))
  form)
