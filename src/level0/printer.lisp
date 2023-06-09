(in-package :treep-impl)

(defmethod print-object ((object form) stream)
  (let ((name (class-name (class-of object))))
    (princ "(" stream)
    (print-symbol (etypecase name
		    (symbol name)
		    (cl:symbol (get name 'symbol)))
		  stream)
    (dolist (slot (remove-if (lambda (slot) (transient-slot? object slot))
			     (closer-mop:class-slots (class-of object))))
      (princ " " stream)
      (princ (cl:slot-value object (closer-mop:slot-definition-name slot)) stream))
    (princ ")" stream)))
