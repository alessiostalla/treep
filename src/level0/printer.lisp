(in-package :treep-impl)

(defmethod print-object ((object form) stream)
  (let ((name (class-name (class-of object))))
    (princ "(" stream)
    (print-symbol (etypecase name
		    (symbol name)
		    (cl:symbol (get name 'symbol)))
		  stream)
    (princ ")" stream)))
