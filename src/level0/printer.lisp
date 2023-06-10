(in-package :treep-impl)

(defclass printer ()
  ((stream :initarg :stream :initform *standard-output* :accessor printer-stream)))

(defmethod transform ((transformer printer) (object form) environment)
  (let ((name (class-name (class-of object)))
	(stream (printer-stream transformer)))
    (princ "(" stream)
    (print-symbol (etypecase name
		    (symbol name)
		    (cl:symbol (get name 'symbol)))
		  stream)
    (dolist (slot (remove-if (lambda (slot) (transient-slot? object slot))
			     (closer-mop:class-slots (class-of object))))
      (princ " " stream)
      (transform transformer (cl:slot-value object (closer-mop:slot-definition-name slot)) environment))
    (princ ")" stream)))

(defmethod transform ((transformer printer) (object symbol) environment)
  (declare (ignore environment))
  (print-symbol object (printer-stream transformer)))

(defmethod transform ((transformer printer) object environment)
  (declare (ignore environment))
  (print-object object (printer-stream transformer)))
