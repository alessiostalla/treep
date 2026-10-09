(in-package :treep)

(defclass source-file (asdf:source-file)
  ((type :initform "treep"))
  (:documentation "A Treep source file in an ASDF system."))

(defmethod asdf:perform ((operation asdf:compile-op) (component source-file))
  (declare (ignore operation))
  (load (namestring (asdf:component-pathname component))))

(defmethod asdf:perform ((operation asdf:load-op) (component source-file))
  (declare (ignore operation))
  (load (namestring (asdf:component-pathname component))))
