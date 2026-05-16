(in-package :treep)

(defun migrate (form to-language)
  (let ((target-concept (lookup-concept-for-migration form to-language)))
    (change-class form (ensure-concept-implementation target-concept)))) ;; TODO features, children

(defgeneric lookup-concept-for-migration (form language))
(defmethod lookup-concept-for-migration (form language)
  (error "Don't know how to look up concept for migrating ~S to ~S" form language))

(defmethod lookup-concept-for-migration ((form form) (language language))
  (let* ((concept (class-of form))
	 (concept-name (concept-name concept)))
  (or (lookup-concept concept-name language)
      (error "Can't find a concept named ~S in ~S to migrate ~S from ~S"
	     concept-name language form concept))))
