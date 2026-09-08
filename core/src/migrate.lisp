(in-package :treep)

(defun migrate (form languages-map)
  (let ((*migrated* nil) (*languages-map* languages-map))
    (declare (special *migrated* *languages-map*))
    (%migrate form languages-map)))

(defun %migrate (form languages-map)
  (declare (special *migrated*))
  (if (find form *migrated*)
      form
      (let ((*migrated* (cons form *migrated*))
	    (target-concept
	     (let ((target-language (cdr (assoc (concept-language (concept-of form)) languages-map))))
	       (when target-language
		 (lookup-concept-for-migration form target-language)))))
	(declare (special *migrated*))
	(if target-concept
	    (change-class form (ensure-concept-implementation target-concept))
	    (migrate-form-contents form form languages-map)))))

(defmethod update-instance-for-different-class :before ((old form) (new form) &key)
  (declare (special *languages-map*))
  (migrate-form-contents old new *languages-map*))

(defun migrate-form-contents (old new languages-map)
  (do+
    (for old-slot (in (closer-mop::class-slots (class-of old))))
    (for old-slot-name (being (closer-mop:slot-definition-name old-slot)))
    (when (slot-boundp old old-slot-name)
      (let ((new-slot (find-matching-slot old-slot new))
	    (old-value (slot-value old old-slot-name)))
	(if (typep new-slot 'concept-slot-definition)
	    (let ((new-value
		   (if (containment? new-slot)
		       (flet ((migrate-child (child)
				(migrate child languages-map)))
			 (if (listp old-value)
			     (mapcar #'migrate-child old-value)
			     (migrate-child old-value)))
		       old-value)))
	      (set-feature new new-slot new-value))
	    (when new-slot
	      (setf (slot-value new (closer-mop:slot-definition-name new-slot)) old-value))))))
  new)

(defun safe-feature-name (slot)
  (ignore-errors (feature-name slot)))

(defun find-matching-slot (slot object)
  (if (typep slot 'concept-slot-definition)
      (find (feature-name slot) (closer-mop:class-slots (class-of object)) :key #'safe-feature-name :test #'equal)
      (find (closer-mop:slot-definition-name slot) (closer-mop:class-slots (class-of object)) :key #'closer-mop:slot-definition-name)))

(defgeneric lookup-concept-for-migration (form language))
(defmethod lookup-concept-for-migration (form language)
  (error "Don't know how to look up concept for migrating ~S to ~S" form language))

(defmethod lookup-concept-for-migration ((form form) (language language))
  (let* ((concept (class-of form))
	 (concept-name (concept-name concept)))
  (or (lookup-concept concept-name language)
      (error "Can't find a concept named ~S in ~S to migrate ~S from ~S"
	     concept-name language form concept))))
