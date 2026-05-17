(in-package :treep)

(defun migrate (form to-language)
  (let ((*migrated* nil) (*to-language* to-language))
    (declare (special *migrated* *to-language*))
    (%migrate form to-language)))

(defun %migrate (form to-language)
  (declare (special *migrated*))
  (if (find form *migrated*)
      form
      (let ((*migrated* (cons form *migrated*))
	    (target-concept (lookup-concept-for-migration form to-language)))
	(change-class form (ensure-concept-implementation target-concept)))))

(defmethod update-instance-for-different-class :before ((old form) (new form) &key)
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
				(declare (special *to-language*))
				(migrate child *to-language*)))
			 (if (listp old-value)
			     (mapcar #'migrate-child old-value)
			     (migrate-child old-value)))
		       old-value)))
	      (set-feature new new-slot new-value))
	    (when new-slot
	      (setf (slot-value new (closer-mop:slot-definition-name new-slot)) old-value)))))))

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
