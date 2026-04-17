(in-package :treep)

(define-condition premature-end-of-form (error) ())
(define-condition unexpected-character (error)
  ((character :initarg :character)))

(defun is-space-character (ch)
  (or (char= ch #\Space) (char= ch #\Tab) (char= ch #\Linefeed) (char= ch #\Return)))

(defun read-form (stream &optional (language *language*))
  (let ((ch (peek-char t stream)))
    (cond
      ((char= ch #\()
       (read-proper-form stream language))
      ((char= ch #\[)
       (read-list stream language))
      ((char= ch #\))
       (read-char stream)
       (error 'unexpected-character :character ch))
      ((digit-char-p ch)
       (cl:read stream))
      ((char= ch #\")
       (cl:read stream))
      ((identifier-start-char? ch)
       (read-name stream))
;;      ((and (or (char= ch #\+) (char= ch #\-) (char= ch #\.)) 
      (t (error 'unexpected-character :character ch)))))

(defun read-proper-form (stream language)
  (let ((ch (read-char stream t)))
    (unless (char= ch #\()
      (error 'unexpected-character :character ch)))
  (let* ((name (read-name stream))
	 (concept (lookup-concept name language)))
    (if concept
	(let ((concept-class (concept-implementation concept)))
	  (if concept-class
	      (let ((form (make-instance concept-class)))
		(fill-form form stream language)
		form)
	      (error "Concept ~S is not implemented" concept)))
	(error "Unknown concept ~S" name))))

(defun read-list (stream language)
  (peek-char t stream)
  (let ((ch (read-char stream t)))
    (unless (char= ch #\[)
      (error 'unexpected-character :character ch)))
  (let ((result nil))
    (loop :do (if (char= (peek-char t stream) #\])
		  (progn
		    (read-char stream)
		    (return (nreverse result)))
		  (push (read-form stream language) result)))))

(defun fill-form (form stream language)
  (loop :do
     (let ((ch (peek-char t stream)))
       (if (char= ch #\))
	   (progn
	     (read-char stream)
	     (return))
	   (let ((name (read-name stream))
		 (subform (read-form stream language)))
	     (set-feature form name subform)))))
  (form-filled form))

(defgeneric form-filled (form)
  (:documentation "Hook method called after reading a form, to give it a chance to sanitize, canonicalize, and in general transform its constituents"))

(defmethod form-filled (form)
  (declare (ignore form)))

(defmethod form-filled ((form language))
  (let ((concepts (concepts form)))
    (when (listp concepts)
      (let ((concepts-map (make-hash-table :test #'equal)))
	(dolist (c concepts)
	  (setf (gethash (concept-name c) concepts-map) c))
	(setf (slot-value form 'concepts) concepts-map)))))
    

(defun identifier-start-char? (ch)
  (or (alpha-char-p ch) (char= ch #\_) (char= ch #\-) (char= ch #\+) (char= ch #\*)))

(defun identifier-char? (ch)
  (or (identifier-start-char? ch)  (digit-char-p ch)))

(defun read-name (stream)
  (peek-char t stream)
  (let ((simple-name "") (name nil))
    (loop :do (let ((ch (peek-char nil stream)))
		(cond
		  ((identifier-char? ch)
		   (when (and (= (length simple-name) 0) (not (identifier-start-char? ch)))
		     (error 'unexpected-character :character ch))
		   (setf simple-name (concatenate 'string simple-name (string (read-char stream t)))))
		  ((char= ch #\:)
		   (if (> (length simple-name) 0)
		       (progn
			 (push simple-name name)
			 (setf simple-name "")
			 (read-char stream t))
		       (error "The :: sequence is invalid")))
		  (t
		   (progn
		     (when (> (length simple-name) 0)
		       (push simple-name name))
		     (return (nreverse name)))))))))

(defun load (stream-designator &optional consumer)
  (typecase stream-designator
    (stream
     (let (contents)
       (loop
	  :while (peek-char t stream-designator nil)
	  :do (let ((form (read-form stream)))
		(if consumer
		    (funcall consumer form)
		    (push form contents))))
       (nreverse contents)))
    (string
     (with-open-file (stream stream-designator)
       (load stream)))
    (t (error "Not a stream designator: ~S" stream-designator))))
