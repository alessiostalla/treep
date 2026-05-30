(in-package :treep)

(define-condition premature-end-of-form (error) ())
(define-condition unexpected-character (error)
  ((character :initarg :character)))
(define-condition not-a-name (error)
  ((starting-with :initarg :starting-with)))

(defun is-space-character (ch)
  (or (char= ch #\Space) (char= ch #\Tab) (char= ch #\Linefeed) (char= ch #\Return)))

(defun read-form (stream &optional (language *language*))
  (let ((ch (peek-char t stream)))
    (cond
      ((char= ch #\()
       (read-proper-form stream language))
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
	(let ((concept-class (ensure-concept-implementation concept)))
	  (if concept-class
	      (let ((form (make-instance concept-class)))
		(fill-form form stream language)
		form)
	      (error "Concept ~S is not implemented" concept)))
	(error "Unknown concept ~S in ~S" name language))))

(defun read-list (stream language element-reader)
  (peek-char t stream)
  (let ((ch (read-char stream t)))
    (unless (char= ch #\[)
      (error 'unexpected-character :character ch)))
  (let ((result nil))
    (loop :do (if (char= (peek-char t stream) #\])
		  (progn
		    (read-char stream)
		    (return (nreverse result)))
		  (push (funcall element-reader stream language) result)))))

(defun fill-form (form stream language)
  (loop :do
     (let ((ch (peek-char t stream)))
       (if (char= ch #\))
	   (progn
	     (read-char stream)
	     (return))
	   (let* ((name (read-name stream))
		  (feature (resolve-feature name form)))
	     (unless feature
	       (error "Unknown feature ~S in ~S" name form))
	     (set-feature form name
			  (let ((mult (feature-multiplicity feature)))
			    (if (or (null mult) (equal mult 1))
				(if (typep feature 'reference)
				    (make-instance 'ref :key (read-name stream))
				    (read-form stream language))
				(read-list stream language
					   (if (reference? feature)
					       (lambda (s l)
						 (declare (ignore l))
						 (make-instance 'ref :key (read-name s)))
					       #'read-form)))))))))
  (form-filled form))

(defgeneric form-filled (form)
  (:documentation "Hook method called after reading a form, to give it a chance to sanitize, canonicalize, and in general transform its constituents"))

(defmethod form-filled (form)
  (declare (ignore form)))

(defmethod form-filled ((form language))
  (let ((concepts (concepts form)))
    (let ((concepts-map (make-hash-table :test #'equal)))
      (dolist (c concepts)
	(setf (gethash (concept-name c) concepts-map) c))
      (setf (slot-value form 'concepts-map) concepts-map))))

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
		     (return (or (nreverse name)
				 (error 'not-a-name :starting-with ch))))))))))

(defun write-form (form stream &optional (language *language*))
  (typecase form
    (form (write-proper-form form stream language))
    (null nil)
    (t (write form :stream stream))))

(defun write-proper-form (form stream language)
  (princ "(" stream)
  (let ((concept (concept-definition (class-of form))))
    (write-concept-name concept stream language)
    (do+
      (for f (in (features concept)))
      (let ((v (get-feature form f)))
	(when v
	  (princ " " stream)
	  (princ (feature-name f) stream)
	  (princ ":" stream)
	  (if (listp v)
	      (let ((first t))
		(princ "[" stream)
		(dolist (v v)
		  (if first
		      (setf first nil)
		      (princ " " stream))
		  (write-form v stream language))
		(princ "]" stream))
	      (write-form v stream language))))))
  (princ ")" stream))

(defun write-concept-name (concept stream ref-language)
  (let ((lang (concept-language concept)))
    (when (and lang (not (eq lang ref-language))) ;; TODO only print the language when necessary
      (princ (language-name lang) stream)
      (princ #\: stream)))
  (princ (concept-name concept) stream))
