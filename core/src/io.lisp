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
      ((char= ch #\{)
       (read-reference stream language))
      ((or (digit-char-p ch) (char= ch #\+) (char= ch #\-) (char= ch #\.))
       (let* ((*read-eval* nil)
	      (num (cl:read stream)))
	 (if (numberp num)
	     num
	     (error "Not a number: ~S" num))))
      ((char= ch #\#)
       (read-special-literal stream))
      ((char= ch #\")
       (let* ((*read-eval* nil)
	      (str (cl:read stream)))
	 (if (stringp str)
	     str
	     (error "Not a string: ~S" str))))
      ((identifier-start-char? ch)
       (read-name stream)) 
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

(defun read-reference (stream language)
  (declare (ignore language))
    (let ((ch (read-char stream t)))
    (unless (char= ch #\{)
      (error 'unexpected-character :character ch)))
  (let* ((key (read-name stream))
	 (terminating-char (peek-char t stream))
	 (ref (make-instance 'ref :key key)))
    (if (char= terminating-char #\})
	(read-char stream)
	(error 'unexpected-character :character terminating-char))
    ref))

(defun read-special-literal (stream)
  (let ((ch (peek-char nil stream)))
    (cond
      ((or (char= ch #\t) (char= ch #\T))
       (read-char stream)
       t)
      ((or (char= ch #\f) (char= ch #\F))
       (read-char stream)
       nil)
      (t (error "Not a boolean: ~A" ch)))))

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
			    (if (or (null mult) (equal mult 1) (equal (cdr mult) 1))
				(read-form stream language)
				(read-list stream language #'read-form))))))))
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
		  ((or
		    (identifier-start-char? ch)
		    (and (> (length simple-name) 0) (identifier-char? ch)))
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
    (ref
     (princ "{" stream)
     (write-name (ref-key form) stream)
     (princ "}" stream))
    (form (write-proper-form form stream language))
    (null nil)
    (t (write form :stream stream))))

(defun feature-transient? (f)
  (or (eq (feature-kind f) :internal)
      (feature-computed f)))

(defun write-proper-form (form stream language)
  (princ "(" stream)
  (let ((concept (concept-of form)))
    (write-concept-name concept stream language)
    (do+
      (for f (in (features concept)))
      (when (feature-transient? f)
	(skip))
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

(defun write-name (name stream)
  (typecase name
    (string (princ name stream))
    (list (do+ (for n (in name))
	       (for f (being t :then nil))
	       (unless f
		 (princ ":" stream))
	       (princ n stream)))
    (t (error "Not a name: ~S" name))))
      
