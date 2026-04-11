(in-package :treep)

(define-condition premature-end-of-form (error) ())
(define-condition unexpected-character (error)
  (character))

(defun consume-space (stream)
  (loop :do (let ((ch (peek-char t stream)))
	      (cond
		((or (char= ch #\Space) (char= ch #\Tab) (char= ch #\Linefeed) (char= ch #\Return)) (read-char stream t))
		(t (return))))))

(defun read-form (stream &optional (language *language*))
  (consume-space stream)
  (let ((ch (peek-char t stream)))
    (cond
      ((char= ch #\()
       (read-proper-form stream language))
      ((char= ch #\))
       (read-char stream)
       (error 'unexpected-character ch))
      ((digit-char-p ch)
       (cl:read stream))
      ((char= ch #\")
       (cl:read stream))
;;      ((and (or (char= ch #\+) (char= ch #\-) (char= ch #\.)) 
      (t (error 'unexpected-character ch)))))

(defun read-proper-form (stream language)
  (let ((ch (read-char stream t)))
    (unless (char= ch #\()
      (error 'unexpected-character ch)))
  (let* ((name (read-name stream))
	 (concept (lookup-concept name language)))
    (if concept
	(let ((form (make-instance concept)))
	  ;; TODO fill the form
	  form)
	(error "Unknown concept ~S" name))))

(defun read-name (stream)
  (consume-space stream)
  (let ((simple-name "") (name nil))
    (loop :do (let ((ch (peek-char t stream)))
		(cond
		  ((or (alpha-char-p ch) (char= ch #\_) (char= ch #\-) (char= ch #\+))
		   (setf simple-name (concatenate 'string simple-name (string (read-char stream t)))))
		  ((char= ch #\:)
		   (progn
		     (push simple-name name)
		     (setf simple-name "")
		     (read-char stream t)))
		  (t
		   (progn
		     (when (> (length simple-name) 0)
		       (push simple-name name))
		     (return (nreverse name)))))))))
