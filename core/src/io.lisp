(in-package :treep)

(define-condition premature-end-of-form (error) ())
(define-condition unexpected-character (error)
  (character))

(defun consume-space (stream)
  (loop :do (let ((ch (peek-char t stream)))
	      (cond
		((or (char= ch #\Space) (char= ch #\Tab) (char= ch #\Linefeed) (char= ch #\Return)) (read-char stream t))
		(t (return-from 'consume-space))))))

(defun read-form (stream language)
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
	 (form-def (lookup-form language name 'definitions)))))

(defun read-name (stream)
  (consume-space stream)
  (let ((name ""))
    (loop :do (let ((ch (peek-char t stream)))
		(cond
		  ((or (alpha-char-p ch) (char= ch #\_) (char= ch #\-) (char= ch #\+))
		   (setf name (concatenate 'string name (string (read-char stream t)))))
		  (t (return-from 'read-name name)))))))
