(in-package :treep)

;; TODO macro
(defconcept (reference-language "language") ()
  ((name :initarg :name :accessor language-name :feature-name "name" :kind :attribute))
  (:language *treep*))

(defvar *languages* (list *treep*) "The languages known to the system")
(defun find-language (name &optional (languages *languages*))
  (find name languages :key #'language-name :test #'string=))

(define-condition not-a-language (error)
  ((name :initarg :name)
   (candidates :initarg :candidates)))

(defun load (stream &optional (language *language*) (languages *languages*))
  (typecase stream
    (stream
     (let ((forms (list)))
       (loop
	  :while (peek-char t stream nil)
	  :do (let ((form (read-form stream language)))
		;; TODO autoparent
		(push form forms)
		(typecase form
		  (reference-language
		   (setf language
			 (or (find-language (language-name form) languages)
			     (error 'not-a-language :name (language-name form) :candidates languages))))
		  (language (push form languages))))) ;; Allow to use a newly defined language immediately
       (nreverse forms)))
    (string
     (with-open-file (stream stream)
       (load stream)))
    (t (error "Not a stream designator: ~S" stream))))
