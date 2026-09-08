(in-package :treep)

;; TODO macro
(defconcept (reference-language "language") ()
  ((name :initarg :name :accessor language-ref-name :feature-name "name" :kind :attribute))
  (:language *treep*))

(defclass system ()
  ((languages :accessor system-languages :initform nil :initarg :languages)
   (roots :accessor system-roots :initform nil)))

(defvar *system* (make-instance 'system :languages (list *treep*)) "The active system")

(defun known-languages (&optional (system *system*))
  (system-languages system))

(defun find-language (name &optional (languages (known-languages)))
  (find name languages :key #'language-name :test #'string=))

(define-condition not-a-language (error)
  ((name :initarg :name)
   (candidates :initarg :candidates)))

(defun load (stream &optional (language *language*) (languages (known-languages)))
  (typecase stream
    (source-position-tracking-input-stream
     (let ((forms (list)))
       (loop
	  :while (peek-char t stream nil)
	  :do (let ((form (read-form stream language)))
		(push form forms)
		(typecase form
		  (reference-language
		   (setf language
			 (or (find-language (language-ref-name form) languages)
			     (error 'not-a-language :name (language-ref-name form) :candidates languages))))
		  (language (push form languages))))) ;; Allow to use a newly defined language immediately
       (nreverse forms)))
    (stream
     (load (make-instance 'source-position-tracking-input-stream :stream stream) language languages))
    (string
     (with-open-file (stream stream)
       (load stream language languages)))
    (t (error "Not a stream designator: ~S" stream))))
