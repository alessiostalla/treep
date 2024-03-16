(in-package :treep-impl)

(defconstant +root-symbol+ '||)
(defconstant +symbol-treep+ (import-lisp-symbol 'treep +root-symbol+))
(defvar *symbol-space* +symbol-treep+)

(defun intern (name &optional (space *symbol-space*))
  (%intern name space))

(defun find-symbol (name &optional (space *symbol-space*) exclude)
  (%find-symbol name space exclude))

(defun print-symbol (symbol &optional (stream *standard-output*))
  (unless (eq (ignore-errors (find-symbol (symbol-name symbol))) symbol)
    (let ((parent (symbol-parent symbol)))
      (when (and parent (not (eq parent *symbol-space*)))
	(print-symbol parent stream)
	(princ ":" stream))))
  (princ (symbol-name symbol) stream)
  symbol)

(defun read-symbol (stream &optional (intern-function #'intern))
  (let* ((separator #\:)
	 (symbol-space (if (eql (peek-char t stream) separator)
			   (progn (read-char stream) +root-symbol+)
			   *symbol-space*))
	 continue
	 (symbol-name (with-output-to-string (s)
			(cl:loop
			   (let ((whitespace '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout))
				 (terminating-chars '(#\( #\) #\# #\[ #\]))
				 (ch (read-char stream nil)))
			     (cond
			       ((eql ch separator)
				(setf continue t)
				(return))
			       ((or (null ch) (member ch whitespace :test #'eql) (member ch terminating-chars :test #'eql))
				(when ch (unread-char ch stream))
				(return))
			       (t (princ ch s)))))))
	 (symbol (funcall intern-function symbol-name symbol-space)))
    (if continue
	(let ((*symbol-space* symbol))
	  (read-symbol stream intern-function))
	symbol)))

(defun read-symbol-from-string (s)
  (with-input-from-string (s s)
    (read-symbol s)))

(defvar *read-symbol-syntax* nil)
(defvar *symbol-dispatch-macro-character* nil)
(defvar *symbol-dispatch-sub-character* nil)

(defmacro with-read-symbol-syntax ((&optional (dispatch-char #\#) (sub-char #\^)) &body body)
  `(let ((*readtable* (copy-readtable))
	 (*symbol-dispatch-macro-character* ,dispatch-char)
	 (*symbol-dispatch-sub-character* ,sub-char))
     ,(if sub-char
	  `(set-dispatch-macro-character ,dispatch-char ,sub-char
					 (lambda (stream sub-char infix)
					   (declare (ignore sub-char infix))
					   (read-symbol stream)))
	  `(set-macro-character ,dispatch-char
				(lambda (stream char)
				  (declare (ignore char))
				  (read-symbol stream))))
     ,@body))

