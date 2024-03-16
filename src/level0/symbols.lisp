(in-package :treep-impl)

(defclass symbol-space ()
  ((name :reader symbol-space-name :initarg :name :type symbol)
   (contents :accessor symbol-space-contents :type fset:map :initform (fset:map))
   (search-path :accessor symbol-space-search-path :initarg :search-path :type fset:seq :initform (fset:seq))))

(defun symbol (name &key parent)
  (let ((symbol (make-symbol name)))
    (when parent
      (setf (getf (symbol-plist symbol) 'parent) parent))
    symbol))

(defun import-lisp-symbol (symbol parent &key (name (string-downcase (cl:symbol-name symbol))))
  (let ((existing-parent (symbol-parent symbol)))
    (when (and existing-parent (not (eq existing-parent parent)))
      (error "Symbol ~A already has a different parent: ~A" symbol existing-parent))) ;; TODO specific error, print symbols correctly
  (setf (getf (symbol-plist symbol) 'parent) parent)
  (%record-symbol symbol (ensure-symbol-space parent) name)
  symbol)

(defun symbol-name (symbol)
  (cl:symbol-name symbol))

(defun symbol-parent (symbol)
  (getf (symbol-plist symbol) 'parent))

(defun symbol-space (symbol)
  (getf (symbol-plist symbol) 'space))
(defun (setf symbol-space) (space symbol)
  (setf (getf (symbol-plist symbol) 'space) space))

(defun symbol-properties (symbol)
  (or (getf (symbol-plist symbol) 'properties) (fset:map)))
(defun (setf symbol-properties) (properties symbol)
  (setf (getf (symbol-plist symbol) 'properties) properties))

(defun ensure-symbol-space (symbol)
  (or (symbol-space symbol)
      (setf (symbol-space symbol) (make-instance 'symbol-space :name symbol))))

(deftype symbol () 'cl:symbol)

(defun %record-symbol (symbol space &optional (name (symbol-name symbol)))
  ;; TODO check if another symbol already exists under the same name
  (setf (symbol-space symbol) ;;TODO make this the default, but optional 
	(make-instance 'symbol-space :name symbol :search-path (fset:seq space)))
  (setf (symbol-space-contents space)
	(fset:with (symbol-space-contents space) name symbol))
  symbol)

(defun %intern (name space)
  (let ((the-name (string name))
	(space (typecase space
		 (symbol-space space)
		 (symbol (ensure-symbol-space space))
		 (t (error "Not a symbol space designator: ~S" space))))) ;TODO dedicated condition
    (or (%find-symbol the-name space)
	(%record-symbol (symbol the-name :parent (symbol-space-name space)) space))))

(defun %find-symbol (name space &optional exclude)
  (let ((the-name (string name))
	(space (typecase space
		 (symbol-space space)
		 (symbol (or (symbol-space space) (return-from %find-symbol)))
		 (t (error "Not a symbol space designator: ~S" space))))) ;TODO dedicated condition
    (let ((symbol (fset:@ (symbol-space-contents space) the-name)))
      (if symbol
	  symbol
	  (fset:do-seq (s (symbol-space-search-path space))
	    (unless (member s exclude)
	      (push s exclude)
	      (let ((symbol (%find-symbol name s exclude)))
		(when symbol (return-from %find-symbol symbol)))))))))

(defun symbol? (object)
  (typep object 'cl:symbol))
