(in-package :treep-impl)

(defclass simple-evaluator () ())

(defun eval (form &optional (environment *environment*))
  (multiple-value-bind (result env)
      (transform (make-instance 'simple-evaluator) form environment)
    (when (eq environment *environment*) ;;Convenience for top-level eval
      (setf *environment* env))
    (values result env)))

(defmethod transform ((transformer simple-evaluator) form environment)
  (values form environment)) ;Lisp objects are self-evaluating

(defmethod transform ((transformer simple-evaluator) (form seq) environment)
  (values
   (fset:image (lambda (f) (transform transformer f environment)) (seq-elements form))
   environment))

(defun resolve-class (designator environment)
  (typecase designator
    (symbol (or (meaning designator +kind-class+ environment) (error "~A is not the name of a class" designator)))
    (cl:symbol (find-class designator))
    (t (error "Not a class designator: ~A" designator))))

(defmethod transform ((transformer simple-evaluator) (form abstraction-definition) environment)
  (let* ((class-name (or (abstraction-definition-name form) (error "The name of an abstraction definition is required")))
	 (direct-superclasses (or (fset:convert 'list ;; TODO compute these in a different phase/transformer
						(fset:image (lambda (c) (resolve-class c environment))
							    (seq-elements (abstraction-definition-direct-superclasses form))))
				  (list (find-class 'form))))
	 ;; TODO is this the proper MOP incantation?
	 (class (make-instance 'form-class :name class-name :direct-superclasses direct-superclasses
					;TODO slots
			       )))
    (setf (form-class-definition class) form)
    (values class (augment-environment environment class-name +kind-class+ class))))

(defmethod transform ((transformer simple-evaluator) (form namespace) environment)
  (values
   (if (slot-boundp form 'name)
       (if (slot-boundp form 'search-path)
	   (let ((ns (namespace-name form)))
	     (setf (symbol-space-search-path (ensure-symbol-space ns))
		   (the fset:seq (transform transformer (namespace-search-path form) environment)))
	     ns)
	   (setf *symbol-space* (namespace-name form)))
       *symbol-space*)
   environment))

#|TODO move/redo

(defclass box ()
  ((value :initarg :value :accessor box-value)))

(defclass interpreted-function (function closer-mop:funcallable-standard-object) () (:metaclass closer-mop:funcallable-standard-class))

(defmethod transform ((transformer simple-evaluator) (form variable-definition) environment)
  (let ((value (transform transformer (variable-definition-init-form form) environment)))
    (make-instance 'box :value value)))
(defmethod transform ((transformer simple-evaluator) (form function-definition) environment)
  (let ((function (transform transformer (function-definition-init-form form) environment)))
    (typecase function
      (interpreted-function function)
      (function (transform transformer function environment))
      (t (error "Not a function: ~S" function))))) ;TODO specific condition

(defmethod transform ((transformer simple-evaluator) (form variable-read) environment)
  (let* ((variable (accessed-variable-name form))
	 (meaning (meaning variable +kind-variable+ environment)))
    (if meaning
	(if (typep meaning 'box) (box-value meaning) meaning)
	(error (format nil "Unknown variable: ~A" (with-output-to-string (out) (print-symbol variable out))))))) ;TODO proper condition class

(defmethod transform ((transformer simple-evaluator) (form variable-write) environment)
  (let* ((variable (accessed-variable-name form))
	 (meaning (meaning variable +kind-variable+ environment)))
    (if meaning
	(if (typep meaning 'box)
	    (setf (box-value meaning) (transform transformer (variable-write-form form) environment))
	    (error (format nil "Not a variable: ~A" (with-output-to-string (out) (print-symbol variable out))))) ;TODO proper condition class
	(error (format nil "Unknown variable: ~A" (with-output-to-string (out) (print-symbol variable out))))))) ;TODO proper condition class

(defun check-function-lambda-list (ll)
  (let (found-optional found-rest)
    (fset:do-seq (arg ll)
      (when found-rest
	(error "No further arguments allowed after the rest argument: ~A" arg)) ;TODO specific condition class
      (etypecase arg
	(optional-function-argument (setf found-optional t))
	(rest-function-argument (setf found-rest t))
	(function-argument
	 (when found-optional
	   (error "No further regular arguments allowed after first optional argument: ~A" arg))))) ;TODO specific condition class
    ll))

(defun to-lisp-lambda-list (lambda-list transformer environment)
  (let ((result (list)) (symbols (list)) &optional-p rest-var)
    (fset:do-seq (arg lambda-list)
      (let ((symbol (make-symbol (symbol-name (function-argument-name arg)))))
	(when (typep arg 'optional-function-argument)
	  (when (not &optional-p)
	    (push '&optional result)
	    (setf &optional-p t))
	  (when (function-argument-default-value arg)
	    (push (list symbol`(transform ,transformer ,(function-argument-default-value arg) ,environment))
		  result)
	    (push symbol symbols)
	    (return)))
	(when (typep arg 'rest-function-argument)
	  (push '&rest result)
	  (setf rest-var symbol))
	(push symbol result)
	(push symbol symbols)))
    (values
     (nreverse result)
     (nreverse symbols)
     rest-var)))

(defmethod transform ((transformer simple-evaluator) (form interpreted-function) environment)
  (declare (ignorable transformer environment))
  form)

(defun make-interpreted-lambda-expression (lambda-list body transformer environment)
  (multiple-value-bind (lisp-args variables rest-var)
      (to-lisp-lambda-list lambda-list transformer environment)
    `(lambda ,lisp-args
       ;;TODO declare args ignorable?
       (transform ,transformer ,body
		  (let ((env ,environment))
		    ,@(cl:loop :for i :from 0 :to (1- (fset:size lambda-list))
			       :collect `(setf env (augment-environment
						    env ,(function-argument-name (fset:@ lambda-list i)) +kind-variable+
						    (make-instance 'box :value ,(let ((var (nth i variables)))
										  (if (eq var rest-var)
										      `(fset:convert 'fset:seq ,var)
										      var)))))) ;TODO should they be constant?
		    env)))))

(defmethod transform ((transformer simple-evaluator) (form function) environment)
  (let* ((lambda-list (check-function-lambda-list (function-lambda-list form)))
	 (body (function-expression form))
	 (fn (make-interpreted-lambda-expression lambda-list body transformer environment))
	 (interpreted-function (make-instance 'interpreted-function :lambda-list lambda-list)))
    (closer-mop:set-funcallable-instance-function interpreted-function (compile nil fn))
    interpreted-function))

(defun resolve-function (transformer function-designator environment)
  (flet ((to-lisp-function (designator)
	   (typecase designator
	     ((or interpreted-function cl:function closer-mop:funcallable-standard-object) designator)
	     (function (transform transformer designator environment))
	     (t (error "Not a function designator: ~S" designator))))) ;TODO proper condition class
    (typecase function-designator	    
      (symbol (let ((meaning (meaning function-designator +kind-function+ environment)))
		(unless meaning
		  (error (format nil "Unknown function: ~A" (with-output-to-string (out) (print-symbol function-designator out))))) ;TODO proper condition class
		(to-lisp-function meaning)))
      (t (to-lisp-function (transform transformer function-designator environment))))))

(defmethod transform ((transformer simple-evaluator) (form function-access) environment)
  (resolve-function transformer (accessed-function-designator form) environment))

(defmethod transform ((transformer simple-evaluator) (form function-call) environment)
  (let ((lisp-function (resolve-function transformer (accessed-function-designator form) environment)))
    (apply lisp-function
	   (fset:convert 'list (fset:image (lambda (a) (transform transformer a environment)) (function-arguments form))))))

(defmethod transform ((transformer simple-evaluator) (form conditional) environment)
  (if (transform transformer (conditional-if   form) environment)
      (transform transformer (conditional-then form) environment)
      (transform transformer (conditional-else form) environment)))

(defmethod transform ((transformer simple-evaluator) (form loop) environment)
  (catch (loop-name form)    
    (cl:loop (transform transformer (loop-body form) environment))))

(defmethod transform ((transformer simple-evaluator) (form loop-break) environment)
  (throw (loop-name form) (transform transformer (return-form form) environment)))
|#
