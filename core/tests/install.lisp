(in-package :treep/tests)

(defmacro with-temporary-package ((var &optional (name (format nil "temporary-package-~A" var))) &body body)
  `(let ((,var (make-package ,(symbol-name (gensym name)))))
     (unwind-protect
	  (progn ,@body)
       (delete-package ,var))))

(defun test-lang-1 ()
  (first (load (make-string-input-stream "(define-language name:\"foo\" concepts:[
  (concept name:\"concept1\" features:[(feature name:\"feature-1\") (feature name:\"feature-2\") (feature name:\"feature-3\")])
])"))))

(deftest test-install ()
  ;; Basic install
  (with-temporary-package (pkg)
   (let ((lang (test-lang-1)))
     (install lang pkg)
     (let ((concept1 (find-symbol "concept1" pkg)))
       (is (not (null concept1)))
       (is (not (null (find-class concept1)))))))
  ;; Redefinition - only of symbols, see design choices
  (with-temporary-package (pkg)
    (let ((lang (test-lang-1)))
      (install lang pkg)
      (let* ((concept1 (find-symbol "concept1" pkg))
	     (class1 (find-class concept1))
	     (instance1 (make-instance concept1)))
	(is (eq class1 (class-of instance1)))
	(install (test-lang-1) pkg)
	(let ((class1-redefined (find-class concept1)))
	  (is (not (eq class1 class1-redefined)))
	  (is (eq class1 (class-of instance1))))))))
