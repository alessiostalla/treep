(in-package :treep/tests)

(deftest test-migrate ()
  (with-temporary-package (pkg)
    (let ((lang (test-lang-1)))
      (install lang pkg)
      (let* ((concept1 (find-symbol "concept1" pkg))
	     (class1 (find-class concept1))
	     (instance1 (make-instance concept1)))
	(is (eq class1 (class-of instance1)))
	(let ((new-version (test-lang-1)))
	  (migrate instance1 new-version)
	  (let ((class1-redefined (class-of instance1)))
	    (is (not (eq class1 class1-redefined))))
	  ;; TODO check features, children
	  )))))
	    
