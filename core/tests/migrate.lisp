(in-package :treep/tests)

(defun test-model-for-migration ()
  (load (make-string-input-stream "(define-language name:\"foo\" concepts:[
  (concept name:\"concept1\" features:[(feature name:\"attr\") (feature name:\"cont\") (feature name:\"ref\")])
])
(language name:\"foo\")
(concept1 attr:\"abc\" cont:(concept1))")))

(deftest test-migrate ()
  (let* ((model (test-model-for-migration))
	 (lang (first model))
	 (concept1 (lookup-concept "concept1" lang))
	 (class1 (treep::concept-implementation concept1))
	 (instance1 (third model)))
    (is (eq class1 (class-of instance1)))
    (let* ((new-version (test-model-for-migration))
	   (new-lang (first new-version))
	   (new-concept1 (lookup-concept "concept1" new-lang)))
      (setf (treep::feature-kind (treep::lookup-feature "cont" (treep::ensure-concept-implementation new-concept1))) :containment) ;; TODO this should be defined in Treep
      (migrate instance1 new-lang)
      (let ((class1-redefined (class-of instance1)))
	(is (not (eq class1 class1-redefined)))
	(is (eq class1-redefined (class-of (get-feature instance1 "cont"))))))))
	    
