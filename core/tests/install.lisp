(in-package :treep/tests)

(deftest test-install ()
  (let ((pkg (make-package "test-install-temp-package")))
    (unwind-protect
	 (let ((lang (first (load (make-string-input-stream "(define-language name:\"foo\" concepts:[
  (concept name:\"concept1\" features:[(feature name:\"feature-1\") (feature name:\"feature-2\") (feature name:\"feature-3\")])
])")))))
	   (install lang pkg)
	   (let ((concept1 (find-symbol "concept1" pkg)))
	     (is (not (null concept1)))
	     (is (not (null (find-class concept1))))))
      (delete-package pkg))))
