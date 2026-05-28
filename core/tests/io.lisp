(in-package :treep/tests)

(deftest test-io ()
  (basic-io)
  (nesting)
  (io-roundtrip)
  (test-load))

(deftest basic-io ()
  (let ((lang (read-form (make-string-input-stream "(treep:language name:\"foo\")"))))
    (is (string= (language-name lang) "foo"))))

(deftest nesting ()
  (let ((lang (read-form (make-string-input-stream "(define-language name:\"foo\" concepts:[])"))))
    (is (string= (language-name lang) "foo"))
    (is (= (length (concepts lang)) 0)))
  (let ((lang (read-form (make-string-input-stream "(define-language name:\"foo\" concepts:[
  (concept name:\"concept1\")
])"))))
    (is (string= (language-name lang) "foo"))
    (is (= (length (concepts lang)) 1))
    (let ((concept (lookup-concept "concept1" lang)))
      (is (not (null concept)))
      (is (typep concept 'concept-definition))
      (is (string= "concept1" (concept-name concept)))))
  (let ((lang (read-form (make-string-input-stream "(define-language name:\"foo\" concepts:[
  (concept name:\"concept1\" features:[(attribute name:\"feature-1\") (containment name:\"feature-2\") (reference name:\"feature-3\")])
])"))))
    (let ((concept (lookup-concept "concept1" lang)))
      (is (= 3 (length (features concept))))
      (is (string= "feature-1" (feature-name (elt (features concept) 0)))))))

(deftest io-roundtrip ()
  (let ((lang (read-form (make-string-input-stream "(define-language name:\"foo\" concepts:[
  (concept name:\"concept1\" features:[(attribute name:\"feature-1\") (containment name:\"feature-2\") (reference name:\"feature-3\")])
])"))))
    (is (string= "(treep:concept)" ;; TODO features
		 (with-output-to-string (s)
		   (write-form (first (concepts lang)) s))))))

(deftest test-load ()
  (test-load-simple)
  (test-load-inheritance))

(deftest test-load-simple ()
  (is (null (load (make-string-input-stream " 
"))))
  (is (eq *treep* (find-language "treep")))
  (let ((result (load (make-string-input-stream "(define-language name:\"foo\")"))))
    (is (= 1 (length result))))
  (let ((result (load (make-string-input-stream "(define-language name:\"foo\" concepts:[
  (concept name:\"concept1\" features:[(attribute name:\"feature-1\") (containment name:\"feature-2\") (reference name:\"feature-3\")])
])
(language name:\"foo\")
(concept1 feature-1: 3 feature-3: \"something\" feature-2: (concept1))"))))
    (is (= 3 (length result)))
    (is (typep (elt result 2) 'treep::form))))

(deftest test-load-inheritance ()
  (let ((result (load (make-string-input-stream "(define-language name:\"foo\" concepts:[
  (concept name:\"concept1\" features:[(attribute name:\"feature-1\") (containment name:\"feature-2\") (reference name:\"feature-3\")])
  (concept name:\"concept2\" superconcepts:[concept1] features:[(attribute name:\"feature-1\")])
])
(language name:\"foo\")
(concept2 feature-1: 3 feature-3: \"something\" feature-2: (concept1))"))))
    (is (= 3 (length result)))
    (is (typep (elt result 2) 'treep::form))))
