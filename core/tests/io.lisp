(in-package :treep/tests)

(deftest io ()
  (basic-io)
  (nesting))

(deftest basic-io ()
  (let ((lang (read-form (make-string-input-stream "(treep:language name:\"foo\")"))))
    (is (string= (language-name lang) "foo"))))

(deftest nesting ()
  (let ((lang (read-form (make-string-input-stream "(define-language name:\"foo\" concepts:[])"))))
    (is (string= (language-name lang) "foo"))
    (is (= (hash-table-count (concepts lang)) 0)))
  (let ((lang (read-form (make-string-input-stream "(define-language name:\"foo\" concepts:[
  (concept name:\"concept1\")
])"))))
    (is (string= (language-name lang) "foo"))
    (is (= (hash-table-count (concepts lang)) 1))
    (let ((concept (gethash "concept1" (concepts lang))))
      (is (not (null concept)))
      (is (typep concept 'concept-definition))
      (is (string= "concept1" (concept-name concept)))))
  (let ((lang (read-form (make-string-input-stream "(define-language name:\"foo\" concepts:[
  (concept name:\"concept1\" features:[(feature name:\"feature-1\") (feature name:\"feature-2\") (feature name:\"feature-3\")])
])"))))
    (let ((concept (gethash "concept1" (concepts lang))))
      (is (= 3 (length (features concept))))
      (is (string= "feature-1" (feature-name (elt (features concept) 0)))))))
