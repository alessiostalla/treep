(in-package :treep/tests)

(deftest io ()
  (let ((lang (read-form (make-string-input-stream "(treep:language name:\"foo\")"))))
    (is (string= (language-name lang) "foo"))))
