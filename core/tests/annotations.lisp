(in-package :treep/tests)

(deftest test-annotations ()
  (source-info))

(deftest source-info ()
  (let* ((lang (read-form (make-string-input-stream "(treep:language
    name:\"foo\")")))
	 (source-info (find-if (lambda (a) (typep a 'source-information)) (form-annotations lang))))
    (is (not (null source-info)))))
