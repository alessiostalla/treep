(in-package :treep/tests)

(deftest run ()
  (test-io)
  (test-annotations)
  (test-install-into-package)
  (test-migrate))
