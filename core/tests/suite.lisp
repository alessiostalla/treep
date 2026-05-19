(in-package :treep/tests)

(deftest run ()
  (test-io)
  (test-install-into-package)
  (test-migrate))
