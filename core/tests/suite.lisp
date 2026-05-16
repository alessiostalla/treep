(in-package :treep/tests)

(deftest run ()
  (test-io)
  (test-install)
  (test-migrate))
