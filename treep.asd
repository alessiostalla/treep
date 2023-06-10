(defsystem "treep"
  :version "0.1.0"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("closer-mop" "fset")
  :components ((:module "src/level0"
                :components
                ((:file "packages") (:file "symbols")
		 (:file "forms") (:file "abstractions")
		 (:file "reader")
		 (:file "printer")
		 (:file "evaluator")
		 (:file "loader")
		 (:file "repl")
		 ;;TODO (:file "lisp") (:file "object-system") (:file "level1-boot")
		 )))
  :description "The Treep language"
  :in-order-to ((test-op (test-op "treep/tests"))))

(defsystem "treep/tests"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("treep" "rove")
  :components ((:module "tests"
                :components
                ((:file "main")))) ;TODO (:file "object-system"))))
  :description "Test system for treep"
  :perform (test-op (op c) (symbol-call :rove :run c)))

(defsystem "treep/server"
  :version "0.1.0"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("treep" "ningle" "cl-json")
  :components ((:module "src/server"
                :components
                ((:file "server"))))
  :description "HTTP access to the running Treep image")
