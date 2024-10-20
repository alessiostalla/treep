(defsystem "treep"
  :version "0.1.0"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("closer-mop" "clutter" "fset")
  :components ((:module "core/src"
                :components
                ((:file "packages")
		 (:file "forms"))))
  :description "The Treep language"
  :in-order-to ((test-op (test-op "treep/tests"))))

(defsystem "treep/tests"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("treep" "rove")
  :components ((:module "core/tests"
                :components
                ((:file "main"))))
  :description "Test system for treep"
  :perform (test-op (op c) (symbol-call :rove :run c)))

(defsystem "treep/server"
  :version "0.1.0"
  :author "Alessio Stalla"
  :license "AGPL"
  :depends-on ("treep" "ningle" "cl-json")
  :components ((:module "server/src"
                :components
                ((:file "server"))))
  :description "HTTP access to the running Treep image")
