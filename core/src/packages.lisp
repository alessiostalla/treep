(defpackage treep
  (:use :cl)
  (:shadow #:load)
  (:export #:concept #:concept-definition #:concept-name #:concepts
	   #:features #:feature-name #:find-language
	   #:install #:install-element
	   #:language #:language-name #:load #:lookup-concept
	   #:migrate
	   #:read-form

	   #:*language* #:*languages* #:*treep*))
