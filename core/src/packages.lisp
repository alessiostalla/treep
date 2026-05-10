(defpackage treep
  (:use :cl)
  (:shadow #:load)
  (:export #:concept #:concept-definition #:concept-name #:concepts
	   #:features #:feature-name #:find-language
	   #:language #:language-name #:load #:lookup-concept
	   #:read-form

	   #:*language* #:*languages* #:*treep*))
