(defpackage treep
  (:use :cl)
  (:shadow #:load)
  (:export #:concept #:concept-definition #:concept-name #:concepts
	   #:features #:feature-name
	   #:language #:language-name #:load
	   #:read-form))
