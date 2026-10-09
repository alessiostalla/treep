(in-package :treep)

(defconcept field ()
  ((form :initarg :form :initform nil :feature-name "form" :reader field-form :kind :reference)
   (feature :initarg :feature :initform nil :feature-name "feature" :reader field-feature :kind :reference))
  (:language *treep*))
