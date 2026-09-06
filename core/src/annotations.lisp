(in-package :treep)

(defconcept annotation () ()
  (:language *treep*))

(defconcept runtime-only-annotation (annotation) ()
  (:language *treep*))

(defconcept source-information (runtime-only-annotation)
  ((file :initarg :file :reader source-file :initform nil :feature-name "file" :kind :attribute)
   (start-line :initarg :start-line :reader source-start-line :initform nil :feature-name "start-line" :kind :attribute)
   (start-column :initarg :start-column :reader source-start-column :initform nil :feature-name "start-column" :kind :attribute)
   (end-line :initarg :end-line :reader source-end-line :initform nil :feature-name "end-line" :kind :attribute)
   (end-column :initarg :end-column :reader source-end-column :initform nil :feature-name "end-column" :kind :attribute))
  (:language *treep*))

(defconcept documentation (annotation)
  ((text :initarg :text :reader documentation-text :initform nil :feature-name "text" :kind :attribute))
  (:language *treep*))
