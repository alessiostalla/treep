(defpackage :treep-server (:use :cl :treep-impl))
(in-package :treep-server)

(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/")
      "Welcome to Treep!")

(setf (ningle:route *app* "/definition/:kind/:name")
      #'(lambda (params)
	  ;;TODO we should only find-symbol, not intern
	  (let ((kind (treep-impl:read-symbol-from-string (cdr (assoc :kind params))))
		(name (treep-impl:read-symbol-from-string (cdr (assoc :name params)))))
	    (let ((definition (treep-impl:meaning name kind)))
	      (when definition
		  (princ-to-string definition)))))) ;;TODO describe it

;(clack:clackup *app*)
