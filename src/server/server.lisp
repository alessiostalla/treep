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
		(setf (lack.response:response-headers ningle:*response*)
		      (append (lack.response:response-headers ningle:*response*)
			      (list :content-type "application/json")))
		(json:encode-json-plist-to-string
		 (list :name (class-name definition)
		       :slots (mapcar #'(lambda (slot)
					  (let ((map (make-hash-table)))
					    (setf (gethash :name map) (closer-mop:slot-definition-name slot))
					    (unless (eq t (closer-mop:slot-definition-type slot))
					      (setf (gethash :type map) (closer-mop:slot-definition-type slot)))
					    (when (closer-mop:slot-definition-initform slot)
					      (setf (gethash :init-form map) (closer-mop:slot-definition-initform slot)))
					    map))
				      (let ((instance (make-instance definition)))
					(remove-if #'(lambda (slot)
						       (and (typep instance 'form)
							    (transient-slot? instance slot)))
						   (closer-mop:class-slots definition)))))))))))

;(clack:clackup *app*)
