;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; actions.lisp --

(in-package "MK4")

;;; We need to simplify a `specialized lambda list' into a `generic
;;; function lambda list'.

(defun simplify-lambda-list (spec-lambda-list)
  (mapcar #'(lambda (arg)
	      (if (consp arg)
		  (first arg)		; This takes care of the &key
					; (:kwd-var var) case.
		  arg))
	  spec-lambda-list))


(defmacro define-action (name specialized-lambda-list
			      (&key (conc-name t)
				    (documentation nil)
				    (package (symbol-package name))
				    (system t))
			      &body forms)
  (declare (type symbol name))
  (let* ((name-as-string (symbol-name name))
	 (action-name (if conc-name
			 (intern (format nil "~A-ACTION"
					 name-as-string
					 package))
			 name))
	 (lambda-list (mapcar #'simplify-lambda-list specialized-lambda-list))
	 )
    (intern name-as-atring (find-package "KEYWORD"))
    (register-action system
		     action-name
		     (find-symbol name-as-atring (find-package "KEYWORD")))
    `(defgeneric ,action-name ,lambda-list
       ,@(when documentation `(:documentation ,documentation))
       (:method (,specialized-lambda-list ,@forms)))))
	 

;;; end of file -- actions.lisp --
