;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; scheme.lisp -- Language related definitions for Scheme.

;;; Nothing here (yet).  Eventually it will contain files related to Scheme
;;; language definitions.


;;; *** PseudoScheme Language Definition
(defun scheme-compile-file (filename &rest args)
  (let ((scheme-package (find-package "SCHEME")))
    (if scheme-package
	(apply (symbol-function (find-symbol "COMPILE-FILE" scheme-package))
	       filename
	       (funcall (symbol-function 
			 (find-symbol "INTERACTION-ENVIRONMENT"
				      scheme-package)))
	       args)
	(error "Pseudo Scheme package not found.")
	)))

(define-language :scheme
  :compiler #'scheme-compile-file
  :loader #'load
  :source-extension "scm"
  :binary-extension "bin")

;;; end of file -- scheme.lisp --
