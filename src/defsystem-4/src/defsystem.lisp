;;; -*- Mode: CLtL -*-

;;; DEFSYSTEM 4.0

;;; defsystem.lisp --

(in-package "MK4")

(define-condition no-loader (error)
  ((component :reader unloadable-component :initarg :component))
  (:report (lambda (nlc stream)
	     (format stream "No loader is known for component ~S."
		     (unloadable-component nlc)))))

(define-condition no-compiler (error)
  ((component :reader uncompilable-component :initarg :component))
  (:report (lambda (nlc stream)
	     (format stream "No compiler is known for component ~S."
		     (uncompilable-component nlc)))))



(defgeneric describe-component (component
				&optional stream recursive-p recursion-depth)
  (:documentation
   "Prints a description of the component.
The COMPONENT argument is a standard component. STREAM is a stream
where to print the description (default*STANDARD-OUTPUT*). RECURSIVE-P
controls whether the printing should recur on the subcomponents and
RECURSION-DEPTH limits the depth of this recursion (its NIL default
means that no limit should be imposed on this recursion)."))


(defgeneric execute-action (component operation &key))

(defgeneric execute-action-on-subcomponents (component operation &key))

(defgeneric load-action (component component-pathname))

(defgeneric compile-action (component component-source-pathname
				      &key
				      error-log-file
				      &allow-other-keys))

(defgeneric needs-loading (component source-or-binary-tag &key))

(defgeneric needs-compiling (component &key))

;;; end of file -- defsystem.lisp --
