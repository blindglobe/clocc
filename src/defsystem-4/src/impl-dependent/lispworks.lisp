;;; -*- Mode: CLtL -*-

(in-package "MK4")

(defun compile-file-internal (input-file
			      &rest keys
			      &key
			      output-file
			      error-file
			      (print *compile-print*)
			      (verbose *compile-verbose*)
			      (external-format :default)
			      ;; old 18c did not have :external format
			      &allow-other-keys)
  (declare (ignore output-file error-file print verbose external-format))

  ;; Remove the next lines when more tests available.
  (remf keys :error-file)
  
  (apply #'compile-file input-file :load nil keys))


(defmethod run-os-program ((program string)
			   &key
			   (arguments ())
			   (input nil)
			   (output nil)
			   (error-output nil)
			   &allow-other-keys)
  (declare (ignore input output error-output))
  (system:call-system-showing-output (format nil "~A~@[~{ ~A~}~]"
					     program arguments)))

(defun run-program (program &rest arguments)
  (system:call-system-showing-output (format nil "~A~@[~{ ~A~}~]"
					      program arguments)))


(defmethod load-object-file ((loadable-object-pathname pathname)
			     &key
			     (print *load-print*)
			     (verbose *load-verbose*)
			     (libraries '("c"))
			     )
  (declare (ignore print libraries))
  (when verbose
    (format *trace-output* ";;; MK4: Loading Foreign File ~A."
	    loadable-c-pathname))
  #+(and :unix (not :linux))
  (link-load:read-foreign-module loadable-object-pathname)

  #+(and :unix (not :linux))
  (fli:register-module loadable-object-pathname)
  )

;;; end of file -- lispworks.lisp --
