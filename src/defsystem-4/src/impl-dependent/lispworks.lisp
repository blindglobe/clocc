;;; -*- Mode: CLtL -*-

(in-package "MK4")

#+unix
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

#+unix
(defun run-program (program &rest arguments)
  (foreign:call-system-showing-output (format nil "~A~@[~{ ~A~}~]"
					      program arguments)))
  

;;; end of file -- lispworks.lisp --
