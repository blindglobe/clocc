;;; -*- Mode: CLtL -*-

;;; load-cl-environment.lisp --
;;;
;;; Copyright (c) 2000-2002 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

;;; Thanks to Kevin Rosemberg for his suggestions to make the loading
;;; of this file more portable.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *cl-environment-directory*
    (make-pathname :host (pathname-host *load-pathname*)
		   :device (pathname-device *load-pathname*)
		   :directory (pathname-directory *load-pathname*)
		   :case :common ; Do we need this?
		   )))

(defun load-cl-environment-library (&key
				    (directory *cl-environment-directory*)
				    (compile-first-p nil)
				    (load-verbose *load-verbose*)
				    (print-herald t)
				    )
  (when print-herald
    (format *standard-output*
	    "~&;;; CL.ENV: Loading CL.ENVIRONMENT package from directory~@
               ;;;         \"~A\"~2%"
	    (namestring (pathname directory))))
  (let ((directory (pathname directory)))
    (flet ((load-and-or-compile (file)
	     (if compile-first-p
		 (multiple-value-bind (output-truename warnings-p failure-p)
		     (compile-file file)
		   ;; (declare (ignore warnings-p))
		   (when failure-p
		     (format *standard-output*
			     ";;; File ~S compiled~@
                              ;;; Warnings ~S, Failure ~S.~%"
			     output-truename
			     warnings-p
			     failure-p)
		     (return-from load-cl-environment-library nil)
		     )
		   (load output-truename :verbose load-verbose))
		 (load file :verbose load-verbose)))
	   )

      (setf (logical-pathname-translations "CL-ENV-LIBRARY")
	    `(("**;*.*.*"
	       ,(make-pathname
		 :host (pathname-host *cl-environment-directory*)
		 :device (pathname-device *cl-environment-directory*)
		 :directory (append (pathname-directory directory)
				    (list :wild-inferiors))))
	      ("**;*.*"
	       ,(make-pathname
		 :host (pathname-host *cl-environment-directory*)
		 :device (pathname-device *cl-environment-directory*)
		 :directory (append (pathname-directory directory)
				    (list :wild-inferiors))))))

      (load-and-or-compile "CL-ENV-LIBRARY:env-package.lisp")
      (load-and-or-compile "CL-ENV-LIBRARY:feature-tagged-type-class.lisp")
      (load-and-or-compile "CL-ENV-LIBRARY:software.lisp")
      (load-and-or-compile "CL-ENV-LIBRARY:machine.lisp")
      (load-and-or-compile "CL-ENV-LIBRARY:operating-system.lisp")
      (load-and-or-compile "CL-ENV-LIBRARY:environment.lisp")
      (load-and-or-compile "CL-ENV-LIBRARY:init-environment.lisp")

      ;; Implementation dependencies (in alphabetical order).
      #+allegro
      (load-and-or-compile "CL-ENV-LIBRARY:impl-dependent;allegro.lisp")

      #+clisp
      (load-and-or-compile "CL-ENV-LIBRARY:impl-dependent;clisp.lisp")

      #+cmu
      (load-and-or-compile "CL-ENV-LIBRARY:impl-dependent;cmucl.lisp")

      #+lcl
      (load-and-or-compile "CL-ENV-LIBRARY:impl-dependent;lcl.lisp")

      #+lispworks
      (load-and-or-compile "CL-ENV-LIBRARY:impl-dependent;lispworks.lisp")

      ;; First usage of the above definitions.
      (load-and-or-compile "CL-ENV-LIBRARY:utilities.lisp")
      (load-and-or-compile "CL-ENV-LIBRARY:system-info.lisp")
      ))
  (pushnew :cl-environment *features*))


;;; end of file -- env.system --
