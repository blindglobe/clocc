;;; -*- Mode: CLtL -*-

;;; load-cl-environment.lisp --
;;;
;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(defun load-cl-environment-library (&key
				    (directory (make-pathname
						:directory
						(pathname-directory
						 *default-pathname-defaults*)))
				    (compile-first-p nil)
				    (load-verbose *load-verbose*)
				    (print-herald t)
				    )
  (when print-herald
    (format *standard-output*
	    "~&;;; CL.ENV: Loading CL.ENVIRONMENT package from directory~@
               ;;;         \"~A\"~2%"
	    directory))
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
	    `(("*.*.*" ,(namestring (truename directory)))
	      ("*.*"   ,(namestring (truename directory)))))

      (load-and-or-compile "CL-ENV-LIBRARY:env-package.lisp")
      (load-and-or-compile "CL-ENV-LIBRARY:software.lisp")
      (load-and-or-compile "CL-ENV-LIBRARY:machine.lisp")
      (load-and-or-compile "CL-ENV-LIBRARY:operating-system.lisp")
      (load-and-or-compile "CL-ENV-LIBRARY:environment.lisp")
      (load-and-or-compile "CL-ENV-LIBRARY:init-environment.lisp")
      (load-and-or-compile "CL-ENV-LIBRARY:system-info.lisp")
      )))


;;; end of file -- env.system --
