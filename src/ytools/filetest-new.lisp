;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(defclass Test-loadable-chunk (Loadable-chunk)
   ((callee :accessor Test-loadable-chunk-callee
	    :initarg :callee)))

(defmethod derive ((tlc Test-loadable-chunk))
   (let ((file-ch (Loadable-chunk-file tlc))
	 (c (Test-loadable-chunk-callee tlc)))
      (cond (c
	     (let ((compiled-ch (place-compiled-chunk file-ch)))
		(setf (File-chunk-callees tfc) (list c))
		(dolist (ssfty standard-sub-file-types*)
		   (format t "Doing ~s~% for pathname ~s~%"
			   ssfty (File-chunk-pathname tfc))
		   (pushnew (funcall
			       (Sub-file-type-chunker ssfty)
			       (File-chunk-pathname c))
			    (Chunk-basis compiled-ch))
		   (pushnew (funcall
			       (Sub-file-type-load-chunker
				  ssfty)
			       (File-chunk-pathname c))
			    (Chunk-update-basis
				 compiled-ch))))))))

(defvar file-chunk-1*)
(defvar file-chunk-2*)

(defvar loaded-file-chunk-1*)
(defvar loaded-file-chunk-2*)

(defvar file-1-status*)
(defvar file-2-status*)

(defparameter file-contents*
    (list
(list '"test1.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(setq file-1-status* ':loaded)"
)
(list '"test2.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)"

(setq file-2-status* ':loaded)
)))

;;; The following test must be able to run prior to the definition
;;; of 'depends-on', and hence does not use 'fload', 'fcompl', or 
;;; slurping.  So some of the stuff normally done by these facilities,
;;; such as incrementing the file-op-count*, is done by the tester --

(defun file-test ()
   (setq file-op-count* (+ file-op-count* 1))
   (dolist (p file-contents*)
      (with-open-file (srm1 (first p)
			:direction ':output :if-exists ':supersede)
	 (princ (second p) srm1)))
   (setq file-1-status* ':not-loaded)
   (setq file-2-status* ':not-loaded)
   (setq file-chunk-1*
         (place-file-chunk (merge-pathnames "test1.lisp" *load-truename*)))
   (change-class file-chunk-1* 'Test-file-chunk :callee false)
   (setq loaded-file-chunk-1*
	 (place-loaded-chunk file-chunk-1* ':source))
   (monitor-file-basis file-chunk-1*)
   (setq file-chunk-2*
	 (place-file-chunk (merge-pathnames "test2.lisp" *load-truename*)))
   (change-class file-chunk-2* 'Test-file-chunk :callee file-chunk-1*)
   (setq loaded-file-chunk-2*
         (place-loaded-chunk file-chunk-2* ':compile))
   (monitor-file-basis file-chunk-2*)
   (chunk-request-mgt loaded-file-chunk-2*)
   (chunk-update loaded-file-chunk-2*)
   (format t "File 1 status = ~s File 2 status = ~s~%"
	   file-1-status* file-2-status*)
   (and (eq file-1-status* ':loaded)
	(eq file-2-status* ':loaded)))

