;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(defclass Test-file-chunk (File-chunk)
   ((callee :accessor Test-file-chunk-callee
	    :initarg :callee)
    (slurpee :accessor Test-file-chunk-slurpee
	     :initarg :slurpee)))

(defclass Test-loadable-chunk (Loadable-chunk)
   ())

(defmethod create-loaded-controller ((tfc Test-file-chunk) (lc Loaded-chunk))
   (chunk-with-name `(:loadable ,tfc)
      (\\ (name)
	 (make-instance 'Test-loadable-chunk
	    :name name
	    :controllee lc))))

(defmethod derive ((tlc Test-loadable-chunk))
   (let* ((loaded-ch (Loadable-chunk-controllee tlc))
	  (file-ch (Loaded-chunk-file loaded-ch))
	  (compiled-ch (place-compiled-chunk file-ch))
	  (c (Test-file-chunk-callee file-ch))
	  (s (Test-file-chunk-slurpee file-ch)))
      (cond (c
	     (setf (File-chunk-callees file-ch) (list c))
	     (compiled-chunk-note-sub-file-bases compiled-ch c)))
      (cond (s
	     (compiled-chunk-note-sub-file-bases compiled-ch s)))
      file-op-count*))

(defvar file-chunk-l*)
(defvar file-chunk-s*)
(defvar file-chunk-c*)

(defvar loaded-file-chunk-l*)
(defvar loaded-file-chunk-s*)
(defvar loaded-file-chunk-c*)

(defvar file-l-status*)
(defvar file-s-status*)
(defvar file-c-status*)

(defparameter file-contents*
    (list

(list '"test-l.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(needed-by-macros (setq file-l-status* ':slurped))

(setq file-l-status* ':loaded)

(eval-when (:load-toplevel)
   (setq file-l-status* ':compiled))"
)

(list '"test-s.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(defmacro set-file-c-status (x)
   `(setq file-c-status* ',x))

(needed-by-macros (setq file-s-status* ':slurped))

(setq file-s-status* ':loaded)

(eval-when (:load-toplevel)
   (setq file-s-status* ':compiled))"
)

(list '"test-c.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(needed-by-macros (setq file-c-status* ':slurped))

(set-file-c-status :loaded)

(eval-when (:load-toplevel)
   (set-file-c-status :compiled))"
)     
))

;;; The following test must be able to run prior to the definition
;;; of 'depends-on', and hence does not use 'fload', 'fcompl', or 
;;; slurping.  So some of the stuff normally done by these facilities,
;;; such as incrementing the file-op-count*, is done by the tester --

(defun file-test ()
   (setq file-op-count* (+ file-op-count* 1))
   (dolist (p file-contents*)
      (with-open-file (srmf (first p)
			:direction ':output :if-exists ':supersede)
	 (princ (second p) srmf)))
   (setq file-l-status* ':initial)
   (setq file-s-status* ':initial)
   (setq file-c-status* ':initial)
   (fmakunbound 'set-file-c-status)

   (labels ((set-em-up (name-chars callee slurpee manip)
	       (let ((file-ch
		        (place-file-chunk
			   (merge-pathnames
			       (concatenate 'string
				  "test-" name-chars ".lisp")
			       *load-truename*))))
		  (change-class file-ch 'Test-file-chunk
				:callee callee :slurpee slurpee)
		  (let ((loaded-ch
			   (place-loaded-chunk file-ch manip)))
		     (monitor-file-basis loaded-ch)
		     (values file-ch loaded-ch)))))
      (multiple-value-setq
	  (file-chunk-l* loaded-file-chunk-l*)
	  (set-em-up "l" false false ':source))
      (multiple-value-setq
	  (file-chunk-s* loaded-file-chunk-s*)
	  (set-em-up "s" false false ':compile))
      (multiple-value-setq
	  (file-chunk-c* loaded-file-chunk-c*)
	  (set-em-up "c" file-chunk-l* file-chunk-s* ':compile))
      (chunk-request-mgt loaded-file-chunk-c*)
      (chunk-update loaded-file-chunk-c*)
      (format t "File 'l' status = ~s File 's' status = ~s File 'c' status = ~s~%"
	      file-l-status* file-s-status* file-c-status*)
      (and (eq file-l-status* ':loaded)
	   (eq file-s-status* ':slurped)
	   (eq file-c-status* ':compiled))))

