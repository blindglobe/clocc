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
;;; Iteration 0 --
      (list

(list '"tezt-l.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(needed-by-macros (setq file-l-status* ':slurped))

(setq file-l-status* ':loaded)

(eval-when (:load-toplevel)
   (setq file-l-status* ':compiled))

(defun stat-c (x) (> x 0))
"
)

(list '"tezt-s.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(defmacro set-file-c-status (x)
   `(setq file-c-status* ',x))

(needed-by-macros (setq file-s-status* ':slurped))

(setq file-s-status* ':loaded)

(eval-when (:load-toplevel)
   (setq file-s-status* ':compiled))
"
)

(list '"tezt-c.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(needed-by-macros (setq file-c-status* ':slurped))

(set-file-c-status :loaded)

(cond ((stat-c -1)
       (set-file-c-status :loaded-alt)))

(eval-when (:load-toplevel)
   (set-file-c-status :compiled)
   (cond ((stat-c -1)
	  (set-file-c-status :compiled-alt))))
"
)
       )

;;; Iteration 1 --
      (list

(list '"tezt-l.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(needed-by-macros (setq file-l-status* ':slurped-1))

(setq file-l-status* ':loaded-1)

(eval-when (:load-toplevel)
   (setq file-l-status* ':compiled-1))

(defun stat-c (x) (< x 0))
"
)

;;; tezt-s stays the same
;;; tezt-c stays the same     
       )

;;; Iteration 2 --
      (list

;;; tezt-l.lisp stays the same

(list '"tezt-s.lisp"
";-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(defmacro set-file-c-status (x)
   `(setq file-c-status* (build-symbol (:package :keyword) z- (:< ',x))))

(needed-by-macros (setq file-s-status* ':slurped-2))

(setq file-s-status* ':loaded-2)

(eval-when (:load-toplevel)
   (setq file-s-status* ':compiled-2))"
)

;;; tezt-c stays the same     
       )
))
  
;;; The following test must be able to run prior to the definition
;;; of 'depends-on', and hence does not use 'fload', 'fcompl', or 
;;; slurping.  So some of the stuff normally done by these facilities,
;;; such as incrementing the file-op-count*, is done by the tester --

(defun file-test ()
   (setq file-op-count* (+ file-op-count* 1))
   ;; Set up the chunk network --
   (setq file-l-status* ':initial)
   (setq file-s-status* ':initial)
   (setq file-c-status* ':initial)
   (fmakunbound 'set-file-c-status)
   (labels ((set-em-up (name-chars callee slurpee manip)
	       (let ((file-ch
		        (place-file-chunk
			   (merge-pathnames
			       (concatenate 'string
				  "tezt-" name-chars ".lisp")
			       *load-truename*))))
		  (change-class file-ch 'Test-file-chunk
				:callee callee :slurpee slurpee)
		  (let ((loaded-ch
			   (place-loaded-chunk file-ch manip)))
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
      (chunk-request-mgt loaded-file-chunk-c*))
   (let ((res (mapcar
		 (\\ (i files-cont)
		    (dolist (p files-cont)
		       (with-open-file (srmf (first p)
					 :direction ':output :if-exists ':supersede)
			  (princ (second p) srmf)))
		    (dolist (ch (list loaded-file-chunk-l*
				      loaded-file-chunk-s*
				      loaded-file-chunk-c*))
		       (monitor-file-basis ch))
		    (case i
		       (0 (chunk-update loaded-file-chunk-c*))
		       (1 (chunk-update file-chunk-l*))
		       (2 (chunk-update file-chunk-s*)))
		    (format t !"Iteration ~s:~
				~% File 'l' status = ~s File 's' status = ~s File 'c' ~
				   status = ~s~%"
			    i file-l-status* file-s-status* file-c-status*)
		    (list file-l-status* file-s-status* file-c-status*))
		 '(0 1 2)
		 file-contents*)))
     (values (equal res '((:loaded :slurped :compiled)
			  (:loaded-1 :slurped :compiled-alt)
			  (:loaded-1 :slurped-2 :z-compiled-alt)))
	     res
	     (mapcar
	        (\\ (r w)
		   (mapcar #'eq r w))
		res
		'((:loaded :slurped :compiled)
		  (:loaded-1 :slurped :compiled-alt)
		  (:loaded-1 :slurped-2 :z-compiled-alt))))))
