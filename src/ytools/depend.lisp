;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:compile-toplevel :load-toplevel)
   (export '(depends-on module funktion)))

(defvar depends-on-enabled* true)

(defun cl-user::depends-on-disable () (setq depends-on-enabled* false))

;;; Syntax (depends-on <filegroup>*)
;;; where <filegroup> is [<times>] --floadable-filespecs--
;;; <times> is (:at <time>*)
;;; where each <time> is either :run-time, :compile-time, :slurp-time
;;; (:at :run-time) may be abbreviated :at-run-time; similarly for 
;;; :at-compile-time and :at-slurp-time
;;; (:at :run-time :compile-time :slurp-time) may be abbreviated
;;; :always.
;;; Let C be the file containing the 'depends-on' form, F be a file
;;; mentioned in the 'depends-on'.
;;; 
;;; Note that :slurp-time means that F will be loaded as soon
;;; as the 'depends-on' form is seen.  :run-time means that F will be
;;; loaded when C or its compiled form is loaded.
;;; :compile-time means that F will be loaded when C is compiled.

(defmacro depends-on (&rest stuff)
   (let ((dgroups (depends-on-args-group stuff)))
;;;;   (let ((c-recorder-var (gensym))
;;;;	 (r-recorder-var (gensym))
;;;;	 (pnl-var (gensym)))
      (let ((some-run (some (\\ (g) 
			       (memq ':run-time (car g)))
			    dgroups))
	    (some-comp (some (\\ (g)
				  (memq ':compile-time (car g)))
			       dgroups)))
	 (cond ((or some-run some-comp)
		`(cond (depends-on-enabled*
			(let (,@(include-if some-comp
				   '(c-recorder (and now-loading-lprec*
						     (lpr-comp-tdo-recorder
							 now-loading-lprec*))))
			      ,@(include-if some-run
				   '(r-recorder (and now-loading-lprec*
						     (lpr-run-tdo-recorder
							 now-loading-lprec*)))))
			   (cond ((not (and ,@(include-if some-comp 'c-recorder)
					    ,@(include-if some-run 'r-recorder)))
				  (format *error-output* ;;;; cerror "Forge on"  ;;;;
					  "Warning: depends-on form in file being loaded without fload: ~% ~s~%"
					  '(depends-on ,@stuff))))
			   ,@(mapcan
				(lambda (g)
				   (let ((at-run-time (memq ':run-time (car g)))
					 (at-compile-time (memq ':compile-time (car g))))
				      (cond ((or at-run-time at-compile-time)
					     (let ((pnl
						      (filespecs->ytools-pathnames
							 (cdr g))))
					        (depends-on-expansion
						   pnl at-run-time at-compile-time)))
					    (t !()))))
				dgroups)))))
	       (t '(values))))))

(defun depends-on-expansion (pnl at-run-time at-compile-time)
   (nconc (cond (at-compile-time
		 (list `(cond (c-recorder
			       (dolist (pn ',pnl)
				  (funcall c-recorder pn))))))
		(t !()))
	  (cond (at-run-time
		 (cons `(cond (r-recorder
			       (dolist (pn ',pnl)
				 (funcall r-recorder pn)
				 (pathname-fload pn false false false))))
		       (mapcan (\\ (pn)
				   (cond ((is-Pseudo-pathname pn)
					  (list-copy
					     (pseudo-pathname-insertables pn)))
					 (t !())))
			       pnl)))
		(t !()))))

;;; 2/3 of these lambdas can be alpha-reduced.

(datafun to-slurp depends-on
  (defun :^ (e slurp-if-substantive)
     (cond (depends-on-enabled*
	    (let ((groups (depends-on-args-group (cdr e))))
	       (labels ((recursive-slurp (pn)
			   (cond ((not (eq slurping-how-much*
				     ':header-only))
			    (pathname-slurp
			       pn false ':at-least-header)))))
		  (dolist (g groups)
		     (let ((pnl (filespecs->ytools-pathnames (cdr g)))
			   (pathname-handlers
			      (mapcar (\\ (m)
					 (ecase m
					     (:compile-time
					      #'(lambda (pn)
						   (pathname-fload pn false false false)
						   (funcall (lpr-comp-tdo-recorder
							       slurping-lprec*)
							    pn)))
					     (:run-time
					      #'(lambda (pn)
						   (recursive-slurp pn)
						   (funcall (lpr-run-tdo-recorder 
								slurping-lprec*)
							    pn)))
					     (:slurp-time
					      #'(lambda (pn)
	  ;;;;					   (format *error-output*
	  ;;;;					      "Loading pathname ~s~%" pn)
						   (pathname-fload pn false false false)
						   ;;;;(pathname-slurp pn false ':whole-file)
						   ))))
				      (car g))))
			(dolist (pn pnl)
;;;;	  		   (format *error-output* "Processing ~s howmuch = ~s~%"
;;;;				   pn slurping-how-much*)
			   (dolist (h pathname-handlers)
			      (funcall h pn))
			   (cond ((is-Pseudo-pathname pn)
				  (dolist (e (pseudo-pathname-insertables pn))
				     (form-slurp e slurp-if-substantive)))))))))))
     false))

(defun lpr-comp-tdo-recorder (lpr)
   #'(lambda (pn)
;;;;	(dbg-save lpr pn)
;;;;	(breakpoint lpr-comp-tdo-recorder-res
;;;;	   "lpr = " lpr :% "pn = " pn)
	(let ((compile-deps
		 (Load-progress-rec-compile-time-depends-on
		    lpr)))
	   (setq pn (pathname-resolve pn false))
	   (cond ((not (member pn compile-deps :test #'pn-equal))
		  (setf (Load-progress-rec-compile-time-depends-on lpr)
		    (nconc compile-deps (list pn))))))))

(defun lpr-run-tdo-recorder (lpr)
   #'(lambda (pn)
	(let ((run-deps
		 (Load-progress-rec-run-time-depends-on
		    lpr)))
	   (setq pn (pathname-resolve pn false))
	   (cond ((not (member pn run-deps :test #'pn-equal))
		  (setf (Load-progress-rec-run-time-depends-on lpr)
		    (nconc run-deps (list pn))))))))

;;; Return a list of groups, each of the form
;;; ((<time>*)
;;;  --filespecs--)
;;; where each <time> is either :run-time, :compile-time, :slurp-time
(defun depends-on-args-group (args)
   (cond ((and (= (len args) 1)
	       (is-Symbol (car args)))
	  ;;; Compatibility with old Nisp
	  (list `((:run-time :compile-time) %module/ ,(car args))))
	 (t
	  (let ((this-mode '(:run-time :compile-time :slurp-time))
		(this-group '())
		(groups '()))
	     (labels ((end-group ()
			 (cond ((not (null this-group))
				(setq groups
				      (cons (cons this-mode
						  (reverse this-group))
					    groups))))))
		(dolist (arg args)
		   (let ((next-mode
			    (cond ((memq arg '(at-run-time :at-run-time
					       at-compile-time :at-compile-time
					       :at-slurp-time
					       :always :at-run-and-compile-time))
				   (case arg
				      ((:at-run-time at-run-time)
				       '(:run-time))
				      ((:at-compile-time at-compile-time)
				       '(:compile-time))
				      ((:at-slurp-time)
				       '(:slurp-time))
				      (t
				       '(:compile-time :run-time :slurp-time))))
				  ((car-eq arg ':at)
				   (cond ((null (cdr arg))
					  (cerror "The next block will be ignored"
						  "depends-on with (:at): ~s"
						  `(depends-on ,@args))))
				   (cdr arg))
				  (t ':no-next-group))))
		      (cond ((eq next-mode ':no-next-group)
			     (setq this-group (cons arg this-group)))
			    (t
			     (end-group)
			     (setq this-mode next-mode)
			     (setq this-group '())))))
	     (end-group)
	     (reverse groups))))))

(defun pseudo-pathname-insertables (pn)
   (let ((expandfn (Pseudo-pathname-expander pn)))
      (cond (expandfn
	     (funcall expandfn pn))
	    (t !()))))

(defun module-parse (operands default-pathname)
                    (ignore default-pathname)
   (let ((remainder (or (member-if (\\ (x) (not (module-name-sym x)))
				   operands)
			!())))
      (cond ((not (null module-trace*))
	     (format *error-output*
		 "Preparing to process modules ~s~%"
		 (ldiff operands remainder))))
      (values (mapcar (\\ (rand)
			 (make-Module-pseudo-pn
			    :control 'module
			    :module rand))
		      (ldiff operands remainder))
	      false
	      remainder)))

(defun module-name-sym (x)
   (and (is-Symbol x)
	(not (char= (elt (Symbol-name x) 0)
		    #\%))))

(declaim (special slurping-lprec*))

(defun module-slurp (mpn _ howmuch)
   (let ((modname (Module-pseudo-pn-module mpn)))
      (let ((module (find-YT-module modname)))
	 (cond (module
		(let ((slurping-lprec*
		         (YT-module-rec module)))
		   (cond ((memq modname module-trace*)
			  (format *error-output*
			      "Considering slurping module ~s... ~%" modname)))
		   (cond ((achieved-load-status
			     slurping-lprec* ':slurped-all)
			  (cond ((memq modname module-trace*)
				 (format *error-output*
				    "Module ~s already slurped~%" modname))))
			 ((eq howmuch ':header-only)
			  (cond ((memq modname module-trace*)
				 (format *error-output*
				    "Not looking inside module ~s for supporters"
				    modname))))
			 (t
			  (setf (Load-progress-rec-run-time-depends-on
				   slurping-lprec*)
				!())
			  (setf (Load-progress-rec-compile-time-depends-on
				   slurping-lprec*)
				!())
			  (cond ((memq modname module-trace*)
				 (format *error-output*
				    "Slurping module ~s, ~% form = ~s"
				    modname (YT-module-contents module))))
			  (form-slurp (YT-module-contents module)
				      (eq howmuch ':whole-file))
;;;;			  (format t "Doing dependents of ~s~%"
;;;;				  slurping-lprec*)
			  ;; The following two forms were lifted from
			  ;; 'lprec-compile'
			  (dolist (rtsupp
				      (Load-progress-rec-run-time-depends-on
					 slurping-lprec*))
			     (pathname-slurp rtsupp false ':at-least-header))
			  (dolist (ctsupp
				    (Load-progress-rec-compile-time-depends-on
					 slurping-lprec*))
			     (pathname-fload ctsupp false false false))
			  (note-load-status slurping-lprec* ':slurped-all)))))
	       (t
		(error "Attempt to slurp non-module ~s"
		       modname))))))

(defun module-load (mod-pspn force-flag force-compile always-ask)
                   (ignore always-ask)
   (let ((modname (Module-pseudo-pn-module mod-pspn)))
      (let ((fload-compile* (or force-compile fload-compile*)))
	 (ytools-module-load modname force-flag))))

(defun module-compile (mod-pspn force-flag)
   (let ((modname (Module-pseudo-pn-module mod-pspn)))
      (let ((fload-compile* ':compile))
	 (ytools-module-load modname force-flag))))

(defun module-expansion (mod-pspn)
   (let ((modname (Module-pseudo-pn-module mod-pspn)))
      (let ((mod (find-YT-module modname)))
	 (cond (mod
		(YT-module-expansion mod))
	       (t
		(error "Undefined YTools module ~s" modname))))))

(def-ytools-pathname-control module
     :parser #'module-parse
     :slurper #'module-slurp
     :loader #'module-load
     :compiler #'module-compile
     :expander #'module-expansion)

(defun pathname-sym (pn)
   (intern (file-name-as-symbol-name (Pathname-name pn))))

(defun file-name-as-symbol-name (name)
   (let ((an (string-case-analyze name)))
      (cond ((or (eq an ':mixed)
		 (eq lisp-preferred-case* an))
	     name)
	    ((eq lisp-preferred-case* ':lower)
	     (string-downcase name))
	    (t
	     (string-upcase name)))))

(defvar patch-files* '())

(defun load-patch-file-spec (pf)
       (let ((files (filespecs->pathnames (cdr pf))))
	  (dolist (file files)
	     (cond ((probe-file file)
		    (pathname-fload file false false false))))))

(defmacro load-patch-file (filename^ label^)
   `(probe-and-load-patch-file ,filename^ ,label^))

(defun probe-and-load-patch-file (name label)
   (let ((pname (->pathname
		    (concatenate 'string
		        name "-"
			(substitute #\- #\. label)))))
      (let ((rname (or (get-pathname-with-suffixes pname object-suffixes*)
		       (get-pathname-with-suffixes pname source-suffixes*))))
	 (cond (rname
		(lprec-load (place-load-progress-rec rname)
			    false false false))))))

(declaim (special final-load*))

(defun note-patch-files (module-name files)
   (let ((p (assq module-name patch-files*)))
      (cond ((not p)
	     (setf p (list module-name))
	     (setf patch-files* (cons p patch-files*)))   )
      (setf (cdr p) files)
      (cond ((not final-load*)
             (load-patch-file-spec p)))))

(def-ytools-module ytools
	 (eval-when (:execute :compile-toplevel :load-toplevel)
	    (setq *readtable* ytools-readtable*))
;;;;         #+allegro
;;;;	 (depends-on :at-run-time %ytools/ prompthack)
	 (depends-on %ytools/ multilet)
	 (depends-on :at-run-time %ytools/ signal misc)
	 (depends-on (:at :slurp-time :compile-time) %ytools/ setter)
	 (depends-on :at-compile-time 
		     %ytools/ object mapper)
	 (note-patch-files 'all-ytools '(%ytools/ "ytools-patches.nsp")))

(def-excl-dispatch #\' (srm _)
   (list 'funktion (read srm true nil true)))

;;;;  (let ((f (read srm)))
;;;;     (cond ((atom f) (list 'funktion f))
;;;;	   (t f))))

(defmacro funktion (f)
   (cond ((and (atom f) (> debuggability* 0))
	  `',f)
	 (t `#',f)))