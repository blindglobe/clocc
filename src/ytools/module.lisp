;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: module.lisp,v 1.9.2.11 2005/02/01 12:30:36 airfoyle Exp $

;;; Copyright (C) 1976-2004
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(def-ytools-module import-export module-trace*)))

(defstruct (YT-module
	      (:constructor make-YT-module
			    (name contents
			     &aux (chunk false) (loaded-chunk false)))
	      (:predicate is-YT-module)
	      (:print-object 
	          (lambda (mod srm)
		     (format srm "#<YT-module ~s>" (YT-module-name mod)))))
   name
   contents     
   chunk
   loaded-chunk)

;;; An alist of <name, yt-module> pairs.
(defvar ytools-modules* !())

(defstruct (Module-pseudo-pn (:include Pseudo-pathname))
   module)

;;; (def-ytools-module name 
;;;     -act-specs-)
;;; Where act-spec is (timespec -acts-)
;;; timespec is a list of symbols drawn from 
;;; :run-support, :compile-support, :expansion,

;;; Let F be a file that depends-on this module.
;;; :expansion: The acts are (as it were) inserted in the
;;;    top level of F.
;;; :run-support: The acts are executed when F is loaded
;;; :compile-support: The acts are executed when F is compiled
;;; If act-spec is not of the form (timespec -acts-), it is treated 
;;; as ((:run-support :compile-support) act-spec).
;;; If timespec is one the three symbols above, it is treated as a 
;;; singleton list of that symbol.
;;; Note that this is orthogonal to the :at-run-time/:at-compile-time
;;; distinction.  If F depends-on a module :at-compile-time, that
;;; means that the :run-support actions of F are executed when F is
;;; compiled.
(defmacro def-ytools-module (name &rest actions &whole dym-exp)
   (labels ((is-timespec (x)
	        (memq x '(:run-support :compile-support :expansion))))
      (setq actions
	    (mapcan (\\ (a)
		       (cond ((atom a)
			      (format *error-output*
				 !"Ignoring meaningless actspec in ~
				   'def-ytools-module': ~s"
				 a)
			      (list))
			     ((listp (car a))
			      (cond ((every #'is-timespec (car a))
				     (list a))
				    (t
				     (list `((:run-support :compile-support)
					     ,@a)))))
			     ((is-timespec (car a))
			      (list `((,(car a)) ,@(cdr a))))
			     (t
			      (list `((:run-support :compile-support)
				      ,a)))))
		    actions))
      `(let ((mod (place-YT-module ',name)))
	  (setf (YT-module-contents mod)
		actions))))

(defun place-YT-module (name)
   (or (lookup-YT-module name)
       (let ((mod (make-YT-module name false)))
	  (on-list (tuple name mod)
		   ytools-modules*)
	  mod)))

(defun lookup-YT-module (name)
   (let ((e (assq name ytools-modules*)))
      (and e (second e))))

(defclass YT-module-chunk (Chunk)
   ((module :reader YT-module-chunk-module
	    :initarg :module
	    :type YT-module)))

;;; loadee is a YT-module
(defclass Loaded-module-chunk (Loaded-chunk))

(def-ytools-pathname-control module
   (defun :^ (operands _)
      (let ((remainder (or (member-if (\\ (x) (not (lookup-YT-module x)))
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
		 remainder))))

(defmethod pathname-denotation-chunk ((mod-pspn Module-pseudo-pn))
   (place-YT-module-chunk (Module-pseudo-pn-module mod-pspn)))

(defun place-YT-module-chunk (mod)
   (chunk-with-name `(:YT-module ,(YT-module-name mod))
      (\\ (name)
	 (make-instance 'YT-module-chunk
	    :module mod))))

(defvar loaded-ytools-modules* !())

;;; This is isomorphic to the method for pathnames, so perhaps they
;;; should be a shared procedure they both call.
(defmethod filoid-fload ((yt-mod YT-module)
			 &key force-load file-manip)
   (let* ((mod-chunk (place-YT-module-chunk yt-mod))
	  (lmod-chunk (place-Loaded-module-chunk mod-chunk)))
      (monitor-filoid-basis lmod-chunk)
      (cond ((Chunk-managed lmod-chunk)
	     (cond (force-load
		    (chunk-derive-and-record lmod-chunk)
		    (chunks-update (Chunk-derivees lmod-chunk)))))
	    (t
	     (chunk-request-mgt lmod-chunk)
	     (chunk-update lmod-chunk)))))

;;; Second arg is apparently not useful (as of now) --
(defmethod place-Loaded-chunk ((mod-ch YT-module-chunk) _)
   (place-Loaded-module-chunk mod-ch))

(defun place-Loaded-module-chunk (mod-chunk)
   (let ((lc (chunk-with-name `(:loaded ,(YT-module-chunk-module mod-chunk))
		(\\ (name)
		  (let ((is-source (file-chunk-is-source file-chunk)))
		     (let ((compiled-chunk
			      (and is-source
				   (place-compiled-chunk file-chunk))))
			(let ((new-lc
				 (make-instance 'Loaded-module-chunk
				    :name name
				    :loadee mod-chunk)))
			   new-lc))))
		:initializer
		   (\\ (new-lc)
		      (cond ((not (slot-truly-filled new-lc 'controller))
			     (setf (Loaded-chunk-controller new-lc)
				   (create-loaded-controller
				      mod-chunk new-lc))))))))
      (cond ((eq file-manip ':noload)
	     (chunk-terminate-mgt lc ':ask))
	    ((and file-manip
		  (not (eq file-manip (Loaded-chunk-manip lc))))
	     (setf (Loaded-chunk-manip lc) file-manip)))
      lc))

(defclass Loadable-module-chunk (Loadable-chunk))

(defmethod create-loaded-controller ((mod-ch Module-chunk)
				     (loaded-ch Loaded-module-chunk))
   (chunk-with-name `(:loadable ,(Chunk-name mod-ch))
      (\\ (name)
	 (make-instance 'Loadable-module-chunk
	    :controllee loaded-ch
	    :name name))))

(defmethod derive ((mod-controller Loadable-module-chunk))
   (let* ((loaded-mod-chunk
		(Loadable-chunk-controllee mod-controller))
	  (mod-chunk
	     (Loaded-chunk-loadee loaded-mod-chunk))
	  (module (YT-module-chunk-module mod-chunk))
	  (clal (YT-module-contents module)))
      (dolist (al clal)
	 (let (;;;;(times (first al))
	       ;; -- times are irrelevant in this context.
	       (acts (rest al)))
	    (dolist (a acts)
;;; Okay, what we really want to do here is pretend we're scanning
;;; a file, but that would require parameterizing scanning so it
;;; works with pseudo-files.  If we can't do it right, no sense in
;;; doing it in a half-assed way for which there is no demand. --	      
;;;;	       (loop
;;;;		  (cond ((and (consp a)
;;;;			      (is-Symbol (car a))
;;;;			      (macro-function (car a))
;;;;			      (not (eq (car a) 'depends-on)))
;;;;			 (setq a (macroexpand-1 a)))
;;;;			(t
;;;;			 (return))))
	      (cond ((car-eq a 'depends-on)
		     (let ((groups (depends-on-args-group (cdr a))))
		        ;; What 'depends-on-args-group' returns is not
		        ;; really tailored to this context, where
		        ;; :run-time, :compile-time, etc. don't mean
		        ;; anything.
		        (dolist (g groups)
			   (cond ((not (memq ':run-time (first g)))
				  (format *error-output*
				     !"Warning: Meaningless to have ~
                                       ':compile-time' dependency in module ~s"
				     (Module-name module))))
			   (let* ((dependee-chunks
				     (mapcar #'pathname-denotation-chunk
					     (filespecs->ytools-pathnames
						(cdr g))))
				  (loaded-dependee-chunks
				     (mapcar (\\ (fc)
						(place-Loaded-chunk fc false))
					     dependee-chunks)))
			      (loaded-chunk-augment-basis
			          loaded-mod-chunk dependee-chunks)))))))))))

(defvar module-now-loading* false)


;;; List of names of modules we're interested in --
(defvar module-trace* !())

;;; THIS IS USED by .lmd files, so don't just delete it!

(defun ytools-module-load (name force-flag)
;;;;  (breakpoint ytools-module-load
;;;;     "Loading module " name)
   (let ((ytm (find-YT-module name)))
      (cond (ytm
	     (let ((module-now-loading* name)
		   (now-loading-lprec* (YT-module-rec ytm)))
		(cond ((or force-flag
			   (not (achieved-load-status now-loading-lprec* ':loaded)))
		       (cond ((memq name module-trace*)
			      (format *error-output*
				 "Loading module ~s Compiling? ~a~%"
				 name (cond ((eq fload-compile* ':compile)
					     "Yes")
					    ((memq fload-compile*
						   '(:object :source))
					     "No")
					    (t
					     "Maybe")))))
		       (with-compilation-unit ()
			  (eval (YT-module-contents ytm))
			  (dolist (e (YT-module-expansion ytm))
			     (eval e)))
		       (note-load-status now-loading-lprec* ':loaded))
		      ((memq name module-trace*)
		       (format *error-output*
			  "Not loading module ~s, because it's already loaded~%"
			  name))))
	     `("Module" ,name "loaded"))
	    (t
	     (cerror "The non-module will be ignored"
		     "Attempt to load nonexistent module ~s"
		     name)))))

(defun import-export (from-pkg-desig strings
		      &optional (exporting-pkg-desig *package*))
   (let ((from-pkg 
	    (cond ((or (is-Symbol from-pkg-desig)
		       (is-String from-pkg-desig))
		   (find-package from-pkg-desig))
		  (t
		   from-pkg-desig)))
	 (exporting-pkg
	     (cond ((or (is-Symbol exporting-pkg-desig)
			(is-String exporting-pkg-desig))
		    (find-package exporting-pkg-desig))
		   (t
		    exporting-pkg-desig))))
   (cond ((and (packagep from-pkg)
	       (packagep exporting-pkg))
	  (dolist (str strings)
	     (cond ((is-Symbol str)
		    (setq str (symbol-name str))))
	     (let ((sym (find-symbol str from-pkg)))
	        (cond ((not sym)
		       (cerror (format nil "I'll put symbol ~a into ~s"
				       str from-pkg)
			       "Symbol with name ~s does not exist in ~s, and so can't ~
                                be exported from ~s"
			 str from-pkg exporting-pkg)
		       (setq sym (intern str from-pkg))))
		(import (list sym) exporting-pkg)
		(export (list sym) exporting-pkg))))
	 ((not (packagep from-pkg))
	  (error "~s does not designate a package"
		 from-pkg-desig))
	 (t
	  (error "~s does not designate a package"
		 exporting-pkg-desig)))))

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
      (bind-fload-compile* (or force-compile fload-compile*)
	 (ytools-module-load modname force-flag))))

(defun module-compile (mod-pspn force-flag)
   (let ((modname (Module-pseudo-pn-module mod-pspn)))
      (bind-fload-compile* ':compile
	 (ytools-module-load modname force-flag))))

(defun module-expansion (mod-pspn)
   (let ((modname (Module-pseudo-pn-module mod-pspn)))
      (let ((mod (find-YT-module modname)))
	 (cond (mod
		(YT-module-expansion mod))
	       (t
		(error "Undefined YTools module ~s" modname))))))


#|
;;; Example: 
(def-ytools-module nisp
   (:run-support
      (depends-on %ytools/ nilscompat)
      (depends-on %module/ ydecl-kernel))
   (:compile-support (depends-on %ydecl/ compnisp))
   (:expansion
       (self-compile-dep :macros)
       (callees-get-nisp-type-slurp))
   )

(datafun scan-depends-on callees-get-nisp-type-slurp
   (defun :^ (d sdo-state)
      (setf (Sds-sub-file-types sdo-state)
	    (adjoin nisp-type-slurp*
		    (Sds-sub-file-types sdo-state)))))


when scanning a sub-file for nisp types, the scan *dies* if you don't see
(depends-on %module/ nisp) before the end of the header

|#

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
   (:run-support
	 (eval-when (:execute :compile-toplevel :load-toplevel)
	    (setq *readtable* ytools-readtable*))
;;;;         #+allegro
;;;;	 (depends-on :at-run-time %ytools/ prompthack)
	 (depends-on %ytools/ multilet)
	 (depends-on :at-run-time %ytools/ signal misc)
	 (depends-on (:at :slurp-time :compile-time) %ytools/ setter)
	 (depends-on :at-compile-time 
		     %ytools/ object mapper)
	 (note-patch-files 'all-ytools '(%ytools/ "ytools-patches.nsp"))))

(defun pn-ur-version (pn)
   (or (pathname-source-version pn)
       (pathname-object-version pn true)))