;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: module.lisp,v 1.9.2.9 2005/01/28 13:24:37 airfoyle Exp $

;;; Copyright (C) 1976-2004
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(def-ytools-module import-export module-trace*)))

(defstruct (YT-module
	      (:constructor make-YT-module
			    (name contents
			     &aux (rec false) (loaded false) (postponed !())))
	      (:predicate is-YT-module))
   name
   contents     
   expansion
   rec    ; Load-progress-rec
   loaded) 

;;; An alist of <name, yt-module> pairs.
(defvar ytools-modules* !())

(defstruct (Module-pseudo-pn (:include Pseudo-pathname))
   module)

;;; (def-ytools-module name 
;;;    (:expansion ---acts---)
;;;    (:run-support ---acts---)
;;;    (:compile-support ---acts---))
;;; Let F be a file that depends-on this module.
;;; :expansion: The acts are (as it were) inserted in the
;;;    top level of F.
;;; :run-support: The acts are executed when F is loaded
;;; :compile-support: The acts are executed when F is compiled
;;; Note that this is orthogonal to the :at-run-time/:at-compile-time
;;; distinction.  If F depends-on a module :at-compile-time, that
;;; means that the :run-support actions of F are executed when F is
;;; compiled.
;;; Actions at the top level of the macro are treated as though
;;; they occurred inside :run-support (and a warning is issued).
;;; :contents is a synonym of :run-support, and :insert is a synonym
;;; for :expansion.
(defmacro def-ytools-module (name &rest actions &whole dym-exp)
   (multiple-value-bind (run-support compile-support expansion warn)
		        (do ((al actions (cdr al))
			     e
			     (rs !())
			     (cs !()) 
			     (expans !())
			     (warn false))
			    ((null al)
			     (values (mapcan #'list-copy (reverse rs))
				     (mapcan #'list-copy (reverse cs))
				     (mapcan #'list-copy (reverse expans))
				     warn))
			  (setq e (car al))
			  (cond ((atom e)
				 (on-list (list e) rs)
				 (setq warn t))
				((memq (car e) '(:run-support :contents))
				 (on-list (cdr e) conts))
				((memq (car e) '(:expansion :insert))
				 (on-list (cdr e) expans))
				(t
				 (on-list (list e) conts)
				 (setq warn t))))
      (cond (warn
	     (format *error-output*
		"Old-style: ~s~%" dym-exp)))
      `(let ((mod (place-YT-module ',name)))
	  (setf (YT-module-contents mod)
		',(cond ((= (length contents) 1)
			 (car contents))
			(t `(progn ,@contents))))
	  (setf (YT-module-expansion mod) ',expansion)
;;;;	  (note-module-def-time mod)
	  )))

(def-ytools-pathname-control module
   (defun :^ (operands _)
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
		 remainder))))

(defun place-YT-module (name)
   (val-or-initialize
      (alref ytools-modules* name)
      :init (let ((pn (make-Pathname :directory
				      '(:relative "%ytools#module")
				      :name
				      (symbol-name name))))


	        (let ((lpr (make-Load-progress-rec
			      :pathname (make-Module-pseudo-pn
					   :control 'module
					   :module name))))
		   (setf (YT-module-rec (cadr p))
		         lpr)
		   (setf (pathname-prop 'load-progress-rec pn)
		         lpr)))
	     (setq ytools-modules* (cons p ytools-modules*))))
      (cadr p)))

(defun find-YT-module (name)
   (alref ytools-modules* name))

(defvar loaded-ytools-modules* !())



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

(defun pn-ur-version (pn)
   (or (pathname-source-version pn)
       (pathname-object-version pn true)))