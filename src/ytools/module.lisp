;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: module.lisp,v 1.9.2.3 2004/11/24 04:24:01 airfoyle Exp $

;;; Copyright (C) 1976-2003 
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
   loaded
   postponed) 

;;; An alist of <name, yt-module> pairs.
(defvar ytools-modules* !())

;;; (def-ytools-module name ---acts--- (:insert ---acts---))
;;; The first group of acts becomes a (run-time or compile-time) supporter
;;; of the file C containing the 'def-ytools-module' form.  The ':insert'
;;; acts are treated as if inserted at this point in C.
(defmacro def-ytools-module (name &rest actions)
   (multiple-value-bind (contents expansion)
		        (do ((al actions (cdr al))
			     e
			     (conts !())
			     (expans !()))
			    ((null al)
			     (values (mapcan #'list-copy (reverse conts))
				     (mapcan #'list-copy (reverse expans))))
			  (setq e (car al))
			  (cond ((atom e)
				 (on-list (list e) conts))
				((eq (car e) ':contents)
				 (on-list (cdr e) conts))
				((memq (car e) '(:insert :expansion))
				 (on-list (cdr e) expans))
				(t
				 (on-list (list e) conts))))
      `(let ((mod (place-YT-module ',name)))
	  (setf (YT-module-contents mod)
		',(cond ((= (length contents) 1)
			 (car contents))
			(t `(progn ,@contents))))
	  (setf (YT-module-expansion mod) ',expansion)
;;;;	  (note-module-def-time mod)
	  )))

(def-ytools-pathname-control module)

(defun place-YT-module (name)
   (let ((p (assq name ytools-modules*)))
      (cond ((not p)
	     (setq p (tuple name (make-YT-module name '(values))))
	     (let ((pn (make-Pathname :directory
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
   (let ((p (assq name ytools-modules*)))
      (and p (cadr p))))

(defvar loaded-ytools-modules* !())

(defvar source-suffixes* (adjoin lisp-source-extn* '("lisp") :test #'equal))
(defvar obj-suffix* lisp-object-extn*)

(defconstant can-get-write-times*
    #.(not (not (file-write-date
		    (concatenate 'string ytools-home-dir* "files.lisp")))))
(defun pathname-source-version (pn)
  (cond ((is-Pseudo-pathname pn) false)
	(t
	 (let ((rpn (cond ((is-Pathname pn) pn)
			  (t (pathname-resolve pn false)))))
	    (let ((pn-type (Pathname-type rpn)))
	       (cond (pn-type
		      (cond ((equal pn-type obj-suffix*)
			     (get-pathname-with-suffixes
				rpn source-suffixes*))
			    ((probe-file rpn)
			     rpn)
			    (t false)))
		     ((probe-file rpn) rpn)
		     (t (get-pathname-with-suffixes rpn source-suffixes*))))))))

(defun pathname-object-version (pn only-if-exists)
   (let ((ob-pn
	    (pathname-find-associate pn 'obj-version obj-suffix* only-if-exists)))
      (cond ((and (not only-if-exists)
		  (not ob-pn))
	     (cerror "I will treat it as :unknown"
		     "Pathname has no object version: ~s" ob-pn)
	     ':none)
	    (t ob-pn))))

(defun pathname-write-time (pname)
  (setq pname (pathname-resolve pname false))
  (and can-get-write-times*
       (probe-file pname)
       (file-write-date pname)))

;;; pn must be a resolved Pathname, not a YTools Pathname.
(defun get-pathname-with-suffixes (pn suffixes)
   (do ((sfl suffixes (cdr sfl))
	(found false)
	newpn)
       ((or found (null sfl))
	(and found newpn))
      (setq newpn (merge-pathnames
		     (make-Pathname :type (car sfl))
		     pn))
      (cond ((probe-file newpn)
	     (setq found true)))))

(defvar module-now-loading* false)

;;; List of names of modules we're interested in --
(defvar module-trace* !())


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

(defun note-ytools-module (ytm)
   (let ((name (YT-module-name ytm)))
     (cond ((not (null (YT-module-postponed ytm)))
	    (note-module-postponement
	       `(check-module-postponed ',name))))
     (setq loaded-ytools-modules*
           (adjoin name loaded-ytools-modules* :test #'eq))))

(defun check-module-postponed (name)
  (let ((ytm (place-YT-module name)))
    (let ((p (YT-module-postponed ytm)))
     (cond ((not (null p))
	    (setf (YT-module-postponed ytm) !())
	    (let ((module-now-loading* name))
	       (dolist (e (reverse p))
		  (eval e)))
	    (cond ((null (YT-module-postponed ytm))
		   `("Finished loading module" ,name))
		  (t
		   (note-module-postponement
		      `(check-module-postponed ',name))
		   `("Module postponed" ,name))))
	 (t
	  `("Already loaded module" ,name))))))

(defun note-module-postponement (vl)
   (cond (module-now-loading*
	  (let ((ytm (place-YT-module module-now-loading*)))
	     (setf (YT-module-postponed ytm)
	           (adjoin vl (YT-module-postponed ytm)
			   :test #'equal))))))

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