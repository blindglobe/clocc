;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: module.lisp,v 1.9.2.14 2005/02/06 05:46:32 airfoyle Exp $

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

;;; Note that YT-module-chunk is the name of the accessor of the 'chunk'
;;; slot of 'YT-module', and also the name of a class.  Have fun keeping
;;; track of them.

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
				 !"Ignoring meaningless actspec ~s in ~
				   'def-ytools-module': ~s"
				 a dym-exp)
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
		',actions))))

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

(defmethod derive ((yt-mod YT-module-chunk))
   false)

;;; loadee is a YT-module
(defclass Loaded-module-chunk (Loaded-chunk)
   ())

;;; List of names of modules we're interested in --
(defvar module-trace* !())

(def-ytools-pathname-control module
   (defun :^ (operands _)
      (do ((opl operands (cdr opl))
	   mod
	   (mods !()))
	  ((or (null opl)
	       (not (setq mod (lookup-YT-module (car opl)))))
	   (let ((remainder opl))
	      (cond ((not (null module-trace*))
		     (format *error-output*
			 "Preparing to process modules ~s~%"
			 mods)))
	      (values (mapcar (\\ (mod)
				 (make-Module-pseudo-pn
				    :name (YT-module-name mod)
				    :module mod))
			      mods)
		      false
		      remainder)))
         (on-list mod mods))))

(defmethod pathname-denotation-chunk ((mod-pspn Module-pseudo-pn))
   (place-YT-module-chunk (Module-pseudo-pn-module mod-pspn)))

(defun place-YT-module-chunk (mod)
   (or (YT-module-chunk mod)
       (let ((mod-ch 
		(chunk-with-name `(:YT-module ,(YT-module-name mod))
		   (\\ (name)
		      (make-instance 'YT-module-chunk
			 :name name
			 :module mod)))))
	  (setf (YT-module-chunk mod)
		mod-ch)
	  mod-ch)))

;;; The key hack in the following two items is that a YT-module's
;;; dependencies are a subset of the files it will load, possibly
;;; an empty subset.
;;; But if there's some other reason to reload or recompile,
;;; all the relevant forms from its contents must be updated,
;;; and in general they will load several files-- 

(defmethod pathname-compile-support ((pspn Module-pseudo-pn))
   (let ((mod (Module-pseudo-pn-module pspn)))
      (values
           (module-loaded-prereqs mod)
	   (module-form-chunks mod ':compile-support))))

(defmethod pathname-run-support ((pspn Module-pseudo-pn))
   (let ((mod (Module-pseudo-pn-module pspn)))
      (values
           (module-loaded-prereqs mod)
	   (list (place-Loaded-module-chunk
		    (place-YT-module-chunk mod)
		    false)))))

(defun module-form-chunks (mod which)
	   (let ((rforms
		    (retain-if
		       (\\ (e) (memq which (first e)))
		       (YT-module-contents mod))))
	      (cond ((null rforms)
		     !())
		    (t
		     (list (place-Form-chunk
			      `(progn ,@(mapcan (\\ (e)
						   (list-copy (rest e)))
						rforms))))))))

(defun module-loaded-prereqs (mod)
   (let ((loaded-ch (place-Loaded-module-chunk
		        (place-YT-module-chunk mod)
			false)))
      (loaded-chunk-verify-basis loaded-ch)))

(defvar loaded-ytools-modules* !())

(defmethod filoid-fload ((yt-mod Module-pseudo-pn)
			 &key force-load manip)
   (let* ((module (Module-pseudo-pn-module yt-mod))
	  (mod-chunk (place-YT-module-chunk module))
	  (lmod-chunk (place-Loaded-module-chunk
		         mod-chunk manip)))
      (loaded-chunk-fload lmod-chunk force-load)))

(defmethod place-Loaded-chunk ((mod-ch YT-module-chunk) mod-manip)
   (place-Loaded-module-chunk mod-ch mod-manip))

(defun place-Loaded-module-chunk (mod-chunk mod-manip)
   (let ((module (YT-module-chunk-module mod-chunk)))
      (or (YT-module-loaded-chunk module)
	  (let ((lc (chunk-with-name `(:loaded ,module)
		       (\\ (name)
			  (let ((new-lc
				   (make-instance 'Loaded-module-chunk
				      :name name
				      :loadee mod-chunk)))
			     new-lc))
		       :initializer
			  (\\ (new-lc)
			     (cond ((not (slot-truly-filled new-lc 'controller))
				    (setf (Loaded-chunk-controller new-lc)
					  (create-loaded-controller
					     mod-chunk new-lc))))))))
	     (cond ((eq mod-manip ':noload)
		    (chunk-terminate-mgt lc ':ask))
       ;;;;	    ((and mod-manip
       ;;;;		  (not (eq mod-manip (Loaded-chunk-manip lc))))
       ;;;;	     (setf (Loaded-chunk-manip lc) mod-manip))
	      )
	     (setf (YT-module-loaded-chunk module)
		   lc)
	     lc))))

(defclass Loadable-module-chunk (Loadable-chunk)
   ())

(defmethod create-loaded-controller ((mod-ch YT-module-chunk)
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
			    (cond ((memq ':run-time (first g))
				   (let ((pnl (filespecs->ytools-pathnames
					         (cdr g))))
				      (pathnames-note-run-support
					  pnl false
					  loaded-mod-chunk)
				      (dolist (pn pnl)
					 (monitor-filoid-basis pn))))
				  (t
				   (format *error-output*
				      !"Warning: Meaningless to have ~
					':compile-time' dependency in ~
                                        module ~s"
				      (YT-module-name module))))))))))))
   true)

(defvar module-now-loading* false)

(defmethod derive ((lmod-ch Loaded-module-chunk))
   (let ((module
	    (YT-module-chunk-module
	       (Loaded-chunk-loadee lmod-ch))))
      (dolist (c (YT-module-contents module))
	 (cond ((memq ':run-support (first c))
		(dolist (e (rest c))
		   (eval e)))))))

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

(def-ytools-module ytools
   (let ((*readtable* ytools-readtable*))
      (fload %ytools/ multilet)
      (fload %ytools/ signal misc)
      (fload %ytools/ setter)
      (fload %ytools/ object mapper)
    ))

(defun ytools-module-load (name)
   (let ((yt-mod (lookup-YT-module name)))
      (cond (yt-mod
	     (let ((chl (module-form-chunks yt-mod ':run-support)))
	        (dolist (ch chl)
		   (chunk-request-mgt ch))
		(chunks-update chl)))
	    (t
	     (error "Can't load YTools module -- undefined")))))
