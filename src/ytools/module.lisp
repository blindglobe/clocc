;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

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

(defstruct (Module-pseudo-pn (:include Pseudo-pathname)
	       (:print-object
		   (lambda (mod-pspn srm)
		      (format srm "#<Module-pseudo-pn ~s>"
			      (Module-pseudo-pn-module mod-pspn)))))
   module)

(defmethod make-load-form ((module-pspn Module-pseudo-pn) &optional env)
   (declare (ignore env))
   `(make-Module-pseudo-pn
        :control ',(Pseudo-pathname-control module-pspn)
	:opvec false
	:module ',(Module-pseudo-pn-module module-pspn)))

(defmethod pn-equal ((pn1 Module-pseudo-pn) (pn2 Module-pseudo-pn))
   (eq (Module-pseudo-pn-module pn1)
       (Module-pseudo-pn-module pn2)))

(defun place-YT-module (name)
   (let ((p (assq name ytools-modules*)))
      (cond ((not p)
	     (setq p (tuple name (make-YT-module name '(values))))
	     (let ((pn (make-Pathname :directory
				      '(:relative "%ytools:module")
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

;;; Number of "fload episodes," that is, top-level calls to 'fload' (or 'fcompl')
(defvar file-op-count* 0)
;;; 'fload' won't be defined until files.lisp .

(defstruct (Load-progress-rec
	      (:print-function
	         (lambda (lpr srm lev)
		    (declare (ignore lev))
		    (let ((pn (Load-progress-rec-pathname lpr)))
		       (format srm "#<Load-progress-rec ~a: ~s~a>"
			       (Load-progress-rec-status lpr)
			       pn
			       (cond ((= (Load-progress-rec-status-timestamp lpr)
					 file-op-count*)
				      "*")
				     (t ""))))))
	    (:predicate is-Load-progress-rec))
   pathname ; Pathname, some with artificial directory names like "%module"
   (source-pathname ':unknown)  ; :none, :undef, or source pathanme
   (object-pathname ':unknown)  ; ditto

   (status ':unseen) ; See parameter +load-progress-progression+ below
   (when-reached -1 :type integer) ; date when status was reached
   (status-timestamp 0 :type integer)
   ;; -- Value of file-op-count* the last time the status of this
   ;; Load-progress-rec was set or checked.

   ;; The "ur-version" is the source, or the object if there is no
   ;; source.  The "loadable version" is the object, or the source if
   ;; the file is not to be compiled.
   ;; These are the last times the files in question were modified --
   (ur-mod-time -1 :type integer)
   (loadable-mod-time -1 :type integer)
   ;; The number of the last file-op-episode where these
   ;; mod times were calculated --
   (file-mod-timestamp 0 :type integer)

   (run-time-depends-on !())
   (compile-time-depends-on !())
   ;; -- lists of pathnames, one per entity this one
   (supporters !())
   ;; -- transitive closure of run-time- and compile-time-depends-on's,
   ;; EXCEPT that supporters includes only real pn's not Pseudo's.
   ;; depends on
   (supp-timestamp 0 :type integer)
   ;; -- Value of file-op-count* the last time the supporters
   ;; were computed.

   ;; Answers question: If this were to be reloaded, should we recompile-reload
   ;; or do something else? -- 
   (whether-compile ':unknown)
   ;; -- :unknown, :compile, :object, :source, :ask
   ;; See comment before defvar of fload-compile*, below

)
;;; Timestamp schemes assume that no one but us writes files during a
;;; "file-op episode," that is, during a top-level call to fload,
;;; fcompl, or fslurp.  E.g., you don't save a file buffer for a file
;;; that might get loaded.  Of course, if you do save such a buffer,
;;; you don't know which version will actually get loaded, so the
;;; assumption is reasonable.  Another reason it's reasonable is that
;;; fload episodes are short (a few seconds).

(defvar source-suffixes* (adjoin lisp-source-extn* '("lisp") :test #'equal))
(defvar obj-suffix* lisp-object-extn*)

(defconstant +load-progress-progression+
    '(:unseen :header-checked :slurped :slurped-all
      :maybe-compiled :loaded :frozen))
;;; -- :maybe-compiled usually means "compiled," but if a file is not
;;; supposed to be compiled, it means we checked whether it was supposed to
;;; be and noted that it wasn't.  
;;; :frozen means that the file was loaded as part of building a core image
;;; Any attempt to change it should cause alarms to go off.

(defconstant can-get-write-times*
    #.(not (not (file-write-date
		    (concatenate 'string ytools-home-dir* "files.lisp")))))

;;;;(defvar write-time-calls* 0)
;;;;(defvar redundant-write-time-calls* 0)

(defun achieved-load-status (lprec status)
   (let ((tl (memq (lprec-get-load-status lprec)
		   +load-progress-progression+)))
      (cond (tl
	     (not (memq status (cdr tl))))
	    (t
	     (error "Illegal Load-progress-rec status ~s"
		    (Load-progress-rec-status lprec))))))

(defun lprec-get-load-status (lprec)
   (let ((stat-time (Load-progress-rec-status-timestamp lprec)))
      (cond ((not (= stat-time file-op-count*))
	     (cond ((not (eq (Load-progress-rec-status lprec)
			     ':unseen))
		    (setf (Load-progress-rec-status lprec)
			  (recompute-load-status lprec))))
	     (setf (Load-progress-rec-status-timestamp lprec)
	           file-op-count*)))
      (Load-progress-rec-status lprec)))

(defun recompute-load-status (lprec)
   (multiple-value-bind (ur-mod-time ldble-mod-time)
                        (lprec-find-version-modtimes lprec)
      (let ((stat-time
	       (Load-progress-rec-when-reached lprec)))
	 (cond ((> ur-mod-time stat-time)
		':unseen)
	       ((and ldble-mod-time
		     (> ldble-mod-time stat-time)
		     (eq (Load-progress-rec-status lprec)
			 ':loaded))
		':maybe-compiled)
	       (t (Load-progress-rec-status lprec))))))

(defun note-load-status (lprec status)
   (cond ((and (eq (Load-progress-rec-status lprec)
		   ':frozen)
	       (not (eq status ':frozen)))
	  (cerror "I'll continue, but please don't try to save a core image"
		  "Attempt to change status of frozen Load-progress-rec to ~s"
		  status)))
   (setf (Load-progress-rec-status lprec)
	 status)
   (setf (Load-progress-rec-when-reached lprec)
	 (get-universal-time))
   (setf (Load-progress-rec-status-timestamp lprec)
          file-op-count*))

;;;;   (let ((tl (memq (Load-progress-rec-status lprec)
;;;;		   +load-progress-progression+)))
;;;;      (cond (tl
;;;;	     (cond ((memq status (cdr tl))
;;;;		    (setf (Load-progress-rec-status lprec)
;;;;			  status)
;;;;		    (setf (Load-progress-rec-when-reached lprec)
;;;;			  (get-universal-time))))
;;;;	     (setf (Load-progress-rec-status-timestamp lprec)
;;;;		   file-op-count*))
;;;;	    (t
;;;;	     (error "Illegal Load-progress-rec status ~s"
;;;;		    status)))))

;;; Principle: You ask whether to compile a file only if a "yes" answer
;;; will cause it to be compiled.  On other occasions, you guess.
(defun lprec-guess-whether-compile (lprec)
   (let ((wc (Load-progress-rec-whether-compile lprec)))
      (cond ((eq wc ':unknown)
	     (cond ((not (memq (lprec-find-source-pathname lprec)
			       '(:none :undef)))
		    ':compile)
		   (t
		    (let ((obj-pn
			     (lprec-find-object-pathname lprec)))
		       (cond ((and (not (member obj-pn '(:undef :none)))
				   (probe-file obj-pn))
			      ':object)
			     (t
			      (error "Load-progress-rec ~s~% corresponds to neither source nor object file"
				     lprec)))))))
	    (t wc))))

;;; Returns < ur-mod-time, loadable-mod-time >
(defun lprec-find-version-modtimes (lprec)
   (cond ((and (not (is-Pseudo-pathname (Load-progress-rec-pathname lprec)))
	       (not (= (Load-progress-rec-file-mod-timestamp lprec)
		       file-op-count*)))
	  (lprec-recompute-version-modtimes lprec)))
   (values (Load-progress-rec-ur-mod-time lprec)
	   (Load-progress-rec-loadable-mod-time lprec)))


(defun lprec-recompute-version-modtimes (lprec)
	  (let ((src-version (lprec-find-source-pathname lprec))
		(obj-version (lprec-find-object-pathname lprec))
		(whether-compile (lprec-guess-whether-compile lprec)))
	     (let ((ur-version
		      (cond ((and (not (eq src-version ':none))
				  (not (eq whether-compile ':object)))
			     src-version)
			    ((and (not (eq obj-version ':none))
				  (probe-file obj-version))
			     obj-version)
			    (t
			     (cerror "I will assume it is to be compiled"
				"Inconsistent instructions regarding whether to compile ~s"
				lprec)
			     (setf (Load-progress-rec-whether-compile lprec)
			           ':compile)
			     src-version)))
		   (loadable-version
		      (cond ((and src-version
				  (eq whether-compile ':source))
			     src-version)
			    (t
			     ;; We know from lprec-guess-whether-compile
			     ;; that either the source or the object exists
			     obj-version))))
		(setf (Load-progress-rec-ur-mod-time lprec)
		      (file-write-date ur-version))
		(setf (Load-progress-rec-loadable-mod-time lprec)
		      (file-write-date loadable-version))
		(setf (Load-progress-rec-file-mod-timestamp lprec)
		      file-op-count*))))

(defun lprec-find-source-pathname (lprec)
   (let ((spn (Load-progress-rec-source-pathname lprec)))
      (cond ((or (not spn) (eq spn ':unknown))
	     (let ((src (pathname-source-version (Load-progress-rec-pathname lprec))))
		(setf (Load-progress-rec-source-pathname lprec)
		      (or src ':none))))
	    (t spn))))

(defun lprec-find-object-pathname (lprec)
   (let ((opn (Load-progress-rec-object-pathname lprec)))
      (cond ((or (not opn) (eq opn ':unknown))
	     (let ((obj (pathname-object-version (Load-progress-rec-pathname lprec)
						 false)))
		(setf (Load-progress-rec-object-pathname lprec)
		      (or obj ':none))))
	    (t opn))))

(defun pathname-source-version (pn)
  (cond ((is-Pseudo-pathname pn) false)
	(t
	 (let ((rpn (cond ((is-Pathname pn) pn)
			  (t (pathname-resolve pn false)))))
	    (cond ((member (Pathname-type rpn)
			   source-suffixes*
			   :test #'equal)
		   rpn)
		  (t
		   (or (get-pathname-with-suffixes
		           rpn source-suffixes*)
		       (and (probe-file rpn) rpn))))))))

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

(defvar now-loading-lprec* false)
   ;; -- Load-progress-rec for file being loaded (usually the
   ;; value of now-loading*, or for its ytools version).

;;; List of names of modules we're interested in --
(defvar module-trace* !())

;;; Possible values: 
;;;  Just like values of Load-progress-rec-whether-compile, except that
;;;  the latter can be :unknown.
;;;  The local whether-compile for an lprec dominates the global flag,
;;;  unless it's :unknown.

;;; :compile -- always compile when object file is missing or out of date)
;;; :object, :source -- always load object or source without compiling
;;;     Even if :object, if object doesn't exist source will be loaded
;;;     with no questions asked.
;;;     (Unlikely you would ever want one of these globally; they can be
;;;     useful in a Load-progress-rec.)
;;; :ask -- ask user what to do
(defvar fload-compile* ':ask) 

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
			  (eval (YT-module-contents ytm)))
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

;;; Returns the insertable actions associated with the named module.
(defun module-insert (mod-pspn)
   (let ((modname (Module-pseudo-pn-module mod-pspn)))
      (let ((module (find-YT-module modname)))
	 (cond (module
		(YT-module-expansion module))
	       (t
		(cerror "I will ignore it"
		   "Undefined module ~s" modname))))))

(def-ytools-logical-pathname ytools ytools-home-dir* ytools-bin-path*)

(defun import-export (from-pkg-desig strings &optional (exporting-pkg-desig *package*))
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