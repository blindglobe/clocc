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
   rec    ; Load-progress-reco
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
	  (note-module-def-time mod))))

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
					   :module name)
			      :status false
			      :when-reached (get-universal-time))))
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
		       (format srm "#<Load-progress-rec ~a~a ~s~a>"
			       (Load-progress-rec-status lpr)
			       (cond ((not (achieved-load-status
					      lpr
					      (Load-progress-rec-status lpr)))
				      "?")
				     (t ":"))
			       pn
			       (cond ((Load-progress-rec-when-loaded lpr)
				      "*")
				     (t ""))))))
	    (:predicate is-Load-progress-rec))
   pathname ; Pathname, some with artificial directory names like "%module"
   status ; See parameter load-progress-progression* below
   when-reached ; date when status was reached
   when-loaded ; date when last loaded (or false if it never was).
   whether-compile ; false, :compile, :object, :source, or :ask
   (run-time-depends-on !())
   (compile-time-depends-on !())
   ;; -- lists of Pathname's, one per entity this one
   ;; depends on
   (status-timestamp 0 :type integer)
   ;; -- Value of file-op-count* the last time the status of this
   ;; Load-progress-rec was set
   (changed-supporters '())
   ;; -- List of supporters that have changed before the current fload
   (supp-timestamp 0 :type integer)
   ;; -- Value of file-op-count* the last time the changed-supporters
   ;; were computed.
)
;;; Timestamp schemes assume that no one writes files during an "fload episode,"
;;; that is, during a top-level call to fload.  E.g., you don't save a file buffer
;;; for a file that might get loaded.  Of course, if you do save such a buffer,
;;; you don't know which version will actually get loaded, so the assumption
;;; is reasonable.  Another reason it's reasonable is that fload episodes are
;;; short (a few seconds).

(defvar source-suffixes* (adjoin lisp-source-extn* '("lisp") :test #'equal))
(defvar obj-suffix* lisp-object-extn*)

(defparameter load-progress-progression*
    '(nil :header-checked :slurped :slurped-all
          :maybe-compiled :loaded :frozen))

(defconstant can-get-write-times*
    #.(not (not (file-write-date
		    (concatenate 'string ytools-home-dir* "files.lisp")))))

;;;;(defvar write-time-calls* 0)
;;;;(defvar redundant-write-time-calls* 0)

;;; Return < boolean, time >
;;; If boolean is true, and time is a number, then it's the time the status
;;; was reached.
(defun achieved-load-status (lprec status)
   (let ((tl (memq (Load-progress-rec-status lprec)
		   load-progress-progression*)))
      (cond (tl
	     (cond ((memq status (cdr tl))
		    (values false nil))
		   ((-- can-get-write-times*)
		    (cond ((= (Load-progress-rec-status-timestamp lprec)
			      file-op-count*)
			   (values true (Load-progress-rec-when-reached lprec)))
			  (t
			   (let ((rpn (Load-progress-rec-pathname lprec)))
			      (cond ((is-Pseudo-pathname rpn)
				     (values true false))
				    (t
				     (let ((ur-version
					      (or (pathname-source-version rpn)
						  (pathname-object-version rpn true))))
				        (let ((ur-write-time
					         (pathname-write-time ur-version))
					      (reach-time
						 (Load-progress-rec-when-reached
						    lprec)))
					   (cond ((> ur-write-time reach-time)
						  (setf (Load-progress-rec-status lprec)
						        false)
						  (setf (Load-progress-rec-when-reached
							    lprec)
						        ur-write-time)
						  (setf (Load-progress-rec-status-timestamp
							   lprec)
						        file-op-count*)
						  (values false ur-write-time))
						 (t
						  (values true reach-time)))))))))))
		   (t
		    (values true nil))))
	    (t
	     (error "Illegal Load-progress-rec status ~s"
		    status)))))

(defun note-load-status (lprec status)
   (let (;;;;(time (get-universal-time))
	 (reach-time (Load-progress-rec-when-reached lprec))
	 (tl (memq (Load-progress-rec-status lprec)
		   load-progress-progression*)))
      (cond ((memq status (cdr tl))
	     (setf (Load-progress-rec-status lprec)
		   status)))
;;;;      (format t "file-op-count* = ~s~%rec-timestamp = ~s~%"
;;;;	        file-op-count*
;;;;		(Load-progress-rec-status-timestamp lprec))
;;;;      (format t "** reach-time = ~s~%** rec-reach-time = ~s~%"
;;;;	      reach-time
;;;;	      (Load-progress-rec-when-reached lprec))
      (cond ((and can-get-write-times*
		  (> file-op-count*
		     (Load-progress-rec-status-timestamp lprec)))
	     (let ((rpn (Load-progress-rec-pathname lprec)))
		(let ((ur-version 
			 (or (pathname-source-version rpn)
			     (pathname-object-version rpn true))))
;;;;		   (break "lprec = ~s" lprec)
		   (let ((write-time (pathname-write-time ur-version)))
;;;;		      (format t "write-time = ~s~%reach-time = ~s~%rec-reach-time = ~s~%"
;;;;			      write-time reach-time
;;;;			      (Load-progress-rec-when-reached lprec))
		      (cond ((> write-time reach-time)
			     (format t "Changing reach-time to ~s~%" write-time)
			     (setf reach-time write-time)
			     (setf (Load-progress-rec-when-reached lprec)
				   reach-time)
			     (cond ((eq status ':loaded)
				    (setf (Load-progress-rec-when-loaded lprec)
					  reach-time)))))
		      (setf (Load-progress-rec-status-timestamp lprec)
			    file-op-count*))))))))

(defun note-module-def-time (ytmod)
   (note-load-status (YT-module-rec ytmod) false))

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
   (pathname-find-associate pn 'obj-version obj-suffix* only-if-exists))

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

(defvar fload-compile* ':ask) 
   ;; -- compile if not yet or source version newer

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