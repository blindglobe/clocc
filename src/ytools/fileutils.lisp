;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: fileutils.lisp,v 1.4.2.1 2004/11/27 20:03:03 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(depends-on :at-compile-time %ytools/ repeat binders)

(depends-on :at-run-time %ytools/ mapper setter)

(eval-when (:load-toplevel)
   (export '(fload-versions fload-recheck files-run-time-deps)))

;;; Check whether any compiled file needs to be recompiled.
;;; ASSUMES that ALL files' progress is recorded in the pathname-prop table!!!
(defun fload-recheck ()
   (maphash
      (\\ (pn props)
	 (repeat :for ((e :in props))
	    (cond ((eq (car e) 'load-progress-rec)
		   (let ((r (cadr e)))
		      (cond ((not (is-Load-progress-rec r))
			     (error "~s has 'load-progress-rec' property ~s"
				    pn r)))
		      (cond ((not (is-Pseudo-pathname
				     (Load-progress-rec-pathname r)))
			     (case (Load-progress-rec-status r)
				((:loaded)
				 (lprec-load r false false false))
				((:maybe-compiled)
				 (lprec-compile
				    r false
				    (files-changed-since
				       (lprec-find-supporters r)
				       (nth-value
					  1 (lprec-find-version-modtimes r)))))))))))))
      pathname-prop-table*))

(defun files-run-time-deps (filespecs)
   (let ((checked !((Load-progress-rec))))
      (let-fun ((findem (pnl)
;;;;		   (trace-around findem
;;;;		      (:> "(findem: " pnl")")
		   (repeat :for ((pn :in pnl))
		    :append
		      (let ((lprec (place-load-progress-rec pn)))
			 (let ((deps (<? neg is-Pseudo-pathname
					 (Load-progress-rec-run-time-depends-on
					    lprec)))
			       (e (memq lprec checked)))
			    (cond (e deps)
				  (t
				   (!= checked (cons lprec *-*))
				   (lprec-slurp lprec false ':header-only)
				   (append deps (findem deps)))))))
;;;;		      (:< (val &rest _) "findem: " val))
		   ))
	 (remove-duplicates
	    (findem (filespecs->pathnames filespecs))
	    :test #'pn-equal))))
