;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

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
		      (case (Load-progress-rec-status r)
			 ((:loaded)
			  (lprec-load r false false false))
			 ((:maybe-compiled)
			  (lprec-compile
			     r false
			     (files-changed-since
			        (lprec-find-supporters r)
				(nth-value
				   1 (lprec-find-version-modtimes r)))))))))))
      pathname-prop-table*))

(defvar fload-version-suffix* ':-new)

(defmacro fload-versions (&rest specs)
   (let ((olds (mapcar (lambda (x) 
			  (cond ((consp x) (car x))
				(t x)))
		       specs))
	 (news (mapcar (lambda (x)
			  (cond ((consp x)
                                 (cond ((null (cdr x))
                                        (build-symbol
					   (:< (car x))
					   (:< fload-version-suffix*)))
                                       ((or (is-String (cadr x))
					    (is-Keyword (cadr x)))
                                        (build-symbol
					   (:< (car x)) (:< (cadr x))))
                                       (t (cadr x))))
				(t x)))
		       specs)))
      `(fload-versions-massage ',olds ',news)))

;;;;(defvar save* nil)

(defun fload-versions-massage (olds news)
   (multiple-value-bind (set-olds set-news reset-olds)
                       (labels ((segregate (olds news)
                                    (cond ((null news)
                                           (values !() !() !()))
                                          (t
                                           (multiple-value-bind
                                                   (so sn rso)
                                                   (segregate (cdr olds)
                                                              (cdr news))
                                              (cond ((eq (car news) '-)
                                                     (values so sn
                                                             (cons (car olds)
                                                                   rso)))
                                                    ((eq (car news)
							 (car olds))
                                                     (values
                                                        (cons (car olds) so)
                                                        (cons (car news) sn)
                                                        (cons (car olds) rso)))
                                                    (t
                                                     (values
                                                        (cons (car olds) so)
                                                        (cons (car news) sn)
                                                        rso))))))))
                            (segregate olds news))
       (do ((oldl (filespecs->ytools-pathnames set-olds) (cdr oldl))
	    (newl (filespecs->ytools-pathnames set-news) (cdr newl)))
	   ((null oldl))
;;;;	  (setq save* (cons (tuple (car oldl) (car newl)) save*))
          (setf (pathname-prop 'version (car oldl))
                (car newl)))
       (do ((oldl (filespecs->ytools-pathnames reset-olds) (cdr oldl)))
	   ((null oldl))
          (setf (pathname-prop 'version (car oldl))
                false))
       (nconc reset-olds (mapcar #'list set-olds set-news))))

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
