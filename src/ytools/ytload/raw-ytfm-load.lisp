;-*- Mode: Common-lisp; Package: ytools; -*-
(in-package :ytools)
;;;$Id: raw-ytfm-load.lisp,v 1.3.2.2 2005/02/03 05:18:45 airfoyle Exp $

;;; This file is for recompiling a subset of ytools-core-files* 
;;; (in the proper order) when debugging YTFM.
;;; It assumes that ytload/ytload.lisp has already been loaded.
;;; The working directory should be set to the ytools directory.

(load-yt-config-file)

(load "ytload/ytfm.lmd")

(load "ytools.lsy")

(setq *readtable* ytools-readtable*)

(defun yt-comp (fname)
   (setq *readtable* ytools-readtable*)
   (compile-file fname
		 :output-file (merge-pathnames
			         (make-pathname :name fname :type lisp-object-extn*)
				 ytools-bin-dir-pathname*)))

(defun yt-bload (fname)
   (load (merge-pathnames
	    (make-pathname :name fname :type lisp-object-extn*)
	    ytools-bin-dir-pathname*)))

;;; There is no particular need for this.  Just do (yt-install :ytfm)
;;; and answer "y" when it asks if you mean to reinstall.
(defun yt-recompile ()
   (dolist (fname ytools-core-files*)
      (yt-comp fname)
      (yt-bload fname)))

;;; More variations on the same theme.
;;; This is all ad-hoc, and contains various directory names hard-wired. --

(defun co (s &optional no-lo)
   (compile-file
      (concatenate 'string "~/CVSified/dev/clocc/src/ytools/" s ".lisp")
      :output-file
      (concatenate 'string "~/CVSified/dev/clocc/bin/acl62/ytools/" s ".fasl"))
  (cond ((not no-lo) (lo s))))

(defun lo (s)
   (load (concatenate 'string "~/CVSified/dev/clocc/bin/acl62/ytools/"
		      s ".fasl")))
