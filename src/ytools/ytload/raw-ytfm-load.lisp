;-*- Mode: Common-lisp; Package: ytools; -*-
(in-package :ytools)

;;; This file is for recompiling a subset of ytools-core-files* 
;;; (in the proper order) when debugging YTFM.
;;; It assumes that ytload/ytload.lisp has already been loaded.
;;; The working directory should be set to the ytools directory.

(load-yt-config-file)

(load "ytload/ytfm.lmd")

(load "ytools.lsy")

(defun yt-comp (fname)
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