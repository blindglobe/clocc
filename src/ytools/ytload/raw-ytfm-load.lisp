;-*- Mode: Common-lisp; Package: ytools; -*-
(in-package :ytools)

;;; This file is for getting a raw Lisp up to the point where the
;;; ytools-core-files* can be loaded.  It is used only for debugging
;;; YTFM.  It assumes that ytload/ytload.lisp has already been loaded.
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

(defun yt-recompile ()
   (dolist (fname ytools-core-files*)
      (yt-comp fname)
      (yt-bload fname)))