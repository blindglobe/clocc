;;; -*- Mode: CLtL -*-

;;; init-environment.lisp --
;;; This file is really hairy.  I decide to leave it so.  The
;;; alternative would be to split it up in several implementation
;;; dependent files, but then things may really get hairier :)
;;; This is the only file with #+/#- (well... almost).
;;; Anyway, I am open for suggestions.
;;;
;;; 20000107 Marco Antoniotti
;;;
;;; Copyright (c) 2000 Marco Antoniotti, all rights reserved.
;;; This software is released under the terms of the GNU Lesser General
;;; Public License (LGPL, see file COPYRIGHT for details).

(in-package "CL.ENVIRONMENT")

(defun featurep (feature)
  (and (symbolp feature)
       (not (null (member (symbol-name feature) *features*
			  :key #'symbol-name
			  :test #'string=)))))

;;; These are parameters - as opposed to constants - to allow (in
;;; principle) some fancy cross development tricks.

(defparameter *common-lisp-implementation* ())

(defparameter *operating-system* ())

(defparameter *machine* ())

(eval-when (:load-toplevel :execute)

  ;;--------------------
  ;; Setup Machine Info.
  
  (setf *machine* (make-instance 'machine))

  ;;-----------------------------
  ;; Setup Operating System Info.
  #+cmu
  (cond ((featurep :solaris)
	 (setf *operating-system*
	       (make-instance 'Solaris
			      :version (common-lisp:software-version))))

	((and (featurep :sunos) (not (featurep :solaris)))
	 (setf *operating-system*
	       (make-instance 'SunOS
			      :version (common-lisp:software-version))))

	((featurep :linux)
	 (setf *operating-system* (make-instance 'Linux)))

	(t
	 (setf *operating-system* (make-instance 'Unix))))

  #+allegro
  (cond ((featurep :unix)
	 (setf *operating-system* (make-instance 'Unix)))

	((featurep :mswindows)
	 (if (featurep :windows-32)
	     (setf *operating-system* (make-instance 'MS-Windows-32))
	     (setf *operating-system* (make-instance 'MS-Windows))))

	(t)				; do nothing?
	)

  #+lispworks
  (cond ((featurep :unix)
	 (setf *operating-system* (make-instance 'Unix)))

	((featurep :win32)
	 (setf *operating-system* (make-instance 'MS-Windows-32)))

	(t)				; do nothing?
	)

  #+clisp
  (cond ((featurep :unix)
	 (setf *operating-system* (make-instance 'Unix)))

	((featurep :os/2)
	 (setf *operating-system* (make-instance 'OS/2)))

	((featurep :ms-dos)
	 (setf *operating-system* (make-instance 'MS-DOS)))

	((featurep :win32)
	 (setf *operating-system* (make-instance 'MS-Windows)))

	((featurep :amiga)
	 (setf *operating-system* (make-instance 'Amiga)))

	(t)				; do nothing?
	)
	 

	
  ;;---------------------------------------
  ;; Setup Common Lisp Implementation Info.

  #+cmu
  (setf *common-lisp-implementation* (make-instance 'cmucl))

  #+sbcl
  (setf *common-lisp-implementation* (make-instance 'sbcl))

  #+clisp
  (setf *common-lisp-implementation* (make-instance 'clisp))

  #+allegro
  (setf *common-lisp-implementation* (make-instance 'allegro))

  #+lispworks
  (setf *common-lisp-implementation* (make-instance 'lispworks))

  #+kcl
  (setf *common-lisp-implementation* (make-instance 'kcl))

  #+ibcl
  (setf *common-lisp-implementation* (make-instance 'ibcl))

  #+akcl
  (setf *common-lisp-implementation* (make-instance 'akcl))

  #+ecolisp
  (setf *common-lisp-implementation* (make-instance 'ecolisp))

  #+gcl
  (setf *common-lisp-implementation* (make-instance 'gcl))

  #+lucid
  (setf *common-lisp-implementation* (make-instance 'lucid))

  #+mcl
  (setf *common-lisp-implementation* (make-instance 'mcl))

  #+genera
  (setf *common-lisp-implementation* (make-instance 'scl))

  )

;;; end of file -- init-environment.lisp --
