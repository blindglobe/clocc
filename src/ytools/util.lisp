;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)
;;;$Id: util.lisp,v 2.1 2006/03/27 00:19:32 airfoyle Exp $

;;; Utilities beyond base.lisp, which this depends on.

;;; Return the printed representation of 'x', snipped
(defun printed-snip (x n)
   (string-snip (format nil "~a" x) n))

;;; Discard all but the first n (or last -n if n<0) characters of s
(defun string-snip (s n)
   (let ((l (length s)))
      (cond ((< n 0)
	     (cond ((> l (- n))
		    (concatenate 'string "..." (subseq s (+ l n))))
		   (t s)))
	    (t
	     (cond ((> l n)
		    (concatenate 'string (subseq s 0 n) "..."))
		   (t s))))))

(defun slot-is-empty (obj slot)
   (not (slot-truly-filled obj slot)))

(defun slot-truly-filled (ob sl)
   (and (slot-boundp ob sl)
	(slot-value ob sl)))

;;; For debugging --
(defun dutl (ut) (values->list (decode-universal-time ut)))

;;; For debugging (it's traceable)
(defun retain-if-x (pred l)
   (retain-if pred l))

