;;; File: <elisp.lisp - 1999-02-20 Sat 14:15:07 EST sds@eho.eaglets.com>
;;;
;;; Load Emacs-Lisp files into Common Lisp
;;;
;;; Copyright (C) 1999 by Sam Steingold
;;;
;;; $Id: elisp.lisp,v 1.1 1999/02/20 19:15:30 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/elisp.lisp,v $
;;; $Log: elisp.lisp,v $
;;; Revision 1.1  1999/02/20 19:15:30  sds
;;; Initial revision
;;;
;;;

(in-package :cl-user)

(eval-when (load compile eval)
  (sds-require "base")
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

(defpackage emacs-lisp
  (:documentation "The package for loading Emacs-Lisp code into Common Lisp")
  (:nicknames elisp el) (:use cl) (:shadow let let* if))

(defmacro el::if (ii tt &rest ee)
  "Emacs-Lisp version of `if'."
  (if ee `(if ,ii ,tt (progn ,@ee)) `(if ,ii ,tt)))

;; (macroexpand '(el::if t (print "then")))
;; (macroexpand '(el::if t (print "th") (print "e1") (print "e2")))

(defmacro el::let ((&rest vars) &rest forms)
  "Emacs-Lisp version of `let'."
  `(let ,vars (declare (special ,@(mapcar #'from-list vars))) ,@forms))

(defmacro el::let* ((&rest vars) &rest forms)
  "Emacs-Lisp version of `let*'."
  `(let* ,vars (declare (special ,@(mapcar #'from-list vars))) ,@forms))

;; (macroexpand '(el::let (v1 (v2 t) v3) (f0) (f1)))

(defun el::read-elisp-special (stream char)
  (declare (stream stream) (character char))
  (ecase char
    (#\[ (read-delimited-list #\] stream t))
    (#\? )))

(defun el::make-elisp-readtable ()
  "Make the readtable for Emacs-Lisp parsing."
  (let ((rt (copy-readtable)))
    (set-macro-character #\? #'read-elisp-special nil rt)
    (set-macro-character #\[ #'read-elisp-special nil rt)
    (set-macro-character #\] (get-macro-character #\)) nil rt)
    rt))

(defparameter *elisp-readtable* (make-elisp-readtable)
  "The readtable for Emacs-Lisp parsing.")

(in-package :emacs-lisp)



(provide "elisp")
;;; file elisp.lisp ends here
