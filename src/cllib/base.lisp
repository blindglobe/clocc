;;; Basis functionality, required everywhere
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: base.lisp,v 2.5 2000/06/02 18:25:11 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/base.lisp,v $

(eval-when (compile load eval)
  (require :ext (translate-logical-pathname "clocc:src;port;ext"))
  (require :sys (translate-logical-pathname "port:sys")))

(defpackage cllib
  (:use "COMMON-LISP" "PORT")
  (:nicknames "ORG.CONS.CLOCC/SDS/CLLIB")
  (:export value code))

(in-package :cllib)

(setf (logical-pathname-translations "cllib")
      `(("**;*" ,(logical-pathname "clocc:src;cllib;**;*"))
        ("**;*.*" ,(logical-pathname "clocc:src;cllib;**;*.*"))))

(defcustom *datadir* pathname
  (merge-pathnames "data/" (user-homedir-pathname))
  "The directory where the data file are created by default.")
(defcustom *mail-host-address* simple-string
  (let ((st (machine-instance))) (subseq st 0 (position #\Space st)))
  "*Name of this machine, for purposes of naming users.")
(defcustom *user-mail-address* simple-string
  (concatenate 'string (getenv "USER") "@" *mail-host-address*)
  "*Full mailing address of this user.
This is initialized based on `mail-host-address'.")

;;;
;;; }}}{{{ generic
;;;

(declaim (ftype (function (t) number) value))
(defgeneric value (xx)
  (:documentation "Get the value of the object.")
  (:method ((xx number)) xx)
  (:method ((xx cons)) (value (cdr xx))))

(declaim (ftype (function (t) symbol) code))
(defgeneric code (xx)
  (:documentation "Get the code of the object.")
  (:method ((xx symbol)) xx))

(provide :base)
;;; }}} base.lisp ends here
