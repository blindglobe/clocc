;;; Basis functionality, required everywhere
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and the precise copyright document.
;;;
;;; $Id: base.lisp,v 2.2 2000/03/22 23:57:59 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/base.lisp,v $

(eval-when (compile load eval)
  (require :ext (translate-logical-pathname "clocc:src;port;ext"))
  (require :sys (translate-logical-pathname "port:sys")))

(defpackage cllib
  (:use "COMMON-LISP" "PORT")
  (:nicknames "ORG.CONS.CLOCC/SDS/CLLIB"))

(in-package :cllib)

(setf (logical-pathname-translations "cllib")
      `(("**;*" ,(logical-pathname "clocc:src;cllib;**;*"))))

(export '(value code))

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
