;;; File: <base.lisp - 2000-02-18 Fri 14:09:00 EST sds@ksp.com>
;;;
;;; Basis functionality, required everywhere
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and the precise copyright document.
;;;
;;; $Id: base.lisp,v 2.0 2000/02/18 20:21:57 sds Exp $
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

;;;
;;; {{{ Prompt
;;;

(defun package-short-name (pkg)
  "Return the shortest (nick)name of the package."
  (declare (type package pkg))
  #+clisp (sys::package-short-name pkg)
  #-clisp
  (let ((name (reduce (lambda (st0 st1)
                        (declare (simple-string st0 st1))
                        (if (> (length st0) (length st1)) st1 st0))
                      (package-nicknames pkg) :initial-value
                      (package-name pkg))))
    (case *print-case*
      (:upcase (string-upcase name))
      (:downcase (string-downcase name))
      (:capitalize (string-capitalize name))
      (t name))))

#+cmu
(defcustom sys::*command-index* index-t 0
  "The number of commands issued so far.")

(flet ((beg-end ()              ; beg-bold beg-it end-all
         (let ((term (getenv "TERM")))
           (if (or (null term)
                   (string-equal term "dumb")
                   (string-equal term "emacs"))
               (values "" "" "")
               (values "[1m" "[7m" "[m")))))
  #+(or cmu clisp)
  (setq lisp::*prompt*
        (lambda ()
          (multiple-value-bind (bb ib ee) (beg-end)
            (format nil "~a~a~a~a[~:d]:~a " ib
                    (package-short-name *package*)
                    ee bb (incf sys::*command-index*) ee))))
  #+allegro
  (multiple-value-bind (bb ib ee) (beg-end)
    (setq tpl:*prompt* (concatenate 'string bb tpl:*prompt* ee))))

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
