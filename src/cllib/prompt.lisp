;;; Prompt
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and the precise copyright document.
;;;
;;; $Id: prompt.lisp,v 2.1 2000/03/23 00:09:15 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/prompt.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `getenv'
  (require :base (translate-logical-pathname "port:sys")))

;; CLISP defines but does not export this function
;; #+clisp (import 'sys::package-short-name)
;; #-clisp
(defun package-short-name (pkg)
  "Return the shortest (nick)name of the package."
  (declare (type package pkg))
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
(defvar sys::*command-index* 0
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
