;;; Prompt
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: prompt.lisp,v 2.4 2000/05/16 17:07:26 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/prompt.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `getenv'
  (require :sys (translate-logical-pathname "port:sys")))

(in-package :cllib)

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
    (declare (ignore ib))
    (setq tpl:*prompt* (concatenate 'string bb tpl:*prompt* ee))))

(provide :prompt)
;; prompt.lisp ends here
