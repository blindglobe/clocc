;;; bugfixes
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold.
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: clocc.lisp,v 1.4 2000/05/01 20:20:09 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/clocc.lisp,v $

(in-package :cl-user)

;; fix some bugs
(eval-when (load compile eval)
  #+(or clisp gcl) (declaim (declaration values))
  #+allegro
  (progn
    ;; Duane Rettig <duane@franz.com>:
    ;; TIME reports 32 other bytes too many CL unless tuned with
    (setq excl::time-other-base 32)
    ;; From Erik Naggum <erik@naggum.no> [22 Feb 1999 10:27:45 +0000]
    ;; fixes the (read-from-string "[#\\x]") problem
    (loop with readtables = (excl::get-objects 11)
          for i from 1 to (aref readtables 0)
          for readtable = (aref readtables i) do
          (when (excl::readtable-dispatch-tables readtable)
            ;; reader for character names immune cltl1
            (set-dispatch-macro-character
             #\# #\\
             (excl::named-function
              excl::sharp-backslash
              (lambda (stream backslash font)
                (declare (ignore font))
                (unread-char backslash stream)
                (let* ((charstring (excl::read-extended-token stream)))
                  (unless *read-suppress*
                    (or (character charstring)
                        (name-char charstring)
                        (excl::internal-reader-error
                         stream "Meaningless character name ~A"
                         (string-upcase charstring)))))))
             readtable))))
  #+gcl (defmacro lambda (bvl &body forms) `#'(lambda ,bvl ,@forms))
  #+allegro-v4.3                ; From Erik Naggum <erik@naggum.no>
  (unless (member :key (excl:arglist #'reduce) :test #'string=)
    (setq excl:*compile-advice* t)
    (excl:defadvice reduce (support-key :before)
      (let ((key (getf (cddr excl:arglist) :key)))
        (when key
          (remf (cddr excl:arglist) :key)
          (setf (second excl:arglist)
                (map 'vector key (second excl:arglist)))))))
  #-(or allegro clisp mcl)
  (define-setf-expander values (&rest places &environment env)
    (loop :for pl :in places :with te :and va :and ne :and se :and ge :do
          (multiple-value-setq (te va ne se ge) (get-setf-expansion pl env))
          :append te :into te1 :append va :into va1 :append ne :into ne1
          :collect se :into se1 :collect ge :into ge1
          :finally (return (values te1 va1 ne1 (cons 'values se1)
                                   (cons 'values ge1)))))
  #+mcl
  (import '(ccl:provide) :common-lisp))


;;;
;;; Path
;;;

;; When using CLISP, you need version 2000-03-06 or newer. The older version
;; 1999-07-22 had serious bugs with logical pathnames.

(setf (logical-pathname-translations "clocc")
      '(("src;defsystem;*" "/usr/local/src/clocc/src/defsystem-3.x/*")
	("src;defsystem-3-x;*" "/usr/local/src/clocc/src/defsystem-3.x/*")
	("**;*" "/usr/local/src/clocc/**/*")))

;;; clocc.lisp ends here
