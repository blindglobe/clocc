;;; parse command line options
;;; this is useful for lisp scripting
;;;
;;; Copyright (C) 2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: getopt.lisp,v 2.1 2000/05/04 18:13:48 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/getopt.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `kwd'
  (require :symb (translate-logical-pathname "cllib:symb")))

(in-package :cllib)

(export '(getopt))

;;;###autoload
(defun getopt (opt-alist arg-list)
  "Parse the command line arguments.
OPT-ALIST is an alist of (option-name . argument-type),
`argument-type' should be `:char', `:number', or `:boolean',
ARG-LIST is a list of options, like (\"-opt1\" \"val1\" ...)
Returns two values -
   a list of non-option arguments,
   and a plist of keyword args;
 or T if there was an error."
  (do ((ll arg-list (cdr ll))
       non-opt opts)
      ((null ll) (values (nreverse non-opt) (nreverse opts)))
    (if (char/= #\- (char (car ll) 0)) (push (car ll) non-opt)
        (let* ((cur (pop ll)) (kw (kwd (nstring-upcase (subseq cur 1))))
               (opt (assoc kw opt-alist :test #'eq)))
          (unless opt
            (format t "unknown option: ~s~%" cur)
            (return t))
          (push kw opts)
          (push (case (cdr opt)
                  (:char (car ll))
                  (:number
                   (let ((res (ignore-errors (read-from-string (car ll)))))
                     (unless (numberp res)
                       (format t "argument for ~s must be a number, not ~s~%"
                               cur (car ll))
                       (return t))
                     res))
                  (:boolean (ignore-errors (read-from-string (car ll))))
                  (t (format t "invalid options spec: ~s~%" opt)
                     (return t)))
                opts)))))

(provide :getopt)
;;; file getopt.lisp ends here
