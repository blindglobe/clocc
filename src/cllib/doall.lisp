;;; run a function, answering questions in a pre-defined way,
;;; possibly under monitor.
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: doall.lisp,v 1.3 2000/05/02 14:59:53 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/doall.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base")))

(export '(do-all))

(eval-when (compile load eval)
  (require :monitor (translate-logical-pathname
                     "clocc:src;tools;metering;metering")))

(in-package :cllib)

(defun do-all (monitorp func answers &rest args)
  "Run FUNC answering ANSWERS (boolean list) to the y-or-n-p's.
ARGS is passed to FUNC.
  (do-all COMPILEP MONITORP FUNC ANSWERS &REST ARGS)"
  (declare (function func) (list answers))
  (let ((ost *query-io*))
    (unwind-protect
         (progn
           (setq *query-io*
                 (make-two-way-stream
                  (make-string-input-stream
                   (format nil "~{~:[n~;y~]~%~}" answers))
                  *standard-output*))
           (if monitorp (mon:monitor-form (apply func args))
               (apply func args)))
      (setq *query-io* ost)))
  (values))

(provide :doall)
;;; file doall.lisp ends here
