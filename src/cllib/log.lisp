;;; logging and progress reporting
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: log.lisp,v 1.9 2000/05/08 20:00:19 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/log.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `with-type', `dfloat'
  (require :withtype (translate-logical-pathname "cllib:withtype"))
  ;; `pr-secs'
  (require :tilsla (translate-logical-pathname "cllib:tilsla")))

(in-package :cllib)

(export '(get-float-time elapsed with-timing *print-log* mesg list-format))

;;;
;;;
;;;

(defun list-format (item-fmt)
  "Return the format string for list printing, printing the item as ITEM-FMT.
Taken from CLtL2 p602."
  (format nil "~~#[ none~~; ~a~~; ~a and ~a~~:;~~@{~~#[~~; and~~] ~a~~^,~~}~~]"
          item-fmt item-fmt item-fmt item-fmt))

;;;
;;; {{{ progress reporting
;;;

(declaim (ftype (function (&optional t) (values (double-float 0.0d0)))
                get-float-time))
(defun get-float-time (&optional (run t))
  "Return the run (or real) time counter, as a double float."
  (dfloat (if run (get-internal-run-time) (get-internal-real-time))))

(defun elapsed (bt run &optional fmt)
  "Return the time in seconds elapsed since BT,
previously set using `get-float-time'.
If FMT is non-NIL, return the corresponding string too."
  (declare (type (double-float 0.0d0) bt))
  (let ((nn (with-type double-float
              (/ (- (get-float-time run) bt)
                 (dfloat internal-time-units-per-second)))))
    (declare (double-float nn))
    (if fmt (values nn (format nil "~/pr-secs/" nn)) nn)))

(defmacro with-timing ((&key (terpri t) (done nil) (run t) (real t)
                             (out '*standard-output*))
                       &body body)
  "Evaluate the body, then print the timing."
  (with-gensyms ("TIMING-" bt bt1)
    `(let ((,bt (get-float-time)) (,bt1 (get-float-time nil)))
      (unwind-protect (progn ,@body)
        (when ,out
          (when ,done (princ "done" ,out))
          (when ,run (format ,out " [run: ~/pr-secs/]" (elapsed ,bt t)))
          (when ,real (format ,out " [real: ~/pr-secs/]" (elapsed ,bt1 nil)))
          (when ,terpri (terpri ,out)))))))

;;;
;;; }}}{{{ logging
;;;

(defcustom *print-log* (simple-array symbol (*))
  (mk-arr 'symbol '(:log :logv :date :plot :head :res :opt :err :test))
  "The list of message types which are being printed.")

(defmacro mesg (type str &rest args)
  "Call format -- conditionally.
This has to be a macro to avoid needless evaluating the args."
  (with-gensyms ("MESG-" out typ)
    `(let ((,out ,str) (,typ ,type))
      (declare (type (or stream (member nil t)) ,out))
      (when (and ,out (or (eq ,typ t) (find ,typ *print-log* :test #'eq)))
        (format ,out ,@args)
        (force-output (if (eq t ,out) *standard-output* ,out))))))

;;; }}}

(provide :log)
;;; file log.lisp ends here
