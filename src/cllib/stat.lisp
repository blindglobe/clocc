;;; n-dim statistics, requires matrix operations
;;; for simple regression, see math.lisp
;;;
;;; Copyright (C) 2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: stat.lisp,v 1.2 2000/03/27 20:02:54 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/stat.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `mean', `divf', `dot', `d/'
  (require :math (translate-logical-pathname "cllib:math"))
  ;; `map-vec'
  (require :withtype (translate-logical-pathname "cllib:withtype"))
  ;; `matrix-solve', `matrix-inverse', `matrix-print',
  ;; `matrix-multiply-mat-col'
  (require :matrix (translate-logical-pathname
                    "clocc:src;math;clmath;matrix")))

(in-package :cllib)

(export '(regress-n regress-poly))

(eval-when (load compile eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

;;;
;;;
;;;

(defun regress-n (yy xx nx &key (func #'aref))
  "Returns: vector [b1 ... bn], free term, Rmult, Ftest."
  (declare (type (simple-array double-float (*)) yy)
           (type simple-array xx) (fixnum nx)
           (type (function (array fixnum fixnum) double-float) func)
           (values (simple-array double-float (*)) double-float
                   (double-float 0.0d0 1.0d0) (double-float 0.0d0)))
  (let ((mx (make-array (list nx nx) :element-type 'double-float
                        :initial-element 0.0d0))
        (cfs (make-array nx :element-type 'double-float ; coeffs
                         :initial-element 0.0d0))
        (rhs (make-array nx :element-type 'double-float ; right hand sides
                         :initial-element 0.0d0))
        (mms (make-array nx :element-type 'double-float ; means
                         :initial-element 0.0d0))
        (len (length yy)) (yyb (mean yy)) (yys 0.0d0) (free 0.0d0) (rr 0.0d0)
        (ff 0.0d0))
    (declare (type (simple-array double-float (* *)) mx) (type index-t len)
             (type (simple-array double-float (*)) cfs rhs mms)
             (double-float yyb yys free ff rr))
    (loop :for kk :of-type index-t :upfrom 0 :and yk :across yy :do
          (incf yys (expt (- yk yyb) 2))
          (dotimes (ii nx)          ; compute X
            (declare (type index-t ii))
            (setf (aref cfs ii) (funcall func xx kk ii))
            (incf (aref mms ii) (aref cfs ii)))
          (dotimes (ii nx)
            (declare (type index-t ii))
            (incf (aref rhs ii) (* yk (aref cfs ii)))
            (loop :for jj :of-type index-t :from 0 :to ii :do
                  (incf (aref mx ii jj) (* (aref cfs ii) (aref cfs jj))))))
    (dotimes (ii nx)            ; subtract the means
      (declare (type index-t ii))
      (decf (aref rhs ii) (* (aref mms ii) yyb))
      (divf (aref mms ii) len))
    (dotimes (ii nx)
      (declare (type index-t ii))
      (loop :for jj :of-type index-t :from 0 :to ii :do
            (decf (aref mx ii jj) (* len (aref mms ii) (aref mms jj)))
            (setf (aref mx jj ii) (aref mx ii jj))))
    (handler-case (clmath:matrix-solve mx (replace cfs rhs))
      (division-by-zero (co)
        (format t "regress-n: `clmath:matrix-solve' failed: ~a~%" co)
        (clmath:matrix-print mx)
        (let ((det (clmath:matrix-inverse mx)))
          (assert (/= 0 det) (mx) "the matrix is degenerate (det = 0)~%")
          (format t "det == ~f; using `clmath:matrix-inverse'.~%" det)
          (clmath:matrix-multiply-mat-col mx rhs cfs))))
    (setq free (- yyb (dot cfs mms))
          rr (/ (dot cfs rhs) yys)
          ff (d/ (* rr (- len nx 1)) (* (- 1 rr) nx)))
    (assert (<= 0.0d0 rr 1.0d0) (rr) "Rmult (~f) outside [0.0; 1.0]" rr)
    (assert (<= 0.0d0 ff) (ff) "Ftest (~f) is negative" ff)
    (values cfs free (sqrt rr) ff)))

(defun regress-poly (seq deg &key (xkey #'car) (ykey #'cdr))
  "Polynomial regression."
  (declare (sequence seq) (fixnum deg)
           (type (function (t) double-float) xkey ykey))
  (let* ((len (length seq)) (ii 0) (yy (map-vec 'double-float len ykey seq))
         (xx (make-array (list len 1) :element-type 'double-float)))
    (declare (type index-t len ii))
    (map nil (lambda (el) (setf (aref xx ii 0) (funcall xkey el)) (incf ii))
         seq)
    (multiple-value-bind (vec free)
        (regress-n yy xx deg :func
                   (lambda (xx ii jj)
                     (declare (type index-t ii jj)
                              (type (simple-array double-float (* *)) xx))
                     (expt (aref xx ii 0) (1+ jj))))
      (concatenate 'simple-vector (nreverse vec) (list free)))))

(provide :stat)
;;; file stat.lisp ends here
