;;; n-dim statistics, histograms &c
;;; for simple regression, see math.lisp
;;;
;;; Copyright (C) 2000-2004 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: stat.lisp,v 1.9 2004/07/30 18:28:22 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/stat.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `mean', `divf', `dot', `d/'
  (require :cllib-math (translate-logical-pathname "cllib:math"))
  ;; `map-vec'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `mesg'
  (require :cllib-log (translate-logical-pathname "cllib:log"))
  ;; `matrix-solve'
  (require :cllib-matrix (translate-logical-pathname "cllib:matrix")))

(in-package :cllib)

(export '(regress-n regress-poly histogram))

;;;
;;; n-dim statistics
;;;

(declaim (ftype (function ((simple-array double-float (*)) simple-array fixnum
                           &key (:func (function (array fixnum fixnum)
                                                 double-float)))
                          (values (simple-array double-float (*))
                                  double-float (double-float 0d0 1d0)
                                  (double-float 0d0)))
                regress-n))
(defun regress-n (yy xx nx &key (func #'aref))
  "Returns: vector [b1 ... bn], free term, Rmult, Ftest."
  (declare (type (simple-array double-float (*)) yy)
           (type simple-array xx) (fixnum nx)
           (type (function (array fixnum fixnum) double-float) func))
  (let ((mx (make-array (list nx nx) :element-type 'double-float
                        :initial-element 0d0))
        (cfs (make-array nx :element-type 'double-float ; coeffs
                         :initial-element 0d0))
        (rhs (make-array nx :element-type 'double-float ; right hand sides
                         :initial-element 0d0))
        (mms (make-array nx :element-type 'double-float ; means
                         :initial-element 0d0))
        (len (length yy)) (yyb (mean yy)) (yys 0d0) (free 0d0) (rr 0d0)
        (ff 0d0))
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
    (matrix-solve mx (replace cfs rhs))
    (setq free (- yyb (dot cfs mms))
          rr (/ (dot cfs rhs) yys)
          ff (d/ (* rr (- len nx 1)) (* (- 1 rr) nx)))
    (assert (<= 0d0 rr 1d0) (rr) "Rmult (~f) outside [0.0; 1.0]" rr)
    (assert (<= 0d0 ff) (ff) "Ftest (~f) is negative" ff)
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

;;;
;;; histograms
;;;

(defun histogram (list nbins &key (key #'value) (out *standard-output*))
  "Return 2 values: vector of length NBINS, bin WIDTH and MDL.
The vector contains the counts in the Ith bin."
  (let* ((mdl (standard-deviation-mdl list :key key))
         (min (mdl-mi mdl)) (max (mdl-ma mdl)))
    (mesg :log out "~&~S: ~S~%" 'histogram mdl)
    (assert (/= min max) (min max) "~S: min=max=~A" 'histogram min)
    (let ((width (/ (- max min) nbins)) (last (1- nbins))
          (vec (make-array nbins :initial-element 0)))
      (loop :for x :in list :for v = (floor (- (funcall key x) min) width)
        :do (incf (aref vec (min v last))))
      (loop :for s :across vec :minimize s :into i :maximize s :into a
        :finally (mesg :log out "~S: bin size from ~:D to ~:D~%"
                       'histogram i a))
      (values vec width mdl))))


(provide :cllib-stat)
;;; file stat.lisp ends here
