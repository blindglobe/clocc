;;; n-dim statistics, histograms &c
;;; for simple regression, see math.lisp
;;;
;;; Copyright (C) 2000-2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: stat.lisp,v 1.14 2005/07/07 21:17:04 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/stat.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `mean', `divf', `dot', `d/', `incomplete-gamma', `approx=-abs'
  (require :cllib-math (translate-logical-pathname "cllib:math"))
  ;; `map-vec'
  (require :cllib-withtype (translate-logical-pathname "cllib:withtype"))
  ;; `mesg'
  (require :cllib-log (translate-logical-pathname "cllib:log"))
  ;; `matrix-solve'
  (require :cllib-matrix (translate-logical-pathname "cllib:matrix")))

(in-package :cllib)

(export '(regress-n regress-poly histogram check-distrib
          chi2-1 chi2-2 chi2-1-ui chi2-2-ui chi2-prob))

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

;;;
;;; Chi square
;;;

(defun check-distrib (distrib)
  "Check the probability distribution vector."
  (let ((sum (reduce #'+ distrib)))
    (unless (cllib:approx=-abs 1 sum)
      (error "~S(~S): total probability is not 1: ~A"
             'check-distrib distrib sum))
    (when (some #'minusp distrib)
      (error "~S(~S): negative probabilities: ~S"
             'check-distrib distrib (remove-if-not #'minusp distrib)))
    distrib))

(defun chi2-1 (seq distrib)
  "Return the chi^2 score & the degree of freedom.
Arguments are a sequence of counts instantiating a distribution
and a distribution which the instantiation is checked against."
  (let ((chi2 0) (df -1) (sum (reduce #'+ seq)) (s2 (reduce #'+ distrib)))
    (unless (approx=-abs 1 s2)
      (error "~S: not a probability distribtion: ~S /= 1: ~S"
             'chi2-1 s2 distrib))
    (map nil (lambda (count prob)
               (incf df)
               (let ((expected (* sum prob)))
                 (incf chi2 (/ (sqr (- count expected)) expected))))
         seq distrib)
    (values chi2 df)))

(defun chi2-2 (seq1 seq2)
  "Return the chi^2 score & the degree of freedom.
Arguments are 2 sequences instantiating the same (or different?) distributions."
  (let* ((chi2 0) (df -1) (s1 (reduce #'+ seq1)) (s2 (reduce #'+ seq2))
         (sum (+ s1 s2)))
    (map nil (lambda (v1 v2)
               (incf df)
               (let* ((p (/ (+ v1 v2) sum)) (e1 (* s1 p)) (e2 (* s2 p)))
                 (incf chi2 (/ (sqr (- v1 e1)) e1))
                 (incf chi2 (/ (sqr (- v2 e2)) e2))))
         seq1 seq2)
    (values chi2 df)))

(defun chi2-prob (chi2 df)
  "Return the probability that this CHI2/DF score is just a random fluke."
  (- 1 (incomplete-gamma (/ df 2) (/ chi2 2))))

(defun chi2-1-ui (sample distrib &key (out *standard-output*)
                  (sample-name "Sample") (distrib-name "Distribution"))
  "Pretty output for CHI2-1."
  (multiple-value-bind (chi2 df) (chi2-1 sample distrib)
    (let ((prob (chi2-prob chi2 df)))
      (format out "~&~A:~15T~S~%~A:~15T~S~%Chi^2=~5F  df=~:D  P=~5F%~%"
              distrib-name distrib sample-name sample chi2 df (* 100 prob))
      (values chi2 df prob))))

(defun chi2-2-ui (sample1 sample2 &key (out *standard-output*)
                  (sample1-name "Sample#1") (sample2-name "Sample#2"))
  "Pretty output for CHI2-2."
  (multiple-value-bind (chi2 df) (chi2-2 sample1 sample2)
    (let ((prob (chi2-prob chi2 df)))
      (format out "~&~A:~15T~S~%~A:~15T~S~%Chi^2=~5F  df=~:D  P=~5F%~%"
              sample1-name sample1 sample2-name sample2 chi2 df (* 100 prob))
      (values chi2 df prob))))

(provide :cllib-stat)
;;; file stat.lisp ends here
