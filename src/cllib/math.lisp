;;; File: <math.lisp - 1999-04-17 Sat 18:33:36 EDT sds@eho.eaglets.com>
;;;
;;; Math utilities (Arithmetical / Statistical functions)
;;;
;;; Copyright (C) 1997-1999 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: math.lisp,v 1.18 1999/04/17 22:34:17 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/math.lisp,v $
;;; $Log: math.lisp,v $
;;; Revision 1.18  1999/04/17 22:34:17  sds
;;; (safe-fun): use `values-list'.
;;; (safe-fun1): new macro.
;;;
;;; Revision 1.17  1999/03/17 21:37:32  sds
;;; Added `eval-cont-fract' and `fract-approx'.
;;;
;;; Revision 1.16  1999/02/25 17:33:51  sds
;;; Moved `lincom' here from date.lisp.
;;;
;;; Revision 1.15  1999/02/25 04:40:36  sds
;;; `volatility': swapped return values.
;;;
;;; Revision 1.14  1999/02/22 22:56:10  sds
;;; Use `:min-len' key in the `call-on-split' call in `volatility'.
;;;
;;; Revision 1.13  1999/01/13 23:37:56  sds
;;; Replaced CMUCL-specific print functions with a call to
;;; `print-struct-object'.
;;;
;;; Revision 1.12  1999/01/07 04:07:34  sds
;;; Use `index-t' instead of (unsigned-byte 20).
;;;
;;; Revision 1.11  1998/11/23 21:22:13  sds
;;; Added MDL structure.
;;;
;;; Revision 1.10  1998/11/13 20:16:58  sds
;;; Added `mean-some'.
;;;
;;; Revision 1.9  1998/10/29 21:57:17  sds
;;; Fixed `standard-deviation-relative' to compute the actual volatility.
;;;
;;; Revision 1.8  1998/09/03 13:54:08  sds
;;; Ditched `sqr'.  Added `fibonacci', `primes-to', `divisors', `primep',
;;; `make-primes-list', `*primes*', `*primes-file*'.
;;; Special case 2 point regression in `regress'; don't signal error on
;;; negative error, just print a message and assume 0.
;;;
;;; Revision 1.7  1998/07/31 16:41:24  sds
;;; Declare `stream' as a stream in `print-*'.
;;; Take `sqrt' of the correlation in `regress-n'.
;;;
;;; Revision 1.6  1998/07/10 21:11:35  sds
;;; Added `regress-n' and `regress-poly'.
;;; Ditched `regress2' and `det3'.
;;;
;;; Revision 1.5  1998/06/19 21:41:40  sds
;;; Use `defmethod' to print structures.
;;;
;;; Revision 1.4  1998/06/19 20:10:38  sds
;;; Made `normalize' work with arbitrary sequences.
;;; Many minor declarations for CMUCL added.
;;;
;;; Revision 1.3  1998/06/16 14:38:30  sds
;;; Replaced division with recursion in `!!'.
;;;
;;; Revision 1.2  1998/05/27 21:24:47  sds
;;; Added :key to `freqs' and moved it to list.lisp.
;;;
;;; Revision 1.1  1998/03/23 16:33:17  sds
;;; Initial revision
;;;

(in-package :cl-user)

(eval-when (load compile eval)
  (sds-require "base") (sds-require "list")
  (sds-require "print") (sds-require "matrix")
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

(define-modify-macro mulf (mult) * "Multiply the arg by a number.")
(define-modify-macro divf (mult) / "Divide the arg by a number.")
(defmacro sqr (xx)
  "Compute the square of a number, taking care to eval only once."
  (if (atom xx) `(* ,xx ,xx)
      (let ((var (gensym "SQR"))) `(let ((,var ,xx)) (* ,var ,var)))))

;;;
;;; Integers
;;;

(defun ! (nn)
  "Compute the factorial: n! = n * (n-1) * (n-2) * ..."
  (declare (fixnum nn) (values integer))
  #+clisp (lisp:! nn)
  #-clisp
  (labels ((ff (aa bb)
             (declare (fixnum aa bb) (values integer))
             (case (- bb aa)
               (1 bb) (2 (* (- bb 1) bb)) (3 (* (- bb 2) (- bb 1) bb))
               (4 (* (- bb 3) (- bb 2) (- bb 1) bb))
               (t (let ((mm (ash (+ aa bb) -1))) (* (ff aa mm) (ff mm bb)))))))
    (if (plusp nn) (ff 0 nn) 1)))

(defun !! (nn)
  "Compute the double factorial: n!! = n * (n-2) * (n-4) * ..."
  (declare (fixnum nn) (values integer))
  (multiple-value-bind (kk rr) (floor nn 2)
    (declare (fixnum kk) (type (integer 0 1) rr))
    (if (zerop rr) (* (! kk) (ash 1 kk))
        (labels ((ff (aa bb)
                   (declare (fixnum aa bb) (values integer))
                   (case (- bb aa)
                     (2 bb) (4 (* bb (- bb 2))) (6 (* bb (- bb 2) (- bb 4)))
                     (8 (* bb (- bb 2) (- bb 4) (- bb 6)))
                     (t (let ((mm (1+ (ash (ash (+ aa bb) -2) 1))))
                          (* (ff aa mm) (ff mm bb)))))))
          (if (> nn 1) (ff 1 nn) 1)))))

(defun stirling (xx)
  "Compute the approximate factorial using the Stirling formula."
  (let ((dd (dfloat xx)))
    (declare (double-float dd))
    (* (sqrt (* 2 pi dd)) (expt (/ dd (exp 1)) dd))))

(defun fibonacci (nn)
  "Return 2 consecutive Fibonacci numbers."
  (declare (type (integer 0) nn) (values (integer 0) (integer 0)))
  (case nn
    (0 (values 0 0)) (1 (values 1 0)) (2 (values 1 1)) (3 (values 2 1))
    (t (multiple-value-bind (mm rr) (floor nn 2)
         (declare (integer mm) (type (integer 0 1) rr))
         (multiple-value-bind (f0 f1) (fibonacci mm)
           (declare (type (integer 0) f0 f1))
           (if (zerop rr)
               (values (* f0 (+ (* f1 2) f0))
                       (+ (* f0 f0) (* f1 f1)))
               (values (+ (* f0 f0) (sqr (+ f0 f1)))
                       (* f0 (+ (* f1 2) f0)))))))))

(defcustom *primes* list nil "The list of primes.
The first element is the upper bound.")
(defcustom *primes-file* pathname (merge-pathnames "primes" *datadir*)
  "The file for keeping the list of primes.")

(defun primes-to (nn &optional int)
  "Return the list of primes up to N, exclusively.
The optional second argument, if non-nil, is a double float
specifying the interval for progress reports."
  (declare (fixnum nn) (type (or null double-float) int))
  (when (and (null *primes*) (probe-file *primes-file*))
    (setq *primes* (read-from-file *primes-file*)))
  (when (and *primes* (>= (car *primes*) nn))
    (return-from primes-to (cdr *primes*)))
  (do* ((ii 3 (+ 2 ii)) (res (if (> nn 2) (list 2))) (end res)
        (rt (isqrt ii) (isqrt ii)) (bt (get-float-time)))
       ((>= ii nn) (setq *primes* (cons nn res)) res)
    (declare (fixnum ii))
    (when (and int (= 1 (mod ii 1000)) (> (elapsed bt t) int))
      (format t "~:d..." ii) (force-output)
      (setq bt (get-float-time)))
    (do ((mm res (cdr mm)))
        ((or (null mm) (> (car mm) rt))
         (setq end (cdr (nconc end (list ii)))))
      (if (zerop (mod ii (car mm))) (return nil)))))

(defun divisors (nn &optional (primes-list (primes-to (1+ (isqrt nn)))))
  "Return the list of prime divisors of the given number.
The optional second argument specifies the list of primes."
  (declare (integer nn) (list primes-list))
  (labels ((ddd (n1 ds)
             (do ((pp primes-list (cdr pp)) (rt (isqrt n1)))
                 ((or (null pp) (> (car pp) rt)) (cons n1 ds))
               (declare (fixnum rt))
               (multiple-value-bind (dd rr) (floor n1 (car pp))
                 (declare (fixnum rr) (integer dd))
                 (when (zerop rr) (return (ddd dd (cons (car pp) ds))))))))
    (nreverse (ddd nn nil))))

(defsubst primep (nn &optional (primes-list (primes-to (1+ (isqrt nn)))))
  "Check whether the number is prime."
  (declare (integer nn) (list primes-list))
  (do ((pp primes-list (cdr pp)) (rt (isqrt nn)))
      ((or (null pp) (> (car pp) rt)) t)
    (declare (fixnum rt))
    (when (zerop (mod nn (car pp))) (return nil))))

(defun make-primes-list (&optional (limit most-positive-fixnum))
  "Initialize `*primes*' and write `*primes-file*'."
  (declare (fixnum limit))
  (let ((bt (get-float-time)) st)
    (format t "Computing primes up to ~:d..." limit) (force-output)
    (primes-to limit 10.0d0)
    (format t "done [~a]~%" (setq st (elapsed-1 bt t)))
    (write-to-file *primes* *primes-file* nil ";; Computed in " st
                   (format nil "~%;; Upper limit: ~:d~%;; ~:d primes~%"
                           limit (length *primes*)))))

;;;
;;; Ratios
;;;

(defun eval-cont-fract (fract)
  "Evaluate a continuous fraction, returning 2 values."
  (loop :for nn :of-type fixnum :in (reverse fract)
        :for v0 :of-type rational = nn :then (+ nn (/ v0))
        :for v1 :of-type rational = (1+ nn) :then (+ nn (/ v1))
        :finally (return (values v0 v1))))
  ;; (if (cdr fract)               ; recursive
  ;;     (multiple-value-bind (v0 v1) (eval-cont-fract (cdr fract))
  ;;       (values (+ (car fract) (/ v0)) (+ (car fract) (/ v1))))
  ;;     (values (car fract) (1+ (car fract)))))

(defcustom *relative-tolerance* double-float 1.0e-3
  "*The default relative tolerance for `approx='.")
(defcustom *absolute-tolerance* double-float 1.0d0
  "*The default absolute tolerance for `approx='.")
(defcustom *num-tolerance* double-float 1.0e-6
  "*The default numerical tolerance for `approx=-[abs|rel]'.")

(defun fract-approx (xx &optional (eps *num-tolerance*))
  "Find an approximation via continous fractions."
  (declare (real xx eps))
  (loop :with val = xx :and num :of-type fixnum
        :and app0 :and app1 :and err0 :and err1
        :for ii :upfrom 0
        :do (setf (values num val) (floor val))
        :collect num :into fract
        :when (zerop val) :do (return (values (eval-cont-fract fract) fract 0))
        :do (setf (values app0 app1) (eval-cont-fract fract)
                  err0 (- xx app0) err1 (- xx app1) val (/ val))
        (format t "~4d [~15a ~15a] [~12,8f ~12,8f] ~12,9f ~12,9f~%"
                num app0 app1 (dfloat app0) (dfloat app1) err0 err1)
        :while (if (integerp eps) (> eps ii)
                   (or (< eps (abs err0)) (< eps (abs err1))))
        :finally
        (return
          (if (<= (abs err0) (abs err1)) (values app0 fract err0)
              (values app1 (progn (incf (car (last fract))) fract) err1)))))

;;;
;;; Floats
;;;

(defun dot (l0 l1 &key (key #'value) (key0 key) (key1 key))
  "Compute the dot-product of the two sequences,
presumed to be of the same size."
  (declare (sequence l0 l1) (type (function (t) double-float) key key0 key1)
           (values double-float))
  (let ((res 0.0d0))
    (declare (double-float res))
    (map nil (lambda (r0 r1)
               (incf res (* (funcall key0 r0) (funcall key1 r1))))
         l0 l1)
    res))

(defun poly1 (var &rest coeffs)
  "Compute the polynomial with the given coefficients. Use the Horner scheme.
COEFFS are (a0 a1 a2 ...) for a0*x^n + a1*x^{n-1} + a2*x^{n-2}...
so that (poly1 10 1 2 3 4 5) ==> 12345."
  (declare (double-float var) (list coeffs) (values double-float))
  (let ((res 0.0d0))
    (declare (double-float res))
    (dolist (cc coeffs res)
      (declare (double-float cc))
      (setq res (+ cc (* var res))))))

(defun poly (var coeffs)
  "Compute the polynomial with the given coefficients. Use the Horner scheme.
COEFFS are #(a0 a1 a2 ...) for a0*x^n + a1*x^{n-1} + a2*x^{n-2}...
so that (poly 10 1 2 3 4 5) ==> 12345."
  (declare (double-float var) (type simple-vector coeffs)
           (values double-float))
  (loop :with res :of-type double-float = 0.0
        :for cc :of-type double-float :across coeffs
        :do (setq res (+ cc (* var res))) :finally (return res)))

(defun erf (xx)
  "Compute the error function, accurate to 1e-6. See Hull p. 243.
The same as
  (+ 0.5 (/ (integrate-simpson (lambda (tt) (exp (* tt tt -0.5))) 0 xx)
            (sqrt (* 2 pi))))
Return the value and the derivative, suitable for `newton'."
  (declare (double-float xx) (values double-float double-float))
  (let* ((der (/ (exp (* -0.5d0 (expt xx 2))) (dfloat (sqrt (* 2 pi)))))
         (val (- 1 (* der (poly (/ (1+ (* (abs xx) 0.2316419d0)))
                                #(1.330274429d0 -1.821255978d0 1.781477937d0
                                  -0.356563782d0 0.319381530d0 0.0d0))))))
    (declare (double-float der val))
    (values (if (minusp xx) (- 1 val) val) der)))

(defun norm (seq &key (key #'value) (order 1))
  "Compute the ORDERth norm of the SEQ. ORDER of 0 means infinity."
  (declare (sequence seq) (real order) (type (function (t) double-float) key))
  (case order
    (0 (reduce #'max seq :key (compose abs 'key)))
    (1 (reduce #'+ seq :key (compose abs 'key)))
    (2 (sqrt (reduce #'+ seq :key (lambda (xx) (expt (funcall key xx) 2)))))
    (t (expt (reduce #'+ seq :key
                     (lambda (xx) (expt (abs (funcall key xx)) order)))
             (/ order)))))

(defun normalize (seq &optional (norm #'norm))
  "Make the SEQ have unit norm. Drop nils."
  (declare (sequence seq) (type (or real (function (t) double-float)) norm))
  (setq seq (delete nil seq))
  (let ((nn (etypecase norm
              (real (norm seq :order norm))
              (function (funcall norm seq)))))
    (declare (double-float nn))
    (assert (> nn 0) (seq) "Zero norm vector: ~a" seq)
    (map-in (lambda (rr) (declare (double-float rr)) (/ rr nn)) seq)))

(defun rel-dist (seq1 seq2 &key (key #'value) (start1 0) (start2 0) depth)
  "Return the square of the relative mismatch between the 2 sequences."
  (declare (sequence seq1 seq2) (type (or null fixnum) depth)
           (type (function (t) double-float) key) (fixnum start1 start2))
  (let ((b1 (funcall key (elt seq1 start1))) (dist 0.0d0)
        (b2 (funcall key (elt seq2 start2)))
        (depth (or depth (min (- (length seq1) start1)
                              (- (length seq2) start2)))))
    (declare (double-float dist b1 b2) (fixnum depth))
    (mismatch seq1 seq2 :key key :start1 (1+ start1) :start2 (1+ start2)
              :end1 (+ start1 depth) :end2 (+ start2 depth) :test
              (lambda (k1 k2) (declare (double-float k1 k2))
                      (incf dist (expt (- (/ k1 b1) (/ k2 b2)) 2))))
    dist))

;;;
;;; Statistics
;;;

(defun mean (seq &key (key #'value) (len (length seq)))
  "Compute the mean of the sequence of real numbers.
Returns 2 values: the mean and the length of the sequence."
  (declare (sequence seq) (type (function (t) double-float) key)
           (fixnum len) (values double-float fixnum))
  (values (/ (reduce #'+ seq :key key) len) len))

(defun mean-cx (seq &key (key #'value) (len (length seq)))
  "Compute the mean of the sequence of complex numbers.
Returns 2 values: the mean and the length of the sequence."
  (declare (sequence seq) (type (function (t) (complex double-float)) key)
           (fixnum len) (values (complex double-float) fixnum))
  (values (/ (reduce #'+ seq :key key) len) len))

(defun weighted-mean (seq wts &key (key #'value))
  "Compute the weighted mean of the sequence SEQ
with weights WTS (not necessarily normalized)."
  (declare (sequence seq wts) (type (function (t) double-float) key)
           (values double-float))
  (/ (dot seq wts :key key) (reduce #'+ wts)))

(defsubst weighted-mean-cons (seq &key (key #'value))
  "Compute the weighted mean of the sequence of conses of numbers and
weights (not necessarily normalized)."
  (declare (sequence seq) (type (function (t) double-float) key)
           (values double-float))
  (weighted-mean (map 'list #'car seq) (map 'list #'cdr seq) :key key))

(defun geometric-mean (seq &key (key #'value))
  "Compute the geometric mean of the sequence of numbers.
Returns 2 values: the mean and the length of the sequence."
  (declare (sequence seq) (values double-float)
           (type (function (t) double-float) key))
  (let ((len (length seq)))
    (values (expt (reduce #'* seq :key key) (/ len)) len)))

(defun weighted-geometric-mean (seq wts &key (key #'value))
  "Compute the weighted geometric mean of the sequence SEQ
with weights WTS (not necessarily normalized)."
  (declare (sequence seq wts) (type (function (t) double-float) key)
           (values double-float))
  (let ((twt (reduce #'+ wts)) (res 1.0d0))
    (declare (double-float res twt))
    (map nil (lambda (rr wt)
               (declare (double-float wt))
               (setq res (* res (expt (funcall key rr) (/ wt twt)))))
         seq wts)
    res))

(defsubst weighted-geometric-mean-cons (seq &key (key #'value))
  "Compute the weighted geometric mean of the sequence
of conses of numbers and weights (not necessarily normalized)."
  (declare (sequence seq) (values double-float))
  (weighted-geometric-mean (map 'list #'car seq) (map 'list #'cdr seq)
                           :key key))

(defun mean-some (seq &key (key #'value))
  "Compute the mean of the sequence of real numbers.
NULLs are ignored, so this is like (mean (remove nil seq :key key) :key key).
Return 2 values: the mean and the length of the sequence."
  (declare (sequence seq) (type (function (t) (or null double-float)) key)
           (values double-float fixnum))
  (let ((len 0))
    (declare (type index-t len))
    (values (s/ (reduce #'+ seq :key (lambda (rr)
                                       (let ((val (funcall key rr)))
                                         (cond (val (incf len) val)
                                               (0.0)))))
                len)
            len)))

(defun standard-deviation (seq &key (len (length seq)) (key #'value)
                           (mean (mean seq :key key :len len)))
  "Compute the standard deviation of the sequence SEQ.
The mean and the length can be pre-computed for speed."
  (declare (sequence seq) (fixnum len) (double-float mean)
           (type (function (t) double-float) key)
           (values double-float double-float fixnum))
  (when (<= len 1) (return-from standard-deviation (values 0.0d0 mean len)))
  (values
   (sqrt (/ (reduce #'+ seq :key
                    (lambda (yy) (expt (- (funcall key yy) mean) 2)))
            (1- len))) mean len))

(defsubst standard-deviation-cx (&rest args)
  "Return the `standard-deviation' of SEQ as #C(mean stdd)."
  (declare (values (complex double-float)))
  (multiple-value-bind (stdd mean) (apply #'standard-deviation args)
    (complex mean stdd)))

(defun standard-deviation-relative (seq &key (key #'value))
  "Compute the relative standard deviation (StD(log(x[i+1]/x[i]))).
Meaningful only if all the numbers are of the same sign,
if this is not the case, the result will be a complex number."
  (declare (sequence seq) (type (function (t) double-float) key)
           (values double-float))
  (let (pr (sq 0.0) (su 0.0) (nn 0))
    (declare (double-float sq su) (type (or null double-float) pr)
             (type index-t nn))
    (map nil (lambda (rr)
               (let* ((cc (funcall key rr))
                      (vv (when pr (log (/ cc pr)))))
                 (declare (double-float cc) (type (or null double-float) vv))
                 (setq pr cc)
                 (when vv (incf sq (sqr vv)) (incf su vv) (incf nn))))
         seq)
    (values (sqrt (max 0.0 (/ (- sq (/ (sqr su) nn)) (1- nn)))) nn)))

(defun covariation (seq0 seq1 &key (key0 #'value) (key1 #'value))
  "Compute the covariation between the data in the two sequences.
Return 6 values: covariation, mean0, mean1, dispersion0,
dispersion1, number of elements considered.
Uses the fast but numerically unstable algorithm
without pre-computing the means."
  (declare (sequence seq0 seq1) (type (function (t) double-float) key0 key1)
           (values double-float double-float double-float (double-float 0.0)
                   (double-float 0.0) fixnum))
  (let ((xb 0.0d0) (yb 0.0d0) (x2b 0.0d0) (xyb 0.0d0) (y2b 0.0d0)
        (nn 0) (c0 0.0d0) (c1 0.0d0))
    (declare (double-float xb yb x2b xyb y2b c0 c1) (type index-t nn))
    (map nil (lambda (r0 r1)
               (let ((xx (funcall key0 r0)) (yy (funcall key1 r1)))
                 (declare (double-float xx yy))
                 (incf nn) (incf xb xx) (incf yb yy) (incf y2b (expt yy 2))
                 (incf xyb (* xx yy)) (incf x2b (expt xx 2))))
         seq0 seq1)
    (assert (> nn 1) (nn) "Too few (~d) points are given to covariation!" nn)
    (setq c0 (/ (dfloat nn)) c1 (/ (dfloat (1- nn))))
    (values (with-type double-float (* (- xyb (* xb yb c0)) c1))
            (with-type double-float (* xb c0))
            (with-type double-float (* yb c0))
            (with-type double-float (* (- x2b (* xb xb c0)) c1))
            (with-type double-float (* (- y2b (* yb yb c0)) c1))
            nn)))

(defun covariation1 (seq0 seq1 &key (key0 #'value) (key1 #'value))
  "Compute the covariation between the data in the two sequences.
Return 6 values: covariation, mean0, mean1, dispersion0,
dispersion1, number of elements considered.
Uses the numerically stable algorithm with pre-computing the means."
  (declare (sequence seq0 seq1) (type (function (t) double-float) key0 key1)
           (values double-float double-float double-float double-float
                   double-float fixnum))
  (let ((m0 (dfloat (mean seq0 :key key0)))
        (m1 (dfloat (mean seq1 :key key1)))
        (nn 0) (d0 0.0d0) (d1 0.0d0) (rr 0.0d0) (co 0.0d0))
    (declare (fixnum nn) (double-float m0 m1 d0 d1 rr co))
    (map nil (lambda (r0 r1)
               (let ((xx (- (funcall key0 r0) m0))
                     (yy (- (funcall key1 r1) m1)))
                 (declare (double-float xx yy))
                 (incf nn) (incf d0 (expt xx 2)) (incf d1 (expt yy 2))
                 (incf rr (* xx yy))))
         seq0 seq1)
    (assert (> nn 1) (nn) "Too few (~d) points are given to covariation!" nn)
    (setq co (/ (dfloat (1- nn))))
    (values (* rr co) m0 m1 (* d0 co) (* d1 co) nn)))

(defsubst cov (seq &key (xkey #'car) (ykey #'cdr))
  "Interface to `covariation' with one sequence."
  (covariation seq seq :key0 xkey :key1 ykey))

(defun volatility (lst split-key &key (key #'value))
  "Return volatilities for the terms corresponding to SPLIT-KEY.
The second value returned is the mean of the volatilities.
E.g., (volatility (currency-hist (find-currency 'dx))
                  (compose date-ye currency-rec-date)
                  :key #'currency-rec-avg)
will return the list of the volatilities for each year
and the average annual volatility for US Dollar Index."
  (declare (type (or function fixnum) split-key) (list lst)
           (type (function (t) double-float) key))
  (let ((vols (call-on-split lst #'standard-deviation-relative
                             :split-key split-key :key key :min-len 2)))
    (values (mean vols :key #'cdr) vols)))

;;; Mean / Deviation / Length

(eval-when (load compile eval)
(defstruct (mdl #+cmu (:print-function print-struct-object))
  (mn 0.0 :type double-float)   ; Mean
  (sd 0.0 :type (double-float 0.0)) ; Deviation
  (le 0 :type index-t))         ; Length
)

(defconst +bad-mdl+ mdl (make-mdl) "The convenient constant for init.")
(defmethod value ((mdl mdl)) (mdl-mn mdl))

(defmethod print-object ((mdl mdl) (stream stream))
  (if *print-readably* (call-next-method)
      (format stream "[~6f ~6f ~5:d]" (mdl-mn mdl) (mdl-sd mdl) (mdl-le mdl))))

(defun standard-deviation-mdl (seq &key (key #'value))
  "Compute an MDL from the SEQ."
  (let ((len (length seq)))
    (if (zerop len) +bad-mdl+
        (multiple-value-bind (std mean)
            (standard-deviation seq :len len :key key)
          (make-mdl :sd std :mn mean :le len)))))

;;;
;;; Misc
;;;

(defsubst below-p (x0 y0 x1 y1 x2 y2)
  "Check whether (x0 y0) is below the line (x1 y1) -- (x2 y2)."
  (declare (double-float x0 y0 x1 y1 x2 y2))
  (< y0 (with-type double-float
          (/ (+ (* y1 (- x2 x0)) (* y2 (- x0 x1))) (- x2 x1)))))

(defsubst linear (x0 y0 x1 y1 tt)
  "Compute the linear function through (x0 y0) and (x1 y1) at tt."
  (declare (double-float x0 y0 x1 y1 tt) (values double-float))
  (with-type double-float
    (/ (+ (* y0 (- x1 tt)) (* y1 (- tt x0))) (- x1 x0))))

(defmacro safe-fun (func pred &optional default)
  "Return a function that will run FUNC when the PRED is non-NIL.
If PRED is NIL return DEFAULT or the arguments if DEFAULT is omitted."
  (if default
      `(lambda (&rest xx) (if (apply ,pred xx) (apply ,func xx) ,default))
      `(lambda (&rest xx) (if (apply ,pred xx) (apply ,func xx)
                              (values-list xx)))))

(defmacro safe-fun1 (func pred &optional default)
  "Return a function that will run FUNC when the PRED is non-NIL.
Just like `safe-fun', but the returned function takes just 1 argument.
If PRED is NIL return DEFAULT or the arguments if DEFAULT is omitted."
  (if default
      `(lambda (xx) (if (,pred xx) (,func xx) ,default))
      `(lambda (xx) (if (,pred xx) (,func xx) xx))))

(defun safe-/ (aa &rest bb)
  "Safe division."
  (declare (number aa) (list bb) (values number))
  (if (some #'zerop bb)
      (cond ((zerop aa) 1) ((plusp aa) most-positive-fixnum)
            (most-negative-fixnum))
      (apply #'/ aa bb)))

(defsubst s/ (aa bb)
  "Fast safe division; only 2 arguments are allowed."
  (declare (number aa bb) (values number))
  (if (zerop bb) (cond ((zerop aa) 1) ((plusp aa) most-positive-fixnum)
                       (most-negative-fixnum))
      (/ aa bb)))

(defsubst d/ (aa bb)
  "Double float fast safe division; only 2 arguments are allowed."
  (declare (double-float aa bb) (values double-float))
  (if (zerop bb) (cond ((zerop aa) 1.0d0)
                       ((plusp aa) most-positive-double-float)
                       (most-negative-double-float))
      (/ aa bb)))

(defun convex-hull1 (lst up &key (xkey #'car) (ykey #'cdr))
  "Compute the `directional' convex hull of a list of 2d points.
More precisely, destructively modify LST, deleting all the points
that lie below (if UP is non-nil) or above (if UP is nil) the upper
\(or lower) part of the boundary of the convex hull.
The accessor keys XKEY and YKEY default to CAR and CDR.
Return the modified LST. 20% slower than `convex-hull'."
  (declare (list lst) (type (function (t) double-float) xkey ykey))
  (do ((mod t) dd (ts (if up #'minusp #'plusp))) ((null mod) lst)
    (setq mod nil)
    (do ((ll lst (cdr ll))) ((null (cddr ll)))
      (setq dd (- (funcall ykey (second ll))
                  (linear (funcall xkey (first ll)) (funcall ykey (first ll))
                          (funcall xkey (third ll)) (funcall ykey (third ll))
                          (funcall xkey (second ll)))))
      (when (funcall ts dd)
        (setf (cdr ll) (cddr ll) mod t)))))

(defun convex-hull (lst up &key (xkey #'car) (ykey #'cdr))
  "Compute the `directional' convex hull of a list of 2d points.
More precisely, destructively modify LST, deleting all the points
that lie below (if UP is non-nil) or above (if UP is nil) the upper
\(or lower) part of the boundary of the convex hull.
The accessor keys XKEY and YKEY default to CAR and CDR.
Return the modified LST. 20% faster than `convex-hull1'."
  (declare (list lst) (type (function (t) double-float) xkey ykey))
  (do* ((ll lst (cdr ll)) (ts (if up #'> #'<)) (x0 0.0d0) (y0 0.0d0)
        (sl (lambda (pp)        ; has to be d/!!!
              (d/ (with-type double-float (- (funcall ykey pp) y0))
                  (abs (with-type double-float (- (funcall xkey pp) x0)))))))
       ((null (cddr ll)) lst)
    (declare (double-float x0 y0) (type function ts)
             (type (function (t) double-float) sl))
    (setf x0 (funcall xkey (car ll)) y0 (funcall ykey (car ll))
          (cdr ll)
          (do ((l1 (cddr ll) (cdr l1)) (top (cdr ll)) (csl 0.0d0)
               (tsl (funcall sl (cadr ll))))
              ((null l1) top)
            (declare (double-float csl tsl))
            (setq csl (funcall sl (car l1)))
            (when (funcall ts csl tsl) (setq tsl csl top l1))))))

(defun sharpe-ratio (seq &key (key #'value))
  "Compute the Sharpe ratio (mean/StD) of the sequence SEQ."
  (declare (sequence seq))
  (multiple-value-bind (mm len) (mean seq :key key)
    (s/ mm (standard-deviation seq :mean mm :len len :key key))))

(defmacro to-percent (vv)
  "1.234 ==> 23.4%"
  `(* 100.0d0 (1- ,vv)))

(defun percent-change (v0 v1 &optional days)
  "Return the percent change in values, from V0 to V1.
If the optional DAYS is given, return the annualized change too."
  (declare (number v0 v1) (type (or null number) days)
           (values double-float (or null double-float)))
  (let ((pers (dfloat (/ v1 v0))))
    (if days
        (values (to-percent pers) (to-percent (expt pers (/ 365.25d0 days))))
        (to-percent pers))))

(defun rel-diff (v0 v1)
  "Return the relative difference between the two numbers.
This function is commutative, and puts the smallest number into the
denominator.  Sign is ignored."
  (declare (double-float v0 v1) (values double-float))
  (d/ (abs (- v1 v0)) (min (abs v0) (abs v1))))

(defsubst approx=-abs (f0 f1 &optional (tol *num-tolerance*))
  "Return T if the args are the same within TOL,
which defaults to *num-tolerance*."
  (declare (number f0 f1 tol))
  (< (abs (- f0 f1)) tol))

(defsubst approx=-rel (f0 f1 &optional (tol *num-tolerance*))
  "Return T if the args are the same relatively within TOL,
which defaults to *num-tolerance*. The first number goes to the
denominator."
  (declare (number f0 f1 tol))
  (< (abs (- f0 f1)) (abs (* f0 tol))))

(defsubst approx= (v0 v1 &optional (rt *relative-tolerance*)
                   (at *absolute-tolerance*))
  "Check whether the two numbers are the same within the relative and
absolute tolerances (which default to *relative-tolerance* and
*absolute-tolerance*)."
  (declare (number v0 v1 rt at))
  (and (> at (abs (- v0 v1))) (> rt (rel-diff v0 v1))))

(defun newton (ff &key (val 0) (ival val) (tol *num-tolerance*) (max-it -1))
  "Solve the equation FF(x)=VAL using the Newton's method.
FF should return its derivative as the second value.
IVAL is the initial approximation, and it better be good!
MAX-IT is the maximum number of iterations. If -1 (default), unlimited.
Returns the solution, the last change (more or less the error),
and the number of iterations made."
  (declare (type (function (number) (values number number)) ff)
           (number val ival) (fixnum max-it) (values number number fixnum))
  (do ((xx ival) f0 f1 (del 10) (it 0 (1+ it)))
      ((or (< (abs del) tol) (= max-it it)) (values xx del it))
    (declare (type index-t it))
    (setf (values f0 f1) (funcall ff xx))
    (incf xx (setq del (/ (- val f0) (if (zerop f1) tol f1))))))

(defun integrate-simpson (ff x0 xm &optional (eps *num-tolerance*))
  "Compute an integral of a real-valued function with a given precision.
Returns the integral, the last approximation, and the number of points."
  (declare (double-float x0 xm eps)
           (type (function (double-float) double-float) ff))
  (do* ((f0 (funcall ff x0)) (f1 (funcall ff (* 0.5 (+ x0 xm))))
        (fm (funcall ff xm)) (hh (* 0.5 (- xm x0)) (* hh 0.5))
        (mm 2 (* mm 2))
        (sum-even 0.0 (+ sum-odd sum-even))
        (sum-odd
         f1 (loop :for ii :of-type index-t :from 1 :below mm :by 2
                  :and step :of-type double-float :from 1.0d0 :by 2.0d0
                  :sum (funcall ff (+ x0 (* hh step))) double-float))
        (int-last 0.0 int)
        (int (* (/ hh 3.0) (+ f0 (* 4.0 f1) fm))
             (* (/ hh 3.0) (+ f0 (* 4.0 sum-odd) (* 2.0 sum-even) fm))))
       ((< (abs (- int int-last)) eps) (values int int-last mm))
    (declare (double-float sum-odd sum-even hh f0 f1 fm int int-last)
             (type index-t mm))))

(defun add-probabilities (&rest pp)
  "Add probabilities.
Returns the probability of at least one event happening."
  (- 1 (reduce #'* pp :key (lambda (xx) (- 1 xx)))))

;;;
;;; Line
;;;

(eval-when (load compile eval)
(defstruct (line #+cmu (:print-function print-struct-object))
  "A straight line."
  (sl 0.0d0 :type double-float) ; slope
  (co 0.0d0 :type double-float)) ; constant
)

(defconst +bad-line+ line (make-line) "*The convenient constant for init.")

(defmethod print-object ((ln line) (stream stream))
  (if *print-readably* (call-next-method)
      (format stream "{~6f ~6f}" (line-sl ln) (line-co ln))))

(defsubst line-val (ln par)
  "Evaluate the line at point."
  (declare (type line ln) (double-float par) (values double-float))
  (with-type double-float (+ (* par (line-sl ln)) (line-co ln))))

(defsubst line-rsl (ln)
  "Return the relative slope of the line."
  (declare (type line ln) (values double-float))
  (d/ (line-sl ln) (line-co ln)))

(defsubst line-below-p (ln xx yy)
  "Return T if the point (xx yy) is below the line LN."
  (declare (double-float xx yy) (type line ln))
  (< yy (line-val ln xx)))

(defsubst line-above-p (ln xx yy)
  "Return T if the point (xx yy) is above the line LN."
  (declare (double-float xx yy) (type line ln))
  (> yy (line-val ln xx)))

(defun intersect (line x0 y0 x1 y1)
  "Return T if the points (x0 y0) and (x1 y1) are on the opposite
sides of the LINE. Call: (intersect LINE X0 Y0 X1 Y1)"
  (declare (type line line) (double-float x0 y0 x1 y1))
  (not (plusp (* (- y0 (line-val line x0)) (- y1 (line-val line x1))))))

(defmacro with-line (ln xx yy above below upon
                     &optional (tol *num-tolerance*))
  "Eval ABOVE/BELOW/UPON depending on the relative position of line LN and
point (XX YY) up to tolerance TOL.  Similar to FORTRAN's arithmetic IF."
  (let ((di (gensym "WL")))
    `(let ((,di (- (line-val ,ln ,xx) ,yy)))
      (declare (double-float ,di))
      (cond ((> ,di ,tol) ,below) ((< ,di (- ,tol)) ,above) (t ,upon)))))

(defsubst line-adjust (ln xx yy)
  "Adjust the line LN to pass through the point, keeping the slope intact."
  (declare (double-float xx yy) (type line ln) (values line))
  (setf (line-co ln) (with-type double-float (- yy (* (line-sl ln) xx)))) ln)

(defun line-adjust-dir (ln xx yy up)
  "Adjust the line LN to be above (if UP) or below (otherwise) of (xx yy)."
  (declare (double-float xx yy) (type line ln))
  (if (funcall (if up #'line-above-p #'line-below-p) ln xx yy)
      (line-adjust ln xx yy) ln))

(defun line-adjust-list (ln ls up &key (xkey #'car) (ykey #'cdr))
  "Adjust the line LN to pass above (if UP) or below (otherwise) of LS,
keeping the slope intact."
  (declare (type line ln) (list ls)
           (type (function (t) double-float) xkey ykey))
  (do ((ff (if up #'line-above-p #'line-below-p))
       (ll ls (cdr ll)) (xx 0.0d0) (yy 0.0d0))
      ((endp ll) ln)
    (declare (double-float xx yy) (type function ff))
    (setq xx (funcall xkey (car ll)) yy (funcall ykey (car ll)))
    (when (funcall ff ln xx yy) (line-adjust ln xx yy))))

(defsubst line-thru-points (x0 y0 x1 y1)
  "Make a new line, passing through these 2 points.
If (= x0 x1), an error will be signaled."
  (declare (double-float x0 y0 x1 y1) (values line))
  (make-line :co (with-type double-float (/ (- (* y0 x1) (* y1 x0)) (- x1 x0)))
             :sl (with-type double-float (/ (- y1 y0) (- x1 x0)))))

(defun regress (seq &key (xkey #'car) (ykey #'cdr))
  "Return the regression line for the sequence of 2d points.
The second value returned is the deviation from the line.
The accessor keys XKEY and YKEY default to CAR and CDR respectively."
  (declare (sequence seq) (type (function (t) double-float) xkey ykey)
           (values line (double-float 0.0)))
  (case (length seq)
    ((0 1) (error "regress: too few points: ~s~%" seq))
    (2 (values (line-thru-points (funcall xkey (elt seq 0))
                                 (funcall ykey (elt seq 0))
                                 (funcall xkey (elt seq 1))
                                 (funcall ykey (elt seq 1)))
               0.0d0))
    (t (multiple-value-bind (co xm ym xd yd nn)
           (cov seq :xkey xkey :ykey ykey)
         (declare (double-float co xm ym xd yd) (fixnum nn))
         (let* ((sl (/ co xd)) (err (d/ (- yd (* sl co)) yd)))
           (declare (double-float sl err))
           (when (minusp err)
             (mesg :err t "REGRESS: error is negative: ~f (assumed 0) [len: ~d]
~s~%" err nn seq)
             (setq err 0.0d0))
           (values (make-line :sl sl :co
                              (with-type double-float (- ym (* xm sl))))
                   (sqrt err)))))))

(defun regress-n (yy xx nx &key (func #'aref))
  "Returns: vector [b1 ... bn], free term, Rmult, Ftest."
  (declare (type (simple-array double-float (*)) yy)
           (type simple-array xx) (fixnum nx)
           (type (function (array fixnum fixnum) double-float) func)
           (values (simple-array double-float (*)) double-float
                   (double-float 0.0 1.0) (double-float 0.0)))
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
    (handler-case (matrix-solve mx (replace cfs rhs))
      (division-by-zero (co)
        (format t "regress-n: `matrix-solve' failed on:~%~/matrix-print/~a"
                mx co)
        (let ((det (matrix-inverse mx)))
          (assert (/= 0 det) (mx) "the matrix is degenerate (det = 0)~%")
          (format t "det == ~f; using `matrix-inverse'.~%" det)
          (matrix-multiply-mat-col mx rhs cfs))))
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

(defsubst lincom (c0 x0 c1 x1)
  "Compute c0*x0+c1*x1."
  (declare (double-float c0 x0 c1 x1) (values double-float))
  (with-type double-float (+ (* c0 x0) (* c1 x1))))

(provide "math")
;;; math.lisp ends here
