;;; File: <math.lisp - 1998-03-23 Mon 11:22:35 EST sds@mute.eaglets.com>
;;;
;;; Math utilities (Arithmetical / Statistical functions)
;;;
;;; Copyright (C) 1997 by Sam Steingold.
;;; This is free software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: math.lisp,v 1.1 1998/03/23 16:33:17 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/math.lisp,v $
;;; $Log: math.lisp,v $
;;; Revision 1.1  1998/03/23 16:33:17  sds
;;; Initial revision
;;;

(eval-when (load compile eval) (sds-require "base"))

(declaim (optimize (speed 3) (space 0) (safety 3) (debug 3)))

(define-modify-macro mulf (mult) * "Multiply the arg by a number.")
(define-modify-macro divf (mult) / "Divide the arg by a number.")

(defmacro sqr (xx)
  "Compute the square of a number, taking care to eval only once."
  (if (atom xx) `(* ,xx ,xx)
      (let ((var (gensym "SQR"))) `(let ((,var ,xx)) (* ,var ,var)))))

(declaim (ftype (function (sequence sequence &key (:key function)
			   (:key1 function) (:key0 function))
			  double-float) dot))
(defun dot (l0 l1 &key (key #'value) (key0 key) (key1 key))
  "Compute the dot-product of the two sequences,
presumed to be of the same size."
  (declare (sequence l0 l1) (type (function (t) double-float) key key0 key1))
  (let ((res 0.0))
    (declare (double-float res))
    (map nil (lambda (r0 r1)
	       (incf res (* (funcall key0 r0) (funcall key1 r1))))
	 l0 l1)
    res))

(declaim (ftype (function (double-float &rest double-float) double-float)
		poly))
(defun poly (var &rest coeffs)
  "Compute the polynomial with the given coefficients. Use the Horner scheme.
COEFFS are (a0 a1 a2 ...) for a0*x^n + a1*x^{n-1} + a2*x^{n-2}...
so that (poly 10 1 2 3 4 5) ==> 12345."
  (declare (double-float var) (list coeffs))	; number
  (let ((res 0.0))
    (declare (double-float res))
    (dolist (cc coeffs res)
      (declare (double-float cc))
      (setq res (+ cc (* var res))))))

(declaim (ftype (function (double-float) double-float) erf))
(defun erf (xx)
  "Compute the error function, accurate to 1e-6. See Hull p. 243.
Return the value and the derivative, suitable for `newton'."
  (declare (double-float xx))	; real
  (let* ((der (/ (exp (* -0.5 (sqr xx))) (sqrt (* 2 pi))))
	 (val (- 1 (* der (poly (/ 1 (1+ (* (abs xx) 0.2316419)))
				1.330274429 -1.821255978 1.781477937
				-0.356563782 0.319381530 0.0)))))
    (declare (double-float der val))
    (values (if (minusp xx) (- 1 val) val) der)))

(defun norm (seq &key (key #'value) (order 1))
  "Compute the ORDERth norm of the SEQ. ORDER of 0 means infinity."
  (declare (sequence seq) (real order) (type (function (t) double-float) key))
  (case order
    (0 (reduce #'max seq :key (compose #'abs key)))
    (1 (reduce #'+ seq :key (compose #'abs key)))
    (2 (sqrt (reduce #'+ seq :key (lambda (xx) (sqr (abs (funcall key xx)))))))
    (t (expt (reduce #'+ seq :key
		     (lambda (xx) (expt (abs (funcall key xx)) order)))
	     (/ 1.0 (double-float order))))))

(defun normalize (lst &optional (norm #'norm))
  "Make the list have unit norm. Drop nils."
  (declare (list lst) (type (function (t) double-float) norm))
  (setq lst (delete nil lst))
  (let ((nn (funcall norm lst)))
    (declare (double-float nn))
    (when (zerop nn) (error "Zero list: ~a" lst))
    (map-in (lambda (rr) (declare (double-float rr)) (/ rr nn)) lst)))

(defun rel-dist (seq1 seq2 &key (key #'value) (start1 0) (start2 0) depth)
  "Return the square of the relative mismatch between the 2 sequences."
  (declare (sequence seq1 seq2) (type (or null fixnum) depth)
	   (type (function (t) double-float) key) (fixnum start1 start2)
	   (optimize (speed 1)))
  (let ((b1 (funcall key (elt seq1 start1))) (dist 0.0)
	(b2 (funcall key (elt seq2 start2)))
	(depth (or depth (min (- (length seq1) start1)
			      (- (length seq2) start2)))))
    (declare (double-float dist b1 b2) (fixnum depth))
    (mismatch seq1 seq2 :key key :start1 (1+ start1) :start2 (1+ start2)
	      :end1 (+ start1 depth) :end2 (+ start2 depth) :test
	      (lambda (k1 k2) (declare (double-float k1 k2))
		      (incf dist (sqr (- (/ k1 b1) (/ k2 b2))))))
    dist))

(defun freqs (seq &key (test #'eql))
  "Return an alist of (num . freq) of elements of the SEQ.
The alist is sorted by decreasing frequencies. TEST defaults to `eql'."
  (declare (sequence seq) (type (function (t t) t) test) (optimize (speed 1)))
  (let (res)
    (declare (list res))
    (map nil (lambda (el)
	       (let ((fi (find el res :key #'car :test test)))
		 (declare (type (or null cons) fi))
		 (if fi (incf (cdr fi)) (push (cons el 1) res)))) seq)
    (sort res #'> :key #'cdr)))

(defun mean (seq &key (key #'value))
  "Compute the mean of the sequence of numbers.
Returns 2 values: the mean and the length of the sequence."
  (declare (type sequence seq) (type (function (t) double-float) key)
	   (optimize (speed 1)))
  (let ((len (length seq)))
    (values (/ (reduce #'+ seq :key key) len) len)))

(defun weighted-mean (seq wts &key (key #'value))
  "Compute the weighted mean of the sequence SEQ
with weights WTS (not necessarily normalized)."
  (declare (type sequence seq wts) (type (function (t) double-float) key)
	   (optimize (speed 1)))
  (/ (dot seq wts :key key) (reduce #'+ wts)))

(defsubst weighted-mean-cons (seq &key (key #'value))
  "Compute the weighted mean of the sequence of conses of numbers and
weights (not necessarily normalized)."
  (declare (type sequence seq) (type (function (t) double-float) key))
  (weighted-mean (map 'list #'car seq) (map 'list #'cdr seq) :key key))

(defun geometric-mean (seq &key (key #'value))
  "Compute the geometric mean of the sequence of numbers.
Returns 2 values: the mean and the length of the sequence."
  (declare (type sequence seq)
	   (type (function (t) double-float) key) (optimize (speed 1)))
  (let ((len (length seq)))
    (values (expt (reduce #'* seq :key key) (/ 1 len)) len)))

(defun weighted-geometric-mean (seq wts &key (key #'value))
  "Compute the weighted geometric mean of the sequence SEQ
with weights WTS (not necessarily normalized)."
  (declare (type sequence seq wts) (type (function (t) double-float) key))
  (let ((twt (reduce #'+ wts)) (res 1.0))
    (declare (double-float res twt))
    (map nil (lambda (rr wt)
	       (declare (double-float wt))
	       (setq res (* res (expt (funcall key rr) (/ wt twt)))))
	 seq wts)
    res))

(defsubst weighted-geometric-mean-cons (seq &key (key #'value))
  "Compute the weighted geometric mean of the sequence
of conses of numbers and weights (not necessarily normalized)."
  (declare (type sequence seq))
  (weighted-geometric-mean (map 'list #'car seq) (map 'list #'cdr seq)
			   :key key))

(defun standard-deviation (seq &key (len (length seq)) (key #'value)
			   (mean (mean seq :key key)))
  "Compute the standard deviation of the sequence SEQ.
The mean and the length can be pre-computed for speed."
  (declare (type sequence seq) (fixnum len) (double-float mean)
	   (type (function (t) double-float) key))
  (when (<= len 1) (return-from standard-deviation (values 0.0 mean len)))
  (values
   (sqrt (/ (reduce #'+ seq :key (lambda (yy) (sqr (- (funcall key yy) mean))))
	    (1- len))) mean len))

(defun standard-deviation-relative (seq &key (key #'value))
  "Compute the relative standard deviation (StD/mean).
Meaningful only if all the numbers are of the same sign."
  (declare (sequence seq) (type (function (t) double-float) key))
  (multiple-value-bind (mean len) (mean seq :key key)
    (declare (fixnum len) (double-float mean))
    (if (or (<= len 1) (zerop mean)) 0
	(/ (standard-deviation seq :len len :key key :mean mean)
	   (abs mean)))))

(defun covariation (seq0 seq1 &key (key0 #'value) (key1 #'value))
  "Compute the covariation between the data in the two sequences.
Return 6 values: covariation, mean0, mean1, dispersion0,
dispersion1, number of elements considered.
Uses the fast but numerically unstable algorithm
without pre-computing the means."
  (declare (sequence seq0 seq1) (type (function (t) double-float) key0 key1))
  (let ((xb 0.0) (yb 0.0) (x2b 0.0) (xyb 0.0) (y2b 0.0) (nn 0) (co 0.0))
    (declare (double-float xb yb x2b xyb y2b co) (fixnum nn))
    (map nil (lambda (r0 r1)
	       (let ((xx (funcall key0 r0)) (yy (funcall key1 r1)))
		 (declare (double-float xx yy))
		 (incf nn) (incf xb xx) (incf yb yy) (incf y2b (sqr yy))
		 (incf xyb (* xx yy)) (incf x2b (sqr xx))))
	 seq0 seq1)
    (assert (> nn 1) (nn) "Too few (~d) points are given to covariation!" nn)
    (setq co (/ 1. (double-float nn)))
    (mulf xb co) (mulf yb co) (mulf xyb co) (mulf x2b co) (mulf y2b co)
    (values (- xyb (* xb yb)) xb yb (- x2b (sqr xb)) (- y2b (sqr yb)) nn)))

(defsubst cov (seq &key (xkey #'car) (ykey #'cdr))
  "Interface tp `covariation' with one sequence."
  (covariation seq seq :key0 xkey :key1 ykey))

(defun volatility (lst split-key &key (key #'value))
  "Return volatilities for the terms corresponding to SPLIT-KEY.
The second value returned is the mean of the volatilities.
E.g., (volatility (currency-hist (get-currency 'dx))
                  (compose #'date-ye #'currency-rec-date)
                  :key #'currency-rec-avg)
will return the list of the volatilities for each year
and the average annual volatility for US Dollar Index."
  (declare (type (or function fixnum) split-key) (list lst)
	   (type (function (t) double-float) key) (optimize (speed 1)))
  (unwind-protect
       (let* ((idx (typecase split-key (function nil) (fixnum 0)))
	      (vols (mapcar (lambda (ll)
			      (declare (type (or null fixnum) idx))
			      (cons (if idx (incf idx)
					(funcall split-key (car ll)))
				    (standard-deviation-relative ll :key key)))
			    (setq lst (nsplit-list lst :key split-key)))))
	 (values vols (mean vols :key #'cdr)))
    (setq lst (apply #'nconc lst))))

(defsubst below-p (x0 y0 x1 y1 x2 y2)
  "Check whether (x0 y0) is below the line (x1 y1) -- (x2 y2)."
  (declare (double-float x0 y0 x1 y1 x2 y2)) ; real
  (< y0 (/ (+ (* y1 (- x2 x0)) (* y2 (- x0 x1))) (- x2 x1))))

(defsubst linear (x0 y0 x1 y1 tt)
  "Compute the linear function through (x0 y0) and (x1 y1) at tt."
  (declare (double-float x0 y0 x1 y1 tt)) ; real
  (/ (+ (* y0 (- x1 tt)) (* y1 (- tt x0))) (- x1 x0)))

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
  (do* ((ll lst (cdr ll)) (ts (if up #'> #'<)) x0 y0
	(sl (lambda (pp)
	      (declare (double-float x0 y0))
	      (s/ (- (funcall ykey pp) y0)
		  (abs (- (funcall xkey pp) x0))))))
       ((null (cddr ll)) lst)
    (setq x0 (funcall xkey (car ll)) y0 (funcall ykey (car ll)))
    (setf (cdr ll)
	  (do ((l1 (cddr ll) (cdr l1)) (top (cdr ll)) csl
	       (tsl (funcall sl (cadr ll))))
	      ((null l1) top)
	    (setq csl (funcall sl (car l1)))
	    (when (funcall ts csl tsl) (setq tsl csl top l1))))))

(defun s/ (aa bb)
  "Fast safe division; only 2 arguments are allowed."
  (declare (number aa bb) (optimize (speed 1)))
  (if (zerop bb) (cond ((zerop aa) 1) ((plusp aa) most-positive-fixnum)
		       ((- most-positive-fixnum)))
      (/ aa bb)))

(defun safe-/ (aa &rest bb)
  "Safe division."
  (declare (number aa) (list bb) (optimize (speed 1)))
  (if (some #'zerop bb)
      (cond ((zerop aa) 1) ((plusp aa) most-positive-fixnum)
	    (- most-positive-fixnum))
      (apply #'/ aa bb)))

(defmacro safe-fun (func pred &optional default)
  "Return a function that will run FUNC when the PRED is non-NIL.
If PRED is NIL return DEFAULT or the arguments if DEFAULT is omitted."
  (if default
      `(lambda (&rest xx) (if (apply ,pred xx) (apply ,func xx) ,default))
      `(lambda (&rest xx) (if (apply ,pred xx) (apply ,func xx)
			      (apply #'values xx)))))

(defun sharpe-ratio (seq &key (key #'value))
  "Compute the Sharpe ratio (mean/StD) of the sequence SEQ."
  (declare (type sequence seq))
  (multiple-value-bind (mm len) (mean seq :key key)
    (s/ mm (standard-deviation seq :mean mm :len len :key key))))


(defmacro to-percent (vv)
  "1.234 ==> 23.4%"
  `(* 100.0d0 (1- ,vv)))

(defun percent-change (v0 v1 &optional days)
  "Return the percent change in values, from V0 to V1.
If the optional DAYS is given, return the annualized change too."
  (declare (number v0 v1) (optimize (speed 1)))
  (let ((pers (/ v1 v0)))
    (if days
	(values (to-percent pers) (to-percent (expt pers (/ 365.25 days))))
	(to-percent pers))))

(defun rel-diff (v0 v1)
  "Return the relative difference between the two numbers.
This function is commutative, and puts the smallest number into the
denominator.  Sign is ignored."
  (declare (double-float v0 v1)) ; real
  (s/ (abs (- v1 v0)) (min (abs v0) (abs v1))))

(defcustom *relative-tolerance* double-float 1.0e-3
  "*The default relative tolerance for `same-num-p'.")
(defcustom *absolute-tolerance* double-float 1.0
  "*The default absolute tolerance for `same-num-p'.")
(defcustom *num-tolerance* double-float 1.0e-6
  "*The default numerical tolerance for `same-num-[abs|rel]-p'.")

(defsubst same-num-abs-p (f0 f1 &optional (tol *num-tolerance*))
  "Return T if the args are the same within TOL,
which defaults to *num-tolerance*."
  (declare (number f0 f1 tol) (optimize (speed 1)))
  (< (abs (- f0 f1)) tol))

(defsubst same-num-rel-p (f0 f1 &optional (tol *num-tolerance*))
  "Return T if the args are the same relatively within TOL,
which defaults to *num-tolerance*. The first number goes to the
denominator."
  (declare (number f0 f1 tol) (optimize (speed 1)))
  (< (abs (- f0 f1)) (abs (* f0 tol))))

(defsubst same-num-p (v0 v1 &optional (rt *relative-tolerance*)
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
	   (number val ival) (fixnum max-it) (optimize (speed 1)))
  (do ((xx ival) f0 f1 (del 10) (it 0 (1+ it)))
      ((or (< (abs del) tol) (= max-it it)) (values xx del it))
    (declare (fixnum it))
    (setf (values f0 f1) (funcall ff xx))
    (incf xx (setq del (/ (- val f0) (if (zerop f1) tol f1))))))

;;;
;;; Line
;;;

(eval-when (load compile eval)
(defstruct (line (:print-function print-line))
  "A straight line."
  (sl 0.0 :type real)		; slope (double-float)
  (co 0.0 :type real))		; constant (double-float)
)

(defun print-line (ln &optional (stream t) (depth 1))
  "Print the line."
  (declare (type line ln))	; "#S(LINE :SL ~g :CO ~g)"
  (if *print-readably* (funcall (print-readably line) ln stream depth)
      (format stream (case depth (1 "Slope: ~,3f; Constant: ~,3f")
			   (t "{~,3f ~,3f}")) (line-sl ln) (line-co ln))))

(defsubst line-val (ln par)
  "Evaluate the line at point."
  (declare (type line ln) (real par)) ; double-float
  (+ (* par (line-sl ln)) (line-co ln)))

(defsubst line-rsl (ln)
  "Return the relative slope of the line."
  (declare (type line ln))
  (s/ (line-sl ln) (line-co ln)))

(defsubst line-below-p (ln xx yy)
  "Return T if the point (xx yy) is below the line LN."
  (declare (real xx yy) (type line ln))	; double-float
  (< yy (line-val ln xx)))

(defsubst line-above-p (ln xx yy)
  "Return T if the point (xx yy) is above the line LN."
  (declare (real xx yy) (type line ln))	; double-float
  (> yy (line-val ln xx)))

(defun intersect (line x0 y0 x1 y1)
  "Return T if the points (x0 y0) and (x1 y1) are on the opposite
sides of the LINE. Call: (intersect LINE X0 Y0 X1 Y1)"
  (declare (type line line) (real x0 y0 x1 y1))	; double-float
  (not (plusp (* (- y0 (line-val line x0)) (- y1 (line-val line x1))))))

(defmacro with-line (ln xx yy above below upon
		     &optional (tol *num-tolerance*))
  "Eval ABOVE/BELOW/UPON depending on the relative position of line LN and
point (XX YY) up to tolerance TOL.  Similar to FORTRAN's arithmetic IF."
  (let ((di (gensym "WL")))
    `(let ((,di (- (line-val ,ln ,xx) ,yy)))
      (cond ((> ,di ,tol) ,below) ((< ,di (- ,tol)) ,above) (t ,upon)))))

(defun line-adjust (ln xx yy)
  "Adjust the line LN to pass through the point, keeping the slope intact."
  (declare (real xx yy) (type line ln))	; double-float
  (setf (line-co ln) (- yy (* (line-sl ln) xx))) ln)

(defun line-adjust-dir (ln xx yy up)
  "Adjust the line LN to be above (if UP) or below (otherwise) of (xx yy)."
  (declare (real xx yy) (type line ln))	; double-float
  (if (funcall (if up #'line-above-p #'line-below-p) ln xx yy)
      (line-adjust ln xx yy) ln))

(defun line-adjust-list (ln ls up &key (xkey #'car) (ykey #'cdr))
  "Adjust the line LN to pass above (if UP) or below (otherwise) of LS,
keeping the slope intact."
  (declare (type line ln) (list ls)
	   (type (function (t) double-float) xkey ykey))
  (dolist (rr ls ln)
    (line-adjust-dir ln (funcall xkey rr) (funcall ykey rr) up)))

(defun line-thru-points (x0 y0 x1 y1)
  "Make a new line, passing through these 2 points.
If (= x0 x1), an error will be signaled."
  (declare (real x0 y0 x1 y1))	; double-float
  (line-adjust (make-line :sl (/ (- y0 y1) (- x0 x1))) x0 y0))

(defun regress (seq &key (xkey #'car) (ykey #'cdr))
  "Return the regression line for the sequence of 2d points.
The second value returned is the deviation from the line.
The accessor keys XKEY and YKEY default to CAR and CDR respectively."
  (declare (sequence seq) (type (function (t) double-float) xkey ykey))
  (multiple-value-bind (co xm ym xd yd nn sl err)
      (cov seq :xkey xkey :ykey ykey)
    (declare (double-float co xm ym xd yd) (fixnum nn))
    (setq sl (/ co xd) err (if (= nn 2) 0 (- yd (* sl co))))
    (assert (not (minusp err)) (err) "Error is negative: ~f~%" err)
    (values (make-line :sl sl :co (- ym (* xm sl))) (sqrt err))))

(provide "math")
;;; math.lisp ends here
