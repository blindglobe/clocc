;;;; $ID: rng.lisp,v 1.8 1999/11/17 16:48:11 toy Exp toy $
;;;; $Source: /cvsroot/clocc/clocc/src/cllib/rng.lisp,v $
;;;;
;;;;  Class of Random number generators
;;;;
;;;;  $Log: rng.lisp,v $
;;;;  Revision 1.1  2001/03/14 23:29:05  sds
;;;;  initial checkin
;;;;
;;;;  Revision 1.8  1999/11/17 16:48:11  toy
;;;;  o Correct some typos in the name of the exponential generator
;;;;    algorithms. (It's Knuth Algorith S, not F.)
;;;;  o Add another gamma generator (gen-gamma-variate-algo-a-2), based on
;;;;    Knuth's suggestion of using a polar method instead of computing
;;;;    tan(pi*u).
;;;;  o Add this new algorithm to the timing test.
;;;;
;;;;  Revision 1.7  1999/11/11 18:47:32  toy
;;;;  o Correct one bug in gen-std-exponential-algo-f (missing one shift).
;;;;    (The sample pdf is better, but it still seems to have some problems.
;;;;    The sample pdf for the log method is much, much better.)
;;;;  o Added generator for geometric distribution.
;;;;
;;;;  Revision 1.6  1999/11/08 17:49:58  toy
;;;;  Remove the output assertion in gen-std-exponential-variate-algo-f.
;;;;  (Speeds things up slightly.)
;;;;
;;;;  Revision 1.5  1999/11/08 16:01:53  toy
;;;;  o Added a deftype for non-negative floats.
;;;;  o Added Ahrens and Dieter's algorithm GO.
;;;;  o Added unstructured versions of some algorithm GN and GO.  These seem
;;;;    to run MUCH faster with MUCH less consing than the structured
;;;;    versions.  I don't know why that is.  They look the same to me.
;;;;
;;;;  Revision 1.4  1999/11/02 17:04:06  toy
;;;;  o Fix a bug in the exponential RV generator.
;;;;  o Move the classes out to another file.
;;;;  o Other minor random fixes.
;;;;
;;;;  Revision 1.3  1997/09/30 22:22:21  toy
;;;;  *** empty log message ***
;;;;
;;;;  Revision 1.2  1996/11/12 17:50:44  toy
;;;;  Lot's of changes that I don't remember, but lots of additions.
;;;;
;;;;  Revision 1.1  1996/10/24 22:12:10  toy
;;;;  Initial revision
;;;;

;; CLOCC should not do this, IMO:
;; (eval-when (compile)
;;   (declaim (optimize (speed 3))))

#+(and cmu negative-zero-is-not-zero)
(deftype non-negative-float (type &optional hi)
  `(or (,type ,(coerce 0 type) ,(or hi *))))

#-(and cmu negative-zero-is-not-zero)
(deftype non-negative-float (type &optional hi)
  `(or (member ,(coerce 0 type))
       (,type (,(coerce 0 type)) ,(or hi *))))

;;;;-------------------------------------------------------------------------
;;;;
;;;; Define the routines for the actual generators here.  If you know
;;;; what you are doing, you can use these routines directly instead
;;;; of encapsulating them within the classes.
;;;;
;;;;-------------------------------------------------------------------------

;;(eval-when (compile)
;;  (proclaim '(inline gen-std-exponential-variate-log-method)))

;;;;-------------------------------------------------------------------------
;;;;
;;;; Standard exponential random variate.
;;;;
;;;; f(x) = 1/m*e^{-x/m}, x >= 0
;;;; F(x) = 1 - e^{-x/m}, x >= 0
;;;;
;;;;-------------------------------------------------------------------------

(defun gen-std-exponential-variate-log-method (mu state)
  "Generate a pseudo-random number drawn from an exponential PDF with a
mean of mu:
                - X/MU
              %E
              --------
                 MU

 STATE is the random state to use.  The logarithmic method is used.
"
  (declare (random-state state)
	   (type (double-float (0d0)) mu))
  (* mu (- (log (random 1d0 state)))))

;; This table is Knuth's Q[k] in Algorithm S.
;;
;; Q[k] = sum (ln 2)^k/k! for k = 1, 2,...,
;;
;; until Q[k] > 1 - 2^(-t) where the uniform deviates have t+1 bit
;; accuracy.
;;
;;
;; Here's how we generated this table.  I used CLISP to get the extra
;; digits in the table.
;;
;;(do* ((nbits (float-digits 1d0))
;;      (k 1 (1+ k))
;;      (term #.(log 2L0) (* term (/ #.(log 2L0) k)))
;;      (sum term (+ sum term)))
;;     ((or (> sum (- 1 (scale-float 1l0 (- nbits))))
;;          (> k 20)))
;;  (format t "~3D ~50,45F ~25G ~%" k sum term))

(declaim (ftype (function ((double-float (0d0))
			   random-state)
			  (non-negative-float double-float))
		gen-std-exponential-variate-algo-s))

;;; Plotting a histogram of the outputs of this function shows that
;;; the pdf doesn't seem quite right.  I think I've implemented
;;; Knuth's Algorithm S correctly.
(let ((std-exponential-variate-table
       (make-array 16
		   :element-type 'double-float
		   :initial-contents
		   '(
		     0d0
		     0.693147180559945309417232121458176568075d0
		     0.933373687519046021750783384621509053940d0
		     0.988877796183867601703925648390130811298d0
		     0.998495925291496078865904719963789676778d0
		     0.999829281106138923208245942162589294252d0
		     0.999983316410072739307790313135916717732d0
		     0.999998569143876799148070338574928727372d0
		     0.999999890692555813579019178950751556207d0
		     0.999999992473415905976016453850827533653d0
		     0.999999999528327526777139783726219715203d0
		     0.999999999972881353964220933485860571090d0
		     0.999999999998559789957709138627855373484d0
		     0.999999999999928938843099551515944568884d0
		     0.999999999999996726106647776972279059926d0
		     0.999999999999999858543354865400900694870d0
		     ))))
  (declare (type (simple-array double-float (*))
                 std-exponential-variate-table))
  (defun gen-std-exponential-variate-algo-s (mu state)
    "Generate a pseudo-random number drawn from an exponential PDF with a
mean of 1:

                - X
              %E

 for X >= 0.

 STATE is the random state to use.  Knuth's Algorithm F is used to
 generate the variate.
"
    (declare (type (double-float (0d0)) mu)
	     (random-state state))

    (multiple-value-bind (new-u j)
	;; Step S1.  Find the first zero bit by comparing against 1/2.
	;; If more than 1/2, the leading bit is 1.  In this case,
	;; multiply by 2, and take the fractional part.  This drops the
	;; leading 1 bit.
	(do ((u-tst (random 1d0 state)
		    (if (>= u-tst 0.5d0)
			(- (+ u-tst u-tst) 1)
			(+ u-tst u-tst)))
	     (cnt 0 (1+ cnt)))
	    ((or (< u-tst 0.5d0)
		 (> cnt (float-digits 1d0)))
	     ;; We found the first zero bit.  Shift that out too.
	     (values (+ u-tst u-tst) cnt))
	  (declare (type (non-negative-float double-float (2d0)) u-tst)
		   (type (integer 0 #.(1+ (float-digits 1d0))) cnt))
	  #+nil
	  (multiple-value-bind (f e s)
	      (integer-decode-float u-tst)
	    (format t "u-tst = ~S~% -> <0.~53,'0,' ,4b> 2^~S~%"
		    u-tst (ash f (+ e (float-digits 1d0)))
		    (+ e (float-digits 1d0)))
	    (format t "cnt   = ~S~%" cnt))
	  )
      (declare (fixnum j))

      #+nil
      (multiple-value-bind (f e s)
	  (integer-decode-float new-u)
	(format t "j = ~S~%" j)
	(format t "new-u = ~S~%   <0.~53,'0,' ,4b> x 2^~S~%"
		new-u (ash f (+ e (float-digits 1d0)))
                (+ e (float-digits 1d0))))
      (cond ((< new-u (aref std-exponential-variate-table 1))
	     (* mu (+ new-u
		      (* j (aref std-exponential-variate-table 1)))))
	    (t
	     ;;(format t "u = ~S~%" new-u)
	     (do ((k 2 (1+ k))
		  (u-seq (min (random 1d0 state)
			      (random 1d0 state))
			 (min u-seq (random 1d0 state))))
		 ((< new-u (aref std-exponential-variate-table k))
		  (* mu
		     (aref std-exponential-variate-table k)
		     (+ j u-seq)
		     ))
	       (declare (type (non-negative-float double-float (1d0)) u-seq))
	       ;;(format t "Q[~S] = ~S~%" k (aref std-exponential-variate-table k))
	       ))))))

;; Use the ratio-of-uniforms method to generate exponential variates.
;; This could probably be optimized further.
(defun gen-std-exponential-variate-ratio (mu state)
  (declare (random-state state)
	   (type (double-float (0d0)) mu))
  (let ((max-v (* mu #.(* 2 (exp -1d0)))))
  (do ((u (random 1d0 state) (random 1d0 state))
       (v (random max-v state) (random max-v state)))
      ((<= (* u u) (exp (- (/ v u mu))))
       (/ v u)))))

;;; Pick the one that works best for you.
;;;
;;; 500000 numbers generated.  Time is elapsed real time in seconds,
;;; Second number is bytes consed.
;;;
;;; CPU	          N    Log      Algo-A  Ratio
;;; U-30 300  500000   0.69     0.85     1.6
;;;                    8M       8M       8M

(defmacro gen-std-exponential-variate (mu state)
  `(gen-std-exponential-variate-log-method ,mu ,state))

;;;;-------------------------------------------------------------------------
;;;;
;;;; Laplacian PDF
;;;;
;;;; f(x) = 1/2*exp(-|x|)
;;;;
;;;;-------------------------------------------------------------------------

(defun gen-std-laplacian-variate (state)
  "Generate a pseudo-random number for a Laplacian random variable, defined by

         1    -|X|
 f(X) = --- %E
         2

 for real X.
"
  (declare (random-state state))
  ;; Instead of using the inverse CDF to generate the random number,
  ;; we generate an exponential and flip its sign with probability
  ;; 1/2.
  (if (zerop (random 2 state))
      (gen-std-exponential-variate 1d0 state)
      (- (gen-std-exponential-variate 1d0 state))))

;;;;-------------------------------------------------------------------------
;;;;
;;;; Gaussian random variate
;;;;
;;;; f(x) = 1/sqrt(2*pi) e^{-1/2 * x^2}
;;;;
;;;;-------------------------------------------------------------------------

;;; Pick the best Gaussian generator.  Here are some numbers to help
;;; you decide.
;;;
;;; 50000 numbers generated.  Time is elapsed real time in seconds,
;;; Second number is bytes consed.
;;;
;;; CPU	          N    Box     Box trig    Ratio
;;; 486-66     50000   1.32     1.22       1.84
;;;                    1239k     825k       825k
;;; Sparc-20  100000   1.22     0.77       0.91
;;;                    2.4M     1.6M       1.6M
;;; U-30 300  500000   0.82     1.42       1.03
;;;                    8M       8M         8M

;;; We have three different generators for Gaussians.  Pick the one
;;; that works best for you.


#+x86
(defmacro gen-std-gaussian-variate (state)
  `(gen-std-gaussian-variate-box-trig ,state))

#-x86
(defmacro gen-std-gaussian-variate (state)
  `(gen-std-gaussian-variate-box-trig ,state))

(let ((use-prev-gauss nil)
      (prev-gauss 0d0))
  (declare (double-float prev-gauss))

  (defun gen-std-gaussian-variate-box (state)
    "Generate a pseudo-random number drawn from a Gaussian PDF with a mean
of zero and a variance of 1.

                    2
                   X
                 - --
                   2
               %E
          ----------------
          SQRT(2) SQRT(PI)

  STATE is the random state to use.

 The Box-Mueller method is used.  See Knuth, Seminumerical Algorithms,
 Algorithm P.
"
    (declare (random-state state))
    (cond (use-prev-gauss
	   ;; Use cached value if we have it.
	   (setf use-prev-gauss nil)
	   prev-gauss)
	  (t
	   ;; Compute new values via Box-Muller method
	   (do* ((u (- (random 2.0d0 state) 1.0d0)
		    (- (random 2.0d0 state) 1.0d0))
		 (v (- (random 2.0d0 state) 1.0d0)
		    (- (random 2.0d0 state) 1.0d0))
		 (s (+ (* u u) (* v v))
		    (+ (* u u) (* v v)))
		 )
		((< s 1.0d0)
		 (locally
		     (declare (type (non-negative-float double-float (1d0)) s))

		   (let ((z (sqrt (/ (* -2.0d0 (log s))
				     s))))
		     (setf prev-gauss (* v z))
		     (setf use-prev-gauss t)
		     (* u z)))))))))

(let ((use-prev-gauss nil)
      (prev-gauss 0d0))
  (declare (double-float prev-gauss))
  (defun gen-std-gaussian-variate-box-trig (state)
    "Generate a pseudo-random number drawn from a Gaussian PDF with a mean
of zero and a variance of 1.  The PDF is

                    2
                   X
                 - --
                   2
               %E
          ----------------
          SQRT(2) SQRT(PI)


 STATE is the random state to use.
 The Box-Mueller method is used but the acceptance/rejection method is
 replaced by direct calculation via trigonometric functions..  See
 Knuth, Seminumerical Algorithms.
"
    (declare (random-state state))
    (cond (use-prev-gauss
	   ;; Use cached value if we have it.
	   (setf use-prev-gauss nil)
	   prev-gauss)
	  (t
	   (let ((r1 (sqrt (* -2.0d0 (log (random 1.0d0 state)))))
		 (r2 (random #.(float (* (float pi 1d0) 2.0d0) 1d0) state)))
	     ;;(declare (double-float r1 r2))
	     (setf prev-gauss (* r1 (sin r2)))
	     (setf use-prev-gauss t)
	     (* r1 (cos r2)))))))

(let ((+sqrt-8/e+ #.(sqrt (/ 8.0d0 (exp 1.0d0))))
      (+4-exp-1/4+ #.(* 4.0d0 (exp 0.25d0)))
      (+4-exp-minus-1.35+ #.(* 4.0d0 (exp (- 1.35d0)))))
  (declare (double-float +sqrt-8/e+ +4-exp-1/4+ +4-exp-minus-1.35+))
  (defun gen-std-gaussian-variate-ratio (state)
    "Generate a pseudo-random number drawn from a Gaussian PDF with a mean
of zero and a variance of 1.

                    2
                   X
                 - --
                   2
               %E
          ----------------
          SQRT(2) SQRT(PI)

 STATE is the random state to use.

 The ratio of uniform variates method is used.  See Knuth,
 Seminumerical Algorithms, Algorithm R.
"
    (declare (random-state state))
    (do ((u (random 1.0d0 state) (random 1.0d0 state)))
	(nil)
      (declare (double-float u))
      (let* ((x (/ (* +sqrt-8/e+
		      (- (random 1.0d0 state) 0.5d0))
		   u))
	     (xs (* x x)))
	(declare (double-float x xs))
	(if (or (<= xs (- 5.0d0 (* u +4-exp-1/4+)))
		(and (<= xs (+ 1.4d0 (/ +4-exp-minus-1.35+ u)))
		     (<= xs (- (* 4.0d0 (log u))))))
	    (return-from gen-std-gaussian-variate-ratio x))))))

;;;;-------------------------------------------------------------------------
;;;;
;;;; Gamma random variate, of order a
;;;;
;;;; f(x) = 1/Gamma(a) x^{a-1} e^{-x}
;;;;
;;;;-------------------------------------------------------------------------

;;;
;;; Here are some timing results for the gamma generators
;;;
;;; 10000 numbers generated.  Time is elapsed real time in seconds,
;;; Second number is bytes consed.  (For the Ultra-30, 50000 numbers
;;; were generated.)
;;;
;;; Order = 1.1
;;; CPU	          Squeeze  GN         Algo. A           Algo GO
;;; 486-66        0.75     39.2       0.94
;;;               525k     11738k     391k
;;; Sparc20       0.19      9.0       0.25
;;;               335k     11493k     160k
;;; U-30 (300)    0.05      2.9       0.13
;;;               334k     12401k     160k
;;;
;;; Order = 10
;;; CPU	          Squeeze  GN         Algo. A  Direct
;;; 486-66        .71      14.9       0.93
;;;               498k     5026k      420k
;;; Sparc20       0.17      3.6       0.22
;;;               321k     4890k      160k
;;; U-30 (300)    0.28      5.00      0.30     0.31       0.26
;;;               2411k    21700k     800k     1600k      1874k
;;;
;;; Order = 100
;;; CPU	          Squeeze  GN         Algo. A
;;; 486-66        .70      13.1       0.93
;;;               496k     4254k      442k
;;; Sparc20       0.17      3.5       0.22
;;;               321k     4102k      160k
;;; U-30 (300)    0.28      0.28      0.31     2.54       0.22
;;;               2400k    1674k      800k     1600k      1663k
;;;
;;; Order = 1000
;;; CPU	          Squeeze  GN         Algo. A
;;; 486-66        .70      12.1       0.96
;;;               496k     4017k      450k
;;; Sparc20       0.17      2.8       0.22
;;;               321k     3878k      160k
;;; U-30 (300)    0.23      0.33      0.32                0.22
;;;               1600k    1627k      800k                1626k
;;;
;;; Order = 10000
;;; CPU	          Squeeze  GN         Algo. A
;;; 486-66        0.67     10.9       0.95
;;;               496K     3950K      458k
;;; Sparc20       0.17      2.6       0.25
;;;               321k     3800k      160k
;;; U-30 (300)    0.24      0.26      0.33                0.25
;;;               1600k    1612k      800k                1619k
;;;

#+nil
(defun gen-gamma-variate-squeeze (order state)
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above),
            ORDER >= 1.
  STATE   = random state to use.

  This uses Marsaglia's squeeze method.
"
  (declare (type (double-float 1d0) order)
	   (random-state state))
  ;; Marsaglia's squeeze method for gamma variates.  This method is
  ;; valid for all order >= 1/3.  However, its efficiency gets better
  ;; with larger orders.  Thus, we want order to be at least 1.

  (let* ((s (/ #.(float 1/3 1d0) (sqrt order)))
	 (z0 (- 1d0 (* s #.(sqrt 3d0))))
	 (cs (- 1d0 (* s s)))
	 (x0 (- s #.(sqrt 3d0)))
	 (cc (- (* order z0 z0 z0)
		(* 0.5d0 x0 x0)))
	 (cl (- 1d0 (* 3d0 order))))
    (do* ((x (gen-std-gaussian-variate state)
	     (gen-std-gaussian-variate state))
	  (z (+ (* s x) cs)
	     (+ (* s x) cs)))
	 (nil)
      (declare (double-float x z))
      (when (plusp z)
	(let* ((z z)
	       (rgama (* order z z z))
	       (e (gen-std-exponential-variate 1d0 state))
	       (cd (- (+ e (* 0.5d0 x x) cc)
		      rgama))
	       (tt (- 1d0 (/ z0 z))))
	  (declare (double-float e)
		   (type (non-negative-float double-float) z))
	  (when (or (plusp (- cd
			      (* cl tt
				 (+ 1d0 (* tt
					   (+ 0.5d0
					      (* #.(float 1/3 1d0) tt)))))))
		    (>= (- cd (* cl (log (/ z z0))))
			0d0))
	    (return-from gen-gamma-variate-squeeze rgama)))))))

(defun gen-gamma-variate-squeeze (order state)
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above),
            ORDER >= 1.
  STATE   = random state to use.

  This uses Marsaglia's squeeze method.
"
  (declare (type (double-float 1d0) order)
	   (random-state state))
  ;; Marsaglia's squeeze method for gamma variates.  This method is
  ;; valid for all order >= 1/3.  However, its efficiency gets better
  ;; with larger orders.  Thus, we want order to be at least 1.

  (let* ((s (/ #.(float 1/3 1d0) (sqrt order)))
	 (z0 (- 1d0 (* s #.(sqrt 3d0))))
	 (cs (- 1d0 (* s s)))
	 (x0 (- s #.(sqrt 3d0)))
	 (cc (- (* order z0 z0 z0)
		(* 0.5d0 x0 x0)))
	 (cl (- 1d0 (* 3d0 order)))
	 (x 0d0)
	 (z 0d0))
    (declare (type double-float x z))
    (loop
	(tagbody
	  step-1
	   (setf x (gen-std-gaussian-variate state))
	   (setf z (+ (* x s) cs))
	   (when (<= z 0)
	     (go step-1))
	   (let* ((z z)
		  (rgama (* order z z z))
		  (e (gen-std-exponential-variate 1d0 state))
		  (cd (- (+ e (* 0.5d0 x x) cc)
			 rgama))
		  (tt (- 1d0 (/ z0 z))))
	     (declare (double-float e)
		      (type (non-negative-float double-float) z))
	     (when (or (plusp (- cd
				 (* cl tt
				    (+ 1d0 (* tt
					      (+ 0.5d0
						 (* #.(float 1/3 1d0) tt)))))))
		       (>= (- cd (* cl (log (/ z z0))))
			   0d0))
	       (return-from gen-gamma-variate-squeeze rgama)))))))


#+nil
(defun gen-gamma-variate-gn (order state)
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER >= 1.
  STATE   = random state to use.

  This uses Ahrens and Dieter's Algorithm GN
"
  (declare (type (double-float 1d0) order)
	   (random-state state)
	   (optimize (speed 3)))
  ;; Ahrens and Dieter Algorithm GN for gamma variates
  (let* ((mu (- order 1d0))
	 (sigma (sqrt (+ order (* (sqrt order) #.(sqrt (float 8/3 1d0))))))
	 (d (* sigma #.(sqrt 6d0)))
	 (b (+ mu d)))
    ;;(declare (double-float mu sigma d b))
    (do ((u (random 1d0 state)
	    (random 1d0 state)))
	(nil)
      (declare (type (non-negative-float double-float (1d0)) u))
      (cond ((> u 0.009572265238289d0)
	     (let* ((s (gen-std-gaussian-variate state))
		    (x (+ mu (* sigma s))))
	       (when (and (<= 0 x b)
			  (<= (log (random 1d0 state))
			      (- (+ (* mu
				       (+ 1d0
					  (log (/ (the (non-negative-float double-float) x)
						  mu))))
				    (* 0.5d0 s s))
				 x)))
		 (return-from gen-gamma-variate-gn x))))
	    (t
	     (let* ((s (gen-std-exponential-variate 1d0 state))
		    (x (* b (+ 1d0 (/ s d)))))
	       (when (<= (log (random 1d0 state))
			 (- (+ (* mu
				  (- (+ 2d0
					(log (/ x mu)))
				     (/ x b)))
			       3.7203284924588704d0
			       (log (the (non-negative-float double-float) (/ (* sigma d) b))))
			    b))
		 (return-from gen-gamma-variate-gn x))))))))

;; This unstructured version (which exactly follows the algorithm
;; description) is significantly faster than the structured version
;; above.  For example, for order = 100d0 and 50000 trials, the above
;; takes 4.4 sec, but this version takes 0.3 sec.  This version also
;; cons about 10 times less.  I don't know why that is.
(defun gen-gamma-variate-gn (order state)
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER >= 1.
  STATE   = random state to use.

  This uses Ahrens and Dieter's Algorithm GN
"
  (declare (type (double-float 1d0) order)
	   (random-state state)
	   (optimize (speed 3)))
  ;; Ahrens and Dieter Algorithm GN for gamma variates
  (let* ((mu (- order 1d0))
	 (sigma (sqrt (+ order (* (sqrt order) #.(sqrt (float 8/3 1d0))))))
	 (d (* sigma #.(sqrt 6d0)))
	 (b (+ mu d))
	 (x 0d0)
	 (s 0d0)
	 (u 0d0))
    (declare (type (non-negative-float double-float (1d0)) u)
	     (type double-float x s))
    (loop
	(tagbody
	  step-2
	   (setf u (random 1d0 state))
	   (when (<= u 0.009572265238289d0)
	     (go step-5))
	   (setf s (gen-std-gaussian-variate state))
	   (setf x (+ mu (* sigma s)))
	   (when (or (< x 0) (> x b))
	     (go step-2))

	   (setf u (random 1d0 state))
	   (if (> (log u)
		  (- (+ (* mu
			   (+ 1d0
			      (log (/ (the (non-negative-float double-float) x)
				      mu))))
			(* 0.5d0 s s))
		     x))
	       (go step-2)
	       (return-from gen-gamma-variate-gn x))
	  step-5
	   (setf s (gen-std-exponential-variate 1d0 state))
	   (setf x (* b (+ 1 (/ s d))))
	   (setf u (random 1d0 state))
	   (if (> (log u)
		  (- (+ (* mu
			   (- (+ 2d0
				 (log (/ (the (non-negative-float double-float) x) mu)))
			      (/ x b)))
			3.7203284924588704d0
			(log (the (non-negative-float double-float) (/ (* sigma d) b))))
		     b))
	       (go step-2)
	       (return-from gen-gamma-variate-gn x))))))

(defun gen-gamma-variate-algo-a (order state)
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER >= 1
  STATE   = random state to use.

  This uses Algorithm A, in Knuth, Seminumerical Algorithms.
"
  (declare (type (double-float 1d0) order)
	   (random-state state)
	   (optimize (speed 3)))
  ;; The large order case. This is Algorithm A, sec 3.4.1 E.
  (let* ((a order)
	 (sqrt2a-1 (sqrt (- (* 2d0 a) 1d0)))
	 (a-1 (- a 1d0)))
    (declare (type (double-float (1.0d0)) a))
    (do* ((y (tan (* #.(float pi 1d0) (random 1d0 state)))
	     (tan (* #.(float pi 1d0) (random 1d0 state))))
	  (x (+ (* sqrt2a-1 y) a-1)
	     (+ (* sqrt2a-1 y) a-1)))
	 ((and (> x 0d0)
	       (<= (random 1d0 state)
		   (* (+ 1d0 (* y y))
		      (exp (- (* a-1 (log (/ (the (double-float (0d0)) x)
					     a-1)))
			      (* sqrt2a-1 y))))))
	  x))))

;; Knuth mentions that instead of computing tan(pi*u), we can use a
;; polar method.  This implements that idea.
;;
;; Some simple timing tests show that this isn't any faster than the
;; above on an Ultra-30 300 MHz.  I guess the tan function is very
;; fast (or the CMUCL's RNG is slow).
(defun gen-gamma-variate-algo-a-2 (order state)
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER >= 1
  STATE   = random state to use.

  This uses Algorithm A, in Knuth, Seminumerical Algorithms.
"
  (declare (type (double-float 1d0) order)
	   (random-state state)
	   (optimize (speed 3)))
  ;; The large order case. This is Algorithm A, sec 3.4.1 E.
  (let* ((a order)
	 (sqrt2a-1 (sqrt (- (* 2d0 a) 1d0)))
	 (a-1 (- a 1d0)))
    (declare (type (double-float (1.0d0)) a))
    (flet ((tan-pi-u (state)
	     (do* ((u (- (random 2.0d0 state) 1.0d0)
		      (- (random 2.0d0 state) 1.0d0))
		   (v (- (random 2.0d0 state) 1.0d0)
		      (- (random 2.0d0 state) 1.0d0))
		   (s (+ (* u u) (* v v))
		      (+ (* u u) (* v v)))
		   )
		((< s 1.0d0)
		 (/ v u)))))
      (do* ((y (tan-pi-u state) (tan-pi-u state))
	    (x (+ (* sqrt2a-1 y) a-1)
	       (+ (* sqrt2a-1 y) a-1)))
	   ((and (> x 0d0)
		 (<= (random 1d0 state)
		     (* (+ 1d0 (* y y))
			(exp (- (* a-1 (log (/ (the (double-float (0d0)) x)
					       a-1)))
				(* sqrt2a-1 y))))))
	    x)))))

(defun gen-gamma-variate-small-order (order state)
  "Generate a pseudo-random number drawn from a Gamma distribution of
order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            0 < ORDER < 1.
  STATE   = random state to use.

 This uses the method given in problem 16, in Knuth, Seminumerical
 Algorithms.
"
  (declare (type (double-float (0d0) (1d0)) order)
	   (random-state state))
  ;; order < 1.  This is the algorithm in problem 16 in Sec. 3.4.1, in
  ;; Knuth.
  (let ((p (/ #.(exp 1d0) (+ order #.(exp 1d0))))
	(recip-order (/ order))
	(a-1 (- order 1d0)))
    (do* ((u (random 1d0 state)
	     (random 1d0 state))
	  (v (random 1d0 state)
	     (random 1d0 state))
	  (x (if (< u p) (expt v recip-order) (- 1d0 (log v)))
	     (if (< u p) (expt v recip-order) (- 1d0 (log v))))
	  (q (if (< u p) (exp (- x)) (expt x a-1))
	     (if (< u p) (exp (- x)) (expt x a-1))))
	 ((< (random 1d0 state) q)
	  x)
      (declare (type (non-negative-float double-float (1d0))
		     u v)
	       (type (double-float (0d0)) x)))))

(defun gen-gamma-variate-direct (order state)
  "Generate a pseudo-random number drawn from a Gamma distribution of order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER > 0
  STATE   = random state to use.

 This uses a direct method to generate the random variates.  It uses
 the fact that the sum of two gamma's of order A and B is also a gamma
 of order A + Note that gamma of order 1 is exponential and we know
 how to generate that easily.  This is only good for moderate orders.
 For non-integral orders, the small-order algorithm is called.
"
  (declare (type (double-float (0d0) (20d0)) order)
	   (random-state state))
  ;; Direct generation of Gamma variates using the fact that the sum
  ;; of two gamma's of order A and B is also a gamma of order A +
  ;; B. Note that gamma of order 1 is exponential and we know how to
  ;; generate that easily.  This is only good for moderate orders.
  ;; Use one of the obove generators for higher orders.
  (multiple-value-bind (n r)
      (floor order)
    (declare (fixnum n)
	     #+nil(type (double-float 0d0) r))
    (let ((x 1d0))
      (declare (type (double-float (0d0)) x))
      ;; Sum up the exponential variates here.  This is done my
      ;; multiplying the uniform variates and taking the log at
      ;; the end, instead of summing the log of uniform variates.
      (dotimes (k n)
	(declare (fixnum k))
	(setf x (* x (random 1d0 state))))
      ;; If we still have some left, add in a gamma variate of
      ;; the remaining order.
      (if (zerop r)
	  (- (log x))
	  (- (gen-gamma-variate-small-order r state)
	     (log x))))))

(eval-when (compile eval)
(defconstant +beta-algo-go+ 0.009572265238289d0)
(declaim (type (double-float 0.009572265238289d0 0.009572265238289d0)
	       +beta-algo-go+))
)

;; Ahrens and Dieter's Algorithm GO.
#+nil
(defun gen-gamma-variate-algo-go (a state)
  (declare (type (double-float (2.5327805161251d0)) a)
	   (type random-state state)
	   (optimize (speed 3)))
  (let* ((mu (- a 1))
	 (v (sqrt a))
	 (sigma-2 (+ a #.(sqrt (/ 8d0 3d0))))
	 (sigma (sqrt sigma-2))
	 (w (/ sigma-2 mu))
	 (d (* sigma #.(sqrt 6d0)))
	 (b (+ mu d)))
    ;;(declare (type (double-float 0d0) w))

    (do ((u (random 1d0 state)
	    (random 1d0 state)))
	(nil)
      (cond ((<= u +beta-algo-go+)
	     ;; Step 8 and 9
	     (let ((x (* b (+ 1 (/ (gen-std-exponential-variate 1d0 state) d))))
		   (u (random 1d0 state)))
	       (when (<= (log u) (- (+ (* mu (+ 2 (log (/ x mu)) (- (/ x b))))
				       #.(- (log (/ (* (sqrt (* 2 pi)) +beta-algo-go+)
						  (- 1 +beta-algo-go+)))))
				    (+ b (log (/ (* sigma d) b)))))
		 (return-from gen-gamma-variate-algo-go x))))

	    (t
	     ;; Step 3
	     (let* ((s (gen-std-gaussian-variate state))
		    (x (+ mu (* sigma s))))
	       (if (<= 0 x b)
		   (let ((x x)
			 (u (random 1d0 state))
			 (big-s (* 0.5d0 s s)))
		     (declare (type (non-negative-float double-float) x))
		     (if (< s 0)
			 (when (<= u (- 1 (* big-s (- (- 1 (* 2 (/ s v))) 1))))
			   (return-from gen-gamma-variate-algo-go x))
			 (when (<= u (- 1 (* big-s (- w 1))))
			   (return-from gen-gamma-variate-algo-go x)))
		     (when (<= (log u)
			       (+ (* mu (+ 1 (log (/ x mu))))
				  (- x)
				  big-s))
		       (return-from gen-gamma-variate-algo-go x))))))))))

;; This unstructured version (which exactly follows the algorithm
;; description) is significantly faster than the structured version
;; above.  For example, for order = 100d0 and 50000 trials, the above
;; takes 4.4 sec, but this version takes 0.3 sec.  This version also
;; cons about 10 times less.  I don't know why that is.
(defun gen-gamma-variate-algo-go (a state)
  (declare (type (double-float (2.5327805161251d0)) a)
	   (type random-state state)
	   (optimize (speed 3)))
  (let* ((mu (- a 1))
	 (v (sqrt a))
	 (sigma-2 (+ a #.(sqrt (/ 8d0 3d0))))
	 (sigma (sqrt sigma-2))
	 (w (/ sigma-2 mu))
	 (d (* sigma #.(sqrt 6d0)))
	 (b (+ mu d))
	 (u 0d0)
	 (s 0d0)
	 (big-s 0d0)
	 (x 0d0))
    (declare (type (non-negative-float double-float (1d0)) u)
	     (type double-float s)
	     (type (non-negative-float double-float) big-s x))
    (loop
	(tagbody
	  step-2
	   (setf u (random 1d0 state))
	   (when (<= u +beta-algo-go+)
	     (go step-8))
	   (setf s (gen-std-gaussian-variate state))
	   (setf x (+ mu (* sigma s)))
	   (when (or (< x 0) (> x b))
	     (go step-2))
	   (setf u (random 1d0 state))
	   (setf big-s (* 0.5d0 s s))
	   (when (>= s 0)
	     (go step-6))
	   (if (<= u (- 1 (* big-s (- (* w (- 1 (/ (+ s s) v))) 1))))
	       (return-from gen-gamma-variate-algo-go x)
	       (go step-7))
	  step-6
	   (when (<= u (- 1 (* s (- w 1))))
	     (return-from gen-gamma-variate-algo-go x))
	  step-7
	   (if (> (log u) (+ (* mu (+ 1 (log (/ x mu))))
			     (- x)
			     big-s))
	       (go step-2)
	       (return-from gen-gamma-variate-algo-go x))
	  step-8
	   (setf s (gen-std-exponential-variate 1d0 state))
	   (setf x (* b (+ 1 (/ s d))))
	   (setf u (random 1d0 state))
	   (if (> (log u) (- (+ (* mu (+ 2 (log (/ x mu)) (- (/ x b))))
				#.(- (log (/ (* (sqrt (* 2 (float pi 1d0)))
                                                +beta-algo-go+)
					     (- 1 +beta-algo-go+)))))
			     (+ b (log (/ (* sigma d) b)))))
	       (go step-2)
	       (return-from gen-gamma-variate-algo-go x))))))

(defun gen-gamma-variate-ratio (a state)
  (declare (type (double-float 1.5d0) a)
	   (type random-state state)
	   (optimize (speed 3)))
  (flet ((g (s)
	   (declare (type (double-float (0d0)) s))
	   (exp (- (* (- a 1) (log s))
		   s)))
	 (vr-limits (s0)
	   (let* ((a+1 (+ a 1))
		  (z (+ a+1 s0))
		  (z/2 (/ z 2))
		  (y (/ (sqrt (+ (expt (- a+1 s0) 2)
				 (* 8 s0)))
			2)))
	     (values (+ z/2 y) (- z/2 y)))))
    (let* ((alpha1 (* (- a 1) (+ (* a a a) (* 3 a a) (* 5 a) -11)))
	   (alpha2 (+ (* a a a) (* 3 a a) (* 12 a) -17))
	   (alpha (expt (+ (/ (sqrt alpha1) #.(* 3 (sqrt 3d0)))
			   (/ alpha2 27))
			#.(float 1/3 1d0)))
	   (s0 (+ (/ (+ a 1) 3)
		  alpha
		  (/ (+ (* a a) a a -2)
		     9 alpha)))
	   (ur-hi (sqrt (* (g s0) (+ 1 (* s0 s0))))))
      (multiple-value-bind (s1 s2)
	  (vr-limits s0)
	(format t "s1, s2 = ~A ~A~%" s1 s2)
	(let ((vr-lo (* (sqrt (g s1))
			(+ 1 (* s1 s0))))
	      (vr-hi (* (sqrt (g s2))
			(+ 1 (* s2 s0)))))
	  (do* ((ur (random ur-hi state)
		    (random ur-hi state))
		(vr (- (random (- vr-hi vr-lo) state) vr-lo)
		    (- (random (- vr-hi vr-lo) state) vr-lo))
		(u (/ (- ur (* vr s0)) (+ 1 (* s0 s0)))
		   (/ (- ur (* vr s0)) (+ 1 (* s0 s0))))
		(v/u (/ (+ (* ur s0) vr)
			(- ur (* vr s0)))
		     (/ (+ (* ur s0) vr)
			(- ur (* vr s0)))))
	       ((<= (* u u) (g v/u))
		v/u)))))))





(defun gen-gamma-variate (order state)
  "Generate a pseudo-random number drawn from a Gamma distribution of order ORDER.

               A - 1   - X
              X      %E
       F(X) = ------------
                GAMMA(A)

  ORDER   = order of the Gamma distribution (the variable A above).
            ORDER > 0
  STATE   = random state to use.

 This is the main routine for generating Gamma variates.
"
  (declare (type (double-float (0d0)) order)
	   (random-state state))
  ;; We divide the set of possible orders into these ranges:
  ;; order > s, 1 < order <= s, order = 1, 0 < order < 1.
  ;; Select the appropriate value of s to minimize runtime.
  (cond ((> order 1d0)
	 ;; Pick the fastest of the three algorithms above.
	 (gen-gamma-variate-squeeze order state))
	;; If the threshold s is 1, comment out this code.
	#+nil
	((> order 1d0)
	 (gen-gamma-variate-direct order state))
	((= order 1d0)
	 ;; Gamma variate of order 1 is an exponential variate
	 (gen-std-exponential-variate 1d0 state))
	(t
	 ;; order < 1.  This is the algorithm in problem 16 in Sec. 3.4.1
	 (gen-gamma-variate-small-order order state))))

;;; Geometric random variable
(defun gen-geometric-variate (p state)
  (declare (type (non-negative-float double-float (1d0)) p) (optimize (speed 3)))
  (let ((u (random 1d0 state)))
    (values (ceiling (/ (log u) (log (- 1 p)))))))

;;; Beta random variable

(defun gen-beta-variate (a b state)
  "Generate a pseudo-random number from a beta distribution function
with parameters a and b:

                   B - 1  A - 1
            (1 - X)      X
     F(X) = -------------------
                BETA(A, B)


 where BETA(A, B) is


          GAMMA(A) GAMMA(B)
          -----------------
            GAMMA(B + A)

 The method uses the fact that

           X1
         -------
         X2 + X1
 is a beta variate if X1 is Gamma of order A, and X2 is Gamma of order
 B.

 A      = first parameter of beta density, A > 0
 B      = second parameter, B > 0
 STATE  = random state to use.
"
  (declare (type (double-float (0d0)) a b)
	   (random-state state))
  (let ((x1 (gen-gamma-variate a state))
	(x2 (gen-gamma-variate b state)))
    (/ x1 (+ x1 x2))))

;;; Binomial random variate

(eval-when (compile eval)
(declaim (ftype (function ((and (integer 0) fixnum)
			   (non-negative-float double-float 1d0)
			   random-state)
			  (and (integer 0) fixnum))
		gen-binomial-variate))
)

(defun gen-binomial-variate (ntrials p state)
  "Generate a pseudo-random number from a beta distribution function
with parameters N and p:

                                   N - K  K
      P(K) = BINOMIAL(N, K) (1 - P)      P

 where BINOMIAL(N, K) is

              N!
          -----------
          M! (N - M)!

 NTRIALS      = N, above, the number of trials
 P            = probability of success
 STATE        = random state to use

 The output is an integer.
"
  (declare (type (and (integer 0) fixnum) ntrials)
	   (type (non-negative-float double-float 1d0) p)
	   (random-state state))
  ;; Select some suitable threshold between the direct generation and
  ;; the iterative technique. For a 486-66, the break-even point is
  ;; near 100.  Same is true for a Sparc-20
  (cond ((< ntrials 100)
	 ;; Direct generation
	 (let ((n 0))
	   (declare (fixnum n))
	   (dotimes (k ntrials n)
	     (declare (fixnum k))
	     (if (< (random 1d0 state) p)
		 (incf n)))))
	(t
	 (let* ((a (1+ (floor ntrials 2)))
		(b (1+ (- ntrials a)))
		(x (gen-beta-variate (float a 1d0)
				     (float b 1d0) state)))
	   (declare (fixnum a b)
		    (double-float x))
	   (if (>= x p)
	       (gen-binomial-variate (1- a) (/ p x)
				     state)
	       (+ a (gen-binomial-variate (1- b)
					  (/ (- p x) (- 1d0 x))
					  state)))))))

;;; Poisson random variate

(eval-when (compile)
  (declaim (ftype (function ((double-float 0d0) random-state)
			    (and (integer 0) fixnum))
		  gen-poisson-variate)))

(defun gen-poisson-variate (mean state)
  "Generate a pseudo-random number from a Poisson distribution
with mean M:

               K   - M
              M  %E
       P(K) = --------
                 K!

 MEAN       = Mean (M) of the distribution, M >= 0
 STATE      = random state to use.

 The output is an integer.
"
  (declare (type (double-float 0d0) mean)
	   (random-state state))
  (let ((threshold 30d0))
    (cond ((< mean threshold)
	   ;; Direct generation
	   (let ((limit (exp (- mean))))
	     (do ((prod (random 1.0d0 state))
		  (n 1 (+ n 1)))
		 ((<= prod limit)
		  (- n 1))
	       (declare (fixnum n)
			(type (double-float 0d0) prod))
	       (setf prod (* prod (random 1.0d0 state))))))
	  (t
	   ;; Indirect generation
	   (let* ((alpha #.(coerce 7/8 'double-float)) ; Suggested value
		  (order (floor (* alpha mean)))
		  (x (gen-gamma-variate (float order 1d0) state)))
	     (declare (fixnum order))
	     (if (< x mean)
		 (+ order (gen-poisson-variate (- mean x) state))
		 (gen-binomial-variate (1- order)
				       (/ mean x)
				       state)))))))


#+want-to-time-everything-that-moves (progn

(defun time-exponential (&optional (n 100000))
  (system:without-gcing
   (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-std-exponential-variate-log-method 1d0 *random-state*)))
   (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-std-exponential-variate-algo-s 1d0 *random-state*)))
   (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-std-exponential-variate-ratio 1d0 *random-state*)))))

(defun time-gaussian (&optional (n 50000))
  (declare (fixnum n))
  (format t "gen-std-gaussian-variate-box~%")
  (gc)
  (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-std-gaussian-variate-box *random-state*)))

  (format t "gen-std-gaussian-variate-trig~%")
  (gc)
  (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-std-gaussian-variate-box-trig *random-state*)))

  (format t "gen-std-gaussian-variate-ratio~%")
  (gc)
  (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-std-gaussian-variate-ratio *random-state*)))
  )

(defun time-gamma (&optional (order 10d0) (n 10000))
  (declare (fixnum n))
  (format t "gen-gamma-variate-squeeze~%")
  (gc)
  (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-gamma-variate-squeeze order *random-state*)))
  (format t "gen-gamma-variate-gn~%")
  (gc)
  (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-gamma-variate-gn order *random-state*)))
  (format t "gen-gamma-variate-algo-go~%")
  (gc)
  (time (dotimes (k n)
	  (declare (fixnum k))
	  (gen-gamma-variate-algo-go order *random-state*)))
  (format t "gen-gamma-variate-algo-a~%")
  (gc)
  (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-gamma-variate-algo-a order *random-state*)))
  (format t "gen-gamma-variate-algo-a-2~%")
  (gc)
  (time (dotimes (k n)
	   (declare (fixnum k))
	   (gen-gamma-variate-algo-a-2 order *random-state*)))
  (format t "gen-gamma-variate-direct~%")
  (gc)
  (time (dotimes (k n)
	  (declare (fixnum k))
	  (gen-gamma-variate-direct order *random-state*)))
  )

(defun test-gamma (&optional (order 10d0) (n 10000))
  (dolist (f '(gen-gamma-variate-squeeze
	       gen-gamma-variate-algo-a
	       gen-gamma-variate-gn
	       gen-gamma-variate-algo-go
	       ))
    (let ((sum 0d0)
	  (ssqr 0d0))
      (dotimes (k n)
	(declare (fixnum k))
	(let ((x (funcall f order *random-state*)))
	  (incf sum x)
	  (incf ssqr (* x x))))
      (format t "~s:  ~s  ~s~%"
	      f (/ sum n) (- (/ ssqr n) (expt (/ sum n) 2))))))

(defun test-binomial (&optional (p 0.5d0) (ntrials 100) (n 10000))
  (let ((sum 0d0)
	(ssqr 0d0))
    (dotimes (k n)
      (declare (fixnum k))
      (let ((x (gen-binomial-variate ntrials p *random-state*)))
	(incf sum x)
	(incf ssqr (* x x))))
    (format t "Mean and variance:  ~s  ~s (should be ~s and ~s)~%"
	    (/ sum n)
	    (- (/ ssqr n) (expt (/ sum n) 2))
	    (* ntrials p)
	    (* ntrials p (- 1d0 p)))))

(defun test-poisson (&optional (mean 10d0) (n 10000))
  (let ((sum 0d0)
	(ssqr 0d0))
    (dotimes (k n)
      (declare (fixnum k))
      (let ((x (gen-poisson-variate mean *random-state*)))
	(incf sum x)
	(incf ssqr (* x x))))
    (format t "Mean and variance:  ~s  ~s (should be ~s and ~s)~%"
	    (/ sum n)
	    (- (/ ssqr n) (expt (/ sum n) 2))
	    mean
	    mean)))
)
