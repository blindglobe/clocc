;;; Math utilities (Arithmetical / Statistical functions)
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: math.lisp,v 2.19 2001/06/24 23:48:25 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/math.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `index-t'
  (require :withtype (translate-logical-pathname "cllib:withtype"))
  ;; `mesg', `get-float-time', `elapsed', `with-timing'
  (require :log (translate-logical-pathname "cllib:log"))
  ;; `read-from-file', `write-to-file'
  (require :fileio (translate-logical-pathname "cllib:fileio"))
  ;; `with-collect'
  (require :simple (translate-logical-pathname "cllib:simple"))
  ;; `call-on-split'
  (require :list (translate-logical-pathname "cllib:list")))

(in-package :cllib)

(export
 '(mulf divf sqr ! !! stirling fibonacci primes-to divisors primep
   product-from-to binomial *primes* *primes-file*
   make-primes-list number-sum-split all-num-split
   vector-shuffle permutation with-permutations-shuffle
   with-permutations-swap with-permutations-lex permutations-list
   eval-cont-fract fract-approx
   *num-tolerance* *relative-tolerance* *absolute-tolerance*
   dot poly1 poly erf norm normalize rel-dist
   mean mean-cx mean-weighted mean-geometric mean-geometric-weighted mean-some
   standard-deviation standard-deviation-cx standard-deviation-weighted
   standard-deviation-relative standard-deviation-mdl mdl
   kurtosis-skewness kurtosis-skewness-weighted
   covariation covariation1 cov volatility
   below-p linear safe-fun safe-fun1 safe-/ s/ d/
   convex-hull1 convex-hull sharpe-ratio to-percent percent-change
   rel-diff approx=-abs approx=-rel approx=
   newton integrate-simpson add-probabilities
   line line-val line-rsl line-below-p line-above-p intersect
   with-line line-adjust line-adjust-dir line-adjust-list
   line-thru-points regress lincom))

;;;
;;;
;;;

(define-modify-macro mulf (mult) * "Multiply the arg by a number.")
(define-modify-macro divf (mult) / "Divide the arg by a number.")
(defmacro sqr (xx)
  "Compute the square of a number, taking care to eval only once."
  (if (atom xx) `(* ,xx ,xx)
      (with-gensyms ("SQR-" var) `(let ((,var ,xx)) (* ,var ,var)))))

;;;
;;; Integers
;;;

(declaim (ftype (function (integer integer) (values integer))
                product-from-to binomial))
(defun product-from-to (aa bb)
  "Compute the product of integers from AA (EXclusive) to BB (INclusive)."
  (declare (integer aa bb))
  (when (> aa bb)
    (error "~s (~:d ~:d): the first argument must be smaller"
           'product-from-to aa bb))
  (when (minusp (* bb aa))
    (return-from product-from-to 0))
  ;; this algorithm insures that we multiply bignums
  ;; of approximately the same size
  ;; we use `labels' since some compilers optimize `labels' better than
  ;; plain recursion and because this avoids doing the above checks in cycle
  (labels ((pft (aa bb)
             (case (- bb aa)
               (0 1) (1 bb) (2 (* (- bb 1) bb)) (3 (* (- bb 2) (- bb 1) bb))
               (4 (* (- bb 3) (- bb 2) (- bb 1) bb))
               (t (let ((mm (ash (+ aa bb) -1)))
                    (* (pft aa mm) (pft mm bb)))))))
    (pft aa bb)))

(defun binomial (nn kk)
  "Compute the binomial coefficient for two integers."
  (declare (integer nn kk))
  ;; we do not use the double recursion a la `product-from-to'
  ;; because it would take us outside the realm of the integers
  (loop :with res = 1
        :for ii :from 1 :to (max kk (- nn kk))
        :for jj :from nn :by -1
        :do (mulf res (/ jj ii))
        :finally (return res)))

(declaim (ftype (function (integer) (values integer)) ! !!))
(defun ! (nn)
  "Compute the factorial: n! = n * (n-1) * (n-2) * ..."
  (declare (integer nn))
  #+clisp (#+lisp=cl ext:! #-lisp=cl lisp:! nn) ; CLISP has built-in factorial
  #-clisp (product-from-to 1 nn))

(defun !! (nn)
  "Compute the double factorial: n!! = n * (n-2) * (n-4) * ..."
  (declare (integer nn))
  (multiple-value-bind (kk rr) (floor nn 2)
    (declare (fixnum kk) (type (integer 0 1) rr))
    (if (zerop rr) (ash (! kk) kk)
        (labels ((ff (aa bb)
                   (declare (fixnum aa bb))
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

(declaim (ftype (function ((integer 0)) (values (integer 0) (integer 0)))
                fibonacci))
(defun fibonacci (nn)
  "Return 2 consecutive Fibonacci numbers."
  (declare (type (integer 0) nn))
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
  (with-timing (:done t)
    (format t "Computing primes up to ~:d..." limit) (force-output)
    (primes-to limit 10d0))
  (write-to-file *primes* *primes-file* nil
                 (format nil "~%;; Upper limit: ~:d~%;; ~:d primes~%"
                         limit (length *primes*))))

(defun number-sum-split (num fun fun-1 &optional (out *standard-output*))
  "Print all the splittings of NUM into (+ (FUN M) (FUN K))
FUN-1 is the integer inverse function.
E.g.: (number-sum-split 10 (lambda (x) (* x x)) 'isqrt) => ((1 . 3))"
  (declare (type (integer 0) num) (type (or null stream) out)
           (type (function ((integer 0)) (integer 0)) fun fun-1))
  (let ((lim (1+ (funcall fun-1 (floor num 2)))) res)
    (dotimes (ii lim (nreverse res))
      (let* ((fi (funcall fun ii)) (jj (funcall fun-1 (- num fi)))
             (fj (funcall fun jj)))
        (when (= num (+ fi fj))
          (push (cons ii jj) res)
          (when out
            (format out "~:d = ~:d + ~:d   [~:d/~:d]~%" num fi fj ii jj)))))))

(defun all-num-split (min find fun &optional int)
  "Find first FIND integers with MIN representations as a sum of 2 FUN's."
  (declare (fixnum min find) (type (function ((integer 0)) (integer 0)) fun)
           (type (or null double-float) int))
  (do* ((ht (make-hash-table :test #'eql :size 100)) (cur 0 (1+ cur)) (found 0)
        (fc (funcall fun cur) (funcall fun cur)) res (bt (get-float-time)))
       ((= found find) (nreverse res))
    (when (and int (= 1 (mod cur 1000)) (> (elapsed bt t) int))
      (format t "~:d..." cur) (force-output)
      (setq bt (get-float-time)))
    (dotimes (ii cur)
      (let* ((sum (+ fc (funcall fun ii))) (ha (gethash sum ht))
             (new (cons (cons cur ii) ha)))
        (when (>= (length new) min)
          (incf found)
          (format t "~& * ~:d --~d--> ~s~%" sum (length new) new))
        (setf (gethash sum ht) new)))))

;;;
;;; sequence permutations
;;;

(defun make-vector-indexed (len)
  "Return a simple vector #(0 1 ... (1-len))."
  (let ((vv (make-array len)))
    (dotimes (ii len vv)
      (setf (aref vv ii) ii))))

(defun vector-shuffle (vec)
  "Generate a random permutation of the vector in place.
If the argument is a number, return a new random vector of this length.
Uses the Fisher/Yates algorithm, see
 Knuth, TAOCP vol 2 Algorithm 3.4.2P, p.145
 R.A. Fisher & F. Yates, Statistical Tables, London 1938, Example 12
 R. Durstenfeld, CACM 7 (1964), 420.
This is more or less the same as
  (permutation vec (random (! (length vec))))
except that the factorial is likely to be far too large for `random'."
  (etypecase vec
    (vector (loop :for ii :downfrom (1- (length vec)) :to 1
                  :for jj = (random (1+ ii))
                  :unless (= jj ii)
                  :do (rotatef (aref vec ii) (aref vec jj)))
            vec)
    (number (vector-shuffle (make-vector-indexed vec)))))

(defun permutation (vec nth &optional (len (1- (length vec))) (fact (! len)))
  "Generate the NTH permutation of the vector VEC in place.
The algorithm is similar to the standard Fisher/Yates one, but instead
of random numbers [a_n-1,...,a_1] it represents a number in [0;n!] as
   x = a_n-1*(n-1)! + ... + a_1
The original vector is returned when NTH = (1- (! (length vec)))."
  (loop :for ff = fact :then (/ ff ii)
        :for ii :downfrom len :to 1 :with jj
        :do (setf (values jj nth) (floor nth ff))
        :unless (= jj ii)
        :do (rotatef (aref vec ii) (aref vec jj)))
  vec)

(defmacro with-permutations-shuffle ((var vec &optional ret-form) &body body)
  "Gererate the successive shufflings of vector VEC using `permutation'.
VEC is not modified, VAR storage is allocated only once,
not n! times, and reused.
The return value is RET-FORM, if given, or the number of
permutations generated (i.e., n!).
The original vector is the last one returned."
  (with-gensyms ("WPU-" vv len len1 fact ii jj tot)
   `(let* ((,vv ,vec) (,len (length ,vv))  (,len1 (1- ,len)) (,fact (! ,len1))
            (,var (copy-seq ,vv)) (,tot (* ,len ,fact)))
      (dotimes (,ii ,tot ,(or ret-form tot))
        (dotimes (,jj ,len) (setf (aref ,var ,jj) (aref ,vv ,jj)))
        (permutation ,var ,ii ,len1 ,fact)
        ,@body))))

(defun check-permutations-end (name found length)
  (let ((fact (! length)))
    (unless (= found fact)
      (error "~s: generated ~:d permutation~:p, not ~d!=~:d, as expected"
             name found length fact))))

(defmacro with-permutations-swap ((var vec &optional ret-form) &body body)
  "Bind VAR to each permutation of vector VEC in turn, then execute the BODY.
Thus, BODY is evaluated (! (length vec)) times.
VEC is not modified; VAR storage is allocated only once,
not n! times, and reused.
The return value is RET-FORM, if given, or the number of
permutations generated (i.e., n!).
The permutations are generated by transposing adjacent elements,
according to the CACM algorithm 115 [H.F.Trotter, Comm ACM 5 (Aug 1962) 434].
The original vector is the last one returned."
  (with-gensyms ("WPS-" nn pp dd kk qq ii done top)
    `(let* ((,var (copy-seq ,vec)) (,ii 0) (,nn (length ,var))
            (,top (- ,nn 2)) (,kk 0) (,qq 0) (,done nil)
            (,pp (make-array (1- ,nn) :element-type 'fixnum
                             :initial-element -1))
            (,dd (make-array (1- ,nn) :element-type 'fixnum
                             :initial-element 1)))
      (declare (type (array fixnum (*)) ,pp ,dd) (fixnum ,kk ,qq))
      (loop
       (tagbody
          (setq ,kk 0 ,top (- ,nn 2))
        :index
          (setf ,qq (+ (aref ,pp ,top) (aref ,dd ,top))
                (aref ,pp ,top) ,qq)
          (when (= ,qq (1+ ,top))
            (setf (aref ,dd ,top) -1)
            (go :loop))
          (when (/= -1 ,qq) (go :swap))
          (setf (aref ,dd ,top) 1)
          (incf ,kk)
        :loop
          (when (> ,top 0)
            (decf ,top)
            (go :index))
          (setq ,qq 0 ,done t)
        :swap
          (incf ,qq ,kk) (rotatef (aref ,var ,qq) (aref ,var (1+ ,qq))))
       ;;(format t "~4d * ~s [k ~d] [n ~d] [p ~s] [d ~s] [q ~d] [done ~s]~%"
       ;; ,ii ,var ,kk ,top ,pp ,dd ,qq ,done)
       (incf ,ii)
       ,@body
       (when ,done
         (check-permutations-end 'with-permutations-swap ,ii ,nn)
         (return ,(or ret-form ii)))))))

(defmacro with-permutations-lex ((var len &optional ret-form) &body body)
  "Bind VAR to each permutation of vector [0:LEN-1] in turn,
then execute the BODY - i.e, BODY is evaluated (! len) times.
VAR storage is allocated only once, not n! times, and reused.
The return value is RET-FORM, if given, or the number of
permutations generated (i.e., n!).
The permutations are generated in the lexicographic order,
according to the CACM algorithm 202 [M.K.Shen, Comm ACL 6 (Sept 1963) 517]."
  (with-gensyms ("WPL-" ll nn ww ii)
    `(let* ((,ll ,len) (,var (make-vector-indexed ,ll)) (,nn 0))
      (declare (fixnum ,ll ,nn))
      (loop
       (unless (zerop ,nn)
         (let ((,ww (1- ,ll)))
           (declare (fixnum ,ww))
           (do () ((or (zerop ,ww) (< (aref ,var (1- ,ww)) (aref ,var ,ww))))
             (decf ,ww))
           (when (zerop ,ww)
             (check-permutations-end 'with-permutations-lex ,nn ,ll)
             (return ,(or ret-form nn)))
           (let ((,ii (position (aref ,var (1- ,ww)) ,var
                                :from-end t :test #'<)))
             (rotatef (aref ,var (1- ,ww)) (aref ,var ,ii)))
           (dotimes (,ii (ash (- ,ll ,ww) -1))
             (rotatef (aref ,var (- ,ll ,ii 1)) (aref ,var (+ ,ww ,ii))))))
       (incf ,nn)
       ,@body))))

(defun permutations-list (vec &key (method :lex))
  "Return the list of all the permutations of the vector VEC.
The order in which the permutations are listed is either
 lexicographic (when :METHOD is :LEX, which is the default),
  in which case `with-permutations-lex' is used;
 shuffling (when :METHOD is :SHUFFLE)
  in which case `with-permutations-shuffle' is used;
 transposing adjacent elements (when :METHOD is :SWAP),
  in which case `with-permutations-swap' is used.
:SWAP is more than twice as fast as :LEX
 and more that 10 times as fast as :SHUFFLE"
  (declare (ignorable method))
  (with-collect (coll)
    (ecase method
      (:lex (with-permutations-lex (vv (length vec))
              (let ((tv (copy-seq vec)))
                (dotimes (ii (length vec))
                  (setf (aref tv ii) (aref vec (aref vv ii))))
                (coll tv))))
      (:shuffle (with-permutations-shuffle (vv vec) (coll (copy-seq vv))))
      (:swap (with-permutations-swap (vv vec) (coll (copy-seq vv)))))))

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

(defcustom *relative-tolerance* double-float 1d-3
  "*The default relative tolerance for `approx='.")
(defcustom *absolute-tolerance* double-float 1d0
  "*The default absolute tolerance for `approx='.")
(defcustom *num-tolerance* double-float 1d-6
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
  (declare (sequence l0 l1) (type (function (t) double-float) key key0 key1))
  (let ((res 0d0))
    (declare (double-float res))
    (map nil (lambda (r0 r1)
               (incf res (* (funcall key0 r0) (funcall key1 r1))))
         l0 l1)
    res))

(defun poly1 (var &rest coeffs)
  "Compute the polynomial with the given coefficients. Use the Horner scheme.
COEFFS are (a0 a1 a2 ...) for a0*x^n + a1*x^{n-1} + a2*x^{n-2}...
so that (poly1 10 1 2 3 4 5) ==> 12345."
  (declare (double-float var) (list coeffs))
  (let ((res 0d0))
    (declare (double-float res))
    (dolist (cc coeffs res)
      (declare (double-float cc))
      (setq res (+ cc (* var res))))))

(defun poly (var coeffs)
  "Compute the polynomial with the given coefficients. Use the Horner scheme.
COEFFS is a sequence #(a0 a1 a2 ...) for a0*x^n + a1*x^{n-1} + a2*x^{n-2}...
so that (poly 10 '(1 2 3 4 5)) ==> 12345."
  (declare (double-float var) (type sequence coeffs))
  (reduce (lambda (res coeff)
            (declare (double-float res coeff))
            (+ (* res var) coeff))
          coeffs :initial-value 0d0))

(defun erf (xx)
  "Compute the error function, accurate to 1e-6. See Hull p. 243.
The same as
  (+ 0.5d0 (/ (integrate-simpson (lambda (tt) (exp (* tt tt -0.5d0))) 0 xx)
              (sqrt (* 2 pi))))
i.e., the comulative normal distribution function.
Return the value and the derivative, suitable for `newton'."
  (declare (double-float xx))
  (let* ((der (/ (exp (* -0.5d0 (expt xx 2))) (dfloat (sqrt (* 2 pi)))))
         (val (- 1 (* der (poly (/ (1+ (* (abs xx) 0.2316419d0)))
                                #(1.330274429d0 -1.821255978d0 1.781477937d0
                                  -0.356563782d0 0.319381530d0 0d0))))))
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
  (let ((b1 (funcall key (elt seq1 start1))) (dist 0d0)
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
           (fixnum len))
  (values (/ (reduce #'+ seq :key key) len) len))

(defun mean-cx (seq &key (key #'value) (len (length seq)))
  "Compute the mean of the sequence of complex numbers.
Returns 2 values: the mean and the length of the sequence."
  (declare (sequence seq) (type (function (t) (complex double-float)) key)
           (fixnum len))
  (values (/ (reduce #'+ seq :key key) len) len))

(defun mean-weighted (seq wts &key (value #'value) (weight #'value))
  "Compute the weighted mean of the sequence SEQ
with weights WTS (not necessarily normalized)."
  (declare (sequence seq wts) (type (function (t) double-float) value)
           (type (function (t) number) weight))
  (let ((twt (reduce #'+ wts :key weight)))
    (values (/ (dot seq wts :key0 value :key1 weight) twt)
            twt)))

(defun mean-geometric (seq &key (key #'value))
  "Compute the geometric mean of the sequence of numbers.
Returns 2 values: the mean and the length of the sequence."
  (declare (sequence seq) (type (function (t) double-float) key))
  (let ((len (length seq)))
    (values (expt (reduce #'* seq :key key) (/ len)) len)))

(defun mean-geometric-weighted (seq wts &key (value #'value) (weight #'value))
  "Compute the weighted geometric mean of the sequence SEQ
with weights WTS (not necessarily normalized)."
  (declare (sequence seq wts) (type (function (t) double-float) value weight))
  (let ((twt (reduce #'+ wts :key weight)) (res 1d0))
    (declare (double-float res twt))
    (map nil (lambda (rr wt)
               (setq res (* res (expt (funcall value rr)
                                      (/ (funcall weight wt) twt)))))
         seq wts)
    res))

(defun mean-some (seq &key (key #'value))
  "Compute the mean of the sequence of real numbers.
NULLs are ignored, so this is like (mean (remove nil seq :key key) :key key).
Return 2 values: the mean and the length of the sequence."
  (declare (sequence seq) (type (function (t) (or null double-float)) key))
  (let ((len 0))
    (declare (type index-t len))
    (values (s/ (reduce #'+ seq :key (lambda (rr)
                                       (let ((val (funcall key rr)))
                                         (cond (val (incf len) val)
                                               (0d0)))))
                len)
            len)))

(defun standard-deviation (seq &key (len (length seq)) (key #'value)
                           (mean (mean seq :key key :len len)))
  "Compute the standard deviation of the sequence SEQ.
The mean and the length can be pre-computed for speed."
  (declare (sequence seq) (fixnum len) (double-float mean)
           (type (function (t) double-float) key))
  (when (<= len 1) (return-from standard-deviation (values 0d0 mean len)))
  (values
   (sqrt (/ (reduce #'+ seq :key (lambda (yy) (sqr (- (funcall key yy) mean))))
            (1- len)))
   mean len))

(defun standard-deviation-weighted (seq wts &key
                                    (value #'value) (weight #'value))
  "Compute the standard deviation of the sequence with weights."
  (declare (sequence seq wts) (type (function (t) double-float) value)
           (type (function (t) number) weight))
  (multiple-value-bind (mn twt)
      (mean-weighted seq wts :value value :weight weight)
    (let ((sum 0d0))
      (map nil (lambda (xx ww)
                 (incf sum (* (funcall weight ww)
                              (sqr (- (funcall value xx) mn)))))
           seq wts)
      (values (sqrt (/ sum (1- twt))) mn twt))))

(defsubst standard-deviation-cx (&rest args)
  "Return the `standard-deviation' of SEQ as #C(mean stdd)."
  (multiple-value-bind (stdd mean) (apply #'standard-deviation args)
    (complex mean stdd)))

(defun standard-deviation-relative (seq &key (key #'value))
  "Compute the relative standard deviation (StD(log(x[i+1]/x[i]))).
Meaningful only if all the numbers are of the same sign,
if this is not the case, the result will be a complex number."
  (declare (sequence seq) (type (function (t) double-float) key))
  (let (pr (sq 0d0) (su 0d0) (nn 0))
    (declare (double-float sq su) (type (or null double-float) pr)
             (type index-t nn))
    (map nil (lambda (rr)
               (let* ((cc (funcall key rr))
                      (vv (when pr (log (/ cc pr)))))
                 (declare (double-float cc) (type (or null double-float) vv))
                 (setq pr cc)
                 (when vv (incf sq (sqr vv)) (incf su vv) (incf nn))))
         seq)
    (values (sqrt (max 0d0 (/ (- sq (/ (sqr su) nn)) (1- nn)))) nn)))

(defun kurtosis-skewness (seq &key (key #'value) std mean len)
  "Compute the skewness and kurtosis (3rd & 4th centered momenta)."
  (declare (sequence seq) (type (function (t) double-float) key))
  (unless std (setf (values std mean len) (standard-deviation seq :key key)))
  (let ((skew 0d0) (kurt 0d0))
    (map nil (lambda (rr)
               (let* ((cc (/ (- (funcall key rr) mean) std))
                      (c3 (* cc cc cc)))
                 (incf skew c3)
                 (incf kurt (* cc c3))))
         seq)
    (values (/ kurt (1- len)) (/ skew (1- len)) std mean len)))

(defun kurtosis-skewness-weighted (seq wts &key std mean tot
                                   (value #'value) (weight #'value))
  "Compute the skewness and kurtosis (3rd & 4th centered momenta)."
  (declare (sequence seq) (type (function (t) double-float) value weight))
  (unless std
    (setf (values std mean tot)
          (standard-deviation-weighted seq wts :value value :weight weight)))
  (let ((skew 0d0) (kurt 0d0))
    (map nil (lambda (rr ww)
               (let* ((cc (/ (- (funcall value rr) mean) std))
                      (wt (funcall weight ww))
                      (c3 (* wt cc cc cc)))
                 (incf skew c3)
                 (incf kurt (* c3 cc))))
         seq wts)
    (values (/ kurt (1- tot)) (/ skew (1- tot)) std mean tot)))

(defun covariation (seq0 seq1 &key (key0 #'value) (key1 #'value))
  "Compute the covariation between the data in the two sequences.
Return 6 values: covariation, mean0, mean1, dispersion0,
dispersion1, number of elements considered.
Uses the fast but numerically unstable algorithm
without pre-computing the means."
  (declare (sequence seq0 seq1) (type (function (t) double-float) key0 key1))
  (let ((xb 0d0) (yb 0d0) (x2b 0d0) (xyb 0d0) (y2b 0d0)
        (nn 0) (c0 0d0) (c1 0d0))
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
  (declare (sequence seq0 seq1) (type (function (t) double-float) key0 key1))
  (let ((m0 (dfloat (mean seq0 :key key0)))
        (m1 (dfloat (mean seq1 :key key1)))
        (nn 0) (d0 0d0) (d1 0d0) (rr 0d0) (co 0d0))
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

(defun volatility (lst split-key &rest args
                   &key (dev-fn #'standard-deviation-relative)
                   &allow-other-keys)
  "Return volatilities for the terms corresponding to SPLIT-KEY.
The first value returned is the mean of the volatilities,
the second - the volatilities themselves.
E.g., (volatility dated-list (compose date-ye date) :key #'value)
will return the average annual volatility for the value in the dated-list
and the list of the volatilities for each year."
  (declare (type (or function fixnum) split-key) (list lst)
           (type (function (sequence) double-float) dev-fn))
  (remf args :dev-fn)
  (let ((vols (apply #'call-on-split lst dev-fn :split-key split-key
                     :min-len 2 args)))
    (values (mean vols :key #'cdr) vols)))

;;; Mean / Deviation / Length

(eval-when (compile load eval)  ; CMUCL
(defstruct (mdl #+cmu (:print-function print-struct-object))
  (mn 0d0 :type double-float)   ; Mean
  (sd 0d0 :type (double-float 0d0)) ; Deviation
  (le 0 :type index-t))         ; Length
)

(defconst +bad-mdl+ mdl (make-mdl) "The convenient constant for init.")
(defmethod value ((mdl mdl)) (mdl-mn mdl))

(defmethod print-object ((mdl mdl) (out stream))
  (if *print-readably* (call-next-method)
      (format out "[~6f ~6f ~5:d]" (mdl-mn mdl) (mdl-sd mdl) (mdl-le mdl))))

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
  (declare (double-float x0 y0 x1 y1 tt))
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
  (declare (number aa) (list bb))
  (if (some #'zerop bb)
      (cond ((zerop aa) 1) ((plusp aa) most-positive-fixnum)
            (most-negative-fixnum))
      (apply #'/ aa bb)))

(defsubst s/ (aa bb)
  "Fast safe division; only 2 arguments are allowed."
  (declare (number aa bb))
  (if (zerop bb) (cond ((zerop aa) 1) ((plusp aa) most-positive-fixnum)
                       (most-negative-fixnum))
      (/ aa bb)))

(defsubst d/ (aa bb)
  "Double float fast safe division; only 2 arguments are allowed."
  (declare (double-float aa bb))
  (if (zerop bb) (cond ((zerop aa) 1d0)
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
  (do* ((ll lst (cdr ll)) (ts (if up #'> #'<)) (x0 0d0) (y0 0d0)
        (sl (lambda (pp)        ; has to be d/!!!
              (d/ (with-type double-float (- (funcall ykey pp) y0))
                  (abs (with-type double-float (- (funcall xkey pp) x0)))))))
       ((null (cddr ll)) lst)
    (declare (double-float x0 y0) (type function ts)
             (type (function (t) double-float) sl))
    (setf x0 (funcall xkey (car ll)) y0 (funcall ykey (car ll))
          (cdr ll)
          (do ((l1 (cddr ll) (cdr l1)) (top (cdr ll)) (csl 0d0)
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
  `(* 100d0 (1- ,vv)))

(defun percent-change (v0 v1 &optional days)
  "Return the percent change in values, from V0 to V1.
If the optional DAYS is given, return the annualized change too."
  (declare (number v0 v1) (type (or null number) days))
  (if (zerop v0) (values 0d0 0d0)
      (let ((pers (dfloat (/ v1 v0))))
        (if (and days (not (zerop days)))
            (values (to-percent pers)
                    (if (zerop days) 0d0
                        (to-percent (expt pers (/ 365.25d0 days)))))
            (to-percent pers)))))

(defun rel-diff (v0 v1)
  "Return the relative difference between the two numbers.
This function is commutative, and puts the smallest number into the
denominator.  Sign is ignored."
  (declare (double-float v0 v1))
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
           (number val ival) (fixnum max-it))
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
  (do* ((f0 (funcall ff x0)) (f1 (funcall ff (* 0.5d0 (+ x0 xm))))
        (fm (funcall ff xm)) (hh (* 0.5d0 (- xm x0)) (* hh 0.5d0))
        (mm 2 (* mm 2))
        (sum-even 0d0 (+ sum-odd sum-even))
        (sum-odd
         f1 (loop :for ii :of-type index-t :from 1 :below mm :by 2
                  :and step :of-type double-float :from 1d0 :by 2d0
                  :sum (funcall ff (+ x0 (* hh step))) :of-type double-float))
        (int-last 0d0 int)
        (int (* (/ hh 3d0) (+ f0 (* 4d0 f1) fm))
             (* (/ hh 3d0) (+ f0 (* 4d0 sum-odd) (* 2d0 sum-even) fm))))
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

(eval-when (compile load eval)  ; CMUCL
(defstruct (line #+cmu (:print-function print-struct-object))
  "A straight line."
  (sl 0d0 :type double-float) ; slope
  (co 0d0 :type double-float)) ; constant
)

(defconst +bad-line+ line (make-line) "*The convenient constant for init.")

(defmethod print-object ((ln line) (out stream))
  (if *print-readably* (call-next-method)
      (format out "{~6f ~6f}" (line-sl ln) (line-co ln))))

(declaim (ftype (function (line double-float) (values double-float)) line-val))
(defsubst line-val (ln par)
  "Evaluate the line at point."
  (declare (type line ln) (double-float par))
  (with-type double-float (+ (* par (line-sl ln)) (line-co ln))))

(declaim (ftype (function (line) (values double-float)) line-rsl))
(defsubst line-rsl (ln)
  "Return the relative slope of the line."
  (declare (type line ln))
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
  (with-gensyms ("WL-" di)
    `(let ((,di (- (line-val ,ln ,xx) ,yy)))
      (declare (double-float ,di))
      (cond ((> ,di ,tol) ,below) ((< ,di (- ,tol)) ,above) (t ,upon)))))

(declaim (ftype (function (line double-float double-float) (values line))
                line-adjust))
(defsubst line-adjust (ln xx yy)
  "Adjust the line LN to pass through the point, keeping the slope intact."
  (declare (double-float xx yy) (type line ln))
  (setf (line-co ln) (with-type double-float (- yy (* (line-sl ln) xx)))) ln)

(declaim (ftype (function (line double-float double-float t) (values line))
                line-adjust-dir))
(defun line-adjust-dir (ln xx yy up)
  "Adjust the line LN to be above (if UP) or below (otherwise) of (xx yy)."
  (declare (double-float xx yy) (type line ln))
  (if (funcall (if up #'line-above-p #'line-below-p) ln xx yy)
      (line-adjust ln xx yy)
      ln))

(defun line-adjust-list (ln ls up &key (xkey #'car) (ykey #'cdr))
  "Adjust the line LN to pass above (if UP) or below (otherwise) of LS,
keeping the slope intact."
  (declare (type line ln) (list ls)
           (type (function (t) double-float) xkey ykey))
  (do ((ff (if up #'line-above-p #'line-below-p))
       (ll ls (cdr ll)) (xx 0d0) (yy 0d0))
      ((endp ll) ln)
    (declare (double-float xx yy) (type function ff))
    (setq xx (funcall xkey (car ll)) yy (funcall ykey (car ll)))
    (when (funcall ff ln xx yy) (line-adjust ln xx yy))))

(declaim (ftype (function (double-float double-float double-float double-float)
                          (values line))
                line-thru-points))
(defsubst line-thru-points (x0 y0 x1 y1)
  "Make a new line, passing through these 2 points.
If (= x0 x1), an error will be signaled."
  (declare (double-float x0 y0 x1 y1))
  (make-line :co (with-type double-float (/ (- (* y0 x1) (* y1 x0)) (- x1 x0)))
             :sl (with-type double-float (/ (- y1 y0) (- x1 x0)))))

(defun regress (seq &key (xkey #'car) (ykey #'cdr))
  "Return the regression line for the sequence of 2d points.
The second value returned is the deviation from the line.
The accessor keys XKEY and YKEY default to CAR and CDR respectively."
  (declare (sequence seq) (type (function (t) double-float) xkey ykey))
  (case (length seq)
    ((0 1) (error "regress: too few points: ~s~%" seq))
    (2 (values (line-thru-points (funcall xkey (elt seq 0))
                                 (funcall ykey (elt seq 0))
                                 (funcall xkey (elt seq 1))
                                 (funcall ykey (elt seq 1)))
               0d0))
    (t (multiple-value-bind (co xm ym xd yd nn)
           (cov seq :xkey xkey :ykey ykey)
         (declare (double-float co xm ym xd yd) (fixnum nn))
         (let* ((sl (/ co xd)) (err (d/ (- yd (* sl co)) yd)))
           (declare (double-float sl err))
           (when (minusp err)
             (mesg :err t "REGRESS: error is negative: ~f (assumed 0) [len: ~d]
~s~%" err nn seq)
             (setq err 0d0))
           (values (make-line :sl sl :co
                              (with-type double-float (- ym (* xm sl))))
                   (sqrt err)))))))


(declaim (ftype (function (double-float double-float double-float double-float)
                          (values double-float))
                lincom))
(defsubst lincom (c0 x0 c1 x1)
  "Compute c0*x0+c1*x1."
  (declare (double-float c0 x0 c1 x1))
  (with-type double-float (+ (* c0 x0) (* c1 x1))))

(provide :math)
;;; math.lisp ends here
