;;; Financial functions
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: fin.lisp,v 2.1 2000/03/27 20:02:54 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/fin.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `*num-tolerance*', `sqr', `erf'
  (require :math (translate-logical-pathname "cllib:math"))
  ;; `comma'
  (require :tilsla (translate-logical-pathname "cllib:tilsla"))
  ;; `dfloat'
  (require :withtype (translate-logical-pathname "cllib:withtype")))

(in-package :cllib)

(export '(mgg-monthly mgg-compare mgg-prepay mgg-interest
          black-scholes-call black-scholes-eput
          solow solow-next-year
          lognormal luhn))

(eval-when (compile load eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

;;;
;;; Mortgage calculator
;;;

(defun mgg-rate (apr &optional monthly)
  "Convert the annual to monthly."
  (if monthly (1- (expt (1+ apr) (/ 1.0 12))) (/ apr 12.0)))

(defun mgg-discount (term apr &optional monthly)
  "The discount (principal/periodic payment) for the TERM and APR."
  (let ((rate (mgg-rate apr monthly)))
    (/ rate (- 1 (expt (1+ rate) (- term))))))

(defun mgg-monthly (principal term apr &optional monthly)
  "Calculate the monthly payment on mortgage.
PRINCIPAL is being repaid monthly over TERM years, with APR compounded
MONTHLY (if it is not given or nil - yearly).
APR of 7% should be given as 0.07, not as 7."
  (declare (number principal term apr))
  (* principal (mgg-discount (* 12 term) apr monthly)))

(defun mgg-term (discount apr &optional monthly)
  "Compute the term for the DISCOUNT and the (MONTHLY) APR."
  (let ((rate (mgg-rate apr monthly)))
    (- (log (- 1 (/ rate discount)) (1+ rate)))))

;;;###autoload
(defun mgg-compare (principal term apr apr-pts &optional monthly)
  "Compare two APRs by their present values."
  (let* ((tt (* 12 term)) (di (mgg-discount tt apr monthly))
         (mm (* principal di))
         (res (sort
               (mapcar (lambda (pa)
                         (let* ((apr1 (car pa)) (pts (cdr pa))
                                (di1 (mgg-discount tt apr1 monthly))
                                (mm1 (* principal di1))
                                (dd (- mm mm1)) (amt (/ dd di1))
                                (pnt-amt (* pts 0.01 principal))
                                (term (/ (mgg-term (/ dd pnt-amt) apr1) 12)))
                           (list (* 100 apr1) mm1 dd amt pts pnt-amt term)))
                       apr-pts)
               #'< :key #'seventh)))
    (format t "Principal: ~2:/comma/   term: ~d years
~5,3f% -> ~2,6:/comma/~%" principal term (* 100 apr) mm)
    (dolist (ll res)
      (apply #'format t "~5,3f% -> ~2,6:/comma/ [~2:/comma/ -> ~
~2:/comma/] pts: ~5,3f% (~2:/comma/) -> ~5fyrs~%" ll))
    ;; (values mm res)
    (values)))

;;;###autoload
(defun mgg-prepay (principal term apr prepay &optional monthly)
  "Print the information about prepaying the loan."
  (let* ((mm (mgg-monthly principal term apr monthly)) (tot (+ prepay mm))
         (trm (/ (mgg-term (/ tot principal) apr monthly) 12)))
    (format t "Principal: ~2:/comma/  APR: ~f  --> ~2,9:/comma/ --> ~5,2f years
Prepay: ~2:/comma/~35t  --> ~2,9:/comma/ --> ~5,2f years~%"
            principal apr mm term prepay tot trm)))

(defun mgg-interest (principal term apr &optional monthly)
  "Calculate the total interest that will ever be paid on the mortgage."
  (declare (number principal term rate))
  (- (* term (/ principal (mgg-discount term apr monthly))) principal))

;;;
;;; Black-Scholes analysis
;;; (see Hull, "Options, Futures, and other Derivatives",  chapter 11)
;;;

(defun black-scholes-roots (cu-str intrst time vltlt)
  "Compute d1 and d2 for Black-Scholes formula."
  (declare (number cu-str intrst time vltlt))
  (let* ((de (* vltlt (sqrt time)))
	 (d1 (/ (+ (log cu-str) (* time (+ intrst (/ (sqr vltlt) 2)))) de)))
    (values d1 (- d1 de))))

;;;###autoload
(defun black-scholes-call (strike curr intrst time vltlt)
  "Compute the Black-Scholes value of a call option (American or European).
Arguments are: option strike price, current stock price, risk-free
interest rate for the term, time to expiration, stock volatility."
  (declare (number strike curr intrst time vltlt))
  (multiple-value-bind (d1 d2)
      (black-scholes-roots (/ curr strike) intrst time vltlt)
    (- (* curr (erf d1))
       (* strike (erf d2) (exp (- (* intrst time)))))))

;;;###autoload
(defun black-scholes-eput (strike curr intrst time vltlt)
  "Compute the Black-Scholes value of a European put option.
See `black-scholes-call' for details."
  (declare (number strike curr intrst time vltlt))
  (multiple-value-bind (d1 d2)
      (black-scholes-roots (/ curr strike) intrst time vltlt)
    (- (* strike (erf (- d2)) (exp (- (* intrst time))))
       (* curr (erf (- d1))))))

;;;
;;; Solow Economic Growth Model
;;;

(defun solow-next-year (kap k-y sav dgn alpha)
  "Return the next year's data from this year.
KAP - capital/productivity unit; K-Y - capital/total income;
SAV - saving rate; DGN = delta + g + n - depreciation of the
capital + technological growth + population growth; ALPHA -
the Cobb-? function parameter.
Returns: new capital and new k/y."
  (declare (double-float kap k-y sav dgn alpha))
  (let ((kk (* kap (- (1+ (/ sav k-y)) dgn))))
    (values kk (expt kk (- 1 alpha)))))

(defun k/k (sav dng alpha)
  "Returns a function (suitable for `newton'), which computes
the ratio K_{n+1}/K_n from K_n/K_{n-1}."
  (declare (double-float sav dgn alpha))
  (lambda (zz) (values (- zz (- 1 dng) (* sav (expt zz (- 1 alpha))))
		       (- 1 (* sav (- 1 alpha) (expt zz (- alpha)))))))

;;;###autoload
(defun solow (k-y-0 dng alpha)
  "Show the economy evolution from the steady state with saving rate SAV
and Capital/Income ratio K-Y-0 to the Golden Rate steady state."
  (declare (double-float k-y-0 sav dgn alpha))
  (format t "~&year capital  income     k/y consumption~%")
  (let* ((kk0 (expt k-y-0 (/ 1.0 (- 1 alpha)))) (sav (* dng k-y-0))
	 (yy0 (expt kk0 alpha)) (cc0 (* (- 1 sav) yy0)))
    (format t "    ~8,3f~8,3f~8,3f~8,3f~%" kk0 yy0 k-y-0 cc0)
    (do ((del 1.0) yy cc (nn 1 (1+ nn)) (kk kk0) (ky k-y-0))
	((or (< del *num-tolerance*) (> nn 100)))
      (multiple-value-setq (kk ky) (solow-next-year kk ky alpha dng alpha))
      (setq yy (/ kk ky) cc (* (- 1 alpha) yy))
      (format t "~4:d~8,3f~8,3f~8,3f~8,3f" nn kk yy ky cc)
      (when (and cc0 (> cc cc0)) (setq cc0 nil) (format t " <---!!!!"))
      (terpri))))

;;;
;;; Lognormal distribution
;;;

(defun lognormal (xx)
  (declare (double-float xx) (values double-float))
  (if (plusp xx)
      (/ (exp (let ((ll (log xx))) (* ll ll -0.5)))
         (dfloat (sqrt (* 2 pi))) xx)
      0.0))

;;;
;;; Checking Credit Card Number validity via Luhn algorithm.
;;; http://prope.insa-lyon.fr/~fcoppola/credit.html
;;; http://www-personal.engin.umich.edu/~jgotts/hack-faq/hack-faq-f.html
;;;

;;;###autoload
(defun luhn (cn &optional (out *standard-output*))
  "Check whether the card number CN is valid.
For a card with an even number of digits, double every odd numbered
digit and subtract 9 if the product is greater than 9. Add up all the
even digits as well as the doubled-odd digits, and the result must be a
multiple of 10 or it's not a valid card. If the card has an odd number
of digits, perform the same addition doubling the even numbered digits
instead."
  (declare (type (or integer string) cn) (type (or null stream) out))
  (etypecase cn
    (string (luhn (parse-integer cn)))
    (integer
     (loop :with vv :of-type integer = cn :and rr :of-type (integer 0 9) = 0
           :do (setf (values vv rr) (floor vv 10))
           :sum rr :into su :of-type index-t
           :while (plusp vv)
           :do (setf (values vv rr) (floor vv 10))
           :sum (let ((rr (* 2 rr))) (if (> rr 9) (- rr 9) rr)) :into su
           :while (plusp vv)
           :finally (when out (format out "Luhn[~,,' ,4:d] = ~d" cn su))
           (when (zerop (mod su 10))
             (return (case rr (3 :amex) (4 :visa) (5 :mastercard)
                           (6 :discover) (t :unknown))))))))

#+nil
(progn

(newton (lambda (xx)
          (values (integrate-simpson #'lognormal 0 xx) (lognormal xx)))
        :val 0.5 :ival 2)
;; ==> 1.0

(time
(setq lll
(loop with lognormalx = (lambda (xx) (* xx (lognormal xx)))
      with mean = (integrate-simpson lognormalx 0.0 100.0)
      for xx of-type double-float from 0.1 to 20.0 by 0.1
      for v0 = (* 100 (integrate-simpson #'lognormal 0.0 xx))
      for v1 = (/ (integrate-simpson lognormalx 0.0 xx) mean 0.01)
      do (format t "~%~4f low ~6,3f% have ~6,3f%, top ~6,3f% have ~6,3f%"
                 xx v0 v1 (- 100 v0) (- 100 v1))
      collect (cons v0 v1))))

(plot-lists-arg (list (cons "low" lll)
                      (cons "high"
                            (mapcar (lambda (cc)
                                      (cons (- 100 (car cc))
                                            (- 100 (cdr cc))))
                                    lll)))
;                       (cons "circle1"
;                             (loop for xx from 0 to 100 by 1 do
;                                   collect
;                                   (cons xx (- 100 (sqrt
;                                                    (- 10000 (* xx xx)))))))
;                       (cons "circle2"
;                             (loop for xx from 0 to 100 by 1 do
;                                   collect
;                                   (cons xx (sqrt (- 10000
;                                                     (sqr (- 100 xx))))))))
                :xbeg 0 :xend 100 :ylabel "percent own" :xlabel "percent below"
                :title "lognormal" :plot :print :xtics 5 :ytics 5 :grid t)

(dolist (cc lll)
  (format t "~6,3f ~6,3f -> ~10f~%" (car cc) (cdr cc)
          (+ (sqr (car cc)) (sqr (- 100 (cdr cc))))))

)

;;; fin.lisp ends here
