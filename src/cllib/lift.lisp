;;; Lift (ROC) curve analysis
;;;
;;; Copyright (C) 2004-2005 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: lift.lisp,v 2.3 2005/07/20 20:49:22 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/lift.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `monotonic-p'
  (require :cllib-math (translate-logical-pathname "cllib:math"))
  ;; `plot-functions'
  (require :cllib-gnuplot (translate-logical-pathname "cllib:gnuplot")))

(defpackage :lift
  (:documentation "Lift (ROC) curve analysis")
  (:export #:discretize #:lift-quality #:plot-lifts #:score2prob
           #:lq-lift-quality #:lq-base-rate #:lq-base-rate-detector
           #:lq-total-count #:lq-target-count #:lq-base-more-detectors
           #:detector-statistics #:ds-recall #:ds-precision #:ds-lift
           #:ds-data-reduction #:ds-dependency #:ds-proficiency
           #:bucket #:bucket-size #:bucket-true #:bucket-beg #:bucket-end
           #:bucket-probability #:bucket-inside-p #:bucket-distance
           #:bucket-midpoint #:thresholds
           #:par-proc-vec #:ppv-1st #:ppv-2nd #:ppv-name1 #:ppv-name2
           #:ppv-thresholds #:ppv-precision #:ppv-size
           #:ppv-target-count #:ppv-total-count
           #:*default-buckets* #:*min-bucket-size*))
(in-package :lift)

;;; detector statistics
(defstruct (detector-statistics (:conc-name ds-))
  recall precision lift data-reduction dependency proficiency)
(defmethod print-object ((ds detector-statistics) (out stream))
  (if (or *print-readably* *print-escape*) (call-next-method)
      (format out "[~5F% ~5F% ~5F% ~5F%]"
              (* 1d2 (ds-recall ds)) (* 1d2 (ds-data-reduction ds))
              (* 1d2 (ds-precision ds)) (* 1d2 (ds-proficiency ds)))))

(macrolet ((s/ (part total &optional (%% 1))
             (let ((%total (gensym "S/-Y-")))
               `(let ((,%total ,total))
                  (if (zerop ,%total) 0 (/ ,part ,total ,%%))))))

(defun detector-statistics (hits true-bad predicted-bad total
                            &key (title 'detector-statistics)
                            (out *standard-output*))
  "compute detector statistics"
  (let ((recall (s/ hits true-bad)) (precision (s/ hits predicted-bad))
        (lift (and (plusp true-bad) (plusp predicted-bad)
                   (/ (* hits total) predicted-bad true-bad)))
        (data-reduction (s/ (- total predicted-bad) total))
        (dependency 0) (proficiency 0))
    (when out
      (format out "~&~@[~A: ~]~:D / ~:D (prevalence=~5F%)  data reduction=~5F%
    recall (true positive): ~5F%, precision: ~5F%~@[, lift: ~5F~]~%"
              title true-bad total (s/ true-bad total 1d-2)
              (* 1d2 data-reduction) (* 1d2 recall) (* 1d2 precision) lift))
    (when (and (< 0 true-bad total) (< 0 predicted-bad total))
      (setq dependency (cllib:dependency hits true-bad predicted-bad total)
            proficiency (cllib:proficiency hits true-bad predicted-bad total))
      (when out
        (format out "  correlation: ~5F  dependency: ~5F  proficiency: ~5F~%"
                (cllib:correlation hits true-bad predicted-bad total)
                dependency proficiency)))
    (make-detector-statistics
     :recall recall :precision precision :lift lift
     :data-reduction data-reduction :dependency dependency
     :proficiency proficiency)))
)


(defvar *min-bucket-size* 5
  "the minimum size of a percentile bucket")
(defvar *default-buckets* 10
  "the default granularity `discretize'")

(defstruct bucket (size 0) (true 0) beg end)

(defun bucket-probability (b)
  "Return the probability of a bucket element to be `true'."
  (/ (bucket-true b) (bucket-size b)))

(defun check-bucket (b) (<= (bucket-beg b) (bucket-end b)))

(defun bucket-inside-p (number bucket)
  "Return true when the number is inside the bucket."
  (<= (bucket-beg bucket) number (bucket-end bucket)))

(defun bucket-distance (number bucket)
  "Return the distance from the number to the bucket.
Inside ==> 0; distance is scaled by the bucket length."
  (let ((b (bucket-beg bucket)) (e (bucket-end bucket)))
    (cond ((< number e) (/ (- e number) (- e b)))
          ((< b number) (/ (- number b) (- e b)))
          (t 0))))

(defun bucket-midpoint (bucket)
  "Return the midpoint between bucket beg and end."
  (/ (+ (bucket-beg bucket) (bucket-end bucket)) 2))

(defun bucket (number bucket-seq)
  "Search for the appropriate bucket
for the NUMBER among the elements of BUCKET-SEQ.
Returns 3 values: the bucket number, the bucket object
and the distance from the number to the bucket
\(0 when the number is inside the bucket)."
  (let (best-pos best-bucket best-dist (pos 0))
    (map nil (lambda (bucket)
               (when (bucket-inside-p number bucket)
                 (return-from bucket (values pos bucket 0)))
               (let ((dist (bucket-distance number bucket)))
                 (when (or (null best-dist) (< dist best-dist))
                   (setq best-dist dist best-pos pos best-bucket bucket)))
               (incf pos))
         bucket-seq)
    (values best-pos best-bucket best-dist)))

(defun discretize (seq &key (buckets *default-buckets*) (key #'identity)
                   true-value)
  "discretize the sequence into (approximately) equal-height buckets"
  (setq seq (sort seq #'< :key key)) ; can we assume this?
  (let* ((total-count (length seq)) (bucket-count 1)
         (bucket-size (float (/ total-count buckets) 0d0))
         (mean-size bucket-size) (pos 0)
         (true-count (and true-value (count-if true-value seq)))
         (curr-score (funcall key (elt seq 0))) (prev-score curr-score)
         (bucket (make-bucket :beg curr-score))
         (bucket-list (list bucket)))
    (unless (>= bucket-size *min-bucket-size*)
      (let ((new-buckets (ceiling total-count *min-bucket-size*)))
        (warn "~S: too small buckets: ~5F<~D, reset bucket count from ~D to ~D"
              'discretize bucket-size *min-bucket-size* buckets new-buckets)
        (setq buckets new-buckets
              bucket-size (float (/ total-count buckets) 0d0)
              mean-size bucket-size)))
    ;; bucket-count == (length bucket-list)
    ;; (bucket-end prev) < (bucket-beg next)
    (map nil (lambda (elt)
               (setq prev-score curr-score
                     curr-score (funcall key elt))
               ;; do we need to make a new bucket?
               (when (and (/= curr-score prev-score)
                          ;; try to create the requested number of buckets
                          (>= pos (* bucket-count bucket-size))
                          ;; do not create small buckets
                          (> (* 2 (- total-count pos)) mean-size))
                 (setf (bucket-end bucket) prev-score)
                 (setq mean-size
                       (/ (+ (bucket-size bucket) (* mean-size bucket-count))
                          (incf bucket-count))
                       bucket (make-bucket :beg curr-score))
                 (push bucket bucket-list))
               (when (and true-value (funcall true-value elt))
                 (incf (bucket-true bucket)))
               (incf (bucket-size bucket))
               (incf pos))
         seq)
    (setf (bucket-end bucket) curr-score
          bucket-list (nreverse bucket-list))
    (let ((total (reduce #'+ bucket-list :key #'bucket-size)))
      (assert (= total-count total) (total-count bucket-list)
              "~S: bad total count: ~S /= ~S~%~S"
              'discretize total-count total bucket-list))
    (when true-value
      (let ((total (reduce #'+ bucket-list :key #'bucket-true)))
        (assert (= true-count total) (true-count bucket-list)
                "~S: bad true count: ~S /= ~S~%~S"
                'discretize true-count total bucket-list)))
    (assert (every #'check-bucket bucket-list)
            (bucket-list) "~S: bad buckets:~%~S" 'discretize
            (remove-if-not #'check-bucket bucket-list))
    (let (bad)
      (assert (null (setq bad (loop :for (a b) :on bucket-list
                                :when (and b (>= (bucket-end a) (bucket-beg b)))
                                :collect (list a b))))
              (bucket-list)
              "~S: bucket thresholds are not increasing:~S" 'discretize bad))
    bucket-list))

(defmacro check-targets (target-count fun)
  `(when (zerop ,target-count)
     (warn "~S: no targets!" ',fun)
     (return-from ,fun (values))))

(defstruct (lift-quality (:conc-name lq-))
  lift-quality base-rate base-rate-detector total-count target-count
  more-rate-detectors)
(defun lift-quality (seq &key score (true-value (port:required-argument))
                     (out *standard-output*) more-thresholds)
  "compute the lift quality"
  (declare (type (or null (function (t) (values real))) score)
           (type (or symbol (function (t) (values boolean))) true-value))
  (when score (setq seq (sort seq #'> :key score)))
  (setq true-value (coerce true-value 'function))
  (let* ((target-count (count-if true-value seq)) (total-count (length seq))
         (base-rate (float (/ target-count total-count)))
         (cph 0) (target-level 0))
    (check-targets target-count lift-quality)
    (map nil (lambda (elt)
               (when (funcall true-value elt)
                 (incf target-level))
               (incf cph target-level))
         seq)
    (assert (= target-level target-count) (target-level target-count)
            "target count mismatch: ~D /= ~D" target-level target-count)
    (let ((lq (/ (1- (/ (- (* 2 cph) target-count) total-count target-count))
                 (- 1 base-rate)))) ; l-quality
      (when out
        (format out "~&;; ~S: ~5F%, base rate=~5F% (~:D/~:D)~%"
                'lift-quality (* lq 100) (* base-rate 100)
                target-count total-count))
      (make-lift-quality
       :lift-quality lq :base-rate base-rate
       :base-rate-detector ; detector stats at BASE-RATE
       (detector-statistics (count-if true-value seq :end target-count)
                            target-count target-count total-count :out out)
       :more-rate-detectors
       (mapcar (lambda (br)
                 (let ((count
                        (etypecase br
                          ((real 0 1) (* br total-count))
                          ((real 1) (round (* br target-count))))))
                   (detector-statistics
                    (count-if true-value seq :end count)
                    target-count count total-count :out out)))
               more-thresholds)
       :total-count total-count :target-count target-count))))

(defun plot-lifts (lifts &key score true-value (title "lift plot") plot
                   &aux (total-count (length (rest (first lifts))))
                   (target-count (count-if true-value (rest (first lifts)))))
  "plot several lift curves for the same dataset"
  (when score
    (dolist (li lifts)
      (setf (rest li) (sort (rest li) #'> :key score))))
  (cllib:plot-lists-arg
   (mapcar (lambda (li)
             (cons (format nil "~A Lq=~5f%" (first li) ; title of this lift
                           (* 100 (lift-quality (rest li) :score score
                                                :true-value true-value)))
                   (loop :with target-level = 0 :for pos ::upfrom 0
                     :for l :in (rest li)
                     :when (funcall true-value l) :do (incf target-level)
                     :collect (cons (/ pos total-count)
                                    (/ target-level target-count)))))
           lifts)
   :lines (list (cllib:make-line :sl 1d0 :co 0d0) ; random
                (cllib:make-line :sl (/ total-count target-count 1d0)
                                 :co 0d0)) ; perfect
   :xb 0 :xe 1 :yb 0 :ye 1 :plot plot :legend '(:bot :right :box)
   :title (format nil "~A (prevalence=~5f%)" title
                  (/ target-count total-count 1d-2))
   :xlabel "population percentile" :ylabel "target percentage"))

(defun score2prob (seq &key score true-value (buckets *default-buckets*)
                   (out *standard-output*) (plot t))
  "Return an increasing piecewise linear function
which converts scores to probabilities."
  (when out (lift-quality seq :score score :true-value true-value :out out))
  (let* ((bucket-list (discretize seq :key score :buckets buckets
                                  :true-value true-value))
         (bucket1 (first bucket-list))
         (len (length bucket-list)) plf
         (yv (make-array (1+ len))) (xv (make-array (1+ len))))
    (setf (aref yv 0) (/ (bucket-probability bucket1) 2d0)
          (aref xv 0) (/ (bucket-beg bucket1) 2d0))
    (loop :for (b0 b1) :on bucket-list
      :for pos :upfrom 1
      :do (setf (aref yv pos)
                (/ (+ (bucket-probability b0)
                      (if b1 (bucket-probability b1) 1d0))
                   2d0)
                (aref xv pos)
                (/ (+ (bucket-end b0)
                      (if b1 (bucket-beg b1) 1d0))
                   2d0)))
    (if (zerop (aref xv 0))
        (if (= 1 (aref xv len))
            nil                 ; no need to augment
            (setq xv (concatenate 'vector xv #(1d0))
                  yv (concatenate 'vector yv #(1d0))))
        (if (= 1 (aref xv len))
            (setq xv (concatenate 'vector #(0d0) xv)
                  yv (concatenate 'vector #(0d0) yv))
            (setq xv (concatenate 'vector #(0d0) xv #(1d0))
                  yv (concatenate 'vector #(0d0) yv #(1d0)))))
    (setf plf (cllib:plf-simplify
               (cllib:make-plf :x xv :y (cllib:increasify yv))))
    (when out
      (format out "~&;; ~S: ~:D node~:P, integral=~5F~%"
              'score2prob (cllib:plf-size plf) (cllib:plf-integral plf)))
    (when plot
      (cllib:plot-functions
       (list (cons 'plf (cllib:plf->function plf))) 0 1 100
       :grid t :xlabel "score" :ylabel "probability"
       :title "score->probability"))
    plf))

(defstruct (par-proc-vec (:conc-name ppv-))
  name1 1st name2 2nd thresholds precision target-count total-count size curr)
(defun make-ppv (name1 1st name2 target-count total-count &aux (l (length 1st)))
  (make-par-proc-vec :name1 name1 :1st (make-array l :initial-contents 1st)
                     :target-count target-count :total-count total-count
                     :thresholds (make-array l) :curr 0 :size l
                     :precision (make-array l)
                     :name2 name2 :2nd (make-array l)))
(defun ppv-maybe-pop (ppv curr-1st 1st-total curr-2nd 2nd-total score precision)
  (loop :with size = (ppv-size ppv)
    :and 1st = (ppv-1st ppv) :and p = (ppv-precision ppv)
    :and 2nd = (ppv-2nd ppv) :and h = (ppv-thresholds ppv)
    :for curr = (ppv-curr ppv)
    :while (and (< curr size) (>= curr-1st (* 1st-total (aref 1st curr))))
    :do (setf (aref 2nd curr) (/ curr-2nd 2nd-total)
              (aref h curr) score
              (aref p curr) precision
              (ppv-curr ppv) (1+ curr))))
(defun ppv-done-p (p) (= (ppv-curr p) (ppv-size p)))
(defun ppv-check-done (p) (assert (ppv-done-p p) (p) "unfinished: ~S" p))
(defun ppv-out (ppv mdl out)
  (loop :with name1 = (ppv-name1 ppv) :and name2 = (ppv-name2 ppv)
    :for x :across (ppv-1st ppv) :and y :across (ppv-2nd ppv)
    :and h :across (ppv-thresholds ppv) :and p :across (ppv-precision ppv)
    :do (format out ";;  ~A=~5F% ==> ~A=~5F% thr=~5F (~5F) precision=~5F%~%"
                name1 (* 100 x) name2 (* 100 y)
                h (/ (- h (cllib:mdl-mn mdl)) (cllib:mdl-sd mdl)) (* p 100))))

(defun thresholds (seq &key (score (port:required-argument))
                   (true-value (port:required-argument))
                   (out *standard-output*) x y)
  "compute recalls (y) for the given cut-offs (x)
and cut-offs (x) for the given recalls (y)"
  (declare (type (function (t) (values real)) score)
           (type (function (t) (values boolean)) true-value))
  (setq seq (sort seq #'> :key score))
  (let* ((target-count (count-if true-value seq)) (total-count (length seq))
         (x-ret (make-ppv "cutoff" (sort x #'<) "recall"
                          target-count total-count))
         (y-ret (make-ppv "recall" (sort y #'<) "cutoff"
                          target-count total-count))
         (curr-score (funcall score (elt seq 0))) (prev-score curr-score)
         (target-level 0) (pos 0))
    (check-targets target-count thresholds)
    (map nil (lambda (elt)
               (setq prev-score curr-score
                     curr-score (funcall score elt))
               (when (/= curr-score prev-score)
                 (let ((precision (/ target-level pos)))
                   (ppv-maybe-pop x-ret pos total-count target-level
                                  target-count prev-score precision)
                   (ppv-maybe-pop y-ret target-level target-count pos
                                  total-count prev-score precision)))
               (incf pos)
               (when (funcall true-value elt)
                 (incf target-level)))
         seq)
    (let ((precision (/ target-level pos)))
      (ppv-maybe-pop x-ret pos total-count target-level
                     target-count prev-score precision)
      (ppv-maybe-pop y-ret target-level target-count pos
                     total-count prev-score precision))
    (assert (= target-level target-count) (target-level target-count)
            "target count mismatch: ~D /= ~D" target-level target-count)
    (assert (= pos total-count) (pos total-count)
            "total count mismatch: ~D /= ~D" pos total-count)
    (ppv-check-done x-ret) (ppv-check-done y-ret)
    (when out
      (let ((mdl (cllib:standard-deviation-mdl seq :key score)))
        (ppv-out x-ret mdl out)
        (ppv-out y-ret mdl out)))
    (values x-ret y-ret)))

(provide :lift)
;;; file lift.lisp ends here
