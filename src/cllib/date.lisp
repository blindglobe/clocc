;;; File: <date.lisp - 1997-10-30 Thu 13:58:53 EST - sds@WINTERMUTE.eagle>
;;;
;;; Date-related structures
;;;

;;;
;;; Date
;;;

(defstruct (date (:print-function print-date))
  "the date structure"
  (ye 1 :type fixnum)
  (mo 1 :type (integer 1 12))
  (da 1 :type (integer 1 31)))

(defun print-date (date &optional stream depth)
  "Print the date"
  (declare (ignore depth) (type date date))
  (format stream *print-date-format* (date-ye date)
	  (date-mo date) (date-da date)))

(defsubst date2num (dt)
  "Convert the date to the numerical format."
  (declare (type date dt))
  (+ (* 10000 (date-ye dt)) (* 100 (date-mo dt)) (date-da dt)))

(defun today ()
  "Return today's date."
  (multiple-value-bind (se mi ho da mo ye) (get-decoded-time)
    (declare (ignore se mi ho))
    (make-date :ye ye :mo mo :da da)))

(defsubst date-fix-year (dt)
  "Fix the year to be from C.E., not from the beginning of the century."
  (declare (type date dt))
  (let ((ye (date-ye dt)))
    (if (< ye *century*) (setf (date-ye dt) (+ ye *century*)))) dt)

(defun read-date (stream)
  "Read date in format MONTH DAY YEAR."
  (make-date :mo (read stream) :da (read stream) :ye (read stream)))

(defun parse-date (st-sy)
  "Get the date out of a string `1969-12-07' or a similar symbol."
  (let ((str (nsubstitute #\space #\- (string st-sy))))
    (multiple-value-bind (ye n0) (read-from-string str)
      (multiple-value-bind (mo n1) (read-from-string str nil 0 :start n0)
	(make-date :ye (or ye 0) :mo mo :da
		   (read-from-string str nil 0 :start n1))))))

(defsubst num2date (num)
  "Get the date from the number."
  (declare (number num))
  (make-date :ye (floor (/ num 10000)) :mo (mod (floor (/ num 100)) 100)
	     :da (mod num 100)))

(defsubst same-date-p (d0 d1)
  "Check that the dates are the same. Like equalp, but works with children."
  (setq d0 (if (date-p d0) d0 (parse-date d0))
	d1 (if (date-p d1) d1 (parse-date d1)))
  (and (= (date-ye d0) (date-ye d1))
       (= (date-mo d0) (date-mo d1))
       (= (date-da d0) (date-da d1))))

(defsubst date-less-p (d0 d1)
  "Check the precedence of the two dates."
  (setq d0 (if (date-p d0) d0 (parse-date d0))
	d1 (if (date-p d1) d1 (parse-date d1)))
  (< (if (date-p d0) (date2num d0) d0)
     (if (date-p d1) (date2num d1) d1)))

(defsubst date-more-p (d0 d1)
  "Check the precedence of the two dates."
  (setq d0 (if (date-p d0) d0 (parse-date d0))
	d1 (if (date-p d1) d1 (parse-date d1)))
  (> (if (date-p d0) (date2num d0) d0)
     (if (date-p d1) (date2num d1) d1)))

(defsubst latest-date (d0 d1)
  "Return the latest date."
  (setq d0 (if (date-p d0) d0 (parse-date d0))
	d1 (if (date-p d1) d1 (parse-date d1)))
  (if (date-less-p d0 d1) d1 d0))

(defsubst earliest-date (d0 d1)
  "Return the latest date."
  (setq d0 (if (date-p d0) d0 (parse-date d0))
	d1 (if (date-p d1) d1 (parse-date d1)))
  (if (date-more-p d0 d1) d1 d0))

(defsubst next-month-p (d0 d1)
  "True if D1 is the next month of D0."
  (declare (type date d0 d1))
  (let ((y0 (date-ye d0)) (y1 (date-ye d1))
	(m0 (date-mo d0)) (m1 (date-mo d1)))
    (or (and (= (1+ m0) m1) (= y0 y1))
	(and (= m0 12) (= m1 1) (= (1+ y0) y1)))))

(defsubst same-month-p (d0 d1)
  "Return t if the dates are in the same month."
  (declare (type date d0 d1))
  (and (= (date-ye d0) (date-ye d1)) (= (date-mo d0) (date-mo d1))))

(defsubst next-quarter-p (d0 d1)
  "True if D1 is the next quarter of D0."
  (declare (type date d0 d1))
  (let ((y0 (date-ye d0)) (y1 (date-ye d1))
	(m0 (date-mo d0)) (m1 (date-mo d1)))
    (or (and (= m1 1) (= (1+ y0) y1) (> m0 9))
	(and (= y0 y1) (< m0 m1) (> (+ 4 m0) m1) (member m1 '(1 4 7 10))))))

(defsubst date-quarter (dt)
  "Return the quarter of the date."
  (declare (type date dt))
  (1+ (floor (1- (date-mo dt)) 3)))

(defsubst same-quarter-p (d0 d1)
  "Return t if the dates are in the same quarter."
  (declare (type date d0 d1))
  (and (= (date-ye d0) (date-ye d1))
       (= (date-quarter d0) (date-quarter d1))))

(defun days-between (d0 &optional (d1 (today)))
  "Return the number of days between the two dates."
  (setq d0 (if (date-p d0) d0 (parse-date d0))
	d1 (if (date-p d1) d1 (parse-date d1)))
  (round				; DST intervenes!
   (/ (- (encode-universal-time 0 0 0 (date-da d1) (date-mo d1) (date-ye d1))
	 (encode-universal-time 0 0 0 (date-da d0) (date-mo d0) (date-ye d0)))
      24 60 60)))

(defun tomorrow (dd &optional (skip 1))
  "Return the next day.
With the optional second argument (defaults to 1) skip as many days.
I.e., (tomorrow (today) -1) is yesterday."
  (declare (type date dd))
  (multiple-value-bind (se mi ho da mo ye)
      (decode-universal-time
       (+ (encode-universal-time 0 0 0 (date-da dd)
				 (date-mo dd) (date-ye dd))
	  (* 24 60 60 skip)))
    (declare (ignore se mi ho))
    (make-date :ye ye :mo mo :da da)))

(defun check-dates (lst order-p same-p gap &key (key #'identity) (stream t))
  "Check the dated list LST for order violations, redundancies and gaps."
  (when (dated-list-p lst)
    (setq key (dated-list-date lst) lst (dated-list-ll lst)))
  (format stream "Checking the list for:~?.~%"
	  (list-format "~{~a~}")
	  (nconc (if order-p (list (list "order")) nil)
		 (if same-p (list (list "redundancies")) nil)
		 (if gap (list (list "gaps of at least " gap)) nil)))
  (do ((rr lst (cdr rr)) (len 0 (1+ len))
       (r0 (first lst) r1) r1
       (k0 (funcall key (first lst)) k1) k1)
      ((null (cdr rr)) (format t "~:d record~:p, ~a through ~a.~%"
			       len (funcall key (first lst)) k1))
    (setq r1 (second rr) k1 (funcall key r1))
    (when (and order-p (date-less-p k1 k0))
      (format stream "Wrong Order:~% - ~S~% - ~S~%~%" r0 r1))
    (when (and same-p (equalp k0 k1))
      (format stream "Same Date:~% - ~S~% - ~S~%~%" r0 r1))
    (when (and gap (> (days-between k0 k1) gap))
      (format stream "Large Gap:~% - ~S~% - ~S~%~%" r0 r1))))

(defun sync-dates (lists &key lables key cpy set (stream t) op skip)
  "Make all the lists have records for all the dates.
If LABLES is not given, no messages are printed.
\(funcall KEY record) ==> date
\(funcall CPY rec) ==> copy of rec
\(funcall SET rec dt) ==> set the date of the rec.
Stops when the lists end. Prints messages to STREAM.
Calls OP on the section of the LISTS with the same dates, using the
previous record when SKIP is non-nil and nil otherwise.
  (sync-dates lists &key lables key cpy set (stream t) op skip)"
  (declare (list lists))
  (when stream
    (format stream "Synching dates in ~d lists.~%" (length lists)))
  (do ((sec (copy-list lists)) (heads (make-list (length lists)))
       bd fnn (err nil nil) ckey (nerr 0) (len 0 (1+ len)))
      ((every #'null sec)
       (when stream
	 (format stream "~:d records in ~d lists checked. ~d error~:p found.~%"
		 len (length lists) nerr)))
    ;; get the current date
    (setq fnn (member nil sec :test-not #'eq)
	  bd (funcall key (caar fnn)))
    (dolist (ls (rest fnn))
      (when ls (setq ckey (funcall key (car ls)))
	    (when (date-more-p bd ckey) (setq bd ckey))
	    (unless (equalp bd ckey) (setq err t))))
    ;; handle date mismatches
    (when (and stream lables err)
      (format stream " --[~:d]--> date mismatch:~:{~%  <~a: ~a>~}~%"
	      len (mapcar #'cons lables sec))
      (incf nerr))
    ;; shift, fix and operate
    (mapl (lambda (ls hd)
	    (if (equalp bd (funcall key (caar ls)))
		(setf (car hd) (caar ls))
		(let ((nr (funcall cpy (caar ls))))
		  (funcall set (caar ls) bd)
		  (setf (cdar ls) (cons nr (cdar ls))
			(car hd) (if skip (caar ls)))))
	    (pop (car ls)))
	  sec heads)
    (when op (apply op heads))))

(defmacro sync-dates-ui (lists &key lables key cpy (stream t) op skip)
  "The use KEY for SET. See `sync-dates'."
  `(sync-dates ,lists :lables ,lables :key ,key :cpy ,cpy :stream ,stream
    :set (lambda (rr dd) (setf (funcall ,key rr) dd)) :op ,op :skip ,skip))

(defmacro process-and-shift (pred akey ckey t0 e0 k0 t1 e1 k1)
  "Used in *-sorted."
  `(cond ((or (null k1) (and k0 (funcall ,pred ,k0 ,k1)))
	  (multiple-value-prog1 (values (funcall ,akey ,e0) nil)
	    (setq ,t0 (cdr ,t0) ,e0 (car ,t0)
		  ,k0 (and ,t0 (funcall ,ckey ,e0)))))
    ((or (null k0) (and k1 (funcall ,pred ,k1 ,k0)))
     (multiple-value-prog1 (values nil (funcall ,akey ,e1))
       (setq ,t1 (cdr ,t1) ,e1 (car ,t1) ,k1 (and ,t1 (funcall ckey ,e1)))))
    (t (multiple-value-prog1 (values (funcall ,akey ,e0) (funcall ,akey ,e1))
	 (setq ,t0 (cdr ,t0) ,e0 (car ,t0) ,k0 (and ,t0 (funcall ,ckey ,e0))
	       ,t1 (cdr ,t1) ,e1 (car ,t1)
	       ,k1 (and ,t1 (funcall ,ckey ,e1)))))))

(defun map-sorted (type func pred l0 l1
		   &key (ckey #'identity) (akey #'identity))
  "Operate on two sorted lists. Call FUNC on the elements of the lists
that are `same' according to PRED. If TYPE is 'LIST, return the list
of whatever FUNC returns."
  (declare (function func pred ckey akey) (list l0 l1) (symbol type))
  (do ((t0 l0) (t1 l1) (e0 (car l0)) (e1 (car l1)) el res
       (k0 (and l0 (funcall ckey (car l0))))
       (k1 (and l1 (funcall ckey (car l1)))))
      ((and (null t0) (null t1)) (nreverse res))
    (setq el (multiple-value-call func
	       (process-and-shift pred akey ckey t0 e0 k0 t1 e1 k1)))
    (when type (push el res))))

(defun reduce-sorted (rfunc func2 pred l0 l1
		      &key (ckey #'identity) (akey #'identity) initial-value)
  "Reduce a pair of sorted sequences."
  (declare (function rfunc func2 pred ckey akey) (list l0 l1))
  (let ((res initial-value) (t0 l0) (t1 l1) (e0 (car l0)) (e1 (car l1))
	(k0 (and l0 (funcall ckey (car l0))))
	(k1 (and l1 (funcall ckey (car l1)))))
    (unless res
      (setq res
	    (if (or l0 l1)
		(multiple-value-call func2
		  (process-and-shift pred akey ckey t0 e0 k0 t1 e1 k1))
		(funcall rfunc))))
    (do () ((and (null t0) (null t1)) res)
      (setq res (funcall rfunc res
			 (multiple-value-call func2
			   (process-and-shift pred akey ckey
					      t0 e0 k0 t1 e1 k1)))))))

(defun sorted-map (type func pred missing ckey akey &rest lists)
  "Operate on the corresponding elements of the sorted lists.  Each list
in LISTS is assumed to be sorted according to the predicate PRED applied
to keys CKEY.  Apply function FUNC to the AKEYs of the elements of the
lists with the same CKEYs.  When a list doesn't have an element with the
particular CKEY, function gets nil (if MISSING is nil) or the previous
AKEY (if MISSING is non-nil).
CKEY and AKEY values of nil are the same as #'identity.
  (sorted-map type func pred missing ckey akey &rest lists)"
  (declare (function func pred) (symbol type))
  (unless ckey (setq ckey #'identity))
  (unless akey (setq akey #'identity))
  (do ((sec (copy-list lists)) (akeys (make-list (length lists)))
       begck ck fnn (err nil nil) res)
      ((every #'null sec) (nreverse res))
    ;; get the current ckey
    (setq fnn (member nil sec :test-not #'eq)
	  begck (funcall ckey (caar fnn)))
    (dolist (ls (rest fnn))
      (when ls (setq ck (funcall ckey (car ls)))
	    (when (funcall pred ck begck) (setq begck ck))))
    ;; shift and operate
    (mapl (lambda (ls ak)
	    (cond ((and (car ls)
			(not (funcall pred begck (funcall ckey (caar ls)))))
		   (setf (car ak) (funcall akey (caar ls)))
		   (pop (car ls)))
		  (t (if missing nil (setf (car ak) nil)))))
	  sec akeys)
    (cond ((eq type 'list) (push (apply func akeys) res))
	  (t (apply func akeys)))))


;;;
;;; Dated List
;;;

(defstruct (dated-list (:print-function print-dlist))
  "A dated list of records."
  (ll nil :type list)			; the actual list
  (code nil :type symbol)		; the code (2 letter symbol)
  (name "??" :type string)		; the name of the data
  (date #'identity :type (function date)) ; the date accessor
  (val #'identity :type (function float)) ; the value accessor
  (chg #'identity :type (function float)) ; the change accessor
  (misc #'identity :type function))	; the miscellaneous accessor

(defsubst dl-len (dl)
  "Return the length of the dated list."
  (declare (type dated-list dl))
  (length (dated-list-ll dl)))

(defsubst dl-beg (dl)
  "Return the first record of the dated list."
  (declare (type dated-list dl))
  (car (dated-list-ll dl)))

(defsubst dl-end (dl)
  "Return the last record of the dated list."
  (declare (type dated-list dl))
  (car (last (dated-list-ll dl))))

(defsubst dl-beg-date (dl)
  "Return the starting date of the dated list."
  (declare (type dated-list dl))
  (let ((bb (dl-beg dl))) (and bb (funcall (dated-list-date dl) bb))))

(defsubst dl-end-date (dl)
  "Return the ending date of the dated list."
  (declare (type dated-list dl))
  (let ((ee (dl-end dl))) (and ee (funcall (dated-list-date dl) ee))))

(defsubst dl-beg-val (dl)
  "Return the starting value of the dated list."
  (declare (type dated-list dl))
  (let ((bb (dl-beg dl))) (and bb (funcall (dated-list-val dl) bb))))

(defsubst dl-beg-chg (dl)
  "Return the starting change of the dated list."
  (declare (type dated-list dl))
  (let ((bb (dl-beg dl))) (and bb (funcall (dated-list-chg dl) bb))))

(defsubst dl-beg-misc (dl)
  "Return the starting MISC of the dated list."
  (declare (type dated-list dl))
  (let ((bb (dl-beg dl))) (and bb (funcall (dated-list-misc dl) bb))))

(defsubst dl-beg-slot (dl slot)
  "Return the starting SLOT of the dated list."
  (declare (type dated-list dl))
  (let ((bb (dl-beg dl))) (and bb (funcall (slot-value dl slot) bb))))

(defsubst print-dlist (dl &optional stream depth)
  "Print the dated list record."
  (declare (type dated-list dl))
  (when (minusp depth)
    (format t "~%date:~10t~a~%val:~10t~a~%chg:~10t~a~%misc:~10t~a~%"
	    (dated-list-date dl) (dated-list-val dl) (dated-list-chg dl)
	    (dated-list-misc dl)))
  (let ((*print-case* :upcase))
    (format stream (case depth (1 "~:d ~a [~a] records [~a -- ~a]")
			 (t "~:d~* [~a] [~a -- ~a]"))
	    (dl-len dl) (dated-list-name dl) (dated-list-code dl)
	    (dl-beg-date dl) (dl-end-date dl))))

(defsubst shift-dl (dl &optional dt)
  "Make DL start from DT. Return the new DL.
If dt is omitted, skip one record."
  (declare (type dated-list dl))
  (setf (dated-list-ll dl)
	  (if dt
	      (member dt (dated-list-ll dl) :key (dated-list-date dl)
		      :test-not #'date-more-p)
	      (cdr (dated-list-ll dl))))
  dl)

(defun dl-copy-shift (dl &key shift date)
  "Copy the dated list DL and shift it to start from DATE,
or from +SHIFT from the beginning or -SHIFT from the end,
whichever is positive."
  (declare (type dated-list dl))
  (let ((cdl (copy-dated-list dl)))
    (cond ((and shift (null date))
	   (setf (dated-list-ll cdl)
		 (if (minusp shift)
		     (last (dated-list-ll dl) (- shift))
		     (nthcdr shift (dated-list-ll dl)))))
	  ((and date (null shift))
	   (setf (dated-list-ll cdl)
		 (member (if (date-p date) date (parse-date date))
			 (dated-list-ll dl) :test #'date-less-p
			 :key (dated-list-date dl))))
	  (t (error "One and only one of SHIFT (~a) and DATE (~a) ~
must be non-nil." shift date)))
    cdl))

(defsubst dl-next-chg (dl)
  "Shift dl to the next date, return the change in val.
Can be used with chain contracts, where there are double records for
rollover dates."
  (declare (type dated-list dl))
  (let ((ll (dated-list-ll dl)))
    (unless (cdr ll) (return-from dl-next-chg nil))
    (cond ((equalp (funcall (dated-list-date dl) (first ll))
		   (funcall (dated-list-date dl) (second ll)))
	   (unless (cddr ll) (return-from dl-next-chg nil))
	   (setf (dated-list-ll dl) (cddr ll))
	   (- (funcall (dated-list-val dl) (third ll))
	      (funcall (dated-list-val dl) (second ll))))
	  (t (setf (dated-list-ll dl) (cdr ll))
	     (- (funcall (dated-list-val dl) (second ll))
		(funcall (dated-list-val dl) (first ll)))))))

(defun dl-jumps (dl)
  "Return a cons of 2 lists - the up and down moves in the dated list."
  (declare (type dated-list dl))
  (do (up dn (dd (copy-dated-list dl)) ch)
      ((null (setq ch (dl-next-chg dd))) (cons (nreverse up) (nreverse dn)))
    (cond ((plusp ch) (push (cons ch (dl-beg-date dd)) up))
	  ((minusp ch) (push (cons ch (dl-beg-date dd)) dn)))))

(defun dl-jumps-ui (dl &optional (out t))
  "Print information about the jumps of the dated list."
  (declare (type dated-list dl))
  (let ((jj (dl-jumps dl)))
    (multiple-value-bind (me nu) (mean (car jj) :key #'car)
      (format out "Up   [~:d]: mean: ~7,3f; standard deviation: ~7,3f~%"
	      nu me (standard-deviation (car jj) :len nu :mean me :key #'car)))
    (top-bottom-ui (car jj) 5 5 t :stream out :key #'car :label #'cdr)
    (multiple-value-bind (me nu) (mean (cdr jj) :key #'car)
      (format out "Down [~:d]: mean: ~7,3f; standard deviation: ~7,3f~%"
	      nu me (standard-deviation (cdr jj) :len nu :mean me :key #'car)))
    (top-bottom-ui (cdr jj) 5 5 t :stream out :key #'car :label #'cdr)))

(defsubst skip-dl-to-date (dl dt &optional stlog)
  "Skip (shift) the dated list DL to the date DT.
Signal error if DL ends before DT.
Print messages for: missing DT in DL and double date in DL.
In the latter case it is assumed that this is a contract switch, the
value difference for this date is ignored, and a message is printed to
STLOG if that is not nil.
Return: the change in misc."
  (declare (type dated-list dl) (type date dt))
  (do* ((dl-t (dated-list-ll dl) (cdr dl-t)) (dr 0)
	(dlr1 (first dl-t) dlr2) (dlr2 (second dl-t) (second dl-t))
	(dld (dl-beg-date dl) (if dlr1 (funcall (dated-list-date dl) dlr1)))
	(rr (dl-beg-misc dl)))
       ((not (date-less-p dld dt))
	(when (date-less-p dt dld)	; next dx is later than fx
	  (format t "Missing ~a data for: ~s~%" (dated-list-name dl) dt))
	(setf (dated-list-ll dl) dl-t)
	(+ dr (- (funcall (dated-list-misc dl) dlr1) rr)))
    (unless dl-t (error "~a ended before ~a~%" (dated-list-name dl) dt))
    (when (and dlr2 (equalp dld (funcall (dated-list-date dl) dlr2)))
      (when stlog
	(format stlog " ---> new ~a contract~%" (dated-list-name dl)))
      (format t "New ~a contract started ~s~%" (dated-list-name dl) dld)
      (incf dr (- (funcall (dated-list-misc dl) dlr1)
		  (funcall (dated-list-misc dl) dlr2))))))

(defun skip-dl-to-extremum (dl)
  "Skip (shift) the dated list to the next extremum.
Return nil if at the end already, or the change in value."
  (declare (type dated-list dl))
  (unless (cdr (dated-list-ll dl)) (return-from skip-dl-to-extremum nil))
  (do ((ll (dated-list-ll dl) (dated-list-ll dl)) ch (mv 0.0d0))
      ((null (setq ch (dl-next-chg dl))) mv)
    (cond ((minusp (* ch mv)) (setf (dated-list-ll dl) ll)
	   (return-from skip-dl-to-extremum mv))
	  (t (incf mv ch)))))

(defun print-dated-lists (begd endd &rest dls)
  "Print the dated lists from BEGD to ENDD, inclusive."
  (setq begd (if (date-p begd) begd (parse-date begd))
	endd (if (date-p endd) endd (parse-date endd)))
  (unless dls (error "nothing to print for ~a -- ~a~%" begd endd))
  (with-output-to-printer (prn)
    (dolist (dl dls)
      (format prn "~a [~a -- ~a]~%" (dated-list-name dl) begd endd)
      (do ((td (shift-dl (copy-dated-list dl) begd) (shift-dl td)))
	  ((date-more-p (dl-beg-date td) endd) (format prn "~%"))
	(format prn "~a~%" (dl-beg td))))))

(defun exp-mov-avg (coeff seq &rest args &key (key #'identity) date
		    &allow-other-keys)
  "Return the dated list of the exponential moving averages with
the given coefficient for the given sequence. If :date is not given,
no dated list is created and just the list of numbers is returned."
  (declare (sequence seq) (function key) (float coeff))
  (let* ((ema (funcall key (elt seq 0)))
	 (ll
	  (map 'list
	       (if date
		   (lambda (el) (cons (setq ema (+ (* coeff (funcall key el))
						  (* (- 1 coeff) ema)))
				      (funcall date el)))
		   (lambda (el) (setq ema (+ (* coeff (funcall key el))
					     (* (- 1 coeff) ema)))))
	       seq)))
    (cond (date
	   (remf args :key) (remf args :date)
	   (apply #'make-dated-list :ll ll :date #'cdr :val #'car args))
	  (t ll))))

;;;
;;; Change
;;;

(defstruct (change (:print-function print-change))
  "Change structure - for computing difference dereivatives."
  (date (make-date) :type date)
  (val 0.0d0 :type float)		; value
  (chf 0.0d0 :type float)		; change forward
  (chb 0.0d0 :type float))		; change backward

(defsubst change-max-p (chg)
  "Is this a local maximum?"
  (declare (type change chg))
  (not (or (plusp (change-chf chg)) (minusp (change-chb chg)))))

(defsubst change-min-p (chg)
  "Is this a local minimum?"
  (declare (type change chg))
  (not (or (minusp (change-chf chg)) (plusp (change-chb chg)))))

(defsubst change-type (chg)
  "Return :min or :max depending on whether chg is a min or an max."
  (declare (type change chg))
  (if (change-max-p chg) :max :min))

(defsubst same-type-p (ch1 ch2)
  "Are these two of the same type (min/max)?"
  (declare (type change ch1 ch2))
  (eq (change-type ch1) (change-type ch2)))

(defun print-change (chg &optional stream depth)
  "Print the change structure."
  (declare (type change chg))
  (let ((str (or stream (make-string-output-stream))))
    (print-date (change-date chg) str depth)
    (format str (case depth ((1 2 3) " [~7,3f <- ~8,3f -> ~7,3f]")
		      (t "~* ~8,3f"))
	    (change-chb chg) (change-val chg) (change-chf chg))
    (unless stream (get-output-stream-string str))))

(defun change-list-to-dated-list (chl &rest args)
  "Make a dated list containg this change list."
  (apply #'make-dated-list :ll chl :date #'change-date :val #'change-val
	 :chg #'change-chf :misc #'change-chb args))

(defun dl-extrema (dl)
  "Return a dated list of changes, each recording a local extremum.
DL may contain double records (for chain contracts), in which case
the difference between next values can differ from the corresponding
ch[bf], and dl-extrema will not be idempotent."
  (declare (type dated-list dl))
  (do ((dd (copy-dated-list dl)) (pd nil (dl-beg-date dd)) res ch
       (chg (make-change :date (dl-beg-date dl) :val (dl-beg-val dl))))
      ((null (setq ch (skip-dl-to-extremum dd)))
       (change-list-to-dated-list
	(nreverse (push chg res)) :code
	(intern (concatenate 'string (string (dated-list-code dl)) "-XTR"))
	 :name (format nil "Extrema of `~a'" (dated-list-name dl))))
    (setf (change-chf chg) ch) (push chg res)
    (setq chg
	  (make-change :date (dl-beg-date dd) :val (dl-beg-val dd) :chb ch))))

;;;
;;; Diff
;;;

(defstruct (diff (:print-function print-diff))
  "A dated diff."
  (date (make-date) :type date)
  (di 0.0 :type real)			; difference
  (ra 1.0 :type real))			; ratio

(defun diff-list-to-dated-list (dl &rest args)
  "Wrap a list of diff's into a dated-list.
Sets ll, date, val, and passes the rest directly to make-dated-list."
  (apply #'make-dated-list :ll dl :date #'diff-date :val #'diff-di args))

(defun print-diff (df &optional stream depth)
  "Print the diff record."
  (declare (type diff df))
  (let ((str (or stream (make-string-output-stream))))
    (print-date (diff-date df) str depth)
    (format str " ~15,6f ~15,6f" (diff-di df) (diff-ra df))
    (unless stream (get-output-stream-string str))))

(defun diff-lists (ls0 ls1 &key date0 date1 val0 val1)
  "Generate a list of diff's from the given 2 lists.
For each pair of records in 2 lists that have the same dates
a diff structure is created with the same date and the difference
and the ratio of the values.
The date is accessed by (funcall date* rec),
the value by (funcall val* rec)."
  (do* ((bd (latest-date (funcall date0 (car ls0))
			 (funcall date1 (car ls1)))) ll c0 c1 d0 d1 cd
	(pd nil cd)			; prev date
	(l0 (member bd ls0 :key date0 :test-not #'date-more-p) (cdr l0))
	(l1 (member bd ls1 :key date1 :test-not #'date-more-p) (cdr l1)))
       ((or (null l0) (null l1)) (nreverse ll))
    (setq c0 (car l0) d0 (funcall date0 c0)
	  c1 (car l1) d1 (funcall date1 c1)
	  cd (latest-date d0 d1))
    (cond ((date-less-p d0 cd)
	   (if (equalp pd d0)
	       (format t " -> Double  record in the 1st list on ~a~%" pd)
	       (format t " -> Missing record in the 2nd list on ~a~%" d0))
	   (setq l0 (cdr l0) c0 (car l0)))
	  ((date-less-p d1 cd)
	   (if (equalp pd d1)
	       (format t " -> Double  record in the 2nd list on ~a~%" pd)
	       (format t " -> Missing record in the 1st list on ~a~%" d1))
	   (setq l1 (cdr l1) c1 (car l1))))
    (push (make-diff :date cd :di (- (funcall val0 c0) (funcall val1 c1))
		     :ra (safe-/ (funcall val0 c0) (funcall val1 c1))) ll)))

;;; date.lisp ends here
