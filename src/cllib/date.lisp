;;; File: <date.lisp - 1998-04-03 Fri 13:40:33 EST sds@mute.eaglets.com>
;;;
;;; Date-related structures
;;;
;;; Copyright (C) 1997 by Sam Steingold.
;;; This is free software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: date.lisp,v 1.8 1998/04/03 18:41:12 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/date.lisp,v $
;;; $Log: date.lisp,v $
;;; Revision 1.8  1998/04/03 18:41:12  sds
;;; Added DD (days since epoch) to the date structure.
;;; Ditched *century*.
;;;
;;; Revision 1.7  1998/03/23 15:52:12  sds
;;; Fixed to work with ACL and CMU CL.
;;;
;;; Revision 1.6  1998/03/10 18:30:07  sds
;;; Replaced `multiple-value-set*' with `(setf (values ))'.
;;;
;;; Revision 1.5  1998/02/12 21:43:27  sds
;;; Switched to `defgeneric' and `require'.
;;;
;;; Revision 1.4  1998/01/14 22:17:46  sds
;;; Portability: runs under CMU CL and ACL.
;;;
;;; Revision 1.3  1997/12/04 20:02:53  sds
;;; Moved `channel' to channel.lisp.
;;; Made printing respect *print-readably*.
;;;
;;; Revision 1.2  1997/11/12 22:26:47  sds
;;; Added `regress-dl'.
;;;

(in-package "CL-USER")

(eval-when (load compile eval)
  (sds-require "base") (sds-require "print") (sds-require "math")
  (sds-require "list") (sds-require "util"))

(defconst *day-sec* fixnum (* 24 60 60) "The number of seconds per day.")

;;;
;;; Date
;;;

(eval-when (load compile eval)
(defstruct (date (:print-function print-date))
  "The date structure -- year, month, and day."
  (ye 1 :type fixnum)
  (mo 1 :type (integer 1 12))
  (da 1 :type (integer 1 31))
  (dd nil :type (or null fixnum))) ; number of days since the epoch (1900-1-1)

(defconst *bad-date* date (make-date) "*The convenient constant for init.")

(defun print-date (dt &optional (str t) (depth 1))
  "Print the date to the STREAM, using `*print-date-format*'."
  (declare (type date dt))	; "#S(DATE :YE ~d :MO ~d :DA ~d)"
  (if *print-readably* (funcall (print-readably date) dt str depth)
      (format str "~4,'0d-~2,'0d-~2,'0d"
	      (date-ye dt) (date-mo dt) (date-da dt))))
;; "~:[~; [~:d]~]" (zerop depth) (date-dd dt)
)

(defun print-date-month (dt &optional (str t) (depth 1))
  "Print the date to the STREAM, month and year only."
  (declare (ignore depth) (type date dt))
  (format str "~a ~d" (aref *month-names* (1- (date-mo dt))) (date-ye dt)))

;;; converters

(defsubst date2num (dt)
  "Convert the date to the numerical format YYYYMMDD."
  (declare (type date dt) (values fixnum))
  (+ (* 10000 (date-ye dt)) (* 100 (date-mo dt)) (date-da dt)))

(defsubst date2time (dt)
  "Call `encode-universal-time' on the date.
Returns the number of seconds since the epoch (1900-01-01)."
  (declare (type date dt) (values integer))
  (encode-universal-time 0 0 0 (date-da dt) (date-mo dt) (date-ye dt)))

(defun time2date (num)
  "Convert the universal time NUM to date."
  (declare (real num) (values date))
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time num)
    (declare (ignore se mi ho))
    (make-date :ye ye :mo mo :da da :dd (floor num *day-sec*))))

(defsubst fix-date (dt)
  "Make sure the date is correct."
  (declare (type date dt) (values date))
  (let ((tm (date2time dt)))
    (declare (integer tm))
    (setf (date-dd dt) (floor tm *day-sec*)
	  (date-ye dt) (nth-value 5 (decode-universal-time tm))))
  dt)

(defsubst mk-date (&key (ye 1) (mo 1) (da 1))
  "Make date, fixing everything."
  (declare (fixnum ye mo da) (values date))
  (fix-date (make-date :ye ye :mo mo :da da)))

(defsubst date2days (dt)
  "Convert the date to the number of days since the epoch (1900-01-01)."
  (declare (type date dt) (values fixnum))
  (or (date-dd dt) (setf (date-dd dt) (floor (date2time dt) *day-sec*))))

(defsubst days2date (days)
  "Convert the number of days since the epoch (1900-01-01) to the date."
  (declare (fixnum days) (values date)) (time2date (* (1+ days) *day-sec*)))

(defun num2date (num)
  "Get the date from the number."
  (declare (number num) (values date))
  (mk-date :ye (floor (/ num 10000)) :mo (mod (floor (/ num 100)) 100)
	   :da (mod num 100)))

(defun infer-month (mon)
  "Get the month from the object, number or name."
  (if (numberp mon) mon
      (1+ (position mon *month-names* :test
		    (lambda (s0 s1)
		      (string-equal s0 s1 :start1 0 :end1 2
				    :start2 0 :end2 2))))))

;;;
;;; generic
;;;

(eval-when (load compile eval)
  (defgeneric date (xx) (:documentation "Convert to or extract a date."))
  (declaim (ftype (function (t) date) date)))
(defmethod date ((xx date)) xx)
(defmethod date ((xx string))
  "Get the date out of a string `1969-12-7'."
  (let ((str (nsubstitute #\space #\- xx)))
    (multiple-value-bind (ye n0) (read-from-string str)
      (multiple-value-bind (mo n1) (read-from-string str nil 0 :start n0)
	(mk-date :ye (or ye 0) :mo mo :da
		 (read-from-string str nil 0 :start n1))))))
(defmethod date ((xx null)) *bad-date*) ; (error "Cannot convert NIL to date")
(defmethod date ((xx symbol)) (date (string xx)))
(defmethod date ((xx real)) (time2date xx))
(defmethod date ((xx stream))
  "Read date in format MONTH DAY YEAR."
  (mk-date :mo (infer-month (read xx)) :da (read xx) :ye (read xx)))
(defmethod date ((xx cons)) (date (car xx)))

;;;
;;; utilities
;;;

(defun days-since (key beg)
  "Return a function that will return the number of days between BEG
and (funcall KEY arg), as a fixnum. KEY should return a date."
  (declare (type (function (t) date) key) (values (function (t) fixnum)))
  (unless (realp beg) (setq beg (date2days (date beg))))
  (lambda (rr) (- (date2days (funcall key rr)) beg)))

(defun days-since-f (key beg)
  "Return a function that will return the number of days between BEG
and (funcall KEY arg), as a double-float. KEY should return a date."
  (declare (type (function (t) date) key) (values (function (t) double-float)))
  (unless (realp beg) (setq beg (date2days (date beg))))
  (lambda (rr) (double-float (- (date2days (funcall key rr)) beg))))

(defun today ()
  "Return today's date."
  (declare (values date)) (time2date (get-universal-time)))

(defun timestamp (&optional (time (get-universal-time)))
  "Return the current time as a string without blanks."
  (declare (integer time) (values simple-string))
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d--~2,'0d-~2,'0d-~2,'0d"
	    ye mo da ho mi se)))

(defun date= (d0 d1)
  "Check that the dates are the same. Like equalp, but works with children."
  (declare (type date d0 d1)) (= (date2days d0) (date2days d1)))
;; (and (= (date-ye d0) (date-ye d1))
;;     (= (date-mo d0) (date-mo d1))
;;     (= (date-da d0) (date-da d1))))

(defun date< (d0 d1)
  "Check the precedence of the two dates."
  (declare (type date d0 d1)) (< (date2days d0) (date2days d1)))
;;  (let ((dd (- (date-ye d0) (date-ye d1))))
;;    (cond ((plusp dd) nil) ((minusp dd) t)
;;	  ((setq dd (- (date-mo d0) (date-mo d1)))
;;	   (cond ((plusp dd) nil) ((minusp dd) t)
;;		 ((minusp (- (date-da d0) (date-da d1)))))))))

(defun date> (d0 d1)
  "Check the precedence of the two dates."
  (declare (type date d0 d1)) (> (date2days d0) (date2days d1)))
;;  (let ((dd (- (date-ye d0) (date-ye d1))))
;;    (cond ((plusp dd) t) ((minusp dd) nil)
;;	  ((setq dd (- (date-mo d0) (date-mo d1)))
;;	   (cond ((plusp dd) t) ((minusp dd) nil)
;;		 ((plusp (- (date-da d0) (date-da d1)))))))))

(defsubst latest-date (d0 d1)
  "Return the latest date."
  (declare (type date d0 d1) (values date)) (if (date< d0 d1) d1 d0))

(defsubst earliest-date (d0 d1)
  "Return the earliest date."
  (declare (type date d0 d1) (values date)) (if (date> d0 d1) d1 d0))

(defun next-month-p (d0 d1)
  "True if D1 is the next month of D0."
  (declare (type date d0 d1))
  (let ((y0 (date-ye d0)) (y1 (date-ye d1))
	(m0 (date-mo d0)) (m1 (date-mo d1)))
    (or (and (= (1+ m0) m1) (= y0 y1))
	(and (= m0 12) (= m1 1) (= (1+ y0) y1)))))

(defun same-month-p (d0 d1)
  "Return t if the dates are in the same month."
  (declare (type date d0 d1))
  (and (= (date-ye d0) (date-ye d1)) (= (date-mo d0) (date-mo d1))))

(defun next-quarter-p (d0 d1)
  "True if D1 is the next quarter of D0."
  (declare (type date d0 d1))
  (let ((y0 (date-ye d0)) (y1 (date-ye d1))
	(m0 (date-mo d0)) (m1 (date-mo d1)))
    (or (and (= m1 1) (= (1+ y0) y1) (> m0 9))
	(and (= y0 y1) (< m0 m1) (> (+ 4 m0) m1) (member m1 '(1 4 7 10))))))

(defun date-quarter (dt)
  "Return the quarter of the date."
  (declare (type date dt))
  (1+ (floor (1- (date-mo dt)) 3)))

(defun same-quarter-p (d0 d1)
  "Return t if the dates are in the same quarter."
  (declare (type date d0 d1))
  (and (= (date-ye d0) (date-ye d1))
       (= (date-quarter d0) (date-quarter d1))))

(defun days-between (d0 &optional (d1 (today)))
  "Return the number of days between the two dates.
Arguments can be dates or objects parsable with `date'."
  (- (date2days (date d1)) (date2days (date d0))))

(defsubst tomorrow (dd &optional (skip 1))
  "Return the next day.
With the optional second argument (defaults to 1) skip as many days.
I.e., (tomorrow (today) -1) is yesterday."
  (declare (type date dd) (fixnum skip) (values date))
  (days2date (+ (date2days dd) skip)))

(defsubst yesterday (dd &optional (skip 1))
  "Return the previous day.  Calls tomorrow."
  (declare (type date dd) (fixnum skip) (values date)) (tomorrow dd (- skip)))

(defun date-in-list (dt lst &optional (key #'date) last)
  "Return the tail of LST starting with DT.
If LAST is non-nil, make sure that the next date is different.
*Important*: assumes that the list is ordered according to `date>'."
  (declare (list lst) (type (function (t) date) key))
  (let ((ll (binary-member (date dt) lst :key key :test (complement #'date>))))
    (if (and last ll (cdr ll)) (skip-to-new ll :key key :test #'date=) ll)))

(defun check-dates (lst order-p same-p jump gap &key (date #'date)
		    (val #'value) (stream *standard-output*))
  "Check the dated list LST for order violations, redundancies, large
data changes and gaps. The accessors DATE and VAL default to CAR and
CDR respectively and may be omitted if LST is a dated list.
If you need to check more than one value for jumps, you can resort
to `check-list-values'.
Return T if any errors are detected."
  (declare (type (function (t) date) date) (stream stream)
	   (type (or null fixnum) gap jump)
	   (type (function (t) double-float) val))
  (format stream "~&Checking the list~:[~; `~:*~a'~] for:~%~5t~?.~%"
	  (if (dated-list-p lst) (dated-list-name lst) nil)
	  (list-format "~{~a~}")
	  (nconc (if order-p (list (list "order")) nil)
		 (if same-p (list (list "redundancies")) nil)
		 (if jump (list (list "jumps of at least " jump)) nil)
		 (if gap (list (list "gaps of at least " gap)) nil)))
  (when (dated-list-p lst)
    (setq date (dl-date lst)
	  val (dl-val lst)
	  lst (dated-list-ll lst)))
  (do ((rr lst (cdr rr)) (len 1 (1+ len)) ; start from 1 (sic!)
       (r0 (first lst) r1) r1 (err 0)
       (v0 (if val (funcall val (first lst))) v1) v1
       (d0 (funcall date (first lst)) d1) d1)
      ((null (cdr rr))
       (format t "~:d record~:p, ~a through ~a. ~d error~:p found.~%"
	       len (funcall date (first lst)) d1 err) (zerop err))
    (setq r1 (second rr) d1 (funcall date r1) v1 (if val (funcall val r1)))
    (when (and order-p (date< d1 d0))
      (format stream "~3d. Wrong Order:~% - ~s~% - ~s~%~%" (incf err) r0 r1))
    (when (and same-p (date= d0 d1))
      (format stream "~3d. Same Date:~% - ~s~% - ~s~%~%" (incf err) r0 r1))
    (when (and jump (> (rel-diff v0 v1) jump))
      (format stream "~3d. Large Jump:~% - ~s~% - ~s~%~%" (incf err) r0 r1))
    (when (and gap (> (days-between d0 d1) gap))
      (format stream "~3d. Large Gap:~% - ~s~% - ~s~%~%" (incf err) r0 r1))))

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
  (mesg stream "Synching dates in ~d lists.~%" (length lists))
  (do ((sec (copy-list lists)) (heads (make-list (length lists)))
       bd fnn (err nil nil) ckey (nerr 0) (len 0 (1+ len)))
      ((every #'null sec)
       (mesg stream "~:d records in ~d lists checked. ~d error~:p found.~%"
	     len (length lists) nerr))
    ;; get the current date
    (setq fnn (member nil sec :test-not #'eq)
	  bd (funcall key (caar fnn)))
    (dolist (ls (rest fnn))
      (when ls (setq ckey (funcall key (car ls)))
	    (when (date> bd ckey) (setq bd ckey))
	    (unless (date= bd ckey) (setq err t))))
    ;; handle date mismatches
    (when (and stream lables err)
      (format stream " --[~:d]--> date mismatch:~:{~%  <~a: ~a>~}~%"
	      len (mapcar #'cons lables sec))
      (incf nerr))
    ;; shift, fix and operate
    (mapl (lambda (ls hd)
	    (if (date= bd (funcall key (caar ls)))
		(setf (car hd) (caar ls))
		(let ((nr (funcall cpy (caar ls))))
		  (funcall set (caar ls) bd)
		  (setf (cdar ls) (cons nr (cdar ls))
			(car hd) (if skip (caar ls)))))
	    (pop (car ls)))
	  sec heads)
    (when op (apply op heads))))

(defmacro sync-dates-ui (lists &key lables key cpy (stream t) op skip)
  "Use KEY for SET, which should be a slot. See `sync-dates'."
  `(sync-dates ,lists :lables ,lables :key (lambda (rr) (slot-value rr ,key))
    :cpy ,cpy :stream ,stream  :op ,op :skip ,skip
    :set (lambda (rr dd) (setf (slot-value rr ,key) dd))))

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
       begck ck fnn res)
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

(eval-when (load compile eval)
(defstruct (dated-list (:print-function print-dlist))
  "A dated list of records."
  (ll nil :type list)		; the actual list
  (code nil :type symbol)	; the code (2 letter symbol)
  (name "??" :type string)	; the name of the data
  (date 'identity :type		; the date accessor
	(or symbol (function (t) date)))
  (val 'identity :type		; the value accessor
       (or symbol (function (t) double-float)))
  (chg 'identity :type		; the change accessor
       (or symbol (function (t) double-float)))
  (misc 'identity :type (or symbol function))) ; the miscellaneous accessor
)

(defsubst dl-date (dl)
  "Return the DATE function of the dated list DL."
  (declare (type dated-list dl) (values (function (t) date)))
  (let ((ff (dated-list-date dl))) (if (symbolp ff) (symbol-function ff) ff)))

(defsubst dl-val (dl)
  "Return the VAL function of the dated list DL."
  (declare (type dated-list dl) (values (function (t) double-float)))
  (let ((ff (dated-list-val dl))) (if (symbolp ff) (symbol-function ff) ff)))

(defsubst dl-chg (dl)
  "Return the CHG function of the dated list DL."
  (declare (type dated-list dl) (values (function (t) double-float)))
  (let ((ff (dated-list-chg dl))) (if (symbolp ff) (symbol-function ff) ff)))

(defsubst dl-misc (dl)
  "Return the MISC function of the dated list DL."
  (declare (type dated-list dl))
  (let ((ff (dated-list-misc dl))) (if (symbolp ff) (symbol-function ff) ff)))

(defsubst dl-len (dl)
  "Return the length of the dated list."
  (declare (type dated-list dl) (values fixnum)) (length (dated-list-ll dl)))

(defsubst dl-endp (dl)
  "Check for the end of the dated list."
  (declare (type dated-list dl)) (endp (dated-list-ll dl)))

(defsubst dl-code (dl)
  "Get the code of the dated list."
  (if (dated-list-p dl) (dated-list-code dl) dl))

(defun dl-nth (dl &optional (nn 0))
  "Return the Nth record of the dated list.
Optional second arg defaults to 0. If it is negative, count from the end,
so that -1 corresponds to the last record."
  (declare (type dated-list dl) (fixnum nn))
  (if (minusp nn) (car (last (dated-list-ll dl) (- nn)))
      (nth nn (dated-list-ll dl))))

(defun dl-nth-date (dl &optional (nn 0))
  "Return the Nth date of the dated list."
  (declare (type dated-list dl) (fixnum nn))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (dl-date dl) bb))))

(defun dl-nth-val (dl &optional (nn 0))
  "Return the Nth value of the dated list."
  (declare (type dated-list dl) (fixnum nn))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (dl-val dl) bb))))

(defun dl-nth-chg (dl &optional (nn 0))
  "Return the Nth change of the dated list."
  (declare (type dated-list dl) (fixnum nn))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (dl-chg dl) bb))))

(defun dl-nth-misc (dl &optional (nn 0))
  "Return the Nth MISC of the dated list."
  (declare (type dated-list dl) (fixnum nn))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (dl-misc dl) bb))))

(defun dl-nth-slot (dl slot &optional (nn 0))
  "Return the Nth SLOT of the dated list."
  (declare (type dated-list dl) (fixnum nn))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (slot-value dl slot) bb))))

(defun print-dlist (dl &optional (stream t) (depth 1))
  "Print the dated list record."
  (declare (type dated-list dl))
  (when (minusp depth)
    (format t "~%date:~10t~a~%val:~10t~a~%chg:~10t~a~%misc:~10t~a~%"
	    (dated-list-date dl) (dated-list-val dl) (dated-list-chg dl)
	    (dated-list-misc dl)))
  (if *print-readably* (funcall (print-readably dated-list) dl stream depth)
      (let ((*print-case* :upcase))
	(format stream (case depth (1 "~:d ~a [~a] [~a -- ~a]")
			     (t "~:d~* [~a] [~a -- ~a]"))
		(dl-len dl) (dated-list-name dl) (dated-list-code dl)
		(dl-nth-date dl) (dl-nth-date dl -1)))))

(defun date-in-dated-list (dt dl &optional last)
  "Call `date-in-list' on the dated list.
If  LAST is non-nil, make sure that the next date is different."
  (declare (type dated-list dl))
  (if dt (date-in-list dt (dated-list-ll dl) (dl-date dl) last)
      (dated-list-ll dl)))

(defun dl-double-date-p (dt dl)
  "Return T if the date DT is present in DL twice."
  (declare (type dated-list dl))
  (setq dt (date dt))
  (let ((ta (second (date-in-dated-list dt dl))))
    (and ta (date= dt (funcall (dl-date dl) ta)))))

(defun dl-shift (dl &optional dt last)
  "Make DL start from DT. Return the DL.
If dt is omitted, skip one record.
If LAST is non-nil, make sure that the next date is different."
  (declare (type dated-list dl))
  (setf (dated-list-ll dl)
	(if dt (date-in-dated-list dt dl last)
	    (cdr (if last (skip-to-new (dated-list-ll dl) :test #'date=
				       :key (dl-date dl))
		     (dated-list-ll dl)))))
  dl)

(defun dl-copy-shift (dl &key shift date)
  "Copy the dated list DL and shift it to start from DATE,
or from +SHIFT from the beginning or -SHIFT from the end,
whichever is positive.  If both SHIFT and DATE are NIL, shift
to the end (set tge list to NIL)."
  (declare (type dated-list dl))
  (let ((cdl (copy-dated-list dl)))
    (cond ((and shift (null date))
	   (setf (dated-list-ll cdl)
		 (if (minusp shift)
		     (last (dated-list-ll dl) (- shift))
		     (nthcdr shift (dated-list-ll dl)))))
	  ((and date (null shift))
	   (setf (dated-list-ll cdl) (date-in-dated-list date dl)))
	  ((and (null date) (null shift)) (setf (dated-list-ll cdl) nil))
	  (t (error "Cannot specify both SHIFT (~a) and DATE (~a)."
		    shift date)))
    cdl))

(defun dl-next-chg (dl)
  "Shift dl to the next date, return the change in val.
Can be used with chain contracts, where there are double records for
roll-over dates."
  (declare (type dated-list dl))
  (let ((ll (dated-list-ll dl)))
    (unless (cdr ll) (return-from dl-next-chg nil))
    (cond ((date= (funcall (dl-date dl) (first ll))
		  (funcall (dl-date dl) (second ll)))
	   (unless (cddr ll) (return-from dl-next-chg nil))
	   (setf (dated-list-ll dl) (cddr ll))
	   (- (funcall (dl-val dl) (third ll))
	      (funcall (dl-val dl) (second ll))))
	  (t (setf (dated-list-ll dl) (cdr ll))
	     (- (funcall (dl-val dl) (second ll))
		(funcall (dl-val dl) (first ll)))))))

(defun dl-jumps (dl)
  "Return a cons of 2 lists - the up and down moves in the dated list."
  (declare (type dated-list dl))
  (do (up dn (dd (copy-dated-list dl)) ch)
      ((null (setq ch (dl-next-chg dd))) (cons (nreverse up) (nreverse dn)))
    (cond ((plusp ch) (push (cons ch (dl-nth-date dd)) up))
	  ((minusp ch) (push (cons ch (dl-nth-date dd)) dn)))))

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

(defun skip-dl-to-date (dl dt &optional stlog)
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
	(dld (dl-nth-date dl) (if dlr1 (funcall (dl-date dl) dlr1)))
	(rr (dl-nth-misc dl)))
       ((not (date< dld dt))
	(when (date< dt dld)	; next dx is later than fx
	  (format t "Missing ~a data for: ~s~%" (dated-list-name dl) dt))
	(setf (dated-list-ll dl) dl-t)
	(+ dr (- (funcall (dl-misc dl) dlr1) rr)))
    (unless dl-t (error "~a ended before ~a~%" (dated-list-name dl) dt))
    (when (and dlr2 (date= dld (funcall (dl-date dl) dlr2)))
      (mesg stlog " ---> new ~a contract~%" (dated-list-name dl))
      (format t "New ~a contract started ~s~%" (dated-list-name dl) dld)
      (incf dr (- (funcall (dl-misc dl) dlr1) (funcall (dl-misc dl) dlr2))))))

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
  (setq begd (date begd) endd (date endd))
  (unless dls (error "nothing to print for ~a -- ~a~%" begd endd))
  (with-printing (prn)
    (dolist (dl dls)
      (format prn "~a [~a -- ~a]~%" (dated-list-name dl) begd endd)
      (do ((td (dl-shift (copy-dated-list dl) begd) (dl-shift td)))
	  ((date> (dl-nth-date td) endd) (format prn "~%"))
	(format prn "~a~%" (dl-nth td))))))

(defsubst volatility-dl (dl &key (split #'date-ye) (slot 'val))
  "Apply `volatility' to the dated list.
Key defaults to VAL; split defaults to `date-ye'."
  (volatility (dated-list-ll dl) (compose split (dl-date dl))
	      :key (slot-value dl slot)))

(defun print-volatilities (ls &key (out t) (dl #'identity) (head #'identity))
  "Print the annual and monthly volatilities of the objects in the list.
DL is the dated list accessor; HEAD is ihe header for the object,
it should return a short symbol or string."
  (declare (list ls) (function dl head))
  (format out "~&~70,,,'_:@< Volatilities ~>~%Name     Monthly Annual :")
  (let ((py t))
    (dolist (ob ls)
      (let ((dl (funcall dl ob)) (he (funcall head ob)) mv lv av)
	(setf (values lv av) (volatility-dl dl :split #'date-ye)
	      mv (nth-value 1 (volatility-dl dl :split #'date-mo)))
	(when py (setq py nil)
	      (dolist (ye lv (terpri out)) (format out "  ~d" (car ye))))
	(format out "~a~10t~6,4f ~6,4f :" he mv av)
	(dolist (yv lv) (format out " ~5,3f" (cdr yv)))
	(terpri out)))))

(defun exp-mov-avg (coeff seq &rest args &key (key #'value) date
		    &allow-other-keys)
  "Return the list of the exponential moving averages with the given
coefficient for the given sequence. If :date is not given, no dated
list object is created and just the list of numbers is returned."
  (declare (sequence seq) (double-float coeff)
	   (type (function (t) double-float) key))
  (let* ((ema (funcall key (elt seq 0)))
	 (ll
	  (map 'list
	       (if date
		   (lambda (el) (cons (funcall date el)
				      (setq ema (+ (* coeff (funcall key el))
						   (* (- 1 coeff) ema)))))
		   (lambda (el) (setq ema (+ (* coeff (funcall key el))
					     (* (- 1 coeff) ema)))))
	       seq)))
    (cond (date
	   (remf args :key) (remf args :date)
	   (apply #'make-dated-list :ll ll :date 'car :val 'cdr args))
	  (t ll))))

(defun exp-mov-avg-dl (coeff dl &optional (slot 'val))
  "UI for `exp-mov-avg' when the argument is a dated list itself."
  (exp-mov-avg coeff (dated-list-ll dl) :date (dl-date dl)
	       :key (slot-value dl slot) :code
	       (intern (concatenate 'string (string (dated-list-code dl))
				    "-EMA"))
	       :name
	       (format nil "EMA [~5,3f] `~a'" coeff (dated-list-name dl))))

(defun regress-dl (dl &optional begd endd)
  "Regress the dated list in the given interval.
When a boundary is omitted, the end (or the beginning) is used.
Return the line object and the deviation therefrom.
Must not assume that the list is properly ordered!"
  (declare (type dated-list dl))
  (setq begd (if begd (date begd)) endd (if endd (date endd)))
  (with-sublist (ll (dated-list-ll dl) begd endd :key (dl-date dl)
		 :test #'date=)
    (regress ll :ykey (dl-val dl)
	     :xkey (days-since-f (dl-date dl)
				 (earliest-date (funcall (dl-date dl) (car ll))
						(funcall (dl-date dl)
							 (car (last ll))))))))

(defsubst mean-dl (dl &key (slot 'val))
  "Apply `mean' to the dated list."
  (declare (type dated-list dl))
  (mean (dated-list-ll dl) :key (slot-value dl slot)))

(defun standard-deviation-dl (dl &rest opts &key (slot 'val)
			      &allow-other-keys)
  "Apply `standard-deviation' to the dated list."
  (declare (type dated-list dl))
  (remf opts :slot)
  (apply #'standard-deviation (dated-list-ll dl) :key (slot-value dl slot)
	 opts))

(defsubst standard-deviation-relative-dl (dl &key (slot 'val))
  "Apply `standard-deviation' to the dated list."
  (declare (type dated-list dl))
  (standard-deviation-relative (dated-list-ll dl) :key (slot-value dl slot)))

(defsubst weighted-mean-dl (dl wts &key (slot 'val))
  "Apply `weighted-mean' to the dated list."
  (declare (type dated-list dl))
  (weighted-mean (dated-list-ll dl) wts :key (slot-value dl slot)))

(defmacro with-truncated-dl ((dt dl) &body body)
  "Evaluate BODY when DL is truncated by the date DT."
  (let ((ll (gensym "WTD")) (tt (gensym "WTD")))
    `(let* ((,ll (date-in-dated-list ,dt ,dl)) (,tt (cdr ,ll)))
      (unwind-protect (progn (setf (cdr ,ll) nil) ,@body)
	(setf (cdr ,ll) ,tt)))))
;;;
;;; Change
;;;

(eval-when (load compile eval)
(defstruct (change (:print-function print-change))
  "Change structure - for computing difference derivatives."
  (date *bad-date* :type date)
  (val 0.0 :type double-float)	; value
  (chf 0.0 :type double-float)	; change forward
  (chb 0.0 :type double-float)) ; change backward
)

(defmethod date ((xx change)) (change-date xx))
(defmethod value ((xx change)) (change-val xx))

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

(defun print-change (chg &optional (stream t) (depth 1))
  "Print the change structure."
  (declare (type change chg))
  (if *print-readably* (funcall (print-readably change) chg stream depth)
      (let ((str (or stream (make-string-output-stream))))
	(print-date (change-date chg) str depth)
	(format str (case depth ((1 2 3) " [~7,3f <- ~8,3f -> ~7,3f]")
			  (t "~* ~8,3f"))
		(change-chb chg) (change-val chg) (change-chf chg))
	(unless stream (get-output-stream-string str)))))

(defun change-list-to-dated-list (chl &rest args)
  "Make a dated list containing this change list."
  (apply #'make-dated-list :ll chl :date 'change-date :val 'change-val
	 :chg 'change-chf :misc 'change-chb args))

(defun dl-extrema (dl)
  "Return a dated list of changes, each recording a local extremum.
DL may contain double records (for chain contracts), in which case
the difference between next values can differ from the corresponding
ch[bf], and dl-extrema will not be idempotent."
  (declare (type dated-list dl))
  (do ((dd (copy-dated-list dl)) res ch
       (chg (make-change :date (dl-nth-date dl) :val (dl-nth-val dl))))
      ((null (setq ch (skip-dl-to-extremum dd)))
       (change-list-to-dated-list
	(nreverse (push chg res)) :code
	(intern (concatenate 'string (string (dated-list-code dl)) "-XTR"))
	 :name (format nil "Extrema of `~a'" (dated-list-name dl))))
    (setf (change-chf chg) ch) (push chg res)
    (setq chg
	  (make-change :date (dl-nth-date dd) :val (dl-nth-val dd) :chb ch))))

;;;
;;; Diff
;;;

(eval-when (load compile eval)
(defstruct (diff (:print-function print-diff))
  "A dated diff."
  (date *bad-date* :type date)
  (di 0.0 :type real)			; difference
  (ra 1.0 :type real))			; ratio
)

(defmethod date ((xx diff)) (diff-date xx))
(defmethod value ((xx diff)) (diff-di xx))

(defun diff-list-to-dated-list (dl &rest args)
  "Wrap a list of diff's into a dated-list.
Sets ll, date, val, and passes the rest directly to make-dated-list."
  (apply #'make-dated-list :ll dl :date 'diff-date :val 'diff-di args))

(defun print-diff (df &optional (stream t) (depth 1))
  "Print the diff record."
  (declare (type diff df))
  (if *print-readably* (funcall (print-readably diff) df stream depth)
      (let ((str (or stream (make-string-output-stream))))
	(print-date (diff-date df) str depth)
	(format str " ~15,6f ~15,6f" (diff-di df) (diff-ra df))
	(unless stream (get-output-stream-string str)))))

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
	(l0 (date-in-list bd ls0 date0) (cdr l0))
	(l1 (date-in-list bd ls1 date1) (cdr l1)))
       ((or (null l0) (null l1)) (nreverse ll))
    (setq c0 (car l0) d0 (funcall date0 c0)
	  c1 (car l1) d1 (funcall date1 c1)
	  cd (latest-date d0 d1))
    (cond ((date< d0 cd)
	   (if (date= pd d0)
	       (format t " -> Double  record in the 1st list on ~a~%" pd)
	       (format t " -> Missing record in the 2nd list on ~a~%" d0))
	   (setq l0 (cdr l0) c0 (car l0)))
	  ((date< d1 cd)
	   (if (date= pd d1)
	       (format t " -> Double  record in the 2nd list on ~a~%" pd)
	       (format t " -> Missing record in the 1st list on ~a~%" d1))
	   (setq l1 (cdr l1) c1 (car l1))))
    (push (make-diff :date cd :di (- (funcall val0 c0) (funcall val1 c1))
		     :ra (s/ (funcall val0 c0) (funcall val1 c1))) ll)))

(provide "date")
;;; date.lisp ends here
