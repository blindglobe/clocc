;;; File: <date.lisp - 1999-02-24 Wed 23:40:12 EST sds@eho.eaglets.com>
;;;
;;; Date-related structures
;;;
;;; Copyright (C) 1997, 1998 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: date.lisp,v 1.30 1999/02/25 04:45:39 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/date.lisp,v $
;;; $Log: date.lisp,v $
;;; Revision 1.30  1999/02/25 04:45:39  sds
;;; Added `date-f-t', `date<=3', `date>=3', `dl-reset', `mk-dl', `rollover'.
;;; Removed `dl-copy-shift'.  Fixed `dl-shift', `dl-overlap'.
;;; Improved `fixing' in `check-dates'.
;;; Added `cp' to `dated-list'.
;;; Added `describe-object' method for `dated-list'.
;;;
;;; Revision 1.29  1999/02/23 23:48:21  sds
;;; Major changes: many lists in a dated list;
;;; `check-dates' can fix some problems.
;;;
;;; Revision 1.28  1999/02/02 00:03:24  sds
;;; Added `to-string', fixed `string->dttm'.
;;;
;;; Revision 1.27  1999/02/01 19:34:04  sds
;;; Added `purge-string' and `*string-junk*'.
;;; Fixed `string->dttm' for the case when the date starts with a weekday.
;;;
;;; Revision 1.26  1999/02/01 18:59:07  sds
;;; Use `string-tokens' in `date'.
;;;
;;; Revision 1.25  1999/01/13 23:37:56  sds
;;; Replaced CMUCL-specific print functions with a call to
;;; `print-struct-object'.
;;;
;;; Revision 1.24  1999/01/08 20:00:39  sds
;;; Replaced calls to `date2days' with calls to `date-dd'.
;;; Speedups expected.
;;;
;;; Revision 1.23  1999/01/07 04:03:05  sds
;;; Use `index-t' instead of (unsigned-byte 20).
;;;
;;; Revision 1.22  1998/12/10 22:40:39  sds
;;; Added `dttm->string', `string->dttm' and `infer-timezone'.
;;; Made `infer-month' return NIL instead of signalling an error when the
;;; month cannot be inferred.
;;;
;;; Revision 1.21  1998/11/13 19:22:39  sds
;;; Added `days-week-day', `date-week-day' and `next-bad-day' for Friday the
;;; 13th handling.
;;;
;;; Revision 1.20  1998/11/05 19:19:24  sds
;;; Added `date<=', `date>=' and `date=*'.
;;;
;;; Revision 1.19  1998/08/05 22:03:28  sds
;;; Added `date-mon-name' and `date-mon-offset'.
;;;
;;; Revision 1.18  1998/08/03 18:37:31  sds
;;; Renamed `earliest-date' and `latest-date' to `date-min' and `date-max'.
;;;
;;; Revision 1.17  1998/07/31 16:43:25  sds
;;; Added `exp-mov-avg-append' and `lincom'.
;;; Declared `stream' as a stream in `print-*'.
;;;
;;; Revision 1.16  1998/07/06 21:58:10  sds
;;; Combined the two &key arguments of `dl-copy-shift' into one optional
;;; argument, branching on its type.
;;;
;;; Revision 1.15  1998/06/26 23:11:30  sds
;;; Added `days-t' and `dl-slot'.  Switched to `print-object'.
;;;
;;; Revision 1.14  1998/06/19 21:41:16  sds
;;; Use `defmethod' to print structures.
;;;
;;; Revision 1.13  1998/06/19 20:18:31  sds
;;; Made `fix-date' not inline.  Added some declarations for CMUCL.
;;;
;;; Revision 1.12  1998/05/27 21:24:20  sds
;;; Moved sorted stuff from here to list.lisp.
;;;
;;; Revision 1.11  1998/05/27 20:42:03  sds
;;; Expanded `date' string recognition to include word months.
;;;
;;; Revision 1.10  1998/04/29 22:41:01  sds
;;; Added (defmethod date ((xx integer)) (days2date xx)).
;;; The new slot DD works fine and all the saved data has correct dates.
;;;
;;; Revision 1.9  1998/04/20 22:53:37  sds
;;; Made the function slots in dated-list only sybols.
;;;
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

(in-package :cl-user)

(eval-when (load compile eval)
  (sds-require "base") (sds-require "print") (sds-require "math")
  (sds-require "list") (sds-require "util")
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

(defconst +day-sec+ fixnum (* 24 60 60) "The number of seconds per day.")

;;;
;;; Date
;;;

(eval-when (load compile eval)
(deftype days-t () '(signed-byte 20))

;; of course, a class would be more appropriate here, especially since
;; this would allow us to use an :after `initialize-instance' method to
;; properly init DD.
;; Unfortunately, this would mean that we will have to define our own
;; print/read procedures.  While the former is doable, the latter would
;; apparently require make-load-form, which is missing from CLISP.

(defstruct (date #+cmu (:print-function print-struct-object))
  "The date structure -- year, month, and day."
  (ye 1 :type days-t)
  (mo 1 :type (integer 1 12))
  (da 1 :type (integer 1 31))
  (dd nil :type (or null days-t))) ; days since the epoch (1900-1-1 == 0)
(deftype date-f-t () '(function (t) date))
)

(defmethod print-object ((dt date) (stream stream))
  (if *print-readably* (call-next-method)
      (format stream "~4,'0d-~2,'0d-~2,'0d" (date-ye dt)
              (date-mo dt) (date-da dt))))

(defconst +bad-date+ date (make-date) "*The convenient constant for init.")

(defun date-mon-name (dt)
  "Return the name of the month."
  (declare (type date dt) (values simple-string))
  (aref +month-names+ (1- (date-mo dt))))

(defun date-mon-offset (dt)
  "Return the number of characters printed for the previous months."
  (declare (type date dt) (values (unsigned-byte 10)))
  (let ((pos (1- (date-mo dt))))
    (reduce #'+ +month-names+ :key #'length :end pos :initial-value pos)))

(defun print-date-month (dt &optional (str t))
  "Print the date to the STREAM, month and year only."
  (declare (type date dt))
  (format str "~a ~d" (date-mon-name dt) (date-ye dt)))

;;; converters

(defsubst date2num (dt)
  "Convert the date to the numerical format YYYYMMDD."
  (declare (type date dt) (values integer))
  (+ (* 10000 (date-ye dt)) (* 100 (date-mo dt)) (date-da dt)))

(defsubst date2time (dt)
  "Call `encode-universal-time' on the date.
Returns the number of seconds since the epoch (1900-01-01)."
  (declare (type date dt) (values integer))
  (encode-universal-time 0 0 0 (date-da dt) (date-mo dt) (date-ye dt) 0))

(defun time2date (num)
  "Convert the universal time (GMT) to date."
  (declare (real num) (values date))
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time num 0)
    (declare (ignore se mi ho))
    (make-date :ye ye :mo mo :da da :dd (floor num +day-sec+))))

(defun fix-date (dt)
  "Make sure the date is correct."
  (declare (type date dt) (values date))
  (let ((tm (date2time dt)))
    (declare (integer tm))
    (setf (date-dd dt) (floor tm +day-sec+)
          (date-ye dt) (nth-value 5 (decode-universal-time tm 0))))
  dt)

(defsubst mk-date (&rest args)
  "Make date, fixing DD and then YE."
  (declare (values date)) (fix-date (apply #'make-date args)))

(defsubst date2days (dt)
  "Convert the date to the number of days since the epoch (1900-01-01)."
  (declare (type date dt) (values days-t))
  (or (date-dd dt)
      (progn (format t "~& *** Fixed date ~a~%" dt)
             (date-dd (fix-date dt)))))

(defsubst days2date (days)
  "Convert the number of days since the epoch (1900-01-01) to the date."
  (declare (type days-t days) (values date)) (time2date (* days +day-sec+)))

(defun num2date (num)
  "Get the date from the number."
  (declare (integer num) (values date))
  (mk-date :ye (floor num 10000) :mo (mod (floor num 100) 100)
           :da (mod num 100)))

(defun to-string (obj)
  "Unintern if symbol and return string."
  (declare (type (or symbol string) obj))
  (etypecase obj (string obj) (symbol (unintern obj) (symbol-name obj))))

(defun infer-timezone (obj)
  "Guess the timezone."
  (etypecase obj
    (symbol (unintern obj) (infer-timezone (symbol-name obj)))
    (string
     (or (car (find obj +time-zones+ :test
                    (lambda (st el) (or (string-equal st (cadr el))
                                        (string-equal st (cddr el))))))
         0))
    (number
     (cond ((< -24 obj 24) obj)
           ((multiple-value-bind (ho mi) (floor obj 100)
              (+ ho (/ mi 60))))))))

(defun dttm->string (dttm &optional old)
  "Print the date/time as returned by `encode-universal-time'."
  (declare (type (integer 0) dttm))
  (multiple-value-bind (se mi ho da mo ye dd) (decode-universal-time dttm 0)
    (if old
        (format nil "~a, ~d ~a ~d ~2,'0d:~2,'0d:~2,'0d GMT"
                (aref +week-days+ dd) da (aref +month-names+ (1- mo))
                ye ho mi se)
        (format nil "~d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d GMT"
                ye mo da (aref +week-days+ dd) ho mi se))))

(defun string->dttm (xx)
  "Parse the string into a date/time integer."
  (declare (simple-string xx))
  (multiple-value-bind (v0 v1 v2 v3 v4 v5 v6)
      (values-list
       (delete-if (lambda (st)
                    (and (symbolp st)
                         (find (to-string st) +week-days+ :test
                               #'string-equal)))
                  (string-tokens (purge-string xx) :max 8)))
    (if (numberp v0)
        (encode-universal-time (or v5 0) (or v4 0) (or v3 0)
                               (min v0 v2) (infer-month v1) (max v0 v2)
                               (infer-timezone v6))
        (encode-universal-time (or v4 0) (or v3 0) (or v2 0)
                               v1 (infer-month v0) v5
                               (infer-timezone v6)))))

(defun infer-month (mon)
  "Get the month from the object, number or name."
  (if (numberp mon) mon
      (let ((pos (position (to-string mon) +month-names+ :test
                           (lambda (s0 s1)
                             (string-equal s0 s1 :start1 0 :end1 3
                                           :start2 0 :end2 3)))))
        (when pos (1+ pos)))))

(defun days-week-day (days)
  "Return the week day of the date DAYS from the epoch."
  (declare (type days-t days) (values (integer 0 6)))
  (nth-value 6 (decode-universal-time (* days +day-sec+) 0)))

(defsubst date-week-day (date)
  "Return the week day of the date."
  (declare (type date date) (values (integer 0 6)))
  (days-week-day (date-dd date)))

(defun next-bad-day (&optional (date (today)) (dir 1))
  "Return the next Friday the 13th after (before) DATE.
The second optional argument can be 1 (default) for `after' and
-1 for `before'."
  (declare (type date date) (type (member 1 -1) dir) (values date))
  (do* ((dd (date-dd date)) (step (* dir 7))
        (fri (+ dd (mod (- 4 (days-week-day dd)) step)) (+ fri step))
        (dt (days2date fri) (days2date fri)))
      ((= 13 (date-da dt)) dt)
    (declare (type date dt) (type (member 7 -7) step) (type days-t dd fri))))

;;;
;;; generic
;;;

(fmakunbound 'date)
(defgeneric date (xx)
  (:documentation "Convert to or extract a date.
The argument can be:
   - a date - it is returned untouched;
   - a string - it is destructively parsed;
   - a symbol - it is uninterned and its name is destructively parsed;
   - an integer - interpreted as the number of days since the epoch,
   - a real number - interpreted as the number of seconds since the epoch;
   - a stream - read as Month Day Year;
   - a structure - the appropriate slot is used;
   - a cons - called recursively on CAR;
   - NIL - an error is signalled.")
  (:method ((xx date)) xx)
  (:method ((xx string))
           ;; The following formats are accepted:
           ;; `1969-12-7', `May 8, 1945', `1945, September 2'.
           (multiple-value-bind (ye mo da)
               (values-list (string-tokens (purge-string xx) :max 3))
             (if (numberp ye) (mk-date :ye ye :mo (infer-month mo) :da da)
                 (mk-date :ye da :mo (infer-month ye) :da mo))))
  (:method ((xx null)) (error "Cannot convert NIL to date")) ; +bad-date+
  (:method ((xx symbol)) (unintern xx) (date (symbol-name xx)))
  (:method ((xx integer)) (days2date xx))
  (:method ((xx real)) (time2date xx))
  (:method ((xx stream))        ; Read date in format MONTH DAY YEAR
           (mk-date :mo (infer-month (read xx)) :da (read xx) :ye (read xx)))
  (:method ((xx cons)) (date (car xx))))
(declaim (ftype date-f-t date))
;;;
;;; utilities
;;;

(defun days-since (key beg)
  "Return a function that will return the number of days between BEG
and (funcall KEY arg), as a fixnum. KEY should return a date."
  (declare (type date-f-t key) (values (function (t) days-t)))
  (let ((dd (date-dd (date beg))))
    (declare (type days-t dd))
    (lambda (rr) (- (date-dd (funcall key rr)) dd))))

(defun days-since-f (key beg)
  "Return a function that will return the number of days between BEG
and (funcall KEY arg), as a double-float. KEY should return a date."
  (declare (type date-f-t key) (values (function (t) double-float)))
  (let ((dd (dfloat (date-dd (date beg)))))
    (declare (double-float dd))
    (lambda (rr) (dfloat (- (date-dd (funcall key rr)) dd)))))

(defun today ()
  "Return today's date."
  (declare (values date)) (time2date (get-universal-time)))

(defun timestamp (&optional (time (get-universal-time)))
  "Return the current time as a string without blanks."
  (declare (integer time) (values simple-string))
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d_~2,'0d:~2,'0d:~2,'0d"
            ye mo da ho mi se)))

(defun date= (d0 d1)
  "Check that the two dates are the same."
  (declare (type date d0 d1)) (= (date-dd d0) (date-dd d1)))

(defun date/= (d0 d1)
  "Check that the two dates are not the same."
  (declare (type date d0 d1)) (/= (date-dd d0) (date-dd d1)))

(defun date< (d0 d1)
  "Check the precedence of the two dates."
  (declare (type date d0 d1)) (< (date-dd d0) (date-dd d1)))

(defun date> (d0 d1)
  "Check the precedence of the two dates."
  (declare (type date d0 d1)) (> (date-dd d0) (date-dd d1)))

(defun date<= (d0 d1)
  "Check the precedence of the two dates."
  (declare (type date d0 d1)) (<= (date-dd d0) (date-dd d1)))

(defun date<=3 (d0 d1 d2)
  "Check the precedence of the three dates."
  (declare (type date d0 d1 d2))
  (<= (date-dd d0) (date-dd d1) (date-dd d2)))

(defun date>= (d0 d1)
  "Check the precedence of the two dates."
  (declare (type date d0 d1)) (>= (date-dd d0) (date-dd d1)))

(defun date<=3 (d0 d1 d2)
  "Check the precedence of the three dates."
  (declare (type date d0 d1 d2))
  (>= (date-dd d0) (date-dd d1) (date-dd d2)))

(defun date=*1 (&rest dates)
  "Check any number of dates for being the same."
  (declare (dynamic-extent dates))
  (apply #'= (map-into dates #'date-dd dates)))

(defun date=* (date1 &rest dates)
  "Check any number of dates for being the same."
  (let ((d1 (date-dd date1)))
    (declare (type days-t d1))
    (every (lambda (dd) (declare (type date dd)) (= d1 (date-dd dd)))
           dates)))

(defsubst date-max (d0 d1)
  "Return the latest date."
  (declare (type date d0 d1) (values date)) (if (date< d0 d1) d1 d0))

(defsubst date-min (d0 d1)
  "Return the earliest date."
  (declare (type date d0 d1) (values date)) (if (date> d0 d1) d1 d0))

(defun next-month-p (d0 d1)
  "True if D1 is the next month of D0."
  (declare (type date d0 d1))
  (let ((y0 (date-ye d0)) (y1 (date-ye d1))
        (m0 (date-mo d0)) (m1 (date-mo d1)))
    (or (and (= (1+ m0) m1) (= y0 y1))
        (and (= m0 12) (= m1 1) (= (1+ y0) y1)))))

(defun date-month= (d0 d1)
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
  (declare (type date dt) (values (integer 1 4)))
  (1+ (floor (1- (date-mo dt)) 3)))

(defun date-quarter= (d0 d1)
  "Return t if the dates are in the same quarter."
  (declare (type date d0 d1))
  (and (= (date-ye d0) (date-ye d1)) (= (date-quarter d0) (date-quarter d1))))

(defsubst days-between (d0 &optional (d1 (today)))
  "Return the number of days between the two dates."
  (declare (type date d0 d1) (values fixnum))
  (- (date-dd d1) (date-dd d0)))

(defun tomorrow (&optional (dd (today)) (skip 1))
  "Return the next day.
With the optional second argument (defaults to 1) skip as many days.
I.e., (tomorrow (today) -1) is yesterday."
  (declare (type date dd) (type days-t skip) (values date))
  (days2date (+ (date-dd dd) skip)))

(defsubst yesterday (&optional (dd (today)) (skip 1))
  "Return the previous day.  Calls tomorrow."
  (declare (type date dd) (type days-t skip) (values date))
  (tomorrow dd (- skip)))

(defun date-in-list (dt lst &optional (key #'date) last)
  "Return the tail of LST starting with DT.
If LAST is non-nil, make sure that the next date is different.
*Important*: assumes that the list is ordered according to `date>'."
  (declare (list lst) (type date-f-t key) (values list))
  (let ((ll (binary-member (date dt) lst :key key :test #'date<=)))
    (if (and last ll (cdr ll)) (skip-to-new ll :key key :test #'date=) ll)))

(defun check-dates (lst order-p same-p jump gap &key (date #'date)
                    (val #'value) (stream *standard-output*) fixp)
  "Check the dated list LST for order violations, redundancies, large
data changes and gaps. The accessors DATE and VAL default to CAR and
CDR respectively and may be omitted if LST is a dated list.
If you need to check more than one value for jumps, you can resort
to `check-list-values'.
If FIXP is non-nil, try to fix the problems.
Return NIL if no errors were detected, the number of errors otherwise."
  (declare (type date-f-t date) (stream stream)
           (type (or null fixnum) gap) (type (or null number) jump)
           (type (function (t) double-float) val))
  (format stream "~&Checking the list~:[~; `~:*~a'~] for:~%~5t~?.~%"
          (if (dated-list-p lst) (dated-list-name lst) nil)
          (list-format "~{~a~}")
          (nconc (if order-p (list (list "order")) nil)
                 (if same-p (list (list "redundancies")) nil)
                 (if jump (list (list "jumps of at least " jump)) nil)
                 (if gap (list (list "gaps of at least " gap)) nil)))
  (labels ((check (lst order-p same-p jump gap date val stream fixp)
             (declare (type date-f-t date) (stream stream)
                      (type (or null fixnum) gap) (type (or null number) jump)
                      (type (function (t) double-float) val))
             (do ((rr lst (cdr rr)) (len 1 (1+ len)) ; start from 1 (sic!)
                  (r0 (first lst) r1) r1 (err 0) (fixed 0) (fff nil nil)
                  (v0 (if val (funcall val (first lst))) v1) v1 jjj ggg
                  (d0 (funcall date (first lst)) d1) (d1 +bad-date+))
                 ((null (cdr rr))
                  (format stream
                          "~:d record~:p, ~a through ~a. ~d error~:p found.~%"
                          len (funcall date (first lst)) d1 err)
                  (if (zerop err) nil
                      (cond ((zerop fixed) err)
                            (t (format
                                stream "~d error~:p; ~d fixed; re-running...~%"
                                err fixed)
                               (check lst order-p same-p jump gap date
                                      val stream nil)))))
               (declare (type index-t len err) (type date d0 d1))
               (setq r1 (second rr) d1 (funcall date r1)
                     v1 (if val (funcall val r1)))
               (when (and (null fff) order-p (date< d1 d0))
                 (format stream "~3d. Wrong Order:~% - ~s~% - ~s~2%"
                         (incf err) r0 r1)
                 (when fixp (format stream " ### cannot fix~%")))
               (when (and (null fff) same-p (date= d0 d1))
                 (format stream "~3d. Same Date:~% - ~s~% - ~s~2%"
                         (incf err) r0 r1)
                 (when fixp
                   (format stream " *** removed ~a~%" r1)
                   (incf fixed) (setq fff t)
                   (setf (cdr rr) (cddr rr))))
               (when (and (null fff)
                          (or (setq ggg (and gap (> (days-between d0 d1) gap)))
                              (setq jjj (and jump (> (rel-diff v0 v1) jump)))))
                 (format stream "~3d. Jump[~a] Gap[~a]:~% - ~s~% - ~s~2%"
                         (incf err) jjj ggg r0 r1)
                 (when fixp
                   (cond ((eq rr lst)
                          (format stream " *** first record, removed ~a~%" r0)
                          (incf fixed) (setq fff t)
                          (setf (car rr) (cadr rr) (cdr rr) (cddr rr)))
                         ((null (cddr rr))
                          (format stream " *** last record, removed ~a~%" r1)
                          (incf fixed) (setq fff t)
                          (setf (cdr rr) nil))
                         ((and ggg (every (lambda (zz) (= v1 (funcall val zz)))
                                          (cddr rr)))
                          (format stream " *** truncated (end), kept ~a~%" r0)
                          (incf fixed) (setq fff t)
                          (setf (cdr rr) nil))
                         ((and ggg (every (lambda (zz) (= v0 (funcall val zz)))
                                          (ldiff lst rr)))
                          (format stream " *** truncated (beg), kept ~a~%" r1)
                          (incf fixed) (setq fff t)
                          (setf (car lst) (cadr rr) (cdr lst) (cddr rr)))
                         (t (format stream " ### cannot fix~%"))))))))
    (if (dated-list-p lst)
        (let ((date (dl-date lst)) (val (dl-val lst)) (ii -1))
          (dolist (ll (dated-list-fl lst))
            (format stream " [~d: ~a ~d] " (incf ii) (car ll)
                    (length (cdr ll)))
            (check (cdr ll) order-p same-p jump gap date val stream fixp)))
        (check lst order-p same-p jump gap date val stream fixp))))

(defun sync-dates (lists &key labels key cpy set (out *standard-output*)
                   op skip)
  "Make all the lists have records for all the dates.
If LABELS is not given, no messages are printed.
\(funcall KEY record) ==> date
\(funcall CPY rec) ==> copy of rec
\(funcall SET rec dt) ==> set the date of the rec.
Stops when the lists end. Prints messages to OUT.
Calls OP on the section of the LISTS with the same dates, using the
previous record when SKIP is non-nil and nil otherwise.
  (sync-dates lists &key labels key cpy set (out *standard-output*) op skip)"
  (declare (list lists) (type (or null stream) out))
  (mesg t out "~&Synching dates in ~d lists.~%" (length lists))
  (do ((sec (copy-list lists)) (heads (make-list (length lists))) fnn (nerr 0)
       (bd +bad-date+) (err nil nil) (ckey +bad-date+) (len 0 (1+ len)))
      ((every #'null sec)
       (mesg t out "~:d records in ~d lists checked. ~d error~:p found.~%"
             len (length lists) nerr))
    (declare (type index-t nerr len) (type date bd ckey))
    ;; get the current date
    (setq fnn (member nil sec :test-not #'eq)
          bd (funcall key (caar fnn)))
    (dolist (ls (rest fnn))
      (when ls (setq ckey (funcall key (car ls)))
            (when (date> bd ckey) (setq bd ckey))
            (unless (date= bd ckey) (setq err t))))
    ;; handle date mismatches
    (when (and out labels err)
      (format out " --[~:d]--> date mismatch:~:{~%  <~a: ~a>~}~%"
              len (mapcar #'cons labels sec))
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

(defmacro sync-dates-ui (lists &key labels key cpy (out '*standard-output*)
                         op skip)
  "Use KEY for SET, which should be a slot. See `sync-dates'."
  `(sync-dates ,lists :labels ,labels :key (lambda (rr) (slot-value rr ,key))
    :cpy ,cpy :out ,out  :op ,op :skip ,skip
    :set (lambda (rr dd) (setf (slot-value rr ,key) dd))))

;;;
;;; Dated List
;;;

(eval-when (load compile eval)
(defstruct (dated-list #+cmu (:print-function print-struct-object))
  "A dated list of records."
  (code nil :type symbol)       ; the code (2 letter symbol)
  (name "??" :type simple-string) ; the name of the data
  (date 'date :type symbol)     ; the date accessor
  (val 'value :type symbol)     ; the value accessor
  (chg 'identity :type symbol)  ; the change accessor
  (misc nil :type (or symbol function)) ; the miscellaneous accessor
  (fl nil :type list)           ; the full list (list of lists)
  (cl nil :type list)           ; the current list (sublist of fl)
  (cp nil :type list))          ; the current position (sublist of (cdar cl))
)

(defsubst dl-ll (dl)
  "The current list."
  (declare (type dated-list dl)) (dated-list-cp dl))
  ;; (cdar (dated-list-cl dl)))

(defsetf dl-ll (dl) (ls)
  "Set the dated list's current list."
  (declare (type dated-list dl))
  `(setf (dated-list-cp ,dl) ,ls))

(defsetf dl-fl (dl) (ls)
  "Init FL and CL."
  (declare (type dated-list dl))
  (let ((xx (gensym "DL-FL")) (yy (gensym "DL-FL")))
    `(let ((,xx ,dl) (,yy ,ls))
      (setf (dated-list-cl ,xx) ,yy (dated-list-fl ,xx) ,yy
       (dated-list-cp ,xx) (cdar ,yy)))))

(defun dl-reset (dl)
  "Reset CL to FL."
  (declare (type dated-list dl))
  (setf (dated-list-cl dl) (dated-list-fl dl)
        (dated-list-cp dl) (cdar (dated-list-fl dl)))
  dl)

(defun mk-dl (ll &rest others)
  "Make and init dated list."
  (let ((dl (apply #'make-dated-list :fl ll others)))
    (dl-reset dl)))

(defsubst dl-date (dl)
  "Return the DATE function of the dated list DL."
  (declare (type dated-list dl) (values date-f-t))
  (fdefinition (dated-list-date dl)))

(defsubst dl-val (dl)
  "Return the VAL function of the dated list DL."
  (declare (type dated-list dl) (values (function (t) double-float)))
  (fdefinition (dated-list-val dl)))

(defsubst dl-chg (dl)
  "Return the CHG function of the dated list DL."
  (declare (type dated-list dl) (values (function (t) double-float)))
  (fdefinition (dated-list-chg dl)))

(defsubst dl-misc (dl)
  "Return the MISC function of the dated list DL."
  (declare (type dated-list dl) (values function))
  (fdefinition (dated-list-misc dl)))

(defsubst dl-slot (dl slot)
  "Return the SLOT function of the dated list DL."
  (declare (type dated-list dl) (type (member val chg) slot)
           (values (function (t) double-float)))
  (fdefinition (slot-value dl slot)))

(defsubst dl-len (dl)
  "Return the length of the dated list."
  (declare (type dated-list dl) (values fixnum)) (length (dl-ll dl)))

(defun dl-full-len (dl)
  "Return the full length of the dated list."
  (declare (type dated-list dl) (values fixnum))
  (reduce #'+ (dated-list-fl dl) :key (compose 1- length)))

(defsubst dl-endp (dl)
  "Check for the end of the dated list."
  (declare (type dated-list dl))
  (and (endp (dl-ll dl)) (endp (cdr (dated-list-cl dl)))))

(defsubst dl-code (dl)
  "Get the code of the dated list."
  (if (dated-list-p dl) (dated-list-code dl) dl))

(defun dl-nth (dl &optional (nn 0))
  "Return the Nth record of the dated list.
Optional second arg defaults to 0. If it is negative, count from the end,
so that -1 corresponds to the last record."
  (declare (type dated-list dl) (fixnum nn))
  (if (minusp nn) (car (last (dl-ll dl) (- nn))) (nth nn (dl-ll dl))))

(defun dl-nth-date (dl &optional (nn 0))
  "Return the Nth date of the dated list."
  (declare (type dated-list dl) (fixnum nn) (values (or null date)))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (dl-date dl) bb))))

(defun dl-nth-val (dl &optional (nn 0))
  "Return the Nth value of the dated list."
  (declare (type dated-list dl) (fixnum nn) (values (or null double-float)))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (dl-val dl) bb))))

(defun dl-nth-chg (dl &optional (nn 0))
  "Return the Nth change of the dated list."
  (declare (type dated-list dl) (fixnum nn) (values (or null double-float)))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (dl-chg dl) bb))))

(defun dl-nth-misc (dl &optional (nn 0))
  "Return the Nth MISC of the dated list."
  (declare (type dated-list dl) (fixnum nn))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (dl-misc dl) bb))))

(defun dl-nth-slot (dl slot &optional (nn 0))
  "Return the Nth SLOT of the dated list."
  (declare (type dated-list dl) (fixnum nn))
  (let ((bb (dl-nth dl nn))) (and bb (funcall (slot-value dl slot) bb))))

(defmethod print-object ((dl dated-list) (stream stream))
  (if *print-readably* (call-next-method)
      (let ((fl (dated-list-fl dl)) (dd (dl-date dl)))
        (format stream "~:d/~d ~a [~:@(~a~)] [~a -- ~a]"
                (dl-full-len dl) (length fl) (dated-list-name dl)
                (dated-list-code dl) (if fl (funcall dd (cadar fl)) nil)
                (if fl (funcall dd (car (last (car (last fl))))) nil)))))

(defmethod describe-object ((dl dated-list) (stream stream))
  (let ((f (dated-list-fl dl)) (c (dated-list-cl dl)) (p (dated-list-cp dl)))
    (setf (dated-list-fl dl) nil (dated-list-cl dl) nil (dated-list-cp dl) nil)
    (call-next-method)
    (setf (dated-list-fl dl) f (dated-list-cl dl) c (dated-list-cp dl) p)
    (format stream "~%fl (~d):~%" (length f))
    (dolist (f1 f)
      (format stream " * ~a (~d)~%   ~a~%   ~a~%" (car f1) (length f1)
              (cadr f1) (car (last f1))))
    (format stream "cl: ~a~%cp: ~a~%" (caar c) (car p))))

(defun rollover (list &optional (datef #'date))
  "Return the rollover date for the list."
  (declare (list list) (type date-f-t datef) (values date))
  (if (date-p (car list)) (car list) (funcall datef (car (last list)))))

(defun date-in-dated-list (dt dl &optional last)
  "Call `date-in-list' on the dated list.
If  LAST is non-nil, make sure that the next date is different.
No side effects."
  (declare (type dated-list dl) (type (or null date) dt) (values list))
  (if (null dt) (dl-ll dl)
      (do ((dd (dl-date dl)) (ls (dated-list-fl dl) (cdr ls)))
          ((date<= dt (rollover (car ls) dd))
           (values (date-in-list dt (cdar ls) dd last) ls))
        (declare (type date-f-t dd)))))

(defun dl-double-date-p (dt dl)
  "Return T if the date DT is present in DL twice."
  (declare (type dated-list dl))
  (setq dt (date dt))
  (let ((ta (second (date-in-dated-list dt dl))))
    (and ta (date= dt (funcall (dl-date dl) ta)))))

(defcustom *dl-max-overlap* index-t 100
  "*The recommended maximum overlap in dated lists.")

(defun dl-overlap (dl &optional (out *standard-output*))
  "Calculate and print the overlap."
  (declare (type dated-list dl) (type (or null stream) out))
  (mesg :log out " *** Overlap for: ~a~%" dl)
  (loop :for ll :in (dated-list-fl dl) :and ii :of-type index-t :from 0
        :for len :of-type index-t = (length (cdr ll))
        :with rd :and col :of-type index-t = len
        :and dd :of-type function = (dl-date dl)
        :when rd :do (setq col (position rd (cdr ll) :test #'date<= :key dd))
        :do (mesg :log out "[~2d ~a ~d]~@[ ~a~] ~d~%" ii (car ll) len rd col)
        (setq rd (rollover ll dd))
        :minimize col :into mol
        :finally (mesg :head out " *** ~a --> ~d~%" dl mol) (return mol)))

(defun (setf dl-overlap) (ol dl)
  "Modify the overlap."
  (loop :with dd :of-type function = (dl-date dl)
        :for ll :in (cdr (dated-list-fl dl))
        :and rd = (rollover (car (dated-list-fl dl)) dd) :then (rollover ll dd)
        :do
        (setf (cdr ll) (nreverse (cdr ll))
              (cdr (nthcdr ol (member rd (cdr ll) :key dd :test #'date>=))) nil
              (cdr ll) (nreverse (cdr ll)))
        :finally (return ol)))

(defun dl-shift (dl &optional (dt 1))
  "Make DL start from DT. Return the DL.
If DT is a fixnum, skip that many records instead.
Defaults to 1."
  (declare (type dated-list dl) (type (or fixnum date) dt))
  (etypecase dt
    (date (setf (values (dated-list-cp dl) (dated-list-cl dl))
                (date-in-dated-list dt dl)))
    (fixnum
     (loop :repeat dt :with dd :of-type date-f-t = (dl-date dl)
           :with rd :of-type date = (rollover (car (dated-list-cl dl)) dd)
           :if (and (cdr (dated-list-cp dl))
                    (date<= (funcall dd (cadr (dated-list-cp dl))) rd))
           :do (pop (dated-list-cp dl))
           :else :do
             (pop (dated-list-cl dl)) :and
             :if (dated-list-cl dl)
             :do (setf (dated-list-cp dl)
                       (cdr (date-in-list rd (cdar (dated-list-cl dl)) dd))
                       rd (rollover (car (dated-list-cl dl)) dd))
             :else :do (setf (dated-list-cp dl) nil) :and :return dl :end
           :end)))
  dl)

(defun dl-next-chg (dl)
  "Shift dl to the next date, return the change in val.
Can be used with chain contracts, where there are double records for
roll-over dates."
  (declare (type dated-list dl))
  (let ((ll (dl-ll dl)))
    (unless (cdr ll) (return-from dl-next-chg nil))
    (cond ((date= (funcall (dl-date dl) (first ll))
                  (funcall (dl-date dl) (second ll)))
           (unless (cddr ll) (return-from dl-next-chg nil))
           (setf (dl-ll dl) (cddr ll))
           (- (funcall (dl-val dl) (third ll))
              (funcall (dl-val dl) (second ll))))
          (t (setf (dl-ll dl) (cdr ll))
             (- (funcall (dl-val dl) (second ll))
                (funcall (dl-val dl) (first ll)))))))

(defsubst dl-count-jumps (dl &optional (key #'date-ye))
  "Return the number of years in the dated list."
  (declare (type dated-list dl) (type (function (date) t) key) (values fixnum))
  (count-jumps (dl-ll dl) :key (compose 'key (dl-date dl))))

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
  (do* ((dl-t (dl-ll dl) (cdr dl-t)) (dr 0)
        (dlr1 (first dl-t) dlr2) (dlr2 (second dl-t) (second dl-t))
        (dld (dl-nth-date dl) (if dlr1 (funcall (dl-date dl) dlr1)))
        (rr (dl-nth-misc dl)))
       ((not (date< dld dt))
        (when (date< dt dld)    ; next dx is later than fx
          (format t "Missing ~a data for: ~a~%" (dated-list-name dl) dt))
        (setf (dl-ll dl) dl-t)
        (+ dr (- (funcall (dl-misc dl) dlr1) rr)))
    (assert dl-t (dt) "~a ended before ~a~%" (dated-list-name dl) dt)
    (when (and dlr2 (date= dld (funcall (dl-date dl) dlr2)))
      (mesg t stlog " ---> new ~a contract~%" (dated-list-name dl))
      (format t "New ~a contract started ~a~%" (dated-list-name dl) dld)
      (incf dr (- (funcall (dl-misc dl) dlr1) (funcall (dl-misc dl) dlr2))))))

(defun skip-dl-to-extremum (dl)
  "Skip (shift) the dated list to the next extremum.
Return nil if at the end already, or the change in value."
  (declare (type dated-list dl))
  (unless (cdr (dl-ll dl)) (return-from skip-dl-to-extremum nil))
  (do ((ll (dl-ll dl) (dl-ll dl)) ch (mv 0.0d0))
      ((null (setq ch (dl-next-chg dl))) mv)
    (declare (double-float mv))
    (cond ((minusp (* ch mv)) (setf (dl-ll dl) ll)
           (return-from skip-dl-to-extremum mv))
          (t (incf mv ch)))))

(defun print-dated-lists (begd endd &rest dls)
  "Print the dated lists from BEGD to ENDD, inclusive."
  (let ((bd (date begd)) (ed (date endd)))
    (declare (type date bd ed))
    (assert dls (dls) "nothing to print for ~a -- ~a~%" bd ed)
    (with-printing (prn)
      (dolist (dl dls)
        (format prn "~a [~a -- ~a]~%" (dated-list-name dl) bd ed)
        (do ((td (dl-shift (copy-dated-list dl) bd) (dl-shift td)))
            ((date> (dl-nth-date td) ed) (format prn "~%"))
          (format prn "~a~%" (dl-nth td)))))))

(defsubst volatility-dl (dl &key (split #'date-ye) (slot 'val))
  "Apply `volatility' to the dated list.
Key defaults to VAL; split defaults to `date-ye'."
  (declare (type dated-list dl) (function split) (symbol slot))
  (loop :for ll :in (dated-list-fl dl) :and nn :of-type index-t :from 0
        :with kk :of-type function = (dl-slot dl slot)
        :and sp :of-type function = (compose 'split (dl-date dl))
        :and ls :of-type list :and vv :of-type double-float
        :do (setf (values vv ls) (volatility (cdr ll) sp :key kk))
        :nconc ls :into lst :sum vv :into tv :of-type double-float
        :finally (return (values (/ tv nn) lst))))

(defun print-volatilities (ls &key (out t) (dl #'identity) (head #'identity))
  "Print the annual and monthly volatilities of the objects in the list.
DL is the dated list accessor; HEAD is ihe header for the object,
it should return a short symbol or string."
  (declare (list ls) (function dl head))
  (format out "~&~70,,,'_:@< Volatilities ~>~%Name     Monthly Annual :")
  (let ((py t))
    (dolist (ob ls)
      (let ((dl (funcall dl ob)) (he (funcall head ob)) mv lv av)
        (setf (values av lv) (volatility-dl dl :split #'date-ye)
              mv (volatility-dl dl :split #'date-mo))
        (when py (setq py nil)
              (dolist (ye lv (terpri out)) (format out "  ~d" (car ye))))
        (format out "~a~10t~6,4f ~6,4f :" he mv av)
        (dolist (yv lv) (format out " ~5,3f" (cdr yv)))
        (terpri out)))))

(defsubst lincom (c0 x0 c1 x1)
  "Compute c0*x0+c1*x1."
  (declare (double-float c0 x0 c1 x1) (values double-float))
  (with-type double-float (+ (* c0 x0) (* c1 x1))))

(defun exp-mov-avg (coeff seq &optional (key #'value) date)
  "Return the list of the exponential moving averages with the given
coefficient for the given sequence."
  (declare (double-float coeff) (sequence seq)
           (type (or null date-f-t) date)
           (type (function (t) double-float) key))
  (let* ((ema (funcall key (elt seq 0))) (c1 (- 1.0d0 coeff)))
    (declare (double-float ema c1))
    (map 'list
         (if date
             (lambda (el) (cons (funcall date el)
                                (setq ema (lincom coeff (funcall key el)
                                                  c1 ema))))
             (lambda (el) (setq ema (lincom coeff (funcall key el) c1 ema))))
         seq)))

(defun exp-mov-avg-append (coeff seq)
  "Put the exponential moving average of SEQ into it.
The SEQ is a sequence of conses with (cdr (last ELT)) being a double-float X.
It is is replaced with (X . EMA)."
  (declare (double-float coeff) (sequence seq))
  (let ((ema (cdr (last (elt seq 0)))) (c1 (- 1.0d0 coeff)))
    (declare (double-float ema c1))
    (map-in (lambda (el)
              (let* ((ee (last el)) (nn (cdr ee)))
                (declare (double-float nn))
                (setf (cdr ee) (cons nn (setq ema (lincom coeff nn c1 ema))))
                el))
            seq)))

(defun exp-mov-avg-dl (coeff idl &optional double (slot 'val))
  "UI for `exp-mov-avg' when the argument is a dated list itself.
When DOUBLE is given, compute 2 averages, with COEFF and COEFF/2,
and make the latter accessible through MISC."
  (declare (double-float coeff) (type dated-list idl) (values dated-list))
  (let* ((c2 (/ coeff 2.0d0)) (dd (dl-date idl)) (kk (dl-slot idl slot))
         (dl (make-dated-list
              :date 'car :val 'cdr :name
              (format nil "EMA [~3,2f~:[~;/~4,3f~]] `~a'" coeff
                      double c2 (dated-list-name idl))
              :code (keyword-concat (dated-list-code idl) :-ema)))
         (ll (mapcar (lambda (l1)
                       (cons (car l1) (exp-mov-avg coeff (cdr l1) kk dd)))
                     (dated-list-fl idl))))
    (declare (type dated-list dl))
    (setf (dl-fl dl) ll)
    (when double
      (setf (dated-list-val dl) 'cadr
            (dated-list-misc dl) 'cddr)
      (dolist (ll (dated-list-fl dl)) (exp-mov-avg-append c2 (cdr ll))))
    dl))

(defun regress-dl (dl &optional begd endd)
  "Regress the dated list in the given interval.
When a boundary is omitted, the end (or the beginning) is used.
Return the line object and the deviation therefrom.
Must not assume that the list is properly ordered!"
  (declare (type dated-list dl))
  (setq begd (if begd (date begd)) endd (if endd (date endd)))
  (with-sublist (ll (dl-ll dl) begd endd :key (dl-date dl)
                 :test #'date=)
    (regress ll :ykey (dl-val dl)
             :xkey (days-since-f (dl-date dl)
                                 (date-min (funcall (dl-date dl) (car ll))
                                           (funcall (dl-date dl)
                                                    (car (last ll))))))))

(defsubst mean-dl (dl &key (slot 'val))
  "Apply `mean' to the dated list."
  (declare (type dated-list dl))
  (mean (dl-ll dl) :key (dl-slot dl slot)))

(defun standard-deviation-dl (dl &rest opts &key (slot 'val)
                              &allow-other-keys)
  "Apply `standard-deviation' to the dated list."
  (declare (type dated-list dl))
  (remf opts :slot)
  (apply #'standard-deviation (dl-ll dl) :key (dl-slot dl slot) opts))

(defsubst standard-deviation-relative-dl (dl &key (slot 'val))
  "Apply `standard-deviation' to the dated list."
  (declare (type dated-list dl))
  (standard-deviation-relative (dl-ll dl) :key (dl-slot dl slot)))

(defsubst weighted-mean-dl (dl wts &key (slot 'val))
  "Apply `weighted-mean' to the dated list."
  (declare (type dated-list dl))
  (weighted-mean (dl-ll dl) wts :key (dl-slot dl slot)))

(defmacro with-truncated-dl ((dt dl) &body body)
  "Evaluate BODY when DL is truncated by the date DT."
  (let ((ll (gensym "WTD")) (tt (gensym "WTD")))
    `(let* ((,ll (date-in-dated-list ,dt ,dl)) (,tt (cdr ,ll)))
      (unwind-protect (progn (when ,ll (setf (cdr ,ll) nil)) ,@body)
        (when ,ll (setf (cdr ,ll) ,tt))))))

;;;
;;; Change
;;;

(defstruct (change #+cmu (:print-function print-struct-object))
  "Change structure - for computing difference derivatives."
  (date +bad-date+ :type date)
  (val 0.0d0 :type double-float) ; value
  (chf 0.0d0 :type double-float) ; change forward
  (chb 0.0d0 :type double-float)) ; change backward

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

(defsubst change-type= (ch1 ch2)
  "Are these two of the same type (min/max)?"
  (declare (type change ch1 ch2))
  (eq (change-type ch1) (change-type ch2)))

(defmethod print-object ((chg change) (stream stream))
  (if *print-readably* (call-next-method)
      (format stream "~a [~7,3f <- ~8,3f -> ~7,3f]" (change-date chg)
              (change-chb chg) (change-val chg) (change-chf chg))))

(defsubst change-list-to-dated-list (chl &rest args)
  "Make a dated list containing this change list."
  (apply #'mk-dl (list (cons :all chl)) :date 'change-date
         :val 'change-val :chg 'change-chf :misc 'change-chb args))

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
        (keyword-concat (dated-list-code dl) :-xtr)
        :name (format nil "Extrema of `~a'" (dated-list-name dl))))
    (setf (change-chf chg) ch) (push chg res)
    (setq chg
          (make-change :date (dl-nth-date dd) :val (dl-nth-val dd) :chb ch))))

;;;
;;; Diff
;;;

(defstruct (diff #+cmu (:print-function print-struct-object))
  "A dated diff."
  (date +bad-date+ :type date)
  (di 0.0 :type real)           ; difference
  (ra 1.0 :type real))          ; ratio

(defmethod date ((xx diff)) (diff-date xx))
(defmethod value ((xx diff)) (diff-di xx))

(defsubst diff-list-to-dated-list (dl &rest args)
  "Wrap a list of diff's into a dated-list.
Sets ll, date, val, and passes the rest directly to `make-dated-list'."
  (apply #'mk-dl (list (cons :all dl)) :date 'diff-date :val 'diff-di args))

(defmethod print-object ((df diff) (stream stream))
  (if *print-readably* (call-next-method)
      (format stream "~a ~15,6f ~15,6f" (diff-date df)
              (diff-di df) (diff-ra df))))

(defun diff-lists (ls0 ls1 &key (date0 #'date) (date1 #'date)
                   (val0 #'value) (val1 #'value))
  "Generate a list of diff's from the given 2 lists.
For each pair of records in 2 lists that have the same dates
a diff structure is created with the same date and the difference
and the ratio of the values.
The date is accessed by (funcall date* rec),
the value by (funcall val* rec)."
  (declare (list ls0 ls1) (type date-f-t date0 date1)
           (type (function (t) real) val0 val1) (values list))
  (do* ((bd (date-max (funcall date0 (car ls0))
                      (funcall date1 (car ls1)))) ll c0 c1 d0 d1 cd
        (pd nil cd)             ; prev date
        (l0 (date-in-list bd ls0 date0) (cdr l0))
        (l1 (date-in-list bd ls1 date1) (cdr l1)))
       ((or (null l0) (null l1)) (nreverse ll))
    (setq c0 (car l0) d0 (funcall date0 c0)
          c1 (car l1) d1 (funcall date1 c1)
          cd (date-max d0 d1))
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
