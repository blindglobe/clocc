;;; File: <gnuplot.lisp - 1997-10-29 Wed 16:51:55 EST - sds@WINTERMUTE.eagle>
;;;
;;; Gnuplot interface
;;;
;;; $Id: gnuplot.lisp,v 1.6 1997/10/29 21:52:08 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/gnuplot.lisp,v $
;;; $Log: gnuplot.lisp,v $
;;; Revision 1.6  1997/10/29 21:52:08  sds
;;; Added plot-functions.
;;;
;;; Revision 1.5  1997/10/17 20:34:51  sds
;;; Added plot-lists-arg.
;;;
;;; Revision 1.4  1997/10/15 15:44:17  sds
;;; Added plot-lists.
;;; Made plot-dated-lists plot exponential moving averages.
;;;
;;; Revision 1.3  1997/10/01 20:48:12  sds
;;; Added plot-dated-lists.
;;;
;;; Revision 1.2  1997/10/01 15:34:55  sds
;;; Cosmetic fixes.
;;;
;;;

;(run-shell-command "ls")
;(shell)
;(run-program "ls")
;(make-pipe-input-stream)
;(make-pipe-io-stream)
;(make-pipe-output-stream)

(defvar *gnuplot-path* (pathname "c:/bin/wgnuplot/wgnuplot.exe")
  "*The path to the gnuplot executable")
(defvar *gnuplot-stream* nil "The current gnuplot output stream.")
(defvar *gnuplot-file* (merge-pathnames "plot.tmp" *fin-dir*)
  "*The tmp file for gnuplot.")

(defmacro with-plot-stream ((str &rest header) &body body)
  "Execute body, wich STR bound to the gnuplot stream."
  `(multiple-value-prog1
    (with-open-file (,str *gnuplot-file* :direction :output
		     :if-exists :supersede)
      (plot-header ,str ,@header) ,@body)
    (format t "~&Prepared file `~a'.
Type \"load '~a'\" at the gnuplot prompt.~%"
     *gnuplot-file* *gnuplot-file*)))

(defun plot-header (str xlabel ylabel data-style timefmt xb xe title)
  "Print the header stuff into the stream."
  (flet ((pr (x) (if (numberp x) x (format nil "'~a'" x))))
    (format str "set time '%Y-%m-%d %a %H:%M:%S %Z' 0,0 'Helvetica'
set xdata~:[~%set format x '%g'~; time~%set timefmt ~:*'~a'
set format x ~:*'~a'~]~%set xlabel '~a'~%set ylabel '~a'
set data style ~a~%set xrange [~a:~a]~%set title '~a'~%"
	    timefmt xlabel ylabel data-style (pr xb) (pr xe) title)))

(defun plot-dated-lists (begd endd dls &key (title "Plot") (xlabel "time")
			 (ylabel "value, $") (data-style "lines")
			 (timefmt "%Y-%m-%d") ema rel (slot 'val))
  "Plot the dated lists from BEGD to ENDD.
Most of the keys are the gnuplot set commands.
REL means plot everything relative to the first value.
EMA is the list of parameters for Exponential Moving Averages."
  (setq begd (cond ((date-p begd) begd)
		   ((null begd) (dl-beg-date (car dls)))
		   (t (parse-date begd)))
	endd (cond ((date-p endd) endd)
		   ((null endd) (dl-end-date (car dls)))
		   (t (parse-date endd))))
  (unless dls (error "nothing to plot for `~a'~%" title))
  (with-open-file (str *gnuplot-file* :direction :output :if-exists :supersede)
    (plot-header str xlabel ylabel data-style timefmt begd endd title)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}~%"
	    ;; Ugly. But gnuplot requires a comma *between* plots,
	    ;; and this is the easiest way to do that.
	    (mapcan (lambda (dl)
		      (cons (dated-list-name dl)
			    (mapcar (lambda (ee)
				      (format nil "~a - EMA [~f]"
					      (dated-list-name dl) ee))
				    ema)))
		    dls))
    (let* ((emal (make-list (length ema))) bv
	   (val (if rel (lambda (dl) (/ (dl-beg-slot dl slot) bv))
		    (lambda (dl) (dl-beg-slot dl slot)))))
      (dolist (dl dls)
	(setq bv (dl-beg-slot dl slot))
	(do* ((td (shift-dl (copy-dated-list dl) begd) (shift-dl td)))
	     ((or (null (dated-list-ll td))
		  (date-more-p (dl-beg-date td) endd))
	      (format str "e~%"))
	  (mapl (lambda (e c)
		  (let ((v (funcall val td)))
		    (push (cons (dl-beg-date td)
				(+ (* (car c) v)
				   (* (- 1 (car c)) (or (cdaar e) v))))
			  (car e))))
		emal ema)
	  (format str "~a ~f~%" (dl-beg-date td) (funcall val td)))
	(dolist (em emal)
	  (dolist (ee (nreverse em) (format str "e~%"))
	    (format str "~a ~f~%" (car ee) (cdr ee))))
	;; clean EMAL for the next pass
	(mapl (lambda (ee) (setf (car ee) nil)) emal))))
  (format t "~&Prepared file `~a'.
Type \"load '~a'\" at the gnuplot prompt.~%"
	  *gnuplot-file* *gnuplot-file*))

(defun plot-lists (lss &key (key #'identity) (title "Plot") (xlabel "nums")
		   (ylabel "value") (data-style "linespoints") rel depth)
  "Plot the given lists of numbers.
LSS is a list of lists, car of each list is the title, cdr is the numbers."
  (declare (list lss))
  (unless depth
    (setq depth (apply #'min (mapcar (compose #'length #'rest) lss))))
  (with-open-file (str *gnuplot-file* :direction :output :if-exists :supersede)
    (plot-header str xlabel ylabel data-style nil 0 (1- depth) title)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}~%" (mapcar #'car lss))
    (let* (bv (val (if rel
		       (lambda (ll) (if ll (/ (funcall key (car ll)) bv) 1))
		       (lambda (ll) (if ll (funcall key (car ll)) bv)))))
      (dolist (ls lss)
	(setq bv (funcall key (cadr ls)))
	(do ((ll (cdr ls) (cdr ll)) (ix 0 (1+ ix)))
	    ((= ix depth) (format str "e~%"))
	  (format str "~f~20t~f~%" ix (funcall val ll))))))
  (format t "~&Prepared file `~a'.
Type \"load '~a'\" at the gnuplot prompt.~%"
	  *gnuplot-file* *gnuplot-file*))

(defun plot-lists-arg (lss &key (key #'identity) (title "Plot") (xlabel "nums")
		       (ylabel "value") (data-style "linespoints") rel)
  "Plot the given lists of numbers.
LSS is a list of lists, car of each list is the title, cdr is the list
of conses of abscissas and ordinates."
  (declare (list lss))
  (let ((beg (apply #'min (mapcar #'caadr lss)))
	(end (apply #'max (mapcar (compose #'caar #'last) lss))))
    (with-open-file (str *gnuplot-file* :direction :output
		     :if-exists :supersede)
      (plot-header str xlabel ylabel data-style nil beg end title)
      (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}~%"
	      (mapcar #'car lss))
      (let* (bv (val (if rel (lambda (kk) (/ kk bv)) #'identity)))
	(dolist (ls lss)
	  (setq bv (funcall key (cdadr ls)))
	  (do ((ll (cdr ls) (cdr ll)) kk)
	      ((null ll) (format str "e~%"))
	    (setq kk (funcall key (car ll)))
	    (format str "~f~20t~f~%" (car kk) (funcall val (cdr kk))))))))
  (format t "~&Prepared file `~a'.
Type \"load '~a'\" at the gnuplot prompt.~%"
	  *gnuplot-file* *gnuplot-file*))

(defun plot-functions (fnl xmin xmax numpts &key (title "Function Plot")
		       (xlabel "x") (ylabel "y") (data-style "lines"))
  "Plot the functions from XMIN to XMAX with NUMPTS+1 points.
FNL is a list of (name . function)."
  (declare (list fnl) (type real xmin xmax) (fixnum numpts))
  (with-plot-stream (str xlabel ylabel data-style nil xmin xmax title)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}~%"
	    (mapcar #'car fnl))
    (do ((fn fnl (cdr fn)) (ptl nil nil)) ((null fn) (format str "e~%"))
      (dotimes (ii (1+ numpts) (nreverse ptl))
	(let ((xx (/ (+ (* ii xmax) (* (- numpts ii) xmin)) numpts)))
	  (format str "~f~20t~f~%" xx (funcall (cdar fn) xx)))))))

(defun plot-dated-lists-depth (depth dls slot &rest opts)
  "Plot the dated lists, DEPTH *days* from the beginning.
OPTS is passed to `plot-lists-arg'."
  (apply #'plot-lists-arg
	 (mapcar
	  (lambda (dl)
	    (cons (print-dlist dl nil 2)
		  (dated-list-to-day-list dl :slot slot :depth depth)))
	  dls)
	 opts))

(defun slot-accessor (slot)
  "Return a slot accessor to slot SLOT."
  (lambda (xx) (slot-value xx slot)))

(defun dated-list-to-day-list (dl &key (slot 'val) (depth (dl-len dl)))
  "Make a list of conses (days-from-beg . value) of length
DEPTH out of the dated list."
  (declare (type dated-list dl) (symbol slot) (fixnum depth))
  (do ((bd (dl-beg-date dl)) (ll (dated-list-ll dl) (cdr ll))
       (ii 0 (1+ ii)) rr)
      ((or (null ll) (= ii depth)) (nreverse rr))
    (push (cons (days-between bd (funcall (dated-list-date dl) (car ll)))
		(slot-value (car ll) slot)) rr)))
