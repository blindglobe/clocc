;;; File: <gnuplot.lisp - 1997-10-17 Fri 16:34:41 EDT - sds@WINTERMUTE.eagle>
;;;
;;; Gnuplot interface
;;;
;;; $Id: gnuplot.lisp,v 1.5 1997/10/17 20:34:51 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/gnuplot.lisp,v $
;;; $Log: gnuplot.lisp,v $
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

;(run-shell-command)
;(shell)
;(run-program)
;(make-pipe-input-stream)
;(make-pipe-io-stream)
;(make-pipe-output-stream)

(defvar *gnuplot-path* (pathname "c:/bin/wgnuplot/wgnuplot.exe")
  "*The path to the gnuplot executable")
(defvar *gnuplot-stream* nil "The current gnuplot output stream.")
(defvar *gnuplot-file* (merge-pathnames "plot.tmp" *fin-dir*)
  "*The tmp file for gnuplot.")

(defun plot-dated-lists (begd endd dls &key (title "Plot") (xlabel "time")
			 (ylabel "value, $") (data-style "lines")
			 (timefmt "%Y-%m-%d") ema rel)
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
    (format str "set xdata time~%set xlabel '~a'~%set ylabel '~a'
set data style ~a~%set timefmt '~a'~%set xrange ['~a':'~a']
set title '~a'~%plot~{ '-' using 1:2 title '~a'~^,~}~%"
	    xlabel ylabel data-style timefmt begd endd title
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
	   (val (if rel (lambda (dl) (/ (dl-beg-misc dl) bv)) #'dl-beg-misc)))
      (dolist (dl dls)
	(setq bv (dl-beg-misc dl))
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
  (format t "Prepared file `~a'. Type \"load '~a'\" at the gnuplot prompt.~%"
	  *gnuplot-file* *gnuplot-file*))

(defun plot-lists (lss &key (key #'identity) (title "Plot") (xlabel "nums")
		   (ylabel "value") (data-style "linespoints") rel depth)
  "Plot the given lists of numbers.
LSS is a list of lists, car of each list is the title, cdr is the numbers."
  (declare (list lss))
  (unless depth
    (setq depth (apply #'min (mapcar (compose #'length #'rest) lss))))
  (with-open-file (str *gnuplot-file* :direction :output :if-exists :supersede)
    (format str "set xdata~%set xlabel '~a'~%set ylabel '~a'
set xrange [0:~d]~%set data style ~a~%set title '~a'
plot~{ '-' using 1:2 title '~a'~^,~}~%"
	    xlabel ylabel (1- depth) data-style title (mapcar #'car lss))
    (let* (bv (val (if rel
		       (lambda (ll) (if ll (/ (funcall key (car ll)) bv) 1))
		       (lambda (ll) (if ll (funcall key (car ll)) bv)))))
      (dolist (ls lss)
	(setq bv (funcall key (cadr ls)))
	(do ((ll (cdr ls) (cdr ll)) (ix 0 (1+ ix)))
	    ((= ix depth) (format str "e~%"))
	  (format str "~f~10t~f~%" ix (funcall val ll))))))
  (format t "Prepared file `~a'. Type \"load '~a'\" at the gnuplot prompt.~%"
	  *gnuplot-file* *gnuplot-file*))

(defun plot-lists-arg (lss &key (key #'identity) (title "Plot") (xlabel "nums")
		       (ylabel "value") (data-style "linespoints") rel)
  "Plot the given lists of numbers.
LSS is a list of lists, car of each list is the title, cdr is the list
of conses of abscissas and ordinatas."
  (declare (list lss))
  (let ((beg (apply #'min (mapcar #'caadr lss)))
	(end (apply #'max (mapcar (compose #'caar #'last) lss))))
    (with-open-file (str *gnuplot-file* :direction :output
		     :if-exists :supersede)
      (format str "set xdata~%set xlabel '~a'~%set ylabel '~a'
set xrange [~d:~d]~%set data style ~a~%set title '~a'
plot~{ '-' using 1:2 title '~a'~^,~}~%"
	      xlabel ylabel beg end data-style title (mapcar #'car lss))
      (let* (bv (val (if rel (lambda (kk) (/ kk bv)) #'identity)))
	(dolist (ls lss)
	  (setq bv (funcall key (cadr ls)))
	  (do ((ll (cdr ls) (cdr ll)) kk)
	      ((null ll) (format str "e~%"))
	    (setq kk (funcall key (car ll)))
	    (format str "~f~10t~f~%" (car kk) (funcall val (cdr kk))))))))
  (format t "Prepared file `~a'. Type \"load '~a'\" at the gnuplot prompt.~%"
	  *gnuplot-file* *gnuplot-file*))

