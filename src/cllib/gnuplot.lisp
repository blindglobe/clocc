;;; File: <gnuplot.lisp - 1997-12-04 Thu 15:09:13 EST - sds@wintermute.eagle>
;;;
;;; Gnuplot interface
;;;
;;; Copyright (C) 1997 by Sam Steingold.
;;; This is free software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: gnuplot.lisp,v 1.7 1997/12/04 20:10:08 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/gnuplot.lisp,v $
;;; $Log: gnuplot.lisp,v $
;;; Revision 1.7  1997/12/04 20:10:08  sds
;;; Made `with-plot-stream' and `plot-header' plot and print,
;;; not just write the file.
;;;
;;; Revision 1.6  1997/10/29 21:52:08  sds
;;; Added `plot-functions'.
;;;
;;; Revision 1.5  1997/10/17 20:34:51  sds
;;; Added `plot-lists-arg'.
;;;
;;; Revision 1.4  1997/10/15 15:44:17  sds
;;; Added plot-lists.
;;; Made `plot-dated-lists' plot exponential moving averages.
;;;
;;; Revision 1.3  1997/10/01 20:48:12  sds
;;; Added `plot-dated-lists'.
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

(defvar *gnuplot-path* "c:/bin/wgnuplot/wgnuplot.exe"
  "*The path to the windows gnuplot executable.")
(defvar *gnuplot-path-console* "c:/bin/cgnuplot.exe"
  "*The path to the console gnuplot executable.")
(defvar *gnuplot-printer* "\\\\server1\\ddslaser"
  "*The printer to print the plots.")
;(defvar *gnuplot-stream* nil "The current gnuplot output stream.")
(defvar *gnuplot-file* (merge-pathnames "plot.tmp" *fin-dir*)
  "*The tmp file for gnuplot.")

(defmacro with-plot-stream ((str plot &rest header) &body body)
  "Execute body, with STR bound to the gnuplot stream.
Usage: (with-plot-stream (stream plot-or-print &rest header) body).
HEADERs are passed to `plot-header', which see.
PLOT means: T => plot; NIL => print; other => write
`*gnuplot-file*' and print a message."
  `(let ((,str (if ,plot (open *gnuplot-file* :direction :output
			       :if-exists :supersede)
		   (make-pipe-output-stream *gnuplot-path-console*))))
    (unwind-protect
	 (progn (plot-header ,str ,plot ,@header) ,@body)
      (when ,str (close ,str))
      (cond ((eq ,plot t)
	     (format t "&Starting gnuplot...")
	     (close (make-pipe-output-stream
		     (format nil "~a ~a" *gnuplot-path* *gnuplot-file*)))
	     (format t "done.~%"))
	    ((eq ,plot nil)
	     (format t "~&Sent the plot to the printer `~a'.~%"
		     *gnuplot-printer*))
	    ((format t "~&Prepared file `~a'.
Type \"load '~a'\" at the gnuplot prompt.~%"
		     *gnuplot-file* *gnuplot-file*))))))

(defun plot-header (str plot xlabel ylabel data-style timefmt xb xe title)
  "Print the header stuff into the stream.
This is called ONLY by `with-plot-stream'.
The following gnuplot options are accepted:
XLABEL YLABEL TIMEFMT XDATA DATA-STYLE TITLE XB XE (xrange)."
  (flet ((pr (x) (if (numberp x) x (format nil "'~a'" x))))
    (format str "set terminal ~:[postscript landscape~;windows~]
set output~:* ~:['~a'~;~*~]
set time '%Y-%m-%d %a %H:%M:%S %Z' 0,0 'Helvetica'
set xdata~:[~%set format x '%g'~; time~%set timefmt ~:*'~a'
set format x ~:*'~a'~]~%set xlabel '~a'~%set ylabel '~a'
set data style ~a~%set xrange [~a:~a]~%set title \"~a\"~%"
	    plot *gnuplot-printer*
	    timefmt xlabel ylabel data-style (pr xb) (pr xe) title)))

(defun plot-dl-channels (dls chs &rest opts)
  "Plot the dated lists and the channels.
This is the simple UI to `plot-dated-lists'.
The first argument is the list of dated lists,
the second id the list of channels.
The rest is passed to `plot-dated-lists'."
  (let ((begd (apply #'min (mapcar (compose #'date2time #'channel-begd) chs)))
	(endd (apply #'max (mapcar (compose #'date2time #'channel-endd) chs))))
    (incf endd (* 0.25 (- endd begd)))
    (setq begd (date begd) endd (date endd))
    (format t "~&Plotting ~d channel~:p and ~d dated list~:p in [~a -- ~a].~%"
	    (length chs) (length dls) begd endd)
    (apply #'plot-dated-lists begd endd dls :channels chs opts)))

(defun plot-dated-lists (begd endd dls &key (title "Plot") (xlabel "time")
			 (ylabel "value, $") (data-style "lines")
			 (timefmt "%Y-%m-%d") ema rel (slot 'val)
			 channels (plot t))
  "Plot the dated lists from BEGD to ENDD.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
REL means plot everything relative to the first value.
EMA is the list of parameters for Exponential Moving Averages.
CHANNELS id the list of channels to plot."
  (setq begd (if begd (date begd) (dl-nth-date (car dls)))
	endd (if endd (date endd) (dl-nth-date (car dls) nil)))
  (unless dls (error "nothing to plot for `~a'~%" title))
  (with-plot-stream (str plot xlabel ylabel data-style timefmt begd endd title)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}"
	    ;; Ugly. But gnuplot requires a comma *between* plots,
	    ;; and this is the easiest way to do that.
	    (mapcan (lambda (dl)
		      (cons (dated-list-name dl)
			    (mapcar (lambda (ee)
				      (format nil "~a - EMA [~f]"
					      (dated-list-name dl) ee))
				    ema)))
		    dls))
    (when channels (dolist (ch channels) (plot-channel-str ch str)))
    (terpri str)
    (let* ((emal (make-list (length ema))) bv
	   (val (if rel (lambda (dl) (/ (dl-nth-slot dl slot) bv))
		    (lambda (dl) (dl-nth-slot dl slot)))))
      (dolist (dl dls)
	(setq bv (dl-nth-slot dl slot))
	(do* ((td (dl-shift (copy-dated-list dl) begd) (dl-shift td)))
	     ((or (dl-endp td) (date-more-p (dl-nth-date td) endd))
	      (format str "e~%"))
	  (mapl (lambda (e c)
		  (let ((v (funcall val td)))
		    (push (cons (dl-nth-date td)
				(+ (* (car c) v)
				   (* (- 1 (car c)) (or (cdaar e) v))))
			  (car e))))
		emal ema)
	  (format str "~a ~f~%" (dl-nth-date td) (funcall val td)))
	(dolist (em emal)
	  (dolist (ee (nreverse em) (format str "e~%"))
	    (format str "~a ~f~%" (car ee) (cdr ee))))
	;; clean EMAL for the next pass
	(mapl (lambda (ee) (setf (car ee) nil)) emal)))))

(defun plot-lists (lss &key (key #'identity) (title "Plot") (xlabel "nums")
		   (ylabel "value") (data-style "linespoints") rel depth
		   (plot t))
  "Plot the given lists of numbers.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
LSS is a list of lists, car of each list is the title, cdr is the numbers."
  (declare (list lss))
  (unless depth
    (setq depth (apply #'min (mapcar (compose #'length #'rest) lss))))
  (with-plot-stream (str plot xlabel ylabel data-style nil 0 (1- depth) title)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}~%" (mapcar #'car lss))
    (let* (bv (val (if rel
		       (lambda (ll) (if ll (/ (funcall key (car ll)) bv) 1))
		       (lambda (ll) (if ll (funcall key (car ll)) bv)))))
      (dolist (ls lss)
	(setq bv (funcall key (cadr ls)))
	(do ((ll (cdr ls) (cdr ll)) (ix 0 (1+ ix)))
	    ((= ix depth) (format str "e~%"))
	  (format str "~f~20t~f~%" ix (funcall val ll)))))))

(defun plot-lists-arg (lss &key (key #'identity) (title "Plot") (xlabel "nums")
		       (ylabel "value") (data-style "linespoints") rel
		       (plot t))
  "Plot the given lists of numbers.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
LSS is a list of lists, car of each list is the title, cdr is the list
of conses of abscissas and ordinates."
  (declare (list lss))
  (let ((beg (apply #'min (mapcar #'caadr lss)))
	(end (apply #'max (mapcar (compose #'caar #'last) lss))))
    (with-plot-stream (str plot xlabel ylabel data-style nil beg end title)
      (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}~%"
	      (mapcar #'car lss))
      (let* (bv (val (if rel (lambda (kk) (/ kk bv)) #'identity)))
	(dolist (ls lss)
	  (setq bv (funcall key (cdadr ls)))
	  (do ((ll (cdr ls) (cdr ll)) kk)
	      ((null ll) (format str "e~%"))
	    (setq kk (funcall key (car ll)))
	    (format str "~f~20t~f~%" (car kk) (funcall val (cdr kk)))))))))

(defun plot-functions (fnl xmin xmax numpts &key (title "Function Plot")
		       (xlabel "x") (ylabel "y") (data-style "lines") (plot t))
  "Plot the functions from XMIN to XMAX with NUMPTS+1 points.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
FNL is a list of (name . function)."
  (declare (list fnl) (real xmin xmax) (fixnum numpts))
  (with-plot-stream (str plot xlabel ylabel data-style nil xmin xmax title)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}~%" (mapcar #'car fnl))
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

(defun dated-list-to-day-list (dl &key (slot 'val) (depth (dl-len dl)))
  "Make a list of conses (days-from-beg . value) of length
DEPTH out of the dated list."
  (declare (type dated-list dl) (symbol slot) (fixnum depth))
  (do ((bd (dl-nth-date dl)) (ll (dated-list-ll dl) (cdr ll))
       (ii 0 (1+ ii)) rr)
      ((or (null ll) (= ii depth)) (nreverse rr))
    (push (cons (days-between bd (funcall (dated-list-date dl) (car ll)))
		(slot-value (car ll) slot)) rr)))

(defun line-day2sec (ln begd)
  "Make a new line, converting from days to seconds."
  (declare (type line ln))
  (unless (realp begd)
    (setq begd (- (date2time (date begd)) (date2time (date "2000-1-1")))))
  (let ((dd (/ 1 60 60 24)))
    (make-line :sl (* (line-sl ln) dd) :co
	       (- (line-co ln) (* (line-sl ln) dd begd)))))

(defun plot-line-str (ln beg end str)
  "Write the string to plot stream STR for plotting the line from BEG to END.
This is not a complete plotting function (not a UI)!"
  (declare (type line ln) (real beg end))
  (format str ", ((x>~f)?((x<~f)?(~f*x+~f):1/0):1/0) title \"\" with lines"
	  beg end (line-sl ln) (line-co ln)))

(defun plot-channel-str (ch str)
  "Write the string to plot stream STR for plotting the channel.
This is not a complete plotting function (not a UI)!"
  (declare (type channel ch) (type stream str))
  (let* ((epoch (date2time (date "2000-1-1")))
	 (beg (- (date2time (channel-begd ch)) epoch))
	 (end (- (date2time (channel-endd ch)) epoch)))
    (plot-line-str (line-day2sec (channel-top ch) beg) beg end str)
    (plot-line-str (line-day2sec (channel-bot ch) beg) beg end str)
    (plot-line-str (line-day2sec (channel-sta ch) beg) beg end str)))

;;; gnuplot.lisp ends here
