;;; Gnuplot (http://www.gnuplot.org/) interface
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold.
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: gnuplot.lisp,v 2.5 2000/06/23 18:42:01 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/gnuplot.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `date2time', `+day-sec+', `days-between', `date>'
  (require :date (translate-logical-pathname "cllib:date"))
  ;; `dl-nth-date', `dated-list-name', `dl-nth-slot', `dl-shift',
  ;; `copy-dated-list', `dl-endp', `dl-len', `dl-ll', `dl-date'
  (require :datedl (translate-logical-pathname "cllib:datedl"))
  ;; `regress', `make-line', `line-sl', `line-co'
  (require :math (translate-logical-pathname "cllib:math"))
  ;; `regress-poly'
  (require :stat (translate-logical-pathname "cllib:stat"))
  ;; `pipe-output', `close-pipe', `run-prog'
  (require :shell (translate-logical-pathname "port:shell")))

(in-package :cllib)

(export '(*gnuplot-path* #+win32 *gnuplot-path-console* *gnuplot-printer*
          with-plot-stream plot-dated-lists plot-dated-lists-depth
          plot-lists plot-lists-arg plot-error-bars plot-functions))

;;;
;;; variables
;;;

(defcustom *gnuplot-path* simple-string
  #+win32 "c:/bin/gnuplot/wgnuplot.exe"
  #+unix (if (string-equal (machine-type) "linux")
             "/usr/bin/gnuplot" "/usr/local/bin/gnuplot")
  "*The path to the windows gnuplot executable.")
(defconst +gnuplot-epoch+ integer (encode-universal-time 0 0 0 1 1 2000 0)
  "*The gnuplot epoch - 2000-1-1.")
#+win32
(defcustom *gnuplot-path-console* simple-string "c:/bin/cgnuplot.exe"
  "*The path to the console gnuplot executable.")
(defcustom *gnuplot-printer* simple-string
  (format nil #+win32 "\\\\server1\\~a" #+unix "|lpr -h~@[ -P~a~]"
          (getenv "SDSPRT"))
  "*The printer to print the plots.")
#+unix
(defcustom *gnuplot-stream* (or null stream) nil
  "The current gnuplot output stream.")
(defcustom *gnuplot-file* pathname (merge-pathnames "plot.tmp" *datadir*)
  "*The tmp file for gnuplot.")
(defcustom *gnuplot-msg-stream* (or stream null t) *standard-output*
  "*The message stream of gnuplot functions.")

(declaim (ftype (function (date) (values integer)) plot-sec-to-epoch))
(defsubst plot-sec-to-epoch (dt)
  "Return the number of seconds from date DT to `+gnuplot-epoch+'."
  (declare (type date dt))
  (- (date2time dt) +gnuplot-epoch+))

(defun plot-msg (&rest args)
  "Write a status message to `*gnuplot-msg-stream*'."
  (fresh-line *gnuplot-msg-stream*)
  (write-string "[" *gnuplot-msg-stream*)
  (current-time *gnuplot-msg-stream*)
  (write-string "] " *gnuplot-msg-stream*)
  (apply #'format *gnuplot-msg-stream* args)
  (force-output *gnuplot-msg-stream*))

(defmacro with-plot-stream ((str plot &rest header) &body body)
  "Execute body, with STR bound to the gnuplot stream.
Usage: (with-plot-stream (stream PLOT &rest HEADER) body).
HEADERs are passed to `plot-header', which see.
PLOT means:
  T, :plot => plot;
  :print   => print;
  :wait    => plot and wait for gnuplot to terminate.
  :file    => write `*gnuplot-file*' and print a message.
  NIL      => do nothing, print nothing, return NIL."
  `(when ,plot
    (let ((,str
           #+win32 (if (eq ,plot :print) (pipe-output *gnuplot-path-console*)
                       (open *gnuplot-file* :direction :output
                             :if-exists :supersede))
           #+unix (if (eq ,plot :file)
                      (open *gnuplot-file* :direction :output
                            :if-exists :supersede)
                      (setq *gnuplot-stream*
                            (or (if (and *gnuplot-stream*
                                         (open-stream-p *gnuplot-stream*))
                                    *gnuplot-stream*)
                                (pipe-output *gnuplot-path*))))))
      (declare (stream ,str))
      (unwind-protect (progn (apply #'plot-header ,str ,plot ,@header) ,@body)
        #+win32 (close-pipe ,str)
        #+win32
        (ecase ,plot
          ((t :plot)
           (plot-msg "Starting gnuplot...")
           (close-pipe (pipe-output *gnuplot-path* "/noend" *gnuplot-file*))
           (format *gnuplot-msg-stream* "done.~%"))
          (:wait
           (plot-msg "Waiting for gnuplot to terminate...")
           (format *gnuplot-msg-stream* "gnuplot returned ~a.~%"
                   (run-prog *gnuplot-path* :args
                             (list "/noend" *gnuplot-file*))))
          (:print (plot-msg "Sent the plot to `~a'.~%" *gnuplot-printer*))
          (:file (plot-msg
                  "Wrote `~a'.~%Type \"load '~a'\" at the gnuplot prompt.~%"
                  *gnuplot-file* *gnuplot-file*)))
        #+unix
        (ecase ,plot
          ((t :plot) (plot-msg "Done plotting.~%"))
          (:wait
           (fresh-line *terminal-io*)
           (princ "Press <enter> to continue..." *terminal-io*)
           (force-output *terminal-io*) (read-line *terminal-io* nil nil))
          (:print (plot-msg "Sent the plot to `~a'.~%" *gnuplot-printer*)
                  (format ,str "set output~%"))
          (:file (plot-msg
                  "Wrote `~a'.~%Type \"load '~a'\" at the gnuplot prompt.~%"
                  *gnuplot-file* *gnuplot-file*)))
        #+unix (force-output ,str)))))

(defun plot-header (str plot &key (xlabel "x") (ylabel "y") data-style
                    timefmt xb xe (title "plot") legend (xtics t) (ytics t)
                    grid term (xfmt (or timefmt "%g")) (yfmt "%g"))
  "Print the header stuff into the stream.
This can be called ONLY by `with-plot-stream'.
The following gnuplot options are accepted:
 XLABEL YLABEL TIMEFMT XDATA DATA-STYLE TITLE XBEG XEND LEGEND GRID TERM"
  (declare (stream str))
  (flet ((pp (xx) (format nil (if (numberp xx) "~g" "'~a'") xx))
         (tt (nm par)
           (case par
             ((t) (format str "set ~a~%" nm))
             ((nil) (format str "set no~a~%" nm))
             (t (format str "set ~a ~a~%" nm par)))))
    (if (eq plot :print)
        (format str "set terminal postscript landscape 'Helvetica' 9
set output '~a'~%" *gnuplot-printer*)
        (format str "set terminal ~a~@[ ~a~]~%set output~%" #+unix "x11"
                #+win32 "windows" term))
    (format str "set timestamp '%Y-%m-%d %a %H:%M:%S %Z' 0,0 'Helvetica'
set xdata~@[ time~%set timefmt '~a'~]~%set format x '~a'
~@[set format y '~a'~%~]set xlabel '~a'~%set ylabel '~a'~%set border
set data style ~a~%set xrange [~a:~a]~%set title \"~a\"~%~@[set key ~a~%~]"
            timefmt xfmt yfmt xlabel ylabel data-style (pp xb) (pp xe)
            title legend)
    (tt "xtics" xtics) (tt "ytics" ytics) (tt "grid" grid)))

(defun plot-data-style (num-ls)
  "Decide upon the appropriate data style for the number of points."
  (when (listp num-ls)
    (setq num-ls (1- (apply #'min (mapcar #'length num-ls)))))
  (assert (realp num-ls) (num-ls)
          "plot-data-style got neither number nor list: ~s" num-ls)
  (if (> num-ls 30) "lines" "linespoints"))

;;;###autoload
(defun plot-dated-lists (begd endd dls &rest opts &key (title "Dated Plot")
                         (xlabel "time") rel data-style
                         (ylabel (if rel "relative value" "value"))
                         (timefmt "%Y-%m-%d") ema (slot 'val)
                         (plot t) &allow-other-keys)
  "Plot the dated lists from BEGD to ENDD.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
REL means plot everything relative to the first value.
EMA is the list of parameters for Exponential Moving Averages."
  (assert dls () "Nothing to plot for `~a'~%" title)
  (setq begd (if begd (date begd) (dl-nth-date (car dls)))
        endd (if endd (date endd) (dl-nth-date (car dls) -1)))
  (remf opts :ema) (remf opts :rel)
  (remf opts :slot) (remf opts :plot)
  (with-plot-stream (str plot :xlabel xlabel :ylabel ylabel :title title
                     :data-style (or data-style (plot-data-style
                                                 (days-between begd endd)))
                     :timefmt timefmt :xb begd :xe endd opts)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}"
            ;; Ugly.  But gnuplot requires a comma *between* plots,
            ;; and this is the easiest way to do that.
            (mapcan (lambda (dl)
                      (cons (dated-list-name dl)
                            (mapcar (lambda (ee)
                                      (format nil "~a - EMA [~a]"
                                              (dated-list-name dl) ee))
                                    ema)))
                    dls))
    (terpri str)                ; the command line is over!
    (let* ((emal (make-list (length ema))) bv
           (val (if rel (lambda (dl) (/ (dl-nth-slot dl slot) bv))
                    (lambda (dl) (dl-nth-slot dl slot)))))
      (dolist (dl dls)
        (setq bv (dl-nth-slot dl slot))
        (do* ((td (dl-shift (copy-dated-list dl) begd) (dl-shift td)))
             ((or (dl-endp td) (date> (dl-nth-date td) endd))
              (format str "e~%"))
          (mapl (lambda (ee cc)
                  (let ((vv (funcall val td)))
                    (push (cons (dl-nth-date td)
                                (+ (* (car cc) vv)
                                   (* (- 1 (car cc)) (or (cdaar ee) vv))))
                          (car ee))))
                emal ema)
          (format str "~a ~f~%" (dl-nth-date td) (funcall val td)))
        (dolist (em emal)
          (dolist (ee (nreverse em) (format str "e~%"))
            (format str "~a ~f~%" (car ee) (cdr ee))))
        ;; clean EMAL for the next pass
        (do ((ee emal (cdr ee))) ((null ee)) (setf (car ee) nil))))))

;;;###autoload
(defun plot-lists (lss &rest opts &key (key #'value) (title "List Plot")
                   (plot t) rel (xlabel "nums")
                   (ylabel (if rel "relative value" "value"))
                   (depth (1- (apply #'min (mapcar #'length lss))))
                   (data-style (plot-data-style depth)) &allow-other-keys)
  "Plot the given lists of numbers.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
LSS is a list of lists, car of each list is the title, cdr is the numbers."
  (declare (list lss) (type fixnum depth))
  (remf opts :depth) (remf opts :rel) (remf opts :key) (remf opts :plot)
  (with-plot-stream (str plot :xlabel xlabel :ylabel ylabel :title title
                     :data-style data-style :xb 0 :xe (1- depth) opts)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}~%" (mapcar #'car lss))
    (let* (bv (val (if rel
                       (lambda (ll) (if ll (/ (funcall key (car ll)) bv) 1))
                       (lambda (ll) (if ll (funcall key (car ll)) bv)))))
      (dolist (ls lss)
        (setq bv (funcall key (cadr ls)))
        (do ((ll (cdr ls) (cdr ll)) (ix 0 (1+ ix)))
            ((= ix depth) (format str "e~%"))
          (declare (fixnum ix))
          (format str "~f~20t~f~%" ix (funcall val ll)))))))

;;;###autoload
(defun plot-lists-arg (lss &rest opts &key (key #'identity) rel lines
                       (title "Arg List Plot") (xlabel "nums")
                       (ylabel (if rel "relative value" "value"))
                       data-style quads (plot t) xbeg xend &allow-other-keys)
  "Plot the given lists of numbers.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
LSS is a list of lists, car of each list is the title, cdr is the list
of conses of abscissas and ordinates. KEY is used to extract the cons."
  (declare (list lss))
  (when (eq lines t)
    (setq lines (mapcar (lambda (ls)
                          (regress (cdr ls) :xkey (compose car 'key)
                                   :ykey (compose cdr 'key))) lss)))
  (when (eq quads t)
    (setq quads (mapcar (lambda (ls)
                          (regress-poly (cdr ls) 2 :xkey (compose car 'key)
                                        :ykey (compose cdr 'key))) lss)))
  (setq xbeg (or xbeg (apply #'min (mapcar (compose car 'key cadr) lss)))
        xend (or xend (apply #'max (mapcar (compose car 'key car last) lss))))
  (remf opts :key) (remf opts :rel) (remf opts :lines) (remf opts :quads)
  (remf opts :xbeg) (remf opts :xend) (remf opts :plot)
  (with-plot-stream (str plot :xlabel xlabel :ylabel ylabel
                     :data-style (or data-style (plot-data-style lss))
                     :xb xbeg :xe xend :title title opts)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}" (mapcar #'car lss))
    (dolist (ln lines) (plot-line-str ln xbeg xend str))
    (dolist (qu quads) (plot-quad-str qu xbeg xend str))
    (terpri str)
    (let* (bv (val (if rel (lambda (kk) (/ kk bv)) #'identity)))
      (dolist (ls lss)
        (setq bv (cdr (funcall key (cadr ls))))
        (do ((ll (cdr ls) (cdr ll)) kk)
            ((null ll) (format str "e~%"))
          (setq kk (funcall key (car ll)))
          (format str "~f~20t~f~%" (car kk) (funcall val (cdr kk))))))))

;;;###autoload
(defun plot-error-bars (ll &rest opts &key (title "Error Bar Plot")
                        (xlabel "nums") (ylabel "value") (plot t)
                        (data-style (plot-data-style (list ll)))
                        (xkey #'first) (ykey #'second) (ydkey #'third)
                        &allow-other-keys)
  "Plot the list with errorbars.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
The first element is the title, all other are records from which we
get x, y and ydelta with xkey, ykey and ydkey."
  (declare (list ll))
  (remf opts :xkey) (remf opts :ykey) (remf opts :ydkey) (remf opts :plot)
  (with-plot-stream (str plot :xlabel xlabel :ylabel ylabel :title title
                     :data-style data-style :xb (funcall xkey (second ll))
                     :xe (funcall xkey (car (last ll))) opts)
    (format str "plot 0 title \"\", '-' title \"~a\" with errorbars,~
 '-' title \"\", '-' title \"\", '-' title \"\"~%" (pop ll))
    (dolist (rr ll (format str "e~%"))
      (format str "~a ~a ~a~%" (funcall xkey rr)
              (funcall ykey rr) (funcall ydkey rr)))
    (dolist (rr ll (format str "e~%"))
      (format str "~a ~a~%" (funcall xkey rr)
              (funcall ykey rr) (funcall ydkey rr)))
    (dolist (rr ll (format str "e~%"))
      (format str "~a ~a~%" (funcall xkey rr)
              (- (funcall ykey rr) (funcall ydkey rr))))
    (dolist (rr ll (format str "e~%"))
      (format str "~a ~a~%" (funcall xkey rr)
              (+ (funcall ykey rr) (funcall ydkey rr))))))

;;;###autoload
(defun plot-functions (fnl xmin xmax numpts &rest opts &key data-style (plot t)
                       (title "Function Plot") &allow-other-keys)
  "Plot the functions from XMIN to XMAX with NUMPTS+1 points.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
FNL is a list of (name . function).
E.g.: (plot-functions  (list (cons 'sine #'sin)) 0 pi 100)"
  (declare (list fnl) (real xmin xmax) (type index-t numpts))
  (remf opts :plot)
  (with-plot-stream (str plot :xb xmin :xe xmax :title title
                     :data-style (or data-style (plot-data-style numpts)) opts)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}~%" (mapcar #'car fnl))
    (dolist (fn fnl)
      (dotimes (ii (1+ numpts) (format str "e~%"))
        (declare (type index-t ii))
        (let ((xx (dfloat (/ (+ (* ii xmax) (* (- numpts ii) xmin)) numpts))))
          (format str "~f~20t~f~%" xx (funcall (cdr fn) xx)))))))

;;;###autoload
(defun plot-dated-lists-depth (depth dls slot &rest opts)
  "Plot the dated lists, DEPTH *days* from the beginning.
OPTS is passed to `plot-lists-arg'."
  (apply #'plot-lists-arg
         (mapcar
          (lambda (dl)
            (cons (prin1-to-string dl)
                  (dated-list-to-day-list dl :slot slot :depth depth)))
          dls)
         opts))

(defun dated-list-to-day-list (dl &key (slot 'val) (depth (dl-len dl)))
  "Make a list of conses (days-from-beg . value) of length
DEPTH out of the dated list."
  (declare (type dated-list dl) (symbol slot) (fixnum depth))
  (do ((bd (dl-nth-date dl)) (ll (dl-ll dl) (cdr ll)) (ii 0 (1+ ii)) rr)
      ((or (null ll) (= ii depth)) (nreverse rr))
    (declare (fixnum ii) (type date bd) (list rr ll))
    (push (cons (days-between bd (funcall (dl-date dl) (car ll)))
                (slot-value (car ll) slot)) rr)))

(defun line-day2sec (ln begd)
  "Make a new line, converting from days to seconds."
  (declare (type line ln) (type integer begd))
  (make-line :sl (/ (line-sl ln) +day-sec+) :co
             (- (line-co ln) (* (line-sl ln) (/ begd +day-sec+)))))

(defun plot-line-str (ln beg end str &optional (title "") lt)
  "Write the string to plot stream STR for plotting the line from BEG to END.
This is not a complete plotting function (not a UI)!"
  (declare (type line ln) (real beg end) (stream str))
  (format str ", ((x>~a)?((x<~a)?(~a*x+~a):1/0):1/0) title \"~a\" with lines~
~@[ ~d~]" beg end (line-sl ln) (line-co ln) title lt))

(defun plot-quad-str (qu beg end str &optional (title "") lt)
  "Write the string to plot stream STR for plotting the parabola
from BEG to END.  This is not a complete plotting function (not a UI)!"
  (declare (type (simple-array double-float (3)) qu) (real beg end))
  (format str ", ((x>~a)?((x<~a)?(~a*x*x+~a*x+~a):1/0):1/0) title \"~a\" ~
with lines~@[ ~d~]" beg end (aref qu 0) (aref qu 1) (aref qu 2) title lt))

(provide :gnuplot)
;;; gnuplot.lisp ends here
