;;; File: <gnuplot.lisp - 1998-10-06 Tue 19:41:04 EDT sds@eho.eaglets.com>
;;;
;;; Gnuplot interface
;;;
;;; Copyright (C) 1997 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: gnuplot.lisp,v 1.17 1998/10/06 23:41:57 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/gnuplot.lisp,v $
;;; $Log: gnuplot.lisp,v $
;;; Revision 1.17  1998/10/06 23:41:57  sds
;;; Added `xtics', `ytics' and `grid' gnuplot options.
;;;
;;; Revision 1.16  1998/08/03 19:15:07  sds
;;; Use (getenv "SDSPRT") to get the printer name.
;;;
;;; Revision 1.15  1998/06/19 21:42:50  sds
;;; Switch from `compose-m' to `compose'.
;;;
;;; Revision 1.14  1998/06/09 15:23:00  sds
;;; After printing, reset terminal and output back to screen - for flushing.
;;;
;;; Revision 1.13  1998/06/08 23:49:44  sds
;;; In function `plot-lists-arg', fixed :key boundaries.
;;;
;;; Revision 1.12  1998/06/03 17:20:35  sds
;;; Added a plot-key key, for placement of the gnuplot legend.
;;;
;;; Revision 1.11  1998/04/29 22:36:32  sds
;;; Made `*gnuplot-epoch*' into a constant `+gnuplot-epoch+'.
;;; Added function `plot-sec-to-epoch'.
;;;
;;; Revision 1.10  1998/03/23 15:53:41  sds
;;; Fixed to work with ACL and CMU CL.
;;;
;;; Revision 1.9  1998/02/19 22:49:42  sds
;;; Switched to automatic guessing of data-style via `plot-data-style'.
;;;
;;; Revision 1.8  1998/02/12 21:38:19  sds
;;; Switched to `defgeneric' and `require'.
;;;
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

(in-package :cl-user)

(eval-when (load compile eval)
  (sds-require "base") (sds-require "date")
  (sds-require "channel") (sds-require "signal")
  ;; the only way to have a good optimization here is to ditch
  ;; flexibility and stick to floats.
  (declaim (optimize (speed 1) (space 0) (safety 3) (debug 3))))

#+cmu
(eval-when (compile)
  (format t " *** `optimize' compile quality is set to 1.~%"))

;(run-shell-command "ls")
;(shell)
;(run-program "ls")
;(make-pipe-input-stream)
;(make-pipe-io-stream)
;(make-pipe-output-stream)

(defcustom *gnuplot-path* simple-string #+win32 "c:/bin/gnuplot/wgnuplot.exe"
        #+unix "/usr/local/bin/gnuplot"
  "*The path to the windows gnuplot executable.")
(defconst +gnuplot-epoch+ date (mk-date :ye 2000 :mo 1 :da 1)
  "*The gnuplot epoch - 2000-1-1.")
#+win32
(defcustom *gnuplot-path-console* simple-string "c:/bin/cgnuplot.exe"
  "*The path to the console gnuplot executable.")
(defcustom *gnuplot-printer* simple-string
  (format nil #+win32 "\\\\server1\\~a" #+unix "|lpr -h -P~a"
          (getenv "SDSPRT"))
  "*The printer to print the plots.")
#+unix
(defcustom *gnuplot-stream* (or null stream) nil
  "The current gnuplot output stream.")
(defcustom *gnuplot-file* pathname (merge-pathnames "plot.tmp" *datadir*)
  "*The tmp file for gnuplot.")
(defcustom *gnuplot-msg-stream* (or stream null t) *standard-output*
  "*The message stream of gnuplot functions.")

(defsubst plot-sec-to-epoch (dt)
  "Return the number of seconds from date DT to `+gnuplot-epoch+'."
  (declare (type date dt) (values integer))
  (* +day-sec+ (days-between +gnuplot-epoch+ dt)))

(defmacro with-plot-stream ((str plot &rest header) &body body)
  "Execute body, with STR bound to the gnuplot stream.
Usage: (with-plot-stream (stream plot-or-print &rest header) body).
HEADERs are passed to `plot-header', which see.
PLOT means: T, :plot => plot; :print => print;
'wait => plot and wait for gnuplot to terminate.
NIL => do nothing, print nothing, return NIL.
other => write `*gnuplot-file*' and print a message."
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
      (unwind-protect (progn (plot-header ,str ,plot ,@header) ,@body)
        #+win32 (when ,str (close ,str))
        #+win32
        (cond ((or (eq ,plot t) (eq ,plot :plot))
               (fresh-line *gnuplot-msg-stream*)
               (princ "Starting gnuplot..." *gnuplot-msg-stream*)
               (force-output *gnuplot-msg-stream*)
               (close (pipe-output *gnuplot-path* "/noend" *gnuplot-file*))
               (format *gnuplot-msg-stream* "done.~%"))
              ((eq ,plot :wait)
               (fresh-line *gnuplot-msg-stream*)
               (princ "Waiting for gnuplot to terminate..."
                      *gnuplot-msg-stream*)
               (force-output *gnuplot-msg-stream*)
               (format *gnuplot-msg-stream* "gnuplot returned ~a.~%"
                       (run-prog *gnuplot-path* :args
                                 (list "/noend" *gnuplot-file*))))
              ((eq ,plot :print)
               (format *gnuplot-msg-stream* "~&Sent the plot to `~a'.~%"
                       *gnuplot-printer*))
              ((format *gnuplot-msg-stream* "~&Prepared file `~a'.
Type \"load '~a'\" at the gnuplot prompt.~%"
                       *gnuplot-file* *gnuplot-file*)))
        #+unix
        (cond ((or (eq ,plot t) (eq ,plot :plot))
               (format *gnuplot-msg-stream* "Done plotting.~%"))
              ((eq ,plot :wait)
               (princ "Press <enter> to continue..." *terminal-io*)
               (force-output *terminal-io*) (read-line *terminal-io* nil nil))
              ((eq ,plot :file) (close ,str)
               (format *gnuplot-msg-stream* "Wrote `~a'.~%" *gnuplot-file*))
              ((eq ,plot :print)
               (format *gnuplot-msg-stream* "~&Sent the plot to `~a'.~%"
                       *gnuplot-printer*)
               (format *gnuplot-stream* "set terminal x11~%set output~%")))))))

(defun plot-header (str plot xlabel ylabel data-style timefmt xb xe title key
                    xtics ytics grid)
  "Print the header stuff into the stream.
This can be called ONLY by `with-plot-stream'.
The following gnuplot options are accepted:
 XLABEL YLABEL TIMEFMT XDATA DATA-STYLE TITLE XBEG XEND KEY GRID"
  (declare (stream str))
  (flet ((pp (xx) (format nil (if (numberp xx) "~g" "'~a'") xx))
         (tt (nm par)
           (case par
             ((t) (format str "set ~a~%" nm))
             ((nil) (format str "set no~a~%" nm))
             (t (format str "set ~a ~a~%" nm par)))))
    (if (eq plot :print)
        (format str "set terminal postscript landscape~%set output '~a'~%"
                *gnuplot-printer*)
        (format str "set terminal ~a~%set output~%" #+unix "x11"
                #+win32 "windows"))
    (format str "set time '%Y-%m-%d %a %H:%M:%S %Z' 0,0 'Helvetica'
set xdata~:[~%set format x '%g'~; time~%set timefmt ~:*'~a'
set format x ~:*'~a'~]~%set xlabel '~a'~%set ylabel '~a'~%set border
set data style ~a~%set xrange [~a:~a]~%set title \"~a\"~%~@[set key ~a~%~]"
            timefmt xlabel ylabel data-style (pp xb) (pp xe) title key)
    (tt "xtics" xtics) (tt "ytics" ytics) (tt "grid" grid)))

(defun plot-dl-channels (dls chs &rest opts)
  "Plot the dated lists and the channels.
This is the simple UI to `plot-dated-lists'.
The first argument is the list of dated lists,
the second is the list of channels.
The rest is passed to `plot-dated-lists'."
  (let ((begd (apply #'min (mapcar (compose date2days channel-begd) chs)))
        (endd (apply #'max (mapcar (compose date2days channel-endd) chs))))
    (incf endd (floor (- endd begd) 4))
    (setq begd (date begd) endd (date endd))
    (format *gnuplot-msg-stream*
            "~&Plotting ~d channel~:p and ~d dated list~:p in [~a -- ~a].~%"
            (length chs) (length dls) begd endd)
    (apply #'plot-dated-lists begd endd dls :channels chs opts)))

(defun plot-data-style (num-ls)
  "Decide upon the appropriate data style for the number of points."
  (when (listp num-ls)
    (setq num-ls (1- (apply #'min (mapcar #'length num-ls)))))
  (unless (realp num-ls)
    (error "plot-data-style got neither number nor list: ~s" num-ls))
  (if (> num-ls 30) "lines" "linespoints"))

(defun plot-dated-lists (begd endd dls &key (title "Plot") (xlabel "time")
                         (ylabel "value") data-style (timefmt "%Y-%m-%d") ema
                         rel (slot 'val) channels posl (plot t) plot-key
                         (xtics t) (ytics t) grid)
  "Plot the dated lists from BEGD to ENDD.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
REL means plot everything relative to the first value.
EMA is the list of parameters for Exponential Moving Averages.
CHANNELS id the list of channels to plot."
  (setq begd (if begd (date begd) (dl-nth-date (car dls)))
        endd (if endd (date endd) (dl-nth-date (car dls) -1)))
  (unless dls (error "nothing to plot for `~a'~%" title))
  (setq data-style (or data-style (plot-data-style (days-between begd endd))))
  (with-plot-stream (str plot xlabel ylabel data-style timefmt begd endd title
                     plot-key xtics ytics grid)
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
    (let ((lt 2))
      (dolist (ch channels) (plot-channel-str ch str "" (incf lt))))
    (dolist (pos posl) (plot-pos-str pos str))
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
        (mapl (lambda (ee) (setf (car ee) nil)) emal)))))

(defun plot-lists (lss &key (key #'value) (title "Plot") (xlabel "nums") rel
                   (ylabel "value") data-style depth (plot t) plot-key grid
                   (xtics t) (ytics t))
  "Plot the given lists of numbers.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
LSS is a list of lists, car of each list is the title, cdr is the numbers."
  (declare (list lss) (type (or null fixnum) depth))
  (setq depth (or depth (1- (apply #'min (mapcar #'length lss))))
        data-style (or data-style (plot-data-style depth)))
  (with-plot-stream (str plot xlabel ylabel data-style nil 0 (1- depth) title
                     plot-key xtics ytics grid)
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

(defun plot-lists-arg (lss &key (key #'identity) (title "Plot") (xlabel "nums")
                       (ylabel "value") data-style rel lines quads (plot t)
                       plot-key xbeg xend (xtics t) (ytics t) grid)
  "Plot the given lists of numbers.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
LSS is a list of lists, car of each list is the title, cdr is the list
of conses of abscissas and ordinates. KEY is used to extract the cons."
  (declare (list lss))
  (setq data-style (or data-style (plot-data-style lss)))
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
  (with-plot-stream (str plot xlabel ylabel data-style nil xbeg xend title
                     plot-key xtics ytics grid)
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

(defun plot-error-bars (ll &key (title "Plot") (xlabel "nums") (ylabel "value")
                        data-style (plot t) (xkey #'first)
                        (ykey #'second) (ydkey #'third) plot-key
                        (xtics t) (ytics t) grid)
  "Plot the list with errorbars.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
The first element is the title, all other are records from which we
get x, y and ydelta with xkey, ykey and ydkey."
  (declare (list ll))
  (setq data-style (or data-style (plot-data-style (list ll))))
  (with-plot-stream (str plot xlabel ylabel data-style nil
                     (funcall xkey (second ll)) (funcall xkey (car (last ll)))
                     title plot-key xtics ytics grid)
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

(defun plot-functions (fnl xmin xmax numpts &key (title "Function Plot")
                       (xlabel "x") (ylabel "y") data-style (plot t) plot-key
                       (xtics t) (ytics t) grid)
  "Plot the functions from XMIN to XMAX with NUMPTS+1 points.
Most of the keys are the gnuplot options (see the documentation
for `plot-header' and `with-plot-stream' for details.)
FNL is a list of (name . function).
E.g.: (plot-functions  (list (cons 'sine #'sin)) 0 pi 100)"
  (declare (list fnl) (real xmin xmax) (type (unsigned-byte 20) numpts))
  (setq data-style (or data-style (plot-data-style numpts)))
  (with-plot-stream (str plot xlabel ylabel data-style nil xmin xmax title
                     plot-key xtics ytics grid)
    (format str "plot~{ '-' using 1:2 title \"~a\"~^,~}~%" (mapcar #'car fnl))
    (dolist (fn fnl)
      (dotimes (ii (1+ numpts) (format str "e~%"))
        (declare (type (unsigned-byte 20) ii))
        (let ((xx (/ (+ (* ii (double-float xmax))
                        (* (- numpts ii) (double-float xmin))) numpts)))
          (format str "~f~20t~f~%" xx (funcall (cdr fn) xx)))))))

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
  (do ((bd (dl-nth-date dl)) (ll (dated-list-ll dl) (cdr ll))
       (ii 0 (1+ ii)) rr)
      ((or (null ll) (= ii depth)) (nreverse rr))
    (declare (fixnum ii) (type date bd) (list rr ll))
    (push (cons (days-between bd (funcall (dl-date dl) (car ll)))
                (slot-value (car ll) slot)) rr)))

(defun line-day2sec (ln begd)
  "Make a new line, converting from days to seconds."
  (declare (type line ln) (type integer begd) (values line))
  (make-line :sl (/ (line-sl ln) +day-sec+) :co
             (- (line-co ln) (* (line-sl ln) (/ begd +day-sec+)))))

(defun plot-line-str (ln beg end str &optional (title "") lt)
  "Write the string to plot stream STR for plotting the line from BEG to END.
This is not a complete plotting function (not a UI)!"
  (declare (type line ln) (real beg end))
  (format str ", ((x>~a)?((x<~a)?(~a*x+~a):1/0):1/0) title \"~a\" with lines~
~@[ ~d~]" beg end (line-sl ln) (line-co ln) title lt))

(defun plot-quad-str (qu beg end str &optional (title "") lt)
  "Write the string to plot stream STR for plotting the parabola
from BEG to END.  This is not a complete plotting function (not a UI)!"
  (declare (type (simple-array double-float (3)) qu) (real beg end))
  (format str ", ((x>~a)?((x<~a)?(~a*x*x+~a*x+~a):1/0):1/0) title \"~a\" ~
with lines~@[ ~d~]" beg end (aref qu 0) (aref qu 1) (aref qu 2) title lt))

(defun plot-pos-str (pos str)
  "Write the string to plot stream STR for plotting the position POS.
This is not a complete plotting function (not a UI)!"
  (declare (type pos pos))
  (let ((beg (plot-sec-to-epoch (pos-begd pos)))
        (end (plot-sec-to-epoch (pos-endd pos))))
    (plot-line-str (line-day2sec (pos2line pos) beg) beg end str
                   (format nil "~4f ~a/~a" (pos-size pos) (pos-begd pos)
                           (pos-endd pos)) -1)
    (plot-channel-str (pos-poch pos) str "" -1)))

(defun plot-channel-str (ch str ttl lt)
  "Write the string to plot stream STR for plotting the channel.
Omit zero lines. This is not a complete plotting function (not a UI)!"
  (declare (type channel ch) (type stream str))
  (let ((beg (plot-sec-to-epoch (channel-begd ch)))
        (end (plot-sec-to-epoch (channel-endd ch))))
    (unless (and (zerop (line-sl (channel-top ch)))
                 (zerop (line-sl (channel-top ch))))
      (plot-line-str (line-day2sec (channel-top ch) beg) beg end str ttl lt))
    (unless (and (zerop (line-sl (channel-bot ch)))
                 (zerop (line-sl (channel-bot ch))))
      (plot-line-str (line-day2sec (channel-bot ch) beg) beg end str ttl lt))
    (unless (and (zerop (line-sl (channel-reg ch)))
                 (zerop (line-sl (channel-reg ch))))
      (plot-line-str (line-day2sec (channel-reg ch) beg) beg end str ttl lt))))

(provide "gnuplot")
;;; gnuplot.lisp ends here
