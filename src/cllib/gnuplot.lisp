;;; File: <gnuplot.lisp - 1997-10-01 Wed 16:19:19 EDT - sds@WINTERMUTE.eagle>
;;;
;;; Gnuplot
;;;
;;; $Id: gnuplot.lisp,v 1.3 1997/10/01 20:48:12 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/gnuplot.lisp,v $
;;; $Log: gnuplot.lisp,v $
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

(defvar *gnuplot-path* "c:\\bin\\wgnuplot\\wgnuplot.exe"
  "*The path to the gnuplot executable")
(defvar *gnuplot-stream* nil "The current gnuplot output stream.")
(defvar *gnuplot-file* (merge-pathnames "plot.tmp" *fin-dir*)
  "*The tmp file for gnuplot.")

(defvar *gnuplot-init*
  "set xdata time
set xlabel 'time'
set ylabel 'value, $'
set data style lines
set timefmt '%Y-%m-%d'
" "*The initial string to be sent to gnuplot.")

(defun plot-dated-lists (begd endd title &rest dls)
  "Plot the dated lists from BEGD to ENDD."
  (declare (type date begd endd))
  (unless dls (error "nothing to plot for `~a'~%" title))
  (with-open-file (str *gnuplot-file* :direction :output :if-exists :supersede)
    (princ *gnuplot-init* str)
    (format str "set title '~a'~%plot~{ '-' using 1:2 title '~a'~^,~}~%"
	    title (mapcar #'dated-list-name dls))
    (dolist (dl dls)
      (do ((td (shift-dl (copy-dated-list dl) begd) (shift-dl td)))
	  ((date-more-p (dl-beg-date td) endd) (format str "e~%"))
	(format str "~a ~f~%" (dl-beg-date td) (dl-beg-misc td)))))
  (format t "File `~a' for gnuplot is prepared.
Type \"load '~a'\" at the gnuplot prompt.~%" *gnuplot-file* *gnuplot-file*))

(defun plot-contract (ctr title &optional (use-old t))
  "Plot contract history using gnuplot.
Try to use the existing gnuplot id use-old is non-nil (default)."
  (unless (and use-old *gnuplot-stream* (open-stream-p *gnuplot-stream*))
    (setq *gnuplot-stream* (make-pipe-output-stream *gnuplot-path*)))
  (unless *gnuplot-stream*
    (princ
     (concat "set xdata time\nset ylabel 'value'\nset title '" title
	     "'\nset data style lines\nset timefmt '%Y-%m-%d'\n"
	     "plot '-' using 1:5 title 'contract value'\n") *gnuplot-stream*)
    (loop for rec across ctr
	  do (princ (concat (print-contract rec) "\n") *gnuplot-stream*))
    (princ "e\n" *gnuplot-stream*)))

(defun plot-contract-file (file title &optional (use-old t))
  "Plot a contract file, using gnuplot.
The file must be saved with `clean' dates.
Try to use the existing gnuplot id use-old is non-nil (default)."
  (unless (and use-old *gnuplot-stream* (open-stream-p *gnuplot-stream*))
    (setq *gnuplot-stream* (make-pipe-output-stream *gnuplot-path*)))
  (princ
   (concat "set xdata time\nset ylabel 'value'\nset title '" title
	   "'\nset data style lines\nset timefmt '%Y-%m-%d'\n"
	   "set format x '%Y %b %e'\nplot '" file
	   "' using 1:5 title 'contract value'\n") *gnuplot-stream*))
