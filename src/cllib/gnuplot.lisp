;;; File: <gnuplot.lisp - 1997-08-28 Thursday 9:52:14 EDT - sds@WINTERMUTE.eagle>
;;; $ID$
;;; Gnuplot
;;;

(proclaim '(optimize (speed 3) (space 0) (safety 3) (debug 3)))

;(run-shell-command)
;(shell)
;(run-program)
;(make-pipe-input-stream)
;(make-pipe-io-stream)
;(make-pipe-output-stream)

(defvar *gnuplot-path* "c:\\bin\\wgnuplot\\wgnuplot.exe"
  "*The path to the gnuplot executable")
(defvar *gnuplot-stream* nil "The current gnuplot output stream.")

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
