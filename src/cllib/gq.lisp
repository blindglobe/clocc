;;; File: <gq.lisp - 1998-03-10 Tue 13:26:49 EST sds@mute.eaglets.com>
;;;
;;; GetQuote
;;; get stock/mutual fund quotes from the Internet
;;; via the WWW using HTTP/1.0, save into a file, plot.
;;;
;;; Copyright (C) 1997 by Sam Steingold.
;;; This is free software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: gq.lisp,v 1.3 1998/03/10 18:29:20 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/gq.lisp,v $
;;; $Log: gq.lisp,v $
;;; Revision 1.3  1998/03/10 18:29:20  sds
;;; Replaced `multiple-value-set*' with `(setf (values ))'.
;;;
;;; Revision 1.2  1997/10/31 21:59:41  sds
;;; Added cite-info and strip-html-markup.
;;;
;;; Revision 1.1  1997/10/15 15:49:26  sds
;;; Initial revision
;;;
;;;
;;; To run regularly (s/a gq.bat)
;;; at 19:00 /every:M,T,W,Th,F "c:\bin\clisp\lisp.exe -M c:\bin\clisp\lispinit.mem -q -i c:/home/sds/lisp/gq -x \"(update-quotes :plot nil)\""

(eval-when (load compile eval)
  (let ((dir #+unix "/home/sds/eagle/" #+win32 "c:/home/sds/fx/")
	(*load-verbose* nil) (*load-print* nil))
    (unless (boundp '*require-table*) (load (concatenate 'string dir "util")))
    (sds-require 'date) (sds-require 'url) (sds-require 'gnuplot)))


(defun gq-complete-url (url &rest ticks)
  "Complete URL to get the quotes for TICKS."
  (setq url (if (url-p url) (copy-url url) (url url)))
  (setf (url-path url) (format nil "~a~{+~:@(~a~)~}" (url-path url) ticks))
  url)

;;;
;;; {{{ Daily Data
;;;

(defstruct (daily-data (:print-function print-dd) (:conc-name dd-))
  (nav 0.0 :type double-float)
  (chg 0.0 :type double-float)
  (prc 0.0 :type double-float)
  (bid 0.0 :type double-float)
  (ask 0.0 :type double-float)
  (pre 0.0 :type double-float)
  (low 0.0 :type double-float)
  (hgh 0.0 :type double-float))

(defun print-dd (dd &optional stream depth)
  "Print the daily datum."
  (declare (ignore depth) (type daily-data dd))
  (format stream
	  "price:~15t~7,2f~35tbid:~45t~7,2f
previous:~15t~7,2f~35task:~45t~7,2f
change:~15t~7,2f~35thigh:~45t~7,2f
%:~15t~7,2f~35tlow:~45t~7,2f~%"
	  (dd-nav dd) (dd-bid dd) (dd-pre dd) (dd-ask dd)
	  (dd-chg dd) (dd-hgh dd) (dd-prc dd) (dd-low dd)))

(defun gq-fix-date (dt)
  "Fix the date to be the last date when the quote could be available."
  (let ((td (today)))
    (if (and (or (null dt) (equalp dt td))
	     (< (nth-value 2 (decode-universal-time
			      (get-universal-time))) 17))
	(tomorrow td -1) (or dt td))))

(defun get-quotes-apl (url &rest ticks)
  "Get the data from the APL WWW server."
  (with-open-url (sock (apply #'gq-complete-url url ticks))
    (do (zz res dt (ts (make-text-stream :sock sock)))
	((or (null ticks) (eq (setq zz (next-token ts)) *eof*))
	 (cons (gq-fix-date dt) (nreverse res)))
      (when (eq (car ticks) zz)
	(pop ticks)
	(push (make-daily-data
	       :nav (next-token ts) :chg (next-token ts) :prc (next-token ts)
	       :bid (next-token ts 3 'float 0.0)
	       :ask (next-token ts 1 'float 0.0)
	       :pre (next-token ts 3 'float 0.0)
	       :low (next-token ts 2 'float 0.0)
	       :hgh (next-token ts 1 'float 0.0)) res)
	;; (setq dt (infer-date (next-token ts 3) (next-token ts)))
	))))

(defun get-quotes-pf (url &rest ticks)
  "Get the data from the PathFinder WWW server."
  (with-open-url (sock (apply #'gq-complete-url url ticks))
    (do (zz res dt (ts (make-text-stream :sock sock)))
	((or (null ticks) (eq (setq zz (next-token ts)) *eof*))
	 (cons (gq-fix-date dt) (nreverse res)))
      (if dt
	  (when (eq (car ticks) zz)
	    (pop ticks)
	    (push (make-daily-data :nav (next-number ts 6)) res))
	  (when (and (eq zz 'latest) (eq (next-token ts) 'prices))
	    (setq dt (infer-date (next-token ts) (next-token ts))))))))

(defun get-quotes-sm (url &rest ticks)
  "Get the data from the StockMaster WWW server."
  (do ((ti ticks (cdr ti)) ds vs hh ar res dt ts)
      ((null ti)
       (values (cons dt (nreverse res))
	       (apply #'mapcar
		      (lambda (&rest cns)
			(unless (every (lambda (cn)
					 (equalp (caar cns) (car cn)))
				       (cdr cns))
			  (error "Date mismatch: ~a~%" cns))
			(make-hist :date (caar cns) :navs (mapcar #'cdr cns)))
		      (nreverse hh))
	       (nreverse ar)))
    (with-open-url (sock (gq-complete-url url (car ti)))
      (skip-tokens (setq ts (make-text-stream :sock sock))
		   (car ti) :key (safe-fun #'car #'consp))
      (push (make-daily-data :nav (next-number ts) :chg (next-number ts)
			     :prc (next-number ts)) res)
      (setq dt (infer-date (next-token ts 4) (next-token ts)))
      (push (list (next-number ts 5) (next-number ts)
		  (next-number ts) (next-number ts)) ar)
      (next-token ts)
      (setq ds nil vs nil)
      (push (mapcar
	     #'cons
	     (dotimes (ii 10 (nreverse ds))
	       (push (infer-date (next-token ts) (next-token ts)) ds))
	     (dotimes (ii 10 (nreverse vs))
	       (push (next-number ts) vs))) hh))))

(defvar *get-quote-url-list*
  (list (list (make-url :prot "http" :host "www.stockmaster.com"
			:path "/cgi-bin/graph?sym=")
	      'get-quotes-sm "StockMaster")
	(list (make-url :prot "http" :host "qs.secapl.com"
			:path "/cgi-bin/qs?ticks=")
	      'get-quotes-apl "APL")
	(list (make-url :prot "http" :host "quote.pathfinder.com"
			:path "/money/quote/qc?symbols=")
	      'get-quotes-pf "PathFinder"))
  "*The list of known URL templates for getting quotes.")

(defun get-quotes (server &rest ticks)
  "Get the quotes from one of `*get-quote-url-list*'.
The first arg, SERVER, when non-nil, specifies which server to use."
  (if server
      (let ((qq (if (numberp server) (elt *get-quote-url-list* server)
		    (find server *get-quote-url-list* :key #'third
			  :test #'string-equal))))
	(unless qq (error "Unknown server `~a'.~%" server))
	(apply #'values (third qq)
	       (multiple-value-list
		   (apply (second qq) (first qq) ticks))))
      (do ((ql *get-quote-url-list* (cdr ql)) res name)
	  ((or (endp ql) (car res)) (values-list (cons name res)))
	(format t "~&Trying ~a..." (setq name (caddar ql)))
	(setq res (multiple-value-list
		      (ignore-errors (apply (cadar ql) (caar ql) ticks))))
	(format t "~:[failed...~;success!~]~%" (car res)))))

(defun infer-date (mon day)
  "Guess what the date is.
Return the most recent date with these month and day."
  (multiple-value-bind (se mi ho da mo ye) (get-decoded-time)
    (declare (ignore se mi ho da))
    (setq mon (infer-month mon))
    (make-date :da day :mo mon :ye (if (<= mon mo) ye (1- ye)))))

;;;
;;; }}}{{{ Holdings
;;;

(defvar *hist-data-file*
  (merge-pathnames "text/invest.txt" (user-homedir-pathname))
  "*The file with the historical data.")
(defvar *hist-data-file-header*
  ";*GetQuote portfolio*
; file format is as follows:
; - lines starting with `;' are ignored
; - empty lines are ignored
; - all non-ignored lines until the separator token, `~',
; are expected to contain ticker info in the following format
;     <ticker> <number of shares> <buying price> \"<comment (fund name)>\"
; - all the lines after that are history of this portfolio, in the format
;     <date> <total value> [<price>]*
" "The header of the data file.")
(defvar *hist-data-file-sep* '~
  "*The separator between the portfolio and its history")
(defvar *holdings* nil "The holdings, to be read from `*hist-data-file*'.")
(defvar *history* nil "The history, to be read from `*hist-data-file*'.")

(defstruct (pfl (:print-function print-pfl))
  (tick nil :type symbol)
  (nums 0.0 :type double-float)
  (bprc 0.0 :type double-float)
  (name "" :type string))

(defun read-pfl (stream ra)
  "Read a PFL from the STREAM, using the read-ahead RA.
Suitable for `read-list-from-stream'."
  (declare (stream stream))
  (values (make-pfl :tick ra :nums (read stream) :bprc (read stream)
		    :name (read stream)) (read stream nil *eof*)))

(defun print-pfl (pfl &optional stream depth)
  "Print the PFL struct."
  (declare (type pfl pfl) (ignore depth))
  (format stream "~:@(~a~) ~8,3f ~7,2f ~s" (pfl-tick pfl) (pfl-nums pfl)
	  (pfl-bprc pfl) (pfl-name pfl)))

(defstruct (hist (:print-function print-hist))
  (date (make-date) :type date)
  (totl 0.0 :type double-float)
  (navs nil :type list))

(defun read-hist (stream ra)
  "Read a HIST from the STREAM, using the read-ahead RA.
Suitable for `read-list-from-stream'."
  (declare (stream stream))
  (do ((hist (make-hist :date (date ra) :totl (read stream)))
       (vl (read stream) (read stream nil *eof*)))
      ((not (numberp vl))
       (progn (nreverse (hist-navs hist)) (values hist vl)))
    (push vl (hist-navs hist))))

(defun print-hist (hist &optional stream depth)
  "Print the HIST struct."
  (declare (type hist hist) (ignore depth))
  (format stream "~a ~15,5f~{ ~7,2f~}" (hist-date hist)
	  (hist-totl hist) (hist-navs hist)))

(defun hist-totl-comp (hold navs)
  "Compute the correct total from the hist records and holdings list."
  (declare (list hold navs))
  (let ((tot 0))
    (map nil (lambda (ho vl) (incf tot (* vl (pfl-nums ho)))) hold navs)
    tot))

(defun read-data-file (file)
  "Return 2 values: the list of holdings and the history list."
  (with-open-file (fl file :direction :input :if-does-not-exist nil)
    (unless fl
      (return-from read-data-file
	(format t " *** Cannot open file `~a' for reading~%" file)))
    (values (read-list-from-stream fl #'read-pfl *hist-data-file-sep*)
	    (read-list-from-stream fl #'read-hist))))

(defun save-data (file hold hist)
  "Save the history into the file.
If the file doesn't exist, it is created. If it exists,
only the data after `*hist-data-file-sep*' is changed."
  (with-open-file (outst file :direction :io :if-exists :overwrite)
    (cond ((read outst nil nil)		; file existed
	   (format t "File `~a' exists. Appending...~%" file)
	   (do (zz)
	       ((or (eq zz *eof*) (eq zz *hist-data-file-sep*))
		(when (eq zz *eof*)
		  (error "File `~a' is corrupted: `~a' not found.~%"
			 file *hist-data-file-sep*)))
	     (setq zz (read outst nil *eof*)))
	   (terpri outst)
	   (write-list-to-stream hist outst #'print-hist))
	  (t				; new file
	   (format t "File `~a' does not exists. Creating...~%" file)
	   (princ *hist-data-file-header* outst)
	   (terpri outst)
	   (write-list-to-stream hold outst #'print-pfl)
	   (terpri outst) (princ *hist-data-file-sep* outst) (terpri outst)
	   (write-list-to-stream hist outst #'print-hist))))
  (format t "~d record~:p about ~r holding~:p written.~%"
	  (length hist) (length hold)))

(defun gq-fix-hist (hold hist hhh)
  "Fix the values in the history. Return T if something was fixed."
  (declare (list hist hold hhh))
  (let ((fixed nil))
    (when hhh
      (setq hhh
	    (map-sorted
	     'list
	     (lambda (hi hs)
	       (when hs
		 (cond (hi
			(unless (every #'= (hist-navs hi) (hist-navs hs))
			  (format t "Incorrect NAVs on ~a:
~5tOriginal:~15t~{~7,2f~}~%~5tActual:~15t~{~7,2f~}~%"
				  (hist-date hi) (hist-navs hi) (hist-navs hs))
			  (setf (hist-navs hi) (hist-navs hs)
				(hist-totl hi)
				(hist-totl-comp hold (hist-navs hs))
				fixed t)))
		       (t
			(setf (hist-totl hs)
			      (hist-totl-comp hold (hist-navs hs)))
			(format t "Missing record added: [~a].~%" hs)
			(setq hi hs fixed t))))
	       hi)
	     #'date-less-p hist hhh :ckey #'hist-date))
      ;; modify hist by side effect
      (setf (car hist) (car hhh) (cdr hist) (cdr hhh)))
    (let (ntot)
      (dolist (hr hist)
	(setq ntot (hist-totl-comp hold (hist-navs hr)))
	(unless (same-num-p ntot (hist-totl hr) 0.01d0 0.00001d0)
	  (format t "Incorrect total (~a): ~15,5f; correct: ~15,5f~%"
		  (hist-date hr) (hist-totl hr) ntot)
	  (setf (hist-totl hr) ntot fixed t))))
    fixed))

(defun process-results (hold hist srv res yea &optional (str t))
  "Process the results, update the history.
Return non-nil if the history was updated and needs to be saved.
RES is a list of SERVER, DATE and DAILY-RECS.
YEA is the list of 1, 3, 5, and 10-year returns."
  (declare (list hold hist res))
  (unless res
    (return-from process-results (format t "no results - timed out?~%")))
  (let ((lh (last hist)) cv ov pv (ctot 0.0d0) (otot 0.0d0) (ptot 0.0d0)
	(begd (hist-date (car hist))) pers apy db
	(nnavs (mapcar #'dd-nav (cdr res)))
	(pnavs (mapcar #'dd-pre (cdr res))))
    (declare (double-float ctot otot))
    (format str "~2%~72:@<~a results [~a]:~>
~72:@<<[~a -- ~a (~:d days)]>~>~2%"
	    srv (current-time nil) begd (car res)
	    (setq db (days-between begd (car res))))
    (apply #'mapc
	   (lambda (hl dd &optional ye)
	     (format str "Fund: ~:@(~a~) [~a]~48t~7,3f * ~6,2f = $~a~%"
		    (pfl-tick hl) (pfl-name hl) (pfl-nums hl) (dd-nav dd)
		    (commas (setq cv (* (pfl-nums hl) (dd-nav dd))) 2 6))
	     (when ye
	       (format str "~10t[1ye:~6,2f%; 3ye:~6,2f%; 5ye:~6,2f%; ~
10ye:~6,2f%]~%" (first ye) (second ye) (third ye) (fourth ye)))
	     (print-dd dd str)
	     (setq ov (* (pfl-nums hl) (pfl-bprc hl))
		   pv (* (pfl-nums hl)
			 (cond ((not (zerop (dd-pre dd))) (dd-pre dd))
			       ((not (zerop (dd-chg dd)))
				(- (dd-nav dd) (dd-chg dd)))
			       (t (elt (hist-navs (car lh))
				       (position (pfl-tick hl) hold :test #'eq
						 :key #'pfl-tick))))))
	     (incf ctot cv) (incf otot ov) (incf ptot pv)
	     (setf (values pers apy) (percent-change ov cv db))
	     (format str "   P/L: total:~15t~a - ~a = ~a~55t[~7,3f% APY:~8,3f%]
        today:~15t~a - ~a = ~a~55t[~7,3f%]~2%"
		     (commas cv 2 7) (commas ov 2 7) (commas (- cv ov) 2 5)
		     pers apy (commas cv 2 7) (commas pv 2 7)
		     (commas (- cv pv) 2 5) (percent-change pv cv)))
	   hold (rest res) (if yea (list yea) nil))
    (setf (values pers apy) (percent-change otot ctot db))
    (format str " * P/L: total:~15t~a - ~a = ~a~55t[~7,3f% APY:~8,3f%]
        today:~15t~a - ~a = ~a~55t[~7,3f%]~2%"
	    (commas ctot 2 7) (commas otot 2 7) (commas (- ctot otot) 2 5)
	    pers apy (commas ctot 2 7) (commas ptot 2 7)
	    (commas (- ctot ptot) 2 5) (percent-change ptot ctot))
    (cond ((date-more-p (car res) (hist-date (car lh)))
	   (unless (or (equalp (tomorrow (hist-date (car lh))) (car res))
		       (every (lambda (cv pv)
				(or (zerop pv)
				    (same-num-p cv pv 0.01d0 0.001d0)))
			      (hist-navs (car lh)) pnavs))
	     (format str "A discrepancy found:~% last record:~15t~{~7,2f~}~% ~
previous day:~15t~{~7,2f~}~%Added an extra record~%~5t~{~a~}~%"
		     (hist-navs (car lh)) pnavs
		     (setf (cdr lh)
			   (list (make-hist :date (tomorrow (car res) -1)
					    :navs pnavs :totl
					    (hist-totl-comp hold pnavs)))))
	     (setq lh (cdr lh)))
	   (cond ((equalp nnavs (hist-navs (car lh)))
		  (format str "Same NAVs as on ~a, no record added.~%"
			  (hist-date (car lh))))
		 (t (format str "A record for date ~a added:~%~5t~{~a~}~%"
			    (car res)
			    (setf (cdr lh)
				  (list (make-hist
					 :date (car res) :navs nnavs
					 :totl (hist-totl-comp hold
							       nnavs)))))
		    t)))
	  (t (format str "Old news [~a].~%" (car res))))))

(defun plot-portfolio (hold hist plot)
  "Plot the portfolio."
  (declare (list hold hist))
  (do ((i 0 (1+ i)) (hl hold (cdr hl)) res)
      ((null hl)
       (plot-dated-lists
	(hist-date (car hist)) (hist-date (car (last hist)))
	(cons (make-dated-list :ll hist :date #'hist-date :name
			       "Total Value" :misc #'hist-totl)
	      (nreverse res)) :rel t :slot 'misc
	:title "Portfolio History" :data-style "linespoints"
	:ylabel "Relative Value" :plot plot))
    (push (make-dated-list
	   :ll hist :date #'hist-date :name (pfl-name (car hl))
	   :code (pfl-tick (car hl)) :misc
	   (let ((i i)) (lambda (hs) (elt (hist-navs hs) i)))) res)))

;;;
;;; }}}{{{ Run the thing
;;;

#+win32
(defvar *gq-log* (merge-pathnames "text/getquote.log" (user-homedir-pathname))
  "The log file for the batch processing.")

(defun update-quotes (&key (plot nil plotp) server)
  "Read the history. Update quotes. Plot (optionally),
if PLOT is non-nil, or if it is not given but there was new data.
If PLOT is T, just plot, do not try to update quotes."
  (let ((ia (eq *standard-output* *standard-input*))) ; interactive
    #+win32 (unless ia
	      (setq *hist-data-file* (pathname "c:/home/sds/text/invest.txt")))
    (setf (values *holdings* *history*) (read-data-file *hist-data-file*))
    (unless (eq plot t)
      (multiple-value-bind (srv res hhh yea str fixed new)
	  (apply #'get-quotes server (mapcar #'pfl-tick *holdings*))
	(setq str (or ia #-win32 t
		      #+win32 (open *gq-log* :direction :output :if-exists
				    :append :if-does-not-exist :create))
	      fixed (gq-fix-hist *holdings* *history* hhh)
	      new (process-results *holdings* *history* srv res yea str))
	(when (or fixed new)
	  (save-data *hist-data-file* *holdings* *history*)
	  (unless plotp (setq plot t)))
	(unless (eq t str) (close str))))
    (when plot (plot-portfolio *holdings* *history* (if ia 'plot 'wait)))))

(provide "gq")
;;; }}} gq.lisp ends here
