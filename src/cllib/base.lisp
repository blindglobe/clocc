;;; File: <base.lisp - 1998-03-23 Mon 11:31:05 EST sds@mute.eaglets.com>
;;;
;;; Basis functionality, required everywhere
;;;
;;; Copyright (C) 1997 by Sam Steingold.
;;; This is free software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: base.lisp,v 1.1 1998/03/23 16:32:23 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/base.lisp,v $
;;; $Log: base.lisp,v $
;;; Revision 1.1  1998/03/23 16:32:23  sds
;;; Initial revision
;;;

(eval-when (compile load eval)
  (setq *read-default-float-format* 'double-float *print-case* :downcase)
  #+cmu (setq *gc-verbose* nil *bytes-consed-between-gcs* 32000000
	      make::*standard-binary-file-types* '("x86f")
	      make::*standard-source-file-types* '("lisp"))
  #+allegro (setq sys:*source-file-types* '("lisp"))
  #+clisp (setq *warn-on-floating-point-contagion* t
		*default-float-format* 'double-float
		;; sys::*source-file-types* '(#".lisp") ; default
		sys::*prompt-with-package* t)
  #+gcl (defmacro lambda (bvl &body forms) `#'(lambda ,bvl ,@forms))
  #+allegro
  (unless (member :key (arglist #'reduce) :test #'string=)
    (setq franz:*compile-advice* t) (in-package :franz)
    (defadvice reduce (support-key :before)
      (let ((key (getf (cddr arglist) :key)))
	(when key
	  (remf (cddr arglist) :key)
	  (setf (second arglist)
		(map 'vector key (second arglist))))))
    (in-package :user)))

;;; {{{ Extensions

(eval-when (compile load eval)
  (unless (fboundp 'defsubst)
    (defmacro defsubst (name arglist &body body)
      "Declare an inline defun."
      `(progn (declaim (inline ,name)) (defun ,name ,arglist ,@body))))

  (unless (fboundp 'defcustom)
    (defmacro defcustom (name type init doc)
      "Define a typed variable."
      `(progn (declaim (type ,type ,name))
	(defvar ,name (the ,type ,init) ,doc))))

  (unless (fboundp 'defconst)
    (defmacro defconst (name type init doc)
      "Define a typed constant."
      `(unless (boundp ',name) (declaim (type ,type ,name))
	(defconstant ,name (the ,type ,init) ,doc))))

  (unless (fboundp 'double-float)
    (declaim (ftype (function (number) double-float) double-float))
    (defsubst double-float (num)
      "Coerce to double float."
      (declare (number num) (optimize (speed 1)))
      (coerce num 'double-float)))

  (unless (fboundp 'getenv)
    (defun getenv (var)
      "Return the value of the environment variable."
      (#+cmu xlib::getenv #+(or allegro clisp) system::getenv
       #+lispworks w:environment-variable #+lucid lcl:environment-variable
       #+gcl si:getenv (string var)))))

(defun run-prog (prog &rest opts &key args wait &allow-other-keys)
  "Common interface to shell. Doesn't return anything useful."
  #+(or clisp gcl) (declare (ignore wait))
  (remf opts :args) (remf opts :wait)
  #+excl (apply #'excl:run-shell-command (apply #'vector prog prog args)
		:wait wait opts)
  #+cmu (run-program prog args :wait wait)
  #+gcl (apply #'run-process prog args)
  #+clisp (apply #'run-program prog :arguments args opts))

(defun pipe-output (prog &rest args)
  "Return an output stream wich will go to the command."
  #+excl (apply #'excl:run-shell-command (apply #'vector prog prog args)
		:input :stream)
  #+gcl (si::fp-input-stream (apply #'run-process prog args))
  #+clisp (make-pipe-output-stream (format nil "~a~{ ~a~}" prog args))
  #+cmu (process-output (run-program prog args :input :stream :wait nil)))

;;; }}}{{{ Environment

(eval-when (compile load eval)
  (defcustom *source-dir* pathname
    (merge-pathnames #+win32 "fx/" #+unix "eagle/" (user-homedir-pathname))
    "*The directory with the Eagle source code.")
  (defcustom *lisp-dir* pathname
    (merge-pathnames "lisp/" (user-homedir-pathname))
    "*The private lisp sources."))
(defcustom *fin-dir* pathname (merge-pathnames "data/" *source-dir*)
  "*The directory with the financial data.")
(defcustom *current-project* list nil
  "*The list of files to work with, in the order of loading.")

#-(or clisp allegro)
(eval-when (eval load compile)
(defun setf-values-aux (places env)
  (do ((temps nil) (vals nil) (stores nil) (storeforms nil)
       (accessforms nil) (placesr places))
      ((atom placesr)
       (setq temps (nreverse temps) vals (nreverse vals)
	     stores (nreverse stores) storeforms (nreverse storeforms)
	     accessforms (nreverse accessforms))
       (values temps vals stores storeforms accessforms))
    (multiple-value-bind (SM1 SM2 SM3 SM4 SM5)
        (get-setf-method (pop placesr) env)
      (setq temps (revappend SM1 temps) vals (revappend SM2 vals)
	    stores (revappend SM3 stores) storeforms (cons SM4 storeforms)
	    accessforms (cons SM5 accessforms)))))

(define-setf-method values (&rest subplaces &environment env)
  (multiple-value-bind (temps vals stores storeforms accessforms)
      (setf-values-aux subplaces env)
    (values temps vals stores `(values ,@storeforms) `(values ,@accessforms))))
)

(eval-when (compile load eval)
  (unless (boundp '*require-table*)
    (defcustom *require-table* hash-table
      (make-hash-table :test #'equal :size 20)
      "*The table for `sds-require'.")
    (dolist (mo '("base" "channel" "currency" "date" "futures" "fx" "gnuplot"
		  "lisp" "math" "octave" "print" "report" "signal" "util"
		  "work"))
      (setf (gethash mo *require-table*)
	    (merge-pathnames (concatenate 'string mo ".lisp") *source-dir*)))
    (dolist (mo '("gq" "url" "geo"))
      (setf (gethash mo *require-table*)
	    (merge-pathnames (concatenate 'string mo ".lisp") *lisp-dir*)))
    (setf (gethash "monitor" *require-table*)
	  (merge-pathnames "metering.lisp" *lisp-dir*))
    (defun sds-require (fl)
      (let ((file (gethash fl *require-table*)) comp)
	(unless file (error "Cannot require `~a'." fl))
	(setq comp (compile-file-pathname file))
	(require fl (if (probe-file comp) comp file))
	(unless (member fl *modules* :test (hash-table-test *require-table*))
	  (error "File `~a' failed to provide `~a'." file fl))))))

(defun autoload (symb file comment)
  "Declare symbol SYMB to call a function defined in FILE."
  (declare (symbol symb) (simple-string comment) (optimize (speed 1)))
  (unless (fboundp symb)
    (setf (symbol-function symb)
	  (lambda (&rest args)
	    (setf (documentation symb 'function) nil)
	    (fmakunbound symb) (sds-require file) (apply symb args))
	  (documentation symb 'function)
	  (format nil "Autoloaded (from ~a):~%~a" file comment))))

#+(or allegro gcl)
(defun default-directory ()
  "The default directory."
  #+allegro (current-directory)
  #+gcl (truename "."))
#+allegro (defsetf default-directory chdir "Change the current directory.")
#+gcl (defsetf default-directory si:chdir "Change the current directory.")
#-allegro (defun chdir (dir) (setf (default-directory) dir))

(defun all-docs (sy &key (out *standard-output*))
  "Print all the possible information about the symbol.
Prints function, compiler-macro, setf, structure, type, and variable
documentation (if any), as well as the value and the arglist (if any)."
  (declare (stream out))
  (format out "~& *** Symbol: `~a':" sy)
  (describe sy out)
  (do ((ty '(compiler-macro setf structure type variable function) (cdr ty))
       (ar (ignore-errors (function-arglist (symbol-function sy)))) doc)
      ((endp ty)
       (when (fboundp sy) (format out "~& *** Args: ~s~%" ar))
       (when (boundp sy) (format out "~& *** Value: ~s~%" (symbol-value sy)))
       (when (symbol-plist sy)
	 (format out "~& *** Plist: ~s~%" (symbol-plist sy)))
       (values))
    (when (setq doc (documentation sy (car ty)))
      (format out "~& *** Documentation as a ~a:~%~a~%" (car ty) doc))))

;;; 1997-11-03 Mon 09:37:59 EST
;;; from Bruno Haible <haible@ma2s2.mathematik.uni-karlsruhe.de>
#+clisp
(defun step-on ()
  "Turn on stepping after break."
  (declare (compile))
  (setq sys::*step-level* 0
        sys::*step-quit* most-positive-fixnum
        *evalhook* #'sys::step-hook-fn))
#+clisp
(defun step-off ()
  "Turn off stepping. `continue' to proceed."
  (declare (compile))
  (setq sys::*step-quit* 0))

;;;
;;; }}}{{{ Environment
;;;

(eval-when (load compile eval)
  (defconst *internal-time-digits* fixnum
    (min 3 (round (/ (log internal-time-units-per-second) (log 10))))
    "The number of digits in `internal-time-units-per-second'."))

(defun print-time (sec &optional (out t) pre post)
  "Print the seconds to the output stream."
  (declare (real sec) (type (or null simple-string) pre post))
  (multiple-value-bind (mi se ho da) (floor (double-float sec) 60)
    (declare (fixnum mi) (double-float se) (type (or null fixnum) ho da))
    (setf (values ho mi) (floor mi 60) (values da ho) (floor ho 24))
    (format out "~@[~a~]~[~:;~:*~:d day~:p ~]~2,'0d:~2,'0d:~2,'0d~@[~a~]"
	    pre da ho mi (round se) post)))
;;    (format out "~@[~a~]~[~:;~:*~:d day~:p ~]~[~:;~:*~dh ~]~[~:;~:*~d' ~]~
;; ~[~:;~:*~d\"~]~@[~a~]" pre da ho mi (round se) post)))

(defun elapsed (bt &optional fmt)
  "Return the time in seconds elapsed since BT,
previously set using `get-internal-real-time'.
If FMT is non-NIL, return the corresponding string too."
  (declare (integer bt) (optimize (speed 1)))
  (let ((nn (double-float (/ (- (get-internal-real-time) bt)
			     internal-time-units-per-second))))
    (declare (double-float nn))
    (if fmt
	(values nn (concatenate 'string (commas nn *internal-time-digits*)
				" sec" (if (< nn 300) ""
					   (print-time nn nil " (" ")"))))
	nn)))

(defsubst elapsed-1 (bt)
  "Return just the string for `elapsed'."
  (declare (integer bt)) (nth-value 1 (elapsed bt t)))

(defun current-environment (&optional (stream *standard-output*))
  "Print the current environment to a stream."
  (declare (stream stream))
  (format stream "~&~%~70~~%~70,,,'-:@<<{[ The current environment]}>~>~%~
LISP: implementation: ~a; version: ~a~%Machine: type: ~
~a; version: ~a; instance: ~a~%OS: "
	  (lisp-implementation-type) (lisp-implementation-version)
	  (machine-type) (machine-version) (machine-instance))
  #+win32
  (if (system::registry "SOFTWARE\\Microsoft\\Windows\\CurrentVersion"
			"ProductName")
      (win95-sysinfo stream)
      (winnt-sysinfo stream))
  #+os/2 (format stream "OS/2")
  #+unix (format stream "Unix")
  #+dos (format stream "DOS")
  #+pc386 (format stream "PC386")
  #+amiga (format stream "Exec/AmigaDOS")
  (multiple-value-bind (used avail) (room nil)
    (format stream "~%Software: type: ~a; version: ~a~%Site:~35t~a (~a)
User home:~35t~a~%Current directory:~35t~a~%Default pathname:~35t~a
Features:~{ ~a~}~%Modules:~{ ~a~}~%Current package:~35t~a;
Current language:~35t~a~%Bytes currently in use:~35t~10:d
Bytes available until next GC:~35t~10:d~%Internal time unit:~35t~f sec
Current time:~35t"
	    (software-type) (software-version) (long-site-name)
	    (short-site-name) (user-homedir-pathname) (default-directory)
	    *default-pathname-defaults* *features* *modules* *package*
	    #+clisp (system::current-language) #-clisp nil used avail
	    (/ 1 internal-time-units-per-second)))
  (current-time stream)
  (format stream "~%~70~~%"))

(defcustom *month-names* simple-vector
  '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "*The names of the months.")
(defcustom *week-days* simple-vector
  '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "*The names of the days of the week.")
(defcustom *time-zones* list
  '((5 "EDT" . "EST") (6 "CDT" . "CST") (7 "MDT" . "MST") (8 "PDT" . "PST")
    (0 "GMT" . "GDT"))
  "*The string representations of the time zones.")

(defun current-time (&optional (stream t))
  "Print the current time to the stream (defaults to t)."
  (multiple-value-bind (se mi ho da mo ye dw dst tz) (get-decoded-time)
    (declare (fixnum se mi ho da mo ye dw tz))
    (format stream "~4d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d ~a(~@d)"
	    ye mo da (aref *week-days* dw) ho mi se
	    (funcall (if dst #'cadr #'cddr) (assoc tz *time-zones*)) tz)))

#+win32
(defun win95-sysinfo (&optional (stream t))
  "Print the MS Windows system information."
  (flet ((env (str)
	   (system::registry "SOFTWARE\\Microsoft\\Windows\\CurrentVersion"
			     str)))
    (format stream "~a (~a - ~a; boot: ~a)~%Registered to: ~a, ~a [~a]"
	    (env "ProductName") (env "Version") (env "VersionNumber")
	    (env "BootCount") (env "RegisteredOwner")
	    (env "RegisteredOrganization") (env "ProductId"))))

#+win32
(defun winnt-sysinfo (&optional (stream t))
  "Print the WinNT system information."
  (flet ((env (str)
	   (system::registry "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion"
			     str)))
    (format stream "WinNT ~a (build ~a: ~a) ~a~%Registered to: ~a, ~a [~a]"
	    (env "CurrentVersion") (env "CurrentBuildNUmber")
	    (env "CSDVersion") (env "CurrentType") (env "RegisteredOwner")
	    (env "RegisteredOrganization") (env "ProductId"))))

(provide "base")
;;; }}} base.lisp ends here
