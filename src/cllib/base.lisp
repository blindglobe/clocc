;;; File: <base.lisp - 1998-06-19 Fri 16:17:10 EDT sds@mute.eaglets.com>
;;;
;;; Basis functionality, required everywhere
;;;
;;; Copyright (C) 1997, 1998 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: base.lisp,v 1.4 1998/06/19 20:17:10 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/base.lisp,v $
;;; $Log: base.lisp,v $
;;; Revision 1.4  1998/06/19 20:17:10  sds
;;; Added `*prompt*'.  Ditched `step-on' and `step-off'.
;;;
;;; Revision 1.3  1998/06/12 18:54:05  sds
;;; Made `compose-m' more flexible, renamed it `compose', renamed the
;;; function `compose' into `compose-f'.
;;; Changed most variables into constants.
;;;
;;; Revision 1.2  1998/05/21 18:22:15  sds
;;; Adopted to work with ACL 5.
;;;
;;; Revision 1.1  1998/03/23 16:32:23  sds
;;; Initial revision
;;;

;;(defpackage finance
;;  (:size 1000) (:nicknames "EAGLE" "FIN") (:use "CL" "LISP")
;;  (:shadowing-import-from "CL" "FLET" "MAKE-PACKAGE" "MACROLET" "LABELS"))
(in-package :cl-user)

(eval-when (compile load eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3)
                     #+cmu-sds-ignore (ext:inhibit-warnings 3))
	   #+(or clisp gcl) (declaration values))
  (setq *read-default-float-format* 'double-float *print-case* :downcase
	*print-array* t)
  #+cmu (setq *gc-verbose* nil *bytes-consed-between-gcs* 32000000
              *efficiency-note-cost-threshold* 20)
  ;; #+cmu (push "x86f" make::*standard-binary-file-types*)
  ;; #+cmu (push "lisp" make::*standard-source-file-types*) ; default
  #+allegro
  (progn (setq excl:*global-gc-behavior* :auto
               excl::stream-buffer-size 8192)
         (tpl:setq-default *read-default-float-format* 'double-float)
         (tpl:setq-default *print-case* :downcase)
         (push "lisp" sys:*source-file-types*))
  #+clisp (setq lisp:*warn-on-floating-point-contagion* nil
                lisp:*floating-point-contagion-ansi* t
		lisp:*default-float-format* 'double-float
		;; sys::*source-file-types* '(#".lisp") ; default
		sys::*prompt-with-package* t)
  #+gcl (defmacro lambda (bvl &body forms) `#'(lambda ,bvl ,@forms))
  #+allegro-v4.3
  (unless (member :key (excl:arglist #'reduce) :test #'string=)
    (setq franz:*compile-advice* t) (in-package :franz)
    (excl:defadvice reduce (support-key :before)
      (let ((key (getf (cddr excl:arglist) :key)))
	(when key
	  (remf (cddr excl:arglist) :key)
	  (setf (second excl:arglist)
		(map 'vector key (second excl:arglist))))))
    (in-package :cl-user)))

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
      `(eval-when (compile load eval)
	(unless (boundp ',name) (declaim (type ,type ,name))
		(defconstant ,name (the ,type ,init) ,doc)))))

  (unless (fboundp 'double-float)
    (defsubst double-float (num)
      "Coerce to double float."
      (declare (number num) (optimize (speed 1)) (values double-float))
      (coerce num 'double-float)))

  (unless (fboundp 'getenv)
    (defun getenv (var)
      "Return the value of the environment variable."
      #+cmu ;; xlib::getenv
      (cdr (assoc (string var) *environment-list* :test #'equalp)) #-cmu
      (#+(or allegro clisp) system::getenv #+lispworks w:environment-variable
       #+lucid lcl:environment-variable #+gcl si:getenv (string var)))))

(defun package-short-name (pkg)
  "Return the shortest (nick)name of the package."
  (declare (type package pkg) (values simple-string))
  (reduce (lambda (st0 st1)
            (declare (simple-string st0 st1))
            (if (> (length st0) (length st1)) st1 st0))
          (package-nicknames pkg) :initial-value (package-name pkg)))

#+(or cmu clisp)
(let* ((dumb (equalp (getenv "TERM") "dumb")) (bb (if dumb "" "[1m"))
       (ib (if dumb "" "[7m")) (ee (if dumb "" "[m")) (cnt 0))
  (declare (fixnum cnt) (simple-string bb ib ee))
  (setq *prompt* (lambda ()
                   (format nil "~a~(~a~)~a ~a[~:d]:~a " ib
                           (package-short-name *package*)
                           ee bb (incf cnt) ee))))

(defun run-prog (prog &rest opts &key args wait &allow-other-keys)
  "Common interface to shell. Doesn't return anything useful."
  #+(or clisp gcl) (declare (ignore wait))
  (remf opts :args) (remf opts :wait)
  #+excl (apply #'excl:run-shell-command (apply #'vector prog prog args)
		:wait wait opts)
  #+cmu (run-program prog args :wait wait)
  #+gcl (apply #'run-process prog args)
  #+clisp (apply #'lisp:run-program prog :arguments args opts))

#+gcl (defun quit () (bye))
#+allegro (defun quit () (exit))

(defun pipe-output (prog &rest args)
  "Return an output stream wich will go to the command."
  #+excl (excl:run-shell-command (apply #'vector prog prog args)
				 :input :stream :wait nil)
  #+gcl (si::fp-input-stream (apply #'run-process prog args))
  #+clisp (lisp:make-pipe-output-stream (format nil "~a~{ ~a~}" prog args))
  #+cmu (process-output (run-program prog args :input :stream :wait nil)))

;;;
;;; }}}{{{ Environment
;;;

(eval-when (compile load eval)
  (defcustom *source-dir* pathname
    (merge-pathnames #+win32 "fx/" #+unix "eagle/" (user-homedir-pathname))
    "*The directory with the Eagle source code.")
  (defcustom *lisp-dir* pathname
    (merge-pathnames "lisp/" (user-homedir-pathname))
    "*The private lisp sources."))
(defcustom *fin-dir* pathname (merge-pathnames "data/" *source-dir*)
  "*The directory with the financial data.")

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
		  "list" "math" "octave" "print" "report" "rules" "signal"
		  "util" "work"))
      (declare (simple-string mo))
      (setf (gethash mo *require-table*)
	    (merge-pathnames (concatenate 'string mo ".lisp") *source-dir*)))
    (dolist (mo '("gq" "url" "geo" "animals"))
      (declare (simple-string mo))
      (setf (gethash mo *require-table*)
	    (merge-pathnames (concatenate 'string mo ".lisp") *lisp-dir*)))
    (setf (gethash "monitor" *require-table*)
	  (merge-pathnames "metering.lisp" *lisp-dir*))
    (defun sds-require (fl)
      "Load the appropriate file."
      (let ((file (gethash (string fl) *require-table*)) comp)
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
	    (fmakunbound symb)
            (format t "; ~s is being autoloaded from `~a'~%" symb file)
            (sds-require file) (apply symb args))
	  (documentation symb 'function)
	  (format nil "Autoloaded (from ~a):~%~a" file comment))))

(defmacro map-in (fn seq &rest seqs)
  "`map-into' the first sequence, evaluating it once.
  (map-in F S) == (map-into S F S)"
  (let ((mi (gensym "MI")))
    `(let ((,mi ,seq)) (map-into ,mi ,fn ,mi ,@seqs))))

#+(or allegro gcl clisp)
(defun default-directory ()
  "The default directory."
  #+clisp (lisp:default-directory)
  #+allegro (current-directory)
  #+gcl (truename "."))
#+allegro (defsetf default-directory chdir "Change the current directory.")
#+gcl (defsetf default-directory si:chdir "Change the current directory.")
#+clisp (defsetf default-directory lisp:cd "Change the current directory.")
#-allegro (defun chdir (dir) (setf (default-directory) dir))

;;;
;;; }}}{{{ Environment
;;;

(defun current-environment (&optional (stream *standard-output*))
  "Print the current environment to a stream."
  (declare (stream stream))
  (format stream "~&~%~70~~%~70,,,'-:@<<{[ The current environment ]}>~>~%~
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

(defconst +month-names+ (simple-array simple-string (12))
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  "*The names of the months.")
(defconst +week-days+ (simple-array simple-string (7))
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  "*The names of the days of the week.")
(defconst +time-zones+ list
  '((5 "EDT" . "EST") (6 "CDT" . "CST") (7 "MDT" . "MST") (8 "PDT" . "PST")
    (0 "GMT" . "GDT"))
  "*The string representations of the time zones.")
(defconst +whitespace+ simple-vector
  #(#\Space #\Newline #\Tab #\Linefeed #\Return #\Page)
  "*The whitespace characters.")

(defun current-time (&optional (stream t))
  "Print the current time to the stream (defaults to t)."
  (multiple-value-bind (se mi ho da mo ye dw dst tz) (get-decoded-time)
    (declare (fixnum se mi ho da mo ye dw tz))
    (format stream "~4d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d ~a(~@d)"
	    ye mo da (aref +week-days+ dw) ho mi se
	    (funcall (if dst #'cadr #'cddr) (assoc tz +time-zones+)) tz)))

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

;;;
;;; }}}{{{ Function Compositions
;;;

(defmacro compose (&rest functions)
  "Macro: compose functions or macros of 1 argument into a lambda.
E.g., (compose abs (dl-val zz) 'key) ==>
  (lambda (yy) (abs (funcall (dl-val zz) (funcall key yy))))"
  (labels ((zz (xx yy)
	     (let ((rr (list (car xx) (if (cdr xx) (zz (cdr xx) yy) yy))))
	       (if (consp (car xx))
                   (cons 'funcall (if (eq (caar xx) 'quote)
                                      (cons (cadar xx) (cdr rr)) rr))
                   rr))))
    (let ((ff (zz functions 'yy))) `(lambda (yy) ,ff))))

(defun compose-f (&rest functions)
  "Return the composition of all the arguments.
All FUNCTIONS should take one argument, except for
the last one, which can take several."
  (reduce (lambda (f0 f1)
	    (declare (function f0 f1))
	    (lambda (&rest args) (funcall f0 (apply f1 args))))
	  functions :initial-value #'identity))

(defun compose-all (&rest functions)
  "Return the composition of all the arguments.
All the values from nth function are fed to the n-1th."
  (reduce (lambda (f0 f1)
	    (declare (function f0 f1))
	    (lambda (&rest args) (multiple-value-call f0 (apply f1 args))))
	  functions :initial-value #'identity))

;;;
;;; }}}{{{ generic
;;;

(eval-when (load compile eval)
  (defgeneric value (xx) (:documentation "Get the value."))
  (declaim (ftype (function (t) number) value)))
(defmethod value ((xx number)) xx)
(defmethod value ((xx cons)) (value (cdr xx)))

;;;
;;; }}}{{{ autoloads
;;;

(autoload 'update-quotes "gq" "Update the quote log.")
(autoload 'dump-url "url" "Dump the contents of this URL.")
(autoload 'view-url "url" "Launch a browser on this URL.")
(autoload 'url "url" "Convert an object to a URL.")
(autoload 'cite-info "geo" "Look at U.S. Gazetteer.")
(autoload 'weather-report "geo" "Get the weather forecast.")
(autoload 'fetch-country-list "geo" "Load the country list via the WWW.")
(autoload 'save-restore-country-list "geo" "Load the country list from disk.")
(autoload 'make-all-contracts "futures" "Init `*contract-list*'.")
(autoload 'make-emas-channels "rules" "Get the EMAs and channels.")
(autoload 'set-depth "signal" "Set the depth and load `*sig-stat*'.")
(autoload 'make-sig-stat "work" "Generate `*sig-stat*'.")
(autoload 'test-par "work" "Test run.")
(autoload 'best-pars "work" "Get the best parameters.")
(autoload 'pr "print" "Print readably, through `with-standard-io-syntax'.")
(autoload 'all-docs "print" "Print all docs for a symbol.")
(autoload 'play-animals "animals" "Play the game of animals.")
(autoload 'load-all-currencies "currency" "Load all the currencies.")

(provide "base")
;;; }}} base.lisp ends here
