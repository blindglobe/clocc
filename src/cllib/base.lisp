;;; File: <base.lisp - 1999-01-28 Thu 09:37:27 EST sds@eho.eaglets.com>
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
;;; $Id: base.lisp,v 1.10 1999/01/28 14:37:36 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/base.lisp,v $
;;; $Log: base.lisp,v $
;;; Revision 1.10  1999/01/28 14:37:36  sds
;;; Moved `with-open-pipe' here.
;;; Renamed `current-environment' to `sysinfo'.
;;;
;;; Revision 1.9  1999/01/12 23:19:48  sds
;;; Added `excl::time-other-base' for ACL's `time'.
;;;
;;; Revision 1.8  1999/01/07 03:42:14  sds
;;; Added `index-t'.
;;;
;;; Revision 1.7  1998/12/28 20:19:33  sds
;;; Added `close-pipe'.
;;;
;;; Revision 1.6  1998/12/09 21:16:34  sds
;;; Added `pipe-input'.  Sorted autoloads.
;;;
;;; Revision 1.5  1998/06/26 23:07:18  sds
;;; Added `mk-arr', `map-vec', fixed CMUCL's `getenv'.
;;; Reworked `current-environment'.
;;;
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

(eval-when (load compile eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3)
                     #+cmu-sds-ignore (ext:inhibit-warnings 3))
           #+(or clisp gcl) (declaration values))
  (setq *read-default-float-format* 'double-float *print-case* :downcase
        *print-array* t)
  #+cmu (setf *gc-verbose* nil *bytes-consed-between-gcs* 32000000
              *efficiency-note-cost-threshold* 20)
  ;; (lisp::fd-stream-buffering system:*stdout*) :none
  ;; (lisp::fd-stream-buffering system:*tty*) :none)
  ;; #+cmu (push "x86f" make::*standard-binary-file-types*)
  ;; #+cmu (push "lisp" make::*standard-source-file-types*) ; default
  #+allegro
  (progn (setq excl:*global-gc-behavior* :auto
               excl::stream-buffer-size 8192)
         (tpl:setq-default *read-default-float-format* 'double-float)
         (tpl:setq-default *print-case* :downcase)
         ;; Duane Rettig <duane@franz.com>:
         ;; TIME reports 32 other bytes too many CL unless tuned with
         (setq excl::time-other-base 32)
         (push "lisp" sys:*source-file-types*))
  #+clisp (setq lisp:*warn-on-floating-point-contagion* t
                lisp:*floating-point-contagion-ansi* t
                lisp:*ansi* t
                clos::*gf-warn-on-removing-all-methods* nil
                clos::*warn-if-gf-already-called* nil
                clos::*gf-warn-on-replacing-method* nil
                ;; lisp:*pprint-first-newline* nil
                ;; sys::*source-file-types* '(#".lisp") ; default
                lisp:*default-float-format* 'double-float)
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

(eval-when (load compile eval)
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
      `(eval-when (load compile eval)
        (unless (boundp ',name) (declaim (type ,type ,name))
                (defconstant ,name (the ,type ,init) ,doc)))))

  (unless (fboundp 'dfloat)
    (defmacro dfloat (num)
      "Coerce to double float."
      `(float ,num 1.0d0)))

  (unless (fboundp 'getenv)
    (defun getenv (var)
      "Return the value of the environment variable."
      #+cmu (cdr (assoc (string var) *environment-list* :test #'equalp
                        :key #'string)) ; xlib::getenv
      #-cmu
      (#+(or allegro clisp) system::getenv #+lispworks w:environment-variable
       #+lucid lcl:environment-variable #+gcl si:getenv (string var)))))

(deftype index-t () '(unsigned-byte 20)) ; for arithmetics

#+cmu
(defun sys::package-short-name (pkg)
  "Return the shortest (nick)name of the package."
  (declare (type package pkg))
  (let ((name (reduce (lambda (st0 st1)
                        (declare (simple-string st0 st1))
                        (if (> (length st0) (length st1)) st1 st0))
                      (package-nicknames pkg) :initial-value
                      (package-name pkg))))
    (case *print-case*
      (:upcase (string-upcase name))
      (:downcase (string-downcase name))
      (:capitalize (string-capitalize name))
      (t name))))
#+cmu
(defvar sys::*command-index* 0)

#+(or cmu clisp)
(setq lisp::*prompt*
      (lambda ()
        (let* ((dumb (string-equal (getenv "TERM") "dumb"))
               (bb (if dumb "" "[1m"))
               (ib (if dumb "" "[7m"))
               (ee (if dumb "" "[m" )))
          (declare (simple-string bb ib ee))
          (format nil "~a~a~a~a[~:d]:~a " ib
                  (sys::package-short-name *package*)
                  ee bb (incf sys::*command-index*) ee))))

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
  "Return an output stream which will go to the command."
  #+excl (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                 :input :stream :wait nil)
  #+gcl (si::fp-input-stream (apply #'run-process prog args)) #+cmu
  (process-input (run-program prog args :input :stream :output t :wait nil))
  #+clisp (lisp:make-pipe-output-stream (format nil "~a~{ ~a~}" prog args)))

(defun pipe-input (prog &rest args)
  "Return an input stream from which the command output will be read."
  #+excl (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                 :output :stream :wait nil)
  #+gcl (si::fp-output-stream (apply #'run-process prog args)) #+cmu
  (process-output (run-program prog args :output :stream :input t :wait nil))
  #+clisp (lisp:make-pipe-input-stream (format nil "~a~{ ~a~}" prog args)))

(defun close-pipe (stream)
  "Close the pipe stream.
The trouble is with ACL: a simple `close' doesn't get rid of the process.
This function takes care of that."
  (declare (stream stream))
  (close stream)
  #+allegro (sys:reap-os-subprocess))

(defmacro with-open-pipe ((pipe open) &body body)
  "Open the pipe, do something, then close it."
  `(let ((,pipe ,open))
    (declare (stream ,pipe))
    (unwind-protect (progn ,@body)
      (close-pipe ,pipe))))

;;;
;;; }}}{{{ Environment
;;;

(eval-when (load compile eval)
  (defcustom *source-dir* pathname
    (merge-pathnames "eagle/" (user-homedir-pathname))
    "*The directory with the Eagle source code.")
  (defcustom *lisp-dir* pathname
    (merge-pathnames "lisp/" (user-homedir-pathname))
    "*The private lisp sources."))
(defcustom *datadir* pathname (merge-pathnames "data/" *source-dir*)
  "*The directory with the financial data.")

#-(or clisp allegro)
(eval-when (eval load compile)
  (define-setf-expander values (&rest places &environment env)
    (loop :for pl :in places :with te :and va :and ne :and se :and ge :do
          (multiple-value-setq (te va ne se ge) (get-setf-expansion pl env))
          :append te :into te1 :append va :into va1 :append ne :into ne1
          :collect se :into se1 :collect ge :into ge1
          :finally (return (values te1 va1 ne1 (cons 'values se1)
                                   (cons 'values ge1))))))

(eval-when (load compile eval)
  (unless (boundp '*require-table*)
    (defcustom *require-table* hash-table
      (make-hash-table :test #'equal :size 20)
      "*The table for `sds-require'.")
    (dolist (mo '("base" "channel" "currency" "date" "futures" "fx" "gnuplot"
                  "list" "math" "octave" "print" "report" "rules" "signal"
                  "util" "work" "matrix"))
      (declare (simple-string mo))
      (setf (gethash mo *require-table*)
            (merge-pathnames (concatenate 'string mo ".lisp") *source-dir*)))
    (dolist (mo '("gq" "url" "rpm" "geo" "animals" "h2lisp" "clhs"))
      (declare (simple-string mo))
      (setf (gethash mo *require-table*)
            (merge-pathnames (concatenate 'string mo ".lisp") *lisp-dir*)))
    (setf (gethash "monitor" *require-table*)
          (merge-pathnames "metering.lisp" *lisp-dir*))
    (defun sds-require (fl)
      "Load the appropriate file."
      (let ((file (gethash (string fl) *require-table*)) comp)
        (assert file () "Cannot require `~a'." fl)
        (setq comp (compile-file-pathname file))
        (require fl (if (probe-file comp) comp file))
        (assert (member fl *modules* :test (hash-table-test *require-table*))
                () "File `~a' failed to provide `~a'." file fl)))))

(defun autoload (symb file &optional comment)
  "Declare symbol SYMB to call a function defined in FILE."
  (declare (symbol symb) (type (or simple-string null) comment))
  (unless (fboundp symb)
    (assert (gethash file *require-table*) (file)
            "Cannot autoload anything from `~a'." file)
    (setf (fdefinition symb)
          (lambda (&rest args)
            (setf (documentation symb 'function) nil)
            (fmakunbound symb)
            (format t "; ~s is being autoloaded from `~a'~%" symb file)
            (sds-require file) (apply symb args))
          (documentation symb 'function)
          (format nil "Autoloaded (from ~a)~@[:~%~a~]" file comment))))

(defmacro map-in (fn seq &rest seqs)
  "`map-into' the first sequence, evaluating it once.
  (map-in F S) == (map-into S F S)"
  (let ((mi (gensym "MI")))
    `(let ((,mi ,seq)) (map-into ,mi ,fn ,mi ,@seqs))))

(eval-when (load compile eval)
(defmacro mk-arr (type init &optional len)
  "Make array with elements of TYPE, initializing."
  (if len `(make-array ,len :element-type ,type :initial-element ,init)
      `(make-array (length ,init) :element-type ,type
        :initial-contents ,init))))

(defmacro map-vec (type len &rest args)
  "MAP into a simple-array with elements of TYPE and length LEN."
  `(map-into (make-array ,len :element-type ,type) ,@args))

#+(or allegro gcl clisp)
(defun default-directory ()
  "The default directory."
  #+clisp (lisp:default-directory)
  #+allegro (current-directory)
  #+gcl (truename "."))
#+allegro (defsetf default-directory chdir "Change the current directory.")
#+gcl (defsetf default-directory si:chdir "Change the current directory.")
#-allegro (defun chdir (dir) (setf (default-directory) dir))

(defun sysinfo ()
  "Print the current environment to a stream."
  (format t "~&~%~75~~%~75,,,'-:@<<{[ The current environment ]}>~>~%~
Implementation:~20t~a~%~7tversion:~20t~a~%Machine:  type:~20t~a
~7tversion:~20t~a~%~6tinstance:~20t~a~%System:~19t"
          (lisp-implementation-type) (lisp-implementation-version)
          (machine-type) (machine-version) (machine-instance))
  #+win32
  (if (system::registry
       "SOFTWARE\\Microsoft\\Windows\\CurrentVersion" "ProductName")
      (flet ((env (str)
               (system::registry
                "SOFTWARE\\Microsoft\\Windows\\CurrentVersion" str)))
        (format t " ~a (~a - ~a; boot: ~a)~%Registered to: ~a, ~a [~a]"
                (env "ProductName") (env "Version") (env "VersionNumber")
                (env "BootCount") (env "RegisteredOwner")
                (env "RegisteredOrganization") (env "ProductId")))
      (flet ((env (str)
               (system::registry
                "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion" str)))
        (format t " WinNT ~a (build ~a: ~a) ~a~%Registered to: ~a, ~a [~a]"
                (env "CurrentVersion") (env "CurrentBuildNUmber")
                (env "CSDVersion") (env "CurrentType") (env "RegisteredOwner")
                (env "RegisteredOrganization") (env "ProductId"))))
  #+os/2 (princ " OS/2")
  #+unix (princ " Unix")
  #+dos (princ " DOS")
  #+pc386 (princ " PC386")
  #+amiga (princ " Exec/AmigaDOS")
  (format t "~%Software: type:~20t~a~%~7tversion:~20t~a~%Site:~20t~a (~a)
User home:~20t~a~%Current directory:~20t~a~%Default pathname:~20t~a
Features:~10t~{~<~%~9t ~1,74:; ~a~>~^,~}.
Modules:~10t~{~<~%~9t ~1,74:; ~a~>~^,~}.~%Current package:~30t~a~%"
          (software-type) (software-version) (long-site-name)
          (short-site-name) (user-homedir-pathname) (default-directory)
          *default-pathname-defaults* *features* *modules* *package*)
  #+clisp (format t "[CLISP] Current language:~30t~a~%"
                  (system::current-language))
  (flet ((exdi (fl) (integer-length (nth-value 1 (decode-float fl)))))
    (format t "Fixnum length:~25t~3d bits
Short Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Single Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Double Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)
Long Floats:~25t~3d bits exponent, ~3d bits significand (mantissa)~%"
            (integer-length most-positive-fixnum)
            (exdi most-positive-short-float)
            (float-digits most-positive-short-float)
            (exdi most-positive-single-float)
            (float-digits most-positive-single-float)
            (exdi most-positive-double-float)
            (float-digits most-positive-double-float)
            (exdi most-positive-long-float)
            (float-digits most-positive-long-float)))
  #+clisp (format t "[CLISP] long-float-digits:~44t~3d~%"
                  (lisp:long-float-digits))
  (dolist (sy '(array-total-size-limit array-rank-limit array-dimension-limit
                lambda-parameters-limit call-arguments-limit
                multiple-values-limit char-code-limit))
    (format t " ~a:~30t~15:d~%" sy (symbol-value sy)))
  (format t "Internal time unit:~25t~f sec~%*gensym-counter*:~25t~:d
Current time:~25t" (/ internal-time-units-per-second) *gensym-counter*)
  (current-time t) (format t "~%~75~~%") (room) (values))

(defconst +month-names+ (simple-array simple-string (12))
  (mk-arr 'simple-string '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
                           "Sep" "Oct" "Nov" "Dec"))
  "*The names of the months.")
(defconst +week-days+ (simple-array simple-string (7))
  (mk-arr 'simple-string '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
  "*The names of the days of the week.")
(defconst +time-zones+ list
  '((5 "EDT" . "EST") (6 "CDT" . "CST") (7 "MDT" . "MST") (8 "PDT" . "PST")
    (0 "GMT" . "GDT"))
  "*The string representations of the time zones.")
(defconst +whitespace+ (simple-array character (*))
  (mk-arr 'character '(#\Space #\Newline #\Tab #\Linefeed #\Return #\Page))
  "*The whitespace characters.")

(defun current-time (&optional (stream t))
  "Print the current time to the stream (defaults to t)."
  (multiple-value-bind (se mi ho da mo ye dw dst tz) (get-decoded-time)
    (declare (fixnum se mi ho da mo ye dw tz))
    (format stream "~4d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d ~a(~@d)"
            ye mo da (aref +week-days+ dw) ho mi se
            (funcall (if dst #'cadr #'cddr) (assoc tz +time-zones+)) tz)))

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

(defgeneric value (xx)
  (:documentation "Get the value.")
  (:method ((xx number)) xx)
  (:method ((xx cons)) (value (cdr xx))))
(declaim (ftype (function (t) number) value))

;;;
;;; }}}{{{ autoloads
;;;

(autoload 'all-docs "print" "Print all docs for a symbol.")
(autoload 'best-pars "work" "Get the best parameters.")
(autoload 'cite-info "geo" "Look at U.S. Gazetteer.")
(autoload 'date "date" "Convert an object to `date'.")
(autoload 'dump-url "url" "Dump the contents of this URL.")
(autoload 'fetch-country-list "geo" "Load the country list via the WWW.")
(autoload 'load-all-currencies "currency" "Load all the currencies.")
(autoload 'load-all-futures "rules" "Load all futures data.")
(autoload 'make "util" "Recompile the files.")
(autoload 'make-all-contracts "futures" "Init `*contract-list*'.")
(autoload 'make-all-sig-stat "rules" "Generate all `sig-stat' files.")
(autoload 'make-emas-channels "rules" "Get the EMAs and channels.")
(autoload 'make-sig-stat "rules" "Generate `*sig-stat*'.")
(autoload 'play-animals "animals" "Play the game of animals.")
(autoload 'plot-dated-lists "gnuplot" "Plot dated lists.")
(autoload 'plot-dl-channels "gnuplot" "Plot dated lists with channels.")
(autoload 'plot-error-bars "gnuplot" "Plot the list with errorbars.")
(autoload 'plot-functions "gnuplot" "Plot functions.")
(autoload 'plot-lists "gnuplot" "Plot the given lists of numbers.")
(autoload 'plot-lists-arg "gnuplot" "Plot the given lists of numbers.")
(autoload 'pr "print" "Print readably, through `with-standard-io-syntax'.")
(autoload 'print-all-ascii "print" "Print all ASCII characters.")
(autoload 'print-all-packages "print" "Print info about all packages.")
(autoload 'rpm "rpm" "Handle RPMs.")
(autoload 'rpm-get-present "rpm" "Get the present RPMs.")
(autoload 'rpm-get-new-rpms "rpm" "Update RPMs.")
(autoload 'save-restore-country-list "geo" "Load the country list from disk.")
(autoload 'set-depth "signal" "Set the depth and load `*sig-stat*'.")
(autoload 'test-par "work" "Test run.")
(autoload 'update-quotes "gq" "Update the quote log.")
(autoload 'url "url" "Convert an object to a URL.")
(autoload 'view-url "url" "Launch a browser on this URL.")
(autoload 'weather-report "geo" "Get the weather forecast.")

(provide "base")
;;; }}} base.lisp ends here
