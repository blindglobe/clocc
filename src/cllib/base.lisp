;;; File: <base.lisp - 1999-05-05 Wed 15:22:15 EDT sds@goems.com>
;;;
;;; Basis functionality, required everywhere
;;;
;;; Copyright (C) 1997-1999 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and the precise copyright document.
;;;
;;; $Id: base.lisp,v 1.20 1999/05/05 20:58:27 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/base.lisp,v $
;;; $Log: base.lisp,v $
;;; Revision 1.20  1999/05/05 20:58:27  sds
;;; LispWorks & GCL compatibility:
;;; (*fas-ext*): added LispWorks extension.
;;; (code, case-error): moved the conditions here.
;;; (not-implemented): new `code' condition.
;;; (run-prog, pipe-output, pipe-input): works with LispWorks.
;;; (probe-directory): added LispWorks and generic code.
;;; (chdir, setf default-directory): base the latter on the former.
;;;
;;; Revision 1.19  1999/04/20 17:38:15  sds
;;; (*current-project*): card now requires `elisp' (for reading BBDB).
;;;
;;; Revision 1.18  1999/04/20 01:10:51  sds
;;; Handle the new file card.lisp.
;;;
;;; Revision 1.17  1999/04/18 05:10:33  sds
;;; Shut up CMUCL GC.
;;;
;;; Revision 1.16  1999/04/15 19:53:35  sds
;;; (current-time): print `tz->string' with ~a.
;;;
;;; Revision 1.15  1999/04/12 21:34:26  sds
;;; (tz->string): new function: print the TimeZone to a string like "-0400"
;;; (current-time): use it.
;;;
;;; Revision 1.14  1999/04/09 19:15:32  sds
;;; Moved `*current-project*' here.
;;;
;;; Revision 1.13  1999/03/27 19:24:36  sds
;;; Added condition `provide-require' and function `require-name-to-file'.
;;; Use the latter in `sds-require' and `autoload'.
;;; Added printing `lambda-list-keywords' in `sysinfo'.
;;;
;;; Revision 1.12  1999/03/02 20:40:09  sds
;;; Added `defgeneric' `code'.
;;;
;;; Revision 1.11  1999/02/22 18:18:51  sds
;;; Added Naggum's ACL readtable fix.
;;; Added `probe-directory' and *fas-ext*.
;;;
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
              *efficiency-note-cost-threshold* 20
              (alien:extern-alien "pointer_filter_verbose" alien:unsigned) 0
              (alien:extern-alien "gencgc_verbose" alien:unsigned) 0)
  #+cmu (pushnew 'compile pcl::*defclass-times*)
  ;; #+cmu (pushnew 'compile pcl::*defgeneric-times*)
  ;; #+cmu (pushnew 'compile pcl::*defmethod-times*)
  ;; (lisp::fd-stream-buffering system:*stdout*) :none
  ;; (lisp::fd-stream-buffering system:*tty*) :none)
  ;; #+cmu (pushnew "x86f" make::*standard-binary-file-types* :test #'equal)
  ;; #+cmu (pushnew "lisp" make::*standard-source-file-types* :test #'equal)
  #+allegro
  (progn (setq excl:*global-gc-behavior* :auto
               excl::stream-buffer-size 8192)
         (tpl:setq-default *read-default-float-format* 'double-float)
         (tpl:setq-default *print-case* :downcase)
         ;; Duane Rettig <duane@franz.com>:
         ;; TIME reports 32 other bytes too many CL unless tuned with
         (setq excl::time-other-base 32)
         (pushnew "lisp" sys:*source-file-types* :test #'equal)
         ;; From Erik Naggum <erik@naggum.no> [22 Feb 1999 10:27:45 +0000]
         ;; fixes the (read-from-string "[#\\x]") problem
         (loop with readtables = (excl::get-objects 11)
               for i from 1 to (aref readtables 0)
               for readtable = (aref readtables i) do
               (when (excl::readtable-dispatch-tables readtable)
                 ;; reader for character names immune cltl1
                 (set-dispatch-macro-character
                  #\# #\\
                  (excl::named-function
                   excl::sharp-backslash
                   (lambda (stream backslash font)
                     (declare (ignore font))
                     (unread-char backslash stream)
                     (let* ((charstring (excl::read-extended-token stream)))
                       (unless *read-suppress*
                         (or (character charstring)
                             (name-char charstring)
                             (excl::internal-reader-error
                              stream "Meaningless character name ~A"
                              (string-upcase charstring)))))))
                  readtable))))
  #+clisp (setq lisp:*warn-on-floating-point-contagion* t
                lisp:*floating-point-contagion-ansi* t
                ;; call with "-a" instead
                ;; lisp:*ansi* t
                clos::*gf-warn-on-removing-all-methods* nil
                clos::*warn-if-gf-already-called* nil
                clos::*gf-warn-on-replacing-method* nil
                ;; lisp:*pprint-first-newline* nil
                ;; sys::*source-file-types* '(#".lisp") ; default
                lisp:*print-rpars* nil ; put closing pars where they belong
                lisp:*print-indent-lists* 1
                lisp:*default-float-format* 'double-float)
  #+gcl (defmacro lambda (bvl &body forms) `#'(lambda ,bvl ,@forms))
  #+allegro-v4.3                ; From Erik Naggum <erik@naggum.no>
  (unless (member :key (excl:arglist #'reduce) :test #'string=)
    (setq excl:*compile-advice* t)
    (excl:defadvice reduce (support-key :before)
      (let ((key (getf (cddr excl:arglist) :key)))
        (when key
          (remf (cddr excl:arglist) :key)
          (setf (second excl:arglist)
                (map 'vector key (second excl:arglist))))))))

(defvar *fas-ext*
  #+gcl ".o" #+clisp ".fas" #+allegro ".fasl" #+cmu ".x86f" ; lw: ".ufsl"
  #+lispworks (concatenate 'string "." (car system::*binary-file-types*))
  #-(or new-compiler compiler) ".lisp"
  "The extension for the compiled file.")

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
      `(eval-when (load compile eval) ; kill compile warnings
        (unless (boundp ',name) (declaim (type ,type ,name))
                (defconstant ,name (the ,type ,init) ,doc)))))

  (unless (fboundp 'dfloat)
    (defmacro dfloat (num)
      "Coerce to double float."
      `(float ,num 1.0d0))))

;;;
;;; System
;;;

(defun getenv (var)
  "Return the value of the environment variable."
  #+cmu (cdr (assoc (string var) *environment-list* :test #'equalp
                    :key #'string)) ; xlib::getenv
  #-cmu
  (#+(or allegro clisp) system::getenv #+lispworks lw:environment-variable
   #+lucid lcl:environment-variable #+gcl si:getenv (string var)))

(unless (fboundp 'gc)           ; #+cmu (gc)
  (defun gc ()
    "Invoke the garbage collector."
    #+clisp (lisp:gc) #+allegro (excl:gc) #+gcl (si::gbc)
    #+lispworks (normal-gc)))

(deftype index-t () '(unsigned-byte 20)) ; for arithmetics

;;;
;;; Conditions
;;;


(define-condition code (error)
  ((proc :type symbol :reader code-proc :initarg :proc)
   (mesg :type simple-string :reader code-mesg :initarg :mesg)
   (args :type list :reader code-args :initarg :args))
  (:report (lambda (cc out)
             (declare (stream out))
             (format out "[~s]~@[ ~?~]" (code-proc cc)
                     (and (slot-boundp cc 'mesg) (code-mesg cc))
                     (and (slot-boundp cc 'args) (code-args cc))))))

(define-condition case-error (code)
  ((mesg :type simple-string :reader code-mesg :initform
         "`~s' evaluated to `~s', not one of [~@{`~s'~^ ~}]")))

(define-condition not-implemented (code)
  ((mesg :type simple-string :reader code-mesg :initform
         "not implemented for ~a [~a]")
   (args :type list :reader code-args :initform
         (list (lisp-implementation-type) (lisp-implementation-version)))))

;;;
;;; Prompt
;;;

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
(defcustom sys::*command-index* index-t 0
  "The number of commands issued so far.")

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

;;;
;;; Shell interface
;;;

(defun run-prog (prog &rest opts &key args wait &allow-other-keys)
  "Common interface to shell. Doesn't return anything useful."
  #+(or clisp gcl lispworks) (declare (ignore wait))
  (remf opts :args) (remf opts :wait)
  #+allegro (apply #'excl:run-shell-command (apply #'vector prog prog args)
                   :wait wait opts)
  #+clisp (apply #'lisp:run-program prog :arguments args opts)
  #+cmu (run-program prog args :wait wait)
  #+gcl (apply #'run-process prog args)
  #+lispworks (sys::call-system (format nil "~a~{ ~a~}" prog args))
  #-(or allegro clisp cmu gcl lispworks)
  (error 'not-implemented :proc (list 'run-prog prog opts)))

#+gcl (defun quit () (bye))
#+allegro (defun quit () (exit))

(defun pipe-output (prog &rest args)
  "Return an output stream which will go to the command."
  #+allegro (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                    :input :stream :wait nil)
  #+clisp (lisp:make-pipe-output-stream (format nil "~a~{ ~a~}" prog args))
  #+cmu
  (process-input (run-program prog args :input :stream :output t :wait nil))
  #+gcl (si::fp-input-stream (apply #'run-process prog args))
  #+lispworks (sys::open-pipe (format nil "~a~{ ~a~}" prog args)
                              :directory :output)
  #-(or allegro clisp cmu gcl lispworks)
  (error 'not-implemented :proc (list 'pipe-output prog args)))

(defun pipe-input (prog &rest args)
  "Return an input stream from which the command output will be read."
  #+allegro (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                    :output :stream :wait nil)
  #+clisp (lisp:make-pipe-input-stream (format nil "~a~{ ~a~}" prog args))
  #+cmu
  (process-output (run-program prog args :output :stream :input t :wait nil))
  #+gcl (si::fp-output-stream (apply #'run-process prog args))
  #+lispworks (sys::open-pipe (format nil "~a~{ ~a~}" prog args)
                              :directory :input)
  #-(or allegro clisp cmu gcl lispworks)
  (error 'not-implemented :proc (list 'pipe-input prog args)))

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

;;;
;;; }}}{{{ Provide/Require
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

(defcustom *require-table* hash-table
  (make-hash-table :test #'equal :size 20)
  "*The table for `sds-require'.")
(dolist (mo '("base" "channel" "currency" "date" "futures" "fx" "gnuplot"
              "list" "math" "octave" "print" "report" "rules" "signal" "util"
              "work" "matrix"))
  (declare (simple-string mo))
  (setf (gethash mo *require-table*)
        (merge-pathnames (concatenate 'string mo ".lisp") *source-dir*)))
(dolist (mo '("gq" "url" "rpm" "geo" "animals" "h2lisp" "clhs" "tests" "elisp"
              "card"))
  (declare (simple-string mo))
  (setf (gethash mo *require-table*)
        (merge-pathnames (concatenate 'string mo ".lisp") *lisp-dir*)))
(setf (gethash "monitor" *require-table*)
      (merge-pathnames "metering.lisp" *lisp-dir*))

(define-condition provide-require (error)
  ((proc :type symbol :reader require-proc :initarg :proc)
   (name :type simple-string :reader require-name :initarg :name))
  (:report (lambda (cc stream)
             (declare (stream stream))
             (format stream "[~s] ~s is not in `~s'"
                     (require-proc cc) (require-name cc) '*require-table*))))

(defun require-name-to-file (name proc)
  "Convert the name to the corresponding file in `*require-table*'."
  (declare (simple-string name) (symbol proc))
  (or (gethash name *require-table*)
      (error 'provide-require :proc proc :name name)))

(defun sds-require (fl)
  "Load the appropriate file."
  (let* ((file (require-name-to-file fl 'sds-require))
         (comp (compile-file-pathname file)))
    (require fl (if (probe-file comp) comp file))
    (assert (member fl *modules* :test (hash-table-test *require-table*))
            () "File `~a' failed to provide `~a'." file fl)))

(defun autoload (symb file &optional comment)
  "Declare symbol SYMB to call a function defined in FILE."
  (declare (symbol symb) (type (or simple-string null) comment))
  (unless (fboundp symb)
    (require-name-to-file file 'autoload)
    (setf (fdefinition symb)
          (lambda (&rest args)
            (setf (documentation symb 'function) nil)
            (fmakunbound symb)
            (format t "; ~s is being autoloaded from `~a'~%" symb file)
            (sds-require file) (apply (fdefinition symb) args))
          (documentation symb 'function)
          (format nil "Autoloaded (from ~a)~@[:~%~a~]" file comment))))

(defcustom *current-project* list
  '(("base") ("print" "base") ("list" "base") ("matrix" "base" "print")
    ("math" "base" "list" "print" "matrix") #-gcl ("monitor")
    ("util" "base" "list" "math" "print" #-gcl "monitor")
    ("date" "base" "util") ("channel" "base" "date") ("futures" "base" "date")
    ("signal" "base" "date" "channel" "futures")
    ("gnuplot" "base" "date" "channel" "signal")
    ("rules" "base" "date" "channel" "futures" "signal" "gnuplot")
    ("work" "base" "date" "channel" "futures" "signal" "gnuplot" "rules")
    ("report" "base" "date" "print" "math" ) ("currency" "base" "date")
    ("fx" "base" "util" "date" "currency" "futures" "report")
    #+nil ("octave" "base" "date" "currency")
    ("url" "base" "util" "date") ("geo" "base" "url")
    ("gq" "base" "url" "date") ("rpm" "base" "url" "date")
    ("h2lisp" "base" "url") #+nil ("clhs" "base" "url")
    ("elisp" "base" "list") ("card" "base" "print" "date" "url" "elisp")
    ("tests" "base" "date" "url" "rpm" "elisp"))
  "*The alist of files to work with, in the order of loading.
Key: name for `sds-require', value - the list of dependencies.")

;;;
;;; }}}{{{ Environment
;;;

(defun probe-directory (filename)
  "Check whether the file name names an existing directory."
  #+allegro (excl::probe-directory filename)
  #+clisp (lisp:probe-directory filename)
  #+cmu (eq :directory (unix:unix-file-kind filename))
  #+lispworks (not (system::probe-file-not-directory-p filename))
  #-(or allegro clisp cmu lispworks)
  ;; From: Bill Schelter <wfs@fireant.ma.utexas.edu>
  ;; Date: Wed, 5 May 1999 11:51:19 -0500
  (let* ((path (pathname fn))
         (dir (pathname-directory path))
         (name (pathname-name path)))
    (when name (setq dir (append dir (list name))))
    (probe-file (make-pathname :directory dir))))

#-cmu
(defun default-directory ()
  "The default directory."
  #+allegro (excl:current-directory)
  #+clisp (lisp:default-directory)
  #+lispworks (hcl:get-working-directory)
  #+lucid (working-directory)
  #-(or allegro clisp lispworks lucid) (truename "."))
#-allegro
(defun chdir (dir)
  #+clisp (lisp:cd dir)
  #+cmu (setf (default-directory) dir)
  #+gcl (si:chdir dir)
  #+lispworks (hcl:change-directory dir)
  #+lucid (working-directory dir)
  #-(or allegro clisp cmu gcl lispworks lucid)
  (error 'not-implemented :proc (list 'chdir dir)))
#-cmu (defsetf default-directory chdir "Change the current directory.")

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
  (format t "lambda-list-keywords:~30t~{~<~%~30t~1,74:; ~a~>~}~%"
          lambda-list-keywords)
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
    (0 "GMT" . "GDT") (-2 "MET" . "MET DST"))
  "*The string representations of the time zones.")
(defconst +whitespace+ (simple-array character (*))
  (mk-arr 'character '(#\Space #\Newline #\Tab #\Linefeed #\Return #\Page))
  "*The whitespace characters.")

(defun tz->string (tz)
  "Convert the CL timezone (rational [-24;24], multiple of 3600) to a string."
  (declare (type rational tz))
  (multiple-value-bind (hr mm) (floor (abs tz))
    (let ((mi (floor (* 60 mm))))
      (format nil "~:[+~;-~]~2,'0d~2,'0d" (minusp tz) hr mi))))

(defun current-time (&optional (stream t))
  "Print the current time to the stream (defaults to t)."
  (multiple-value-bind (se mi ho da mo ye dw dst tz) (get-decoded-time)
    (declare (fixnum se mi ho da mo ye dw tz))
    (format stream "~4d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d ~a (~a)"
            ye mo da (aref +week-days+ dw) ho mi se
            (tz->string (- (if dst 1 0) tz))
            (funcall (if dst #'cadr #'cddr) (assoc tz +time-zones+)))))

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

(declaim (ftype (function (t) number) value))
(defgeneric value (xx)
  (:documentation "Get the value of the object.")
  (:method ((xx number)) xx)
  (:method ((xx cons)) (value (cdr xx))))

(declaim (ftype (function (t) symbol) code))
(defgeneric code (xx)
  (:documentation "Get the code of the object.")
  (:method ((xx symbol)) xx))

;;;
;;; }}}{{{ autoloads
;;;

(autoload 'all-docs "print" "Print all docs for a symbol.")
(autoload 'cite-info "geo" "Look at U.S. Gazetteer.")
(autoload 'date "date" "Convert an object to `date'.")
(autoload 'dump-url "url" "Dump the contents of this URL.")
(autoload 'fetch-country-list "geo" "Load the country list via the WWW.")
(autoload 'make "util" "Recompile the files.")
(autoload 'play-animals "animals" "Play the game of animals.")
(autoload 'plot-dated-lists "gnuplot" "Plot dated lists.")
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
(autoload 'update-quotes "gq" "Update the quote log.")
(autoload 'url "url" "Convert an object to a URL.")
(autoload 'view-url "url" "Launch a browser on this URL.")
(autoload 'weather-report "geo" "Get the weather forecast.")

(autoload 'best-pars "work" "Get the best parameters.")
(autoload 'load-all-currencies "currency" "Load all the currencies.")
(autoload 'load-all-futures "rules" "Load all futures data.")
(autoload 'make-all-contracts "futures" "Init `*contract-list*'.")
(autoload 'make-all-sig-stat "rules" "Generate all `sig-stat' files.")
(autoload 'make-emas-channels "rules" "Get the EMAs and channels.")
(autoload 'make-sig-stat "rules" "Generate `*sig-stat*'.")
(autoload 'plot-dl-channels "gnuplot" "Plot dated lists with channels.")
(autoload 'set-depth "signal" "Set the depth and load `*sig-stat*'.")
(autoload 'test-par "work" "Test run.")

(provide "base")
;;; }}} base.lisp ends here
