;;; Environment & System access
;;;
;;; Copyright (C) 1999-2001 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: sys.lisp,v 1.31 2001/06/25 19:54:29 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/port/sys.lisp,v $

(eval-when (compile load eval)
  (require :ext (translate-logical-pathname "clocc:src;port;ext"))
  #+(and allegro mswindows)
  (require :ole))

(in-package :port)

(export
 '(getenv finalize variable-special-p arglist
   class-slot-list class-slot-initargs
   pathname-ensure-name probe-directory default-directory chdir mkdir rmdir
   +month-names+ +week-days+ +time-zones+ tz->string current-time sysinfo))

;;;
;;; System
;;;

(defun getenv (var)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (sys::getenv (string var))
  #+cmu (cdr (assoc (string var) ext:*environment-list* :test #'equalp
                    :key #'string)) ; xlib::getenv
  #+gcl (si:getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #-(or allegro clisp cmu gcl lispworks lucid)
  (error 'not-implemented :proc (list 'getenv var)))

(defun finalize (obj func)
  "When OBJ is GCed, FUNC is called on it."
  #+allegro (excl:schedule-finalization obj func)
  #+clisp (#+lisp=cl ext:finalize #-lisp=cl lisp:finalize obj func)
  #+cmu (ext:finalize obj func)
  #+cormanlisp (cl::register-finalization obj func)
  #-(or allegro clisp cmu cormanlisp)
  (error 'not-implemented :proc (list 'finalize obj func)))

;;;
;;; Introspection
;;;

(defun variable-special-p (symbol)
  "Return T if the symbol names a global special variable."
  #+(and allegro (not (version>= 6))) (clos::variable-special-p symbol nil)
  #+(and allegro (version>= 6)) (excl::variable-special-p symbol nil)
  #+clisp (sys::special-variable-p symbol)
  #+cmu (walker:variable-globally-special-p symbol)
  #+gcl (si:specialp symbol)
  #+lispworks (eq :special (hcl:variable-information symbol))
  #+lucid (system:proclaimed-special-p symbol)
  #-(or allegro clisp cmu gcl lispworks lucid)
  (error 'not-implemented :proc (list 'variable-special-p symbol)))

(defun arglist (fn)
  "Return the signature of the function."
  #+allegro (excl:arglist fn)
  #+clisp (sys::arglist fn)
  #+cmu (values (let ((st (kernel:%function-arglist fn)))
                  (if (stringp st) (read-from-string st)
                      (eval:interpreted-function-arglist fn))))
  #+cormanlisp (ccl:function-lambda-list
                (typecase fn (symbol (fdefinition fn)) (t fn)))
  #+gcl (let ((fn (etypecase fn
                    (symbol fn)
                    (function (si:compiled-function-name fn)))))
          (get fn 'si:debug))
  #+lispworks (lw:function-lambda-list fn)
  #+lucid (lcl:arglist fn)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid)
  (error 'not-implemented :proc (list 'arglist fn)))

;; implementations with MOP-ish CLOS
#+(or allegro clisp cmu cormanlisp lispworks lucid)
;; we use `macrolet' for speed - so please be careful about double evaluations
;; and mapping (you cannot map or funcall a macro, you know)
(macrolet ((class-slots* (class)
             #+allegro `(clos:class-slots ,class)
             #+clisp `(clos::class-slots ,class)
             #+cmu `(pcl::class-slots ,class)
             #+cormanlisp `(cl:class-slots ,class)
             #+lispworks `(hcl::class-slots ,class)
             #+lucid `(clos:class-slots ,class))
           (class-slots1 (obj)
             `(class-slots*
               (typecase ,obj
                 (class ,obj)
                 (symbol (find-class ,obj))
                 (t (class-of ,obj)))))
           (slot-name (slot)
             #+(and allegro (not (version>= 6))) `(clos::slotd-name ,slot)
             #+(and allegro (version>= 6)) `(clos:slot-definition-name ,slot)
             #+clisp `(clos::slotdef-name ,slot)
             #+cmu `(slot-value ,slot 'pcl::name)
             #+cormanlisp `(getf ,slot :name)
             #+lispworks `(hcl::slot-definition-name ,slot)
             #+lucid `(clos:slot-definition-name ,slot))
           (slot-initargs (slot)
             #+(and allegro (not (version>= 6))) `(clos::slotd-initargs ,slot)
             #+(and allegro (version>= 6))
             `(clos:slot-definition-initargs ,slot)
             #+clisp `(clos::slotdef-initargs ,slot)
             #+cmu `(slot-value ,slot 'pcl::initargs)
             #+cormanlisp `(getf ,slot :initargs)
             #+lispworks `(hcl::slot-definition-initargs ,slot)
             #+lucid `(clos:slot-definition-initargs ,slot))
           (slot-one-initarg (slot) `(car (slot-initargs ,slot)))
           (slot-alloc (slot)
             #+(and allegro (not (version>= 6)))
             `(clos::slotd-allocation ,slot)
             #+(and allegro (version>= 6))
             `(clos:slot-definition-allocation ,slot)
             #+clisp `(clos::slotdef-allocation ,slot)
             #+cmu `(pcl::slot-definition-allocation ,slot)
             #+cormanlisp `(getf ,slot :allocation)
             #+lispworks `(hcl::slot-definition-allocation ,slot)
             #+lucid `(clos:slot-definition-allocation ,slot)))

  (defun class-slot-list (class &optional (all t))
    "Return the list of slots of a CLASS.
CLASS can be a symbol, a class object (as returned by `class-of')
or an instance of a class.
If the second optional argument ALL is non-NIL (default),
all slots are returned, otherwise only the slots with
:allocation type :instance are returned."
    (mapcan (if all (compose list slot-name)
                (lambda (slot)
                  (when (eq (slot-alloc slot) :instance)
                    (list (slot-name slot)))))
            (class-slots1 class)))

  (defun class-slot-initargs (class &optional (all t))
    "Return the list of initargs of a CLASS.
CLASS can be a symbol, a class object (as returned by `class-of')
or an instance of a class.
If the second optional argument ALL is non-NIL (default),
initargs for all slots are returned, otherwise only the slots with
:allocation type :instance are returned."
    (mapcan (if all (compose list slot-one-initarg)
                (lambda (slot)
                  (when (eq (slot-alloc slot) :instance)
                    (list (slot-one-initarg slot)))))
            (class-slots1 class))))

;;;
;;; Environment
;;;

(defun pathname-ensure-name (path)
  "Make sure that the pathname has a name slot.
Call `pathname' on it argument and, if there is no NAME slot,
but there is a TYPE slot, move TYPE into NAME."
  (let ((path (pathname path)))
    (if (or (pathname-name path) (null (pathname-type path))) path
        ;; if you use CLISP, you will need 2000-03-09 or newer
        (make-pathname :name (concatenate 'string "." (pathname-type path))
                       :type nil :defaults path))))

(defun probe-directory (filename)
  "Check whether the file name names an existing directory."
  #+allegro (excl::probe-directory filename)
  #+clisp (values
           (ignore-errors
             (#+lisp=cl ext:probe-directory #-lisp=cl lisp:probe-directory
                        filename)))
  #+cmu (eq :directory (unix:unix-file-kind (namestring filename)))
  #+lispworks (lw:file-directory-p filename)
  #-(or allegro clisp cmu lispworks)
  ;; From: Bill Schelter <wfs@fireant.ma.utexas.edu>
  ;; Date: Wed, 5 May 1999 11:51:19 -0500
  (let* ((path (pathname filename))
         (dir (pathname-directory path))
         (name (pathname-name path)))
    (when name (setq dir (append dir (list name))))
    (probe-file (make-pathname :directory dir))))

(defun default-directory ()
  "The default directory."
  #+allegro (excl:current-directory)
  #+clisp (#+lisp=cl ext:default-directory #-lisp=cl lisp:default-directory)
  #+cmucl (ext:default-directory)
  #+cormanlisp (ccl:get-current-directory)
  #+lispworks (hcl:get-working-directory)
  #+lucid (lcl:working-directory)
  #-(or allegro clisp cmucl cormanlisp lispworks lucid) (truename "."))

(defun chdir (dir)
  #+allegro (excl:chdir dir)
  #+clisp (#+lisp=cl ext:cd #-lisp=cl lisp:cd dir)
  #+cmu (setf (ext:default-directory) dir)
  #+cormanlisp (ccl:set-current-directory dir)
  #+gcl (si:chdir dir)
  #+lispworks (hcl:change-directory dir)
  #+lucid (lcl:working-directory dir)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid)
  (error 'not-implemented :proc (list 'chdir dir)))

(defsetf default-directory chdir "Change the current directory.")

(defun mkdir (dir)
  #+allegro (excl:make-directory dir)
  #+clisp (#+lisp=cl ext:make-dir #-lisp=cl lisp:make-dir dir)
  #+cmu (unix:unix-mkdir (directory-namestring dir) #o777)
  #+lispworks (system:make-directory dir)
  #-(or allegro clisp cmu lispworks)
  (error 'not-implemented :proc (list 'mkdir dir)))

(defun rmdir (dir)
  #+allegro (excl:delete-directory dir)
  #+clisp (#+lisp=cl ext:delete-dir #-lisp=cl lisp:delete-dir dir)
  #+cmu (unix:unix-rmdir dir)
  #+lispworks
  ;; `lw:delete-directory' is present in LWW 4.1.20 but not on LWL 4.1.0
  (if (fboundp 'lw::delete-directory)
      (lw::delete-directory dir)
      (delete-file dir))
  #-(or allegro clisp cmu lispworks) (delete-file dir))

(defun sysinfo (&optional (out *standard-output*))
  "Print the current environment to a stream."
  (declare (stream out))
  (format out "~&~%~75~~%~75,,,'-:@<<{[ The current environment ]}>~>~%~
Implementation:~20t~a~%~7tversion:~20t~a~%Machine:  type:~20t~a
~7tversion:~20t~a~%~6tinstance:~20t~a~%Opeating System:~19t"
          (lisp-implementation-type) (lisp-implementation-version)
          (machine-type) (machine-version) (machine-instance))
  #+(or (and clisp win32) (and allegro mswindows))
  (let* ((root-nt "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion")
         (root-9x "SOFTWARE\\Microsoft\\Windows\\CurrentVersion")
         (key-nt #+(and clisp win32) root-nt
                 #+(and allegro mswindows)
                 (ole:open-registry-key ole:rkey-local-machine root-nt))
         (key-9x #+(and clisp win32) root-9x
                 #+(and allegro mswindows)
                 (ole:open-registry-key ole:rkey-local-machine root-9x)))
    (labels ((9x-p ()
               #+(and clisp win32) (sys::registry key-9x "ProductName")
               #+(and allegro mswindows)
               (ole:registry-value key-9x "ProductName"))
             (env (str)
               #+(and clisp win32)
               (sys::registry (if (9x-p) key-9x key-nt) str)
               #+(and allegro mswindows)
               (ole:registry-value (if (9x-p) key-9x key-nt) str)))
      (if (9x-p)
          (format out " ~a (~a - ~a; boot: ~a)~%~5tRegistered to: ~a, ~a [~a]"
                  (env "ProductName") (env "Version") (env "VersionNumber")
                  (env "BootCount") (env "RegisteredOwner")
                  (env "RegisteredOrganization") (env "ProductId"))
          (format
           out " WinNT ~a (build ~a: ~a) ~a~%~5tRegistered to: ~a, ~a [~a]"
           (env "CurrentVersion") (env "CurrentBuildNUmber") (env "CSDVersion")
           (env "CurrentType") (env "RegisteredOwner")
           (env "RegisteredOrganization") (env "ProductId")))))
  #+os/2 (princ " OS/2")
  #+unix (princ " Unix")
  #+dos (princ " DOS")
  #+pc386 (princ " PC386")
  #+amiga (princ " Exec/AmigaDOS")
  (format out "~%Software: type:~20t~a~%~7tversion:~20t~a~%Site:~20t~a (~a)
User home:~20t~a~%Current directory:~20t~a~%Default pathname:~20t~a
Features:~10t~{~<~%~9t ~1,74:; ~a~>~^,~}.
Modules:~10t~{~<~%~9t ~1,74:; ~a~>~^,~}.~%Current package:~30t~a~%"
          (software-type) (software-version) (long-site-name)
          (short-site-name) (user-homedir-pathname) (default-directory)
          *default-pathname-defaults* *features* *modules* *package*)
  #+clisp (format out "[CLISP] Current language:~30t~a~%"
                  (sys::current-language))
  (flet ((exdi (fl) (integer-length (nth-value 1 (decode-float fl)))))
    (format out "Fixnum length:~25t~3d bits
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
  #+clisp (format out "[CLISP] long-float-digits:~44t~3d~%"
                  #+lisp=cl (ext:long-float-digits)
                  #-lisp=cl (lisp:long-float-digits))
  (dolist (sy '(array-total-size-limit array-rank-limit array-dimension-limit
                lambda-parameters-limit call-arguments-limit
                multiple-values-limit char-code-limit))
    (format out " ~a:~30t~15:d~%" sy (symbol-value sy)))
  (format out "lambda-list-keywords:~30t~{~<~%~30t~1,74:; ~a~>~}~%"
          lambda-list-keywords)
  (format out "Internal time unit:~25t~f sec~%*gensym-counter*:~25t~:d
Current time:~25t" (/ internal-time-units-per-second) *gensym-counter*)
  (current-time out) (format out "~%~75~~%") (room) (values))

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

(defun tz->string (tz)
  "Convert the CL timezone (rational [-24;24], multiple of 3600) to a string."
  (declare (type rational tz))
  (multiple-value-bind (hr mm) (floor (abs tz))
    (let ((mi (floor (* 60 mm))))
      (format nil "~:[+~;-~]~2,'0d~2,'0d" (minusp tz) hr mi))))

(defun current-time (&optional (out t))
  "Print the current time to the stream (defaults to t)."
  (multiple-value-bind (se mi ho da mo ye dw dst tz) (get-decoded-time)
    (declare (fixnum se mi ho da mo ye dw tz))
    (format out "~4d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d ~a (~a)"
            ye mo da (aref +week-days+ dw) ho mi se
            (tz->string (- (if dst 1 0) tz))
            (funcall (if dst #'cadr #'cddr) (assoc tz +time-zones+)))))

(provide :sys)
;;; file sys.lisp ends here
