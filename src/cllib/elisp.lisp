;;; File: <elisp.lisp - 1999-04-20 Tue 13:33:32 EDT sds@eho.eaglets.com>
;;;
;;; Load Emacs-Lisp files into Common Lisp
;;;
;;; Copyright (C) 1999 by Sam Steingold
;;;
;;; $Id: elisp.lisp,v 1.3 1999/04/20 17:34:46 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/elisp.lisp,v $
;;; $Log: elisp.lisp,v $
;;; Revision 1.3  1999/04/20 17:34:46  sds
;;; (el::make-elisp-readtable): redefine macro #\".
;;; (el::read-elisp-special): handle #\": \n, \r, \f, \t, \v.
;;;
;;; Revision 1.2  1999/02/22 18:20:12  sds
;;; Works now.
;;;
;;; Revision 1.1  1999/02/20 19:15:30  sds
;;; Initial revision
;;;
;;;

(in-package :cl-user)

(eval-when (load compile eval)
  (sds-require "base") (sds-require "list")
  (export '(cl-user::defsubst) :cl-user)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

(defpackage emacs-lisp
  #-allegro
  (:documentation "The package for loading Emacs-Lisp code into Common Lisp")
  (:nicknames elisp el) (:use cl cl-user)
  (:shadow let let* if member delete load require defcustom defconst provide
           ignore format /))

;;;
;;; Emacs-Lisp-specific special forms
;;;

(defmacro el::if (ii tt &rest ee)
  "Emacs-Lisp version of `if'."
  ;; (macroexpand '(el::if t (print "then")))
  ;; (macroexpand '(el::if t (print "th") (print "e1") (print "e2")))
  (if ee `(if ,ii ,tt (progn ,@ee)) `(if ,ii ,tt)))

(defmacro el::let ((&rest vars) &rest forms)
  ;; (macroexpand '(el::let (v1 (v2 t) v3) (f0) (f1)))
  "Emacs-Lisp version of `let'."
  `(let ,vars (declare (special ,@(mapcar #'from-list vars))) ,@forms))

(defmacro el::let* ((&rest vars) &rest forms)
  "Emacs-Lisp version of `let*'."
  `(let* ,vars (declare (special ,@(mapcar #'from-list vars))) ,@forms))

(defmacro el::while (cc &body forms)
  "Emacs-Lisp `while'."
  ;; (macroexpand '(el::while x (print z)))
  `(do () ((not ,cc)) ,@forms))

;;;
;;; Read Emacs-Lisp objects
;;;

(defun el::read-elisp-special (stream char)
  (declare (stream stream) (character char))
  (ecase char
    (#\[ (coerce (read-delimited-list #\] stream t) 'vector))
    (#\" (coerce
          (loop :with char-to-add :and next-char
                :for this-char = (read-char stream t nil t)
                :until (char= this-char #\")
                :if (char= this-char #\\)
                :do  (setq char-to-add
                           (case (setq next-char (read-char stream t nil t))
                             (#\n #\Newline) (#\r #\Return) (#\f #\Page)
                             (#\t #\Tab) (#\v #\Linefeed) (t next-char)))
                :else :do (setq char-to-add this-char)
                :collect char-to-add)
          'string))
    (#\? (loop :for cc :of-type character = (read-char stream t nil t)
               :collect (if (char/= cc #\\) cc
                            (ecase (read-char stream t nil t)
                              (#\C (read-char stream t nil t) :control)
                              (#\^ :control)
                              (#\S (read-char stream t nil t) :shift)
                              (#\M (read-char stream t nil t) :meta)
                              (#\s (read-char stream t nil t) :super)
                              (#\H (read-char stream t nil t) :hyper)
                              (#\n (setq cc #\Null) #\Newline)
                              (#\t (setq cc #\Null) #\Tab)
                              (#\r (setq cc #\Null) #\Return)))
               :into res
               :finally (return (if (characterp (car res)) (car res) res))
               :while (char= cc #\\)))))

(defun el::make-elisp-readtable ()
  "Make the readtable for Emacs-Lisp parsing."
  (let ((rt (copy-readtable)))
    (set-syntax-from-char #\[ #\( rt)
    (set-syntax-from-char #\] #\) rt)
    (set-macro-character #\? #'el::read-elisp-special nil rt)
    (set-macro-character #\[ #'el::read-elisp-special nil rt)
    (set-macro-character #\" #'el::read-elisp-special nil rt)
    (set-macro-character #\] (get-macro-character #\) rt) nil rt)
    ;; (setf (readtable-case rt) :downcase)
    rt))

(defparameter *elisp-readtable* (el::make-elisp-readtable)
  "The readtable for Emacs-Lisp parsing.")

;;; bug in ACL and CMUCL
#+nil (progn
(defun read-vector (stream char)
  (coerce (read-delimited-list #\] stream t) 'vector))
(set-syntax-from-char #\[ #\()
(set-syntax-from-char #\] #\))
(set-macro-character #\[ #'read-vector nil)
(set-macro-character #\] (get-macro-character #\)) nil)
(read-from-string "[1 2 3]")
(read-from-string "#\\x")
(read-from-string "[#\\x]")
)

;;;
;;; Emacs-Lisp-specific functions
;;;

(defun el::memq (elt list) (member elt list :test #'eq))
(defun el::delq (elt list) (delete elt list :test #'eq))
(defun el::member (elt list) (member elt list :test #'equal))
(defun el::delete (elt list) (delete elt list :test #'equal))
(defun el::concat (&rest args) (apply #'concatenate 'string args))
(defun el::message (&rest args) (apply #'format t args))
(defun el::format (&rest args) (apply #'format nil args))
(defun el::put (symbol propname value) (setf (get symbol propname) value))
(defun el::fset (symbol def)
  (if (null def) (fmakunbound symbol)
      (setf (fdefinition symbol) (if (functionp def) def (fdefinition def)))))
(defun el::setcar (cons obj) (setf (car cons) obj))
(defun el::setcdr (cons obj) (setf (cdr cons) obj))
(defun el::ignore (&rest ignore) (declare (ignore ignore)) nil)
(defun el::sit-for (sec &optional (ms 0) nodisp)
  (declare (ignore nodisp))
  (sleep (+ sec (/ ms 1000))))
(defun el::string-to-number (string &optional (base 10))
  (parse-integer string :radix base))
(defun el::/ (&rest args)
  (if (every #'integerp args)
      (reduce #'floor (cdr args) :initial-value (car args))
      (apply #'/ args)))

(defun el::decode-time (&optional (time (get-universal-time)))
  (unless (numberp time)
    (setq time (+ (ash (car time) 16)
                  (if (numberp (cdr time)) (cdr time) (cadr time)))))
  (multiple-value-bind (sec minute hour day month year dow dst zone)
      (decode-universal-time time)
    (list sec minute hour day month year (mod (1+ dow) 7) dst (* 3600 zone))))

(defun el::encode-time (second minute hour day month year &rest zone)
  (encode-universal-time second minute hour day month year (car (last zone))))

(defmacro el::defalias (symbol def)
  `(setf (fdefinition ,symbol) (fdefinition ,def)))
(defmacro el::defgroup (&rest args) (declare (ignore args)))
(defmacro el::defcustom (var val doc &key (type t) &allow-other-keys)
  (let ((type (if (and (consp type) (eq 'quote (car type))) (cadr type) type)))
    `(progn (declaim (type ,type ,var)) (defvar ,var ,val ,doc))))
(defmacro el::defface (name val doc &rest args)
  (declare (ignore args))
  `(defvar ,name ,val ,doc))
(defmacro el::defconst (name val doc) `(defconstant ,name ,val ,doc))
(defmacro el::eval-when-compile (&rest body)
  `(eval-when (compile load) ,@body))
(defmacro el::setq-default (&rest body) `(setq ,@body))
(defmacro el::save-window-excursion (&rest body) `(progn ,@body))
(defmacro el::save-excursion (&rest body) `(progn ,@body))
(defmacro el::with-output-to-temp-buffer (&rest body) `(progn ,@body))

(setf (fdefinition 'el::number-to-string) #'prin1-to-string)
(setf (fdefinition 'el::int-to-string) #'el::number-to-string)
(setf (fdefinition 'el::char-to-string) #'string)
(setf (fdefinition 'el::string-to-int) #'el::string-to-number)
(setf (fdefinition 'el::file-truename) #'truename)
(setf (fdefinition 'el::file-exists-p) #'probe-file)
(setf (fdefinition 'el::substring) #'subseq)
(setf (fdefinition 'el::%) #'rem)
(setf (fdefinition 'el::file-directory-p) #'probe-directory)
(setf (fdefinition 'el::sref) #'aref)
(setf (fdefinition 'el::set-default) #'set)
(setf (fdefinition 'el::default-value) #'symbol-value)

(defun el::run-hooks (&rest hooks)
  (labels ((rh (hook)
             (etypecase hook
               (list (dolist (hk hook) (rh hk)))
               ((or symbol function) (funcall hook)))))
    (dolist (hk hooks) (when (boundp hk) (rh (symbol-value hk))))))

(defun el::add-hook (hook function &optional append local)
  (declare (ignore local))
  (let ((val (if (boundp hook) (symbol-value hook) nil)))
    (when (or (functionp val) (symbolp val)) (setq val (list val)))
    (unless (member function val :test #'equal)
      (setf (symbol-value hook)
            (if append (append val (list function))
                (cons function val))))))

(defun el::remove-hook (hook function &optional local)
  (declare (ignore local))
  (when (and (boundp hook) (symbol-value hook) function)
    (let ((val (symbol-value hook)))
      (if (or (functionp val) (symbolp val))
          (when (eq val function) (setf val nil))
          (setf (symbol-value hook) (remove function val :test #'equal))))))

(defun el::add-to-list (list el)
  (unless (boundp list) (setf (symbol-value list) nil))
  (pushnew el (symbol-value list) :test #'equal))

;;; tmp hacks
(setf (fdefinition 'el::define-key) #'el::ignore)
(setf (fdefinition 'el::make-sparse-keymap) #'el::ignore)
(setf (fdefinition 'el::substitute-key-definition) #'el::ignore)
;; (setf (fdefinition 'el::) #'el::ignore)
(setf (fdefinition 'el::interactive) #'el::ignore)
(setf (fdefinition 'el::make-help-screen) #'el::ignore)
(setf (fdefinition 'el::help-for-help) #'el::ignore)
(setf (fdefinition 'el::start-kbd-macro) #'el::ignore)
(setf (fdefinition 'el::substitute-command-keys) #'identity)
(defvar el::global-map (el::make-sparse-keymap))
(defvar el::help-char (code-char 8))
(defvar el::help-form nil)

(defvar el::window-system nil)
(defvar el::mode-line-format nil)
(defvar el::buffer-read-only nil)
(defvar el::indent-tabs-mode nil)

(deftype el::sexp () 'list)
(deftype el::file () 'string)
(deftype el::hook () '(or list symbol))

(defun el::mapconcat (function sequence separator)
  ;; (el::mapconcat #'identity '("a" "b" "c") " ") ==> "a b c"
  (apply #'concatenate 'string
         (cdr (mapcan (lambda (el) (list separator el))
                      (map 'list function sequence)))))

(defun to-directory (path)
  (if (char= #\/ (schar path (1- (length path)))) path
      (concatenate 'string path "/")))

(defun el::directory-files (directory &optional full match nosort)
  ;; incomplete
  (declare (ignore full nosort))
  (let ((dir (to-directory directory)))
    (directory (if match (merge-pathnames match dir) dir))))

(defun el::expand-file-name (name &optional (default-dir (default-directory)))
  (namestring (merge-pathnames (to-directory default-dir) name)))

(defun el::autoload (function file &optional docstring interactive type)
  (declare (symbol function) (type (or simple-string null) docstring)
           (ignore interactive type))
  (unless (fboundp function)
    (setf (fdefinition function)
          (lambda (&rest args)
            (setf (documentation function 'function) nil)
            (fmakunbound function)
            (format t "; ~s is being autoloaded from `~a'~%" function file)
            (el::load file)
            (apply function args))
          (documentation function 'function)
          (format nil "Autoloaded (from ~a)~@[:~%~a~]" file docstring))))

;;;
;;; Load Emacs-Lisp files
;;;

(defvar el::load-path
  '("/usr/share/emacs/site-lisp/"
    "/usr/src/emacs/lisp"
    "/usr/src/emacs/lisp/textmodes"
    "/usr/src/emacs/lisp/progmodes"
    "/usr/src/emacs/lisp/play"
    "/usr/src/emacs/lisp/mail"
    "/usr/src/emacs/lisp/language"
    "/usr/src/emacs/lisp/international"
    "/usr/src/emacs/lisp/gnus"
    "/usr/src/emacs/lisp/emulation"
    "/usr/src/emacs/lisp/emacs-lisp"
    "/usr/src/emacs/lisp/calendar"
    "/usr/src/emacs/leim"))

(defvar el::features nil)

(defun el::featurep (feature) (el::memq feature el::features))

#+allegro (pushnew "el" sys:*source-file-types* :test #'equal)
#+cmu (pushnew "el" ext::*load-source-types* :test #'equal)
#+clisp (pushnew #p".el" sys::*source-file-types* :test #'equalp)

(defun locate-file (file &optional source-only)
  (dolist (path el::load-path)
    (let ((dir (to-directory path)))
      (let ((ff (merge-pathnames file dir)))
        (when (and (ignore-errors (probe-file ff)) (not (probe-directory ff)))
          (return-from locate-file ff)))
      (unless source-only
        (let ((ff (merge-pathnames (concatenate 'string file *fas-ext*) dir)))
          (when (probe-file ff) (return-from locate-file ff))))
      (let ((ff (merge-pathnames (concatenate 'string file ".el") dir)))
        (when (probe-file ff) (return-from locate-file ff))))))

(defun el::load (file &optional noerror nomessage nosuffix must-suffix)
  "Emacs-Lisp load.
The suffix stuff is ignored."
  (declare (ignore nosuffix must-suffix))
  ;; (in-package :emacs-lisp)
  (let ((*readtable* *elisp-readtable*) (ff (locate-file file)))
    (if ff (load ff :verbose (not nomessage))
        (unless noerror (error "file `~a' not found" file)))))

(defun el::require (feature &optional file-name noerror)
  "Emacs-Lisp require"
  (unless (el::featurep feature)
    (let ((file (or file-name (string-downcase (string feature)))))
      (el::load file noerror)
      (assert (el::featurep feature) ()
              "loading `~a' failed to provide `~a'" file feature)))
  feature)

(defun compile-el-file (file)
  ;; (in-package :emacs-lisp)
  (let ((*readtable* *elisp-readtable*) (ff (or (locate-file file t) file)))
    (compile-file ff)))

(defun el::provide (feature) (pushnew feature el::features))

;; (in-package :cl-user)
;; (in-package :emacs-lisp)

;; (el::load "backquote")
;; (el::load "calendar")
;; (el::load "cal-hebrew")
;; (el::load "subr")
;; (el::load "help")
;; (cl-user::compile-el-file "backquote")
;; (cl-user::compile-el-file "calendar")
;; (cl-user::compile-el-file "cal-hebrew")
;; (calendar-hebrew-date-string)

(provide "elisp")
;;; file elisp.lisp ends here
