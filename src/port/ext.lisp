;;; Basic extensions: conditions &c
;;;
;;; Copyright (C) 1999 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: ext.lisp,v 1.5 2000/03/22 23:51:52 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/port/ext.lisp,v $
;;; $Log: ext.lisp,v $
;;; Revision 1.5  2000/03/22 23:51:52  sds
;;; (quit): optional error code argument
;;;
;;; Revision 1.4  2000/03/03 22:01:03  sds
;;; fixed provide statements
;;;
;;; Revision 1.3  2000/02/18 21:16:45  sds
;;; in-package :port now; make system works
;;;
;;; Revision 1.2  2000/02/10 17:53:40  sds
;;; (+eof+): new constant (for `string-tokens')
;;; (string-tokens): new function (for net)
;;;
;;; Revision 1.1  1999/11/24 17:07:09  sds
;;; Cross-implementation Portability System
;;;
;;;

(defpackage port
  (:use "COMMON-LISP")
  (:nicknames "ORG.CONS.CLOCC/SDS/PORT"))

(in-package :port)

(setf (logical-pathname-translations "port")
      `(("**;*" ,(logical-pathname "clocc:src;port;**;*"))))

(export
 '(code case-error not-implemented ; conditions
   defsubst defcustom defconst
   mk-arr map-in with-gensyms
   gc quit
   +eof+ string-tokens
   compose compose-f compose-all))

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
;;; Extensions
;;;

(defmacro defsubst (name arglist &body body)
  "Declare an inline defun."
  `(progn (declaim (inline ,name)) (defun ,name ,arglist ,@body)))

(defmacro defcustom (name type init doc)
  "Define a typed variable."
  `(progn (declaim (type ,type ,name))
    (defvar ,name (the ,type ,init) ,doc)))

(defmacro defconst (name type init doc)
  "Define a typed constant."
  `(eval-when (compile load eval) ; kill compile warnings
    (unless (boundp ',name) (declaim (type ,type ,name))
            (defconstant ,name (the ,type ,init) ,doc))))

(defmacro mk-arr (type init &optional len)
  "Make array with elements of TYPE, initializing."
  (if len `(make-array ,len :element-type ,type :initial-element ,init)
      `(make-array (length ,init) :element-type ,type
        :initial-contents ,init)))

(defmacro with-gensyms (syms &body body)
  "Bind symbols to gensyms.  First sym is a string - `gensym' prefix.
Inspired by Paul Graham, <On Lisp>, p. 145."
  `(let (,@(mapcar (lambda (sy) `(,sy (gensym ,(car syms)))) (cdr syms)))
    ,@body))

(defmacro map-in (fn seq &rest seqs)
  "`map-into' the first sequence, evaluating it once.
  (map-in F S) == (map-into S F S)"
  (with-gensyms ("MI-" mi)
    `(let ((,mi ,seq)) (map-into ,mi ,fn ,mi ,@seqs))))

(defun gc ()
  "Invoke the garbage collector."
  #+allegro (excl:gc)
  #+clisp (lisp:gc)
  #+cmu (ext:gc)
  #+gcl (si::gbc)
  #+lispworks (normal-gc)
  #-(or allegro clisp cmu gcl lispworks)
  (error 'not-implemented :proc (list 'gc)))

(defun quit (&optional code)
  #+allegro (excl:exit code)
  #+clisp (lisp:quit code)
  #+cmu (ext:quit code)
  #+gcl (lisp:bye code)
  #-(or allegro clisp cmu gcl)
  (error 'not-implemented :proc (list 'quit code)))

(defconst +eof+ cons (cons nil nil)
  "*The end-of-file object.
To be passed as the third arg to `read' and checked against using `eq'.")

(defun string-tokens (string &key (start 0) max)
  "Read from STRING repeatedly, starting with START, up to MAX tokens.
Return the list of objects read and the final index in STRING.
Binds `*package*' to the keyword package,
so that the bare symbols are read as keywords."
  (declare (type (or null fixnum) max) (type fixnum start))
  (do ((beg start) obj res (num 0 (1+ num))
       (*package* (find-package "KEYWORD")))
      ((and max (= max num)) (values (nreverse res) beg))
    (declare (fixnum beg num))
    (setf (values obj beg)
          (read-from-string string nil +eof+ :start beg))
    (if (eq obj +eof+)
        (return (values (nreverse res) beg))
        (push obj res))))

;;;
;;; Function Compositions
;;;

(defmacro compose (&rest functions)
  "Macro: compose functions or macros of 1 argument into a lambda.
E.g., (compose abs (dl-val zz) 'key) ==>
  (lambda (yy) (abs (funcall (dl-val zz) (funcall key yy))))"
  (labels ((rec (xx yy)
             (let ((rr (list (car xx) (if (cdr xx) (rec (cdr xx) yy) yy))))
               (if (consp (car xx))
                   (cons 'funcall (if (eq (caar xx) 'quote)
                                      (cons (cadar xx) (cdr rr)) rr))
                   rr))))
    (with-gensyms ("COMPOSE-" arg)
      (let ((ff (rec functions arg)))
        `(lambda (,arg) ,ff)))))

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

(provide :ext)
;;; file ext.lisp ends here
