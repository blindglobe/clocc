;;; File: <closio.lisp - 2000-02-18 Fri 12:19:23 EST sds@ksp.com>
;;;
;;; Read/Write CLOS objects
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold
;;;
;;; Load this file and your CLOS objects will be printeed with #[] format;
;;; bind `*readtable*' to `*clos-readtable*' and `read' will read #[]
;;;
;;; $Id: closio.lisp,v 1.1 2000/02/18 20:24:11 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/closio.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base")))

(in-package :cllib)

(eval-when (load compile eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

(export '(pr *clos-readtable* make-clos-readtable class-slot-list
          arglist macroexpand-r))

;;;
;;; {{{ print the object readably
;;;

;;;###autoload
(defun pr (obj &optional (str *standard-output*) (nice t))
  "Print the OBJECT readably to the STREAM (default `*standard-output*').
Set `*print-circle*' and `*print-pretty*' to the third argument
NICE (default T).  Uses `with-standard-io-syntax'."
  (declare (stream str))
  (with-standard-io-syntax
    (let (#+clisp (lisp:*print-indent-lists* 1)
          #+clisp (lisp:*print-rpars* nil))
      (write obj :stream str :case :downcase :circle nice :pretty nice)))
  (values))

;;;
;;; }}}{{{ print structure in CMUCL via `print-object'
;;;

#+cmu
(progn
  (defmethod print-object ((xx structure-object) (out stream))
      (kernel:default-structure-print xx out 1))
  (defun print-struct-object (xx out depth)
    (declare (ignore depth))
    (print-object xx out)))

;;;
;;; }}}{{{ read CLOS objects
;;;

(eval-when (compile load eval)  ; for `*clos-readtable*'
(defun read-object (st char arg)
  "Read an instance of a CLOS class printed as #[name{ slot val}]"
  (declare (ignore char arg))
  (apply #'make-instance (read-delimited-list #\] st t)))

(defun make-clos-readtable ()
  "Return the readtable for reading #[]."
  (let ((rt (copy-readtable)))
    (set-syntax-from-char #\[ #\( rt)
    (set-syntax-from-char #\] #\) rt)
    (set-macro-character #\] (get-macro-character #\) rt) nil rt)
    (set-dispatch-macro-character #\# #\[ #'read-object rt)
    rt))
)

(defconst *clos-readtable* readtable (make-clos-readtable)
  "The readtable for reading CLOS objects printed readably with #[].")

;;;
;;; }}}{{{ print CLOS objects readably
;;;

(defun class-slot-list (class &optional (all t))
  "Return the list of slots of a CLASS.
CLASS can be a symbol, a class object (as returned by `class-of')
or an instance of a class.
If the second optional argument ALL is non-NIL (default),
all slots are returned, otherwise only the slots with
:allocation type :instance are returned."
  #-(or allegro clisp cmu lispworks)
  (error 'not-implemented :proc 'class-slot-list)
  (macrolet ((class-slots* (class)
               `(#+allegro clos:class-slots
                 #+clisp clos::class-slots
                 #+cmu pcl::class-slots
                 #+lispworks hcl::class-slots ,class))
             (slot-name (slot)
               #+allegro `(slot-value ,slot 'clos::name)
               #+clisp `(clos::slotdef-name ,slot)
               #+cmu `(slot-value ,slot 'pcl::name)
               #+lispworks `(hcl::slot-definition-name ,slot))
             (slot-alloc (slot)
               `(#+allegro clos::slotd-allocation
                 #+clisp clos::slotdef-allocation
                 #+cmu pcl::slot-definition-allocation
                 #+lispworks hcl::slot-definition-allocation ,slot)))
    (mapcan (lambda (slot)
              (when (or all (eq (slot-alloc slot) :instance))
                (list (slot-name slot))))
            (class-slots*
             (typecase class
               (class class) (symbol (find-class class))
               ((or structure-object standard-object) (class-of class))
               (t (error 'case-error :proc 'class-slot-list
                         :args (list 'class class 'class 'symbol
                                     'structure-object 'standard-object))))))))

(defmethod print-object ((obj standard-object) (out stream))
  (let ((cl (class-of obj)))
    (format out "#[~s" (class-name cl))
    (dolist (slot (class-slot-list cl nil))
      (when (slot-boundp obj slot)
        (format out " ~s ~s" slot (slot-value obj slot))))
    (write-string "]" out)))

;;;
;;; }}}{{{ arglist & macroexpand-r
;;;

(defun arglist (fn)
  "Return the signature of the function."
  #+allegro (ext:arglist fn)
  #+clisp (sys::arglist fn)
  #+cmu (values (let ((st (kernel:%function-arglist fn)))
                  (if (stringp st) (read-from-string st)
                      (eval:interpreted-function-arglist fn))))
  #+gcl (let ((fn (etypecase fn
                    (symbol fn)
                    (function (system:compiled-function-name fn)))))
          (get fn 'si:debug))
  ;; #+lispworks
  #-(or allegro clisp cmu gcl) (error 'not-implemented :proc 'arglist))

(defun macroexpand-r (form)
  "Recursive macroexpand - unreliable because of `macrolet' &c."
  (if (atom form) form
      (let ((res (macroexpand form)))
        (cons (car res) (mapcar #'macroexpand-r (cdr res))))))

;;; }}}

(provide :closio)
;;; file closio.lisp ends here
