;;; File: <list.lisp - 1999-04-09 Fri 14:47:08 EDT sds@eho.eaglets.com>
;;;
;;; Additional List Operations
;;;
;;; Copyright (C) 1997, 1998 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: list.lisp,v 1.8 1999/04/09 18:48:18 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/list.lisp,v $
;;; $Log: list.lisp,v $
;;; Revision 1.8  1999/04/09 18:48:18  sds
;;; Added `collecting'.
;;;
;;; Revision 1.7  1999/02/22 22:56:53  sds
;;; `call-on-split': new key `:min-len'.
;;;
;;; Revision 1.6  1999/01/12 22:09:36  sds
;;; Fixed the previous feature.
;;;
;;; Revision 1.5  1999/01/12 18:52:19  sds
;;; Added key `obj' to `nsplit-list'.
;;;
;;; Revision 1.4  1999/01/07 04:06:30  sds
;;; Use `index-t' instead of (unsigned-byte 20).
;;;
;;; Revision 1.3  1998/05/27 21:23:41  sds
;;; Moved the sorted stuff from date.lisp here.
;;; Moved `freqs' from math.lisp here.
;;; Added `zero-len-p'.
;;;
;;; Revision 1.2  1998/04/21 23:31:40  sds
;;; Added `with-nsplit' and `call-on-split'.
;;;
;;; Revision 1.1  1998/03/23 16:31:44  sds
;;; Initial revision
;;;

(in-package :cl-user)

(eval-when (load compile eval)
  (sds-require "base")
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

(defun ppprint-list (lst &optional (stream t))
  "Print a long list nicely."
  (declare (list lst))
  (format stream "[~a <~:d> ~a]" (car lst) (length lst) (car (last lst))))

(defun nsublist (lst &optional pos0 pos1)
  "Return the part of the list between pos0 and pos1, *destructively*.
The indexing starts from 0, so (nsublist '(1 2 3 4 5) nil 2) ==> (1 2 3)."
  (declare (list lst))
  (when pos1 (let ((cut (nthcdr pos1 lst))) (when cut (setf (cdr cut) nil))))
  (if pos0 (nthcdr pos0 lst) lst))

(defun fix-list (ls)
  "Turn (aa bb . cc) into (aa bb cc)."
  (let ((ll (last ls)))
    (when (cdr ll) (setf (cdr ll) (cons (cdr ll) nil)))) ls)

(defsubst to-list (zz)
  "If ZZ is a list, return ZZ, otherwise return (list ZZ)."
  (if (listp zz) zz (list zz)))

(defsubst from-list (zz)
  "If ZZ is a list, return (car ZZ), otherwise return ZZ."
  (if (listp zz) (car zz) zz))

(defun zero-len-p (seq)
  "Returns T iff the sequence has zero length.
Works in constant time even with lists."
  (declare (sequence seq))
  (or (null seq) (and (vectorp seq) (zerop (length seq)))))

(defsubst paste (new ls)
  "Like `push', but do not modify LS."
  (declare (cons ls))
  (setf (cdr ls) (cons (car ls) (cdr ls)) (car ls) new) ls)

(defun skip-to-new (lst &key (test #'eql) (key #'value))
  "Return the tail of the list LST with the KEY different by TEST
from the previous one."
  (declare (list lst) (type (function (t t) t) test)
           (type (function (t) t) key))
  (do ((ll lst (cdr ll)) (k0 (funcall key (first lst)) k1) k1)
      ((or (null (cdr lst))
           (not (funcall test k0 (setq k1 (funcall key (second ll))))))
       ll)))

(defun jumps (seq &key (pred #'eql) (key #'value) args (what :next))
  "Return the list of elements of the sequence SEQ whose KEY differs
from that of the previous element according to the predicate PRED.
ARGS (list) are passed to PRED after the previous and the current KEYs.
WHAT can be :BOTH (list of conses of the previous and the next records,
:PREV (list of records before the jump) or :NEXT (list of records after
the jump). Default is :NEXT."
  (declare (sequence seq) (type (function (t t) t) pred)
           (type (function (t) t) key))
  (let (pkey res prec)
    (declare (list res))
    (map nil (lambda (rec)
               (let ((ckey (funcall key rec)))
                 (unless (apply pred pkey ckey args)
                   (push (cond ((eq what :both) (cons prec rec))
                               ((eq what :prev) prec)
                               (t rec)) res)
                   (setq pkey ckey))
                 (setq prec rec)))
         seq)
    (nreverse res)))

(defun count-jumps (seq &key (pred #'eql) (key #'value) args)
  "Like `jumps', but only count the jumps.
Thus, (apply #'count-jumps args) == (length (apply #'jumps args))."
  (declare (sequence seq) (type (function (t t) t) pred)
           (type (function (t) t) key) (values index-t))
  (let (pkey (res 0))
    (declare (type index-t res))
    (map nil (lambda (rec)
               (let ((ckey (funcall key rec)))
                 (unless (apply pred pkey ckey args)
                   (incf res) (setq pkey ckey))))
         seq)
    res))

(defun freqs (seq &key (test #'eql) (key #'identity))
  "Return an alist of (num . freq) of elements of the SEQ.
The alist is sorted by decreasing frequencies. TEST defaults to `eql'."
  (declare (sequence seq) (type (function (t t) t) test)
           (type (function (t) t) key))
  (unless (zero-len-p seq)
    (sort
     (reduce (lambda (res el)
               (let ((fi (assoc el res :test test)))
                 (cond (fi (incf (cdr fi)) res) ((acons el 1 res)))))
             seq :key key :initial-value nil)
     #'> :key #'cdr)))

(defmacro collecting (&body forms)
  "Evaluate forms, collecting objects into a list.
Within the FORMS, you can use a local macro `collect'."
  #+clisp                       ; this is faster in CLISP
  (let ((ret (gensym "COLLECTING")))
    `(let ((,ret nil))
      (macrolet ((collect (form) `(push ,form ,',ret)))
        ,@forms
        (nreverse ,ret))))
  #-clisp                       ; this is faster in natively compiling lisps
  (let ((ret (gensym "COLLECTING")) (tail (gensym "COLLECTING")))
    `(let ((,ret nil) (,tail nil))
      (macrolet ((collect (form)
                   `(if ,',ret
                     (setf (cdr ,',tail) (setf ,',tail (list ,form)))
                     (setf ,',ret (setf ,',tail (list ,form))))))
        ,@forms
        ,ret))))

;;;
;;; Sorted
;;;

(defmacro process-and-shift (pred akey ckey t0 e0 k0 t1 e1 k1)
  "Used in *-sorted."
  `(cond ((or (null k1) (and k0 (funcall ,pred ,k0 ,k1)))
          (multiple-value-prog1 (values (funcall ,akey ,e0) nil)
            (setq ,t0 (cdr ,t0) ,e0 (car ,t0)
                  ,k0 (and ,t0 (funcall ,ckey ,e0)))))
    ((or (null k0) (and k1 (funcall ,pred ,k1 ,k0)))
     (multiple-value-prog1 (values nil (funcall ,akey ,e1))
       (setq ,t1 (cdr ,t1) ,e1 (car ,t1) ,k1 (and ,t1 (funcall ckey ,e1)))))
    (t (multiple-value-prog1 (values (funcall ,akey ,e0) (funcall ,akey ,e1))
         (setq ,t0 (cdr ,t0) ,e0 (car ,t0) ,k0 (and ,t0 (funcall ,ckey ,e0))
               ,t1 (cdr ,t1) ,e1 (car ,t1)
               ,k1 (and ,t1 (funcall ,ckey ,e1)))))))

(defun map-sorted (type func pred l0 l1
                   &key (ckey #'identity) (akey #'identity))
  "Operate on two sorted lists. Call FUNC on the elements of the lists
that are `same' according to PRED. If TYPE is 'LIST, return the list
of whatever FUNC returns."
  (declare (function func pred ckey akey) (list l0 l1) (symbol type))
  (do ((t0 l0) (t1 l1) (e0 (car l0)) (e1 (car l1)) el res
       (k0 (and l0 (funcall ckey (car l0))))
       (k1 (and l1 (funcall ckey (car l1)))))
      ((and (null t0) (null t1)) (nreverse res))
    (setq el (multiple-value-call func
               (process-and-shift pred akey ckey t0 e0 k0 t1 e1 k1)))
    (when type (push el res))))

(defun reduce-sorted (rfunc func2 pred l0 l1
                      &key (ckey #'identity) (akey #'identity) initial-value)
  "Reduce a pair of sorted sequences."
  (declare (function rfunc func2 pred ckey akey) (list l0 l1))
  (let ((res initial-value) (t0 l0) (t1 l1) (e0 (car l0)) (e1 (car l1))
        (k0 (and l0 (funcall ckey (car l0))))
        (k1 (and l1 (funcall ckey (car l1)))))
    (unless res
      (setq res
            (if (or l0 l1)
                (multiple-value-call func2
                  (process-and-shift pred akey ckey t0 e0 k0 t1 e1 k1))
                (funcall rfunc))))
    (do () ((and (null t0) (null t1)) res)
      (setq res (funcall rfunc res
                         (multiple-value-call func2
                           (process-and-shift pred akey ckey
                                              t0 e0 k0 t1 e1 k1)))))))

(defun sorted-map (type func pred missing ckey akey &rest lists)
  "Operate on the corresponding elements of the sorted lists.  Each list
in LISTS is assumed to be sorted according to the predicate PRED applied
to keys CKEY.  Apply function FUNC to the AKEYs of the elements of the
lists with the same CKEYs.  When a list doesn't have an element with the
particular CKEY, function gets nil (if MISSING is nil) or the previous
AKEY (if MISSING is non-nil).
CKEY and AKEY values of nil are the same as #'identity.
  (sorted-map type func pred missing ckey akey &rest lists)"
  (declare (function func pred) (symbol type) (list lists)
           (type (or function null) ckey akey))
  (do ((sec (copy-list lists)) (akeys (make-list (length lists))) begck ck
       (ckey (or ckey #'identity)) (akey (or akey #'identity)) fnn res)
      ((every #'null sec) (nreverse res))
    #-cmu (declare (type (function (t) t) ckey akey))
    ;; get the current ckey
    (setq fnn (member nil sec :test-not #'eq)
          begck (funcall ckey (caar fnn)))
    (dolist (ls (rest fnn))
      (when ls (setq ck (funcall ckey (car ls)))
            (when (funcall pred ck begck) (setq begck ck))))
    ;; shift and operate
    (mapl (lambda (ls ak)
            (cond ((and (car ls)
                        (not (funcall pred begck (funcall ckey (caar ls)))))
                   (setf (car ak) (funcall akey (caar ls)))
                   (pop (car ls)))
                  (t (if missing nil (setf (car ak) nil)))))
          sec akeys)
    (cond ((eq type 'list) (push (apply func akeys) res))
          (t (apply func akeys)))))

(defun delete-duplicate-entries (lst &key (key #'identity) (test #'eql)
                                 keep-first)
  "Like `delete-duplicates', but assumes that the list LST is ordered.
Keeps the last entry, or the first if KEEP-FIRST non-nil."
  (declare (list lst) (type (function (t) t) key)
           (type (function (t t) t) test))
  (do ((ls lst) (kk (and (car lst) (funcall key (car lst))) k1) k1)
      ((endp (cdr ls)) lst)
    (if (funcall test kk (setq k1 (funcall key (second ls))))
        (setf (car ls) (if keep-first (car ls) (cadr ls)) (cdr ls) (cddr ls))
        (setq ls (cdr ls)))))

;;;
;;; Checking
;;;

(defun check-list-type (lst pred &key (key #'value) (out *standard-output*))
  "Check that all the elements of the list satisfy the predicate.
Like (every lst pred), but prints a message."
  (declare (list lst) (type (function (t) t) key pred) (stream out))
  (let ((err 0) kk)
    (declare (type index-t err))
    (format out "~&Checking list (length: ~d) for type `~a'.~%"
            (length lst) pred)
    (dolist (rec lst)
      (setq kk (funcall key rec))
      (unless (funcall pred kk)
        (format out " *** Record `~a' ~:[~?~;~2*~]fails predicate `~a'.~%"
                rec (eq #'identity key) "[key (~a): `~a'] " (list key kk) pred)
        (incf err)))
    (if (zerop err)
        (format out "No errors.~%")
        (format out "~d records failed the test.~%" err))
    err))

;;;
;;; splitting, sublists
;;;

(defun nsplit-list (lst &key (pred #'eql) (key #'identity) (obj nil objp))
  "Return the list of sublists of LST, separated using PRED. Destructive.
When (funcall pred a0 a1) is nil, a1 starts another sublist,
i.e., in all sublists KEY is the same according to PRED.
When OBJ is given, it serves as separator and is omitted from the list."
  (declare (list lst) (type (function (t t) t) pred)
           (type (or function fixnum symbol) key))
  (when (symbolp key) (setq key (fdefinition key)))
  (unless lst (return-from nsplit-list nil))
  (if objp
      (do ((ll lst) (bb lst) res)
          ((null ll) (nreverse (if bb (cons bb res) res)))
        (if (funcall pred (funcall key (cadr ll)) obj)
            (setf res (cons bb res) bb (cddr ll) (cdr ll) nil ll bb)
            (setq ll (cdr ll))))
      (etypecase key
        (function
         (do ((ll lst) (k0 (funcall key (first lst)) k1) k1 (res (list lst)))
             ((endp (cdr ll)) (nreverse res))
           (setq k1 (funcall key (second ll)))
           (cond ((not (funcall pred k0 k1))
                  (push (cdr ll) res)
                  (setf (cdr ll) nil)
                  (setq ll (car res)))
                 (t (setq ll (cdr ll))))))
        (fixnum
         (decf key)
         (do* ((ll lst) ta res) ((endp ll) (nreverse res))
           (push ll res) (setq ta (nthcdr key ll) ll (cdr ta))
           (when ta (setf (cdr ta) nil)))))))

(defmacro with-sublist ((newl oldl e0 e1 &key (key '#'identity) (test '#'eql))
                        &body body)
  "Evaluate BODY, binding the NEWL to the sublist of OLDL from E0 to E1
inclusively. KEY and TEST have the usual meaning and default.
BODY may not modify the list structure of NEWL, or else!
Also, do NOT try to return a cons from NEWL.  You'd be surprised!"
  (let ((tt (gensym "WSL")) (kk (gensym "WSL")))
    `(let* (,kk (,newl (member-if (lambda (el) (setq ,kk (funcall ,key el))
                                          (or (funcall ,test ,kk ,e0)
                                              (funcall ,test ,kk ,e1))) ,oldl))
            (,tt (member (if (funcall ,test ,kk ,e0) ,e1 ,e0) ,newl :key
                         ,key :test ,test)))
      (unwind-protect
           (progn (when ,tt (setq ,kk (cdr ,tt)) (setf (cdr ,tt) nil))
                  ,@body)
        (when ,tt (setf (cdr ,tt) ,kk))))))

(defmacro with-nsplit ((newl oldl &rest split-args) &body body)
  "Evaluate BODY, binding NEWL to the splitting of OLDL.
BODY may not modify the list structure of NEWL, or else!
Also, do NOT try to return a cons from NEWL.  You'd be surprised!"
  `(let (,newl)
    (unwind-protect
         (progn (setq ,newl (nsplit-list ,oldl ,@split-args)) ,@body)
      (setq ,oldl (apply #'nconc ,newl)))))

(defun call-on-split (lst func &rest args &key (split-key #'value)
                      (split-pred #'eql) min-len &allow-other-keys)
  "Call FUNC on all sublists of LST generated by `nsplit-list'."
  (declare (list lst) (function func) (type (or null fixnum) min-len))
  (remf args :split-key) (remf args :split-pred) (remf args :min-len)
  (with-nsplit (nl lst :key split-key :pred split-pred)
    (let ((ii -1) (cnt? (typep split-key 'fixnum)))
      (declare (type (signed-byte 21) ii))
      (mapcan (lambda (ll)
                (when (or (null min-len) (> (length ll) min-len))
                  (list (cons (if cnt? (incf ii) (funcall split-key (car ll)))
                              (apply func ll args)))))
              nl))))

(provide "list")
;;; list.lisp ends here
