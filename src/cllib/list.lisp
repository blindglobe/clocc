;;; File: <list.lisp - 1998-04-21 Tue 19:31:18 EDT sds@mute.eaglets.com>
;;;
;;; Additional List Operations
;;;
;;; Copyright (C) 1997 by Sam Steingold.
;;; This is free software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: list.lisp,v 1.2 1998/04/21 23:31:40 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/list.lisp,v $
;;; $Log: list.lisp,v $
;;; Revision 1.2  1998/04/21 23:31:40  sds
;;; Added `with-nsplit' and `call-on-split'.
;;;
;;; Revision 1.1  1998/03/23 16:31:44  sds
;;; Initial revision
;;;

(in-package "CL-USER")

(eval-when (load compile eval) (sds-require "base"))

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

(defsubst paste (new ls)
  "Like PUSH, but do not modify LS."
  (declare (cons ls)) (setf (cdr ls) (cons (car ls) (cdr ls)) (car ls) new))

(defun nsplit-list (lst &key (pred #'eql) (key #'value))
  "Return the list of sublists of LST, separated using PRED. Destructive.
When (funcall pred a0 a1) is nil, a1 starts another sublist,
i.e., in all sublists KEY is the same according to PRED."
  (declare (list lst) (type (function (t t) t) pred)
	   (type (or function fixnum symbol) key))
  (when (symbolp key) (setq key (symbol-function key)))
  (typecase key
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
       (when ta (setf (cdr ta) nil))))
    (t (error "nsplit-list: wrong type of key: `~s' (~s)" key (type-of key)))))

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
	   (type (function (t) t) key))
  (let (pkey (res 0))
    (declare (fixnum res))
    (map nil (lambda (rec)
	       (let ((ckey (funcall key rec)))
		 (unless (apply pred pkey ckey args)
		   (incf res) (setq pkey ckey))))
	 seq)
    res))

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

(defun check-list-type (lst pred &key (key #'value) (out *standard-output*))
  "Check that all the elements of the list satisfy the predicate.
Like (every lst pred), but prints a message."
  (declare (list lst) (type (function (t) t) key pred) (stream out))
  (let ((err 0) kk)
    (declare (fixnum err))
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

(defmacro with-sublist ((newl oldl e0 e1 &key (key '#'identity) (test '#'eql))
			&body body)
  "Evaluate BODY, binding the NEWL to the sublist of OLDL from E0 to E1
inclusively. KEY and TEST have the usual meaning and default.
BODY may not modify the list structure of NEWL, or else!
Also, do NOT try to return a cons from NEWL. You'd be surprised!"
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
Also, do NOT try to return a cons from NEWL. You'd be surprised!"
  `(let (,newl)
    (unwind-protect
	 (progn (setq ,newl (nsplit-list ,oldl ,@split-args)) ,@body)
      (setq ,oldl (apply #'nconc ,newl)))))

(defun call-on-split (lst func &rest args
		      &key (split-key #'value) (split-pred #'eql)
		      &allow-other-keys)
  "Call FUNC on all sublists of LST generated by `nsplit-list'."
  (declare (list lst) (function func))
  (remf args :split-key) (remf args :split-pred)
  (with-nsplit (nl lst :key split-key :pred split-pred)
    (let ((ii -1) (cnt? (typep split-key 'fixnum)))
      (declare (fixnum ii))
      (mapcar (lambda (ll)
		(cons (if cnt? (incf ii) (funcall split-key (car ll)))
		      (apply func ll args))) nl))))

(provide "list")
;;; list.lisp ends here
