;;; File: <list.lisp - 1998-03-23 Mon 11:31:05 EST sds@mute.eaglets.com>
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
;;; $Id: list.lisp,v 1.1 1998/03/23 16:31:44 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/list.lisp,v $
;;; $Log: list.lisp,v $
;;; Revision 1.1  1998/03/23 16:31:44  sds
;;; Initial revision
;;;

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
    (t (error "nsplit-list: wrong type of key: `~s'" key))))

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

(defcustom *data-change-tolerance* double-float 0.25
  "*The default maximum relative change permissible in the data.")

(defun check-list-values (lst &key (tol *data-change-tolerance*)
			  (key #'value) (out *standard-output*) label)
  "Check that the successive values in the list LST are within
TOL (which defaults to *data-change-tolerance*) from each other.
KEY may be a function or a list of functions.
LABEL is the printing name of KEY (or list thereof). If omitted,
KEY is printed with ~a.
This is more of a UI function. See also `jumps'.
Return T if not errors found."
  (declare (list lst) (double-float tol) (stream out)
	   (type (or function symbol list) key))
  (format out "Checking the list for values of~?
for relative change of at least ~5,3f~%"
	  (list-format "~a") (to-list (if label label key)) tol)
  (do* ((chkey (lambda (v0 v1) (> (rel-diff v0 v1) tol)))
	(mkkey (if (listp key)
		   (lambda (rec ks)
		     (map-into ks #'(lambda (kk) (funcall kk rec)) key))
		   (lambda (rec ks) (declare (ignore ks)) (funcall key rec))))
	(chkeys (if (listp key) (lambda (k0 k1) (some chkey k0 k1)) chkey))
	(ll lst (cdr ll)) (r0 (car lst) r1) r1
	(k0 (funcall mkkey r0 (if (listp key) (make-list (length key)) nil)))
	(k1 (if (consp key) (make-list (length key)) nil))
	(err 0) (ix 0 (1+ ix)))
       ((null (cdr ll))
	(format out "~:d record~:p checked. ~d error~:p found.~%" (1+ ix) err)
	(zerop err))
    (declare (fixnum err ix))
    (setq r1 (second ll) k1 (funcall mkkey r1 k1))
    (when (funcall chkeys k0 k1)
      (format out " *** ~3d *** Error between records ~:d and ~:d:
 --- ~a~% --- ~a~%" (incf err) ix (1+ ix) r0 r1)
      (if (listp key)
	  (format out "Relative differences:~?~%"
		  (list-format "~5,3f") (mapcar #'rel-diff k0 k1))
	  (format out "Relative difference:~5,3f~%" (rel-diff k0 k1))))
    (rotatef k0 k1)))

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

(provide "list")
;;; list.lisp ends here
