;;; simple operations
;;;
;;; Copyright (C) 2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: simple.lisp,v 1.6 2000/09/26 16:53:47 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/simple.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base")))

(in-package :cllib)

(export '(ppprint-list nsublist fix-list to-list from-list zero-len-p paste
          skip-to-new flatten with-collect filter list-length-dotted))

;;;
;;; {{{ `with-collect'
;;;

(defmacro with-collect ((&rest collectors) &body forms)
  "Evaluate forms, collecting objects into lists.
Within the FORMS, you can use local macros listed among collectors,
they are returned as multiple values.
E.g., (with-collect (c1 c2) (dotimes (i 10) (if (oddp i) (c1 i) (c2 i))))
 ==> (1 3 5 7 9); (0 2 4 6 8) [2 values]
In CLISP, push/nreverse is about 1.25 times as fast as pushing into the
tail, so this macro uses push/nreverse on CLISP and push into the tail
on other lisps (which is 1.5-2 times as fast as push/nreverse there)."
  #+clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~s-RET-" cc)))
                     collectors)))
    `(let (,@ret)
      (declare (list ,@ret))
      (macrolet ,(mapcar (lambda (co re) `(,co (form) `(push ,form ,',re)))
                         collectors ret)
        ,@forms
        (values ,@(mapcar (lambda (re) `(sys::list-nreverse ,re)) ret)))))
  #-clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~s-RET-" cc)))
                     collectors))
        (tail (mapcar (lambda (cc) (gensym (format nil "~s-TAIL-" cc)))
                      collectors))
        (tmp (mapcar (lambda (cc) (gensym (format nil "~s-TMP-" cc)))
                     collectors)))
    `(let (,@ret ,@tail)
      (declare (list ,@ret ,@tail))
      (macrolet ,(mapcar (lambda (co re ta tm)
                           `(,co (form)
                             `(let ((,',tm (list ,form)))
                               (if ,',re (setf (cdr ,',ta) (setf ,',ta ,',tm))
                                   (setf ,',re (setf ,',ta ,',tm))))))
                         collectors ret tail tmp)
        ,@forms
        (values ,@ret)))))

(defun filter (lst test collect &key (key #'identity))
  "COLLECT those elements of LST which satisfy TEST.
AKA `remove-if-not':
 (filter lst test collect :key key) ==
 (let ((res (remove-if-not test lst :key key)))
   (map-into res (compose collect key) res))"
  (declare (list lst) (type (function (t) t) test collect key))
  (with-collect (coll)
    (dolist (el lst)
      (let ((kk (funcall key el)))
        (when (funcall test kk) (coll (funcall collect kk)))))))

;;;
;;; }}}{{{ misc
;;;

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

(defun flatten (ll)
  "atom -> (atom); (1 (2) (3 (4) (5 (6) 7) 8) 9) -> (1 2 3 4 5 6 7 8 9)"
  (labels ((fl (ll acc)
             (cond ((null ll) acc)
                   ((atom ll) (cons ll acc))
                   (t (fl (car ll) (fl (cdr ll) acc))))))
    (fl ll nil)))

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

(defun list-length-dotted (list)
  "Return the length of the list or nil if it is circular.
The second value is the last atom (i.e., `dotted-p')."
  (do ((nn 0 (+ nn 2))
       (fast list (cddr fast))
       (slow list (cdr slow)))
      (nil)
    (declare (type (integer 0) nn))
    (when (atom fast) (return (values nn fast)))
    (when (atom (cdr fast)) (return (values (1+ nn) (cdr fast))))
    (when (eq (cdr fast) slow) (return nil))))

;;; }}}

(provide :simple)
;;; file simple.lisp ends here
