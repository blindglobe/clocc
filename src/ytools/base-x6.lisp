;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools -*-
(in-package :ytools)
;;;$Id: base-x6.lisp,v 1.1 2009/11/21 04:24:57 airfoyle Exp $

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.



(eval-when (:compile-toplevel :load-toplevel :execute)
   (shadowing-import 'excl:*current-case-mode*))


(eval-when (:compile-toplevel :load-toplevel :execute)
   (export '(ytools-readtable* _ defun defmacro \\
	     make-eq-hash-table table-entry href walk-table clear-table
	     make-Array make-Symbol
	     Array-dimension Array-dimensions
	     is-Vector is-Array is-Symbol
             Symbol-name Symbol-plist Symbol-function Symbol-value
	     is-Keyword is-String memq assq nodup =< retain-if
	     is-Pair is-cons list-copy is-list
	     tuple tuple. pair head tail nthrest nthtail endtail left right
	     ;;;; one two three four five six seven eight nine ten
	     is-Char is-Integer is-Number
	     is-Float is-Single-float is-Double-float
	     is-Fixnum is-Ratio is-even is-odd is-sublist is-whitespace
	     is-Stream list->values values->list lastelt len
             string-length string-concat
	     build-symbol symno* true false keyword-package*
	     eval-when condense
	     assoc= alist-entry alist-entry-set alref. alref
	     include-if series car-eq take drop occurs-in empty-list
	     on-list on-list-if-new off-list -- loading-bogus
	     *current-case-mode*)))

;;;;(eval-when (:compile-toplevel :load-toplevel :execute)
;;;;
;;;; (defvar subr-synonyms* '())
;;;;		  
;;;;(defun define-subr-synonym (syn subr)
;;;;   (setf (symbol-function syn) (symbol-function subr))
;;;;   (let ((p (assoc syn subr-synonyms* :test #'eq)))
;;;;      (cond ((not p)
;;;;	     (setq subr-synonyms*
;;;;		   (cons (list syn subr) subr-synonyms*)))
;;;;	    (t
;;;;	     (setf (cadr p) subr))))
;;;;)
;;;;)

;;;; (subr-synonym =< <=)
;;; Replaced with its expansion -- 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-function '=<) (symbol-function '<=))
;;;;  (define-subr-synonym '=< '<=)
  (define-compiler-macro =< (&rest args) (excl::bq-cons '<= args)))

