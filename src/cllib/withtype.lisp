;;; File: <withtype.lisp - 2000-02-18 Fri 11:01:49 EST sds@ksp.com>
;;;
;;; Stuff for Type-optimizations and declarations
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold
;;;
;;; $Id: withtype.lisp,v 1.1 2000/02/18 20:24:11 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/withtype.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base")))

(in-package :cllib)

(eval-when (load compile eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

(export '(index-t map-vec dfloat with-type))

;;;
;;;
;;;

(defmacro map-vec (type len &rest args)
  "MAP into a simple-array with elements of TYPE and length LEN."
  `(map-into (make-array ,len :element-type ,type) ,@args))

(defmacro dfloat (num)
  "Coerce to double float."
  `(float ,num 1.0d0))

(deftype index-t () '(unsigned-byte 20))

(defmacro with-type (type expr)
  "Evaluate the arithmetic expression in TYPE.
Adopted from P.Graham `ANSI CL', p 410; with some modifications."
  `(the ,type
    ,(if (and (consp expr)
              (member (car expr) '(+ - * / abs sin cos tan cot
                                   signum log exp expt)
                      :test #'eq))
         (let ((nexp
                (labels ((binarize (expr)
                           (if (and (nthcdr 3 expr)
                                    (member (car expr) '(+ - * /)))
                               (destructuring-bind (op a1 a2 . rest) expr
                                 (binarize `(,op (,op ,a1 ,a2) ,@rest)))
                               expr)))
                  (binarize expr))))
           `(,(car nexp) ,@(mapcar #'(lambda (ee) `(with-type ,type ,ee))
                                   (cdr nexp))))
         expr)))

(provide :withtype)
;;; file withtype.lisp ends here
