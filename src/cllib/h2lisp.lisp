;#!/usr/bin/clisp -M ~sds/bin/clisp.mem -C
;;; File: <h2lisp.lisp - 1999-01-12 Tue 18:11:48 EST sds@eho.eaglets.com>
;;;
;;; Convert *.c to CLISP's ffi
;;;
;;; Copyright (C) 1999 by Sam Steingold
;;;
;;; $Id: h2lisp.lisp,v 1.1 1999/01/12 23:12:16 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/h2lisp.lisp,v $
;;; $Log: h2lisp.lisp,v $
;;; Revision 1.1  1999/01/12 23:12:16  sds
;;; Initial revision
;;;
;;;

(in-package :cl-user)

(eval-when (load compile eval)
  (sds-require "base") (sds-require "url")
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

;;;
;;; C parsing
;;;

(defcustom *c-readtable* readtable (copy-readtable nil)
  "The readtable for C parsing.")

(defcustom *c-comment* cons (list :*c-comment*)
  "The comment marker.")
(defcustom *c-dim* cons (list :*c-dim*)
  "The comment array dimension marker.")

(defun read-c-junk (stream char)
  (declare (stream stream) (character char))
  (ecase char
    (#\{ (read-delimited-list #\} stream t))
    (#\[ (cons *c-dim* (read-delimited-list #\] stream t)))
    (#\; #\;) (#\, #\,) (#\: #\:) (#\* #\*)
    (#\#
     (let ((com (read stream t nil t)))
       (case com )))
;;     (loop :for line :of-type simple-string =
;;           (concatenate 'string "#" (read-line stream))
;;           :then (read-line stream)
;;           :until (char= #\\ (schar line (1- (length line))))
;;           :collect line)
    (#\/
     (case (peek-char nil stream nil nil t)
       (#\*
        (read-char stream)
        (cons *c-comment*
              (concatenate
               'string "/*"
               (coerce
                (loop :for c1 :of-type character = (read-char stream)
                      :and c2 :of-type character = #\Null :then c1
                      :until (and (char= c2 #\*) (char= c1 #\/))
                      :collect c1)
                'string) "/")))
       (#\/ (cons *c-comment* (concatenate 'string "/" (read-line stream))))
       (t '/)))))

(set-macro-character #\/ #'read-c-junk nil *c-readtable*)
;; (set-macro-character #\# #'read-c-junk nil *c-readtable*)

(set-syntax-from-char #\; #\a *c-readtable*)
(set-macro-character #\; #'read-c-junk nil *c-readtable*)
(set-syntax-from-char #\# #\a *c-readtable*)
(set-macro-character #\# #'read-c-junk nil *c-readtable*)
(set-syntax-from-char #\: #\a *c-readtable*)
(set-macro-character #\: #'read-c-junk nil *c-readtable*)
(set-syntax-from-char #\, #\a *c-readtable*)
(set-macro-character #\, #'read-c-junk nil *c-readtable*)
(set-macro-character #\* #'read-c-junk nil *c-readtable*)
(set-macro-character #\{ #'read-c-junk nil *c-readtable*)
(set-macro-character #\} (get-macro-character #\)) nil *c-readtable*)
(set-macro-character #\[ #'read-c-junk nil *c-readtable*)
(set-macro-character #\] (get-macro-character #\)) nil *c-readtable*)

(setf (readtable-case *c-readtable*) :preserve)

(defsubst c-comment-p (obj) (and (consp obj) (eq (car obj) *c-comment*)))

(defun uncomment-split (lst obj)
  "Read object from TS, remove comments, split on OBJ."
  (declare (list lst))
  (nsplit-list (delete-if #'c-comment-p lst) :obj obj))

(defun read-statement (ts)
  (declare (type text-stream ts))
  (loop :for zz = (read-next ts) :until (eql zz #\;)
        :unless (c-comment-p zz) :collect zz))

(defparameter *c-types* ; list
  '(int uint char)
  "Known C types.")
(defparameter *c-un-types* nil "UnKnown C types.")

(defun c-see-type (sym)
  (assert (and sym (not (string-equal (string sym) "void"))) ()
          "An attempt to define VOID or NIL: ~s" sym)
  (unless (member sym *c-types* :test #'eq)
    (pushnew sym *c-un-types* :test #'eq)))
(defun c-def-type (sym)
  ;(assert (not (member sym *c-types* :test #'eq)) () "redefining type ~s" sym)
  (push sym *c-types*)
  (setq *c-un-types* (delete sym *c-un-types* :test #'eq)))

(defun c-convert-decl (lst)
  "Convert a list of form (type nm [dim]) to (nm (c-array type dim))."
  (labels ((voidp (sy) (string-equal (string sy) "void"))
           (cc (ll)
             (etypecase (car ll)
               (cons (assert (eq (caar ll) *c-dim*))
                     `(c-array ,(cc (cdr ll)) ,(cadar ll)))
               (null nil)
               (symbol
                (cond ((voidp (car ll))
                       (if (eql #\* (cadr ll))
                           'c-pointer nil))
                      ((prog1 (if (eql #\* (cadr ll))
                                  `(c-ptr ,(car ll))
                                  (car ll))
                         (c-see-type (car ll)))))))))
    (let* ((fu (find-if-not #'consp lst :from-end t))
           (ta (member fu lst)) (ld (nconc (cdr ta) (ldiff lst ta))))
      (unless (voidp fu)
        (list fu (cc ld))))))

(defmacro string-beg-with (beg strv &optional (lenv `(length ,strv)))
  (let ((len (length beg)))
    `(and (>= ,lenv ,len) (string= ,beg ,strv :end2 ,len))))

;;;
;;; H --> LISP
;;;

(defgeneric h2lisp (in out)
  (:documentation "Convert C header to lisp.
Return the number of forms processed."))
(defmethod h2lisp ((in string) (out t)) (h2lisp (merge-pathnames in) out))
(defmethod h2lisp ((in cons) (out t))
  (reduce #'+ in :key (lambda (in0) (h2lisp in0 out))))
(defmethod h2lisp ((in t) (out string)) (h2lisp in (merge-pathnames out)))
(defmethod h2lisp ((in t) (out pathname))
  (with-open-file (fout out :direction :output :if-exists :supersede
                        :if-does-not-exist :create)
    (h2lisp in fout)))
(defmethod h2lisp ((in pathname) (out t))
  (with-open-file (fin in :direction :input)
    (h2lisp fin out)))
(defmethod h2lisp ((in stream) (out stream))
  (format out "~%;;; reading from ~a~2%" in)
  (unless (eq out *standard-output*)
    (format t "~%;;; reading from ~a~2%" in))
  (loop :with *readtable* = *c-readtable* :and numf :of-type index-t = 0
        ;; :initially (setq *c-un-types* nil)
        :for line :of-type simple-string =
        (string-trim
         +whitespace+
         (or (read-line in nil nil)
             (return (progn (format out "~%;;; ~:d form~:p in ~a~2%" numf in)
                            (unless (eq out *standard-output*)
                              (format t "~%;;; ~:d form~:p in ~a~2%" numf in))
                            (when *c-un-types*
                              (format t "~% *** Undefined types:~
~{~<~%~20t ~1,79:; ~a~>~^,~}~%" *c-un-types*))
                            numf))))
        :for len :of-type index-t = (length line)
        :with depth :of-type index-t = 0 ; #if nesting
        :with ts :of-type text-stream = (make-text-stream :sock in)
        :do                     ; process statement
        (cond ((zerop len) (terpri out))
              ((< len 3) (format out ";;; ~a~%" line))
              ((string-beg-with "/*" line len)
               ;; comment, assume start/end on own line
               (format out ";;; ~a~%" line)
               (loop :until (search "*/" line :test #'char=)
                     :do (setq line (read-line in))
                     (format out ";;; ~a~%" line)))
              ((string-beg-with "#if" line len)
               (incf depth)
               (format out ";;; ~a~%" line))
              ((string-beg-with "#endif" line len)
               (decf depth)
               (format out ";;; ~a~%" line))
              ((string-beg-with "#define" line len) (incf numf)
               ;; only 1-line simple defines!
               (format out ";;; ~a~%" line)
               (multiple-value-bind (var pos)
                   (read-from-string line nil +eof+ :start 7)
                 (let ((val (read-from-string line nil +eof+ :start pos)))
                   (typecase val
                     (symbol
                      (format out "(define-symbol-macro ~s ~s)~%" var val))
                     ((or number string)
                      (format out "(defconstant ~s ~s)~%" var val))))))
              ((string-beg-with "#include" line len) (incf numf)
               (format out "(c-lines \"~a~~%\")~%" line))
              ((string-beg-with "typedef struct" line len) (incf numf)
               ;; (format out ";;; ~a~%" line)
               (multiple-value-bind (t0 t1)
                   (string-tokens line :start 15 :max 2)
                 (c-def-type t1)
                 (c-see-type t0)
                 (format out "(def-c-type ~s ~s)~%" t0 t1)))
              ((string-beg-with "typedef enum" line len) (incf numf)
               (setf (ts-buff ts) line (ts-posn ts) 12)
               (let ((en (delete #\, (read-next ts))) (tt (read-next ts)))
                 (c-def-type tt)
                 (format out "(def-c-enum ~s~{~%  ~s~})~%" tt en)))
              ((string-beg-with "typedef" line len) (incf numf)
               ;; (format out ";;; ~a~%" line)
               (multiple-value-bind (t0 t1)
                   (string-tokens line :start 8 :max 2)
                 (c-def-type t1)
                 (c-see-type t0)
                 (format out "(def-c-type ~s ~s)~%" t0 t1)))
              ((string-beg-with "struct" line len) (incf numf)
               (setf (ts-buff ts) line (ts-posn ts) 6)
               (let ((tt (read-next ts))
                     (ll (map-in #'c-convert-decl
                                 (uncomment-split (read-next ts) #\;))))
                 (c-def-type tt)
                 (format out "(def-c-struct ~s~{~%  ~s~})~%" tt ll)))
              ((string-beg-with "extern \"C\" {" line len)
               (format out ";;; ~a~%" line))
              ((incf numf)  ; function declaration
               (setf (ts-buff ts) line (ts-posn ts) 0)
               (let* ((ll (read-statement ts))
                      (zz (uncomment-split (car (last ll)) #\,))
                      (args (mapcar #'c-convert-decl zz))
                      (fun (c-convert-decl (nbutlast ll))))
                 (format out "(def-c-call-out ~s
  (:arguments~{~<~%~12t ~1,79:; ~s~>~})~%  (:return-type~{ ~s~}))~%"
                         (car fun) args (cdr fun)))))))

;(h2lisp *standard-input* *standard-output*)

(provide "h2lisp")
;;; file h2lisp.lisp ends here
