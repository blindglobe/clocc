;;; File: <string.lisp - 2000-02-18 Fri 13:11:41 EST sds@ksp.com>
;;;
;;; string utilities
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and the precise copyright document.
;;;
;;; $Id: string.lisp,v 1.1 2000/02/18 20:24:11 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/string.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base")))
(in-package :cllib)

(eval-when (compile load eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

(export '(string-beg-with string-end-with string-beg-with-cs string-end-with-cs
          purge-string split-string split-seq substitute-subseq))

;;;
;;;
;;;

(defmacro string-beg-with (beg strv &optional (lenv `(length ,strv)))
  "Check whether the string STRV starts with BEG."
  (if (stringp beg)
      (let ((len (length beg)))
        `(and (>= ,lenv ,len) (string-equal ,beg ,strv :end2 ,len)))
      (with-gensyms ("SBW-" len)
        `(let ((,len (length ,beg)))
          (and (>= ,lenv ,len) (string-equal ,beg ,strv :end2 ,len))))))

(defmacro string-end-with (end strv &optional (lenv `(length ,strv)))
  "Check whether the string STRV ends with END."
  (if (stringp end)
      (let ((len (length end)) (ll (gensym "SEW-")))
        `(let ((,ll ,lenv))
          (and (>= ,ll ,len) (string-equal ,end ,strv :start2 (- ,ll ,len)))))
      (with-gensyms ("SEW-" len ll)
        `(let ((,len (length ,end)) (,ll ,lenv))
          (and (>= ,ll ,len)
           (string-equal ,end ,strv :start2 (- ,ll ,len)))))))

(defmacro string-beg-with-cs (beg strv &optional (lenv `(length ,strv)))
  "Check whether the string STRV starts with BEG, case sensitive."
  (if (stringp beg)
      (let ((len (length beg)))
        `(and (>= ,lenv ,len) (string= ,beg ,strv :end2 ,len)))
      (with-gensyms ("SBWC-" len)
        `(let ((,len (length ,beg)))
          (and (>= ,lenv ,len) (string= ,beg ,strv :end2 ,len))))))

(defmacro string-end-with-cs (end strv &optional (lenv `(length ,strv)))
  "Check whether the string STRV ends with END, case sensitive."
  (if (stringp end)
      (let ((len (length end)) (ll (gensym "SEWC-")))
        `(let ((,ll ,lenv))
          (and (>= ,ll ,len) (string= ,end ,strv :start2 (- ,ll ,len)))))
      (with-gensyms ("SEWC-" len ll)
        `(let ((,len (length ,end)) (,ll ,lenv))
          (and (>= ,ll ,len) (string= ,end ,strv :start2 (- ,ll ,len)))))))

(defcustom *string-junk* (simple-string 5) ":-,./"
  "The characters removed from a string by `purge-string'.")

(defsubst purge-string (str &optional (junk *string-junk*))
  "Non-destructively remove junk from string."
  (declare (simple-string str junk))
  (substitute-if #\Space (lambda (cc) (find cc junk :test #'char=)) str))

(defsubst split-string (str chars &rest opts)
  "Split the string on chars."
  (declare (string str) (sequence chars))
  (apply #'split-seq str (lambda (ch) (declare (character ch)) (find ch chars))
         opts))

(defun split-seq (seq pred &key (start 0) end key strict)
  "Return a list of subseq's of SEQ, split on predicate PRED.
Start from START, end with END.  If STRICT is non-nil, collect
zero-length subsequences too.
  (split-seq SEQ PRED &key (start 0) end key strict)"
  (declare (sequence seq) (type (function (t t) t) pred) (fixnum start))
  (loop :for st0 = (if strict start
                       (position-if-not pred seq :start start
                                        :end end :key key))
        :then (if strict (if st1 (1+ st1))
                  (position-if-not pred seq :start (or st1 st0)
                                   :end end :key key))
        :with st1 = 0 :while (and st0 st1) :do
        (setq st1 (position-if pred seq :start st0 :end end :key key))
        :collect (subseq seq st0 st1)))

(defun substitute-subseq (seq sub rep &key (start 0) end
                          (test #'eql) (key #'identity))
  "Substitute all subsequences in a sequence with a replacement sequence.
The result is of the same type as SEQ.
  (substitute-subseq SEQ SUB REPL &key START END TEST KEY)"
  (declare (sequence seq sub rep) (type index-t start))
  (do* ((olen (length sub)) (last start (+ next olen))
        (new (subseq seq 0 start))
        (type (etypecase seq (string 'string) (vector 'vector) (list 'list)))
        (next (search sub seq :start2 last :end2 end :test test :key key)
              (search sub seq :start2 last :end2 end :test test :key key)))
       ((null next) (concatenate type new (subseq seq last)))
    (declare (type index-t olen last) (symbol type) (sequence new))
    (setq new (concatenate type new (subseq seq last next) rep))))

(provide :string)
;;; }}} string.lisp ends here
