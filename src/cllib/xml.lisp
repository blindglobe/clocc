;;; File: <xml.lisp - 2000-03-21 Tue 16:33:51 EST sds@ksp.com>
;;;
;;; XML parsing
;;;
;;; Copyright (C) 2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: xml.lisp,v 2.1 2000/03/21 22:39:21 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/xml.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  (require :gray (translate-logical-pathname "port:gray")))

(in-package :cllib)

(defpackage xml-tags (:use))
(defcustom *xml-pack* 'package (find-package :xml-tags)
  "The package with all the XML tags and entities.")
(defcustom *xml-amp* 'hash-table (make-hash-table :test 'eq)
  "The `&' entities")
(defcustom *xml-per* 'hash-table (make-hash-table :test 'eq)
  "The `%' entities")

(defstruct (xml-obj (:conc-name xml-))
  (name nil :type symbol)       ; in package XML-TAGS
  (attribs nil :type list)      ; alist of attrib/value
  (data nil :type list))        ; list of objects in the tag

(defmethod print-object ((xml xml-obj) (out stream))
  (if *print-readably* (call-next-method)
      (format out "<~a~:{ ~a=~s~}>~{~a~%~}</~a>~%" (xml-name xml)
              (xml-attribs xml) (xml-data xml) (xml-name xml))))

(defclass xml-stream-in (fundamental-character-input-stream)
  ((input :initarg :stream :initarg :input :type stream :reader xml-in)
   (tag-stack :type list :accessor xml-stack)
   (comment :accessor xml-comment :documentation
            "nil - outside comment, t - inside, 1 - inside, one `-' read"))
  (:documentation "The input stream for reading XML."))

(defmethod stream-read-char ((in xml-stream-in))
  (read-char (xml-in in)))
(defmethod stream-unread-char ((in xml-stream-in) (char character))
  (unread-char (xml-in in) char))
(defmethod stream-read-char-no-hang ((in xml-stream-in))
  (read-char-no-hang (xml-in in)))
(defmethod stream-peek-char ((in xml-stream-in))
  (peek-char (xml-in in)))
(defmethod stream-listen ((in xml-stream-in))
  (listen (xml-in in)))
(defmethod stream-read-line ((in xml-stream-in))
  (read-line (xml-in in)))
(defmethod stream-clear-input ((in xml-stream-in))
  (clear-input (xml-in in)))

(defun xml-trim-key (symbol)
  (intern (string-right-trim "=;" (symbol-name symbol)) *xml-pack*))

(defun xml-obj-from-list (list)
  (make-xml-obj
   :name (car list) :attribs
   (loop :for xx :on (cdr list) :by #'cddr
         :collect (list (xml-trim-key (car xx)) (cadr xx)))))

(defun xml-read-text (str term)
  (coerce (loop :for ch = (read-char str t nil t)
                :while (char/= ch term) :collect ch
                :finally (unread-char term str))
          'string))

(defun xml-read-comment (str)
  (loop :with ch :for data = (xml-read-text str '#\-)
        :collect data :into all :do (read-char str t nil t)
        :if (char= #\- (setq ch (read-char str t nil t))) :do
        (setq ch (read-char str t nil t))
        (assert (char= ch #\>) (ch) "~s[~a]: ~s instead of #\>"
                'xml-read-comment str ch)
        (return (reduce (lambda (s0 s1) (concatenate 'string s0 s1))
                        all :initial-value ""))
        :else :collect (concatenate 'string "-" (string ch)) :into all))

(defun read-xml (stream char)
  (case char
    (#\<                        ; read tag
     (let ((ch (read-char stream t nil t)) (*package* *xml-pack*))
       (case ch
         (#\/ (let ((tag (read-delimited-list #\> stream t)))
                (assert (null (cdr tag)) (tag)
                        "~s[~a]: end tag ~s has attributes ~s" 'read-xml stream
                        (car tag) (cdr tag))
                (car tag)))
         (#\? (let ((tags (read-delimited-list #\> stream t)))
                (assert (eq 'xml-tags::? (car (last tags))) (tags)
                        "~s[~a]: <? was terminated by ~s" 'read-xml stream
                        (car (last tags)))
                (nbutlast tags)
                (xml-obj-from-list tags)))
         (#\! (let ((obj (read stream t nil t)))
                (if (eq 'xml-tags::-- obj) (xml-read-comment stream)
                    (cons obj (read-delimited-list #\> stream t)))))
         (t (unread-char ch stream)
            (do* ((xml (xml-obj-from-list (read-delimited-list #\> stream t)))
                  (next (read stream t nil t) (read stream t nil t)))
                 ((symbolp next)
                  (assert (eq next (xml-name xml)) (next)
                          "~s[~a]: ~s was terminated by ~s" 'read-xml stream
                          (xml-name xml) next)
                  (nreverse (xml-data xml))
                  xml)
              (push next (xml-data xml)))))))
    (#\[ (read-delimited-list #\] stream t))
    ;; (#\& (gethash (xml-trim-key (read stream t nil t)) *xml-amp*))
    ;; (#\% (gethash (xml-trim-key (read stream t nil t)) *xml-per*))
    (t (xml-read-text stream #\<))))
;; (error "~s[~a]: ~s" 'read-xml stream char)

(defun make-xml-readtable ()
  (let ((rt (copy-readtable)))
    (set-macro-character #\< #'read-xml nil rt)
    (set-macro-character #\[ #'read-xml nil rt)
    ;; (set-macro-character #\& #'read-xml nil rt)
    ;; (set-macro-character #\% #'read-xml nil rt)
    (set-macro-character #\> (get-macro-character #\)) nil rt)
    (set-macro-character #\] (get-macro-character #\)) nil rt)
    (set-syntax-from-char #\; #\a rt)
    rt))

(defcustom *xml-readtable* readtable (make-xml-readtable)
  "The readtable for XML parsing.")

#+nil (progn
(in-package :cllib)

(read-from-file "~/tools/clisp/doc/impnotes.xml" :repeat 5
                :readtable (make-xml-readtable))
(lisp::setq lisp:*PRINT-READABLY* lisp::nil)
(let ((*readtable* (make-xml-readtable)))
  (read-from-string "abcd"))
)

(provide :xml)
;;; file xml.lisp ends here
