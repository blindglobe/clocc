;;; File: <htmlgen.lisp - 2000-03-08 Wed 21:20:22 Eastern Standard Time sds@ksp.com>
;;;
;;; HTML generation
;;;
;;; Copyright (C) 2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: htmlgen.lisp,v 1.4 2000/03/13 21:53:28 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/htmlgen.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `dttm->string' - needed only for `directory-index'
  ;; (require :date (translate-logical-pathname "cllib:date"))
  ;; "Gray streams"
  (require :gray (translate-logical-pathname "port:gray")))

(in-package :cllib)

(export '(with-html-output with-tag directory-index html-stream-out))

;;;
;;; preparation
;;;

(defcustom *html-chars* list '((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;"))
  "The characters which must be replaced before putting a string into HTML.")

(defclass html-stream-out (fundamental-character-output-stream)
  ((target-stream :initarg :stream :type stream)))
(defmethod stream-write-char ((stream html-stream-out) ch)
  (with-slots (target-stream) stream
    (let ((char-cons (assoc ch *html-chars* :test #'char=)))
      (if char-cons (write-string (cdr char-cons) target-stream)
          (write-char ch target-stream)))))
(defmethod stream-line-column ((stream html-stream-out)) nil)
(defmethod stream-finish-output ((stream html-stream-out))
  (with-slots (target-stream) stream (finish-output target-stream)))
(defmethod stream-force-output ((stream html-stream-out))
  (with-slots (target-stream) stream (force-output target-stream)))
(defmethod stream-clear-output ((stream html-stream-out))
  (with-slots (target-stream) stream (clear-output target-stream)))

;;;
;;; HTML generation
;;;

(defmacro with-html-output ((var stream
                             &key (doctype ''(html public
                                              "-//W3C//DTD HTML 3.2//EN"))
                                  (meta '(:http-equiv "Content-Type"
                                          :content "text/html"))
                                  base comment (title "untitled") (footer t)
                                  head)
                            &body body)
  (with-gensyms ("HTML-" raw mailto)
    `(let* ((,raw ,stream) (,var (make-instance 'html-stream-out :stream ,raw))
            (,mailto (concatenate 'string "mailto:" *user-mail-address*)))
      (macrolet ((with-tag ((tag &rest options) &body forms)
                   `(progn (format ,',raw "<~a~@{ ~a=~s~}>" ,tag ,@options)
                     ,@forms (format ,',raw "</~a>~%" ,tag)))
                 (with-tagl ((tag &rest options) &body forms)
                   `(progn (format ,',raw "<~a~@{ ~a=~s~}>" ,tag ,@options)
                     ,@forms (format ,',raw "</~a>" ,tag))))
        (unwind-protect
             (progn
               (format ,raw "<!doctype~{ ~s~}>~%" ,doctype)
               ;; print the comment
               (format ,raw "<!--~% Created on ") (current-time ,raw)
               (format ,raw "~% by ~a@~a~% using `with-open-html'
 Lisp: ~a ~a~@[~%~a~]~% -->~2%"
                       (getenv "USER") (machine-instance)
                       (lisp-implementation-type) (lisp-implementation-version)
                       ,comment)
               (when ,base
                 (with-tag (:base :href ,base)))
               (with-tag (:html)
                 (with-tag (:head ,@head)
                   (with-tag (:meta ,@meta))
                   (with-tag (:link :rev 'made :href ,mailto))
                   (with-tag (:title) (princ ,title ,var)))
                 (with-tag (:body)
                   ,@body
                   (when ,footer
                     (with-tag (:p)
                       (with-tag (:hr))
                       (with-tag (:address)
                         (with-tag (:a :href ,mailto)
                           (princ *user-mail-address* ,var)))
                       (with-tagl (:strong) (current-time ,var)))))))
          (when ,raw (close ,raw)))))))

;;;
;;; this is an example on how to use `with-open-html' and `with-tag'.
;;;

(defun directory-index (dir file &rest opts
                        &key (title (format nil "Index of ~a" dir)))
  "Output the index for a directory."
  ;; (directory-index "/etc/*" "/tmp/z.html")
  (with-html-output (out (open file :direction :output)
                     :title title :comment
                     (format nil " Called: (directory-index ~s ~s~{ ~s~})"
                             dir file opts))
    (with-tag (:h1) (format out "Index of ~a" dir))
    (with-tag (:table :border "1")
      (dolist (fi (sort (directory dir #+cmu :follow-links #+cmu nil)
                        #'string< :key #'namestring))
        (with-tag (:tr)
          (with-tag (:th :align "left")
            (with-tag (:a :href (namestring fi)) (princ fi out)))
          (with-tag (:td :align "right")
            (format out "~:d" (ignore-errors (file-size fi))))
          (with-tag (:td :align "right")
            (princ (ignore-errors (dttm->string (file-write-date fi)))
                   out)))))))


(provide :htmlgen)
;;; file htmlgen.lisp ends here
