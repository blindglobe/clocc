;;; HTML generation
;;;
;;; Copyright (C) 2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: htmlgen.lisp,v 1.11 2001/04/20 19:28:30 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/htmlgen.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `dttm->string' - needed only for `directory-index'
  ;; (require :date (translate-logical-pathname "cllib:date"))
  ;; "Gray streams"
  (require :gray (translate-logical-pathname "port:gray")))

(in-package :cllib)

(export '(html-stream-out with-html-output with-http-output with-tag
          flush-http directory-index))

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
(defmethod close ((stream html-stream-out) &rest opts)
  (with-slots (target-stream) stream (apply #'close target-stream opts)))

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
  "Create an `html-stream-out' stream out of STREAM, bind it to VAR.
Two local macros are defined inside this one - `with-tag' and `with-tagl'.
Both print a tag but the second one does not do a `terpri' afterwards."
  (with-gensyms ("HTML-" raw mailto)
    `(let ((,raw ,stream)
           (,mailto (concatenate 'string "mailto:" *user-mail-address*)))
      (macrolet ((with-tag ((tag &rest options) &body forms)
                   `(progn (format ,',raw "<~a~@{ ~a=~s~}>" ,tag ,@options)
                     ,@forms (format ,',raw "</~a>~%" ,tag)))
                 (with-tagl ((tag &rest options) &body forms)
                   `(progn (format ,',raw "<~a~@{ ~a=~s~}>" ,tag ,@options)
                     ,@forms (format ,',raw "</~a>" ,tag))))
        (with-open-stream (,var (make-instance 'html-stream-out :stream ,raw))
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
              ,(when footer
                     `(when ,footer
                       (with-tag (:p)
                         (with-tag (:hr))
                         (with-tag (:address)
                           (with-tag (:a :href ,mailto)
                             (princ *user-mail-address* ,var)))
                         (with-tagl (:strong) (current-time ,var))))))))))))

(defun crlf (sock)
  "Write CR/LF into the socket SOCK."
  (write-char (code-char 13) sock)
  (write-char (code-char 10) sock))

(defmacro with-http-output ((var raw &rest opts &key keep-alive (debug 0)
                             &allow-other-keys)
                            &body body)
  "Write some HTML to an http client on socket stream RAW.
Supplies some HTTP/1.0 headers and calls `with-html-output'."
  (with-gensyms ("HTTP-" string stream sock header line)
    (remf opts :keep-alive) (remf opts :debug)
    `(let* ((,sock ,raw)
            (,string (with-output-to-string (,stream)
                       (with-html-output (,var ,stream ,@opts) ,@body)))
            (,header (list "HTTP/1.0 200 OK" "Content-type: text/html"
                           (format nil "Content-length: ~d" (length ,string))
                           (format nil "Connection: ~:[Close~;Keep-Alive~]"
                                   ,keep-alive))))
      (dolist (,line ,header)
        (write-string ,line ,sock)
        (when (and ,debug (> ,debug 0))
          (format t "<- ~a~%" ,line))
        (crlf ,sock))
      (crlf ,sock)
      (write-string ,string ,sock)
      (when (and ,debug (> ,debug 3))
        (format t "<- ~s~%" ,string))
      (unless ,keep-alive
        (when (and ,debug (> ,debug 0))
          (format t "~s: closing ~s~%" 'with-http-output ,sock))
        (close ,sock)))))

(defun flush-http (sock)
  "Read everything from the HTTP socket SOCK, until a blank line."
  (loop :for line = (read-line sock nil nil)
        :while (and line (plusp (length line)))
        :collect line))

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
            (princ (ignore-errors (dttm->string (file-write-date fi) :short))
                   out)))))))


(provide :htmlgen)
;;; file htmlgen.lisp ends here
