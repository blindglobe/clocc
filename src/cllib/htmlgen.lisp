;;; File: <htmlgen.lisp - 2000-03-03 Fri 12:14:02 EST sds@ksp.com>
;;;
;;; HTML generation
;;;
;;; Copyright (C) 2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: htmlgen.lisp,v 1.1 2000/03/03 17:25:19 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/htmlgen.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base")))

(in-package :cllib)

(eval-when (load compile eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

;;;
;;; HTML generation
;;;

(defcustom *html-output* stream *standard-output*
  "The stream where the HTML is printed by `with-tag' and `with-open-html'.
It is bound only by `with-open-html'.")
(makunbound '*html-output*)

(defmacro with-tag ((tag &rest options &key (close t) value
                         (terpri (and close (not value))) &allow-other-keys)
                    &body forms)
  (remf options :close) (remf options :terpri) (remf options :value)
  `(progn
    (when ,terpri (fresh-line *html-output*))
    (format *html-output* "<~a~@{ ~a=~s~}>" ,tag ,@options)
    (when ,value (princ ,value *html-output*))
    (when ,terpri (terpri *html-output*))
    ,@forms
    (when ,close (format *html-output* "~@[~&~*~]</~a>" ,terpri ,tag))))

(defmacro with-open-html (((&rest open-pars)
                           (&key (doctype ''(html public
                                             "-//W3C//DTD HTML 3.2//EN"))
                                 (meta '(:http-equiv "Content-Type"
                                         :content "text/html"))
                                 base comment (title "untitled") (footer t)
                                 head))
                          &body body)
  "Output HTML to a file."
  (with-gensyms ("WOH-MAILTO-" mailto)
    `(let ((,mailto (concatenate 'string "mailto:" *user-mail-address*)))
      (with-open-file (*html-output* ,@open-pars)
        (format *html-output* "<!doctype~{ ~s~}>~%" ,doctype)
        ;; print the comment
        (format *html-output* "<!--~% File: <~a - " ,(car open-pars))
        (current-time *html-output*)
        (format *html-output*
         " ~a@~a>~% Created by `with-open-html'~% Lisp: ~a ~a~@[~%~a~]~% -->~%"
         (getenv "USER") (machine-instance) (lisp-implementation-type)
         (lisp-implementation-version) ,comment)
        (terpri *html-output*)
        (when ,base
          (with-tag (:base :close nil :href ,base))
          (terpri *html-output*))
        (with-tag (:html)
          (terpri *html-output*)
          (with-tag (:head ,@head)
            (with-tag (:meta :close nil ,@meta))
            (terpri *html-output*)
            (with-tag (:link :close nil :rev 'made :href ,mailto))
            (terpri *html-output*)
            (with-tag (:title :value ,title)))
          (terpri *html-output*) (terpri *html-output*)
          (with-tag (:body)
            ,@body
            (when ,footer
              (terpri *html-output*) (terpri *html-output*)
              (with-tag (:p :terpri nil)
                (with-tag (:hr :close nil))
                (with-tag (:address :terpri nil)
                  (with-tag (:a :href ,mailto :value *user-mail-address*)))
                (terpri *html-output*) (with-tag (:br :close nil))
                (with-tag (:strong :value (current-time nil)))))))
        (terpri *html-output*)))))

;;; this is an example on how to use `with-open-html' and `with-tag'.
(defun directory-index (dir file &rest opts
                        &key (title (format nil "Index of ~a" dir)))
  "Output the index for a directory."
  ;; (directory-index "/etc/*" "/tmp/z.html")
  (with-open-html ((file :direction :output)
                   (:title title :comment
                    (format nil " Called: (directory-index ~s ~s~{ ~s~})"
                            dir file opts)))
    (with-tag (:h1 :terpri nil) (format *html-output* "Index of ~a" dir))
    (terpri *html-output*) (terpri *html-output*)
    (with-tag (:table :border 1)
      (dolist (fi (sort (directory dir #+cmu :follow-links #+cmu nil)
                        #'string< :key #'namestring))
        (with-tag (:tr)
          (with-tag (:th :align "left" :terpri nil)
            (with-tag (:a :href (namestring fi) :value fi)))
          (terpri *html-output*)
          (with-tag (:td :align "right" :terpri nil)
            (format *html-output* "~:d" (ignore-errors (file-size fi))))
          (terpri *html-output*)
          (with-tag (:td :align "right" :value
                         (ignore-errors (dttm->string (file-write-date
                                                       fi))))))))))


(provide :htmlgen)
;;; file htmlgen.lisp ends here
