;;; File: <html.lisp - 2000-02-18 Fri 12:59:37 EST sds@ksp.com>
;;;
;;; HTML parsing - very rudimentary
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold
;;;
;;; $Id: html.lisp,v 1.1 2000/02/18 20:24:11 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/html.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `with-open-url' in `dump-url-tokens'
  (require :url (translate-logical-pathname "cllib:url")))

(in-package :cllib)

(eval-when (load compile eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

(export
 '(*html-parse-tags* *html-readtable* html-translate-specials
   text-stream *ts-kill* read-next next-token next-number dump-url-tokens
   *html-output* with-tag with-open-html directory-index))

;;;
;;; {{{ HTML parsing via `read'
;;;

;; (setq *read-eval* nil *read-suppress* t) ; for parsing
;; (setq *read-eval* t *read-suppress* nil) ; original

(defcustom *html-parse-tags* (member t nil) nil
  "*If non-nil, parse tags, if nil - return nil for all tags.")
(defstruct html-tag data)

;; ftp://ftp.unicode.org/Public/MAPPINGS/VENDORS/MISC/SGML.TXT

(defcustom *html-specials* list
  '(("gt" . #\>) ("lt" . #\<) ("quot" . #\") ("amp" . #\&) ("nbsp" . #\Space)
    ("acute" . #\') ("ast" . #\*) ("colon" . #\:) ("comma" . #\,)
    ("commat" . #\@) ("copy" . "(C)") ("curren" . #\$) ("divide" . #\/)
    ("dollar" . #\$) ("equals" . #\=) ("excl" . #\!) ("grave" . #\`)
    ("half" . "1/2") ("hyphen" . #\-) ("lowbar" . #\_) ("lpar" . #\()
    ("rpar" . #\)) ("lsqb" . #\[) ("rsqb" . #\]) ("num" . #\#) ("period" . #\.)
    ("plus" . #\+) ("plusmn" . "+-") ("pound" . #\#) ("quest" . #\?)
    ("laquo" . "<<") ("raquo" . ">>") ("lcub" . #\{) ("rcub" . #\})
    ("semi" . #\;) ("shy" . #\-) ("times" . #\*) ("verbar" . #\|))
  "Alist of translations of HTML specials like `&*'.")

(defun html-translate-specials (str &optional space)
  "Replace (non-destructively) HTML specals with their interpretations.
HTML tags, surrounded by `<>', are removed or replaced with a space, if
optional argument SPACE is non-nil."
  (declare (string str))
  (do ((beg 0 (1+ beg)) res (len (length str)))
      ((>= beg len) (coerce (nreverse res) 'string))
    (declare (type index-t beg len))
    (case (char str beg)
      (#\< (setq beg (or (position #\> str :start beg) len))
           (when space (push #\Space res)))
      (#\&
       (let ((pa (assoc str *html-specials* :test
                        (lambda (str tag)
                          (let ((end (+ beg (length tag))))
                            (and (>= len end)
                                 (string= str tag :start1 beg
                                          :end1 end)))))))
         (cond (pa (incf beg (1- (length (car pa))))
                   (push (cdr pa) res))
               (t (when space (push #\Space res))
                  (setq beg (or (position #\; str :start beg) len))))))
      (t (push (char str beg) res)))))

(defun strip-html-markup (str)
  "Return a new string, sans HTML."
  (declare (simple-string str))
  (do* ((p0 (position #\< str) (position #\< str :start p1))
        (res (list (subseq str 0 p0)))
        (p1 (position #\> str) (position #\> str :start (or p0 0))))
       ((or (null p0) (null p1))
        (apply #'concatenate 'string (nreverse res)))
    (push (subseq str (1+ p1) (position #\< str :start p1)) res)))

(defun read-html-markup (stream char)
  "Skip through the HTML markup. CHAR=`<'"
  (declare (stream stream) (character char))
  (ecase char
    (#\; #\;) (#\, #\,) (#\: #\:)
    (#\< (make-html-tag
          :data (if *html-parse-tags* (read-delimited-list #\> stream t)
                    (do () ((char= (read-char stream t nil t) #\>))))))
    (#\&
     (do ((cc (read-char stream nil nil t) (read-char stream nil nil t)) rr)
         ((or (null cc) (char= cc #\;) (char= cc #\#))
          (if (null cc) (error "`&' must be terminated with `;' or `#'")
              (or (and *html-parse-tags*
                       (cdr (assoc (coerce (nreverse rr) 'string)
                                   *html-specials* :test #'string-equal)))
                  #\Space)))
       (when *html-parse-tags* (push cc rr))))))

(defun make-html-readtable ()
  "Make the readtable for parsing HTML."
  (let ((rt (copy-readtable)))
    (set-macro-character #\< #'read-html-markup nil rt)
    (set-macro-character #\& #'read-html-markup nil rt)
    (set-macro-character #\> (get-macro-character #\)) nil rt)
    (set-syntax-from-char #\; #\a rt)
    ;;(set-macro-character #\; #'read-html-markup nil rt)
    (set-syntax-from-char #\# #\a rt)
    (set-syntax-from-char #\: #\a rt)
    (set-macro-character #\: #'read-html-markup nil rt)
    (set-syntax-from-char #\, #\a rt)
    (set-macro-character #\, #'read-html-markup nil rt)
    rt))

(defcustom *html-readtable* readtable (make-html-readtable)
  "The readtable for HTML parsing.")

;;;
;;; }}}{{{ HTML streams
;;;

#+(or clisp acl cmu) (progn

(defclass html-stream (#+acl excl:fundamental-character-input-stream
                       #+clisp lisp:fundamental-character-input-stream
                       #+cmu ext:fundamental-character-input-stream)
  ((input :initarg :stream :initarg :input :type stream :reader html-in))
  (:documentation "The input stream for reading HTML."))

(defcustom *html-unterminated-tags* list '(:p :li :dd :dt :tr :td :th)
   "*The list of tags without the corresponding `/' tag.")

(defun html-end-tag (tag)
  (if (member tag *html-unterminated-tags* :test #'eq) tag
      (keyword-concat "/" tag)))

(defmethod stream-read-char ((in html-stream)) (read-char (html-in in)))
(defmethod stream-unread-char ((in html-stream)) (unread-char (html-in in)))
(defmethod stream-read-char-no-hang ((in html-stream))
  (read-char-no-hang (html-in in)))
;; (defmethod stream-peak-char ((in html-stream)) (peak-char (html-in in)))
(defmethod stream-listen ((in html-stream)) (listen (html-in in)))
(defmethod stream-read-line ((in html-stream)) (read-line (html-in in)))
(defmethod stream-clear-output ((in html-stream)) (clear-input (html-in in)))

)

;;;
;;; }}}{{{ HTML parsing via `text-stream'
;;;

(defstruct (text-stream (:conc-name ts-))
  "Text stream - to read a tream of text - skipping junk."
  (sock nil)                    ; socket to read from
  (buff "" :type simple-string) ; buffer string
  (posn 0 :type fixnum))        ; position in the buffer

(defcustom *ts-kill* list nil "*The list of extra characters to kill.")

(defun ts-pull-next (ts &optional (concat-p t) (kill *ts-kill*))
  "Read the next line from the socket, put it into the buffer.
If CONCAT-P is non-NIL, the new line is appended,
otherwise the buffer is replaced.
Return the new buffer or NIL on EOF."
  (declare (type text-stream ts))
  (let ((str (or (read-line (ts-sock ts) nil nil)
                 (return-from ts-pull-next nil))))
    (declare (type simple-string str))
    (when kill
      (dolist (ch (to-list kill))
        (setq str (nsubstitute #\Space ch str))))
    ;; ' .. ' is an error and
    ;; (nsubstitute #\space #\. str) breaks floats, so we have to be smart
    (do ((beg -1) (len (1- (length str))))
        ((or (>= beg len)
             (null (setq beg (position #\. str :start (1+ beg))))))
      (declare (type (signed-byte 21) beg len))
      (if (or (and (plusp beg) (alphanumericp (schar str (1- beg))))
              (and (< beg len) (alphanumericp (schar str (1+ beg)))))
          (incf beg) (setf (schar str beg) #\Space)))
    (if concat-p
        (setf (ts-buff ts) (concatenate 'string (ts-buff ts) str))
        (setf (ts-posn ts) 0 (ts-buff ts) str))))

(defun read-next (ts &key errorp (kill *ts-kill*) skip)
  "Read the next something from TS - a text stream."
  (declare (type text-stream ts) (type (or null function) skip))
  (loop :with *package* = +kwd+ :and tok :and pos
        :when (and (or (typep pos 'error)
                       (>= (ts-posn ts) (length (ts-buff ts))))
                   (null (ts-pull-next ts (typep pos 'error) kill)))
        :do (if (typep pos 'error) (error pos)
                (if errorp (error "EOF on ~a" ts)
                    (return-from read-next +eof+)))
        :do (setf (values tok pos)
                  (ignore-errors (read-from-string (ts-buff ts) nil +eof+
                                                   :start (ts-posn ts))))
        :unless (typep pos 'error) :do (setf (ts-posn ts) pos)
        :unless (or (typep pos 'error) (eq +eof+ tok))
        :return (if (and skip (funcall skip tok))
                    (read-next ts :errorp errorp :kill kill :skip skip)
                    tok)))

(defun ts-skip-scripts (ts)
  "Read from the text stream one script."
  (declare (type text-stream ts))
  (let ((*html-parse-tags* t) pos)
    (do ((tok (read-next ts) (read-next ts)))
        ((and (html-tag-p tok) (eq :script (car (html-tag-data tok))))))
    (do () ((setq pos (search "</script>" (ts-buff ts) :test #'char-equal)))
      (ts-pull-next ts))
    (setf (ts-buff ts) (subseq (ts-buff ts) (+ pos (length "</script>"))))))

(defun next-token (ts &key (num 1) type dflt (kill *ts-kill*))
  "Get the next NUM-th non-tag token from the HTML stream TS."
  (declare (type text-stream ts) (type index-t num))
  (let (tt)
    (dotimes (ii num)
      (declare (type index-t ii))
      (do () ((not (html-tag-p (setq tt (read-next ts :errorp t :kill kill))))
              (mesg :log t "~d token (~s): ~s~%" ii (type-of tt) tt))
        (mesg :log t "tag: ~s~%" tt)))
    (if (and type (not (typep tt type))) dflt tt)))

(defun next-number (ts &key (num 1) (kill *ts-kill*))
  "Get the next NUM-th number from the HTML stream TS."
  (declare (type text-stream ts) (type index-t num))
  (pushnew #\% kill :test #'char=)
  (let (tt)
    (dotimes (ii num)
      (declare (type index-t ii))
      (do () ((numberp (setq tt (next-token ts :kill kill)))))
      (mesg :log t "~d - number: ~a~%" ii tt))
    (mesg :log t " -><- number: ~a~%" tt)
    tt))

(defun skip-tokens (ts end &key (test #'eql) (key #'identity) kill)
  "Skip tokens until END, i.e., until (test (key token) end) is T."
  (declare (type text-stream ts))
  (do (tt) ((funcall test (setq tt (funcall key (next-token ts :kill kill)))
                     end)
            tt)))

;;;###autoload
(defun dump-url-tokens (url &key (fmt "~3d: ~a~%")
                        (out *standard-output*) (err *error-output*)
                        (max-retry *url-default-max-retry*))
  "Dump the URL token by token.
See `dump-url' about the optional parameters.
This is mostly a debugging function, to be called interactively."
  (declare (stream out) (simple-string fmt))
  (setq url (url url))
  (with-open-url (sock url :rt *html-readtable* :err err :max-retry max-retry)
    (do (rr (ii 0 (1+ ii)) (ts (make-text-stream :sock sock)))
        ((eq +eof+ (setq rr (read-next ts))))
      (declare (type index-t ii))
      (format out fmt ii rr))))

;;;
;;; }}}{{{ HTML generation
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

;;;}}}

(provide :html)
;;; file html.lisp ends here
