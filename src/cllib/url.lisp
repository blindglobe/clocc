;;; File: <url.lisp - 1998-11-19 Thu 15:17:13 EST sds@eho.eaglets.com>
;;;
;;; Url.lisp - handle url's and parse HTTP
;;;
;;; Copyright (C) 1998 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: url.lisp,v 1.6 1998/11/19 20:19:37 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/url.lisp,v $
;;; $Log: url.lisp,v $
;;; Revision 1.6  1998/11/19 20:19:37  sds
;;; Added ftp handling: `ftp-ask', `ftp-parse-sextuple', `url-open-ftp',
;;; `ftp-get-passive-socket', `ftp-get-file', `*buf-size*', `ftp-list'.
;;; Separated `open-url' from `open-socket' and made sure that the former
;;; does indeed opens a socket.
;;;
;;; Revision 1.5  1998/10/30 20:55:40  sds
;;; Replaced `parse-url' with a generic function.
;;; Added `*html-specials*', `html-translate-specials',
;;; `*hyperspec-root*' and `hyperspec-snarf-examples'.
;;;
;;; Revision 1.4  1998/07/31 16:53:21  sds
;;; Declared `stream' as a stream in `print-*'.
;;;
;;; Revision 1.3  1998/06/30 13:48:08  sds
;;; Switched to `print-object'.
;;;
;;; Revision 1.2  1998/05/26 20:19:35  sds
;;; Adopted to work with ACL 5beta.
;;;
;;; Revision 1.1  1998/03/10 18:31:44  sds
;;; Initial revision
;;;

(in-package :cl-user)

(eval-when (load compile eval)
  (sds-require "base") (sds-require "print") (sds-require "util")
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

;;;
;;; {{{ HTML parsing
;;;

;(setq *read-eval* nil *read-suppress* t) ; for parsing
;(setq *read-eval* t *read-suppress* nil) ; original

(defcustom *html-readtable* readtable (copy-readtable nil)
  "The readtable for HTML parsing.")
(defcustom *html-parse-tags* (member t nil) nil
  "*If non-nil, parse tags, if nil - return nil for all tags.")
(defcustom *html-verbose* (member t nil) nil "*Be verbose while parsing.")
(defconst +html-tag+ cons (list :*html-tag*)
  "*The car of any html tag that is read.")

(defun strip-html-markup (str)
  "Return a new string, sans HTML."
  (declare (string str))
  (apply #'concatenate 'string
	 (do* ((p0 (position #\< str) (position #\< str :start p1))
	       (res (list (subseq str 0 p0)))
	       (p1 (position #\> str) (position #\> str :start (or p0 0))))
	      ((or (null p0) (null p1)) (nreverse res))
	   (push (subseq str (1+ p1) (position #\< str :start p1)) res))))

(defun read-html-markup (stream char)
  "Skip through the HTML markup. CHAR=`<'"
  (declare (ignore char))
  (if *html-parse-tags*
      (cons +html-tag+ (read-delimited-list #\> stream t))
      (do () ((char= (read-char stream t nil t) #\>)))))

(defun html-tag-p (obj)
  "Return T if the object is an HTML tag."
  (if *html-parse-tags* (and (consp obj) (eq (car obj) +html-tag+))
      (null obj)))

(set-macro-character #\< #'read-html-markup nil *html-readtable*)
(set-macro-character #\> (get-macro-character #\)) nil *html-readtable*)
(set-syntax-from-char #\# #\a *html-readtable*)
(set-syntax-from-char #\: #\a *html-readtable*)
(set-syntax-from-char #\; #\a *html-readtable*)
;(set-macro-character #\: (get-macro-character #\)) nil *html-readtable*)
;(set-macro-character #\, (get-macro-character #\a) nil *html-readtable*)

;;;
;;; }}}{{{ URL handling
;;;

(defstruct (url)
  "URL - Uniform Resource Locator: protocol://user#password@host:port/path."
  (prot "" :type simple-string)		; protocol
  (user "" :type simple-string)		; username
  (pass "" :type simple-string)		; password
  (host "" :type simple-string)		; hostname
  (port 0  :type fixnum)		; port number
  (path "/" :type simple-string)) ; pathname

(eval-when (load compile eval)
(unless (fboundp 'socket-service-port)
(defun socket-service-port (prot)
  "Return the port number of the protocol."
  (declare (simple-string prot))
  (with-open-file (fl #+unix "/etc/services" #+win32
                      (concatenate 'string (system:getenv "windir")
                                   "/system32/etc/services")
                      :direction :input)
    (do ((st (read-line fl nil +eof+) (read-line fl nil +eof+))
         res pos srv)
        ((or res (eq st +eof+)) res)
      (unless (or (zerop (length st)) (char= #\# (char st 0)))
        (setf (values srv pos) (read-from-string st nil ""))
        (when (string-equal prot srv)
          (nsubstitute #\space #\/ st)
          (setq res (read-from-string st nil nil :start pos)))))))))

(defun url-get-port (url)
  "Get the correct port of the URL - if the port is not recorded there,
guess from the protocol."
  (setq url (url url))
  (if (zerop (url-port url)) (socket-service-port (url-prot url))
      (url-port url)))

(defun url-path-dir (url)
  "Return the dir part of the url's path, dropping the file name."
  (setq url (url url))
  (subseq (url-path url) 0 (1+ (position #\/ (url-path url) :from-end t))))

(defun url-path-file (url)
  "Return the file part of the url's path, dropping the dir."
  (setq url (url url))
  (subseq (url-path url) (1+ (position #\/ (url-path url) :from-end t))))

(defmethod print-object ((url url) (stream stream))
  "Print the URL in the standard form."
  (when *print-readably* (return-from print-object (call-next-method)))
  (when *print-escape* (write-string "\"" stream))
  (let ((*print-escape* nil))
    (write (url-prot url) :stream stream) (write-string "://" stream)
    (unless (string= "" (url-user url))
      (write (url-user url) :stream stream)
      (unless (string= "" (url-pass url))
        (write-string "#" stream) (write (url-pass url) :stream stream))
      (write-string "@" stream))
    (write (url-host url) :stream stream)
    (unless (zerop (url-port url))
      (write-string ":" stream) (write (url-port url) :stream stream))
    (unless (or (zerop (length (url-path url)))
                (eq #\/ (aref (url-path url) 0)))
      (write-string "/" stream))
    (write (url-path url) :stream stream))
  (when *print-escape* (write-string "\"" stream)))

(defcustom *url-special-chars* simple-string "#%&*+,-./:=?@_~"
  "*The string consisting of non-alphanumeric characters allowed in a URL.")

(defun url-constituent (char)
  "Check whether the character can be part of a URL."
  (declare (character char))
  (and (characterp char)
       (or (alphanumericp char)
	   (find char *url-special-chars* :test #'char=))))

(fmakunbound 'url)
(defgeneric url (xx) (:documentation "Convert the object into URL.
The argument can be:
   - a URL - returned untouched;
   - a string - it is non-destructively parsed;
   - a symbol - it is uninterned and its name is non-destructively parsed;
   - a stream - read from."))
(declaim (ftype (function (t) url) url))
(defmethod url ((xx url)) xx)
(defmethod url ((xx string))
  (let* ((string (coerce (string-trim +whitespace+ xx) 'simple-string))
         (idx (search "://" string :test #'char=)) (start 0)
         idx0 (url (make-url)))
    (declare (simple-string string) (fixnum start))
    (when idx
      (setf (url-prot url) (subseq string 0 idx))
      (setq start (+ idx 3)))
    (setq idx (position #\@ string :start start :test #'char=))
    (when idx
      (setq idx0 (position #\# string :start start :test #'char=))
      (if idx0 (setf (url-pass url) (subseq string (1+ idx0) idx)
		     (url-user url) (subseq string start idx0))
	  (setf (url-user url) (subseq string start idx)))
      (setq start (1+ idx)))
    (setq idx (position #\: string :start start :test #'char=))
    (setq idx0 (position #\/ string :start start :test #'char=))
    (when idx
      (setf (url-port url) (parse-integer string :start (1+ idx) :end idx0)))
    (when idx0
      (setf (url-path url) (subseq string idx0)))
    (setf (url-host url) (subseq string start (or idx idx0)))
    (when (equal "" (url-prot url))
      (cond ((not (mismatch "www" (url-host url) :test #'char= :end2 3))
	     (setf (url-prot url) "http"))
	    ((not (mismatch "ftp" (url-host url) :test #'char= :end2 3))
	     (setf (url-prot url) "ftp"))))
    url))
(defmethod url ((xx symbol)) (unintern xx) (url (string xx)))
(defmethod url ((xx stream))
  (url (with-output-to-string (st)
         (do (zz) ((not (url-constituent (setq zz (read-char xx)))) st)
           (write zz :stream st)))))

(defstruct (text-stream (:conc-name ts-))
  "Text stream - to read a tream of text - skipping `:'."
  (sock nil)			; socket to read from
  (buff "" :type simple-string)	; buffer string
  (posn 0 :type fixnum))	; position in the buffer

(defun open-socket (host port)
  "Open a socket connection to HOST at PORT."
  #+cmu (system:make-fd-stream (ext:connect-to-inet-socket host port)
                               :input t :output t)
  #+clisp (lisp:socket-connect port host)
  #+allegro (socket:make-socket :remote-host host :remote-port port))

#+allegro (deftype socket () 'excl::socket-stream)

(defun open-url (url &key (err t) (timeout 10))
  "Open a socket connection to the URL."
  (declare (type url url) (real timeout))
  (loop :for sock =
        (multiple-value-bind (sk cond)
            (ignore-errors (open-socket (url-host url) (url-get-port url)))
          (unless sk (format err "error connecting: ~a~%" cond))
          sk)
        :when (and sock (open-stream-p sock)
                   (cond ((equal "http" (url-prot url))
                          (setq sock (url-open-http sock url err)))
                         ((equal "ftp" (url-prot url))
                          (ftp-ask sock err 220)
                          (or (url-open-ftp sock url err)
                              (ftp-ask sock err 421)))
                         ((equal "telnet" (url-prot url))
                          (dolist (word (split-string (url-path url) "/") t)
                            (format sock "~a~%" word)))
                         ((equal "whois" (url-prot url))
                          (format sock "~a~%" (url-path-file url)) t)
                         (t)))
        :return sock :do
        (format err "sleeping for ~d seconds...~%" timeout) (sleep timeout)
        (format err "trying to connect to `~a'...~%" url)))

(defmacro with-open-url ((socket url &key (rt '*readtable*) err)
                         &body body)
  "Execute BODY, binding SOCK to the socket corresponding to the URL.
The *readtable* is temporarily set to RT (defaults to *html-readtable*).
If this is an HTTP URL, also issue the GET command.
If this is an FTP URL, cd and get (if there is a file part).
ERR is the stream for information messages."
  (let ((rt-old (gensym "WOU")) (uuu (gensym "WOU")))
    `(let* ((,uuu ,url) (,socket (open-url ,uuu :err ,err))
	    (,rt-old *readtable*))
      (declare (type url ,uuu) (type socket ,socket))
      (unwind-protect (progn (setq *readtable* (or ,rt *readtable*)) ,@body)
	(setq *readtable* ,rt-old)
        (when (equal "ftp" (url-prot ,uuu))
          (ftp-ask ,socket ,err 221 "quit"))
	(close ,socket)))))

(defun url-open-http (sock url err)
  "Open the socket to the HTTP url."
  (declare (type socket sock) (type url url))
  (format sock "GET ~a HTTP/1.0~%~%" (url-path url))
  (do ((sk sock) (stat 302) sym res) ((not (eql stat 302)) sk)
    (declare (fixnum stat))
    (setq sym (read sk) stat (read sk))
    (when (string-equal sym "http/1.1")
      (when (>= stat 400) (error "~d: ~a~%" res (read-line sk))) ; error
      (when (= stat 302) ; redirection
	(setq res (read-line sk)) (read-line sk) (read-line sk)
	(setq sym (read-line sk)
	      sym (url (subseq sym (1+ (position #\: sym)))))
	(when (equal "" (url-host sym)) (setf (url-host sym) (url-host url)))
	(format err " *** redirected to `~a' [~a]~%" sym res)
	(when *html-verbose*
	  (do (st) ((eq +eof+ (setq st (read-line sk nil +eof+))))
	    (format t "~a~%" st)))
	(close sk) (setq sk (open-url sym :err err))
	(format sk "GET ~a HTTP/1.0~%~%" (url-path sym))))))

(defun ftp-ask (sock out end &rest req)
  "Send an ftp request."
  (declare (type socket sock))
  (when req
    (apply #'format sock req) (fresh-line sock)
    (when out (apply #'format out "ftp-ask[~d]: `~@?'~%" end req)))
  (loop :for ln = (read-line sock nil nil) :unless ln :return nil
        :when out :do (format out "~a~%"
                              (setq ln (string-right-trim +whitespace+ ln)))
        :while (or (< (length ln) 3) (char/= #\Space (schar ln 3))
                   (not (eql end (read-from-string ln))))
        :finally (return ln)))

(defun ftp-parse-sextuple (line)
  "Convert a0,a1,a2,a3,b0,b1 to HOST and PORT."
  (declare (simple-string line))
  (let* ((p1 (position #\, line :from-end t))
         (p2 (position #\, line :from-end t :end (1- p1))))
    (declare (type (unsigned-byte 20) p1 p2))
    (setf (schar line p1) #\Space (schar line p2) #\Space)
    (nsubstitute #\. #\, line)
    (values (subseq line (1+ (position #\( line :from-end t)) p2)
            (+ (ash (read-from-string line nil nil :start p2) 8)
               (read-from-string line nil nil :start p1)))))

(defun ftp-get-passive-socket (sock out)
  "Get a passive socket."
  (declare (type socket sock))
  (multiple-value-call #'open-socket
    (ftp-parse-sextuple (ftp-ask sock out 227 "pasv"))))

(defun url-open-ftp (sock url err)
  "Open the socket to the HTTP url."
  (declare (type socket sock) (type url url))
  (and (ftp-ask sock err 331 "user ~a" (if (zerop (length (url-user url)))
                                           "anonymous" (url-user url)))
       (ftp-ask sock err 230 "pass ~a" (if (zerop (length (url-pass url)))
                                           "ftp@ftp.net" (url-pass url)))
       (ftp-ask sock err 250 "cwd ~a" (url-path-dir url))))

(defcustom *buf-size* (unsigned-byte 20) 8192
  "The buffer size for download.")

(defun ftp-get-file (sock rmt loc &key log)
  "Get the remote file."
  (let* ((data (ftp-get-passive-socket sock t)) (tot 0)
         (buf (make-array *buf-size* :element-type 'unsigned-byte))
         (bt (get-float-time-real))
         (line (ftp-ask sock log 150 "retr ~a" rmt))
         (len (read-from-string line nil nil :start
                                (1+ (position #\( line :from-end t)))))
    (declare (type socket data) (integer tot len) (double-float bt))
    (when log
      (format log "Expect ~d dots for ~:d bytes~%"
              (ceiling len *buf-size*) len))
    (with-open-file (fl (merge-pathnames rmt loc)
                        :direction :output :if-exists :supersede
                        :element-type 'unsigned-byte)
      (loop :for pos = (#+clisp lisp:read-byte-sequence #-clisp read-sequence
                                buf data)
            :do (write-sequence buf fl :end pos) (incf tot pos)
            :when log :do (princ "." log) (force-output log)
            :while (= pos (length buf))))
    (assert (= tot len) (tot len)
            "Wrong file length: ~:d (expected: ~:d)" tot len)
    (values-list (cons tot (multiple-value-list (elapsed bt nil t))))))

(defun ftp-list (sock)
  "Get the file list."
  (let ((data (ftp-get-passive-socket sock t)))
    (ftp-ask sock t 150 "list")
    (loop :for line = (read-line data nil nil) :while line :do
          (format t "~a~%" (string-right-trim +whitespace+ line)))
    (ftp-ask sock t 226)))

(defun read-next (ts &optional errorp)
  "Read the next something from TS - a text stream."
  (do (str tok pos) (nil)
    (when (or (typep pos 'error) (>= (ts-posn ts) (length (ts-buff ts))))
      (unless (typep pos 'error) (setf (ts-posn ts) 0))
      (setq str (read-line (ts-sock ts) nil +eof+))
      (when (eq str +eof+)
	(if (typep pos 'error) (error pos)
	    (if errorp (error "EOF on ~a" ts) (return-from read-next +eof+))))
      (setq str (nsubstitute #\space #\: str)
	    str (nsubstitute #\space #\, str)
	    str (nsubstitute #\space #\/ str))
      ;; (nsubstitute #\space #\. str) breaks floats, so we have to be smart
      (do ((beg -1) (len (1- (length str))))
	  ((or (= beg len)
	       (null (setq beg (position #\. str :start (1+ beg))))))
        (declare (type (signed-byte 20) beg len))
	(if (or (digit-char-p (schar str (1- beg)))
		(and (< beg len) (digit-char-p (schar str (1+ beg)))))
	    (incf beg) (setf (schar str beg) #\Space)))
      (setf (ts-buff ts) (if (typep pos 'error)
			     (concatenate 'string (ts-buff ts) str) str)))
    (setf (values tok pos)
	  (ignore-errors
	    (read-from-string (ts-buff ts) nil +eof+ :start (ts-posn ts))))
    (unless (typep pos 'error) (setf (ts-posn ts) pos))
    (unless (or (typep pos 'error) (eq tok +eof+))
      (return-from read-next tok))))

;(defun read-next (ts) (read (ts-sock ts) nil +eof+))

(defun next-token (ts &optional (num 1) type dflt)
  "Get the next NUM-th non-tag token from the HTML stream TS."
  (declare (type text-stream ts))
  (let (tt)
    (dotimes (ii num (if (and type (not (typep tt type))) dflt tt))
      (do () ((not (html-tag-p (setq tt (read-next ts t)))))))))

(defun next-number (ts &optional (num 1))
  "Get the next NUM-th number from the HTML stream TS."
  (declare (type text-stream ts))
  (let (tt)
    (dotimes (ii num tt) (do () ((numberp (setq tt (next-token ts))))))))

(defun skip-tokens (ts end &key (test #'eql) (key #'identity))
  "Skip tokens until END, i.e., until (test (key token) end) is T."
  (declare (type text-stream ts))
  (do (tt) ((funcall test (setq tt (funcall key (next-token ts))) end) tt)))

(defun skip-to-line (st ln &optional out)
  "Read from stream ST until a line starting with LN.
The optional third argument specifies where the message should go.
By default nothing is printed."
  (declare (stream st) (simple-string ln))
  (mesg :head out " +++ `skip-to-line' --> `~a'~%" ln)
  (do ((len (length ln)) (rr (read-line st) (read-line st)))
      ((and (>= (length rr) len) (string-equal ln rr :end2 len))
       (subseq rr (length ln)))
    (declare (fixnum len) (simple-string rr))))

(defun skip-search (stream string &optional out)
  "Read from STREAM until STRING is found by `search.'"
  (declare (stream stream) (simple-string string)
           (values (or null simple-string)))
  (mesg :head out " +++ `skip-search' --> `~a'~%" string)
  (do ((st (read-line stream nil nil) (read-line stream nil nil)))
      ((or (null st) (search string st :test #'char-equal)) st)
    (declare (type (or null simple-string) st))))

(defsubst read-trim (stream)
  "Read a line from stream and trim it."
  (declare (type stream stream) (values simple-string))
  (string-trim +whitespace+ (read-line stream)))

(defun skip-blanks (stream)
  "Read from STREAM first non-blank string is found."
  (declare (type stream stream) (values simple-string))
  (do ((st (read-trim stream) (read-trim stream)))
      ((/= 0 (length st)) st)
    (declare (simple-string st))))

(defun read-non-blanks (stream)
  "Read from STREAM through the first blank string."
  (declare (type stream stream) (values simple-string))
  (do* ((st (read-trim stream) (read-trim stream))
	(res st (concatenate 'string res " " st)))
       ((zerop (length st)) res)
    (declare (simple-string st res))))

(defcustom *browsers* list
  '((netscape "/usr/bin/netscape" "-remote" "openURL(~a)")
    (emacs-w3 "/usr/bin/gnudoit" "(w3-fetch \"~a\")"))
  "The ALIST of browsers.")

(defun view-url (url &optional (bro 'netscape))
  "Lounch a browser to view a url."
  (let ((br (copy-list (assoc bro *browsers* :test #'eq))) pos)
    (assert (consp br) (br) "Unknown browser ~a. Must be one of~?." bro
	    (list-format "~a") (mapcar #'car *browsers*))
    (setq pos (1+ (position "~a" (cdr br) :test #'search)))
    (setf (nth pos br) (format nil (nth pos br) url))
    (run-prog (second br) :args (cddr br))
    (format t "launched ~a with args ~a~%" (car br) (cddr br))))

(defun dump-url (url &key (fmt "~3d: ~a~%") (out t))
  "Dump the URL line by line.
FMT is the printing format. 2 args are given: line number and the line
itself. FMT defaults to \"~3d: ~a~%\".
OUT is the output stream and defaults to T."
  (setq url (url url))
  (with-open-url (sock url :err t)
    (do (rr (ii 0 (1+ ii)))
	((eq +eof+ (setq rr (read-line sock nil +eof+))))
      (declare (type (unsigned-byte 20) ii))
      (format out fmt ii rr))))

(defun whois (host)
  "Get the whois information on a host."
  (dump-url (make-url :prot "whois" :host "rs.internic.net"
		      :path (concatenate 'string "/" (string host)))
	    :fmt "~*~a~%"))

(defun dump-url-tokens (url &key (fmt "~3d: ~a~%") (out t))
  "Dump the URL token by token.
See `dump-url' about the optional parameters."
  (setq url (url url))
  (with-open-url (sock url :rt *html-readtable* :err t)
    (do (rr (ii 0 (1+ ii)) (ts (make-text-stream :sock sock)))
	((eq +eof+ (setq rr (read-next ts))))
      (declare (type (unsigned-byte 20) ii))
      (format out fmt ii rr))))

;;;
;;; HyperSpec examples
;;;

(defcustom *html-specials* list
  '(("&gt;" . #\>) ("&lt;" . #\<) ("&quot;" . #\") ("&amp;" . #\&))
  "Alist of translations of HTML specials like `&*'.")

(defun html-translate-specials (str)
  "Replace (non-destructively) HTML specals with their interpretations."
  (declare (string str))
  (do ((beg 0 (1+ beg)) res (len (length str)))
      ((>= beg len) (coerce (nreverse res) 'string))
    (declare (fixnum beg))
    (case (char str beg)
      (#\< (setq beg (or (position #\> str :start beg) len)))
      (#\&
       (let ((pa (find str *html-specials* :test
                       (lambda (str pair)
                         (let ((end (+ beg (length (car pair)))))
                           (and (>= len end)
                                (string= str (car pair) :start1 beg
                                         :end1 end)))))))
         (incf beg (1- (length (car pa))))
         (push (cdr pa) res)))
      (t (push (char str beg) res)))))

(defcustom *hyperspec-root* simple-string "/usr/doc/lisp/HyperSpec/"
  "The root of the HyperSpec tree.")

(defun hyperspec-snarf-examples (&key (root *hyperspec-root*)
                                 (out *standard-output*))
  "Get the examples from the HyperSpec."
  (declare (pathname root) (type output-stream out))
  (format t " *** processing `~a'~%" root)
  (dolist (fl (directory (merge-pathnames "*.html" root)))
    (with-open-file (ff fl)
      (unless (or (null (skip-search ff "<P><B>Examples:</B><P>"))
                  (null (skip-search ff "<PRE>")))
        (format out " +++ `~a'~%" fl)
        (do ((st (read-line ff nil +eof+) (read-line ff nil +eof+)))
            ((or (eq st +eof+) (string= st "</PRE>")))
          (princ (html-translate-specials st) out) (terpri out)))))
  (dolist (dir (directory (merge-pathnames "*/" root)))
    (hyperspec-snarf-examples :root dir :out out)))

(provide "url")
;;; url.lisp ends here
