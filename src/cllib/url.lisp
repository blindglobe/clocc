;;; File: <url.lisp - 1999-04-19 Mon 19:38:13 EDT sds@eho.eaglets.com>
;;;
;;; Url.lisp - handle url's and parse HTTP
;;;
;;; Copyright (C) 1998-1999 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: url.lisp,v 1.25 1999/04/19 23:40:28 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/url.lisp,v $
;;; $Log: url.lisp,v $
;;; Revision 1.25  1999/04/19 23:40:28  sds
;;; (url-time): print the time difference.
;;; (open-url, url-get): fixed the call to (error 'code).
;;;
;;; Revision 1.24  1999/04/19 15:56:12  sds
;;; (*url-default-max-retry*): new user variable, the
;;; default for the `max-retry' key.
;;;
;;; Revision 1.23  1999/04/16 16:01:18  sds
;;; (with-tag): added `value' key; better default for `terpri'.
;;; (with-open-html): added `head', `comment' and `footer' keys;
;;; fixed `doctype' key.
;;; (directory-index): added `&rest opts' for comment.
;;; use `value' key when calling `with-tag'.
;;;
;;; Revision 1.22  1999/04/11 19:58:00  sds
;;; Added `*html-output*' and `with-tag'.
;;; (with-open-html): bind `*html-output*'.  use `with-tag'.
;;; (directory-index): use `with-tag'.
;;;
;;; Revision 1.21  1999/04/06 21:57:33  sds
;;; Added `directory-index' and `with-open-html'.
;;;
;;; Revision 1.20  1999/03/24 17:01:54  sds
;;; More `*html-specials*'.  Added `url-rfc'.
;;; `socket-service-port' returns 4 values now.
;;; Added `*url-bytes-transferred*', `*url-opening-time*' and `url-eta'.
;;; Added two new keyword arguments to `ftp-list' - :name and :log.
;;;
;;; Revision 1.19  1999/02/09 23:19:58  sds
;;; Removed `throw-timeout'.
;;; Added `socket-host', `socket-port' and a condition `timeout'.
;;;
;;; Revision 1.18  1999/02/08 20:45:37  sds
;;; Updated for cmucl 18b.
;;;
;;; Revision 1.17  1999/02/02 00:02:33  sds
;;; Moved `html-translate-specials' &c to clhs.lisp
;;; Expanded `url-time'.
;;;
;;; Revision 1.16  1999/01/13 20:43:24  sds
;;; Replaced top-level `*html-readtable*' creation forms with a new
;;; function, `make-html-readtable'.
;;;
;;; Revision 1.15  1999/01/13 18:27:03  sds
;;; `read-html-markup' now handles #\;, #\: and #\, so it is not necessary
;;; now to remove these characters from buffer in `ts-pull-next'.
;;;
;;; Revision 1.14  1999/01/09 22:15:42  sds
;;; Extracted `ts-pull-next' from `read-next'.
;;;
;;; Revision 1.13  1999/01/08 17:15:25  sds
;;; Made `read-html-markup' skip `*html-specials*'.
;;; Added `with-timeout' for CMUCL, `socket-to-file', `*ts-kill*' (used in
;;; `read-next'), `url-get' (unifies all `url-get-*' functions).
;;;
;;; Revision 1.12  1999/01/07 03:58:08  sds
;;; Use `index-t' instead of (unsigned-byte 20).
;;; Use `file-size-t' instead of (unsigned-byte 32).
;;;
;;; Revision 1.11  1998/12/29 17:12:14  sds
;;; Added `*nntpserver*', `url-get-host', `*url-default-sleep*',
;;; `*url-default-timeout*', `sleep-mesg', `with-timeout',
;;; `y-or-n-p-timeout', `finger'.
;;; Added news URL handling.
;;; Added `bin' argument to `ftp-get-passive-socket'.
;;;
;;; Revision 1.10  1998/12/07 16:53:22  sds
;;; Added MAILTO handling; made :prot a keyword.
;;; New function: `send-mail'.
;;; Renamed `ftp-ask' to `url-ask'.
;;;
;;; Revision 1.9  1998/11/21 21:01:43  sds
;;; Added `throw-timeout' to `open-socket-retry' and `open-url'.
;;;
;;; Revision 1.8  1998/11/20 21:52:11  sds
;;; Added reget functionality to `ftp-get-file'.
;;;
;;; Revision 1.7  1998/11/20 03:15:50  sds
;;; Added `open-socket-retry'.
;;;
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
  (sds-require "base") (sds-require "print")
  (sds-require "util") (sds-require "date")
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

;;;
;;; {{{ HTML parsing
;;;

;;(setq *read-eval* nil *read-suppress* t) ; for parsing
;;(setq *read-eval* t *read-suppress* nil) ; original

(defcustom *html-parse-tags* (member t nil) nil
  "*If non-nil, parse tags, if nil - return nil for all tags.")
(defcustom *html-verbose* (member t nil) nil "*Be verbose while parsing.")
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
;;; }}}{{{ URL handling
;;;

(defstruct (url #+cmu (:print-function print-struct-object))
  "URL - Uniform Resource Locator: protocol://user#password@host:port/path."
  (prot nil :type symbol)       ; protocol
  (user "" :type simple-string) ; username
  (pass "" :type simple-string) ; password
  (host "" :type simple-string) ; hostname
  (port 0  :type fixnum)        ; port number
  (path "" :type simple-string)) ; pathname

(defun url-rfc (protocol)
  "Return the RFC url for the given protocol.
See <http://www.cis.ohio-state.edu/hypertext/information/rfc.html>
<http://www.internic.net/wp>, <ftp://ds.internic.net/rfc>"
  (ecase (etypecase protocol (symbol protocol) (url (url-prot protocol)))
    ((:http :www) #(1945 2068))
    (:ftp #(959))
    ((:smtp :mailto) #(821))
    (:telnet #(1205))
    (:whois #(954 2167))
    (:finger #(1288))
    (:time #(1305))
    ((:nntp :news) #(977))))

(defun socket-service-port (&optional service (protocol "tcp"))
  "Return the port number of the SERVICE."
  ;; #+clisp (lisp:socket-service-port service protocol)
  ;; #-clisp
  (flet ((parse (str)
           (let ((tok (string-tokens
                       (nsubstitute
                        #\Space #\/ (subseq str 0 (or (position #\# str)
                                                      (length str)))))))
             (values (string-downcase (string (first tok)))
                     (mapcar (compose string-downcase string) (cdddr tok))
                     (second tok)
                     (string-downcase (string (third tok)))))))
    (with-open-file (fl #+unix "/etc/services" #+win32
                        (concatenate 'string (system:getenv "windir")
                                     "/system32/etc/services")
                        :direction :input)
      (loop :with name :and alis :and port :and prot
            :for st = (read-line fl nil +eof+)
            :until (eq st +eof+)
            :unless (or (equal "" st) (char= #\# (schar st 0)))
              :do (setf (values name alis port prot) (parse st)) :and
              :if service
                :when (and (string-equal protocol prot)
                           (or (string-equal service name)
                               (member service alis :test #'string-equal)))
                  :return (values name alis port prot) :end
                :else :collect (vector name alis port prot) :end :end
            :finally (when service
                       (error "service ~s is not found for protocol ~s"
                              service protocol))))))

(defun url-get-port (url)
  "Get the correct port of the URL - if the port is not recorded there,
guess from the protocol."
  (declare (type url url))
  (if (zerop (url-port url))
      (flet ((ssp (st) (ignore-errors (nth-value 2 (socket-service-port st)))))
        (or (ssp (string-downcase (string (url-prot url))))
            (ssp (case (url-prot url)
                   (:mailto "smtp") (:news "nntp") (:www "http")))
            (error "[url-get-port] Cannot guess the port for ~s" url)))
      (url-port url)))

(defcustom *nntpserver* simple-string
  (or (getenv "NNTPSERVER") "localhost")
  ;; (setq *nntpserver* "news0-alterdial.uu.net")
  "*The NNTP server to be user for `news' URLs.")

(defun url-get-host (url)
  "Get the right host for the URL: if it is a `news', use `*nntpserver*'."
  (declare (type url url))
  (if (plusp (length (url-host url))) (url-host url)
      (case (url-prot url) ((:news :nntp) *nntpserver*))))

(defun url-path-dir (url)
  "Return the dir part of the url's path, dropping the file name."
  (setq url (url url))
  (subseq (url-path url) 0
          (1+ (or (position #\/ (url-path url) :from-end t) -1))))

(defun url-path-file (url)
  "Return the file part of the url's path, dropping the dir."
  (setq url (url url))
  (subseq (url-path url)
          (1+ (or (position #\/ (url-path url) :from-end t) -1))))

(defmethod print-object ((url url) (stream stream))
  "Print the URL in the standard form."
  (when *print-readably* (return-from print-object (call-next-method)))
  (when *print-escape* (write-string "\"" stream))
  (let ((*print-escape* nil))
    (write (url-prot url) :stream stream :case :downcase)
    (write-string ":" stream)
    (unless (or (eq :mailto (url-prot url)) (eq :file (url-prot url))
                (zerop (length (url-host url))))
      (write-string "//" stream))
    (unless (zerop (length (url-user url)))
      (write (url-user url) :stream stream)
      (unless (zerop (length (url-pass url)))
        (write-string "#" stream) (write (url-pass url) :stream stream))
      (write-string "@" stream))
    (unless (zerop (length (url-host url)))
      (write (url-host url) :stream stream)
      (unless (zerop (url-port url))
        (write-string ":" stream) (write (url-port url) :stream stream)))
    (assert (or (zerop (length (url-path url)))
                (eq (url-prot url) :news) (eq (url-prot url) :nntp)
                (char= #\/ (aref (url-path url) 0)))
            ((url-path url))
            "non-absolute path in url: `~a'" (url-path url))
    (when (and (not (zerop (length (url-host url))))
               (or (eq (url-prot url) :news) (eq (url-prot url) :nntp))
               (not (zerop (length (url-path url))))
               (not (char= #\/ (aref (url-path url) 0))))
      (write-string "/" stream))
    (write-string (url-path url) stream))
  (when *print-escape* (write-string "\"" stream)))

(defcustom *url-special-chars* simple-string "#%&*+,-./:=?@_~"
  "*The string consisting of non-alphanumeric characters allowed in a URL.")

(defun url-constituent-p (char)
  "Check whether the character can be part of a URL."
  (declare (character char))
  (and (characterp char)
       (or (alphanumericp char)
           (find char *url-special-chars* :test #'char=))))

(fmakunbound 'url)
(defgeneric url (xx)
  (:documentation "Convert the object into URL.
The argument can be:
   - a URL - returned untouched;
   - a string - it is non-destructively parsed;
   - a symbol - it is uninterned and its name is non-destructively parsed;
   - a stream - read from.")
  (:method ((xx url)) xx)
  (:method ((xx symbol)) (unintern xx) (url (symbol-name xx)))
  (:method ((xx stream))
    (url (with-output-to-string (st)
           (peek-char t xx) ; skip whitespace
           (loop :for zz :of-type character = (read-char xx)
                 :while (url-constituent-p zz)
                 :do (write zz :stream st)))))
  (:method ((xx pathname)) (make-url :prot :file :path (namestring xx))))
(declaim (ftype (function (t) url) url))
(defcustom *url-guess-protocol* list
  '(("www" . :http) ("web" . :http) ("w3" . :http)
    ("ftp" . :ftp) ("news" . :news) ("nntp" . :nntp))
  "*The alist of (\"string\" . protocol) to guess the protocol from the host.")
(defmethod url ((xx string))
  (let* ((string (string-trim +whitespace+ xx)) (url (make-url)) slashp
         (idx (position #\: string :test #'char=)) (start 0) idx0)
    (declare (simple-string string) (type index-t start) (type url url)
             (type (or null index-t) idx))
    (when (char= #\/ (char string 0))
      (return-from url
        (progn (setf (url-prot url) :file (url-path url) string) url)))
    (when idx
      (setf (url-prot url) (kwd (nstring-upcase (subseq string 0 idx))))
      (setq start (position #\/ string :start (1+ idx) :test #'char/=)
            slashp (/= (1+ idx) start)))
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
    (if (and (not slashp)
             (or (eq (url-prot url) :nntp) (eq (url-prot url) :news)))
        (setf (url-path url)
              (concatenate 'string (subseq string start (or idx idx0))
                           (url-path url)))
        (setf (url-host url) (subseq string start (or idx idx0))))
    (unless (url-prot url)
      (cond ((let ((pa (assoc (url-host url) *url-guess-protocol* :test
                              (lambda (ho st)
                                (declare (simple-string ho st))
                                (string-beg-with st ho)))))
               (when pa (setf (url-prot url) (cdr pa)))))
            ((position #\@ string) (setf (url-prot url) :mailto))
            ((error "url: `~a': no protocol specified" string))))
    url))

;;;
;;; }}}{{{ Sockets
;;;

#+allegro (deftype socket () 'excl::socket-stream)
#+clisp (deftype socket () 'stream)
#+cmu (deftype socket () 'system:fd-stream)

(defun open-socket (host port &optional bin)
  "Open a socket connection to HOST at PORT."
  (declare (type (or integer string) host) (fixnum port) (type boolean bin))
  (let ((host (etypecase host
                (string host) (integer (resolve-host-ipaddr host)))))
    #+allegro (socket:make-socket :remote-host host :remote-port port
                                  :format (if bin :binary :text))
    #+clisp (lisp:socket-connect port host :element-type
                                 (if bin '(unsigned-byte 8) 'character))
    #+cmu (system:make-fd-stream (ext:connect-to-inet-socket host port)
                                 :input t :output t :element-type
                                 (if bin '(unsigned-byte 8) 'character))))

(defun resolve-host-ipaddr (host)
  "Call gethostbyname(3) or gethostbyaddr()."
  #+allegro
  (etypecase host
    (string
     (if (every (lambda (ch) (or (char= ch #\.) (digit-char-p ch))) host)
         (resolve-host-ipaddr (dotted-to-ipaddr host))
         (values host nil (socket:lookup-hostname host) 2)))
    (integer (values (socket:ipaddr-to-hostname host) nil
                     (ipaddr-to-dotted host) 2)))
  #+clisp (error "resolve-host-ipaddr [~s] not implemented" host)
                                ; (lisp:resolve-host-ipaddr host)
  #+cmu (let ((he (ext:lookup-host-entry host)))
          (values (ext:host-entry-name he)
                  (ext:host-entry-aliases he)
                  (mapcar #'ipaddr-to-dotted (ext:host-entry-addr-list he))
                  (ext::host-entry-addr-type he))))

(defun ipaddr-to-dotted (ipaddr)
  "Number --> string."
  (declare (type (unsigned-byte 32) ipaddr) (values simple-string))
  #+allegro (socket:ipaddr-to-dotted ipaddr)
  #-allegro
  (format nil "~d.~d.~d.~d"
          (logand #xff (ash ipaddr -24)) (logand #xff (ash ipaddr -16))
          (logand #xff (ash ipaddr -8)) (logand #xff ipaddr)))

(defun dotted-to-ipaddr (dotted)
  "String --> number."
  (declare (string dotted) (values (unsigned-byte 32)))
  #+allegro (socket:dotted-to-ipaddr dotted)
  #-allegro
  (let ((ll (string-tokens (substitute #\Space #\. dotted))))
    (+ (ash (first ll) 24) (ash (second ll) 16)
       (ash (third ll) 8) (fourth ll))))

(defun socket-host (sock)
  "Return the remote host name."
  (declare (type socket sock))
  #+clisp (lisp:socket-stream-host sock)
  #+allegro (socket:ipaddr-to-dotted (socket:remote-host sock))
  #+cmu (extensions::gethostbyaddr (ext:get-socket-host-and-port sock)))

(defun socket-port (sock)
  "Return the remote port number."
  (declare (type socket sock))
  #+clisp (lisp:socket-stream-port sock)
  #+allegro (socket:remote-port sock)
  #+cmu (nth-value 1 (ext:get-socket-host-and-port sock)))

(defun open-socket-server (sock)
  "Open a `generic' socket server."
  (declare (ignorable sock) (type socket sock))
  #+allegro (socket:make-socket :connect :passive)
  #+clisp (lisp:socket-server sock)
  #+cmu (ext:create-inet-socket))

(define-condition network (error)
  ((proc :type symbol :reader net-proc :initarg :proc)
   (host :type simple-string :reader net-host :initarg :host)
   (port :type (unsigned-byte 16) :reader net-port :initarg :port)
   (mesg :type simple-string :reader net-mesg :initarg :mesg :initform "")
   (args :type list :reader net-args :initarg :args :initform nil))
  (:report (lambda (cc out)
             (declare (stream out))
             (format out "[~s] ~s:~d~@[ ~?~]"
                     (net-proc cc) (net-host cc) (net-port cc)
                     (when (slot-boundp cc 'mesg) (net-mesg cc))
                     (when (slot-boundp cc 'args) (net-args cc))))))

(define-condition timeout (network)
  ((time :type (real 0) :reader timeout-time :initarg :time))
  (:report (lambda (cc out)
             (declare (stream out))
             (call-next-method)
             (when (slot-boundp cc 'time)
               (format out " [timeout ~a sec]" (timeout-time cc))))))

(define-condition code (error)
  ((proc :type symbol :reader code-proc :initarg :proc)
   (mesg :type simple-string :reader code-mesg :initarg :mesg :initform "")
   (args :type list :reader code-args :initarg :args :initform nil))
  (:report (lambda (cc out)
             (declare (stream out))
             (format out "[~s] ~?" (code-proc cc) (code-mesg cc)
                     (code-args cc)))))

(defcustom *url-default-sleep* (real 0) 30
  "*The number of seconds to sleep when necessary.")
(defcustom *url-default-timeout* (real 0) 86400
  "*The default timeout, in seconds.")
(defcustom *url-default-max-retry* (or null index-t) nil
  "*The default value of max-retry.
If nil, retry ad infinitum, otherwise a positive fixnum.")

(defun sleep-mesg (sleep out mesg)
  "Sleep for a random period of up to SLEEP seconds.
Print the appropriate message MESG to OUT."
  (declare (type (or null stream) out) (real sleep))
  (let ((sleep (random sleep)))
    (when out
      (format out "~a; sleeping for ~d second~:p..." mesg sleep)
      (force-output out))
    (sleep sleep)
    (when out (format out "done~%"))))

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds, terminate
and evaluate TIMEOUT-FORMS."
  (declare (ignorable seconds timeout-forms))
  #+(or allegro cmu) `(mp:with-timeout (,seconds ,@timeout-forms) ,@body)
  #+clisp `(progn ,@body))

(defun y-or-n-p-timeout (seconds default &rest args)
  "`y-or-n-p' with timeout."
  (declare (ignorable seconds default))
  (with-timeout (seconds (format t "[Timed out] ~:[NO~;YES~]~%" default)
                         default)
    (apply #'y-or-n-p args)))

(defsubst url-prot-bin (prot)
  "Return T if the protocol is binary."
  (declare (symbol prot))
  (eq prot :time))

(defun open-socket-retry (host port &key (err *standard-output*) bin
                          (sleep *url-default-sleep*)
                          (max-retry *url-default-max-retry*)
                          (timeout *url-default-timeout*))
  "Open a socket connection, retrying until success."
  (declare (simple-string host) (fixnum port) (type (or null stream) err)
           (type (or null index-t) max-retry) (type (real 0) sleep timeout))
  (loop :with begt = (get-universal-time) :and err-cond
        :for ii :of-type index-t :upfrom 1
        :for sock =
        (handler-case
            (progn
              (mesg :log err
                    "~&[~d~@[/~d~]] Connecting to ~a:~d [timeout ~:d sec]..."
                    ii max-retry host port timeout)
              (with-timeout (timeout
                             (error 'timeout :proc 'open-socket-retry :host
                                    host :port port :time timeout))
                (open-socket host port bin)))
          (error (co)
            (setq err-cond co)
            (mesg :log err "~%Error connecting: ~a~%" co)))
        :when (and err sock) :do (mesg :log err "done: ~a~%" sock)
        :when (and sock (open-stream-p sock)) :return sock
        :when (and max-retry (>= ii max-retry)) :return err-cond
        :when (>= (- (get-universal-time) begt) timeout)
        :do (error 'timeout :proc 'open-socket-retry :host host :port port
                   :time timeout)
        :do (sleep-mesg sleep err "[open-socket-retry] Error")))

(defun open-url (url &key (err *standard-output*) (sleep *url-default-sleep*)
                 (timeout *url-default-timeout*)
                 (max-retry *url-default-max-retry*))
  "Open a socket connection to the URL.
Issue the appropriate initial commands:
 if this is an HTTP URL, also issue the GET command;
 if this is an FTP URL, login and cwd;
 if this is a NEWS/NNTP URL, set group and possibly request article;
 if this is a WHOIS/FINGER URL, ask about the host/user.
If timeout is non-nil, it specifies the number of seconds before
the error `timeout' is signaled."
  (declare (type url url) (type (real 0) sleep timeout)
           (type (or null stream) err))
  (when (eq (url-prot url) :file)
    (return-from open-url (open (url-path url) :direction :input)))
  (loop :with begt = (get-universal-time) :and host = (url-get-host url)
        :and port = (url-get-port url)
        :for sock :of-type (or null socket error) =
        (open-socket-retry host port :err err :sleep sleep :timeout timeout
                           :bin (url-prot-bin (url-prot url))
                           :max-retry max-retry)
        :when (or (null sock) (typep sock 'error))
        :do (error 'network :proc 'open-url :host host :port port
                   :mesg "max-retry [~a] exceeded~@[ last error: ~a]"
                   :args (list max-retry sock))
        :when
        (handler-case
            (with-timeout (timeout nil)
              (case (url-prot url)
                ((:http :www) (setq sock (url-open-http sock url err)))
                (:ftp (url-ask sock err 220)
                      (url-login-ftp sock url err))
                (:telnet (dolist (word (split-string (url-path url) "/") t)
                           (format sock "~a~%" word)))
                ((:whois :finger :cfinger)
                 (format sock "~a~%" (url-path-file url)) t)
                (:mailto (url-ask sock err 220))
                ((:news :nntp)
                 (url-ask sock err 200)
                 (unless (zerop (length (url-path url)))
                   (let ((strs (split-string (url-path url) "/")))
                     (url-ask sock err 211 "group ~a" (car strs))
                     (when (cadr strs)
                       (url-ask sock err 220 "article ~a" (cadr strs)))))
                 t)
                ((:time :daytime) t)
                (t (error 'code :proc 'open-url :args (list (url-prot url))
                          :mesg "Cannot handle protocol ~s"))))
          (code (co) (error co))
          (error (co)
            (mesg :err err "Connection to `~a' dropped: `~a'~%" url co)))
        :return sock :when sock :do (close sock)
        :when (> (- (get-universal-time) begt) timeout)
        :do (error 'timeout :proc 'open-url :host host :port port
                   :time timeout)
        :do (sleep-mesg sleep err "[open-url] Connection dropped")))

(defcustom *url-bytes-transferred* integer 0
  "The number of bytes transferred during the current connection.")
(makunbound '*url-bytes-transferred*)
(defcustom *url-opening-time* double-float 0.0
  "The time when the current connection was open.")
(makunbound '*url-opening-time*)

(defmacro with-open-url ((socket url &key (rt '*readtable*) err
                                 (max-retry '*url-default-max-retry*)
                                 (timeout '*url-default-timeout*))
                         &body body)
  "Execute BODY, binding SOCK to the socket corresponding to the URL.
`*readtable*' is temporarily set to RT (defaults to `*readtable*').
ERR is the stream for information messages or NIL for none."
  (let ((uuu (gensym "WOU")))
    `(let* ((,uuu (url ,url)) (*readtable* ,rt)
            (,socket (open-url ,uuu :err ,err :timeout ,timeout
                               :max-retry ,max-retry))
            (*url-opening-time* (get-float-time nil))
            (*url-bytes-transferred* 0))
      (declare (type url ,uuu) (type socket ,socket))
      (unwind-protect (progn ,@body)
        (case (url-prot ,uuu)
          ((:ftp :mailto) (url-ask ,socket ,err 221 "quit"))
          ((:news :nntp) (url-ask ,socket ,err 205 "quit")))
        (close ,socket)))))

(defun url-open-http (sock url err)
  "Open the socket to the HTTP url."
  (declare (type socket sock) (type url url) (type (or null stream) err))
  (format sock "GET ~a HTTP/1.0~2%" (url-path url))
  (do ((sk sock) (stat 302) sym res) ((not (eql stat 302)) sk)
    (declare (fixnum stat))
    (setq sym (read sk) stat (read sk))
    (when (string-equal sym "http/1.1")
      (when (>= stat 400) (error "~d: ~a~%" res (read-line sk))) ; error
      (when (= stat 302)        ; redirection
        (setq res (read-line sk)) (read-line sk) (read-line sk)
        (setq sym (read-line sk)
              sym (url (subseq sym (1+ (position #\: sym)))))
        (when (equal "" (url-host sym)) (setf (url-host sym) (url-host url)))
        (format err " *** redirected to `~a' [~a]~%" sym res)
        (when *html-verbose*
          (do (st) ((eq +eof+ (setq st (read-line sk nil +eof+))))
            (format t "~a~%" st)))
        (close sk) (setq sk (open-url sym :err err))
        (format sk "GET ~a HTTP/1.0~2%" (url-path sym))))))

(defun url-ask (sock out end &rest req)
  "Send a request; read the response."
  (declare (type socket sock) (type (or null stream) out)
           (type (unsigned-byte 10) end))
  (when req
    (apply #'format sock req) (fresh-line sock)
    (when out (apply #'format out "~&url-ask[~d]: `~@?'~%" end req)))
  (loop :for ln :of-type simple-string = (read-line sock)
        :and code :of-type (unsigned-byte 10) = 0
        :when out :do (format out "~&url-ask[~d]: ~a~%" end
                              (setq ln (string-right-trim +whitespace+ ln)))
        :while (or (< (length ln) 3) (char/= #\Space (schar ln 3))
                   (progn (setq code (or (parse-integer
                                          ln :end 3 :junk-allowed t) 0))
                          (and (< code 400) (/= end code))))
        :finally (return (values ln code))))

(defun ftp-parse-sextuple (line)
  "Convert a0,a1,a2,a3,b0,b1 to HOST and PORT."
  (declare (simple-string line))
  (let* ((p0 (position #\) line :from-end t))
         (p1 (position #\, line :from-end t :end p0))
         (p2 (position #\, line :from-end t :end (1- p1))))
    (declare (type index-t p1 p2))
    (setf (schar line p1) #\Space (schar line p2) #\Space)
    (nsubstitute #\. #\, line)
    (values (subseq line (1+ (or (position #\( line :from-end t) -1)) p2)
            (+ (ash (parse-integer line :start p2 :end p1) 8)
               (parse-integer line :start p1 :end p0)))))

(defun ftp-get-passive-socket (sock out bin timeout)
  "Get a passive socket."
  (declare (type socket sock) (values socket))
  (loop :for sck =
        (multiple-value-bind (st cd) (url-ask sock out 227 "pasv")
          (when (>= cd 400)
            (error 'timeout :proc 'ftp-get-passive-socket :mesg st :host
                   (socket-host sock) :port (socket-port sock)))
          (multiple-value-call #'open-socket-retry (ftp-parse-sextuple st)
                               :err out :max-retry 5 :bin bin
                               :timeout timeout))
        :when sck :return sck))

(defun url-login-ftp (sock url err)
  "Login and cd to the FTP url."
  (declare (type socket sock) (type url url) (type (or null stream) err))
  (and (> 400 (nth-value 1 (url-ask sock err 331 "user ~a"
                                    (if (zerop (length (url-user url)))
                                        "anonymous" (url-user url)))))
       (url-ask sock err 230 "pass ~a" (if (zerop (length (url-pass url)))
                                           "ftp@ftp.net" (url-pass url)))
       (url-ask sock err 215 "syst")
       ;; (url-ask sock err 200 "type i")
       (url-ask sock err 211 "stat")
       (url-ask sock err 250 "cwd ~a" (url-path-dir url))))

(defcustom *buffer* (simple-array (unsigned-byte 8) (10240))
  (make-array 10240 :element-type '(unsigned-byte 8))
  "The download buffer - simple array of bytes.
The reasonable value for it's length is determined by your connection speed.
I recommend 10240 for 112kbps ISDN and 2048 for 28.8kbps, i.e.,
approximately, the number of bytes you can receive per second.")

(defun socket-to-file (data path &key rest (out *standard-output*))
  "Read from a binary socket to a file.
Read until the end, then close the socket."
  (declare (type socket data) (type pathname path) (type (or null stream) out))
  (with-open-file (fl path :direction :output :element-type 'unsigned-byte
                      :if-exists (if rest :append :supersede))
    (loop :for pos :of-type index-t = (read-sequence *buffer* data)
          :do (write-sequence *buffer* fl :end pos)
          :sum pos :of-type file-size-t
          :when out :do (princ "." out) (force-output out)
          :while (= pos (length *buffer*))
          :finally (when out (terpri out)) :finally (close data))))

(defun url-eta (len)
  "Return the ETA for the given length or nil or cannot determine."
  (declare (type (or null real) len))
  (and len (boundp '*url-bytes-transferred*)
       (not (zerop *url-bytes-transferred*))
       (/ (* len (elapsed *url-opening-time* nil))
          *url-bytes-transferred*)))

(defun ftp-get-file (sock rmt loc &key (out *standard-output*) (reget t)
                     (bin t) (retry 2) (timeout *url-default-timeout*)
                     (err *error-output*))
  "Get the remote file RMT from the FTP socket SOCK,
writing it into the local directory LOC.  Log to OUT.
Append if the file exists and REGET is non-nil.
Use binary mode if BIN is non-nil (default).
Retry (+ 1 RETRY) times if the file length doesn't match the expected."
  (declare (type socket sock) (type index-t retry)
           (type (or null stream) out err) (simple-string rmt)
           (values file-size-t double-float simple-string pathname))
  (let* ((data (ftp-get-passive-socket sock err t timeout)) (tot 0)
         (bt (get-float-time nil)) (path (merge-pathnames rmt loc))
         (rest (when (and reget (probe-file path))
                 (let ((sz (file-size path)))
                   (mesg :log out "File `~a' exists (~:d bytes), ~
~:[appending~;overwriting~]...~%" path sz (zerop sz))
                   (unless (zerop sz)
                     (url-ask sock err 350 "rest ~d" sz)
                     sz))))
         (line (progn (url-ask sock err 200 "type ~:[a~;i~]" bin)
                      (url-ask sock err 150 "retr ~a" rmt)))
         (pos (position #\( line :from-end t))
         (len (when pos (read-from-string line nil nil :start (1+ pos)))))
    (declare (type socket data) (type file-size-t tot) (type pathname path)
             (double-float bt) (type (or null file-size-t) rest len))
    ;; (when rest (decf len rest))
    (if (null len) (mesg :log out "File lenth unknown.~%")
        (mesg :log out "Expect ~:d dot~:p for ~:d bytes~@[ [~/pr-secs/]~]~%"
              (ceiling (1+ len) (length *buffer*)) len (url-eta len)))
    (setq tot (socket-to-file data path :rest rest :out out))
    (url-ask sock err 226)
    (cond ((or (null len) (= tot len))
           (when (boundp '*url-bytes-transferred*)
             (incf *url-bytes-transferred* tot))
           (multiple-value-call #'values tot (elapsed bt nil t) path))
          ((plusp retry)
           (mesg :log out "### Wrong file length: ~:d (expected: ~:d [~@:d])
 +++ ~r more attempt~:p +++~%" tot len (- tot len) retry)
           (ftp-get-file sock rmt loc :out out :err err :reget nil :bin bin
                         :retry (1- retry) :timeout timeout))
          ((error "Wrong file length: ~:d (expected: ~:d [~@:d])"
                  tot len (- tot len))))))

(defun url-ftp-get (url loc &rest opts &key (out *standard-output*)
                    (err *error-output*) (max-retry *url-default-max-retry*)
                    (timeout *url-default-timeout*) &allow-other-keys)
  "Get the file specified by the URL, writing it into a local file.
The local file is located in directory LOC and has the same name
as the remote one."
  (declare (type url url) (type (or null stream) out err))
  (mesg :log out "~& *** getting `~a'...~%" url)
  (remf opts :max-retry)
  (with-open-url (sock url :err err :timeout timeout :max-retry max-retry)
    (multiple-value-bind (tot el st)
        (apply #'ftp-get-file sock (url-path-file url) loc opts)
      (mesg :log out " *** done [~:d bytes, ~a, ~:d bytes/sec]~%" tot st
            (round tot el)))))

(defun ftp-list (sock &key name (out *standard-output*) (err *error-output*)
                 (timeout *url-default-timeout*))
  "Get the file list."
  (declare (type socket sock) (type (or null stream) out err))
  (let ((data (ftp-get-passive-socket sock err nil timeout)))
    (url-ask sock err 150 "list~@[ ~a~]" name)
    (loop :for line = (read-line data nil nil) :while line :when out :do
          (format out "~a~%" (string-right-trim +whitespace+ line)))
    (url-ask sock err 226)))

;;; Mail

(defcustom *mail-host-address* simple-string
  (let ((st (machine-instance))) (subseq st 0 (position #\Space st)))
  "*Name of this machine, for purposes of naming users.")
(defcustom *user-mail-address* simple-string
  (concatenate 'string (getenv "USER") "@" *mail-host-address*)
  "*Full mailing address of this user.
This is initialized based on `mail-host-address'.")

(defun url-send-mail (url &key (err *error-output*)
                      (text (current-time nil))
                      (helo *mail-host-address*)
                      (from *user-mail-address*))
  "Send TEXT to URL (which should be a MAILTO)."
  (declare (type url url) (type (or null stream) err)
           (simple-string text helo from))
  (assert (eq :mailto (url-prot url)) (url)
          "url-send-mail: `~a' is not a `mailto'" url)
  (with-open-url (sock url :err err)
    (url-ask sock err 250 "helo ~a" helo)
    (url-ask sock err 250 "mail from: ~a" from)
    (url-ask sock err 250 "rcpt to: ~a" (url-user url))
    (url-ask sock err 354 "data")
    (url-ask sock err 250 "~a~%." text)))

;;; News

(defstruct (article #+cmu (:print-function print-struct-object))
  (numb 0 :type (unsigned-byte 32)) ; article number
  (subj "" :type simple-string) ; subject
  (auth "" :type simple-string) ; author
  (dttm 0 :type (integer 0))    ; date/time
  (msid "" :type simple-string) ; message-ID
  (msid1 "" :type simple-string) ; ????
  (bytes 0 :type file-size-t)   ; size in bytes
  (lines 0 :type index-t)       ; size in lines
  (xref nil :type list))

(defmethod print-object ((art article) (out stream))
  (if *print-readably* (call-next-method)
      (format out "~:[~;\"~]~d~c~a~c~a~c~a~c~a~c~d~c~d~cXref:~{ ~a~}~:[~;\"~]"
              *print-escape* (article-numb art) #\Tab (article-subj art) #\Tab
              (article-auth art) #\Tab (dttm->string (article-dttm art)) #\Tab
              (article-msid art) #\Tab (article-bytes art) #\Tab
              (article-lines art) #\Tab (article-xref art) *print-escape*)))

(defun string->article (string)
  "Parse the string as returned by `xover'."
  (declare (simple-string string))
  (multiple-value-bind (numb subj auth dttm msid msid1 bytes lines xref)
      (values-list (split-string string '(#\Tab) :strict t))
    (make-article :numb (parse-integer numb) :subj subj :auth auth
                  :dttm (string->dttm dttm) :msid msid :msid1 msid1
                  :bytes (parse-integer bytes) :lines (parse-integer lines)
                  :xref (cdr (split-string xref " ")))))

(defsubst read-trim (stream)
  "Read a line from stream and trim it."
  (declare (type stream stream) (values simple-string))
  (string-trim +whitespace+ (read-line stream nil ".")))

(defun url-dump-to-dot (sock &key (out *standard-output*) collect)
  "Read from SOCK until dot."
  (declare (type socket sock) (type (or string stream) out))
  (let ((str (if (streamp out) out
                 (open out :direction :output :if-exists :supersede))))
    (declare (stream str))
    (unwind-protect
         (loop :for st :of-type simple-string = (read-trim sock)
               :until (string= "." st) :do (write-string st str) (terpri str)
               :when collect :collect st)
      (unless (streamp out) (close str)))))

(defun url-get-news (url loc &key (out *standard-output*) (err *error-output*)
                     (max-retry *url-default-max-retry*) re)
  "Get the news article to the OUT stream.
When RE is supplied, articles whose subject match it are retrieved."
  (declare (type url url) (stream out))
  (assert (or (eq :nntp (url-prot url)) (eq :news (url-prot url))) (url)
          "url-get-news: `~a' is not a `news'" url)
  (flet ((out (st) (if loc (merge-pathnames st loc) out)))
    (with-open-url (sock url :err err :max-retry max-retry)
      (let ((spl (split-string (url-path url) "/")))
        (cond ((cadr spl)       ; group and article
               (url-dump-to-dot sock :out (out (cadr spl))))
              ((car spl)        ; group only
               (multiple-value-bind (na a1 a2)
                   (values-list
                    (string-tokens (url-ask sock err 211 "group ~a" (car spl))
                                   :start 3 :max 3))
                 (let ((nm (format nil "~d-~d" a1 a2)))
                   (format out "~:d articles, from ~:d to ~:d~%" na a1 a2)
                   (url-ask sock err 224 "xover ~a" nm)
                   (let ((ls (map-in #'string->article
                                     (url-dump-to-dot sock :out (out nm)
                                                      :collect t))))
                     (when re
                       (dolist (art ls)
                         (when (search re (article-subj art))
                           (url-ask sock err 220 "article ~d"
                                    (article-numb art))
                           (url-dump-to-dot sock :out
                                            (out (article-numb art))))))
                     ls))))
              (t               ; not even group, just host
               (url-ask sock err 215 "list active")
               (url-dump-to-dot sock :out (out "active"))))))))

(defcustom *time-servers* list
  '("clock.psu.edu" "black-ice.cc.vt.edu" "clock1.unc.edu" "time-b.nist.gov"
    "time-b.timefreq.bldrdoc.gov" "clock-1.cs.cmu.edu" "ntp0.cornell.edu")
  "Public NTP servers (secondary).
For additional servers see http://www.eecis.udel.edu/~mills/ntp/servers.htm")

(defun url-time (&optional (url *time-servers*) (out *standard-output*))
  "Get the time out of the date/time url."
  (declare (stream out))
  (etypecase url
    (string (multiple-value-call #'values
              (url-time (make-url :prot :time :host url))
              (url-time (make-url :prot :daytime :host url))))
    (sequence
     (map 'list (lambda (uu)
                  (format out "~&~a:" uu) (force-output out)
                  (let ((val (multiple-value-list (url-time uu))))
                    (format out "~{~30t[~a -- ~a~@[ [~d]~]]~%~}" val)
                    val))
          url))
    (url (with-open-url (sock url)
           (ecase (url-prot url)
             (:time
              (let ((nn (+ (ash (read-byte sock) 24) (ash (read-byte sock) 16)
                           (ash (read-byte sock) 8) (read-byte sock))))
                (values nn (dttm->string nn) (- nn (get-universal-time)))))
             (:daytime
              (let ((str (read-line sock)))
                (if (zerop (length str)) (values)
                    (values (string->dttm (copy-seq str)) str nil)))))))))

;;;
;;; }}}{{{ HTML parsing
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
    ;; (nsubstitute #\space #\. str) breaks floats, so we have to be smart
    (do ((beg -1) (len (1- (length str))))
        ((or (= beg len)
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
  (loop :with tok :and pos
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
        :unless (or (typep pos 'error) (eq tok +eof+))
        :return (if (and skip (funcall skip tok))
                    (read-next ts :errorp errorp :kill kill :skip skip)
                    tok)))

(defun next-token (ts &key (num 1) type dflt (kill *ts-kill*))
  "Get the next NUM-th non-tag token from the HTML stream TS."
  (declare (type text-stream ts) (type index-t num))
  (let (tt)
    (dotimes (ii num (if (and type (not (typep tt type))) dflt tt))
      (declare (type index-t ii))
      (do () ((not (html-tag-p (setq tt (read-next ts :errorp t :kill kill))))
              (mesg :log t "token: ~a~%" tt))
        (mesg :log t "tag: ~a~%" tt)))))

(defun next-number (ts &key (num 1) (kill *ts-kill*))
  "Get the next NUM-th number from the HTML stream TS."
  (declare (type text-stream ts) (type index-t num))
  (let (tt)
    (dotimes (ii num tt)
      (declare (type index-t ii))
      (do () ((numberp (setq tt (next-token ts :kill kill))))))))

(defun skip-tokens (ts end &key (test #'eql) (key #'identity) kill)
  "Skip tokens until END, i.e., until (test (key token) end) is T."
  (declare (type text-stream ts))
  (do (tt) ((funcall test (setq tt (funcall key (next-token ts :kill kill)))
                     end)
            tt)))

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
  '((netscape "/usr/bin/netscape" "-remote" "openURL(~a,new-window)")
    (emacs-w3 "/usr/bin/gnudoit" "(w3-fetch \"~a\")"))
  "The ALIST of browsers.")

(defun view-url (url &optional (bro 'netscape))
  "Lounch a browser to view a url."
  (declare (type url url))
  (let ((br (copy-list (assoc bro *browsers* :test #'eq))) pos)
    (assert (consp br) (br) "Unknown browser ~a. Must be one of~?." bro
            (list-format "~a") (mapcar #'car *browsers*))
    (setq pos (1+ (position "~a" (cdr br) :test #'search)))
    (setf (nth pos br) (format nil (nth pos br) url))
    (run-prog (second br) :args (cddr br))
    (format t "launched ~a with args ~a~%" (car br) (cddr br))))

(defun dump-url (url &key (fmt "~3d: ~a~%") (out *standard-output*)
                 (err *error-output*) (timeout *url-default-timeout*)
                 (proc #'identity) (max-retry *url-default-max-retry*))
  "Dump the URL line by line.
FMT is the printing format. 2 args are given: line number and the line
itself. FMT defaults to \"~3d: ~a~%\".
OUT is the output stream and defaults to `*standard-output*'.
This is mostly a debugging function, to be called interactively."
  (declare (type url url) (stream out) (function proc))
  (format out "Opening URL: `~a'...~%" url)
  (with-open-url (sock url :err err :timeout timeout :max-retry max-retry)
    (loop :for ii :of-type index-t :from  1
          :and rr = (read-line sock nil +eof+) :until (eq +eof+ rr)
          :do (format out fmt ii
                      (funcall proc (string-right-trim +whitespace+ rr))))))

(defun url-get (url loc &key (timeout *url-default-timeout*)
                (max-retry *url-default-max-retry*)
                (err *error-output*) (out *standard-output*))
  "Get the URL.
This is the function to be called in programs.
Arguments: URL - what to get, LOC - where to place it.
Keywords: `timeout', `max-retry', `out', `err'."
  (declare (type url url) (type (or stream null) err out))
  (case (url-prot url)
    (:ftp (url-ftp-get url loc :timeout timeout :err err :out out
                       :max-retry max-retry))
    ((:nntp :news) (url-get-news url loc :out out :max-retry max-retry))
    ((:http :www)
     (let* ((path (merge-pathnames (url-path-file url) loc))
            (bt (get-float-time nil))
            (size (with-open-url (sock url :err err :timeout timeout
                                       :max-retry max-retry)
                    (socket-to-file sock path :out out))))
       (declare (type file-size-t size))
       (multiple-value-bind (el st) (elapsed bt nil t)
         (declare (double-float el))
         (mesg :log out "Wrote `~a' [~:d bytes, ~a, ~:d bytes/sec]."
               path size st (round size el)))))
    (t (error 'code :proc 'url-get :mesg "Cannot handle protocol ~s"
              :args (list (url-prot url))))))

(defun whois (host &rest keys)
  "Get the whois information on a host."
  (apply #'dump-url (make-url :prot :whois :host "rs.internic.net"
                              :path (concatenate 'string "/" (string host)))
         :fmt "~*~a~%" keys))

(defun finger (address &rest keys &key gnu &allow-other-keys)
  "Finger the mail address."
  (let* ((str-address (string address))
         (pos (position #\@ str-address :test #'char=)))
    (declare (simple-string str-address) (type (unsigned-byte 10) pos))
    (remf keys :gnu)
    (apply #'dump-url
           (make-url :prot (if gnu :cfinger :finger)
                     :host (subseq str-address (1+ pos))
                     :path (concatenate 'string "/"
                                        (subseq str-address 0 pos)))
           :fmt "~*~a~%" keys)))

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
  `(with-open-file (*html-output* ,@open-pars)
    (format *html-output* "<!doctype~{ ~s~}>~%" ,doctype)
    ;; print the comment
    (format *html-output* "<!--~% File: <~a - " ,(car open-pars))
    (current-time *html-output*)
    (format *html-output*
     " ~a@~a>~% Created by `with-open-html'~% Lisp: ~a ~a~@[~%~a~]~% -->~%"
     (getenv "USER") (machine-instance) (lisp-implementation-type)
     (lisp-implementation-version) ,comment)
    (with-tag (:meta :close nil ,@meta))
    (terpri *html-output*)
    (when ,base
      (with-tag (:base :close nil :href ,base))
      (terpri *html-output*))
    (with-tag (:html)
      (terpri *html-output*)
      (with-tag (:head ,@head) (with-tag (:title :value ,title)))
      (terpri *html-output*) (terpri *html-output*)
      (with-tag (:body)
        ,@body
        (when ,footer
          (terpri *html-output*) (terpri *html-output*)
          (with-tag (:p :terpri nil)
            (with-tag (:hr :close nil))
            (with-tag (:address :terpri nil)
              (with-tag (:a :href (format nil "mailto:~a" *user-mail-address*)
                            :value *user-mail-address*)))
            (terpri *html-output*) (with-tag (:br :close nil))
            (with-tag (:strong :value (current-time nil)))))))
    (terpri *html-output*)))

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

(provide "url")
;;; }}} url.lisp ends here
