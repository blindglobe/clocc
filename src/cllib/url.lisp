;;; File: <url.lisp - 1999-01-06 Wed 22:57:22 EST sds@eho.eaglets.com>
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
;;; $Id: url.lisp,v 1.12 1999/01/07 03:58:08 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/url.lisp,v $
;;; $Log: url.lisp,v $
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

(defcustom *html-readtable* readtable (copy-readtable nil)
  "The readtable for HTML parsing.")
(defcustom *html-parse-tags* (member t nil) nil
  "*If non-nil, parse tags, if nil - return nil for all tags.")
(defcustom *html-verbose* (member t nil) nil "*Be verbose while parsing.")
(defconst +html-tag+ cons (list :*html-tag*)
  "*The car of any html tag that is read.")

(defun strip-html-markup (str)
  "Return a new string, sans HTML."
  (declare (simple-string str))
  (apply #'concatenate 'string
         (do* ((p0 (position #\< str) (position #\< str :start p1))
               (res (list (subseq str 0 p0)))
               (p1 (position #\> str) (position #\> str :start (or p0 0))))
              ((or (null p0) (null p1)) (nreverse res))
           (push (subseq str (1+ p1) (position #\< str :start p1)) res))))

(defun read-html-markup (stream char)
  "Skip through the HTML markup. CHAR=`<'"
  (declare (stream stream) (ignore char))
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
;;(set-macro-character #\: (get-macro-character #\)) nil *html-readtable*)
;;(set-macro-character #\, (get-macro-character #\a) nil *html-readtable*)

;;;
;;; }}}{{{ URL handling
;;;

(eval-when (load compile eval)
(defstruct (url)
  "URL - Uniform Resource Locator: protocol://user#password@host:port/path."
  (prot nil :type symbol)       ; protocol
  (user "" :type simple-string) ; username
  (pass "" :type simple-string) ; password
  (host "" :type simple-string) ; hostname
  (port 0  :type fixnum)        ; port number
  (path "" :type simple-string)) ; pathname
)

(defun socket-service-port (protocol)
  "Return the port number of the PROTOCOL."
  (let ((prot (string-downcase (string protocol))))
    (declare (simple-string prot))
    #+clisp (let ((port (lisp:socket-service-port prot)))
              (if (/= -1 port) port nil))
    #-clisp
    (with-open-file (fl #+unix "/etc/services" #+win32
                        (concatenate 'string (system:getenv "windir")
                                     "/system32/etc/services")
                        :direction :input)
      (do ((st (read-line fl nil +eof+) (read-line fl nil +eof+))
           res pos srv)
          ((or res (eq st +eof+)) res)
        (unless (or (equal "" st) (char= #\# (schar st 0)))
          (setf (values srv pos) (read-from-string st nil ""))
          (when (string-equal prot srv)
            (nsubstitute #\space #\/ st)
            (setq res (read-from-string st nil nil :start pos))))))))

(defun url-get-port (url)
  "Get the correct port of the URL - if the port is not recorded there,
guess from the protocol."
  (declare (type url url))
  (if (zerop (url-port url))
      (or (case (url-prot url)
            (:mailto (socket-service-port "smtp"))
            (:news (socket-service-port "nntp")))
          (socket-service-port (url-prot url)))
      (url-port url)))

(defcustom *nntpserver* simple-string
  (or (getenv "NNTPSERVER") "localhost")
  ;; (setq *nntpserver* "news0-alterdial.uu.net")
  "*The NNTP server to be user for `news' URLs.")

(defun url-get-host (url)
  "Get the right host for the URL: if it is a `news', use `*nntpserver*'."
  (declare (type url url))
  (if (or (eq (url-prot url) :news) (eq (url-prot url) :nntp))
      *nntpserver* (url-host url)))

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
    (if (or (eq (url-prot url) :mailto) (eq (url-prot url) :news)
            (eq (url-prot url) :nntp))
        (write-string ":" stream)
        (write-string "://" stream))
    (unless (string= "" (url-user url))
      (write (url-user url) :stream stream)
      (unless (string= "" (url-pass url))
        (write-string "#" stream) (write (url-pass url) :stream stream))
      (write-string "@" stream))
    (write (url-host url) :stream stream)
    (unless (zerop (url-port url))
      (write-string ":" stream) (write (url-port url) :stream stream))
    (assert (or (zerop (length (url-path url)))
                (eq #\/ (aref (url-path url) 0)))
            ((url-path url))
            "non-absolute path in url: `~a'" (url-path url))
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
(defcustom *url-guess-protocol* list
  '(("www" . :http) ("web" . :http) ("ftp" . :ftp) ("news" . :news)
    ("nntp" . :nntp))
  "*The alist of (\"string\" . protocol) to guess the protocol from the host.")
(defmethod url ((xx string))
  (let* ((string (coerce (string-trim +whitespace+ xx) 'simple-string))
         (idx (position #\: string :test #'char=)) (start 0)
         idx0 (url (make-url)))
    (declare (simple-string string) (type index-t start)
             (type (or null index-t) idx))
    (when idx
      (setf (url-prot url) (kwd (string-upcase (subseq string 0 idx))))
      (setq start (position #\/ string :start (1+ idx) :test #'char/=)))
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
    (unless (url-prot url)
      (cond ((let ((pa (assoc (url-host url) *url-guess-protocol* :test
                              (lambda (ho st)
                                (declare (simple-string ho st))
                                (string-equal ho st :end1 (length st))))))
               (when pa (setf (url-prot url) (cdr pa)))))
            ((position #\@ xx) (setf (url-prot url) :mailto))
            ((error "url: `~a': no protocol specified" xx))))
    url))
(defmethod url ((xx symbol)) (unintern xx) (url (symbol-name xx)))
(defmethod url ((xx stream))
  (url (with-output-to-string (st)
         (do (zz) ((not (url-constituent (setq zz (read-char xx)))) st)
           (write zz :stream st)))))

;;;
;;; }}}{{{ Sockets
;;;

#+allegro (deftype socket () 'excl::socket-stream)
#+cmu (deftype socket () 'system:fd-stream)
#+clisp (deftype socket () 'stream)

(defun open-socket (host port &optional bin)
  "Open a socket connection to HOST at PORT."
  (declare (simple-string host) (fixnum port))
  #+cmu (system:make-fd-stream (ext:connect-to-inet-socket host port)
                               :input t :output t)
  #+clisp (lisp:socket-connect port host)
  #+allegro (socket:make-socket :remote-host host :remote-port port
                                :format (if bin :binary :text)))

(defun open-socket-server (sock)
  "Open a `generic' socket server."
  (declare (ignorable sock) (type socket sock))
  #+clisp (lisp:socket-server sock)
  #+allegro (socket:make-socket :connect :passive))

(defun throw-timeout (&rest args)
  "Throw timeout."
  (apply #'format *error-output* args)
  (throw 'timeout nil))

(defcustom *url-default-sleep* (real 0) 20
  "*The number of seconds to sleep when necessary.")
(defcustom *url-default-timeout* (real 0) 86400
  "*The default timeout, in seconds.")

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
  #+allegro
  `(mp:with-timeout (,seconds ,@timeout-forms) ,@body)
  #-allegro
  `(progn ,@body))

(defun y-or-n-p-timeout (seconds default &rest args)
  "`y-or-n-p' with timeout."
  (with-timeout (seconds (format t "[Timed out] ~:[NO~;YES~]~%" default)
                         default)
    (apply #'y-or-n-p args)))

(defun open-socket-retry (host port &key (err *standard-output*) bin
                          (sleep *url-default-sleep*) max-retry
                          (timeout *url-default-timeout*))
  "Open a socket connection, retrying until success."
  (declare (simple-string host) (fixnum port) (type (or null stream) err)
           (type (or null index-t) max-retry) (type (real 0) sleep timeout))
  (loop :with begt = (get-universal-time) :for ii :upfrom 1 :and sock =
        (multiple-value-bind (sk cond)
            (ignore-errors
              (when err
                (format err "~&Connecting to ~a:~d [timeout ~:d sec]..."
                        host port timeout))
              (with-timeout (timeout
                             (values nil (format nil "timed out [~:d sec]"
                                                 timeout)))
                (open-socket host port bin)))
          (when (and err (null sk))
            (format err "~%Error connecting: ~a~%" cond))
          sk)
        :when (and err sock) :do (format err "done: ~a~%" sock)
        :when (and sock (open-stream-p sock)) :return sock
        :when (and max-retry (> ii max-retry)) :return nil
        :when (>= (- (get-universal-time) begt) timeout)
        :do (throw-timeout "open-socket-retry (~a:~d): timeout (~d sec)~%"
                           host port timeout)
        :do (sleep-mesg sleep err "Error")
        (format err "[~d~@[/~d~]] trying to connect to ~a:~d...~%"
                ii max-retry host port)))

(defun open-url (url &key (err *standard-output*) (sleep *url-default-sleep*)
                 (timeout *url-default-timeout*))
  "Open a socket connection to the URL.
Issue the appropriate initial commands:
 if this is an HTTP URL, also issue the GET command;
 if this is an FTP URL, login and cwd;
 if this is a NEWS/NNTP URL, set group and possibly request article;
 if this is a WHOIS/FINGER URL, ask about the host/user.
If timeout is non-nil, it specifies the number of seconds before
the tag `timeout' is thrown."
  (declare (type url url) (type (real 0) sleep timeout)
           (type (or null stream) err))
  (loop :with begt = (get-universal-time)
        :for sock = (open-socket-retry (url-get-host url) (url-get-port url)
                                       :err err :sleep sleep :timeout timeout)
        :when
        (handler-case
            (with-timeout ((or timeout *url-default-timeout*) nil)
              (ecase (url-prot url)
                (:http (setq sock (url-open-http sock url err)))
                (:ftp (url-ask sock err 220)
                      (url-login-ftp sock url err))
                (:telnet (dolist (word (split-string (url-path url) "/") t)
                           (format sock "~a~%" word)))
                ((:whois :finger :cfinger)
                 (format sock "~a~%" (url-path-file url)) t)
                (:mailto (url-ask sock err 220))
                ((:news :nntp)
                 (url-ask sock err 200)
                 (url-ask sock err 211 "group ~a" (url-host url))
                 (unless (zerop (length (url-path url)))
                   (url-ask sock err 220 "article ~a"
                            (subseq (url-path url) 1)))
                 t)))
          (error (co)
            (when err (format err "Connection to `~a' dropped: `~a'~%"
                              url co))))
        :return sock :when sock :do (close sock)
        :when (> (- (get-universal-time) begt) timeout)
        :do (throw-timeout "open-url (~a): timeout (~d sec)~%" url timeout)
        :do (sleep-mesg sleep err "Connection dropped")
        (format err "Trying to connect to `~a'...~%" url)))

(defmacro with-open-url ((socket url &key (rt '*readtable*) err
                                 (timeout '*url-default-timeout*))
                         &body body)
  "Execute BODY, binding SOCK to the socket corresponding to the URL.
The *readtable* is temporarily set to RT (defaults to *readtable*).
ERR is the stream for information messages or NIL for none."
  (let ((rt-old (gensym "WOU")) (uuu (gensym "WOU")))
    `(let* ((,uuu ,url) (,socket (open-url ,uuu :err ,err :timeout ,timeout))
            (,rt-old *readtable*))
      (declare (type url ,uuu) (type socket ,socket))
      (unwind-protect (progn (setq *readtable* (or ,rt *readtable*)) ,@body)
        (setq *readtable* ,rt-old)
        (case (url-prot ,uuu)
          ((:ftp :mailto) (url-ask ,socket ,err 221 "quit"))
          ((:news :nntp) (url-ask ,socket ,err 205 "quit")))
        (close ,socket)))))

(defun url-open-http (sock url err)
  "Open the socket to the HTTP url."
  (declare (type socket sock) (type url url) (type (or null stream) err))
  (format sock "GET ~a HTTP/1.0~%~%" (url-path url))
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
        (format sk "GET ~a HTTP/1.0~%~%" (url-path sym))))))

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
            (throw-timeout "Cannot create data connection: ~a~%" st))
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

(deftype file-size-t () '(unsigned-byte 32))

(defun ftp-get-file (sock rmt loc &key (log *standard-output*) (reget t)
                     (bin t) (retry 2) (timeout *url-default-timeout*))
  "Get the remote file RMT from the FTP socket SOCK,
writing it into the local directory LOC.  Log to LOG.
Append if the file exists and REGET is non-nil.
Use binary mode if BIN is non-nil (default).
Retry (+ 1 RETRY) times if the file length doesn't match the expected."
  (declare (type socket sock) (type index-t retry) (type (or null stream) log)
           (simple-string rmt) (values file-size-t double-float simple-string))
  (let* ((data (ftp-get-passive-socket sock log t timeout)) (tot 0)
         (bt (get-float-time nil)) (path (merge-pathnames rmt loc))
         (rest (when (and reget (probe-file path))
                 (let ((sz (file-size path)))
                   (when log (format log "File `~a' exists (~:d bytes), ~
~:[appending~;overwriting~]...~%" path sz (zerop sz)))
                   (unless (zerop sz)
                     (url-ask sock log 350 "rest ~d" sz)
                     sz))))
         (line (progn (url-ask sock log 200 "type ~:[a~;i~]" bin)
                      (url-ask sock log 150 "retr ~a" rmt)))
         (pos (position #\( line :from-end t))
         (len (when pos (read-from-string line nil nil :start (1+ pos)))))
    (declare (type socket data) (type file-size-t tot)
             (double-float bt) (type (or null file-size-t) rest len))
    ;; (when rest (decf len rest))
    (when log
      (if len (format log "Expect ~:d dot~:p for ~:d bytes~%"
                      (ceiling (1+ len) (length *buffer*)) len)
          (format log "File lenth unknown.~%")))
    (with-open-file (fl path :direction :output :element-type 'unsigned-byte
                        :if-exists (if rest :append :supersede))
      (loop :for pos = (#+clisp lisp:read-byte-sequence #-clisp read-sequence
                                *buffer* data)
            :do (write-sequence *buffer* fl :end pos) (incf tot pos)
            :when log :do (princ "." log) (force-output log)
            :while (= pos (length *buffer*))))
    (when log (terpri log))
    (url-ask sock log 226)
    (cond ((or (null len) (= tot len))
           (multiple-value-call #'values tot (elapsed bt nil t)))
          ((plusp retry)
           (when log
             (format log "### Wrong file length: ~:d (expected: ~:d [~@:d]) ###
 +++ ~r more attempt~:p +++~%" tot len (- tot len) retry))
           (ftp-get-file sock rmt loc :log log :reget nil :bin bin
                         :retry (1- retry) :timeout timeout))
          ((error "Wrong file length: ~:d (expected: ~:d [~@:d])"
                  tot len (- tot len))))))

(defun url-ftp-get (url loc &rest opts &key (log *standard-output*)
                    (timeout *url-default-timeout*) &allow-other-keys)
  "Get the file specified by the URL, writing it into a local file.
The local file is located in directory LOC and has the same name
as the remote one."
  (declare (type url url) (type (or null stream) log))
  (format t "~& *** getting `~a'...~%" url)
  (with-open-url (sock url :err log :timeout timeout)
    (multiple-value-bind (tot el st)
        (apply #'ftp-get-file sock (url-path-file url) loc opts)
      (format t " *** done [~:d bytes, ~a, ~:d bytes/sec]~%" tot st
              (round tot el)))))

(defun ftp-list (sock &key (out *standard-output*)
                 (timeout *url-default-timeout*))
  "Get the file list."
  (declare (type socket sock) (type (or null stream) out))
  (let ((data (ftp-get-passive-socket sock t nil timeout)))
    (url-ask sock out 150 "list")
    (loop :for line = (read-line data nil nil) :while line :when out :do
          (format out "~a~%" (string-right-trim +whitespace+ line)))
    (url-ask sock out 226)))

;;; Mail

(defcustom *mail-host-address* simple-string
  (let ((st (machine-instance))) (subseq st 0 (position #\Space st)))
  "*Name of this machine, for purposes of naming users.")
(defcustom *user-mail-address* simple-string
  (concatenate 'string (getenv "USER") "@" *mail-host-address*)
  "*Full mailing address of this user.
This is initialized based on `mail-host-address'.")

(defun url-send-mail (url &key (out *standard-output*)
                      (text (current-time nil))
                      (helo *mail-host-address*)
                      (from *user-mail-address*))
  "Send TEXT to URL (which should be a MAILTO)."
  (declare (type url url) (type (or null stream) out)
           (simple-string text helo from))
  (assert (eq :mailto (url-prot url)) (url)
          "url-send-mail: `~a' is not a `mailto'" url)
  (with-open-url (sock url :err out)
    (url-ask sock out 250 "helo ~a" helo)
    (url-ask sock out 250 "mail from: ~a" from)
    (url-ask sock out 250 "rcpt to: ~a" (url-user url))
    (url-ask sock out 354 "data")
    (url-ask sock out 250 "~a~%." text)))

;;; News

(defstruct (article)
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
      (format out "~d~c~a~c~a~c~a~c~a~c~d~c~d~cXref:{ ~a}"
              (article-numb art) #\Tab (article-subj art) #\Tab
              (article-auth art) #\Tab (dttm->string (article-dttm art)) #\Tab
              (article-msid art) #\Tab (article-bytes art) #\Tab
              (article-lines art) #\Tab (article-xref art))))

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
  (declare (type socket sock) (stream out))
  (loop :for st :of-type simple-string = (read-trim sock)
        :until (string= "." st) :do (format out "~a~%" st)
        :when collect :collect st))

(defun url-get-news (url &optional (out *standard-output*))
  "Get the news article to the OUT stream."
  (declare (type url url) (stream out))
  (assert (or (eq :nntp (url-prot url)) (eq :news (url-prot url))) (url)
          "url-get-news: `~a' is not a `news'" url)
  (with-open-url (sock url :err out)
    (if (zerop (length (url-path url)))
        (let ((st (url-ask sock nil 211 "group ~a" (url-host url))))
          (multiple-value-bind (na p0) (read-from-string st nil -1 :start 3)
            (multiple-value-bind (a1 p1) (read-from-string st nil -1 :start p0)
              (let ((a2 (read-from-string st nil -1 :start p1)))
                (format out "~:d articles, from ~:d to ~:d~%" na a1 a2)
                (url-ask sock out 224 "xover ~d-~d" a1 a2)
                (url-dump-to-dot sock :out out :collect t)))))
        (url-dump-to-dot sock :out out))))

;;;
;;; }}}{{{ HTML parsing
;;;

(defstruct (text-stream (:conc-name ts-))
  "Text stream - to read a tream of text - skipping `:'."
  (sock nil)                    ; socket to read from
  (buff "" :type simple-string) ; buffer string
  (posn 0 :type fixnum))        ; position in the buffer

(defun read-next (ts &optional errorp)
  "Read the next something from TS - a text stream."
  (do (str tok pos) (nil)
    (declare (type (or null simple-string) str))
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

;;(defun read-next (ts) (read (ts-sock ts) nil +eof+))

(defun next-token (ts &optional (num 1) type dflt)
  "Get the next NUM-th non-tag token from the HTML stream TS."
  (declare (type text-stream ts) (type index-t num))
  (let (tt)
    (dotimes (ii num (if (and type (not (typep tt type))) dflt tt))
      (declare (type index-t ii))
      (do () ((not (html-tag-p (setq tt (read-next ts t)))))))))

(defun next-number (ts &optional (num 1))
  "Get the next NUM-th number from the HTML stream TS."
  (declare (type text-stream ts) (type index-t num))
  (let (tt)
    (dotimes (ii num tt)
      (declare (type index-t ii))
      (do () ((numberp (setq tt (next-token ts))))))))

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

(defun dump-url (url &key (fmt "~3d: ~a~%") (out *standard-output*)
                 (timeout *url-default-timeout*))
  "Dump the URL line by line.
FMT is the printing format. 2 args are given: line number and the line
itself. FMT defaults to \"~3d: ~a~%\".
OUT is the output stream and defaults to T."
  (declare (stream out))
  (format out "Opening URL: `~a'...~%" (setq url (url url)))
  (catch 'timeout
    (with-open-url (sock url :err *standard-output* :timeout timeout)
      (loop :for ii :of-type index-t :from  1
            :and rr = (read-line sock nil +eof+) :until (eq +eof+ rr)
            :do (format out fmt ii (string-right-trim +whitespace+ rr))))))

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

(defun dump-url-tokens (url &key (fmt "~3d: ~a~%") (out *standard-output*))
  "Dump the URL token by token.
See `dump-url' about the optional parameters."
  (declare (stream out))
  (setq url (url url))
  (with-open-url (sock url :rt *html-readtable* :err *standard-output*)
    (do (rr (ii 0 (1+ ii)) (ts (make-text-stream :sock sock)))
        ((eq +eof+ (setq rr (read-next ts))))
      (declare (type index-t ii))
      (format out fmt ii rr))))

;;;
;;; HyperSpec examples
;;;

(defcustom *html-specials* list
  '(("&gt;" . #\>) ("&lt;" . #\<) ("&quot;" . #\") ("&amp;" . #\&))
  "Alist of translations of HTML specials like `&*'.")

(defun html-translate-specials (str)
  "Replace (non-destructively) HTML specals with their interpretations.
HTML tags, surrounded by `<>', are removed."
  (declare (string str))
  (do ((beg 0 (1+ beg)) res (len (length str)))
      ((>= beg len) (coerce (nreverse res) 'string))
    (declare (type index-t beg len))
    (case (char str beg)
      (#\< (setq beg (or (position #\> str :start beg) len)))
      (#\&
       (let ((pa (assoc str *html-specials* :test
                        (lambda (str tag)
                          (let ((end (+ beg (length tag))))
                            (and (>= len end)
                                 (string= str tag :start1 beg
                                          :end1 end)))))))
         (incf beg (1- (length (car pa))))
         (push (cdr pa) res)))
      (t (push (char str beg) res)))))

(defcustom *hyperspec-root* pathname
  #p"/usr/doc/lisp/HyperSpec/"
  "The root of the HyperSpec tree.")

(defun hyperspec-snarf-examples (&key (root *hyperspec-root*)
                                 (out *standard-output*))
  "Get the examples from the HyperSpec."
  (declare (pathname root) (stream out))
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
