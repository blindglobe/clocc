;;; File: <url.lisp - 2000-03-03 Fri 12:11:39 EST sds@ksp.com>
;;;
;;; Url.lisp - handle url's and parse HTTP
;;;
;;; Copyright (C) 1998-2000 by Sam Steingold.
;;; This is open-source software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and the precise copyright document.
;;;
;;; $Id: url.lisp,v 2.1 2000/03/03 17:21:41 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/url.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `index-t'
  (require :withtype (translate-logical-pathname "cllib:withtype"))
  ;; `kwd', `+kwd+'
  (require :symb (translate-logical-pathname "cllib:symb"))
  ;; `string-beg-with', `split-string'
  (require :string (translate-logical-pathname "cllib:string"))
  ;; `read-trim', `file-size-t', `file-size'
  (require : (translate-logical-pathname "cllib:fileio"))
  ;; `mesg', `elapsed', `get-float-time', `list-format'
  (require :log (translate-logical-pathname "cllib:log"))
  ;; `to-list'
  (require :simple (translate-logical-pathname "cllib:simple"))
  ;; `pr-secs'
  (require :tilsla (translate-logical-pathname "cllib:tilsla"))
  ;; `dttm->string', `string->dttm'
  (require :date (translate-logical-pathname "cllib:date"))
  ;; `run-prog'
  (require :shell (translate-logical-pathname "port:shell"))
  ;; `with-timeout'
  (require :proc (translate-logical-pathname "port:proc"))
  ;; `socket', `network', `socket-service-port', `open-socket'
  (require :net (translate-logical-pathname "port:net")))

(in-package :cllib)

(eval-when (compile load eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

(export '(url url-ask url-eta protocol-rfc
          open-socket-retry open-url with-open-url
          ftp-list url-send-mail url-get-news url-time view-url
          dump-url url-get whois finger))

;;;
;;; {{{ URL handling
;;;

(eval-when (compile load eval)  ; acl compile warning
(defstruct (url #+cmu (:print-function print-struct-object))
  "URL - Uniform Resource Locator: protocol://user#password@host:port/path."
  (prot nil :type symbol)       ; protocol
  (user "" :type simple-string) ; username
  (pass "" :type simple-string) ; password
  (host "" :type simple-string) ; hostname
  (port 0  :type fixnum)        ; port number
  (path "" :type simple-string)) ; pathname
)

(defconst +bad-url+ url (make-url) "*The convenient constant for init.")

(defcustom *rfc-base* (or null string)
  "http://www.cis.ohio-state.edu/htbin/rfc/rfc~d.html"
  "*The format string used to generate the URL in `protocol-rfc'.
When NIL, just the RFC numbers are returned.")

(defun protocol-rfc (protocol)
  "Return the RFC url for the given protocol.
See <http://www.cis.ohio-state.edu/hypertext/information/rfc.html>
<http://www.internic.net/wp>, <ftp://ds.internic.net/rfc> and `*rfc-base*'."
  (let* ((prot (typecase protocol
                 (symbol (if (keywordp protocol) protocol (kwd protocol)))
                 (string (kwd protocol))
                 (url (url-prot protocol))
                 (t (error 'case-error :proc 'protocol-rfc :args
                           (list 'protocol protocol 'symbol 'string 'url)))))
         (rfcs (case prot
                 ((:http :www) '(1945 2068))
                 (:ftp '(959))
                 ((:smtp :mailto) '(821))
                 (:telnet '(1205))
                 (:whois '(954 2167))
                 (:finger '(1288))
                 (:time '(1305))
                 ((:nntp :news) '(977))
                 (t (error 'code :proc 'protocol-rfc :args (list prot)
                           :mesg "Cannot handle protocol ~s")))))
    (maplist (lambda (cc)
               (setf (car cc)
                     (if *rfc-base*
                         (url (format nil *rfc-base* (car cc))) (car cc))))
             rfcs)))

(defun url-get-port (url)
  "Get the correct port of the URL - if the port is not recorded there,
guess from the protocol."
  (declare (type url url))
  (if (zerop (url-port url))
      (flet ((ssp (st) (ignore-errors (nth-value 2 (socket-service-port st)))))
        (or (ssp (string-downcase (string (url-prot url))))
            (ssp (case (url-prot url)
                   (:mailto "smtp") (:news "nntp") (:www "http")))
            (error 'code :proc 'url-get-port :args (list url)
                   :mesg "Cannot guess the port for ~s")))
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

(defmethod print-object ((url url) (out stream))
  "Print the URL in the standard form."
  (when *print-readably* (return-from print-object (call-next-method)))
  (when *print-escape* (write-string "\"" out))
  (let ((*print-escape* nil))
    (write (url-prot url) :stream out :case :downcase)
    (write-string ":" out)
    (unless (or (eq :mailto (url-prot url)) (eq :file (url-prot url))
                (zerop (length (url-host url))))
      (write-string "//" out))
    (unless (zerop (length (url-user url)))
      (write (url-user url) :stream out)
      (unless (zerop (length (url-pass url)))
        (write-string "#" out) (write (url-pass url) :stream out))
      (write-string "@" out))
    (unless (zerop (length (url-host url)))
      (write (url-host url) :stream out)
      (unless (zerop (url-port url))
        (write-string ":" out) (write (url-port url) :stream out)))
    (assert (or (zerop (length (url-path url)))
                (eq :news (url-prot url)) (eq :nntp (url-prot url))
                (char= #\/ (aref (url-path url) 0)))
            ((url-path url))
            "non-absolute path in url: `~a'" (url-path url))
    (when (and (not (zerop (length (url-host url))))
               (or (eq :news (url-prot url)) (eq :nntp (url-prot url)))
               (not (zerop (length (url-path url))))
               (not (char= #\/ (aref (url-path url) 0))))
      (write-string "/" out))
    (write-string (url-path url) out))
  (when *print-escape* (write-string "\"" out)))

(defcustom *url-special-chars* simple-string "#%&*+,-./:=?@_~"
  "*The string consisting of non-alphanumeric characters allowed in a URL.")

(defun url-constituent-p (char)
  "Check whether the character can be part of a URL."
  (declare (character char))
  (and (characterp char)
       (or (alphanumericp char)
           (find char *url-special-chars* :test #'char=))))

(eval-when (compile load eval) (fmakunbound 'url))
(fmakunbound 'url)
;;;###autoload
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
         (idx (position #\: string :test #'char=)) (start 0) idx0
         (end (position #\? string :test #'char=)))
    (declare (simple-string string) (type index-t start) (type url url)
             (type (or null index-t) idx))
    (when (char= #\/ (char string 0))
      (return-from url
        (progn (setf (url-prot url) :file (url-path url) string) url)))
    (when idx
      (setf (url-prot url) (kwd (nstring-upcase (subseq string 0 idx))))
      (setq start (position #\/ string :start (1+ idx) :test #'char/= :end end)
            slashp (/= (1+ idx) start)))
    (setq idx (position #\@ string :start start :test #'char= :end end))
    (when idx
      (setq idx0 (position #\# string :start start :test #'char= :end end))
      (if idx0 (setf (url-pass url) (subseq string (1+ idx0) idx)
                     (url-user url) (subseq string start idx0))
          (setf (url-user url) (subseq string start idx)))
      (setq start (1+ idx)))
    (setq idx (position #\: string :start start :test #'char= :end end))
    (setq idx0 (position #\/ string :start start :test #'char= :end end))
    (when idx
      (setf (url-port url) (parse-integer string :start (1+ idx) :end idx0)))
    (when idx0
      (setf (url-path url) (subseq string idx0)))
    (if (and (not slashp)
             (or (eq :nntp (url-prot url)) (eq :news (url-prot url))))
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
            ((position #\@ string :test #'char= :end end)
             (setf (url-prot url) :mailto))
            ((error "url: `~a': no protocol specified" string))))
    url))

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

(defsubst url-prot-bin (prot)
  "Return T if the protocol is binary."
  (declare (symbol prot))
  (eq :time prot))

(defun open-socket-retry (host port &key (err *standard-output*) bin
                          (sleep *url-default-sleep*)
                          (max-retry *url-default-max-retry*)
                          (timeout *url-default-timeout*))
  "Open a socket connection, retrying until success."
  (declare (simple-string host) (fixnum port) (type (or null stream) err)
           (type (or null index-t) max-retry) (type (real 0) sleep timeout)
           (values socket))
  (loop :with begt = (get-universal-time) :and err-cond
        :for ii :of-type index-t :upfrom 1
        :for sock :of-type (or null socket) =
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
        :when sock :do (mesg :log err "done:~% [~a]~%" sock)
        :when (and sock (open-stream-p sock)) :return sock
        :when (and max-retry (>= ii max-retry))
        :do (error 'network :proc 'open-socket-retry :host host :port port
                   :mesg "max-retry [~a] exceeded~@[~% - last error: ~a~]"
                   :args (list max-retry err-cond))
        :when (>= (- (get-universal-time) begt) timeout)
        :do (error 'timeout :proc 'open-socket-retry :host host :port port
                   :time timeout)
        :do (sleep-mesg sleep err "[open-socket-retry] Error")))

(defun open-url (url &key (err *error-output*) (sleep *url-default-sleep*)
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
  (when (eq :file (url-prot url))
    (return-from open-url (open (url-path url) :direction :input)))
  (loop :with begt = (get-universal-time) :and host = (url-get-host url)
        :and port = (url-get-port url)
        :for sock :of-type socket =
        (open-socket-retry host port :err err :sleep sleep :timeout timeout
                           :bin (url-prot-bin (url-prot url))
                           :max-retry max-retry)
        :when
        (handler-case
            (with-timeout (timeout nil)
              (case (url-prot url)
                ((:http :www) (setq sock (url-open-http sock url err)))
                (:ftp (url-ask sock err :conn)
                      (url-login-ftp sock url err))
                (:telnet (dolist (word (split-string (url-path url) "/") t)
                           (format sock "~a~%" word)))
                ((:whois :finger :cfinger)
                 (format sock "~a~%" (url-path-file url)) t)
                (:mailto (url-ask sock err :conn))
                ((:news :nntp)
                 (url-ask sock err :noop)
                 (unless (zerop (length (url-path url)))
                   (let ((strs (split-string (url-path url) "/")))
                     (url-ask sock err :group "group ~a" (car strs))
                     (when (cadr strs)
                       (url-ask sock err :article "article ~a" (cadr strs)))))
                 t)
                ((:time :daytime) t)
                (t (error 'code :proc 'open-url :args (list (url-prot url))
                          :mesg "Cannot handle protocol ~s"))))
          (code (co) (error co))
          (login (co) (error co))
          (error (co)
            (mesg :err err "Connection to <~a> dropped:~% - ~a~%" url co)))
        :return sock
        :when sock :do (close sock)
        :when (> (- (get-universal-time) begt) timeout)
        :do (error 'timeout :proc 'open-url :host host :port port
                   :time timeout)
        :do (sleep-mesg sleep err "[open-url] Connection dropped")))

(defcustom *url-bytes-transferred* integer 0
  "The number of bytes transferred during the current connection.")
(makunbound '*url-bytes-transferred*)
(defcustom *url-opening-time* double-float 0.0d0
  "The time when the current connection was open.")
(makunbound '*url-opening-time*)

(defmacro with-open-url ((socket url &key (rt '*readtable*) err
                                 (max-retry '*url-default-max-retry*)
                                 (timeout '*url-default-timeout*))
                         &body body)
  "Execute BODY, binding SOCK to the socket corresponding to the URL.
`*readtable*' is temporarily set to RT (defaults to `*readtable*').
ERR is the stream for information messages or NIL for none."
  (with-gensyms ("WOU-" uuu)
    `(let* ((,uuu (url ,url)) (*readtable* ,rt)
            (,socket (open-url ,uuu :err ,err :timeout ,timeout
                               :max-retry ,max-retry))
            (*url-opening-time* (get-float-time nil))
            (*url-bytes-transferred* 0))
      (declare (type url ,uuu) (type socket ,socket))
      (unwind-protect (progn ,@body)
        (case (url-prot ,uuu)
          ((:ftp :mailto) (url-ask ,socket ,err :quit "quit"))
          ((:news :nntp) (url-ask ,socket ,err :nntp-quit "quit")))
        (close ,socket)))))

(defun url-open-http (sock url err)
  "Open the socket to the HTTP url."
  (declare (type socket sock) (type url url) (type (or null stream) err))
  (format sock "GET ~a HTTP/1.0~2%" (url-path url))
  (do ((sk sock) (stat 302) sym res (*package* +kwd+))
      ((not (eql stat 302)) sk)
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
        (close sk) (setq sk (open-url sym :err err))
        (format sk "GET ~a HTTP/1.0~2%" (url-path sym))))))

(defcustom *url-replies* hash-table
  (let ((ht (make-hash-table :test 'eq)))
    (dolist (cc '(((:user) 331 332) ((:pass) 230 332) ((:acct) 230 202)
                  ((:syst) 215) ((:stat) 211 212 213) ((:abor) 225 226)
                  ((:cwd :rnto :dele :rmd) 250) ((:mkd :pwd) 257)
                  ((:cdup :mode :type :stru :port :noop) 200) ((:quit) 221)
                  ((:help) 211 214) ((:smnt) 202 250) ((:rein) 120 220)
                  ((:pasv) 227) ((:allo :site) 200 202) ((:rest :rnfr) 350)
                  ((:stor :list :stou :retr) 125 150 226 250) ((:xover) 224)
                  ((:group) 211) ((:article) 220) ((:nntp-quit) 205)
                  ((:nntp-list) 215) ((:smtp-data) 250 354)
                  ((:smtp-helo :mail-from :rcpt-to) 250))
             ht)
      (dolist (re (car cc)) (setf (gethash re ht) (cdr cc)))))
  "*The table of URL requests and replies, for use in `url-ask'.
See RFC959 (FTP) &c.")

(defun url-ask (sock out end &rest req)
  "Send a request; read the response."
  (declare (type socket sock) (type (or null stream) out)
           (type (or (unsigned-byte 10) symbol list) end))
  (when req
    (apply #'format sock req) (fresh-line sock)
    (when out (apply #'format out "~&url-ask[~s]: `~@?'~%" end req)))
  (loop :with endl :of-type list =
        (typecase end
          (integer (to-list end)) (list end)
          (symbol (gethash end *url-replies*))
          (t (error 'case-error :proc 'url-ask
                    :args (list 'end end 'integer 'list 'symbol))))
        :for ln :of-type simple-string =
        (string-right-trim +whitespace+ (read-line sock))
        :for len :of-type index-t = (length ln)
        :and code :of-type (or null (unsigned-byte 10)) = nil
        :when out :do (format out "~&url-ask[~s]: ~a~%" end ln)
        :while (or (< len 3) (and (> len 3) (char/= #\Space (schar ln 3)))
                   (null (setq code (parse-integer ln :end 3 :junk-allowed t)))
                   (and (< code 400) endl (not (member code endl :test #'=))))
        :finally (if (< code 400) (return (values ln code))
                     (error 'network :proc 'url-ask :host (socket-host sock)
                            :port (socket-port sock) :mesg ln))))

;;;
;;; }}}{{{ ftp
;;;

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

(defun ftp-get-passive-socket (sock err bin timeout)
  "Get a passive socket."
  (declare (type socket sock) (type (or null stream) err) (values socket))
  (multiple-value-call #'open-socket-retry
    (ftp-parse-sextuple (url-ask sock err :pasv "pasv"))
    :err err :max-retry 5 :bin bin :timeout timeout))

(defcustom *ftp-anonymous-passwords* list '("abc@ftp.net" "abc@")
  "*The list of passwords to try with anonymous ftp login.
Some ftp servers do not like `user@host' if `host' is not what they expect.")

(defun url-login-ftp (sock url err)
  "Login and cd to the FTP url."
  (declare (type socket sock) (type url url) (type (or null stream) err))
  (let ((host (socket-host sock)) (port (socket-port sock)) co)
    (dolist (pwd *ftp-anonymous-passwords*
             (error 'login :proc 'url-login-ftp :host host :port port
                    :mesg "All passwords failed: ~{ ~s~}~% -- ~a"
                    :args (list *ftp-anonymous-passwords* co)))
      (url-ask sock err :user "user ~a" (if (zerop (length (url-user url)))
                                            "anonymous" (url-user url)))
      (unless (typep (setq co (nth-value
                               1 (ignore-errors
                                   (url-ask sock err :pass "pass ~a"
                                            (if (zerop (length (url-pass url)))
                                                pwd (url-pass url))))))
                     'error)
        (return)))
    (ignore-errors (url-ask sock err nil "syst")) ; :syst
    ;; (url-ask sock err :type "type i")
    (ignore-errors (url-ask sock err nil "stat")) ; :stat
    (handler-bind ((network (lambda (co)
                              (error 'login :proc 'url-login-ftp :host host
                                     :port port :mesg "CWD error:~% - ~a"
                                     :args (list (port::net-mesg co))))))
      (url-ask sock err :cwd "cwd ~a" (url-path-dir url)))))

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
                     (url-ask sock err :rest "rest ~d" sz)
                     sz))))
         (line (progn (url-ask sock err :type "type ~:[a~;i~]" bin)
                      (url-ask sock err :retr "retr ~a" rmt))) ; 150
         (pos (position #\( line :from-end t))
         (len (when pos (read-from-string line nil nil :start (1+ pos)))))
    (declare (type socket data) (type file-size-t tot) (type pathname path)
             (double-float bt) (type (or null file-size-t) rest len))
    ;; (when rest (decf len rest))
    (if (null len) (mesg :log out "File lenth unknown.~%")
        (mesg :log out "Expect ~:d dot~:p for ~:d bytes~@[ [~/pr-secs/]~]~%"
              (ceiling (1+ len) (length *buffer*)) len (url-eta len)))
    (setq tot (socket-to-file data path :rest rest :out out))
    (url-ask sock err :retr)      ; 226
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
    (url-ask sock err :list "list~@[ ~a~]" name) ; 150
    (loop :for line = (read-line data nil nil) :while line :when out :do
          (format out "~a~%" (string-right-trim +whitespace+ line)))
    (url-ask sock err :list)))  ; 226

;;;
;;; }}}{{{ mail
;;;

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
    (url-ask sock err :smtp-helo "helo ~a" helo) ; 250
    (url-ask sock err :mail-from "mail from: ~a" from) ; 250
    (url-ask sock err :rcpt-to "rcpt to: ~a" (url-user url)) ; 250
    (url-ask sock err :smtp-data "data") ; 354
    (url-ask sock err :smtp-date "~a~%." text))) ; 250

;;;
;;; }}}{{{ news
;;;

(defstruct (article #+cmu (:print-function print-struct-object))
  (numb 0 :type (unsigned-byte 32)) ; article number
  (subj "" :type simple-string) ; subject
  (auth "" :type simple-string) ; author
  (dttm 0 :type (integer 0))    ; date/time
  (msid "" :type simple-string) ; message-ID
  (msid1 "" :type simple-string) ; ????
  (bytes 0 :type file-size-t)   ; size in bytes
  (lines 0 :type index-t)       ; size in lines
  (xref nil :type list))        ; list of cross-references

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
                    (string-tokens (url-ask sock err :group "group ~a"
                                            (car spl)) ; 211
                                   :start 3 :max 3))
                 (let ((nm (format nil "~d-~d" a1 a2)))
                   (format out "~:d articles, from ~:d to ~:d~%" na a1 a2)
                   (url-ask sock err :xover "xover ~a" nm) ; 224
                   (let ((ls (map-in #'string->article
                                     (url-dump-to-dot sock :out (out nm)
                                                      :collect t))))
                     (when re
                       (dolist (art ls)
                         (when (search re (article-subj art))
                           (url-ask sock err :article "article ~d" ; 220
                                    (article-numb art))
                           (url-dump-to-dot sock :out
                                            (out (article-numb art))))))
                     ls))))
              (t               ; not even group, just host
               (url-ask sock err :nntp-list "list active") ; 215
               (url-dump-to-dot sock :out (out "active"))))))))

;;;
;;; }}}{{{ time
;;;

(defcustom *time-servers* list
  '("clock.psu.edu" "black-ice.cc.vt.edu" "clock1.unc.edu" "time-b.nist.gov"
    "time-b.timefreq.bldrdoc.gov" "clock-1.cs.cmu.edu" "ntp0.cornell.edu")
  "Public NTP servers (secondary).
For additional servers see http://www.eecis.udel.edu/~mills/ntp/servers.htm")

;;;###autoload
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
;;; }}}{{{ browse
;;;

(defcustom *browsers* list
  '((netscape "/usr/bin/netscape" "-remote" "openURL(~a,new-window)")
    (emacs-w3 "/usr/bin/gnudoit" "(w3-fetch \"~a\")"))
  "The ALIST of browsers.")

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun whois (host &rest keys)
  "Get the whois information on a host."
  (apply #'dump-url (make-url :prot :whois :host "rs.internic.net"
                              :path (concatenate 'string "/" (string host)))
         :fmt "~*~a~%" keys))

;;;###autoload
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

(provide :url)
;;; }}} url.lisp ends here
