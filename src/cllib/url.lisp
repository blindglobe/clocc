;;; File: <url.lisp - 1998-03-10 Tue 13:31:36 EST sds@mute.eaglets.com>
;;;
;;; Url.lisp - handle url's and parse HTTP
;;;
;;; $Id: url.lisp,v 1.1 1998/03/10 18:31:44 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/url.lisp,v $
;;; $Log: url.lisp,v $
;;; Revision 1.1  1998/03/10 18:31:44  sds
;;; Initial revision
;;;

(eval-when (load compile eval) (sds-require 'util))

;;;
;;; {{{ HTML parsing
;;;

;(setq *read-eval* nil *read-suppress* t) ; for parsing
;(setq *read-eval* t *read-suppress* nil) ; original

(defvar *html-readtable* (copy-readtable nil)
  "The readtable for HTML parsing.")
(defvar *html-tag* (list '*html-tag*)
  "*The car of any html tag that is read.")
(defvar *html-parse-tags* nil
  "*If non-nil, parse tags, if nil - return nil for all tags.")
(defvar *whitespace* '#(#\Space #\Newline #\Tab #\Linefeed #\Return #\Page)
  "*The whitespace characters.")

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
      (cons *html-tag* (read-delimited-list #\> stream t))
      (do () ((char= (read-char stream t nil t) #\>)))))

(defun html-tag-p (obj)
  "Return T if the object is an HTML tag."
  (if *html-parse-tags* (and (consp obj) (eq (car obj) *html-tag*))
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

(defstruct (url (:print-function print-url))
  "URL - Uniform Resource Locator: protocol://user#password@host:port/path."
  (prot "" :type string)		; protocol
  (user "" :type string)		; username
  (pass "" :type string)		; password
  (host "" :type string)		; hostname
  (port 0  :type fixnum)		; port number
  (path "/" :type string))		; pathname

(unless (fboundp 'socket-service-port)
  (defun socket-service-port (prot)
    "Return the port number of the protocol."
    (with-open-file (fl #+unix "/etc/services" #+win32
			(concatenate 'string (system:getenv "windir")
				     "/system32/etc/services")
			:direction :input)
      (do ((st (read-line fl nil *eof*) (read-line fl nil *eof*))
	   res pos srv)
	  ((or res (eq st *eof*)) res)
	(unless (or (zerop (length st)) (char= #\# (char st 0)))
	  (setf (values srv pos) (read-from-string st nil ""))
	  (when (string-equal prot srv)
	    (nsubstitute #\space #\/ st)
	    (setq res (read-from-string st nil nil :start pos))))))))

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

(defun print-url (url &optional (stream t) depth)
  "Print the URL in the standard form."
  (declare (ignore depth) (type url url))
  (let ((str (or stream (make-string-output-stream))))
    (princ (url-prot url) str) (princ "://" str)
    (unless (equal "" (url-user url))
      (princ (url-user url) str)
      (unless (equal "" (url-pass url))
	(princ "#" str) (princ (url-pass url) str))
      (princ "@" str))
    (princ (url-host url) str)
    (unless (zerop (url-port url))
      (princ ":" str) (princ (url-port url) str))
    (unless (or (zerop (length (url-path url)))
		(eq #\/ (aref (url-path url) 0)))
      (princ "/" str))
    (princ (url-path url) str)
    (unless stream (get-output-stream-string str))))

(defvar *url-special-chars* "#%&*+,-./:=?@_~"
  "*The string consisting of non-alphanumeric characters allowed in a URL.")

(defun url-constituent (char)
  "Check whether the character can be part of a URL."
  (declare (character char))
  (and (characterp char)
       (or (alphanumericp char)
	   (find char *url-special-chars* :test #'char=))))

(defun read-url (&optional (str t))
  "Read a URL from the stream."
  (parse-url
   (with-output-to-string (st)
     (do (zz) ((not (url-constituent (setq zz (read-char str)))) st)
       (princ zz st)))))

(defun parse-url (string &key (start 0) end)
  "Parse a string into a new URL."
  (declare (string string))
  (setq string (string-trim *whitespace* string))
  (let ((idx (search "://" string :start2 start :end2 end :test #'char=))
	idx0 (url (make-url)))
    (when idx
      (setf (url-prot url) (subseq string start idx))
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

;(defmacro parse-url (string &rest keys)
;  "Read a URL from the string."
;  (let ((st (getsym "url")))
;    `(with-input-from-string (,st ,string ,@keys)
;      (read-url ,st))))

(defun url (url)
  "Coerce the object into URL."
  (cond ((url-p url) url)
	((or (stringp url) (symbolp url)) (parse-url (string url)))
	((error "Cannot infer a URL from `~s'.~%" url))))

(defstruct (text-stream (:conc-name ts-))
  "Text stream - to read a tream of text - skipping `:'."
  (sock nil)				; socket to read from
  (buff "" :type string)		; buffer string
  (posn 0 :type fixnum))		; position in the buffer

(defmacro with-open-url ((socket url &optional (rt '*html-readtable*) err)
			 &body body)
  "Execute BODY, binding SOCK to the socket corresponding to the URL.
The *readtable* is temporarily set to RT (defaults to *html-readtable*).
If this is an HTTP URL, also issue the GET command.
If this is an FTP URL, cd and get (if there is a file part).
ERR is the stream for information messages."
  (let ((rt-old (gensym "WOU")))
    `(let ((,socket (socket-connect (url-get-port ,url) (url-host ,url)))
	   (,rt-old *readtable*))
      (setq *readtable* (or ,rt *readtable*))
      (unwind-protect
	   (progn
	     (cond ((equal "http" (url-prot ,url))
		    (format ,socket "GET ~a HTTP/1.0~%~%" (url-path ,url))
		    (let ((sym (read ,socket)) (res (read ,socket)))
		      (when (string-equal sym "http/1.1")
			(when (>= res 400) ; error
			  (error "~d: ~a~%" res (read-line ,socket)))
			(when (= 302 res) ; redirection
			  (setq res (read-line ,socket))
			  (read-line ,socket) (read-line ,socket)
			  (setq sym (read-line ,socket)
				sym (parse-url (subseq sym (1+ (position
								#\: sym)))))
			  (close ,socket)
			  (format ,err " *** redirected to `~a' [~a]~%"
				  sym res)
			  (setq ,socket (socket-connect (url-get-port sym)
							(url-host sym)))
			  (format ,socket "GET ~a HTTP/1.0~%~%"
				  (url-path sym))))))
		   ((equal "ftp" (url-prot ,url))
		    (format ,socket "cd ~a~%" (url-path-dir ,url))
		    (unless (equal "" (url-path-file ,url))
		      (format "get ~a~%" (url-path-file ,url))))
		   ((equal "telnet" (url-prot ,url))
		    (dolist (word (split-string (url-path ,url) "/"))
		      (format ,socket "~a~%" word)))
		   ((equal "whois" (url-prot ,url))
		    (format ,socket "~a~%" (url-path-file ,url))))
	     ,@body)
	(setq *readtable* ,rt-old)
	(close ,socket)))))

(defun read-next (ts)
  "Read the next something from TS - a text stream."
  (do (str tok pos) (nil)
    (when (or (typep pos 'error) (>= (ts-posn ts) (length (ts-buff ts))))
      (unless (typep pos 'error) (setf (ts-posn ts) 0))
      (setf str (read-line (ts-sock ts) nil *eof*))
      (when (eq str *eof*)
	(if (typep pos 'error) (error pos) (return-from read-next *eof*)))
      (setq str (nsubstitute #\space #\: str)
	    str (nsubstitute #\space #\, str)
	    str (nsubstitute #\space #\/ str))
      ;; (nsubstitute #\space #\. str) breaks floats, so we have to be smart
      (do ((beg -1)) ((null (setq beg (position #\. str :start (1+ beg)))))
	(if (or (digit-char-p (elt str (1- beg)))
		(digit-char-p (elt str (1+ beg))))
	    (incf beg) (setf (elt str beg) #\Space)))
      (setf (ts-buff ts) (if (typep pos 'error)
			     (concatenate 'string (ts-buff ts) str) str)))
    (setf (values tok pos)
	  (ignore-errors
	    (read-from-string (ts-buff ts) nil *eof* :start (ts-posn ts))))
    (unless (typep pos 'error) (setf (ts-posn ts) pos))
    (unless (or (typep pos 'error) (eq tok *eof*))
      (return-from read-next tok))))

;(defun read-next (ts) (read (ts-sock ts) nil *eof*))

(defun next-token (ts &optional (num 1) type dflt)
  "Get the next NUM-th non-tag token from the HTML stream TS."
  (declare (type text-stream ts))
  (let (tt)
    (dotimes (ii num (if (and type (not (typep tt type))) dflt tt))
      (do () ((not (html-tag-p (setq tt (read-next ts)))))))))

(defun next-number (ts &optional (num 1))
  "Get the next NUM-th number from the HTML stream TS."
  (declare (type text-stream ts))
  (let (tt)
    (dotimes (ii num tt) (do () ((numberp (setq tt (next-token ts))))))))

(defun skip-tokens (ts end &key (test #'eql) (key #'identity))
  "Skip tokens until END, i.e., until (test (key token) end) is T."
  (declare (type text-stream ts))
  (do (tt) ((funcall test (setq tt (funcall key (next-token ts))) end) tt)))

(defun dump-url (url &key (fmt "~3d: ~a~%") (out t))
  "Dump the URL line by line.
FMT is the printing format. 2 args are given: line number and the line
itself. FMT defaults to \"~3d: ~a~%\".
OUT is the output stream and defaults to T."
  (setq url (url url))
  (with-open-url (sock url *readtable* t)
    (do (rr (ii 0 (1+ ii)))
	((eq *eof* (setq rr (read-line sock nil *eof*))))
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
  (with-open-url (sock url *html-readtable* t)
    (do (rr (ii 0 (1+ ii)) (ts (make-text-stream :sock sock)))
	((eq *eof* (setq rr (read-next ts))))
      (format out fmt ii rr))))

(provide "url")
;;; url.lisp ends here
