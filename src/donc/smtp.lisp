#| spam filtering esmtp (extended simple mail transport protocol) demon 

Mail to donc@compsvcs.com (,donc@isi.edu, donc@ap5.com)

==== license ====
Copyright (c) 2000 CS3 (see http://www.compsvcs.com/)
All rights reserved.

This program is distributed under the terms of
the GNU Lesser General Public License (LGPL) which can be found at
 http://www.gnu.org/copyleft/lesser.html 
as clarified by the Franz AllegroServe Prequel which can be found at
 http://AllegroServe.sourceforge.net/license-allegroserve.txt 
with the obvious substitutions:
- replace "Franz Inc." with "CS3"
- replace references to "AllegroServe" with "this program" or a name
  for this program.
====

This server is meant to be customized for each site.  The places where
I anticipate you'll want to change things are marked with ***.

This is written with these objectives in mind:
- prevention of various sorts of attacks, which is inherited from the 
shared server in server.lsp on which this code relies.
- a rather aggressive sort of spam prevention.
- relative simplicity so you can understand and alter this code to do
what you really want

Some limitiations:
- See server.lsp for its limitations (only runs in a few lisp implementations)
- The delivery mechanism has only been tested in linux and probably can be
  ported easily to other unix variants
- In order to accept mail for delivery this service must run as root on 
  port 25.
- This is meant to accept mail only for the local machine.  It does not 
  have support for mailing lists.  It does contain internal support for 
  an analog of /etc/aliases.  We do not directly support the .forward file,
  but instead use something a little more primitive.  The whole subject of
  delivery seems ridiculously complicated, but it's discussed near the end.
  The summary is that you can process mail however you want by supplying a
  program to run on messages when they arrive.

Spam prevention: mail is delivered directly only from those you permit to 
send to you.  Mail from someone you have explicitly allowed is expected 
to be the most usual case.  Mail can also be explicitly disallowed, but
this is the unusual case.  The interesting case is mail from an address 
you've never seen before.  This could be either someone you want to talk 
to or spam.  The assumption is that someone who really wants to talk to 
you will be willing to go to a little more trouble than a spammer in order 
to send you mail.  The demon refuses such mail but assigns a one-time 
address that the sender can use in order to send you that mail.  The sender 
then resends the message to that one time address and you get it.  You 
should, if you want to allow that person to send to you without that extra 
trouble, add his address to your allowed list.  To me the most attractive 
part of this implementation is that the spammer actually gets back a message 
from his own mailer telling him that your address was rejected.  (Notice
that you can never actually reply to spam!)  This is due to the fact
that our server is actually talking to the mailer at the other end.
Notice that this does not work so well if that mailer is just forwarding 
the mail to you from another site.

A possible problem to work out: If you have mail forwarded from another 
site and for some reason you reject it, then do you get a message about
the rejection?  In which case it tries to send you that mail and gets
another rejection, etc.
6/2000 -- tried this and had no problem 
It probably depends on the software the forwarding site uses.  
It was sendmail.
Notice that if mail from A@B is sent to C@D and D forwards it to E@F
then as far as the .pw and .ac files below are concerned it comes from
A@B but with the IP address of D.  I guess this is both good and bad.

Each user can have his own set of files which this server uses in order 
to decide what to do.  These files and their contents are described below.
The actual places where these files reside are adjustable, but the 
expectation is that they will be in the user's home directory.

file .smtp.cf - a configuration file containing some of the data below
(;;we do one READ in order to read this file
 ;; this is normal lisp comment syntax
 (:expire 604800)
 ;; how long temporary addresses last (seconds) - default is 7 days
 (:mode :off) ;; :off or :training or :on
 ;; off means always deliver (default)
 ;; on means the behavior described above
 ;; training means always deliver and add addresses to allowed file
 (:possword-limits (32 2)(24 4)(16 8)(8 16)(0 32))
 ;; these are the default values
 ;; This limits the number of entries in the temporary password file 
 ;; After we get, e.g., 2 passwords stored for one IP address or
 ;; 4 for multiple ip addresses that have the same first 24 bits,
 ;; the next one will be shared among all such addresses.
 ;; Currently only these 5 values (0, 8, 16, 24, 32) are used.
 ;; Whenever a password is used it is removed, so if there really are
 ;; 4 people from one host trying to send you mail, then the last two
 ;; will share a password that can only be used by one.  After the 
 ;; first one uses it the second will find that it no longer works,
 ;; and will get another password.  That means that you can get 
 ;; repeated failures if you wait too long.  So it's best to check your
 ;; mail shortly after sending and respond with the password right away.
)

File .smtp.pw
;; This file is written by the server and generally should not be altered
;; by the user.  It contains entries of the form
;; <ip-addr> <sender> <universal-time> <password>

File .smtp.ac
(;; again lisp syntax, one form containing a list
 ;;  of entries of form (ip-addr-list mail-addr-list), as in
 ((1 2 3 T) ("sender" "foo" "com")) ;; ip address, from address,
 ;; or (ip-addr-list mail-addr-list :accept), which means the same thing
 ((1 2 3 T) ("sender" "foo" "com") :accept)
 ;; or (ip-addr-list mail-addr-list :reject) meaning do NOT accept mail
 ((5 6 7 8) ("sender" "foo" "com") :reject) 
 ;; T matches anything 
 ;; mail addresses are compared to userid then the tail of the domain
 ;; so ("sam" "foo" "com") matches sam@bar.baz.foo.com, as does ("sam" "foo" T)
 ;; ip addresses are lists of 4 numbers.  (1 2 3 T) matches, e.g. 1.2.3.35
 ;; user id is case sensitive, but domains are case insensitive
 ;; You should edit this file in order to allow people to send to you.
)

and finally, 
File .smtp.deliver
If this file exists it is expected to be an executable program for 
delivering your mail.  (Details to be added along with a demo)

|#

(in-package :user)

;; why does this give me trouble?
(eval-when (compile eval load) (require :sss "server")) ;; shared socket server

(defpackage "ESMTP" (:use "COMMON-LISP")
	    (:export
	     "*MYDOMAIN*"  ;; string that identifies this domain
	     "*PASSWORD-CHAR*" ;; character described below
	     "*USER-TRANSLATIONS*" ;; alist described below
	     ;; functions you might want to replace
	     "TRANSLATE-USER"
	     "USER-FILE"
	     ))

(in-package :esmtp)

;; debugging
(defmacro ignore-errs (form)
  ;; normally
  ;; `(ignore-errors ,form)
  ;; debugging
  `(progn ,form))

;; Much of this is explained in sss 

(pushnew 25 sss:*ports*) ;; smtp listens on port 25

(defclass smtp-connection (sss:connection)
  ())

(defmethod sss:connection-class ((port (eql 25))) 'smtp-connection)

(defun print-current-time (&optional (stream *standard-output*))
   (multiple-value-bind
    (second minute hour day month year)
    (get-decoded-time)
    (format stream "~@?" "~d/~d/~d ~2,'0d:~2,'0d:~2,'0d"
	    month day year hour minute second)))

(defvar *log* nil) ;; log file [***]
;; If you want to log the inputs, set *log* to the name of a log file
(defun logform (form)
  (when (stringp *log*)
    (with-open-file (log *log* :direction :output
		     :if-does-not-exist :create :if-exists :append)
      (with-standard-io-syntax (print form log)))))

(defvar *mydomain*)
;; I see no general way to set this - have to leave it to user
;; *** after loading this file,
;; (setf esmtp:*mydomain* "compsvcs.com") or whatever

(defvar crlf (format nil "~C~C" #.(code-char 13) #.(code-char 10)))

(defun crlf (con)
  (sss:send-byte-vector con #(13 10)))

(defun crlf+ (con)
  (crlf con)
  (sss:process-output con))

(defmethod sss:accept-p ((connection smtp-connection))
  (let ((complaint (sss:reason-not-to-accept connection)))
    (if complaint
	(progn (sss:send-string
		connection
		(format nil "421-~A Service not available" *mydomain*))
	       (crlf connection)
	       (sss:send-string connection (format nil "421 ~A" complaint))
	       (crlf+ connection)
	       nil)
	(progn (sss:send-string
		connection
		(format nil
			"220 ~A ESMTP Don's anti-spam mail demon - ~A"
			*mydomain* (date)))
	       (crlf+ connection)
	       t))))
;; no shut-down-connection method - just use the default that closes it
;; we have to be sure to send the last message from elsewhere

(defmethod sss:max-input-size ((connection smtp-connection)) 1000000) ;; ***
;; I'll set max-input-size to 10^6 and advertise as a max length
;; something a little less.

;; I'll also use the default on minute limit but have to send a close message
(defmethod sss:connection-close-p ((connection smtp-connection))
  ;; timing suggests that for 10^6 chars we need ~ 2.5 minutes
  (when (> (get-universal-time)
	   (+ 300 (slot-value connection 'sss:creation-time)))
    (sss:send-string connection "451 time limit exceeded")
    (crlf+ connection)
    t))
(defmethod sss:max-input-reached ((connection smtp-connection))
  (sss:send-string connection "552 length limit exceeded")
  (crlf+ connection))

(defvar clrf-dot-crlf (concatenate 'string crlf "." crlf))
(defmethod sss:reader ((c smtp-connection) string start)
  ;; Everything of interest ends in crlf
  (if (boundp 'data)
      ;; too inefficient to do the whole loop for every line of data
      (if (search clrf-dot-crlf string :start1 2
		  :start2 start :end2 (+ start 3))
	  (values "" (+ start 3)) ;; leave off the final dot line
	(let ((pos (search clrf-dot-crlf string :start2 start)))
	  (when pos (values (subseq string start (+ pos 2)) (+ pos 5)))))
    (let ((pos (search crlf string :start2 start)))
      (when pos (values (subseq string start (+ pos 2)) (+ pos 2))))))

#| 


MUST insert a
         "Received:" line at the beginning of a message.  In this line,
         called a "time stamp line" in RFC-821:

         *    The FROM field SHOULD contain both (1) the name of the
              source host as presented in the HELO command and (2) a
              domain literal containing the IP address of the source,
              determined from the TCP connection.

         *    The ID field MAY contain an "@" as suggested in RFC-822,
              but this is not required.

         *    The FOR field MAY contain a list of <path> entries when
              multiple RCPT commands have been given.

         When the receiver-SMTP makes "final delivery" of a message,
         then it MUST pass the MAIL FROM: address from the SMTP envelope
         with the message, for use if an error notification message must
         be sent later (see Section 5.3.3).

         the case of an empty path:  "MAIL FROM: <>" (see RFC-821 Page
         15).  An empty reverse path MUST be supported.

         The syntax for the date is hereby changed to:
            date = 1*2DIGIT month 2*4DIGIT
|#

(defmethod sss:evaler ((c smtp-connection) string)
  ;; string is a line of input ending with crlf
  ;; The most common case (the one we should optimize) is in the middle of data
  (if (boundp 'data) (moredata c string)
      (macrolet ((cmd (cmd)
		   `(and (>= (length string) 4)
			 (string-equal ,cmd string :end2 4))))
      (cond ((cmd "HELO") (helo c string))
	    ((cmd "EHLO") (ehlo c string))
	    ((cmd "MAIL") (mail c string))
	    ((cmd "RCPT") (rcpt c string))
	    ((cmd "DATA") (data c string))
	    ((cmd "SEND") (send c string))
	    ((cmd "SOML") (soml c string))
	    ((cmd "SAML") (saml c string))
	    ((cmd "RSET") (rset c string))
	    ((cmd "VRFY") (vrfy c string))
	    ((cmd "EXPN") (expn c string))
	    ((cmd "HELP") (help c string))
	    ((cmd "NOOP") (noop c string))
	    ((cmd "QUIT") (quit c string))
	    ((cmd "TURN") (turn c string))
	    (t (other c string))))))



(defun other (connection string)
  (declare (ignore string))
  (sss:send-string connection "500 Syntax error, command unrecognized")
  (crlf+ connection))

(defun send (connection string)
  (declare (ignore string))
  (sss:send-string connection "502 Command not implemented")
  (crlf+ connection))

(defun saml (connection string)
  (declare (ignore string))
  (sss:send-string connection "502 Command not implemented")
  (crlf+ connection))

(defun soml (connection string)
  (declare (ignore string))
  (sss:send-string connection "502 Command not implemented")
  (crlf+ connection))

(defun turn (connection string)
  (declare (ignore string))
  (sss:send-string connection "502 Command not implemented")
  (crlf+ connection))

(defun help (connection string)
  (declare (ignore string))
  (sss:send-string connection "214 see rfc 821, 1123, 1869, 1870")
  (crlf+ connection))

#| The state of a connection is held by the following variables:
data - if bound we are collecting data lines by pushing them onto data
domain - the domain name sent in the last helo/ehlo command
domain-ip - the ip address from which the connection came
reverse-path - the path supplied in the mail command
forward-path - list of paths accepted so far in rcpt commands
These are all unbound if there has been no helo/ehlo or if there
has been an helo/ehlo since they were last set
|#

(defun ehlo (c string) (helo c string))

(defun helo (connection string)
  (when (boundp 'domain)
    ;; I want to allow only one helo/ehlo per session so that the
    ;; limit on time/chars will translate properly into length limit
    (sss:send-string connection "421-Service not available")
    (crlf connection)
    (sss:send-string connection
		     "421 only one message per session supported here")
    (crlf+ connection)
    (return-from helo nil))
  (unless (eql #\space (char string 4))
    (sss:send-string connection "501 Syntax error in parameters or arguments")
    (crlf+ connection)
    (return-from helo nil))
  (let ((arg (string-trim " " (subseq string 4 (- (length string) 2)))))
    (when (= 0 (length arg))
      (sss:send-string connection
		       "501 Syntax error in parameters or arguments")
      (crlf+ connection)
      (return-from helo nil))
    (sss:bind 'domain-ip (sss:connection-ipaddr connection))
    ;; would have done that earlier, but have to be in an evaler
    (sss:bind 'domain arg)
    (sss:bind 'forward-path nil)
    (sss:send-string
     connection
     (format nil "250-~A ~A Requested mail action okay, completed"
	     *mydomain* (date)))
    (crlf connection)
    (sss:send-string
     connection
     (format nil "250 SIZE ~A" (- (sss:max-input-size connection) 1000)))
    ;; figure 1000 chars should be enough for all commands
    (crlf+ connection)))

(defun mail (connection string)
  ;; MAIL From:<don@baja.nichimen.com> SIZE=47
  (unless (boundp 'domain)
    (sss:send-string connection "503 you need to start with helo/ehlo")
    (crlf+ connection)
    (return-from mail nil))
  (unless (eql #\space (char string 4))
    (sss:send-string connection "501 Syntax error in parameters or arguments")
    (crlf+ connection)
    (return-from mail nil))
  (let* ((args (string-trim " " (subseq string 4 (- (length string) 2))))
	 (space (position #\space args))
	 (arg1 (subseq args 0 space))
	 (arg2 (when space (string-trim " " (subseq args space)))))
    (when (= 0 (length arg1))
      (sss:send-string connection
		       "501 Syntax error in parameters or arguments")
      (crlf+ connection)
      (return-from mail nil))
    (when arg2 ;; rfc 1870
      (unless (and (>= (length arg2) 5) (string-equal "SIZE=" arg2 :end2 5))
	(sss:send-string connection "504 Command parameter not implemented")
	(crlf+ connection) (return-from mail nil))
      (unless (integerp (setf arg2
			  (ignore-errs
			   (let (*read-eval*)
			     (read-from-string arg2 nil nil :start 5)))))
	(sss:send-string connection
			 "501 Syntax error in parameters or arguments")
	(crlf+ connection) (return-from mail nil))
      (when (> arg2 (- (sss:max-input-size connection) 1000))
	(sss:send-string connection "552 message too long")
	(crlf+ connection) (return-from mail nil)))
    (unless (and (>= (length arg1) 5) (string-equal "FROM:" arg1 :end2 5))
      (sss:send-string connection
		       "501 Syntax error in parameters or arguments")
      (crlf+ connection) (return-from mail nil))
    (sss:bind 'reverse-path (subseq arg1 5))
    (sss:send-string connection "250 ok")
    (crlf+ connection)))

(defvar *PASSWORD-CHAR* #\+) ;; ***
;; this char is not to appear in legitimate user names on your system

(defun rcpt (connection string)
  ;; RCPT To:<root@we-24-130-53-144.we.mediaone.net>
  (declare (special domain-ip reverse-path forward-path))
  (unless (and (boundp 'domain) (boundp 'reverse-path))
    (sss:send-string connection
		     "503 you need to start with helo/ehlo then mail")
    (crlf+ connection)
    (return-from rcpt nil))
  (unless (eql #\space (char string 4))
    (sss:send-string connection "501 Syntax error in parameters or arguments")
    (crlf+ connection)
    (return-from rcpt nil))
  (let ((arg (string-trim " " (subseq string 4 (- (length string) 2))))
	to pos user password cf-data ac-data)
    (when (= 0 (length arg))
      (sss:send-string connection
		       "501 Syntax error in parameters or arguments")
      (crlf+ connection)
      (return-from rcpt nil))
    (unless (and (>= (length arg) 3) (string-equal "TO:<" arg :end2 4))
      (sss:send-string connection
		       "501 Syntax error in parameters or arguments")
      (crlf+ connection) (return-from rcpt nil))
    ;; and now begins the fun ...
    (setf to (subseq arg 4 (position #\@ arg))
	  user (subseq to 0 (setf pos (position *PASSWORD-CHAR* to)))
	  password (and pos (subseq to (1+ pos)))
	  user (translate-user user)
	  cf-data (user-data user :cf)
	  ac-data (user-data user :ac))
    (let ((mode (ignore-errs (cadr (assoc :mode cf-data)))) accept t-acc err)
      (unless (or (eql mode :on) (eql mode :training)) (setf mode :off))
      (cond ((eql mode :off) (setf accept t))
	    ((eql (multiple-value-setq (t-acc err)
		    (ignore-errs (test-accept ac-data)))
		  :accept)
	     (setf accept t))
	    ((eql t-acc :reject)
	     (sss:send-string
	      connection
	      (format nil "553-It seems that ~A does not want your mail" user))
	     (crlf connection)
	     (setf accept nil))
	    ;; if the accept file causes an error we should 
	    ;; go on and act like the address was not in accept
	    ;; but it would be nice to at least log the problem
	    ((and err
		  (logform (format nil "error in test-accept for ~A: ~A"
				       user err))
		  nil))
	    ((eql mode :training)
	     (add-accept user ac-data domain-ip reverse-path)
	     (setf accept t))
	    (t (multiple-value-setq (accept err)
		 (test-password user password cf-data connection))
	       (when err
		 (logform (format nil "error in test-password for ~A: ~A"
				      user err))
		 (sss:send-string connection (format nil "451 error: ~A" err))
		 (crlf+ connection) (return-from rcpt nil))))
      (when accept
	(unless (member user forward-path :test 'equal)
	  (sss:bind 'forward-path
		    (cons (format nil "<~A~A" user
				  (subseq arg (position #\@ arg)))
			  forward-path))))
      (if accept
	  (sss:send-string connection "250 ok")
	  (sss:send-string connection "553 Sorry"))
      (crlf+ connection))))


;; *** this may be redefined for your system
;; we use these values for file: "cf" "pw" "ac" "deliver"
(defun user-file (user file)
  (if (equal user "root")
      (format nil "/root/.smtp.~A" file)
      (format nil "/home/~A/.smtp.~A" user file)))

;; caches for data read from files 
;; (gethash user table) => (cons write-date data)
(defvar *cf-data* (make-hash-table :test 'equal))
(defvar *pw-data* (make-hash-table :test 'equal))
(defvar *ac-data* (make-hash-table :test 'equal))

(defun user-data (user table)
  ;; table in (:cf :pw :ac)
  (let (file userfile data)
    (ecase table
      (:cf (setf file "cf" table *cf-data*))
      (:pw (setf file "pw" table *pw-data*))
      (:ac (setf file "ac" table *ac-data*)))
    (setf userfile (user-file user file))
    (or (setf data (gethash user table))
	(setf (gethash user table) (setf data (list 0))))
    (if (probe-file userfile)
	(when (> (file-write-date userfile) (car data))
	  (ignore-errs
	   (with-open-file (x userfile)
	     (setf data (cons (file-write-date userfile)
			      (let (*read-eval*) (read x)))
		   (gethash user table) data))))
	(setf (gethash user table) (setf data (list 0))))
    (cdr data)))

(defun list-bytes (unsigned32)
  (loop for i from 3 downto 0 collect
	(ldb (byte 8 (* 8 i)) unsigned32)))

(defun parse-addr (string)
  ;; expect <user@foo.bar.baz>
  (let ((at (position #\@ string)) dot ans (end (1- (length string)))) ;; >
    (when at
      (loop while
	    (setf dot (position #\. string :start at :end end :from-end t)) do
	    (push (subseq string (1+ dot) end) ans)
	    (setf end dot))
      (push (subseq string (1+ at) end) ans))
    (push (subseq string 1 at) ans) ;; <
    ans))

(defun add-accept (user data domain-ip reverse-path)
  (let ((newac
	 (append data
		 (list (list (list-bytes domain-ip)
			     (parse-addr reverse-path))))))
    (with-open-file (f (user-file user "ac") :direction :output
		     :if-does-not-exist :create :if-exists :supersede)
      ;; we'll refresh it on next use
      (print newac f))))

(defun test-accept (data)
  ;; return :accept, :reject or nil to go on to the password check
  (declare (special domain domain-ip reverse-path))
  (loop for entry in data do
	(if (and (match-ip (car entry) (list-bytes domain-ip))
		 (match-from (cadr entry) (parse-addr reverse-path)))
	    (if (eql (caddr entry) :reject)
		(return-from test-accept :reject)
		(return-from test-accept :accept)))))

(defun match-ip (pattern-ip real-ip)
  (loop for p in pattern-ip as i in real-ip always
	(or (eql p t) (= p i))))
  
(defun match-from (pattern-addr addr)
  (and (or (eq (car pattern-addr) t)
	   (string= (car pattern-addr) (car addr)))
       (>= (length addr) (length pattern-addr))
       (loop for x in (reverse (cdr pattern-addr))
	   as y in (reverse (cdr addr))
	   always (or (eq x t) (string-equal x y)))))

(defun test-password (user password cf-data con)
  ;; In this case we return t/nil possibly after writing a new password 
  ;; file and sending stuff to the connection
  (declare (special domain domain-ip reverse-path))
  (let* (changed
	 (ip-bytes (list-bytes domain-ip))
	 (from (parse-addr reverse-path))
	 (pw-data (user-data user :pw))
	 (matchn (make-array 5 :initial-element 0))
	 ;; number of entries with same first n bytes
	 (now (get-universal-time))
	 (expire 
	  (let ((assoc (ignore-errs (cadr (assoc :expire cf-data)))))
	    (if (numberp assoc) assoc (* 7 24 2600))))) ;; default 1 week
    ;; validate the data even though nobody else should write it
    (let ((len (length pw-data)))
      (setf pw-data
	(loop for d in pw-data
	    when (and (listp d) (= (length d) 4)
		      (listp (car d)) (= (length (car d)) 4)
		      (loop for x in (car d) always (or (numberp x) (eq x t)))
		      (listp (cadr d))
		      (loop for x in (cadr d) always (or (stringp x) (eq x t)))
		      (numberp (third d)) (> (third d) now)
		      (stringp (fourth d)))
	    collect d))
      (unless (= (length pw-data) len) (setf changed t)))
    ;; first look for exact matches
    (loop for d in pw-data do
	(unless (or (member t (car d)) (member t (cadr d)))
	  (let ((nbytes (loop for x in (car d) as y in ip-bytes
			    while (= x y) count t)))
	    (when (and (= nbytes 4) (equal (cadr d) from))
	      (cond ((equal password (fourth d))
		     (write-pw (remove d pw-data) user)
		     (sss:send-string con "250-one time password accepted")
		     (crlf con)
		     (return-from test-password t))
		    (t
		     (sss:send-string
		      con "553-You've already been told what to do")
		     (crlf con)
		     (sss:send-string
		      con
		      (format nil "553-Send this mail to ~A~C~A"
			      user *password-char* (fourth d)))
		     (crlf con)
		     (when changed (write-pw pw-data user))
		     (return-from test-password nil)))
	    (incf (aref matchn nbytes))))))
    ;; now inexact matches
    (loop for d in pw-data
	when (or (member t (car d)) (member t (cadr d)))
	when (and (match-ip (car d) ip-bytes)
		  (match-from (cadr d) from))
	do (cond ((equal password (fourth d))
		  (write-pw (remove d pw-data) user)
		  (sss:send-string con "250-one time password accepted")
		  (crlf con)
		  (return-from test-password t))
		 (t ;; we don't know whether this person already got the pw
		  (setf (third d) (+ now expire))
		  (write-pw pw-data user)
		  (report-pw user d con)
		  (return-from test-password nil))))
    ;; no matches
    (let ((limits (make-array 5 :initial-element 0)))
      (let ((cflimits (cdr (assoc :password-limits cf-data))))
	(loop for i below 5 as nb in '(0 8 16 24 32)
	    as default in '(2 4 8 16 32) do
	      (setf (aref limits i)
		(let ((l (ignore-errs (cadr (assoc nb cflimits :test '=)))))
		  (if (numberp l) l default)))))
      (loop for i below 5 when (>= (aref matchn i) (aref limits i)) do
	    (push (list
		   (loop for b in ip-bytes as j below 4 collect
			 (if (< j i) b t))
		   (list t) ;; user will always be wild
		   (+ now expire)
		   (new-password))
		  pw-data)
	    (write-pw pw-data user)
	    (report-pw user (car pw-data) con)
	    (return-from test-password nil))
      ;; we can afford to give this guy his own password
      (push (list ip-bytes from (+ now expire) (new-password)) pw-data)
      (write-pw pw-data user)
      (report-pw user (car pw-data) con)
      (return-from test-password nil))))

(defun report-pw (user data con)
  ;; data is the password entry - ip list, return-path list, expire, password
  (sss:send-string con "553- ") (crlf con)
  (sss:send-string
   con (format nil "553-  This is the spam filter at ~A" *mydomain*))
  (crlf con)
  (sss:send-string
   con (format nil "553-  If the stuff below doesn't make sense to you,"))
  (crlf con)
  (sss:send-string
   con (format nil "553-  see http://www.ap5.com/~~donc/spamfilter.html"))
  (crlf con)
  (sss:send-string
   con
   (format nil
	   "553-  You're not on the list of senders from whom ~A accepts mail"
	   user))
  (crlf con)
  (sss:send-string
   con
   (format nil "553-  However, you can send ONE message to ~A by using the"
	   user))
  (crlf con)
  (sss:send-string
   con
   (format nil "553-  address ~A~C~A instead of ~A."
	   user *PASSWORD-CHAR* (fourth data) user))
  (crlf con)
  (when (or (member t (car data)) (member t (cadr data)))
    (loop for m in
	  '("553- "
	    "553-  Due to a large number of messages from addresses like yours,"
	    "553-  this password may be shared with others."
	    "553-  In that case, the first to use it will succeed and the rest"
	    "553-  will get another annoying message like this."
	    "553- ")
	  do (sss:send-string con m) (crlf con)))
  (sss:send-string
   con (format nil "553-  This one time address will expire at ~A"
	       (date (third data))))
  (crlf con)
  (sss:send-string
   con "553-  If you are not sending spam you might include in your")
  (crlf con)
  (sss:send-string
   con "553-  next message a request that you be added to the list of")
  (crlf con)
  (sss:send-string con "553-  acceptable addresses.")
  (crlf con) (sss:send-string con "553- ") (crlf con))

(defun write-pw (data user)
  (with-open-file (f (user-file user "pw") :direction :output
		   :if-does-not-exist :create :if-exists :supersede)
    (print data f)))

;; rfc821 except that rfc1123 says no 2 digit years
(defvar months '("JAN" "FEB" "MAR" "APR" "MAY" "JUN"
		 "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))

(defun date (&optional (ut (get-universal-time)))
  (multiple-value-bind
      (second minute hour day month year)
      (decode-universal-time ut 0)
    (format nil "~a ~a ~a ~2,'0d:~2,'0d:~2,'0d UT"
	    day (nth (1- month) months) year hour minute second)))

(defun new-password ()
  (format nil "~A" (random 999999)))

(defvar *user-translations* ;; ***
    ;; rfc 1123 says we MUST support mailbox "Postmaster"
    ;; you may, of course, wish to add others
    '(("Postmaster" . "root")))
;; Note that I don't promise to deliver to Postmaster - then you could fill
;; my disk with mail to Postmaster.  I treat it just like any other mail.
;; hmm - this seems to be the equivalent of /etc/aliases 

;; *** you might also want to change this whole function
;; I now think that we should at least treat *user-translations*
;; as a cache for some file owned by root, and recache it when
;; necessary as we do for the user files. &&&
(defun translate-user (user)
  (or (cdr (assoc user *user-translations* :test 'equal))
      user))

(defun data (c string)
  (declare (ignore string))
  (unless (and (boundp 'domain) (boundp 'reverse-path))
    (sss:send-string c "503 you need to start with helo/ehlo then mail")
    (crlf+ c)
    (return-from data nil))
  (sss:send-string c "354 Start mail input; end with <CRLF>.<CRLF>")
  (crlf+ c)
  (sss:bind 'data nil))

(defun moredata (c string)
  (declare (special data))
  (deliver-msg c string)
  (sss:unbind 'data))

#|     Delivery

Here's what I gather from experimentation.
- procmail converts CR to LF, so CRLF => LFLF
- when I send a message with CRLF in it the CR is deleted
- when I send a message with CR in it not followed by LF it's preserved.
--> therefore these mails must not be going thru procmail ! 
- procmail inserts ">" at the beginnings of lines that would otherwise
  conflict with the mailbox format, e.g., "From "
- if you run procmail as non-root <user> and pass -f <sender>, it inserts
 From <user>
 >From <sender>
(which I guess does no harm)
 
ls -l /usr/bin/procmail
-rwsr-sr-x   1 root     root        54740 Apr 25 11:20 /usr/bin/procmail
which explains why I can do
su eve -c "procmail -f sender -d don" 

I'll start the easy way, using procmail to deliver mail.

I'd like the user to be able to provide a .smtp.deliver file which
controls what's done with the mail.  Here's my solution.  If the user
has a deliver file,
- I find a new file name in which to write the mail and create the file.
- I change the owner of the file to the user and the permissions to only
  allow him to read it.
- I now append the message into that file and close it
- I call his delivery program as follows:

su <user> -c "<deliver> <filename> <sender> <user>" &

Having no delivery program approximates a deliver file like this:

 #! /bin/sh
 cat $1 | procmail -f $2 -Y -d $3
 rm $1

It's only approximate cause
(1) it turns out (on my linux box at least) that this requires users to 
have write permission in the mail directory in order to write lock files,
(2) running as the user, as noted above, gives different "From " lines, 
(3) If there's no deliver program I consider the mail successfully 
delivered if I procmail returns 0.  If there is a deliver program, I write 
the file and call the program as above.  The & means I'll always succeed.  
So even if your delivery program does nothing I consider the mail delivered.
Note that if you create a deliver file that doesn't work at all, e.g.,
is not even executable, you'll just end up with a lot of mail files 
that were meant to be temporary.  It's a matter of configuration (below)
where these files are written.

It turns out that if you replace the procmail line above with this:
 cat $1 | sendmail $3
you can use the .forward file again, e.g., to send to yourself and 
also another address.  For that matter, you can then use procmail too.
Notice that sendmail to a local address does not go through our demon.
The only slight drawback in this case is that the mail appears to be 
"From " you rather than the original sender.  

|#

(defun shell-command (string)
  (#+allegro excl::run-shell-command
   #+clisp lisp:run-shell-command
   #-(or clisp allegro) (error "no implementation yet")
   string))

(defun temp-mail-location (user) ;; ***
  (declare (ignore user))
  ;; you might want this in the user's directory or ...
  "/tmp/mail")

(defun write-mail (stream body)
  (declare (special data))
  ;; send each input line (minus any leading . as required by rfc821)
  ;; without the final crlf and then add the lf
  (loop for d in data do (write-line d stream))
  (let ((start 0) pos) ;; process body
    (loop while (setf pos (search crlf body :start2 start)) do
	  (when (eql (char body start) #\.) (incf start))
	  (loop for i from start below pos do
		(write-char (char body i) stream))
	  (terpri stream)
	  (setf start (+ pos 2)))))

(defun write-temp-mail (user body &aux file)
  ;; can fail if out of space
  ;; returns name of file
  (loop for i from 0 with ut = (get-universal-time) do 
	(when (> i 10) (error "can't find a temporary mail file name!?"))
	(setf file (format nil "~A/~A-~A" (temp-mail-location user) ut i))
	(ensure-directories-exist file)
	(ignore-errors
	 (with-open-file (f file :direction :output
			  :if-does-not-exist :create :if-exists :error)
	   (return nil) ;; leave loop with the file created
	   )))
  (shell-command
   (with-standard-io-syntax ;; no #1= ...
     (format nil "chown ~A ~A; chmod 400 ~A" user file file)))
  (with-open-file (f file :direction :output
		   :if-does-not-exist :error :if-exists :append)
    (write-mail f body))
  file)

(defun deliver-msg (connection body)
  (declare (special domain domain-ip reverse-path forward-path data))
  (setf data (reverse data)) ;; in transmission order
  ;; a few more things required by rfc821 
  (push (format nil "Received: FROM ~A ~A by ~A with ESMTP ; ~A"
		domain (list-bytes domain-ip) *mydomain* (date))
	data)
  (push (format nil "Return-Path: ~A" reverse-path) data)
  (let (shared-file deliver-file user file result err)
    (unwind-protect
	(loop for fp in forward-path do
	      (setf user (subseq fp 1 (position #\@ fp)))
	      (setf deliver-file (user-file user "deliver"))
	      (if (probe-file deliver-file)
		  (progn ;; every such user gets his own copy
		    (setf file (write-temp-mail user body))
		    (shell-command
		     ;; su <user> -c "<deliver> <filename> <sender> <user>" &
		     (format nil "su ~A -c \"~A ~A ~A ~A\" &"
			     user deliver-file file
			     (string-trim "<>" reverse-path) user)))
		(progn
		  (unless shared-file
		    (setf shared-file (write-temp-mail "root" body)))
		  (setf result
		    (shell-command
		     ;;  cat <file> | procmail -f <sender> -Y -d <user>
		     (format nil "cat ~A | /usr/bin/procmail -f ~A -Y -d ~A"
			     shared-file (string-trim "<>" reverse-path)
			     user)))
		  ;; I guess the protocol doesn't allow for the possiblity
		  ;; of succeeding in some deliveries and failing in others
		  (unless (= 0 result)
		    (error (format nil "procmail returned ~A" result))))))
      (when shared-file (delete-file shared-file)))
    (if err
	(sss:send-string connection (format nil "451 aborted: ~A" err))
	(sss:send-string connection "250 ok!"))
    (crlf+ connection)))

(defun rset (c string)
  (declare (ignore string))
  (sss:send-string c "250 ok, start again")
  (crlf+ c)
  (sss:unbind 'domain)
  (sss:unbind 'forward-path)
  (sss:unbind 'reverse-path)
  (sss:unbind 'data))

(defun vrfy (c string)
  (declare (ignore string))
  (sss:send-string c "502 Command not implemented ")
  (crlf+ c))

(defun expn (c string)
  (declare (ignore string))
  (sss:send-string c "502 I don't even support mailing lists ")
  (crlf+ c))

(defun noop (c string)
  (declare (ignore string))
  (sss:send-string c "250 OK, fine.  Be that way.")
  (crlf+ c))

(defun quit (c string)
  (declare (ignore string))
  (sss:send-string c (format nil "221 ~A - good bye" *mydomain*))
  (crlf+ c)
  (sss:done c))


#| For my site:
(load "/d/non-cl-http/server.lsp")
(load "/d/non-cl-http/smtp.lsp")
(setf esmtp:*mydomain* "we-24-130-53-144.we.mediaone.net")
;; (defun esmtp:translate-user (user) "root") ;; all mail to root
(setf sss:*DEBUG* t)
(sss:start-servers)
(sss:serve)

|#
