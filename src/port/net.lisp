;;; Network Access
;;;
;;; Copyright (C) 1999 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.
;;;
;;; $Id: net.lisp,v 1.13 2000/04/19 16:39:26 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/port/net.lisp,v $

(eval-when (compile load eval)
  (require :ext (translate-logical-pathname "clocc:src;port;ext"))
  #+lispworks (require "comm"))

(in-package :port)

(export
 '(resolve-host-ipaddr ipaddr-to-dotted dotted-to-ipaddr hostent
   socket open-socket socket-host/port socket-string socket-server
   socket-accept open-socket-server socket-server-close
   socket-service-port socket-server-host/port socket-server-string
   network timeout login))

;;;
;;; {{{ name resulution
;;;

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

(defstruct hostent
  "see gethostbyname(3) for details"
  (name "" :type simple-string) ; canonical name of host
  (aliases nil :type list)      ; alias list
  (addr-list nil :type list)    ; list of addresses
  (addr-type 2 :type fixnum))   ; host address type

(defun resolve-host-ipaddr (host)
  "Call gethostbyname(3) or gethostbyaddr(3)."
  #+allegro
  (let* ((ipaddr
          (etypecase host
            (string
             (if (every (lambda (ch) (or (char= ch #\.) (digit-char-p ch)))
                        host)
                 (socket:dotted-to-ipaddr host) (socket:lookup-hostname host)))
            (integer host)))
         (name (socket:ipaddr-to-hostname ipaddr)))
    (make-hostent :name name :addr-list
                  (list (socket:ipaddr-to-dotted ipaddr))))
  #+(and clisp syscalls)
  (let ((he (posix:resolve-host-ipaddr host)))
    (make-hostent :name (posix::hostent-name he)
                  :aliases (posix::hostent-aliases he)
                  :addr-list (posix::hostent-addr-list he)
                  :addr-type (posix::hostent-addrtype he)))
  #+cmu
  (let ((he (ext:lookup-host-entry host)))
    (make-hostent :name (ext:host-entry-name he)
                  :aliases (ext:host-entry-aliases he)
                  :addr-list (mapcar #'ipaddr-to-dotted
                                     (ext:host-entry-addr-list he))
                  :addr-type (ext::host-entry-addr-type he)))
  #+gcl (make-hostent :name (or (si:hostid-to-hostname host) host)
                      :addr-list (list (si:hostname-to-hostid host)))
  #+lispworks
  (multiple-value-bind (name addr aliases)
      (comm:get-host-entry host :fields '(:name :address :aliases))
    (make-hostent :name name :addr-list (list (ipaddr-to-dotted addr))
                  :aliases aliases))
  #-(or allegro (and clisp syscalls) cmu gcl lispworks)
  (error 'not-implemented :proc (list 'resolve-host-ipaddr host)))

;;;
;;; }}}{{{ sockets
;;;

(deftype socket ()
  #+allegro 'excl::socket-stream
  #+clisp 'stream
  #+cmu 'sys:fd-stream
  #+gcl 'si:socket-stream
  #+lispworks 'comm:socket-stream
  #-(or allegro clisp cmucl gcl lispworks) 'stream)

(defun open-socket (host port &optional bin)
  "Open a socket connection to HOST at PORT."
  (declare (type (or integer string) host) (fixnum port) (type boolean bin))
  (let ((host (etypecase host
                (string host)
                (integer (hostent-name (resolve-host-ipaddr host))))))
    #+allegro (socket:make-socket :remote-host host :remote-port port
                                  :format (if bin :binary :text))
    #+clisp (lisp:socket-connect port host :element-type
                                 (if bin '(unsigned-byte 8) 'character))
    #+cmu (sys:make-fd-stream (ext:connect-to-inet-socket host port)
                              :input t :output t :element-type
                              (if bin '(unsigned-byte 8) 'character))
    #+gcl (si:make-socket-stream host port bin) ; FIXME
    #+lispworks (comm:open-tcp-stream host port :direction :io :element-type
                                      (if bin 'unsigned-byte 'base-char))
    #-(or allegro clisp cmu gcl lispworks)
    (error 'not-implemented :proc (list 'open-socket host port bin))))

(defun socket-host/port (sock)
  "Return the remote host&port, as 2 values."
  (declare (type socket sock))
  #+allegro (values (socket:ipaddr-to-dotted (socket:remote-host sock))
                    (socket:remote-port sock))
  #+clisp (values (lisp:socket-stream-host sock)
                  (lisp:socket-stream-port sock))
  #+cmu (multiple-value-bind (ho po)
            (ext:get-socket-host-and-port (sys:fd-stream-fd sock))
          (values (ipaddr-to-dotted ho) po))
  #+gcl (let ((peer (si:getpeername sock)))
          (values (car peer) (caddr peer)))
  #+lispworks (multiple-value-bind (ho po)
                  (comm:socket-stream-peer-address sock)
                (values (ipaddr-to-dotted ho) po))
  #-(or allegro clisp cmu gcl lispworks)
  (error 'not-implemented :proc (list 'socket-host/port sock)))

(defun socket-string (sock)
  "Print the socket host and port to a string."
  (multiple-value-bind (ho po) (socket-host/port sock)
    (format nil "~s:~d" ho po)))

#+lispworks (defstruct socket-server proc mbox port)
#-lispworks
(deftype socket-server ()
  #+allegro 'acl-socket::socket-stream-internet-passive
  #+clisp 'lisp:socket-server
  #+cmu 'integer
  #+gcl 'si:socket-stream
  #-(or allegro clisp cmucl gcl) t)

(defun open-socket-server (&optional port)
  "Open a `generic' socket server."
  (declare (type (or null integer socket) port) (values socket-server))
  #+allegro (socket:make-socket :connect :passive :local-port
                                (when (integerp port) port))
  #+clisp (lisp:socket-server port)
  #+cmu (ext:create-inet-listener port)
  #+gcl (si:make-socket-pair port) ; FIXME
  #+lispworks (let ((mbox (mp:make-mailbox :size 1)))
                (make-socket-server
                 :mbox mbox :port port
                 :proc (comm:start-up-server-and-mp
                        :function (lambda (sock) (mp:mailbox-send mbox sock))
                        :service port)))
  #-(or allegro clisp cmu gcl lispworks)
  (error 'not-implemented :proc (list 'open-socket-server port)))

(defun socket-accept (serv &optional bin)
  "Accept a connection on a socket server (passive socket)."
  (declare (type socket-server serv) (values socket))
  #+allegro (let ((sock (socket:accept-connection serv :wait t)))
              (socket:set-socket-format sock (if bin :binary :text))
              sock)
  #+clisp (lisp:socket-accept serv :element-type
                              (if bin '(unsigned-byte 8) 'character))
  #+cmu (progn
          (sys:wait-until-fd-usable serv :input)
          (sys:make-fd-stream (ext:accept-tcp-connection serv)
                              :input t :output t :element-type
                              (if bin '(unsigned-byte 8) 'character)))
  #+gcl (si:accept-socket-connection serv bin) ; FIXME
  #+lispworks (make-instance
               'comm:socket-stream :direction :io
               :socket (mp:mailbox-read (socket-server-mbox serv))
               :element-type (if bin 'unsigned-byte 'base-char))
  #-(or allegro clisp cmu gcl lispworks)
  (error 'not-implemented :proc (list 'socket-accept serv bin)))

(defun socket-server-close (server)
  "Close the server."
  (declare (type socket-server server))
  #+allegro (close server)
  #+clisp (lisp:socket-server-close server)
  #+cmu (unix:unix-close server)
  #+lispworks (mp:process-kill (socket-server-proc server))
  #+gcl (close server)
  #-(or allegro clisp cmu gcl lispworks)
  (error 'not-implemented :proc (list 'socket-server-close server)))

(defun socket-server-host/port (server)
  "Return the local host&port on which the server is running, as 2 values."
  (declare (type socket-server server))
  #+allegro (values (socket:ipaddr-to-dotted (socket:local-host server))
                    (socket:local-port server))
  #+clisp (values (lisp:socket-server-host server)
                  (lisp:socket-server-port server))
  #+cmu (values (ipaddr-to-dotted (car (ext:host-entry-addr-list
                                        (ext:lookup-host-entry "localhost"))))
                server)
  #+gcl (let ((sock (si:getsockname server)))
          (values (car sock) (caddr sock)))
  #+lispworks (values (ipaddr-to-dotted (comm:get-host-entry
                                         "localhost" :fields '(:address)))
                      (socket-server-port server))
  #-(or allegro clisp cmu gcl lispworks)
  (error 'not-implemented :proc (list 'socket-server-host/port server)))

(defun socket-server-string (server)
  "Print the socket server host and port to a string."
  (multiple-value-bind (ho po) (socket-server-host/port server)
    (format nil "~s:~d" ho po)))

;;;
;;; }}}{{{ conditions
;;;

(defun report-network-condition (cc out)
  (declare (stream out))
  (format out "[~s] ~s:~d~@[ ~?~]"
          (net-proc cc) (net-host cc) (net-port cc)
          (and (slot-boundp cc 'mesg) (net-mesg cc))
          (and (slot-boundp cc 'args) (net-args cc))))

(define-condition network (error)
  ((proc :type symbol :reader net-proc :initarg :proc)
   (host :type simple-string :reader net-host :initarg :host)
   (port :type (unsigned-byte 16) :reader net-port :initarg :port)
   (mesg :type simple-string :reader net-mesg :initarg :mesg)
   (args :type list :reader net-args :initarg :args))
  (:report report-network-condition))

(define-condition timeout (network)
  ((time :type (real 0) :reader timeout-time :initarg :time))
  (:report (lambda (cc out)
             (declare (stream out))
             (report-network-condition cc out)
             (when (slot-boundp cc 'time)
               (format out " [timeout ~a sec]" (timeout-time cc))))))

(define-condition login (network) ())

;;;
;;; }}}{{{ `socket-service-port'
;;;

(defstruct servent
  "see getservbyname(3) for details"
  (name "" :type simple-string) ; official name of service
  (aliases nil :type list)      ; alias list
  (port -1 :type fixnum)        ; port service resides at
  (proto "tcp" :type simple-string)) ; protocol to use

(defun socket-service-port (&optional service (protocol "tcp"))
  "Return the SERVENT structure corresponding to the SERVICE.
When SERVICE is NIL, return the list of all services."
  (flet ((parse (str)
           (let ((tok (string-tokens
                       (nsubstitute
                        #\Space #\/ (subseq str 0 (or (position #\# str)
                                                      (length str)))))))
             (values (string-downcase (string (first tok)))
                     (mapcar (compose string-downcase string) (cdddr tok))
                     (second tok)
                     (string-downcase (string (third tok))))))
         (mkse (na al po pr)
           (make-servent :name na :aliases al :port po :proto pr)))
    (with-open-file (fl #+unix "/etc/services" #+win32
                        (concatenate 'string (getenv "windir")
                                     "/system32/drivers/etc/services")
                        :direction :input)
      (loop :with name :and alis :and port :and prot
            :for st = (read-line fl nil +eof+)
            :until (eq +eof+ st)
            :unless (or (equal "" st) (char= #\# (schar st 0)))
              :do (setf (values name alis port prot) (parse st)) :and
              :if service
                :when (and (string-equal protocol prot)
                           (or (string-equal service name)
                               (member service alis :test #'string-equal)))
                  :return (mkse name alis port prot) :end
                :else :collect (mkse name alis port prot) :end :end
            :finally (when service
                       (error "~s: service ~s is not found for protocol ~s"
                              'socket-service-port service protocol))))))

;;; }}}

(provide :net)
;;; file net.lisp ends here
