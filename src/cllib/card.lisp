;;; File: <card.lisp - 1999-04-19 Mon 19:28:24 EDT sds@eho.eaglets.com>
;;;
;;; Personal Database  - Rolodex (BBDB, VCARD)
;;;
;;; Copyright (C) 1999 by Sam Steingold
;;;
;;; $Id: card.lisp,v 1.1 1999/04/19 23:35:05 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/card.lisp,v $
;;; $Log: card.lisp,v $
;;; Revision 1.1  1999/04/19 23:35:05  sds
;;; Initial revision
;;;
;;;

(in-package :cl-user)

(eval-when (load compile eval)
  (sds-require "base") (sds-require "print")
  (sds-require "date") (sds-require "url")
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

;;;
;;; {{{Definitions
;;;

(defclass name ()
  ((first :type simple-string :initarg first :accessor name-first
          :documentation "the first name")
   (ini :type simple-string :initarg ini :accessor name-ini
        :documentation "the middle initial name")
   (last :type simple-string :initarg last :accessor name-last
         :documentation "the last name")
   (prefix :type simple-string :initarg prefix :accessor name-prefix
           :documentation "the prefix (like `PhD')")
   (suffix :type simple-string :initarg sufffix :accessor name-suffix
            :documentation "the suffix (like `VII')")
   (aka :type list :initarg aka :accessor name-aka
        :documentation "the list of aliases"))
  (:documentation "The name - with bells and whistles."))

(defclass phone ()
  ((loc :type simple-string :initarg loc :accessor phone-loc
        :documentation "location")
   (nmb :type simple-string :initarg nmb :accessor phone-nmb
        :documentation "the phone number"))
  (:documentation "The phone - location and number."))

(defclass address ()
  ((loc :type simple-string :initarg loc :accessor address-loc
        :documentation "location")
   (street1 :type simple-string :initarg street1 :accessor address-street1
            :documentation "line one of the street address")
   (street2 :type simple-string :initarg street1 :accessor address-street2
            :documentation "line two of the street address")
   (city :type simple-string :initarg city :accessor address-city
         :documentation "the city")
   (state :type simple-string :initarg state :accessor address-state
          :documentation "the state")
   (zip :type simple-string :initarg zip :accessor address-zip
        :documentation "the zip code")
   (country :initform "USA" :type simple-string :initarg country
            :accessor address-country :documentation "the country"))
  (:documentation "The full mailing address."))

(defclass card ()
  ((lable :type simple-string :initarg lable :accessor card-lable
          :documentation "The unique record ID.")
   (name :type name :initarg name :accessor card-name
         :documentation "The name of the person.")
   (addrl :type list :initform nil :initarg addrl :accessor card-addrl
          :documentation "The list of addresses.")
   (phonel :type list :initform nil :initarg phonel :accessor card-phonel
           :documentation "The list of phones.")
   (emaill :type list :initform nil :initarg emaill :accessor card-emaill
           :documentation "The list of e-mail addresses.")
   (urll :type list :initform nil :initarg urll :accessor card-urll
           :documentation "The list of URLs.")
   (security :type simple-string :initarg security :accessor card-security
             :documentation "PGP or X509 key.")
   (note :type simple-string :initarg note :accessor card-note
         :documentation "Random note not fitting anything else.")
   (org :type simple-string :initarg org :accessor card-org
        :documentation "The organization or company.")
   (title :type simple-string :initarg title :accessor card-title
          :documentation "The title of the person.")
   (tz :type (rational -24 24) :initarg tz :accessor card-tz
       :documentation "The time zone.")
   (created :type (integer 0) :initarg created :accessor card-created
            :documentation "The creation time, in seconds since the epoch.")
   (timestamp :type (integer 0) :initarg timestamp :accessor card-timestamp
              :documentation "The last modification time."))
  (:documentation "The personal information record."))

(defun slot-val (obj slot &optional default)
  (or (when (slot-boundp obj slot) (slot-value obj slot)) default))

(defmethod initialize-instance :after ((cc card) &rest junk)
  (declare (ignore junk))
  (unless (slot-boundp cc 'lable)
    (let ((*card-output-type* +card-output-pretty+))
      (setf (card-lable cc) (format nil "~@[~a ~]<~@[~a~]>" (slot-val cc 'name)
                                    (car (card-emaill cc))))))
  (let ((tm (get-universal-time)))
    (unless (slot-boundp cc 'timestamp) (setf (card-timestamp cc) tm))
    (unless (slot-boundp cc 'created) (setf (card-created cc) tm))))

;;;
;;; }}}{{{Output
;;;

(defcustom *card-output-type* (or null card-output) nil
  "The type of output for CARD.
Valid value are instances of `card-output' or NIL (for #[card ...]).
See constants `+card-output-bbdb+', `+card-output-vcard+',
`+card-output-pretty+'.")

(eval-when (load compile eval)

(defstruct card-output
  (card #'identity :type (function (card stream) t))
  (name #'identity :type (function (card stream) t))
  (phone #'identity :type (function (card stream) t))
  (address #'identity :type (function (card stream) t)))

(defun card-print-as-bbdb (cc out)
  (declare (type card cc) (stream out))
  (if (slot-boundp cc 'name) (format out "[~a " (card-name cc))
      (write-string "[nil nil nil " out))
  (if (or (slot-boundp cc 'org) (slot-boundp cc 'title))
      (format out "\"~a~:[~;; ~]~a\"" (slot-val cc 'org)
              (and (slot-boundp cc 'org) (slot-boundp cc 'title))
              (slot-val cc 'title))
      (write-string "nil " out))
  (format out "~w ~w ~w (" (card-phonel cc) (card-addrl cc) (card-emaill cc))
  (when (slot-boundp cc 'note) (format out "(notes . ~s" (card-note cc)))
  (format out "~@[(url . \"~{~a~^, ~}\") ~]~
\(creation-date . \"~a\") (timestamp . \"~a\")) nil]"
          (card-urll cc) (time2date (card-created cc))
          (time2date (card-timestamp cc))))

(defun card-print-as-vcard (cc out) ; FIXME
  (declare (type card cc) (stream out))
  (format out "BEGIN:VCARD~%FN:~a~%N:~a~%~{ADR;~a~%~}~{TEL;~a~%~}END:VCARD~2%"
          (card-lable cc) (slot-val cc 'name "") (card-addrl cc)
          (card-phonel cc)))

(defun card-print-as-pretty (cc out) ; FIXME
  (declare (type card cc) (stream out))
  (when (slot-boundp cc 'name) (format out "~a~%" (card-name cc)))
  (when (card-emaill cc)
    (format out "[email:~10t~{~<~%~9t ~1,74:; ~a~>~^,~}.~%" (card-emaill cc)))
  (format out "~{~a~%~}~{~a~%~}~2%" (card-phonel cc) (card-addrl cc)))

(defun name-print-as-bbdb (nm out)
  (declare (type name nm) (stream out))
  (format out "~:[~*~;~:*\"~a~@[ ~a~]\"~] ~s ~s" (slot-val nm 'first)
          (slot-val nm 'ini) (slot-val nm 'last) (slot-val nm 'aka)))

(defun name-print-as-vcard (nm out)
  (declare (type name nm) (stream out))
  (format out "~@[~a~];~@[~a~];~@[~a~];~@[~a~];~@[~a~]~%"
          (slot-val nm 'prefix) (slot-val nm 'first) (slot-val nm 'ini)
          (slot-val nm 'last) (slot-val nm 'suffix)))

(defun name-print-as-pretty (nm out)
  (declare (type name nm) (stream out))
  (format out "~@[~a ~]~@[~a ~]~@[~a. ~]~@[~a~]~@[, ~a~]"
          (slot-val nm 'prefix) (slot-val nm 'first) (slot-val nm 'ini)
          (slot-val nm 'last) (slot-val nm 'suffix)))

(defun phone-print-as-bbdb (ph out)
  (declare (type phone ph) (stream out))
  (format out "[~s ~s]" (phone-loc ph) (phone-nmb ph)))

(defun phone-print-as-vcard (ph out)
  (declare (type phone ph) (stream out))
  (format out "~a:~a" (phone-loc ph) (phone-nmb ph)))

(defun phone-print-as-pretty (ph out)
  (declare (type phone ph) (stream out))
  (format out "~a: ~a" (phone-loc ph) (phone-nmb ph)))

(defun address-print-as-bbdb (adrs out)
  (declare (type address adrs) (stream out))
  (format out "[~s ~s ~s ~s ~s ~s ~s]"
          (slot-val adrs 'street1 "") (slot-val adrs 'street2 "")
          (slot-val adrs 'city "") (slot-val adrs 'state "")
          (slot-val adrs 'zip "") (slot-val adrs 'country "")))

(defun address-print-as-vcard (adrs out)
  (declare (type address adrs) (stream out))
  (format out "~a:~a;~a;~a;~a;~a;~a"
          (slot-val adrs 'street1 "") (slot-val adrs 'street2 "")
          (slot-val adrs 'city "") (slot-val adrs 'state "")
          (slot-val adrs 'zip "") (slot-val adrs 'country "")))

(defun address-print-as-pretty (adrs out)
  (declare (type address adrs) (stream out))
  (format out "~@[~a~%~]~@[~a~%~]~@[~a ~]~@[~a ~]~@[~a ~]~@[~a~]"
          (slot-val adrs 'street1) (slot-val adrs 'street2)
          (slot-val adrs 'city) (slot-val adrs 'state)
          (slot-val adrs 'zip) (slot-val adrs 'country)))

)                               ; `eval-when'

(defconst +card-output-bbdb+ card-output
  (make-card-output :card #'card-print-as-bbdb
                    :name #'name-print-as-bbdb
                    :phone #'phone-print-as-bbdb
                    :address #'address-print-as-bbdb)
  "The constant, a valid value for `*card-output-type*'.")

(defconst +card-output-vcard+ card-output
  (make-card-output :card #'card-print-as-vcard
                    :name #'name-print-as-vcard
                    :phone #'phone-print-as-vcard
                    :address #'address-print-as-vcard)
  "The constant, a valid value for `*card-output-type*'.")

(defconst +card-output-pretty+ card-output
  (make-card-output :card #'card-print-as-pretty
                    :name #'name-print-as-pretty
                    :phone #'phone-print-as-pretty
                    :address #'address-print-as-pretty)
  "The constant, a valid value for `*card-output-type*'.")

(defmacro define-print-method (cla)
  `(defmethod print-object ((cc ,cla) (out stream))
    (cond ((or *print-readably* (null *card-output-type*)) (call-next-method))
          ((card-output-p *card-output-type*)
           (funcall (slot-value *card-output-type* ',cla) cc out))
          (t (error 'code :proc 'print-object :args (list *card-output-type*)
                    :mesg "Illegal value of `*card-output-type*': `~s'")))))

(define-print-method card)
(define-print-method name)
(define-print-method phone)
(define-print-method address)

;;;
;;; }}}{{{Input
;;;

(defun set-reading-braces (&optional (rt *readtable*))
  "Set the readtable RT to read [] as a vector and #[] as a CLOS class.
Use (setq *readtable* (copy-readtable)) to undo the effects."
  (set-syntax-from-char #\[ #\( rt)
  (set-syntax-from-char #\] #\) rt)
  (set-macro-character #\[ #'read-vector nil rt)
  (set-macro-character #\] (get-macro-character #\)) nil rt)
  (set-dispatch-macro-character #\# #\[ #'read-class rt)
  (set-macro-character #\] (get-macro-character #\)) nil rt))

(defun vector-to-phone (vec)
  "Convert the vector (from BBDB) to a PHONE instance."
  (declare (simple-vector vec) (values phone))
  (make-instance 'phone 'loc (aref vec 0) 'nmb
                 (typecase (aref vec 1)
                   (string (aref vec 1))
                   (number (format nil "(~3,'0d) ~3,'0d-~4,'0d~:[~;x~d~]"
                                   (aref vec 1) (aref vec 2) (aref vec 3)
                                   (zerop (aref vec 4)) (aref vec 4))))))

(defun vector-to-address (vec)
  "Convert the vector (from BBDB) to an ADDRESS  instance."
  (declare (simple-vector vec) (values address))
  (let ((ad (make-instance 'address 'loc (aref vec 0))))
    (macrolet ((putslot (nn slot)
                 `(unless (zerop (length (aref vec ,nn)))
                   (setf (slot-value ad ',slot) (aref vec ,nn)))))
      (putslot 1 street1)
      (putslot 2 street2)
      (putslot 3 city)
      (putslot 4 state))
    (setf (slot-value ad 'zip)
          (typecase (aref vec 5)
            (number (format nil "~5,'0d" (aref vec 5)))
            (string (aref vec 5))
            (list (format nil "~5,'0d-~4,'0d"
                          (first (aref vec 5)) (second (aref vec 5))))))
    ad))

(defun vector-to-card (vec)
  "Convert the vector (as in BBDB) to a CARD."
  (declare (simple-vector vec) (values card))
  (let ((cc (make-instance 'card)))
    (when (or (aref vec 0) (aref vec 1) (aref vec 2))
      (let ((nm (make-instance 'name))) ; FIXME: midle ini, pre/suffix
        (setf (card-name cc) nm)
        (when (aref vec 0) (setf (name-first nm) (aref vec 0)))
        (when (aref vec 1) (setf (name-last nm) (aref vec 1)))
        (when (aref vec 2) (setf (name-aka nm) (aref vec 2)))))
    (when (aref vec 3) (setf (card-org cc) (aref vec 3)))
    (setf (card-phonel cc) (mapcar #'vector-to-phone (aref vec 4)))
    (setf (card-addrl cc) (mapcar #'vector-to-address (aref vec 5)))
    (setf (card-emaill cc) (aref vec 6))
    (let ((nt (aref vec 7)) tmp)
      (when (setq tmp (assoc 'notes nt :test #'eq))
        (setf (card-note cc) (cdr tmp)))
      (when (setq tmp (assoc 'creation-date nt :test #'eq))
        (setf (card-created cc) (date2time (date (cdr tmp)))))
      (when (setq tmp (assoc 'timestamp nt :test #'eq))
        (setf (card-timestamp cc) (date2time (date (cdr tmp)))))
      (dolist (ff '(www ftp gopher telnet))
        (when (setq tmp (assoc ff nt :test #'eq))
          (setf (card-urll cc)
                (nconc (mapcar #'url (split-string (cdr tmp)
                                                   #(#\Space #\Newline #\,)))
                       (card-urll cc))))))
    cc))

(defun card-read-bbdb (in ra)
  "Read CARD from a BBDB stream.  Suitable for `read-list-from-file'."
  (declare (stream in) (simple-vector ra))
  (values (vector-to-card ra) (read in nil +eof+)))

#+nil (progn

(set-reading-braces)
(read-list-from-file
 (merge-pathnames (make-pathname :name ".bbdb") (user-homedir-pathname))
 #'card-read-bbdb)

)

(provide "card")
;;; }}}file card.lisp ends here
