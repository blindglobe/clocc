;;; File: <card.lisp - 1999-04-20 Tue 19:39:43 EDT sds@eho.eaglets.com>
;;;
;;; Personal Database  - Rolodex (BBDB, VCARD)
;;;
;;; Copyright (C) 1999 by Sam Steingold
;;;
;;; $Id: card.lisp,v 1.2 1999/04/20 23:39:58 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/card.lisp,v $
;;; $Log: card.lisp,v $
;;; Revision 1.2  1999/04/20 23:39:58  sds
;;; BBDB input and all output methods work now.
;;; (card-output): slots contain symbols, not functions.
;;; (set-reading-braces): removed; use `el::make-elisp-readtable'.
;;; (init-sans-null-args): new function for `initialize-instance'.
;;; (initialize-instance card): primary method now.
;;; (*card-apellations*, *card-suffixes*): new variables.
;;; (initialize-instance name): new method.
;;; (card-org/title, time2string): new functions for printing.
;;; Searching:
;;; (*user-cards*): new variable.
;;; (object-match-p, find-card): new functions.
;;;
;;; Revision 1.1  1999/04/19 23:35:05  sds
;;; Initial revision
;;;
;;;

(in-package :cl-user)

(eval-when (load compile eval)
  (sds-require "base") (sds-require "print")
  (sds-require "date") (sds-require "url") (sds-require "elisp")
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
   (aka :type cons :initarg aka :accessor name-aka
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
   (street2 :type simple-string :initarg street2 :accessor address-street2
            :documentation "line two of the street address")
   (street3 :type simple-string :initarg street3 :accessor address-street3
            :documentation "line three of the street address")
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
  ((label :type simple-string :initarg label :accessor card-label
          :documentation "The unique record ID.")
   (name :type name :initarg name :accessor card-name
         :documentation "The name of the person.")
   (addrl :type cons :initarg addrl :accessor card-addrl
          :documentation "The list of addresses.")
   (phonel :type cons :initarg phonel :accessor card-phonel
           :documentation "The list of phones.")
   (emaill :type cons :initarg emaill :accessor card-emaill
           :documentation "The list of e-mail addresses.")
   (urll :type list :initarg urll :accessor card-urll
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
   (geo :type complex :initarg geo :accessor card-geo
        :documentation "The geografic coordinates, in degrees.")
   (class :type symbol :initarg class :accessor card-class
          :documentation "Access classification: PRIVATE, PUBLIC &c")
   ;; (bday
   (created :type (integer 0) :initarg created :accessor card-created
            :documentation "The creation time, in seconds since the epoch.")
   (timestamp :type (integer 0) :initarg timestamp :accessor card-timestamp
              :documentation "The last modification time."))
  (:documentation "The personal information record."))

(defun slot-val (obj slot &optional default)
  (or (when (slot-boundp obj slot) (slot-value obj slot)) default))

;;;
;;; }}}{{{Search
;;;

(defcustom *user-cards* list nil "*The user database of records.")

(defun object-match-p (cc nm)
  "Check whether the object matches the string."
  (declare (type card cc) (simple-string nm))
  (dolist (slot (class-slot-list cc))
    (when (slot-boundp cc slot)
      (typecase (slot-value cc slot)
        (string (when (search nm (slot-value cc slot)) (return t)))
        (standard-object
         (when (object-match-p (slot-value cc slot) nm) (return t)))))))

(defun find-card (nm &optional (cards *user-cards*))
  "Return the list of cards matching NM."
  (declare (simple-string nm))
  (loop :for cc :of-type card :in cards
        :when (object-match-p cc nm) :collect cc))

;;;
;;; }}}{{{Output
;;;

(defcustom *card-output-type* (or null card-output) nil
  "The type of output for CARD.
Valid value are instances of `card-output' or NIL (for #[card ...]).
See constants `+card-output-bbdb+', `+card-output-vcard+',
`+card-output-pretty+'.")

(defcustom *user-bbdb-file* pathname
  (merge-pathnames (make-pathname :name ".bbdb") (user-homedir-pathname))
  "*The path to the user's BBDB file.")

(defcustom *user-vcard-file* pathname
  (merge-pathnames (make-pathname :directory '(:relative ".Gnome")
                                  :name "GnomeCard.gcrd")
                   (user-homedir-pathname))
  "*The path to the user's BBDB file.")

(defstruct card-output
  (card nil :type symbol)
  (name nil :type symbol)
  (phone nil :type symbol)
  (address nil :type symbol))

(defconst +card-output-bbdb+ card-output
  (make-card-output :card 'card-print-as-bbdb
                    :name 'name-print-as-bbdb
                    :phone 'phone-print-as-bbdb
                    :address 'address-print-as-bbdb)
  "The constant, a valid value for `*card-output-type*'.")

(defconst +card-output-vcard+ card-output
  (make-card-output :card 'card-print-as-vcard
                    :name 'name-print-as-vcard
                    :phone 'phone-print-as-vcard
                    :address 'address-print-as-vcard)
  "The constant, a valid value for `*card-output-type*'.")

(defconst +card-output-pretty+ card-output
  (make-card-output :card 'card-print-as-pretty
                    :name 'name-print-as-pretty
                    :phone 'phone-print-as-pretty
                    :address 'address-print-as-pretty)
  "The constant, a valid value for `*card-output-type*'.")

(defun card-org/title (cc)
  "Merge the TITLE with ORG."
  (when (or (slot-boundp cc 'org) (slot-boundp cc 'title))
    (format nil "~@[~a~]~:[~;; ~]~@[~a~]" (slot-val cc 'org)
            (and (slot-boundp cc 'org) (slot-boundp cc 'title))
            (slot-val cc 'title))))

(defun time2string (ut &optional brief)
  "YYYY-MM-DDTHH:MM:SS"
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time ut)
    (if brief (format nil "~d-~2,'0d-~2,'0d" ye mo da)
        (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
                ye mo da ho mi se))))

(defun card-print-as-bbdb (cc out)
  (declare (type card cc) (stream out))
  (let ((*print-right-margin* nil) (*print-pretty* nil))
    (if (slot-boundp cc 'name) (format out "[~a " (card-name cc))
        (write-string "[nil nil nil " out))
    (if (or (slot-boundp cc 'org) (slot-boundp cc 'title))
        (format out "\"~a~:[~;; ~a~]\" " (slot-val cc 'org)
                (and (slot-boundp cc 'org) (slot-boundp cc 'title))
                (slot-val cc 'title))
        (write-string "nil " out))
    (format out "~s ~s ~s ~s (" (card-org/title cc) (slot-val cc 'phonel)
            (slot-val cc 'addrl) (slot-val cc 'emaill))
    (when (slot-boundp cc 'note) (format out "(notes . ~s) " (card-note cc)))
    (format out "~@[(url . \"~{~a~^, ~}\") ~]~
\(creation-date . \"~a\") (timestamp . \"~a\")) nil]"
            (slot-val cc 'urll) (time2string (card-created cc) t)
            (time2string (card-timestamp cc) t))))

(defun card-print-as-vcard (cc out)
  (declare (type card cc) (stream out))
  (format out "~&BEGIN:VCARD~%FN:~a~%~@[N:~a~%~]~{ADR:~a~%~}~{TEL:~a~%~}~
~{EMAIL:~a~%~}~{URL:~a~%~}~@[TITLE:~a~%~]~@[ORG:~a~%~]~@[TZ:~a~%~]~
~@[NOTE:~a~%~]"
          (card-label cc) (slot-val cc 'name) (slot-val cc 'addrl)
          (slot-val cc 'phonel) (slot-val cc 'emaill) (slot-val cc 'urll)
          (slot-val cc 'title) (slot-val cc 'org) (slot-val cc 'tz)
          (slot-val cc 'note))
  (when (slot-boundp cc 'geo)
    (format out "GEO:~f:~f~%"
            (realpart (card-geo cc)) (imagpart (card-geo cc))))
  (format out "REV:~a~%CRE~a~%~@[CLASS:~a~%~]END:VCARD~%"
          (time2string (card-timestamp cc)) (time2string (card-created cc))
          (slot-val cc 'security)))

(defun card-print-as-pretty (cc out)
  (declare (type card cc) (stream out))
  (format out "~@[~a~%~]~@[[~a]~%~]" (slot-val cc 'name) (card-org/title cc))
  (when (slot-boundp cc 'emaill)
    (format out "[email:~10t~{~<~%~9t ~1,74:; <~a>~>~^,~}.]~%"
            (card-emaill cc)))
  (when (slot-boundp cc 'urll)
    (format out "[URL:~10t~{~<~%~9t ~1,74:; <~a>~>~^,~}.]~%" (card-urll cc)))
  (format out "~{~a~%~}~{~a~%~}~@[[~a]~%~]{~a  ~a}~2%"
          (slot-val cc 'phonel) (slot-val cc 'addrl) (slot-val cc 'note)
          (time2string (card-created cc)) (time2string (card-timestamp cc))))

(defun name-print-as-bbdb (nm out)
  (declare (type name nm) (stream out))
  (format out "~:[~*~;~:*\"~a~@[ ~a~]\"~] ~s ~s" (slot-val nm 'first)
          (slot-val nm 'ini) (slot-val nm 'last) (slot-val nm 'aka)))

(defun name-print-as-vcard (nm out)
  (declare (type name nm) (stream out))
  (format out "~@[~a~];~@[~a~];~@[~a~];~@[~a~];~@[~a~]"
          (slot-val nm 'prefix) (slot-val nm 'first) (slot-val nm 'ini)
          (slot-val nm 'last) (slot-val nm 'suffix)))

(defun name-print-as-pretty (nm out)
  (declare (type name nm) (stream out))
  (format out "~@[~a ~]~@[~a ~]~@[~a ~]~@[~a~]~@[, ~a~]"
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
  (format out " ~20@a:  ~a" (phone-loc ph) (phone-nmb ph)))

(defun address-print-as-bbdb (adrs out)
  (declare (type address adrs) (stream out))
  (format out "[~s ~s ~s ~s ~s ~s ~s ~s]"
          (slot-val adrs 'loc "") (slot-val adrs 'street1 "")
          (slot-val adrs 'street2 "") (slot-val adrs 'street3 "")
          (slot-val adrs 'city "") (slot-val adrs 'state "")
          (slot-val adrs 'zip "") (slot-val adrs 'country "")))

(defun address-print-as-vcard (adrs out)
  (declare (type address adrs) (stream out))
  (format out "~a:~a;~a;~a;~a;~a;~a;~a"
          (slot-val adrs 'loc "") (slot-val adrs 'street1 "")
          (slot-val adrs 'street2 "") (slot-val adrs 'street2 "")
          (slot-val adrs 'city "") (slot-val adrs 'state "")
          (slot-val adrs 'zip "") (slot-val adrs 'country "")))

(defun address-print-as-pretty (adrs out)
  (declare (type address adrs) (stream out))
  (format
   out "~a:~%~@[ ~a~%~]~@[ ~a~%~]~@[ ~a~%~]~@[ ~a ~]~@[~a ~]~@[~a ~]~@[~a~]"
   (slot-val adrs 'loc) (slot-val adrs 'street1)
   (slot-val adrs 'street2) (slot-val adrs 'street3)
   (slot-val adrs 'city) (slot-val adrs 'state)
   (slot-val adrs 'zip) (slot-val adrs 'country)))

(defmacro define-print-method (cla)
  `(defmethod print-object ((cc ,cla) (out stream))
    (cond ((or *print-readably* (null *card-output-type*)) (call-next-method))
          ((card-output-p *card-output-type*)
           (funcall (fdefinition (slot-value *card-output-type* ',cla))
                    cc out))
          (t (error 'code :proc 'print-object :args (list *card-output-type*)
                    :mesg "Illegal value of `*card-output-type*': `~s'")))))

(define-print-method card)
(define-print-method name)
(define-print-method phone)
(define-print-method address)

(defun init-sans-null-args (obj list)
  "(a 10 b nil c 1 d nil) --> (a 10 c 1)"
  (loop :for arg :in list :by #'cddr :and val :in (cdr list) :by #'cddr
        :do (when val (setf (slot-value obj arg) val))))

;; has to be here since it uses `+card-output-pretty+'
(defmethod initialize-instance ((cc card) &rest args)
  (init-sans-null-args cc args)
  ;; init `label' from `name' and `emaill'
  (unless (slot-boundp cc 'label)
    (let ((*card-output-type* +card-output-pretty+))
      (setf (card-label cc)
            (format nil "~@[~a ~]<~@[~a~]>" (slot-val cc 'name)
                    (car (slot-val cc 'emaill))))))
  ;; init `timestamp' and `created' from `get-universal-time'
  (let ((tm (get-universal-time)))
    (unless (slot-boundp cc 'timestamp) (setf (card-timestamp cc) tm))
    (unless (slot-boundp cc 'created) (setf (card-created cc) tm)))
  ;; init `tz' from `geo'
  (when (and (slot-boundp cc 'geo) (not (slot-boundp cc 'tz)))
    (setf (card-tz cc) (round (realpart (card-geo cc)) 15)))
  ;; replace #\Newline with \n in multiline strings.
  (when (slot-boundp cc 'note)
    (setf (card-note cc)
          (substitute-subseq (card-note cc) (string #\Newline) "\\n")))
  (when (slot-boundp cc 'security)
    (setf (card-note cc)
          (substitute-subseq (card-security cc) (string #\Newline) "\\n")))
  cc)

(defcustom *card-apellations* list
  '("Mr." "Mrs." "Ms." "Miss" "Dr" "Dr." "Prof.")
  "*The list of recognizable apellations.")

(defcustom *card-suffixes* list
  '("PhD" "Ph.D." "PhD." "MD" "CPA" "Jr" "3rd")
  "*The list of recognizable apellations.")

(defmethod initialize-instance ((nn name) &rest args)
  (init-sans-null-args nn args)
  (when (slot-boundp nn 'first)
    (let* ((fi (name-first nn)) (len (length fi)))
      (declare (simple-string fi))
      (unless (slot-boundp nn 'prefix)
        (dolist (st *card-apellations*)
          (when (string-beg-with st fi len)
            (setf (name-prefix nn) st
                  fi (string-left-trim +whitespace+ (subseq fi (length st)))
                  (name-first nn) fi)
            (return))))
      (unless (slot-boundp nn 'ini)
        (let ((pos (position #\Space fi :from-end t)))
          (when pos
            (setf (name-ini nn) (subseq fi (1+ pos))
                  (name-first nn) (subseq fi 0 pos)))))))
  (when (and (slot-boundp nn 'last) (not (slot-boundp nn 'suffix)))
    (let* ((la (name-last nn)) (len (length la)))
      (declare (simple-string la))
      (dolist (st *card-suffixes*)
        (when (string-end-with st la len)
          (setf (name-suffix nn) st
                (name-last nn)
                (string-right-trim
                 +whitespace+ (subseq la 0 (- len (length st))))
                len (length (name-last nn)))
          ;; remove the taling comma
          (when (char= #\, (schar (name-last nn) (1- len)))
            (setf (name-last nn) (subseq (name-last nn) 0 (1- len))))
          (return)))))
  nn)

;;;
;;; }}}{{{Input
;;;

(defun vector-to-phone (vec)
  "Convert the vector (from BBDB) to a PHONE instance."
  (declare (simple-vector vec) (values phone))
  (make-instance 'phone 'loc (aref vec 0) 'nmb
                 (typecase (aref vec 1)
                   (string (aref vec 1))
                   (number (format nil "(~3,'0d) ~3,'0d-~4,'0d~:[ x~d~;~]"
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
      (putslot 3 street3)
      (putslot 4 city)
      (putslot 5 state))
    (setf (address-zip ad)
          (let ((zip (aref vec 6)))
            (typecase zip
              (number (format nil "~5,'0d" zip))
              (string zip)
              (list (format nil "~5,'0d-~4,'0d" (first zip) (second zip))))))
    ad))

(defun vector-to-card (vec)
  "Convert the vector (as in BBDB) to a CARD."
  (declare (simple-vector vec) (values card))
  (let ((nts (aref vec 7)) tmp)
    (make-instance
     'card
     'name (make-instance 'name 'first (aref vec 0) 'last (aref vec 1)
                          'aka (aref vec 2))
     'org (aref vec 3)
     'phonel (mapcar #'vector-to-phone (aref vec 4))
     'addrl (mapcar #'vector-to-address (aref vec 5))
     'emaill (aref vec 6)
     'note (cdr (assoc 'notes nts :test #'eq))
     'created (when (setq tmp (assoc 'creation-date nts :test #'eq))
                (date2time (date (cdr tmp))))
     'timestamp (when (setq tmp (assoc 'timestamp nts :test #'eq))
                  (date2time (date (cdr tmp))))
     'urll (mapcan
            (lambda (ff)
              (mapcar #'url (split-string (cdr (assoc ff nts :test #'eq))
                                          #(#\Space #\Newline #\,))))
            '(www ftp gopher telnet)))))

(defun card-read-bbdb (in ra)
  "Read CARD from a BBDB stream.  Suitable for `read-list-from-file'."
  (declare (stream in) (simple-vector ra))
  (handler-case (values (vector-to-card ra) (read in nil +eof+))
    (error (co)
      (format t "~& *** [vector-to-card] Broken on~%~s~%" ra)
      (error co))))

#+nil (progn

(setq *readtable* (el::make-elisp-readtable))
(setq *user-cards* (read-list-from-file *user-bbdb-file* #'card-read-bbdb))

(let ((*card-output-type* +card-output-vcard+))
  (write-list-to-file *user-cards* *user-vcard-file*))

(let ((*card-output-type* +card-output-pretty+))
  (write-list-to-stream *user-cards* *standard-output*))

(let ((*card-output-type* +card-output-bbdb+))
  (write-list-to-stream *user-cards* *user-bbdb-file*)) ; *standard-output*

(setq *user-cards* (read-list-from-file ))

(setq *readtable* (copy-readtable))

nil
)

(provide "card")
;;; }}}file card.lisp ends here
