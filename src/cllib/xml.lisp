;;; XML parsing
;;;
;;; Copyright (C) 2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: xml.lisp,v 2.9 2000/05/08 17:56:28 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/xml.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `+whitespace+', `whitespace-char-p'
  (require :withtype (translate-logical-pathname "cllib:withtype"))
  ;; `substitute-subseq-if'
  (require :string (translate-logical-pathname "cllib:string"))
  ;; print CLOS objects readably
  (require :closio (translate-logical-pathname "cllib:closio"))
  ;; `with-timing'
  (require :log (translate-logical-pathname "cllib:log"))
  (require :gray (translate-logical-pathname "port:gray")))

(in-package :cllib)

(export '(*xml-readtable* with-xml-input with-xml-file xml-read-from-file))

;;;
;;; Entities
;;;

(defpackage xml-tags (:use))
(defcustom *xml-pack* package (find-package :xml-tags)
  "The package with all the XML tags and entities.")
(defcustom *xml-amp* hash-table (make-hash-table :test 'equal)
  "The `&' entities")
(defcustom *xml-per* hash-table (make-hash-table :test 'equal)
  "The `%' entities")
(defcustom *xml-ent-file* string "entities.xml"
  "*The file with the default entities, like &gt; and &amp;.
See <http://www.w3.org/TR/WD-html40-970708/sgml/entities.html>.")
(defcustom *xml-keep-comments* boolean nil
  "*When non-nil, keep the comments inside the XML-OBJ structure.")

(defun xml-init-entities ()
  "Clear both `*xml-amp*' and `*xml-per*' and then read `*xml-ent-file*'."
  (clrhash *xml-amp*) (clrhash *xml-per*)
  (xml-read-from-file *xml-ent-file* :reset-ent nil))

(defun xml-read-entity (stream)
  "read <!ENTITY ....>"
  (let ((ch (peek-char t stream)) (ht *xml-amp*) ent type data def)
    (when (char= ch #\%)
      (read-char stream) (peek-char t stream) (setq ht *xml-per*))
    (setq ent (xml-read-text stream #'whitespace-char-p :clean nil)
          type (read stream)
          data (if (symbolp type) (read stream) type))
    (check-type data string)
    (setf def (cond ((eq type 'xml-tags::system)
                     (lambda (&optional junk)
                       (declare (ignore junk))
                       (let ((str (handler-case
                                      (open (merge-pathnames
                                             data (xml-path stream))
                                            :direction :input)
                                    (error (err)
                                      (format t "cannot open file [~s]/[~s]~%"
                                              data (xml-path stream t))
                                      (error err)))))
                         (format t "~& * [~a ~:d bytes]..."
                                 data (file-length str))
                         str)))
                    ((eq type 'xml-tags::cdata)
                     (lambda (&optional string)
                       (if string data (make-string-input-stream data))))
                    ((symbolp type)
                     (error "~s[~a]: not implemented: type: ~s data: ~s"
                            'xml-read-entity stream type data))
                    ((setq data (nsubstitute #\" #\' data))
                     (lambda (&optional string)
                       (if string data (make-string-input-stream data))))))
    ;; finish reading - till #\>
    (xml-read-text stream #\> :clean nil) (read-char stream)
    (multiple-value-bind (val fp) (gethash ent ht)
      (when (and fp (not (equalp val def)))
        (warn "[~s]: redefining ~s [~c]" ; from ~s to ~s
              'xml-read-entity ent (if ch #\& #\%)))) ; val def
    (setf (gethash ent ht) def)
    ent))

(defun xml-entity (ent hash type &key (proc 'xml-entity) string)
  "Find the entity in the hash table."
  (multiple-value-bind (val fp) (gethash ent hash)
    (unless fp
      (warn "[~s]: ~c entity ~s undefined" proc type ent))
    (typecase val
      (function (funcall val string))
      (null "??")
      (t (error 'code :proc proc :args (list type ent val) :mesg
                "~c entity ~s was defined as ~s" )))))

(defun xml-expand-entities (string &key (start 0) end)
  "Substitute the expansions of all the entities in the string."
  (substitute-subseq-if
   string (lambda (seq beg fin)
            (position #\& seq :start beg :end fin :test #'char=))
   (lambda (seq beg fin) (1+ (position #\; seq :start beg :end fin)))
   (lambda (seq beg fin type)
     (declare (ignore type))
     (xml-entity (subseq seq (1+ beg) (1- fin)) *xml-amp* #\&
                 :proc 'xml-expand-entities :string t))
   :start start :end end))

;;;
;;; XML objects
;;;

(defstruct xml-comment (data "" :type string))

(defmethod print-object ((cmt xml-comment) (out stream))
  (if *print-readably* (call-next-method)
      (format out "~&<!-- ~a -->~%" (xml-comment-data cmt))))

(defstruct (xml-obj (:conc-name xmlo-))
  (name nil :type symbol)       ; in package XML-TAGS
  (attribs nil :type list)      ; alist of attrib/value
  (data nil :type list))        ; list of objects in the tag

(defun xml-size (obj)
  "Compute the approximate size of the object.
The first number returned is `text sise' (no tags)
the second is `file size' (including tags)."
  (typecase obj
    (string (let ((ll (length obj))) (values ll ll)))
    (xml-obj
     (do ((fs (reduce #'+ (xmlo-attribs obj)
                      :key (lambda (att)
                             (+ (length (symbol-name (car att)))
                                (length (cadr att)) 4)) ; space=""
                      :initial-value ; <></>
                      (+ 5 (* 2 (length (symbol-name (xmlo-name obj)))))))
          (ts 0) (dl (xmlo-data obj) (cdr dl)))
         ((null dl) (values ts fs))
       (multiple-value-bind (t0 f0) (xml-size (car dl))
         (incf fs f0) (incf ts t0))))
    (t (error 'case-error :proc 'xml-size :args
              (list 'object obj 'string 'xml-obj)))))

(defcustom *xml-print-xml* boolean nil
  "*Set to non-NIL to print XML-OBJ for XML parsing.
Note that the Unicode characters will NOT be printed as &#nnnn;.")

(defun xml-ascii-p (char) (> 256 (char-code char)))

(defun xml-de-uunicode (string)
  "Replace the unicode characters with &#NNNN;"
  (substitute-subseq-if
   string
   (lambda (str beg end)
     (position-if (complement #'xml-ascii-p) str :start beg :end end))
   (lambda (str beg end) (declare (ignore str end)) (1+ beg))
   (lambda (str beg end type)
     (declare (ignore end type))
     (format nil "&#~d;" (char-code (char str beg))))))

(defmethod print-object ((xml xml-obj) (out stream))
  (cond (*xml-print-xml*
         (format out "<~a~:{ ~a=~s~}>" (xmlo-name xml) (xmlo-attribs xml))
         (dolist (dd (xmlo-data xml))
           (princ (typecase dd (string (xml-de-uunicode dd)) (t dd)) out))
         (format out "</~a>" (xmlo-name xml)))
        (*print-readably* (call-next-method))
        ((print-unreadable-object (xml out :type t :identity t)
           (multiple-value-bind (ts fs) (xml-size xml)
             (format out "~a [~:{~a=~s~^ ~}] ~:d object~:p ~:d/~:d chars"
                     (xmlo-name xml) (xmlo-attribs xml)
                     (length (xmlo-data xml)) ts fs))))))

(defun xml-push (new xml)
  "Add NEW to data in XML."
  (typecase new
    (xml-obj (push new (xmlo-data xml)))
    (string (unless (zerop (length new))
              (if (stringp (car (xmlo-data xml)))
                  (let ((last (pop (xmlo-data xml))))
                    (push (concatenate 'string last new) (xmlo-data xml)))
                  (push new (xmlo-data xml)))))
    (xml-comment (when *xml-keep-comments* (push new (xmlo-data xml))))
    (cons (assert (eq (car new) (xmlo-name xml)) (new)
                  "~s: ~s was terminated by ~s" 'xml-push (xmlo-name xml) new)
          (setf (xmlo-data xml) (nreverse (xmlo-data xml))))
    (t (error 'case-error :proc 'xml-push :args
              (list 'new new 'xml-obj 'string 'xml-comment 'cons)))))

;;;
;;; XML streams
;;;

(eval-when (compile load eval)  ; for CMUCL
(defclass xml-stream-in (fundamental-character-input-stream)
  ((input :initarg :stream :initarg :input :type stream :accessor xmlis-st)
   (all-streams :type list :accessor xmlis-all)
   (tag-stack :type list :initform nil :accessor xmlis-stack)
   (size :type integer :initform 0 :accessor xmlis-size)
   (comment :accessor xmlis-comment :documentation
            "nil - outside comment, t - inside, 1 - inside, one `-' read"))
  (:documentation "The input stream for reading XML."))
)

(defsubst stream-length (st)
  "A safe wrap around for `file-stream'."
  (typecase st (file-stream (file-length st)) (t 0)))

(defmethod initialize-instance :after ((str xml-stream-in) &rest junk)
  (declare (ignore junk))
  (cond ((typep (xmlis-st str) 'concatenated-stream)
         (setf (xmlis-all str) (concatenated-stream-streams (xmlis-st str))
               (xmlis-size str)
               (reduce #'+ (concatenated-stream-streams (xmlis-st str))
                       :key #'stream-length)))
        ((setf (xmlis-all str) (list (xmlis-st str))
               (xmlis-st str) (make-concatenated-stream (xmlis-st str))
               (xmlis-size str) (stream-length (xmlis-st str))))))

(defmethod stream-read-char ((in xml-stream-in))
  (read-char (xmlis-st in) nil :eof))
(defmethod stream-unread-char ((in xml-stream-in) (char character))
  (unread-char char (xmlis-st in)))
;; the default method is good enough
;;(defmethod stream-read-char-no-hang ((in xml-stream-in))
;;  (read-char-no-hang (xmlis-st in) nil :eof))
(defmethod stream-peek-char ((in xml-stream-in))
  (peek-char nil (xmlis-st in) nil :eof))
(defmethod stream-listen ((in xml-stream-in))
  (listen (xmlis-st in)))
(defmethod stream-read-line ((in xml-stream-in))
  (read-line (xmlis-st in)))
(defmethod stream-clear-input ((in xml-stream-in))
  (clear-input (xmlis-st in)))
(defmethod close ((in xml-stream-in) &rest opts)
  (dolist (st (xmlis-all in)) (apply #'close st opts))
  (apply #'close (xmlis-st in) opts))
(defun xml-path (str &optional debug-p)
  (declare (type xml-stream-in str))
  (when debug-p
    (format t "~& * All streams:~{~%~s~}~% * Pending:~{~%~s~}~%"
            (xmlis-all str) (concatenated-stream-streams (xmlis-st str))))
  (dolist (st (concatenated-stream-streams (xmlis-st str)))
    (when (typep st 'file-stream)
      (when debug-p (format t " == ~s -> ~s~%" st (truename st)))
      (return (truename st)))))

;;;
;;; Reading
;;;

(defun xml-obj-from-list (list)
  (make-xml-obj
   :name (car list) :attribs
   (loop :for xx :on (cdr list) :by #'cddr
         :collect (list (intern (string-right-trim "=" (symbol-name (car xx)))
                                *xml-pack*)
                        (xml-expand-entities (cadr xx))))))

(defun compress-whitespace (list &optional ends)
  "Replace internal whitespace with #\\Space
and trim (when ENDS is non-NIL) the leading and trailing whitespace."
  (do ((ll list (cdr ll)) good)
      ((null ll)
       (when (and ends good) (setf (cdr good) nil))
       (if ends (member-if (complement #'whitespace-char-p) list) list))
    (if (whitespace-char-p (car ll))
        (loop :initially (setf (car ll) #\Space)
              :while (and (cdr ll) (whitespace-char-p (cadr ll)))
              :do (setf (cdr ll) (cddr ll)))
        (setq good ll))))

(defun xml-read-text (str term &key (clean t) base)
  "Read characters from stream STR until TERM.
Return a string (with whitespace compressed with `compress-whitespace'
if the keyword argument `clean' is non-NIL, which is the default, and
the unicode entities &#nnnn; replaced with the appropriate characters
base the keyword argument BASE, when it is non-NIL, default - NIL)
TERM can be a predicate, a chacacter or a sequence of chacacters."
  (loop :with endp = (etypecase term
                       (function term)
                       (character (lambda (ch) (char= ch term)))
                       (sequence (lambda (ch) (find ch term :test #'char=))))
        :for ch = (read-char str t nil t)
        :until (funcall endp ch) :collect ch :into list
        :finally
        (progn (unread-char ch str)
               (when clean (setq list (compress-whitespace list)))
               (when base
                 (do ((ll list (cdr ll))) ((null ll)) ; &#nnnn;
                   (when (and (char= #\& (car ll)) (char= #\# (cadr ll)))
                     (do* ((l1 (cddr ll) (cdr l1)) (nn 0))
                          ((char= #\; (car l1))
                           (setf (car ll) (code-char nn)
                                 (cdr ll) (cdr l1)))
                       (setq nn (+ (* nn base)
                                   (digit-char-p (car l1) base)))))))
               (return (values (coerce list 'string) ch)))))

(defun xml-read-comment (str)
  "we are inside a comment; read it and return as a string"
  (loop :with ch :for data = (xml-read-text str '#\- :clean nil)
        :collect data :into all :do (read-char str t nil t)
        :if (char= #\- (setq ch (read-char str t nil t))) :do
        (setq ch (read-char str t nil t))
        (assert (char= ch #\>) (ch) "~s[~a]: ~s instead of #\>"
                'xml-read-comment str ch)
        (return (reduce (lambda (s0 s1) (concatenate 'string s0 s1))
                        all :initial-value ""))
        :else :collect (concatenate 'string "-" (string ch)) :into all))

(defun xml-read-tag (str)
  "read the tag, from <TAG-NAME> to </TAG-NAME>,
the first character to be read is #\T"
  (loop :with next :and xml =
        (xml-obj-from-list (read-delimited-list #\> str t))
        :initially (push (xmlo-name xml) (xmlis-stack str))
        :while (not (consp next)) :do
        (xml-push (xml-read-text str "<&") xml)
        (xml-push (setq next (read str t nil t)) xml)
        :finally (progn (assert (eq (car next) (car (xmlis-stack str))) (next)
                                "~s[~a]: ~s terminated ~s" 'xml-read-tag str
                                next (xmlis-stack str))
                        (pop (xmlis-stack str))
                        (return xml))))

(defun read-xml (stream char)
  (ecase char
    (#\<                        ; read tag
     (let ((ch (read-char stream t nil t)) (*package* *xml-pack*))
       (case ch
         (#\/ (let ((tag (read-delimited-list #\> stream t)))
                (assert (null (cdr tag)) (tag)
                        "~s[~a]: end tag ~s has attributes ~s" 'read-xml stream
                        (car tag) (cdr tag))
                tag))
         (#\? (let ((tags (read-delimited-list #\> stream t)))
                (assert (eq 'xml-tags::? (car (last tags))) (tags)
                        "~s[~a]: <? was terminated by ~s" 'read-xml stream
                        (car (last tags)))
                (nbutlast tags)
                (xml-obj-from-list tags)))
         (#\! (let ((obj (read stream t nil t)))
                (case obj
                  (xml-tags::-- (make-xml-comment
                                 :data (xml-read-comment stream)))
                  (xml-tags::entity (xml-read-entity stream))
                  ;; FIXME - there might be comments!
                  (t (cons obj (read-delimited-list #\> stream t))))))
         (t (unread-char ch stream)
            (xml-read-tag stream)))))
    (#\[ (read-delimited-list #\] stream t))
    ;;(#\> (funcall (get-macro-character #\)) stream char)
    ;;     (xml-read-text stream #\<))
    ((#\& #\%)
     (let* ((ent (xml-read-text stream #\;))
            (str (if (and (char= #\& char) (char= #\# (char ent 0)))
                     (let ((code (parse-integer ent :start 1)))
                       (if (< code char-code-limit)
                           (string (code-char code))
                           (concatenate 'string "&" ent)))
                     (xml-entity ent (case char
                                       (#\& *xml-amp*) (#\% *xml-per*))
                                 char :proc 'read-xml))))
       (etypecase str
         (string str)           ; "??" for undefined entities and &#nnnn;
         (stream
          (setf (xmlis-st stream) ; push
                (apply #'make-concatenated-stream str
                       (concatenated-stream-streams (xmlis-st stream)))
                (xmlis-all stream)
                (cons str
                      (delete-if (lambda (st) ; clean up
                                   (when (and (typep st 'string-stream)
                                              (eof-p st))
                                     (close st)))
                                 (xmlis-all stream))))
          (incf (xmlis-size stream) (stream-length str))
          (read stream t nil t)))))))

;;;
;;; UI
;;;

(defun make-xml-readtable ()
  (let ((rt (copy-readtable)))
    (set-macro-character #\< #'read-xml nil rt)
    (set-macro-character #\[ #'read-xml nil rt)
    (set-macro-character #\& #'read-xml nil rt)
    (set-macro-character #\% #'read-xml nil rt)
    ;; (set-macro-character #\> #'read-xml nil rt)
    (set-macro-character #\> (get-macro-character #\)) nil rt)
    (set-macro-character #\] (get-macro-character #\)) nil rt)
    (set-syntax-from-char #\; #\a rt)
    ;; (set-macro-character #\; (get-macro-character #\a) nil rt)
    rt))

(defcustom *xml-readtable* readtable (make-xml-readtable)
  "The readtable for XML parsing.")

(defmacro with-xml-input ((var stream) &body body)
  "Open the XML stream, evaluate the forms, make sure the stream is closed."
  `(with-open-stream (,var (make-instance 'xml-stream-in :input ,stream))
    (let ((*readtable* (make-xml-readtable))) ,@body)))

(defmacro with-xml-file ((var file &key reset-ent (out '*standard-output*))
                         &body body)
  "Open the XML stream to file."
  `(with-timing (:out ,out)
    (when ,reset-ent (xml-init-entities))
    (with-xml-input (,var (open ,file :direction :input))
      (format ,out "~&[~s]~% * [~a ~:d bytes]..." 'with-xml-input
       file (file-length (car (xmlis-all ,var))))
      (force-output ,out)
      (let ((*readtable* (make-xml-readtable)))
        (prog1 (progn ,@body)
          (format ,out "done [entities(%/&): ~:d/~:d] [bytes: ~:d]"
                  (hash-table-count *xml-per*) (hash-table-count *xml-amp*)
                  (xmlis-size ,var)))))))

(defun xml-read-from-file (file &key (reset-ent t))
  "Read all XML objects from the file."
  (with-xml-file (str file :reset-ent reset-ent)
    (loop :for obj = (read str nil +eof+)
          :while (not (eq obj +eof+)) :collect obj)))

(provide :xml)
;;; file xml.lisp ends here
