;;; File: <inspect.lisp - 2000-03-09 Thu 14:08:14 Eastern Standard Time sds@ksp.com>
;;;
;;; Inspect
;;;
;;; Copyright (C) 2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: inspect.lisp,v 1.2 2000/03/09 19:09:37 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/inspect.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `list-length-dotted'
  (require :simple (translate-logical-pathname "cllib:simple"))
  ;; `class-slot-list'
  (require :closio (translate-logical-pathname "cllib:closio"))
  ;; `open-socket-server'
  (require :net (translate-logical-pathname "port:net"))
  ;; `browse-url'
  (require :url (translate-logical-pathname "cllib:url"))
  ;; `string-beg-with'
  (require :string (translate-logical-pathname "cllib:string"))
  ;; `with-tag'
  (require :htmlgen (translate-logical-pathname "cllib:htmlgen")))

(in-package :cllib)

(export '(inspect))

;;;
;;; options
;;;

(defvar *inspect-frontend* :tty) ; the default frontend
(defvar *inspect-print-lines* 5) ; default for `*print-lines*'
(defvar *inspect-print-level* 5) ; default for `*print-level*'
(defvar *inspect-print-length* 10) ; default for `*print-length*'

(defparameter *inspect-debug* 0) ; debug level

;;;
;;; backend
;;;

(defvar *inspect-length* 5)     ; the number of sequence elements to print
(defvar *inspect-depth*)        ; the depth of the call

(defstruct (inspection (:conc-name insp-))
  self
  (title "" :type string)
  (blurb "" :type string)
  (up nil :type (or null inspection))
  (left nil :type boolean)
  (right nil :type boolean)
  (num-slots 0 :type fixnum)
  (nth-slot nil :type (or null (function (integer) (t t)))) ; value & name
  (set-slot nil :type (or null (function (integer t) t))))

(defmethod print-object ((ins inspection) (out stream))
  (if *print-readably* (call-next-method)
      (handler-case (print-inspection ins out *inspect-frontend*)
        (error (err)
          ;(declare (ignore err))
          (format t "~&*** print error for inspection! ***~%")
          (call-next-method)
          (format t "~&*** print error: ~s***~%" err)))))

(defgeneric inspect-backend (object &rest opts)
  (:method ((obj array) &rest opts)
    (let* ((siz (array-total-size obj)) (type (array-element-type obj))
           (arr (make-array siz :displaced-to obj :element-type type)))
      (apply #'make-inspection
             :self obj :title (format nil "Array ~{~:d~^x~} (= ~:d)"
                                      (array-dimensions obj) siz)
             :blurb (format nil "element-type: ~s" type)
             :num-slots siz :nth-slot (lambda (ii) (aref arr ii))
             :set-slot (lambda (ii val) (setf (aref arr ii) val))
             opts)))
  (:method ((obj cons) &rest opts)
    (multiple-value-bind (len dotted-p) (list-length-dotted obj)
      (apply
       #'make-inspection
       :num-slots (if dotted-p (1+ len) len)
       :nth-slot (lambda (ii) (if (and dotted-p (= ii len)) dotted-p
                                  (nth ii obj)))
       :set-slot (lambda (ii val)
                   (if (and dotted-p (= ii len))
                       (setf (cdr (nthcdr ii obj)) val)
                       (setf (nth ii obj) val)))
       :blurb (if len
                  (if dotted-p
                      (if (> len 1)
                          (format nil "a dotted list of length ~:d" len)
                          (format nil "a cons"))
                      (format nil "a list of length ~:d" len))
                  (format nil "a cyclic list"))
       :self obj :title (format nil "Cons") opts)))
  (:method ((obj symbol) &rest opts)
    (apply #'make-inspection :num-slots 2
           :nth-slot (lambda (ii)
                       (case ii
                         (0 (values (if (boundp obj) (symbol-value obj)
                                        nil) ; #<unbound>?
                                    :symbol-value))
                         (1 (values (symbol-plist obj) :symbol-plist))))
           :set-slot (lambda (ii val)
                       (case ii
                         (0 (setf (symbol-value obj) val))
                         (1 (setf (symbol-plist obj) val))))
           :blurb (format nil "package: ~s" (symbol-package obj))
           :self obj :title "Symbol" opts))
  (:method ((obj structure-object) &rest opts)
    (let ((slots (class-slot-list obj)))
      (apply #'make-inspection
             :num-slots (length slots)
             :nth-slot (lambda (ii)
                         (let ((slot (nth ii slots)))
                           (values (slot-value obj slot) slot)))
             :set-slot (lambda (ii val)
                         (setf (slot-value obj (nth ii slots)) val))
             :self obj :title "structure object"
             :blurb (format nil "type: ~s" (type-of obj)) opts)))
  (:method ((obj standard-object) &rest opts)
    (let ((slots (class-slot-list obj)))
      (apply #'make-inspection
             :num-slots (length slots)
             :nth-slot (lambda (ii)
                         (let ((slot (nth ii slots)))
                           (values (slot-value obj slot) slot)))
             :set-slot (lambda (ii val)
                         (setf (slot-value obj (nth ii slots)) val))
             :self obj :title "structure object"
             :blurb (format nil "type: ~s" (type-of obj)) opts)))
  (:method ((obj t) &rest opts)
    (apply #'make-inspection :self obj :title "atom"
           :blurb (format nil "type: ~s" (type-of obj)) opts)))

;;;
;;; To define a frontend, one has to define methods for
;;;  `print-inspection' and `inspect-frontend'

(defgeneric print-inspection (insp out frontend)
  (:method ((insp inspection) (out stream) (frontend t))
    (error "~s: unknown inspect front end: ~s" 'print-inspection frontend)))
(defgeneric inspect-frontend (insp frontend)
  (:method ((insp inspection) (frontend t))
    (error "~s: unknown inspect front end: ~s" 'inspect-frontend frontend)))

;;;
;;; common frontend utilities
;;;

(defun inspect-child-loop (com insp frontend &key (out *standard-output*))
  (let ((last-slot (1- (insp-num-slots insp))))
    (when (> *inspect-debug* 0)
      (format t "~s: ~s [0;~:d]~%" 'inspect-child-loop com last-slot))
    (typecase com
      (integer
       (if (<= 0 com last-slot)
           (loop
            (let ((ret (inspect-frontend
                        (inspect-backend
                         (funcall (insp-nth-slot insp) com)
                         :up insp :left (> com 0) :right (< com last-slot))
                        frontend)))
              (cond ((eq ret :u) (return :u))
                    ((and (eq ret :l) (> com 0)) (setq com (1- com)))
                    ((and (eq ret :r) (< com last-slot)) (setq com (1+ com)))
                    ((eq ret :q) (return :q))
                    (t (error "invalid return from ~s: ~s {~:d [0;~:d]}"
                              'inspect-frontend ret com last-slot)))))
           (format t " * ~:d out of range [0;~:d]~%" com last-slot)))
      (cons
       (labels ((clean (form)
                  (cond ((eq (car form) :self)
                         (setf (car form) `',(insp-self insp))
                         (clean-up (cdr form)))
                        ((eq (car form) :slot)
                         (setf (car form) 'funcall
                               (cdr form) (cons (insp-nth-slot insp)
                                                (cdr form)))
                         (clean-up (cddr form)))
                        (t (clean-up (car form))
                           (clean-up (cdr form)))))
                (clean-up (form) (when (consp form) (clean form)) form))
         (print (eval (print (clean-up com) out)) out)))
      (t (format t "** unknown command: ~s~%" com)))))

;;;
;;; TTY frontend
;;;

(defmethod print-inspection ((insp inspection) (out stream)
                             (backend (eql :tty)))
  (declare (ignore backend))
  (format out "~s:  ~a~% ~a~%" (insp-self insp) (insp-title insp)
          (insp-blurb insp))
  (when (insp-nth-slot insp)
    (loop :for ii :from 0 :to (min (1- (insp-num-slots insp))
                                   *inspect-print-length*)
          :do (multiple-value-bind (val name) (funcall (insp-nth-slot insp) ii)
                (format out "~d~@[ [~a]~]:  ~s~%" ii name val)))))

(defmethod inspect-frontend ((insp inspection) (frontend (eql :tty)))
  (when (> *inspect-debug* 0)
    (format t "~s [front: ~s] [self: ~s]: level ~:d~%" 'inspect-frontend
            frontend (insp-self insp) (1+ *inspect-depth*)))
  (format t "~70~~%~s~%" insp)
  (do ((*inspect-depth* (1+ *inspect-depth*)))
      (nil)
    (format
     t "~2&~v~INSPECT-- type :h for help; :q to return to the REPL ---> "
     *inspect-depth*)
    (force-output nil)
    (let ((com (read *terminal-io* nil :q)))
      (case com
        ((:q :e) (return :q))
        ((:h :?) (format t " *** commands:~% :h, :?~15t this help
 :p, :a~15t Print the current item Again
 :d~15t Describe the current item~%")
         (when (insp-up insp)
           (format t " :u~15t return UP to the parent~%"))
         (when (insp-left insp)
           (format t " :l~15t inspect the left neighbor~%"))
         (when (insp-right insp)
          (format t " :r~15t inspect the right neighbor~%"))
         (when (insp-nth-slot insp)
           (format t " number~15t inspect this slot~%"))
         (format t " lisp form~15t eval this form, with these substitutions:
 ~20t (:slot number) is replaced with the appropriate slot value
 ~20t :self is replaced with this object
 :q, :e~15t return to the main Read/Eval/Print loop~%"))
        (:d (describe (insp-self insp)))
        ((:p :a) (princ insp))
        (:l (if (insp-left insp) (return :l)  ; end recursive call
                (format t " * no left neighbor~%")))
        (:r (if (insp-right insp) (return :r) ; end recursive call
                (format t " * no right neighbor~%")))
        (:u (if (insp-up insp) (return :u)    ; end recursive call
                (format t " * no parent~%")))
        (t (when (eq :q (inspect-child-loop com insp frontend))
             (return :q)))))))


;;;
;;; HTTP backend
;;;

(defmethod print-inspection ((insp inspection) (raw stream)
                             (backend (eql :http)))
  (declare (ignore backend))
  (with-html-output (out raw :title (insp-title insp) :footer nil)
    (with-tag (:h1) (princ (insp-title insp) out))
    (with-tag (:h2) (princ (insp-blurb insp) out))
    (with-tag (:font :size "+4")
      (with-tag (:pre) (write (insp-self insp) :stream out)))
    (when (insp-nth-slot insp)
      (with-tag (:ol)
        (loop :for ii :from 0 :to (min (1- (insp-num-slots insp))
                                       *inspect-print-length*)
              :do (multiple-value-bind (val name)
                      (funcall (insp-nth-slot insp) ii)
                    (with-tag (:li)
                      (with-tag (:a :href ii)
                        (princ (string (or name "inspect")) out))
                      (with-tag (:pre) (write val :stream out)))))))
    (with-tag (:hr))
    (with-tag (:h2) (princ "describe:" out))
    (with-tag (:pre) (describe (insp-self insp) out))
    (with-tag (:hr))
    (with-tag (:table :width "100%")
      (with-tag (:tr)
        (with-tag (:td :align "left")
          (with-tag (:a :href ":q") (princ "quit" out)))
        (when (insp-left insp)
          (with-tag (:td :align "center")
            (with-tag (:a :href ":l") (princ "left" out))))
        (when (insp-right insp)
          (with-tag (:td :align "center")
            (with-tag (:a :href ":r") (princ "right" out))))
        (when (insp-up insp)
          (with-tag (:td :align "right")
            (with-tag (:a :href ":u") (princ "parent" out))))))
    (with-tag (:hr))
    (with-tag (:strong)
      (princ "do NOT use your browser's BACK/FORWARD facility" out))))

(defparameter *inspect-http-server* nil)
(defparameter *inspect-http-socket* nil)

(defun http-command ()
  (when *inspect-http-socket*
    (format t "*** ~s: ~s~%" '*inspect-http-socket* *inspect-http-socket*))
  (setq *inspect-http-socket* (socket-accept *inspect-http-server*))
  ;; read everything from the HTTP socket and return the GET command
  (loop (let ((line (read-line *inspect-http-socket* nil nil)))
          (when (> *inspect-debug* 1) (format t "-> ~a~%" line))
          (when (string-beg-with "GET /" line)
            (return (let ((com (if (char= (char line 5) #\Space) nil
                                   (read-from-string line nil nil :start 5))))
                      (when (> *inspect-debug* 0)
                        (format t "~s: ~s~%" 'http-command com))
                      (when (> *inspect-debug* 1)
                        (loop (format
                               t "-> ~a~%"
                               (read-line *inspect-http-socket* nil nil))
                              (unless (listen *inspect-http-socket*)
                                (return))))
                      com))))))

(defun http-insp-out (insp)
  ;; write INSP and close `*inspect-http-socket*'
  (write insp :stream *inspect-http-socket*)
  (close *inspect-http-socket*)
  (setq *inspect-http-socket* nil))

(defmethod inspect-frontend ((insp inspection) (frontend (eql :http)))
  (declare (ignore backend))
  (when (> *inspect-debug* 0)
    (format t "~s [front: ~s] [self: ~s]: level ~:d~%" 'inspect-frontend
            frontend (insp-self insp) (1+ *inspect-depth*)))
  (do ((*inspect-http-server*
        (or *inspect-http-server*
            (let ((server (open-socket-server 17356)))
              (browse-url (format nil "http://~a:~d"
                                  (socket-server-host server)
                                  (socket-server-port server)))
             server)))
       (*inspect-depth* (1+ *inspect-depth*)) com)
      ((eq com :q)
       (when (zerop *inspect-depth*)
         (when (> *inspect-debug* 0)
           (format t "closing ~s~%" '*inspect-http-server*))
         (socket-server-close *inspect-http-server*)
         (when *inspect-http-socket*
           (with-html-output (out *inspect-http-socket* :title
                                  "thanks for using inspect" :footer nil)
             (with-tag (:h1) (princ "thanks for using inspect" out))
             (with-tag (:p) (princ "you may close this window now" out)))
           (when (> *inspect-debug* 0)
             (format t "closing ~s~%" '*inspect-http-socket*))
           (close *inspect-http-socket*)
           (setq *inspect-http-socket* nil)))
       com)
    (if *inspect-http-socket*
        (http-insp-out insp)
        (if (setq com (http-command))
            (case com
              (:q)
              ((:l :r :u) (return com))
              (t (setq com (inspect-child-loop com insp frontend))))
            (http-insp-out insp)))))

;;;
;;; the juice
;;;

;;;###autoload
(defun inspect (object &key (frontend *inspect-frontend*))
  (let ((*print-array* nil) (*print-pretty* t)
        (*print-circle* t) (*print-escape* t)
        #-clisp (*print-lines* *inspect-print-lines*)
        (*print-level* *inspect-print-level*)
        (*print-length* *inspect-print-length*)
        (*package* (make-package (gensym))) ; for `read'
        (*inspect-frontend* frontend) (*inspect-depth* -1))
    (unwind-protect
         (inspect-frontend (inspect-backend object) frontend)
      (delete-package *package*))
    (values)))

(provide :inspect)
;;; inspect.lisp ends here
