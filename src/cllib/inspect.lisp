;;; File: <inspect.lisp - 2000-03-02 Thu 22:55:48 EST sds@ksp.com>
;;;
;;; Inspect
;;;
;;; Copyright (C) 2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: inspect.lisp,v 1.1 2000/03/03 03:58:18 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/inspect.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `list-length-dotted'
  (require :simple (translate-logical-pathname "cllib:simple"))
  ;; `class-slot-list'
  (require :closio (translate-logical-pathname "cllib:closio")))

(in-package :cllib)

(eval-when (load compile eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

(export '(inspect))

;;;
;;; options
;;;

(defvar *inspect-frontend* :tty) ; the default frontend
(defvar *inspect-print-lines* 5) ; default for `*print-lines*'
(defvar *inspect-print-level* 5) ; default for `*print-level*'
(defvar *inspect-print-length* 10) ; default for `*print-length*'

;;;
;;; backend
;;;

(defvar *inspect-length* 5)     ; the number of sequence elements to print
(defvar *inspect-depth*)        ; the depth call

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
          (format t "~&*** print error ***~%")
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
  (princ insp)
  (do ((*inspect-depth* (1+ *inspect-depth*)))
      (nil)
    (format t "~&~v~INSPECT-- type :h for help; :q to return to the REPL ---> "
            *inspect-depth*)
    (force-output nil)
    (let ((com (read *terminal-io* nil :q)))
      (case com
        ((:q :e) (return))
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
        (t
         (let ((last-slot (1- (insp-num-slots insp))))
           (typecase com
             (integer
              (if (<= 0 com last-slot)
                  (loop
                   (let ((ret (inspect-frontend
                               (inspect-backend
                                (funcall (insp-nth-slot insp) com)
                                :up insp :left (> com 0) :right
                                (< com last-slot))
                               frontend)))
                     (case ret
                       (:u (return)) ; from child loop
                       (:l (if (> com 0) (setq com (1- com))
                               (error "invalid return from ~s: ~s/~s"
                                      'inspect-frontend ret com)))
                       (:r (if (< com last-slot) (setq com (1+ com))
                               (error "invalid return from ~s: ~s/~s"
                                      'inspect-frontend ret com)))
                       ((nil) (return-from inspect-frontend)) ; terminate
                       (t (error "invalid return from ~s: ~s/~s"
                                 'inspect-frontend ret com)))))
                  (format t " * ~:d out of range [0;~:d]~%" com last-slot)))
             (cons
              (let ((form (sublis `((:self . ,(insp-self insp))
                                    (:slot . ,(insp-nth-slot insp)))
                                  com)))
                (print (eval (print form)))))
             (t (format t "** unknown command: ~s~%" com)))))))))

;;;
;;; the juice
;;;

;;;###autoload
(defun inspect (object &key (frontend *inspect-frontend*))
  (let ((*print-array* nil) (*print-pretty* t)
        (*print-circle* t) (*print-escape* t)
        (*print-lines* *inspect-print-lines*)
        (*print-level* *inspect-print-level*)
        (*print-length* *inspect-print-length*)
        (*package* (make-package (gensym))) ; for `read'
        (*inspect-frontend* frontend) (*inspect-depth* -1))
    (inspect-frontend (inspect-backend object) frontend)
    (delete-package *package*)
    (values)))
