;;; Read/Write CLOS objects
;;; Load this file and you will be able to print CLOS objects with #[] format,
;;; bind `*readtable*' to `+clos-readtable+' and `read' will read #[]
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: closio.lisp,v 1.17 2002/04/21 19:57:45 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/closio.lisp,v $

(eval-when (compile load eval)
  (require :cllib-base (translate-logical-pathname "clocc:src;cllib;base"))
  ;; `class-slot-list', `class-slot-initargs'
  (require :port-sys (translate-logical-pathname "clocc:src;port;sys")))

(in-package :cllib)

(export
 '(*closio-method* +clos-readtable+ make-clos-readtable macroexpand-r))

;;;
;;; {{{ print structure in CMUCL via `print-object'
;;;

#+cmu
(progn
  (defmethod print-object ((xx structure-object) (out stream))
    (kernel:default-structure-print xx out 1))
  (defun print-struct-object (xx out depth)
    (declare (ignore depth))
    (print-object xx out)))

;;;
;;; }}}{{{ read CLOS objects
;;;

(defcustom *closio-method* symbol nil
  "The technique used for CLOS i/o.
Acceptable values are:
  nil         -- use the default (unreadable) technique - #<>.
  :slot-name  -- output each (bound) slot, by name (really readable: `read'
                 will return an `equal' object when `*print-readably*' is T)
  :initarg    -- output only the slots which have an initarg (user-controlled)
This variable should have the same value when reading and writing,
otherwise you will probably get an error.")

(eval-when (compile load eval)  ; CMUCL for `+clos-readtable+'
(defun read-object (st char arg)
  "Read an instance of a CLOS class printed as #[name{ slot val}]"
  (declare (ignore char arg))
  (case *closio-method*
    (:slot-name
     (do* ((all (read-delimited-list #\] st t)) (res (make-instance (car all)))
           (tail (cdr all) (cddr tail)))
          ((endp tail) res)
       (setf (slot-value res (car tail)) (cadr tail))))
    (:initarg (apply #'make-instance (read-delimited-list #\] st t)))
    ((nil) (error 'code :proc 'read-object :args '(*closio-method* nil)
                  :mesg "~s is ~s"))
    (t (error 'case-error :proc 'read-object :args
              (list '*closio-method* *closio-method*
                    :initarg :slot-name nil)))))

(defun make-clos-readtable (&optional (rt (copy-readtable)))
  "Return the readtable for reading #[]."
  (set-syntax-from-char #\[ #\( rt)
  (set-syntax-from-char #\] #\) rt)
  (set-macro-character #\] (get-macro-character #\) rt) nil rt)
  (set-dispatch-macro-character #\# #\[ #'read-object rt)
  rt)
)

(defconst +clos-readtable+ readtable (make-clos-readtable)
  "The readtable for reading CLOS objects printed readably with #[].")

;;;
;;; }}}{{{ print CLOS objects readably
;;;

;; without this, some CL implementations issue a warning or an error
;; about redefining a symbol (`print-object') in a locked package
(unlock-package :common-lisp)

(defmethod print-object ((obj standard-object) (out stream))
  (case *closio-method*
    (:slot-name
     (loop :with cl = (class-of obj)
           :initially (format out "#[~w" (class-name cl))
           :for slot :in (class-slot-list cl nil)
           :when (slot-boundp obj slot)
           :do (format out " ~w ~w" slot (slot-value obj slot))
           :finally (write-string "]" out) (return obj)))
    (:initarg
     (loop :with cl = (class-of obj)
           :initially (format out "#[~w" (class-name cl))
           :for slot :in (class-slot-list cl nil)
           :and init :in (class-slot-initargs cl nil)
           :when (and init (slot-boundp obj slot))
           :do (format out " ~w ~w" init (slot-value obj slot))
           :finally (write-string "]" out) (return obj)))
    (t (call-next-method))))

(restore-package-lock :common-lisp)

;;;
;;; }}}{{{ macroexpand-r
;;;

;;;###autoload
(defun macroexpand-r (form)
  "Recursive macroexpand - unreliable because of `macrolet' &c."
  (if (atom form) form
      (let ((res (macroexpand form)))
        (cons (car res) (mapcar #'macroexpand-r (cdr res))))))

;;; }}}

(provide :cllib-closio)
;;; file closio.lisp ends here
