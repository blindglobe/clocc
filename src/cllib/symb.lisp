;;; Symbols & Keywords
;;;
;;; Copyright (C) 1997-2000 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
;;;
;;; $Id: symb.lisp,v 1.2 2000/03/27 20:02:54 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/symb.lisp,v $

(eval-when (compile load eval)
  (require :base (translate-logical-pathname "clocc:src;cllib;base")))

(in-package :cllib)

(export '(symbol-concat +kwd+ kwd keyword-concat read-key keyword=))

(eval-when (load compile eval)
  (declaim (optimize (speed 3) (space 0) (safety 3) (debug 3))))

;;(defmacro symbol-concat (&rest args)
;;  (let ((lst (mapcar (lambda (zz) (if (stringp zz) zz `(string ,zz))) args)))
;;    `(intern (concatenate 'string ,@lst))))

(defsubst symbol-concat (&rest args)
  "Concatenate objects into a symbol."
  (intern (apply #'concatenate 'string (mapcar #'string args))))

(defconst +kwd+ package (find-package :keyword) "The KEYWORD package.")

(defsubst kwd (sy-st)
  "Convert the argument, symbol or string, to a keyword."
  (declare (type (or symbol string) sy-st) (values keyword))
  (when (symbolp sy-st) (unintern sy-st) (setq sy-st (symbol-name sy-st)))
  (intern sy-st +kwd+))

(defsubst keyword-concat (&rest args)
  "Concatenate objects into a keyword."
  (declare (values keyword))
  (kwd (apply #'concatenate 'string (mapcar #'string args))))

(defsubst read-key (stream)
  "Read the symbol and make it a keyword."
  (declare (stream stream) (values keyword))
  (let ((*package* +kwd+)) (read stream)))

(defun keyword= (key1 key2 &key (suffix1 nil suff1p) (suffix2 nil suff2p))
  "Check whether the two keywords with the suffixes are the same."
  (eq (if suff1p (keyword-concat (code key1) (code suffix1)) (code key1))
      (if suff2p (keyword-concat (code key2) (code suffix2)) (code key2))))

(provide :symb)
;;; file symb.lisp ends here
