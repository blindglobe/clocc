;;; File: <animals.lisp - 1999-05-23 Sun 22:08:52 EDT sds@goems.com>
;;;
;;; Guess an Animal - CL implementation.
;;;
;;; Copyright (C) 1997-1999 by Sam Steingold.
;;; This is free software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and the precise copyright document.
;;;
;;; $Id: animals.lisp,v 1.4 1999/05/24 02:13:11 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/animals.lisp,v $
;;; $Log: animals.lisp,v $
;;; Revision 1.4  1999/05/24 02:13:11  sds
;;; (node): new class; this begins a network implementation.
;;; (*network*, *root-node*): new variables.
;;; (symbol->node, resolve, mknode, get-symbol, add-node, play-game):
;;; new functions.
;;; (get-string): doesn't take a `stream' argument anymore.
;;; (fix-question, get-question): new functions.
;;; (anml-finish): use `get-question'.
;;;
;;; Revision 1.3  1999/04/09 19:17:49  sds
;;; Use `string-beg-with' in `anml-add-article'.
;;;
;;; Revision 1.2  1998/05/22 16:34:19  sds
;;; Added `anml-add-article'.
;;;
;;; Revision 1.1  1997/12/08 21:54:03  sds
;;; Initial revision
;;;
;;;

(in-package :cl-user)

(eval-when (load compile eval)
  (sds-require "base") (sds-require "util"))

(defvar animals-debug-output nil "Print more debugging info.")
(defvar animals-debug-use-built-in-data nil "Do not read the file.")
(defvar animals-default-data '("Is it an insect" ("Can it sting" "a bee" .
			       "a roach") "Can it fly" "a duck" . "a penguin")
  "The built-in to be used if `animals-debug-use-built-in-data' is non-nil.")
(defvar animals-data nil "The actual data tree.")
(defvar animals-file-name nil "The data file. Defaults to ~/.animals.")

(defvar animals-data-modified nil "Has the data been modified?")

(defun anml-get-question (old new)
  "Get the question that will distinguish betwee OLD and NEW.
The question will contain `it'."
  (do ((quest nil (if (search "it" quest) quest nil)))
      ((null quest) quest)
    (setq quest
	  (get-string t "What Yes/No question distinguishes between ~a and ~a?"
		      old new))))

(defun get-string (fmt &rest args)
  "Ask for input, return it."
  (apply #'format t fmt args)
  (read-line t))

(defun anml-add-article (str)
  "Add an article in the beginning of the string."
  (declare (simple-string str))
  (setq str (string-trim +whitespace+ str))
  (if (let ((len (length str)))
        (or (string-beg-with "a " str len)
            (string-beg-with "an " str len)
            (string-beg-with "the " str len)))
      str
      (concatenate 'string (if (find (schar str 0) #(#\a #\e #\i #\o #\u)
                                     :test #'char=)
                               "an " "a ") str)))

(defun fix-question (quest)
  (declare (simple-string quest))
  (let ((qq (string-trim +whitespace+ quest)))
    (declare (simple-string qq))
    (if (char= #\? (schar qq (1- (length qq)))) qq
        (concatenate 'string qq "?"))))

(defun get-question (1st 2nd)
  (loop :for quest :of-type simple-string =
        (fix-question
         (get-string "What Yes/No question distinguishes between ~a and ~a?
 ==> " 1st 2nd))
        :for it-pos :of-type (or null index-t) = (search "it" quest)
        :unless it-pos :do (format t "The question must contain `it'.~%")
        :else :return
        (if (y-or-n-p "~a~a~a" (subseq quest 0 it-pos) 1st
                      (subseq quest (+ it-pos 2)))
            (values quest 1st 2nd) (values quest 2nd 1st))))

(defun anml-finish (tail)
  "Endgame."
  (cond ((y-or-n-p "Is it ~a? " tail)
	 (format t "I won!~%") tail)
	(t
         (let ((new (anml-add-article
                     (get-string "I lost...~%What was your animal?..."))))
           (multiple-value-bind (quest 1st 2nd) (get-question new tail)
             (cons quest (cons 1st 2nd)))))))

(defun play-animals ()
  "Play the famous game!"
  (setq animals-data-modified nil)
  ;; read the initial data
  (cond (animals-debug-use-built-in-data
	 (setq animals-data animals-default-data))
	(t
	 (setq animals-file-name
	       (or (getenv "ANIMALS")
		   (merge-pathnames (make-pathname :name ".animals")
				    (user-homedir-pathname))))
	 (format t "~&Using animals file: `~a'...~%" animals-file-name)
	 (setq animals-data
	       (with-open-file (anfl animals-file-name :direction :input
				     :if-does-not-exist nil)
		 (if anfl (prog1 (read anfl) (format t "File read.~%"))
		     (format t "Cannot read `~a'.~%" animals-file-name))))
	 (unless (consp animals-data)
	   (format t "Invalid data: ~a~%Using the default.~%" animals-data)
	   (setq animals-data animals-default-data))))
  (when animals-debug-output
    (format t "animals-data now: ~a~%" animals-data))
  ;; play the game
  (do ((root animals-data animals-data) follow over)
      (over)
    (do () ((not (consp root)))
      (setq follow root
	    root (if (y-or-n-p (car root)) (cadr root) (cddr root))))
    (setf (cdr (or follow animals-data))
	  (if (eq root (cadr follow))
	      (cons (anml-finish root) (cddr follow))
	      (cons (cadr follow) (anml-finish root))))
    (when animals-debug-output
      (format t "animals-data now: ~a~%" animals-data))
    (setq over (y-or-n-p "Quit?")))
    ;; save the new information
  (when animals-debug-output
    (format t "done - saving the information...~%"))
  (cond (animals-debug-use-built-in-data
	 (print "used built-in data -- no save!"))
	(animals-data-modified
	 (let ((*print-readably* t) (*print-circle* t))
	   (with-open-file (anfl animals-file-name :direction :output)
	     (print animals-data anfl) (terpri anfl)))
	 (format t "Wrote file `~a'~%" animals-file-name))
	(t (format t "You taught me no new animals this time...~%")))
  (when (y-or-n-p "Exit lisp?") (quit)))

;;;
;;; Network implementation
;;;

(defclass node ()
  ((name :type symbol :accessor node-name :initarg :name)
   (question :type simple-string :accessor node-question :initarg :question)
   (yes :type symbol :accessor node-yes :initarg :yes)
   (no :type symbol :accessor node-no :initarg :no))
  (:documentation "The information node."))

(defcustom *network* hash-table (make-hash-table :test 'eq)
  "*symbol --> node.")

;(defconst +bad-node+ node (make-instance 'node)
;  "*The convenient constant for init.")

(defun symbol->node (name)
  (declare (symbol name))
  (multiple-value-bind (node found-p) (gethash name *network*)
    (assert found-p (name) "No such node: `~s'" name)
    node))

(defun mknode (name &optional quest yes no)
  (let ((new (if quest (make-instance 'node :name name
                                      :question quest :yes yes :no no)
                 (make-instance 'node :name name))))
    (multiple-value-bind (old found-p) (gethash name *network*)
      (when found-p
        (warn " ** Redefining node `~s' from~% -- ~s~% ** to~% -- ~s~%"
              name old new)))
    (setf (gethash name *network*) new)))

(defcustom *root-node* node (mknode 'root)
  "*The root node, from which the search starts by default.")

(defun resolve (&optional (root *root-node*))
  (declare (type (or node symbol) root))
  (when (symbolp root) (setq root (symbol->node root)))
  (if (slot-boundp root 'question)
      (let* ((ans (if (y-or-n-p (node-question root))
                      (node-yes root) (node-no root)))
             (new (gethash ans *network*)))
        (if new (resolve new) (values ans root)))
      (values (node-name root) root)))

(defun get-symbol (&rest args)
  (intern (string-upcase (apply #'get-string args))))

(defun add-node (name node)
  (declare (symbol name) (type node node))
  (if (eq name (node-name node))
      (setf (node-question node)
            (get-string "What is the question for the `~s' node?" name)
            (node-yes node) (get-symbol "What is the `yes' answer?")
            (node-no node) (get-symbol "What is the `no' answer?"))
      (multiple-value-bind (quest yes no)
          (get-question name (get-symbol "I lost...~%...What was it?"))
        (mknode (let ((new (get-symbol "New node name?")))
                  (cond ((eq name (node-yes node)) (setf (node-yes node) new))
                        ((eq name (node-no node)) (setf (node-no node) new))
                        (t (error 'code :proc 'add-node :args (list name node)
                                  "Bad arguments: name: `~s'; node: ~s"))))
                quest yes no))))

(defun play-game (&optional (root *root-node*))
  (declare (type (or node symbol) root))
  (loop :do (multiple-value-bind (name node) (resolve root)
              (declare (symbol name) (type node node))
              (if (y-or-n-p "Is it ~s?" name)
                  (format t "I won!~%")
                  (add-node name node)))
        :while (y-or-n-p "One more game?")))

(provide "animals")
;;; animals.lisp ends here
