;;; File: <animals.lisp - 1997-12-08 Mon 16:54:02 EST - sds@wintermute.eagle>
;;;
;;; Guess an Animal - CL implementation.
;;;
;;; Copyright (C) 1997 by Sam Steingold.
;;; This is free software.
;;; GNU General Public License v.2 (GPL2) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code. See <URL:http://www.gnu.org>
;;; for details and precise copyright document.
;;;
;;; $Id: animals.lisp,v 1.1 1997/12/08 21:54:03 sds Exp $
;;; $Source: /cvsroot/clocc/clocc/src/cllib/animals.lisp,v $
;;; $Log: animals.lisp,v $
;;; Revision 1.1  1997/12/08 21:54:03  sds
;;; Initial revision
;;;
;;;

(proclaim '(optimize (speed 3) (space 3) (safety 0) (debug 0)))

(defvar animals-debug-output nil "Print more debugging info.")
(defvar animals-debug-use-built-in-data nil "Do not read the file.")
(defvar animals-default-data '("Is it an insect" ("Can it sting" "a bee" .
			       "an roach") "Can it fly" "a duck" . "a penguin")
  "The built-in to be used if `animals-debug-use-built-in-data' is non-nil.")
(defvar animals-data nil "The actual data tree.")
(defvar animals-file-name nil "The data file. Defaults to ~/animals.")

(defvar animals-data-modified nil "Has the data been modified?")

(defun anml-get-question (old new)
  "Get the question that will distinguish betwee OLD and NEW.
The question will contain `it'."
  (do ((quest nil (if (search "it" quest) quest nil)))
      ((null quest) quest)
    (setq quest
	  (get-string t "What Yes/No question distinguishes between ~a and ~a?"
		      old new))))

(defun get-string (stream fmt &rest args)
  "Ask for input, return it."
  (apply #'format stream fmt args)
  (read-line stream))

(defun anml-finish (tail)
  "Endgame."
  (cond ((y-or-n-p "Is it ~a? " tail)
	 (format t "I won!~%") tail)
	(t
	 (let ((new (get-string t "I lost...~%What was your animal?..."))
	       quest res)
	   (do () (quest)
	     (setq quest
		   (get-string
		    t "What Yes/No question distinguishes between ~a and ~a?
 ==> " tail new)
		   res (search "it" quest))
	     (unless res
	       (setq quest nil)
	       (format t "The question must contain `it'.~%")))
	   (setq animals-data-modified t)
	   (when animals-debug-output
	     (format t "~%question: `~a'~%tail: ~a~%new: ~a~%" quest tail new))
	   (cons quest
		 (if (y-or-n-p "~a~a~a? " (subseq quest 0 res) tail
			       (subseq quest (+ res 2)))
		     (cons tail new) (cons new tail)))))))

(defun play-animals()
  "Play the famous game!."
  (setq animals-data-modified nil)
  ;; read the initial data
  (cond (animals-debug-use-built-in-data
	 (setq animals-data animals-default-data))
	(t
	 (setq animals-file-name
	       (or (system::getenv "animals")
		   (concatenate 'string (namestring (user-homedir-pathname))
				"animals")))
	 (format t "Using animals file: `~a'...~%" animals-file-name)
	 (setq animals-data
	       (with-open-file (anfl animals-file-name :direction :input
				     :if-does-not-exist nil)
		 (prog1 (read anfl) (format t "File read.~%"))))
	 (unless (consp animals-data)
	   (format t "Invalid data: ~a~%Using the dafault.~%" animals-data)
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
  (get-string t "Press <Enter> to leave lisp...")
  (exit))

