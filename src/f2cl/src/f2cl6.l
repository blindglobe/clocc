; f2cl6.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;
;;;;;;;;;;Hamilton, New Zealand 1992-95 - all rights reserved;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suite of functions for replacing ".EQ.", ".AND." etc with "=", "AND" etc
;; as a preprocessor for fortran-to-lisp.
;; Numbers such as 6. are replaced with 6.0
;;                 .6                   0.6
;;                 -.6                  -0.6
;;                 6.D0                 6.0D0
;;  etc
;; Also replaces x.xE-x with x.xE~x and x.xE+x with x.xEx so that the lisp reader
;; when set to read FORTRAN can cope.
;; Replaces single quotes (') with double quotes (").

;; Comments are removed

; functions:
;     preprocess
;     adjust_nrs_and_dots
;     parse-logl-op
;     replace-str
;     parse-number
;	write-comment-line
;--------------------------------------------------------------------------
(in-package :f2cl)
(eval-when (compile load eval) (proclaim '(special *verbose* *comments*)))
;--------------------------------------------------------------------------  
; global stream input for parse-number:
(defvar *inp* nil)
;---------------------------------------------------------------------------
		 
(defun preprocess (file &key (outfile-name "prep.tmp"))
  (when *verbose*
    (format t "~&preprocessing begins ...~%"))
  (with-open-file (inport file :direction :input)
    (with-open-file (outport outfile-name
			     :direction :output
			     :if-exists :rename-and-delete)
      (let ((*package* (find-package "F2CL"))
	    (collected-comments nil))
	(do ((prev-line nil)
	     (line (read-line inport nil 'eof)
		   (read-line inport nil 'eof)))
	    ((eq line 'eof)
	     (when prev-line
	       (write-line prev-line outport))
	     outfile-name)
	  ;; Hmm, technically it might not be right to trim spaces from
	  ;; the end of the line.  Consider what happens we are trying
	  ;; to continue a string with trailing spaces.  The resulting
	  ;; string is wrong.
	  (let ((line (string-right-trim '(#\Space) line)))
	    (cond 
	      ((string= line "") nil)	; we leave out blank lines
	      ((or (char-equal (char line 0) #\C)
		   (char-equal (char line 0) #\*))
	       (when *comments*
		 ;; If we don't have anything buffered, we can print
		 ;; out the comment now.  Otherwise, we need to save
		 ;; the comment and print it out later, after we've
		 ;; accumulated the continuation line, if any.
		 (if prev-line
		     (push line collected-comments)
		     (write-comment-line line outport))))
	      (t
	       (when *verbose* (princ line) (terpri))
	       ;; Handle tabs.  If the line begins with a tab, replace it with 6
	       ;; spaces.
	       (when (char-equal (aref line 0) #\Tab)
		 (setf line (concatenate 'string "      " (subseq line 1))))
	       ;; Do we really want to do this?  What if the tab is in a
	       ;; string?  I think tabs are illegal in Fortran, though.
	       (setf line (substitute #\Space #\Tab line))
	       ;; If line is a continuation line, append it.  If not,
	       ;; print out our buffered line.
	       (cond ((or (< (length line) 6)
			  (char-equal (aref line 5) #\Space))
		      ;; Not continuation.  Print out the buffered line, if
		      ;; any, and save this line for later.
		      (when prev-line
			(write-line (adjust_nrs (replace_logl_ops prev-line)) outport))
		      (setf prev-line (copy-seq line))
		      ;; Print out the collected comments, if any
		      (when collected-comments
			(dolist (c (nreverse collected-comments))
			  (write-comment-line c outport))
			(setf collected-comments nil)))
		     (t
		      ;; A continuation line!  Append the line to our
		      ;; buffer, carefully stripping off leading spaces.
		      ;; If the last character of the buffer or the
		      ;; first character of this line is a punctuation,
		      ;; strip off leading spaces.  Otherwise, leave a
		      ;; space between.
		      (let ((punc-list '(#\. #\/))
			    (new-line (string-left-trim '(#\Space) (subseq line 6))))
			(setf prev-line
			      (concatenate 'string prev-line
					   (if (and prev-line
						    (or (member (aref prev-line (1- (length prev-line))) punc-list)
							(member (aref new-line 0) punc-list)))
					       ""
					       " ")
					   new-line)))))))))))))

;--------------------------------------------------------------------------
(defun write-comment-line (line outport)
 (if *verbose* 
   (format t "      FORTRAN_COMMENT ~S~%" (string-downcase (subsequence line 1))))
 (format outport "      FORTRAN_COMMENT ~S~%" (string-downcase (subsequence line 1))))

(defun replace_logl_ops  (line)
   (prog (inport outport char)
     (setq inport (make-string-input-stream line)
           outport (make-string-output-stream))
     loop
     (setq char (read-char inport nil 'EOF t))
     (cond ((eql char 'EOF) 
            (return (get-output-stream-string outport)))
           ((char= char '#\.) 
            (parse-logl-op inport outport)
            (go loop))
           (t (write-char char outport)
              (go loop)))))
;--------------------------------------------------------------------------

(defun adjust_nrs (line)
   (prog (outport char prev-char)
     (setq prev-char '#\space
           *inp* (make-string-input-stream line)
           outport (make-string-output-stream))
     loop
     (setq char (read-char *inp* nil 'EOF t))
     (cond ((eql char 'EOF) )

           ; check for single quote and pass over all char until next one
           ((char= char '#\')
            (setq prev-char '#\space)
            (write-char '#\" outport)
            (do ((char (read-char *inp* nil 'EOF t) 
                       (read-char *inp* nil 'EOF t)))
                ((char= char '#\') (write-char '#\" outport))
                (write-char char outport))
            (go loop))

           ; check for non-complex number 
           ((parse-number prev-char char outport)
            (setq prev-char '#\space)
            (go loop))

           (t (write-char char outport)
              (if (digit-char-p char)
                  (setq prev-char '#\?)
                  (setq prev-char char))
              (go loop)))
     (return (get-output-stream-string outport))))
;------------------------------------------------------------------------------

(defun parse-logl-op (inport outport)
   (prog (str nxt-char logl-op)
      (setq nxt-char (peek-char nil inport nil 'EOF #+broken-peek-char t))
      (if (or (member nxt-char `(EOF #\space #\' #\.)) 
              (digit-char-p nxt-char))
          (and (write-char '#\. outport)
               (return)))
      (setq str (make-string-output-stream))
      loop
        (write-char (read-char inport nil 'EOF t) str)
        (setq nxt-char (peek-char nil inport nil 'EOF #+broken-peek-char t))
        (if (and (not (eql nxt-char 'EOF)) 
                      (char/= nxt-char `#\.)) (go loop))
      (cond ((setq str (get-output-stream-string str)
                   logl-op (replace-str str))
             (read-char inport nil 'EOF t) ; read final dot
             (write-char '#\space outport)
             (write logl-op :stream outport) ; write the converted string with spaces
             (write-char '#\space outport))
            (t (write-char '#\. outport) ; put the dot back
               (write  str :stream outport :escape nil))))) ; put everything else back

;------------------------------------------------------------------------------
(defun replace-str (str)
   (cond ((equalp str "GT")    '>)
         ((equalp str "GE")    '>=)
         ((equalp str "EQ")    'equal)
         ((equalp str "LE")    '<=)
         ((equalp str "LT")    '<)
         ((equalp str "NE")    '><)
         ((equalp str "NOT")   'NOT)
         ((equalp str "AND")   'AND)
         ((equalp str "OR")    'OR)
         ((equalp str "TRUE")  '%TRUE%)
         ((equalp str "T")     '%TRUE%)
         ((equalp str "FALSE") '%FALSE%)
         ((equalp str "F")     '%FALSE%)
         ((equalp str "EQV")   'logeqv)
         ((equalp str "NEQV")  'logxor)
         (t nil)))
;------------------------------------------------------------------------------
;inport
#+nil
(defun parse-number (prev-char char outport)
  (flet ((my-digit-char-p (char)
	   (and (characterp char)
		(digit-char-p char)))
	 (my-char= (char1 char2)
	   (and (characterp char1)
		(characterp char2)
		(char= char1 char2))))
    #-clisp(declare (inline my-digit-char-p my-char=))
    (prog (str nxt-char)
       ;; first check prev-char suitable to precede a number
       (if (not (member prev-char 
			'(#\newline #\space #\. #\= #\+ #\- #\* #\/ #\( #\, )))
	   (return nil))
       (setq str (make-string-output-stream))
       ;; examine first char and peek at second to ascertain this is a number
       (setq nxt-char (peek-char nil *inp* nil 'EOF #+broken-peek-char t))
       (cond ((and (my-digit-char-p char) 
		   (or (and (not (eql nxt-char 'EOF)) 
			    (my-digit-char-p nxt-char))
		       (member nxt-char '(#\. #\D #\E))))
	      (write-char char str) )
	     ((and (or (my-char= char '#\+) (my-char= char '#\-))
		   (my-char= nxt-char '#\.))
	      (write-char char str) (write-char '#\0 str) )
	     ((and (or (my-char= char '#\+) (my-char= char '#\-))
		   (my-digit-char-p nxt-char))
	      (write-char char str))
	     ((and (my-char= char '#\.) (my-digit-char-p nxt-char))
	      (write-char '#\0 str) (write-char '#\. str) )
	     (t (return nil)))
       ;; parse rest of number;EOF
       loop
       (setq char (read-char *inp* nil 'EOF t)
	     nxt-char (peek-char nil *inp* nil 'EOF #+broken-peek-char t))
       (if (eql nxt-char 'EOF) (setq nxt-char `#\space))
       (cond ((eql char 'EOF)
	      ;; Assume number is complete.
	      (write (get-output-stream-string str) :stream outport :escape nil)
	      (write-char '#\space outport)
	      (return t))
	     ((my-digit-char-p char)
	      (write-char char str) 
	      (go loop))
	     ((and (my-char= char '#\.)
		   (not (or (my-digit-char-p nxt-char)
			    (my-char= nxt-char '#\D)
			    (my-char= nxt-char '#\E))))
	      ;; Convert "0." to "0.0"
	      (write-char '#\. str) (write-char '#\0 str)
	      (go loop))
	     ((and (my-char= char '#\.)
		   (my-digit-char-p nxt-char))
	      ;; We have "n.n"
	      (write-char '#\. str) 
	      (go loop))
	     ((and (my-char= char '#\.)
		   (not (my-digit-char-p nxt-char)))
	      ;; We have "." followed by some non digit.  Append a 0
	      ;; to the number.
	      (write-char '#\. str) (write-char '#\0 str)
	      (go loop))
	     ((char-equal char '#\E)
	      (write-char '#\e str)
	      (cond ((my-char= nxt-char '#\+) (read-char *inp* nil 'EOF t))
		    ((my-char= nxt-char '#\-) (read-char *inp* nil 'EOF t)
		     (write-char '#\% str)))
	      (go loop))
	     ((char-equal char '#\D) 
	      (write-char '#\d str)
	      (cond ((my-char= nxt-char '#\+) (read-char *inp* nil 'EOF t))
		    ((my-char= nxt-char '#\-) (read-char *inp* nil 'EOF t)
		     (write-char '#\% str)))
	      (go loop))
	     ((or (my-char= char '#\-) (my-char= char '#\+))
	      (write-char char str) 
	      (go loop))
	     ((my-char= char #\Space)
	      ;; Spaces embedded within a number don't terminate the
	      ;; number so skip over the space.  (Spaces in Fortran
	      ;; are "invisible".)
	      (go loop))
	     (t				;number complete
	      #+clisp 
	      (if (equal (peek-char nil *inp* nil 'EOF t) 'EOF)
		  (setq *inp* (make-string-input-stream (string char)))
		  (setq *inp* 
			(make-string-input-stream
			 (concatenate 'string (string char) (read-line *inp*)))))
	      #-clisp (unread-char char *inp*)
	      (write (get-output-stream-string str) :stream outport :escape nil)
	      (write-char '#\space outport)
	      (return t))))))

(defun parse-number (prev-char char outport)
  (labels ((my-digit-char-p (char)
	     (and (characterp char)
		  (digit-char-p char)))
	   (my-char= (char1 char2)
	     (and (characterp char1)
		  (characterp char2)
		  (char= char1 char2)))
	   (skip-spaces ()
	     (do ((nxt (peek-char nil *inp* nil 'EOF)
		       (peek-char nil *inp* nil 'EOF)))
		 ((not (my-char= nxt #\Space))
		  nxt)
	       (read-char *inp* nil 'EOF t))))
    #-clisp(declare (inline my-digit-char-p my-char=))
    ;; first check prev-char suitable to precede a number
    (unless (member prev-char 
		    '(#\newline #\space #\. #\= #\+ #\- #\* #\/ #\( #\, ))
      (return-from parse-number nil))
    (let ((str (make-string-output-stream))
	  ;; examine first char and peek at second to ascertain this
	  ;; is a number.  Skip over spaces if this looks like it
	  ;; starts a number.
	  (nxt-char (when (or (my-digit-char-p char)
			      (member char '(#\. #\+ #\-)))
		      (skip-spaces)
		      (peek-char nil *inp* nil 'EOF #+broken-peek-char t))))
      (cond ((and (my-digit-char-p char) 
		  (or (and (not (eql nxt-char 'EOF))
			   (my-digit-char-p nxt-char))
		      (member nxt-char '(#\. #\D #\E))))
	     (write-char char str))
	    ((and (or (my-char= char '#\+)
		      (my-char= char '#\-))
		  (my-char= nxt-char '#\.))
	     (write-char char str)
	     (write-char '#\0 str))
	    ((and (or (my-char= char '#\+)
		      (my-char= char '#\-))
		  (my-digit-char-p nxt-char))
	     (write-char char str))
	    ((and (my-char= char '#\.)
		  (my-digit-char-p nxt-char))
	     (write-char '#\0 str)
	     (write-char '#\. str) )
	    (t
	     (return-from parse-number nil)))
      ;; parse rest of number;EOF
      (loop
	(skip-spaces)
	(setq char (read-char *inp* nil 'EOF t)
	      nxt-char (peek-char nil *inp* nil 'EOF #+broken-peek-char t))
	;; Skip over spaces if the current character is a part of a number.
	(when (and (eql nxt-char #\Space)
		   (or (my-digit-char-p char)
		       (member char '(#\E #\D #\- #\+ #\e #\d #\.))))
	  (skip-spaces)
	  (setq nxt-char (peek-char nil *inp* nil 'EOF)))
	(when (eql nxt-char 'EOF)
	  (setq nxt-char `#\space))
	(cond ((eql char 'EOF)
	       ;; Assume number is complete.
	       (write (get-output-stream-string str) :stream outport :escape nil)
	       (write-char '#\space outport)
	       (return-from parse-number t))
	      ((my-digit-char-p char)
	       (write-char char str))
	      ((and (my-char= char '#\.)
		    (not (or (my-digit-char-p nxt-char)
			     (my-char= nxt-char '#\D)
			     (my-char= nxt-char '#\E))))
	       ;; Convert "n." to "n.0" if there's no following number.

	       ;;(format t "inserting 0: char, nxt = ~S ~S~%" char nxt-char)
	       (write-char '#\. str)
	       (write-char '#\0 str))
	      ((and (my-char= char '#\.)
		    (my-digit-char-p nxt-char))
	       ;; We have "n.n"
	       (write-char '#\. str))
	      ((and (my-char= char '#\.)
		    (not (my-digit-char-p nxt-char)))
	       ;; We have "." followed by some non-digit or non-space.
	       ;; Append a 0 to the number.

	       ;;(format t "inserting 0: char, nxt = ~S ~S~%" char nxt-char)
	       (write-char '#\. str)
	       (write-char '#\0 str))
	      ((char-equal char '#\E)
	       (write-char '#\e str)
	       ;; Process exponent, skip over white space.
	       (when (my-char= nxt-char #\Space)
		 (skip-spaces)
		 (setf nxt-char (peek-char nil *inp* nil 'EOF)))
	       (cond ((my-char= nxt-char '#\+)
		      (read-char *inp* nil 'EOF t))
		     ((my-char= nxt-char '#\-)
		      (read-char *inp* nil 'EOF t)
		      (write-char '#\% str))))
	      ((char-equal char '#\D) 
	       (write-char '#\d str)
	       (when (my-char= nxt-char #\Space)
		 (skip-spaces)
		 (setf nxt-char (peek-char nil *inp* nil 'EOF)))
	       (cond ((my-char= nxt-char '#\+)
		      (read-char *inp* nil 'EOF t))
		     ((my-char= nxt-char '#\-)
		      (read-char *inp* nil 'EOF t)
		      (write-char '#\% str))))
	      ((or (my-char= char '#\-)
		   (my-char= char '#\+))
	       (write-char char str))
	      (t			;number complete
	       ;; Clisp doesn't have unread-char?!?
	       #+(and nil clisp)
	       (if (equal (peek-char nil *inp* nil 'EOF t) 'EOF)
		   (setq *inp* (make-string-input-stream (string char)))
		   (setq *inp* 
			 (make-string-input-stream
			  (concatenate 'string (string char) (read-line *inp*)))))
	       #-(and nil clisp) (unread-char char *inp*)
	       (write (get-output-stream-string str) :stream outport :escape nil)
	       (write-char '#\space outport)
	       (return-from parse-number t)))))))
;;;-----------------------------------------------------------------------------
;;; end of f2cl6.l
;;;
;;; $Id: f2cl6.l,v 1.18 2000/08/13 04:18:44 rtoy Exp $
;;; $Log: f2cl6.l,v $
;;; Revision 1.18  2000/08/13 04:18:44  rtoy
;;; Make PARSE-NUMBER handle sapces within numbers, which is perfectly
;;; acceptable Fortran.  (May be a bit buggy still; probably should
;;; rewrite PARSE-NUMBER.)
;;;
;;; Revision 1.17  2000/08/01 23:02:36  rtoy
;;; We were messing up the printing of comments after we changed
;;; PREPROCESS to handle continuation lines.  We need to accumulate
;;; comment lines until we know we have a complete line (with all
;;; continuation lines).  Then we can print out the line followed by the
;;; accumulated comment lines.  This makes the comments possible out of
;;; sync with the generated Lisp code, but they were never really in sync
;;; anyway.
;;;
;;; Revision 1.16  2000/07/28 17:07:11  rtoy
;;; o We are in the f2cl package now.
;;; o When we preprocess the file, we want to READ using the F2CL package
;;;   so the symbols are in our package.  (I think that's what I want.)
;;;
;;; Revision 1.15  2000/07/27 16:39:17  rtoy
;;; We want to be in the CL-USER package, not the USER package.
;;;
;;; Revision 1.14  2000/07/21 21:15:17  rtoy
;;; Fix a typo in the punctuation list and add "/" to the list.
;;;
;;; Revision 1.13  2000/07/21 17:43:17  rtoy
;;; Need to be more careful about continuation lines.  We now leave a
;;; space between lines when concatenating them, except when the previous
;;; line ends with punctuation, or the current line begins with
;;; punctuation.  This is intended to handle cases like:
;;;
;;; 	double precision
;;;        & x
;;;
;;; from being converted to
;;;
;;; 	double precisionx
;;;
;;; which is wrong.  We want a space before the x.
;;;
;;; We want to handle these cases to, though:
;;;
;;; 	x = y .le
;;;        &. 42
;;;
;;; which shouldn't get a space between lines because we want "x = y
;;; .le. 42", not the invalid "x = y .le . 42".
;;;
;;; Revision 1.12  2000/07/13 16:55:34  rtoy
;;; To satisfy the Copyright statement, we have placed the RCS logs in
;;; each source file in f2cl.  (Hope this satisfies the copyright.)
;;;
;;;-----------------------------------------------------------------------------
