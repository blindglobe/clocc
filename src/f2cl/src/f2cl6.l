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
(in-package :user)
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
      (do ((line (read-line inport nil 'eof)
		 (read-line inport nil 'eof)))
	  ((eq line 'eof)
	   outfile-name)
	(let ((line (string-right-trim '(#\Space) line)))
	  (cond 
	    ((string= line "") nil)	; we leave out blank lines
	    ((or (char-equal (char line 0) #\C)
		 (char-equal (char line 0) #\*))
	     (if *comments* (write-comment-line line outport)))
	    (t
	     (when *verbose* (princ line) (terpri))
	     ;; Handle tabs.  If the line begins with a tab, replace it with 6
	     ;; spaces.  Then replace all tabs with a single space.
	     (when (char-equal (aref line 0) #\Tab)
	       (setf line (concatenate 'string "      " (subseq line 1))))
	     (setf line (substitute #\Space #\Tab line))

	     (write-line (adjust_nrs (replace_logl_ops line)) outport))))))))

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
(defun parse-number (prev-char char outport)
  (flet ((my-digit-char-p (char)
	   (and (characterp char)
		(digit-char-p char)))
	 (my-char= (char1 char2)
	   (and (characterp char1)
		(characterp char2)
		(char= char1 char2))))
    (declare (inline my-digit-char-p my-char=))
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
	      (write-char '#\. str) (write-char '#\0 str)
	      (write (get-output-stream-string str) :stream outport :escape nil)
	      (write-char '#\space outport)
	      (return t))
	     ((and (my-char= char '#\.)
		   (my-digit-char-p nxt-char))
	      (write-char '#\. str) 
	      (go loop))
	     ((and (my-char= char '#\.)
		   (not (my-digit-char-p nxt-char)))
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
;-----------------------------------------------------------------------------
; end of f2cl6.l
 
