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
#+xxxx
(defvar intrinsic_function_names
  '(int ifix idint real float sngl dble cmplx ichar char aint dint
    anint dnint nint idnint iabs abs dabs cabs mod amod dmod isign sign dsign
    idim dim ddim dprod max max0 amax1 dmax1 amax0 amax1 min min0 amini dmini
    amini min1 len index lge lgt lle llt aimag conjg sqrt dsqrt csqrt 
    exp dexp cexp log alog dlog clog log10 alog10 dlog10 sin dsin csin
    cos dcos ccos tan dtan asin dasin acos dacos atan datan atan2 datan2
    sinh dsinh cosh dcosh tanh dtanh))
; global stream input for parse-number:
(defvar *inp* nil)
;---------------------------------------------------------------------------
(defun preprocess (file &key (outfile-name "prep.tmp"))
   (prog (inport outport line)
(if *verbose* (format t "~&preprocessing begins ...~%"))
     (setq inport (infile file)
           outport (outfile outfile-name))
   loop
     (setq line (read-line inport nil 'EOF))
     (when (eq line 'EOF) 
           (close outport)
           (close inport)
           (return outfile-name))
     (setq line (string-upcase (string-right-trim '(#\Space) line)))
     (cond 
           ((string= line "") nil) ; we leave out blank lines
           ((char-equal (char line 0) '#\C) 
            (if *comments* (write-comment-line line outport)))
           (t (when *verbose* (princ line) (terpri))
              (write-line (adjust_nrs (replace_logl_ops line)) outport)))
     (go loop)))
;--------------------------------------------------------------------------
(defun write-comment-line (line outport)
 (if *verbose* 
   (format t "      COMMENT \"~a\"~%" (string-downcase (subsequence line 1))))
 (format outport "      COMMENT \"~a\"~%" (string-downcase (subsequence line 1))))

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
      (setq nxt-char (peek-char nil inport nil 'EOF t))
      (if (or (member nxt-char `(EOF #\space #\' #\.)) 
              (digit-char-p nxt-char))
          (and (write-char '#\. outport)
               (return)))
      (setq str (make-string-output-stream))
      loop
        (write-char (read-char inport nil 'EOF t) str)
        (setq nxt-char (peek-char nil inport nil 'EOF t))
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
   (cond ((equal str "GT")    '>)
         ((equal str "GE")    '>=)
         ((equal str "EQ")    'equal)
         ((equal str "LE")    '<=)
         ((equal str "LT")    '<)
         ((equal str "NE")    '><)
         ((equal str "NOT")   'NOT)
         ((equal str "AND")   'AND)
         ((equal str "OR")    'OR)
         ((equal str "TRUE")  'TRUE)
         ((equal str "T")     'TRUE)
         ((equal str "FALSE") 'FALSE)
         ((equal str "F")     'FALSE)
         ((equal str "EQV")   'logeqv)
         ((equal str "NEQV")  'logxor)
         (t nil)))
;------------------------------------------------------------------------------
;inport
(defun parse-number (prev-char char outport)
   (prog (str nxt-char)
      ; first check prev-char suitable to precede a number
      (if (not (member prev-char 
                       '(#\newline #\space #\. #\= #\+ #\- #\* #\/ #\( #\, )))
          (return nil))
      (setq str (make-string-output-stream))
      ; examine first char and peek at second to ascertain this is a number
      (setq nxt-char (peek-char nil *inp* nil 'EOF t))
      (cond ((and (digit-char-p char) 
                  (or (and (not (equalp nxt-char 'EOF)) 
                                (digit-char-p nxt-char))
                      (member nxt-char '(#\. #\D #\E))))
             (write-char char str) )
            ((and (or (char= char '#\+) (char= char '#\-))
                  (char= nxt-char '#\.))
             (write-char char str) (write-char '#\0 str) )
            ((and (or (char= char '#\+) (char= char '#\-))
                  (digit-char-p nxt-char))
             (write-char char str))
            ((and (char= char '#\.) (digit-char-p nxt-char))
             (write-char '#\0 str) (write-char '#\. str) )
            (t (return nil)))
     ; parse rest of number;EOF
    loop
     (setq char (read-char *inp* nil 'EOF t)
           nxt-char (peek-char nil *inp* nil 'EOF t))
     (if (eql nxt-char 'EOF) (setq nxt-char `#\space))
     (cond ((eql char 'EOF) 
            (write (get-output-stream-string str) :stream outport :escape nil)
            (write-char '#\space outport)
            (return t))
           ((digit-char-p char)
             (write-char char str) 
             (go loop))
           ((and (char= char '#\.) (not (or (digit-char-p nxt-char)
                                         (char= nxt-char '#\D)
                                         (char= nxt-char '#\E))))
            (write-char '#\. str) (write-char '#\0 str)
            (write (get-output-stream-string str) :stream outport :escape nil)
            (write-char '#\space outport)
            (return t))
           ((and (char= char '#\.) (digit-char-p nxt-char))
            (write-char '#\. str) 
            (go loop))
           ((and (char= char '#\.) (not (digit-char-p nxt-char)))
            (write-char '#\. str) (write-char '#\0 str)
            (go loop))
           ((char= char '#\E)
            (write-char '#\e str)
            (cond ((char= nxt-char '#\+) (read-char *inp* nil 'EOF t))
                  ((char= nxt-char '#\-) (read-char *inp* nil 'EOF t)
                                    (write-char '#\% str)))
            (go loop))
           ((char= char '#\D) 
            (write-char '#\d str)
            (go loop))
           ((or (char= char '#\-) (char= char '#\+))
            (write-char char str) 
            (go loop))
           (t ;number complete
#+clisp 
            (if (equal (peek-char nil *inp* nil 'EOF t) 'EOF)
                (setq *inp* (make-string-input-stream (string char)))
                (setq *inp* 
                   (make-string-input-stream
                     (concatenate 'string (string char) (read-line *inp*)))))
#-clisp (unread-char char *inp*)
            (write (get-output-stream-string str) :stream outport :escape nil)
            (write-char '#\space outport)
            (return t))))) 
;-----------------------------------------------------------------------------
; end of f2cl6.l
 
