
 ; f2cl1.l
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Copyright (c) University of Waikato;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Hamilton, New Zealand 1992-95 - all rights reserved;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;check
;functions:
;errorset - commented out
;	f2cl
;	concaten
;	f-to-l
;---------------------------------------------------------------------------
(in-package :user)
(defvar *verbose* nil)
(defvar *comments* nil)
(eval-when (compile load eval)
  (proclaim '(special *readtable* *sentable*))
  (proclaim '(special *intrinsic_function_names* *external_function_names*
	      *undeclared_vbles* *declared_vbles* *implicit_vble_decls*
	      *subprog-arglist* *data-init*
	      *explicit_vble_decls* *function-flag* *key_params*
	      *save_vbles* *program-flag* *subprog_name*
	      *subprog_common_vars*  *common_array_dims*
	      *format_stmts* *current_label*
	      *subprog_stmt_fns* *subprog_stmt_fns_bodies* *prune_labels*
	      *auto-save-data*
	      *functions-used*
	      *vble-declaration-done*)))

(defvar *common-blocks* (make-hash-table)
  "Hash table of all common blocks.  The key is the name of the common
block; the value is a list of all variables in the common block.")

(defvar *common-block-initialized* '())
(defvar *relaxed-array-decls* t
  "If T, array declarations with sizes are relaxed to be any size.")
(defvar *coerce-assignments* :never
  "This controls how assignment statements are coerced.  If T or
:always, assignments are always ccoerced.  If NIL or :never,
assignments are never coerced.  For any other value, coercion is done
only when needed. ")
(defvar *default-lisp-extension* "lisp"
  "The default extension (file type) for the output file name")

(defvar *fortran-extensions* '("f" "for")
  "A list of possible extensions for Fortran files.")
;----------------------------------------------------------------------------

(defun f2cl (input-file &key output-file verbose prune-labels include-comments
			(auto-save t) (relaxed-array-decls t)
			(coerce-assigns :as-needed)
			(extension (or *default-lisp-extension* "lisp"))
			keep-temp-file)
  "Fortran to Common Lisp converter

INPUT-FILE               File containing Fortran code

:OUTPUT-FILE             File to contain Lisp code

:VERBOSE                 verbose output. Default = NIL.
:PRUNE-LABELS            Prune unused labels. Default = NIL.
:INCLUDE-COMMENTS        Include Fortran comments in the Lisp output (May be buggy.)
                         Default = NIL
:AUTO-SAVE               Variables in DATA statements are automatically SAVE'd.  Default = T.
:RELAXED-ARRAY-DECLS     Declarations of array sizes are relaxed in formal parameters
                         to functions.  Default = T.
:COERCE-ASSIGNS          If T or :ALWAYS, all assignment statements automatically coerce the
                         RHS to the appropriate type for the assignment.  If NIL or :NEVER,
                         coercion never happens.  Otherwise, coercion happens as needed.
                         The Default = :AS-NEEDED
:EXTENSION               The extension to use for the output file, if needed.  Defaults to
                         *DEFAULT-LISP-EXTENSION* or \"lisp\"
:KEEP-TEMP-FILE          If T, the temporary file is not deleted.  Default = NIL.
"


  (format t "Copyright(c) 92-95 University of Waikato - all rights reserved~%")
  (format t "1997, 1999 Many changes and fixes by Raymond Toy (toy@rtp.ericsson.se)~%")

  (let ((*verbose* verbose)
	(*prune_labels* prune-labels)
	(*comments* include-comments)
	(*auto-save-data* auto-save)
	(*common-block-initialized* nil)
	(*relaxed-array-decls* relaxed-array-decls)
	(*coerce-assignments* coerce-assigns))

    (unless (probe-file input-file)
      ;; Can't find it, so look for some other possibilities
      (do* ((ext *fortran-extensions* (rest ext))
	    (path (merge-pathnames input-file (make-pathname :type (first ext)))
		  (merge-pathnames input-file (make-pathname :type (first ext)))))
	  ((or (null ext)
	       (probe-file path))
	   (setf input-file path))))
      
    (unless output-file
      (let ((input-path (pathname input-file)))
	(setf output-file (make-pathname :host (pathname-host input-path)
					 :device (pathname-device input-path)
					 :directory (pathname-directory input-path)
					 :name (pathname-name input-path)
					 :type extension))))
    (format t ";; ~S -> ~S~%" input-file output-file)
    (let ((processed-file (preprocess input-file)))
      (fortran-to-lisp processed-file output-file)
      (unless keep-temp-file
	(delete-file processed-file))
      (values output-file))))
;---------------------------------------------------------------------------
(defun process-data (x) 
   (print x)
   (fortran-to-lisp 
     (concatenate 'string 
        "[.xnr]" (string-downcase (princ-to-string x)) ".for")
     (concatenate 'string 
        "[.lnr]" (string-downcase (princ-to-string x)) ".l")))
;-----------------------------------------------------------------------------  
; not the same as Senac's concat:
 (defun concaten (x &rest more-args)
  (intern
    (apply #'concatenate 'string
      (princ-to-string x) (mapcar #'princ-to-string more-args)) 
    (find-package :user)))
;----------------------------------------------------------------------------
  
; utilities
;	fortran-to-lisp
;       readsubprog-extract-format-stmts
;       translate-and-write-subprog
;	translate-line
;	setsyntax
;	single-macro-character
;	set-fortran-read
;	setlispread
;	lineread
;	read-six-chars
;	introduce-continue
;	find-do
;----------------------------------
; identifiers
;       id-definition-prog
;	id-definition-sub
;	id-definition-fun-typed
;	id-definition-fun
;	id-declaration
;	id-parameter
;       id-implicit-decl
;	id-assignment
;	id-subroutine-call
;	id-do-loop
;       id-pause
;       id-return
;       id-predicate
;	id-if
;	id-endif
;       id-if-goto
;	id-if-then
;	id-goto
;       id-continue
;-----------------------------------
; parsers
;       parse-prog-definition
;	parse-subr-definition
;       parse-typed-fun-definition
;       parse-fun-definition
;       parse-declaration
;       parse-implicit-decl
;	parse-parameter
;	parse-assignment
;	parse-expression
;       parse-pause
;	parse-subroutine-call
;	parse-do-loop
;	parse-if
;       parse-if-goto
;	parse-if-then
;	parse-return
;	parse-goto
;       parse-save
;       parse-common
;       parse-char-decl
;       parse-data
;       parse-data1
;	princ-reset
;-------------------------------------
; parsing utilities
;       extract-atoms
;       tail-chop
;       head-chop
;       list-split
;       gen-list-split
;       list-split-multi
;       list-split-bin
;       concat-operators
;       brackets-check
;	subsequence
;-------------------------------------
; matching
;	binding-value
;	variablep
;	variable-value
;	unify - returns fail or a bindings list/nil
;	match - returns multiple values or nil
;	maybe-extend-bindings
;------------------------------------------------------------------------------

(defun fortran-to-lisp (file ofile)
  (let (*common_array_dims* *format_stmts*)
    (when *verbose*
      (format t "beginning the main translation ...~%"))
    (with-open-file (inport file :direction :input)
      (with-open-file (outport ofile :direction :output 
			       :if-exists :rename-and-delete)
        (setq *common_array_dims* nil)
        (do ((char (peek-char nil inport nil 'EOF) 
                   (peek-char nil inport nil 'EOF)))
            ((eq char 'EOF) )
          (setq *format_stmts* nil)
	  (let ((*print-level* nil)
		(*print-length* nil))
	    (translate-and-write-subprog 
	     (introduce-continue (readsubprog-extract-format-stmts inport)) 
	     outport)))
        ))
    t))
;-----------------------------------------------------------------------------
(defun readsubprog-extract-format-stmts (inport)
 (prog (input-list output-list (margin nil) (multiple-line-flag nil)
        current-line *current_label*)
    (if *verbose* (format t "~&extracting format statements ...~%"))
main-loop 

   ; label
   (if (not multiple-line-flag)
       (setq *current_label* 
            (if (integerp (read-from-string (coerce margin 'string) nil))
                (read-from-string (coerce margin 'string) nil)
                nil)))

   ;; Make sure we aren't hosed if we break out of this!
   (unwind-protect
	(progn
	  (set-fortran-read)
	  ;; read body of a line
	  (setq input-list 
		(if multiple-line-flag 
		    (progn (setq multiple-line-flag nil)
			   (setq current-line (lineread inport))
			   (append input-list current-line))
		    (lineread inport)))
	  ;; read newline character
	  (read-char inport nil 'EOF t) 

	  ;;(format t "~% input-list: ~S" input-list)
	  )
     (setlispread))
   ; read the next margin
   (cond ((not (eq (peek-char nil inport nil 'EOF) 'EOF))
          (setq margin (read-six-chars inport))
          (when (not (char-equal (nth 5 margin) #\space)) ;line continuation
                (setq multiple-line-flag t)
                (go main-loop)))
         (t (setq margin nil)))

   ; extract format-stmts
   (cond ((eq (car input-list) 'FORMAT)
          (parse-format (brackets-check (concat-operators input-list))))
         (t (setq output-list (cons (list *current_label* input-list) 
                                    output-list))))

   ; check for end of subprogram
   (if (and (eq (car input-list) 'end) (null (cdr input-list)))
          (return (nreverse output-list)))
  (go main-loop)))
;------------------------------------------------------------------------------
(defun introduce-continue (prog-list) ; ((margin line) (margin line) ...)
 (prog (ret labels next rest)
  (setq labels (remove nil (mapcar #'find-do prog-list)))
  (setq next (first prog-list) rest (rest prog-list))
  loop
  (if (and (member (car next) labels)
           (multiple-do-labelp 
              (concatenate 'string "LABEL" 
                           (princ-to-string (car next))) labels))
      (setq ret (append  (list (list (car next) '(continue)))
                         (list (list nil (second next)))  ret))
      (setq ret (cons next ret)))
 (if (null rest) (return (reverse ret)))
 (setq next (car rest) rest (cdr rest))
 (go loop)))

(defun find-do (margin-line)
  (if (eq (caadr margin-line) 'do) (cadadr margin-line) nil))
;------------------------------------------------------------------------------
; prog-list of form ((margin line) (margin line) ...)
(defun translate-and-write-subprog (prog-list outport)
  (prog (fort-fun *external_function_names*
		  *undeclared_vbles* *declared_vbles* *implicit_vble_decls* *explicit_vble_decls*
		  *save_vbles* *key_params* *subprog_common_vars* 
		  *subprog_stmt_fns* *subprog_stmt_fns_bodies* *subprog_name*
		  *function-flag* *program-flag*
		  labels)		; labels is the do integers
     (setq labels (remove nil (mapcar #'find-do prog-list)))

     (setq *external_function_names* nil
	   *undeclared_vbles* nil
	   *declared_vbles* nil
	   *implicit_vble_decls* nil
	   *explicit_vble_decls* nil
	   *save_vbles* nil
	   *key_params* nil
	   *subprog_common_vars* nil
	   *subprog_stmt_fns* nil
	   *subprog_stmt_fns_bodies* nil
	   *function-flag* nil
	   *program-flag* nil
	   *subprog-arglist* nil
	   *data-init* nil
	   *functions-used* nil
	   *vble-declaration-done* nil)

     (clrhash *common-blocks*)
     (setq fort-fun
	   (do ((lines prog-list (cdr lines)) 
		(fort-fun nil (append fort-fun
				      (append (translate-label (caar lines))
					      (translate-line 
					       (brackets-check (concat-operators
								(cadar lines))))))))
	       ((null (cdr lines))
		(append fort-fun
			(cond (*function-flag*
			       ;; A function.  Return the value of the
			       ;; function, the value of the variable
			       ;; with the same name as the function.
			       `(end_label (return ,(cadr fort-fun))))
			      ((or *program-flag*
				   (not (eq (car fort-fun) 'defun)))
			       ;; Return nil if this is the main
			       ;; program, indicated by either having
			       ;; a PROGRAM statement (*program-flag*
			       ;; set) or no DEFUN in FORT-FUN.
			       `(end_label (return nil)))
			      (t
			       ;; A subroutine definition.  Return the
			       ;; arguments of the subroutine.
			       `(end_label (return (values 
						    ,@(caddr fort-fun))))))))))
     ;; check for missing PROGRAM stmt
     (if (not (eq (car fort-fun) 'defun))
         (setq fort-fun (append '(defun *main* nil) fort-fun)))
     ;;     (format t "~%after translation : ~A" fort-fun)
     (multiple-value-bind (spec-proc spec-decl fun)
	 (insert-declarations (prune-labels (fix-structure fort-fun labels)))
       ;;(format t "spec-proc = ~S~%" spec-proc)
       ;;(format t "spec-decl = ~S~%" spec-decl)
       (when spec-proc
	 (mapc #'(lambda (s-proc s-decl)
		   (unless (find (caadr s-proc) *common-block-initialized*)
		     (special-print s-proc outport)
		     (terpri outport)
		     (format outport "~%~%")
		     (special-print s-decl outport)
		     (terpri outport)
		     (format outport "~%~%")
		   (pushnew (caadr s-proc) *common-block-initialized*)))
	       spec-proc
	       spec-decl)
	 (terpri outport))
       (special-print fun outport))
     (print (cadr fort-fun))		;indicate which subprogram is being translated
     (write-char '#\newline outport) (write-char '#\newline outport)
     ))

;=============================================================================
(defun special-print (x o)
   (write x :escape t :stream o :pretty t :readably t :case :downcase))
;------------------------------------------------------------------------------
(defun princ-reset (x &optional (y ""))
  (print x)
  (princ y))

(defun translate-label (label)
   (if label 
       (list (read-from-string
                  (concatenate 'string "label" (princ-to-string label))
                   nil))
       nil))

;------------------------------------------------------------------------------

(defun translate-line (x)
  ;; x is a list being a line body-> list of lisp
  (prog (bindings)
     ;; reduce any DOUBLE PRECSION, REAL*8 etc to one word data types
     (when (member (car x) '(double real integer complex)) 
       (setq x (reduce-data-type x)))
     (when *verbose* (format t "~a~%" (string-downcase (princ-to-string x))))

     ;;return
     (when (id-return x)
       (return '((go end_label))))
     ;;continue
     (setq bindings (id-continue x))
     (when (not (eq bindings 'fail))
       (return `(continue_place_holder)))
     ;;definition
     (setq bindings (id-definition-prog-name x))
     (when (not (eq bindings 'fail))
       (return (parse-prog-definition bindings)))
     (setq bindings (id-definition-prog x))
     (when (not (eq bindings 'fail))
       (return (parse-prog-definition bindings)))
     (setq bindings (id-definition-sub x))
     (when (not (eq bindings 'fail))
       (return (parse-subr-definition bindings)))
     (setq bindings (id-definition-fun x))
     (when (not (eq bindings 'fail))
       (return (parse-fun-definition bindings)))
     (setq bindings (id-definition-fun-typed x))
     (when (not (eq bindings 'fail))
	
       (return (parse-typed-fun-definition bindings)))
     ;;goto/go to
     (when (id-comp-goto x)
       (return (parse-comp-goto x)))
     (when (id-comp-go-to x)
       (return (parse-comp-go-to x)))
     (when (id-assgn-goto x)
       (return (parse-assgn-goto x)))
     (setq bindings (id-goto x))
     (when (not (eq bindings 'fail))
       (return (parse-goto bindings)))
     (when (id-assgn-go-to x)
       (return (parse-assgn-go-to x)))
     (setq bindings (id-go-to x))
     (when (not (eq bindings 'fail))
       (return (parse-goto bindings)))
     ;;declarations
     (when (id-declaration x)
       (return (parse-declaration x))) 
     ;;parameters
     (when (id-parameter x)
       (return (parse-parameter x)))
     ;;implicit declarations
     (when (id-implicit-decl x)
       (return (parse-implicit-decl x)))
     ;;do loop
     (when (id-do-loop x)
       (return (parse-do-loop (check_new_vbles x))))
     ;; if-then
     (when (id-if-then x)
       (return (parse-if-then (check_new_vbles x))))
     ;; elseif (or else-if)
     (when (id-elseif x)
       (return `((elseif_place_holder 
		  ,(id-logical (cadr (check_new_vbles x)))))))
     (when (id-else-if x)
       (return `((elseif_place_holder
		  ,(id-logical (caddr (check_new_vbles x)))))))
     (when (id-else x)
       (return '((elseif_place_holder t))))
     (when (id-endif x)
       (return '(endif_place_holder)))

     ;;if
     (setq bindings (id-if-goto x))
     (when (not (eq bindings 'fail))
       (return (parse-if-goto bindings)))
     (when (id-if x)
       (return (parse-if (check_new_vbles x))))

     ;;assignments or statement functions
     (when (id-assignment x)
       (return (parse-assignment x)))
     ;;subroutine call
     (when (id-subroutine-call x) 
	
       (return (parse-subroutine-call x)))
     ;;pause
     (when (id-pause x)
       (return (parse-pause x)))
     ;;write
     (when (eq (car x) 'write)
       (return (parse-write x)))
     ;;print
     (when (eq (car x) 'print)
       (return (parse-print x)))
       
     ;;format
     (when (eq (car x) 'format)
       (return (parse-format x)))
     ;;data
     (when (eq (car x) 'data)
       (return (parse-data x)))
     ;;save
     (when (eq (car x) 'save)
       (return (parse-save x)))
     ;;intrinsic
     (when (eq (car x) 'intrinsic)
       (return nil))
     ;;external
     (when (eq (car x) 'external)
       (return (parse-external x)))
     ;;common
     (when (eq (car x) 'common)
       (return (parse-common x)))
     ;;stop
     (when (eq (car x) 'stop)
       (return nil))			; 
     ;;fall out the bottom:
     (when (eq (car x) 'fortran_comment)
       (return (list x)))
     ;; (return (list '----> (check_new_vbles x)))

     (return (list (list 'quote (concatenate ' string  "****NOT TRANSLATED: " 
					       (write-to-string (check_new_vbles x))))))
     ))
;------------------------------------------------------------------------------
;program 
(defun id-definition-prog-name (x) 
  (unify x '(program %name) nil))

(defun id-definition-prog (x) 
  (unify x '(program) nil))

;subroutine s(x,...)
(defun id-definition-sub (x) 
  (prog (bindings)
     (setq bindings (unify x '(subroutine %name %arg-list) nil))
     (if (eq bindings 'fail)
         (setq bindings (unify x '(subroutine %name) nil)))
     (return bindings)))

;type function f(x,...)
(defun id-definition-fun-typed (x) 
  (unify x '(%type function %name %arg-list) nil))

;untyped function f(x,...)
(defun id-definition-fun (x) 
  (unify x '(function %name %arg-list) nil))

; continue
(defun id-continue (x) (unify x '(continue) nil))

;goto label
(defun id-goto (x)
 (unify x '(goto %label) nil))
(defun id-assgn-goto (x)
 (and (eq (car x) 'goto)
      (listp (cadr x))))
(defun id-comp-goto (x)
 (and (eq (car x) 'goto)
      (or (and (not (listp (cadr x)))
               (cddr x))
          (not (numberp (cadr x))))))
;go to label
(defun id-go-to (x)
 (unify x '(go to %label) nil))
(defun id-assgn-go-to (x)
 (and (eq (car x) 'go)
      (eq (cadr x) 'to)
      (listp (caddr x))))
(defun id-comp-go-to (x)
 (and (eq (car x) 'go)
      (eq (cadr x) 'to)
      (or (and (not (listp (caddr x)))
               (cdddr x))
          (not (numberp (caddr x))))))


;integer x,y,z,...
(defun id-declaration (x) 
  (member (first x) '(integer integer1 integer2 integer4
		      double real real8 character
		      complex complex8 complex16
		      logical dimension) :test #'eq))
;------------------------------------------------------------------------------
(defun id-if-then (x) (and (eq (car x) 'if) (member 'then x :test #'eq))) 
(defun id-if-goto (x) (unify x '(if %pred goto %label) nil)) 
;(defun id-if-assignment (x) (and (eq (car x) 'if) (member '= x :test #'eq))) 
;(defun id-if-pause (x) (and (eq (car x) 'if) (member 'pause x :test #'eq))) 
(defun id-if (x) (eq (car x) 'if))
(defun id-else (x) (eq (car x) 'else)) 
;; elseif can be written either as "ELSE IF" or "ELSEIF"
(defun id-else-if (x)
  (and (eq (car x) 'else)
       (cadr x)
       (eq (cadr x) 'if)))

(defun id-elseif (x)
  (eq (car x) 'elseif))

(defun id-endif (x) 
       (or (eq (car x) 'endif) 
           (and (eq (car x) 'end) (eq (cadr x) 'if)))) 

(defun id-assignment (x) (member '= x :test #'eq))
(defun id-subroutine-call (x) (eq (car x) 'call))
(defun id-do-loop (x) (eq (car x) 'do))

(defun id-pause (x) (eq (car x) 'pause))

(defun id-return (x) (and (eq (car x) 'return) (null (cdr x))))
(defun id-predicate (x) (id-logical x))

(defun id-implicit-decl (x) (eq (car x) 'implicit))
(defun id-parameter (x) (eq (car x) 'parameter))

;=============================================================================
; parsers

; program definition
(defun parse-prog-definition (bindings) 
   (setq *program-flag* t)
   (setq *subprog_name* (check-reserved-lisp-names (variable-value '%name bindings)))
   (list 'defun (if *subprog_name* *subprog_name* '*MAIN*)
;         (read-from-string 
;             (concatenate 'string "*MAIN*"
;                    (princ-to-string *subprog_name*)))
         nil
         ))

; subroutine definition
(defun parse-subr-definition (bindings) 
  (prog (arg-list)
   (setq *subprog_name* (check-reserved-lisp-names (variable-value '%name bindings)))
   (setq arg-list (mapcar #'check-reserved-lisp-names
			  (remove '|,| (variable-value '%arg-list bindings))))
   (setq *subprog-arglist* arg-list)
   (return (list 'defun *subprog_name* 
                    (if arg-list arg-list nil)))))

; typed function definition
(defun parse-typed-fun-definition (bindings)
  (setq *function-flag* t)
  (prog (fun-name)
   (setq fun-name (check-reserved-lisp-names (variable-value '%name bindings))
         *declared_vbles* (list fun-name)
         *explicit_vble_decls* 
              (list (list (convert-data-type (variable-value '%type bindings))
                          (list fun-name))))
   (setq *subprog-arglist* (remove '|,| (variable-value '%arg-list bindings)))
   (return
   (list 'defun 
         fun-name 
         (remove '|,| (variable-value '%arg-list bindings))))))

; untyped function definition
(defun parse-fun-definition (bindings)
   (setq *function-flag* t)
   (setq *subprog-arglist* (remove '|,| (variable-value '%arg-list bindings)))
   (list 'defun 
         (check-reserved-lisp-names (variable-value '%name bindings))
         (remove '|,| (variable-value '%arg-list bindings))))



;goto
(defun parse-goto (bindings) 
   `((go ,(read-from-string 
            (concatenate 'string "label" 
                 (princ-to-string (variable-value '%label bindings))) 
            nil))) )
;------------------------------------------------------------------------------
(defun make-label (n) 
  (read-from-string (concatenate 'string "label" (princ-to-string n))))

(defun parse-assgn-goto (x)
 (prog (labels len)
  (setq labels (remove '|,| (cadr x)))
  (setq len (length labels))
  (return
    `((case 
       ,(id-expression (cdddr x)) 
       ,@(mapcar #'(lambda (x n)
		     (list n (make-label x)))
		 labels (listn 1 len)))))))

(defun listn (a b)
  "Compute a list of integers from A to B, inclusive"
  (if (<= a b)
      (loop for x from a to b collect x)
      (loop for x from b downto a collect x)))

;------------------------------------------------------------------------------
(defun parse-comp-goto (x)
  (if (cddr x)
      `((computed-goto 
	 ,(mapcar #'(lambda (l) 
		      (read-from-string 
		       (concatenate 'string "label" (princ-to-string l))))
		  (remove '|,| (second x)))
	 ,(third x)))
      `((computed-goto ,(cadr x)))))

(defun parse-comp-go-to (x)
  (if (fourth x)
      `((computed-goto 
	 ,(mapcar #'(lambda (l) 
		      (read-from-string 
		       (concatenate 'string "label" (princ-to-string l))))
		  (remove '|,| (third x)))
	 ,(fourth x)))
      `((computed-goto ,(caddr x)))))
;------------------------------------------------------------------------------
(defun parse-assgn-go-to (x)
 (prog (labels len)
  (setq labels (remove '|,| (caddr x)))
  (setq len (length labels))
  (return
    `((case 
       ,(id-expression (cddddr x)) 
       ,@(mapcar #'(lambda (x n) (list n (make-label x))) labels (listn 1 len)))))))

;------------------------------------------------------------------------------

;declaration
(defun parse-declaration (x)		; x is the line
  (setq *declared_vbles*
	(append (mapcar #'(lambda (v)
			    (check-reserved-lisp-names (car v)))
			(list-split '|,|
			 (if (eq (cadr x) '*)
			     (cdddr x)
			     (cdr x))))
		*declared_vbles*))
  ;; If we declared an intrinsic function name, remove that from the
  ;; list of declared variables.
  (setf *declared_vbles*
	(set-difference *declared_vbles* *intrinsic_function_names*))
  (let ((type (find (first x) '((integer integer4)
				(integer4 integer4)
				(integer2 integer2)
				(integer1 integer1)
				(double double-float)
				(real single-float)
				(real8 double-float)
				(character #'parse-char-decl)
				(complex complex8)
				(complex8 complex8)
				(complex16 complex16)
				(logical logical)
				(dimension array))
		    :key #'car)))
    (when type
      (cond ((symbolp (second type))
	     (setq *explicit_vble_decls* 
		   (build_decl_list *explicit_vble_decls* (second type) (cdr x))))
	    ((consp (second type))
	     (funcall (second (second type)) x))
	    (t
	     (error "Failed to parse a declaration!")))))
  nil)
	
(defun parse-implicit-decl (x)
   (if (eq (third x) '*) (setq x `(,(first x) 
                                   (,(second x) ,(third x) ,(fourth x))
                                   ,@(cddddr x))))
   (setq *implicit_vble_decls* 
         (append (mapcar #'(lambda (y)
                               `(,(car (convert-data-type y))
                                 ,@(list-split '|,| (remove '- (car (last y))))))
                         (list-split '|,| (cdr x)))
                 *implicit_vble_decls*)) 
   nil)

(defun build_decl_list (decl_list type decl)
  (cons `(,type 
	  ,@(mapcar #'(lambda (y)
			`(,(check-reserved-lisp-names (car y))
			  ,@(parse_dimension_specs (cadr y))))
		    (list-split '|,| decl)))
	decl_list))

(defun parse_dimension_specs (specs)
  (if (null specs) nil
      (mapcar #'parse_upper_and_lower_bounds (list-split '|,| specs))))

;; Array bounds can be either (l:u) or (u).  However, U can also be
;; "*" to mean unbounded or unknown.
(defun parse_upper_and_lower_bounds (bds)
  (flet ((fixup-negative (expr)
	   ;; If a bound is negative, f2cl separates the negative sign
	   ;; from the expression.  This function replaces that
	   ;; expression with the actual negative number if the
	   ;; expression is, in fact, a number.
	   (if (and (eq '- (first expr))
		    (numberp (second expr)))
	       (list (- (second expr)))
	       expr)))
    (let ((fixed-up-bds (mapcar #'fixup-negative (list-split '|:| bds))))
      ;; If a bound is "*", just return T instead.
      (if (null (cdr fixed-up-bds))
	  (list 1 (or (eq (caar fixed-up-bds) '*)
		      (id-expression (car fixed-up-bds))))
	  (list (or (eq (caar fixed-up-bds) '*)
		    (id-expression (car fixed-up-bds)))
		(or (eq (caadr fixed-up-bds) '*)
		    (id-expression (cdr fixed-up-bds))))))))

;-----------------------------------------------------------------------------
; expression is algebraic with function calls:
(defun parse-assignment (x)
  (let ((lhs (parse-expression (tail-chop '= x)))
	(rhs (parse-expression (head-chop '= x))))
    (cond ((listp lhs)
	   ;; Look for undeclared variables in the rhs
	   (check_new_vbles rhs)
	   (parse-arrayref-or-stmtfn lhs rhs))
	  (t
	   ;; Look for undeclared variables in the lhs or rhs.
	   (check_new_vbles (list lhs))
	   (check_new_vbles (list rhs))
	   (let* ((lhs-type (first (get-upgraded-fun-arg-type (list (list lhs)))))
		  (rhs-type (first (get-upgraded-fun-arg-type (list (list rhs))))))
	     ;;(format t "~&")
	     ;;(format t "lhs = ~A, type ~A~%" lhs lhs-type)
	     ;;(format t "rhs = ~A, type ~A~%" rhs rhs-type)
	     (cond ((subtypep lhs-type 'string)
		    ;; Strings need to be handled carefully
		    `((f2cl-set-string ,lhs ,rhs ,lhs-type)))
		   (t
		    (let ((new-rhs
			   (cond ((find *coerce-assignments* '(t :always))
				  `(coerce ,rhs (type-of ,lhs)))
				 ((find *coerce-assignments* '(nil :never))
				  rhs)
				 (t
				  ;;(format t "rhs-type, rhs = ~S ~S~%" rhs-type rhs)
				  ;; RHS.  Otherwise, coerce the RHS to the
				  ;; type of the LHS.  However, we can't coerce
				  ;; something to an integer.  Use truncate for
				  ;; that.
				  (cond ((or (eq t rhs-type)
					     (eq t lhs-type)
					     (subtypep rhs-type lhs-type))
					 ;; No coercion is needed if the types
					 ;; are compatible, or if we can't
					 ;; determine the type of the LHS or
					 ;; RHS.
					 rhs)
					((and (subtypep lhs-type 'integer)
					      (not (subtypep rhs-type 'integer)))
					 ;; We're trying to set a integer
					 ;; variable to non-integer value.  Use
					 ;; truncate.
					 `(truncate ,rhs))
					(t
					 ;; Haven't a clue, so coerce
					 `(coerce ,rhs ',lhs-type)))))))
		      #+nil
		      (when (and (listp new-rhs)
				 (eq 'coerce (first new-rhs)))
			(format t "lhs, rhs types = ~A (~A) = ~A (~A) ~%      -> ~A~%"
				lhs lhs-type rhs rhs-type new-rhs))
		      `((setf ,lhs ,new-rhs))))))))))

(defun parse-arrayref-or-stmtfn (lhs rhs)
  ;;lhs is either an array ref or a statement function name
  (cond ((eq (car lhs) 'fref)
	 ;;array_ref 
	 (let* ((lhs-type (first (get-upgraded-fun-arg-type (list (list lhs)))))
		(rhs-type (first (get-upgraded-fun-arg-type (list (list rhs))))))
	   ;;(format t "~&")
	   ;;(format t "lhs = ~A, type ~A~%" lhs lhs-type)
	   ;;(format t "rhs = ~A, type ~A~%" rhs rhs-type)
	   (cond ((subtypep lhs-type 'string)
		  ;; Strings need to be handled specially
		  `((f2cl-set-string ,lhs ,rhs ,lhs-type)))
		 (t
		  (let ((new-rhs
			 (cond ((find *coerce-assignments* '(t :always))
				`(coerce ,rhs (type-of ,lhs)))
			       ((find *coerce-assignments* '(nil :never))
				rhs)
			       (t
				;;(format t "rhs-type, rhs = ~S ~S~%" rhs-type rhs)
				;; RHS.  Otherwise, coerce the RHS to the
				;; type of the LHS.  However, we can't coerce
				;; something to an integer.  Use truncate for
				;; that.
				(cond ((or (eq t rhs-type)
					   (eq t lhs-type)
					   (subtypep rhs-type lhs-type))
				       ;; No coercion is needed if the types
				       ;; match, or if we can't determine the
				       ;; type of the LHS or RHS.
				       rhs)
				      ((and (subtypep lhs-type 'integer)
					    (not (subtypep rhs-type 'integer)))
				       ;; We're trying to set a integer
				       ;; variable to non-integer value.  Use
				       ;; truncate.
				       `(truncate ,rhs))
				      (t
				       ;; Haven't a clue, so coerce
				       `(coerce ,rhs ',lhs-type)))))))
		    `((fset ,lhs ,new-rhs)))))))
	((eq (car lhs) 'fref-string)
	 ;; Fortran string ref
	 `((fset-string ,lhs ,rhs)))
	(t
	 ;;statement_function
	 (progn
	   (setq *subprog_stmt_fns* (append *subprog_stmt_fns* (list (car lhs)))
		 *subprog_stmt_fns_bodies*
		 (append *subprog_stmt_fns_bodies* 
			 `((,(car lhs) ,(cdr lhs) ,rhs))))
	   nil))))
;       (and (setq *subprog_stmt_fns* (append *subprog_stmt_fns* (list (car lhs))))
;            `((defun ,(read-from-string 
;                           (concatenate 'string 
;                                   (princ-to-string *subprog_name*)
;                                   (princ-to-string (car lhs))))
;                     ,(cdr lhs) ,rhs)))))

(defun parse-expression (x) (id-expression x))

(defun parse-pause (x) 
   `((error ,(cadr x))))

;------------------------------------------------------------------------------
(defun parse-subroutine-call (x)
  ;; X looks like (CALL SUBNAME (comma-separated list of args, if any))
  (let ((arglist (if (third x)
		     (mapcar  #'id-expression 
			      (list-split '|,| (check_new_vbles (third x))))
		     nil)))
    ;; Note that this is not a variable.
    (update-called-functions-list (second x) arglist)
    (cond ((null arglist)
	   ;; No args to the subroutine
	   `((,(second x))))
	  ((every #'symbolp arglist)
	   ;; If every argument is a simple variable (or array), use m-v-setq to
	   ;; set the simple variables to the correct values.
	   `((multiple-value-setq 
		 ,(mapcar #'(lambda (x)
			      (cond ((not (symbolp x)) '*dummy_var*)
				    ((vble-is-array-p x) '*dummy_var*)
				    (t x)))
			  arglist)
	       (,(second x) ,@arglist))))	   
	  (t
	   ;; This is the more complicated case where we need to be
	   ;; more careful.  M-v-bind is used to get all of the return
	   ;; values.  Then we go and set the parameters according to
	   ;; the returned values.  If the parameter is a simple
	   ;; variable or an array reference, set the value.
	   ;; Otherwise, we do nothing.
	   ;;
	   ;; The user will have to check to make sure this is right.
	   ;; Array references can also be a slice of an array that is
	   ;; passed to the subroutine, and that isn't handled here!
	   ;;
	   (let* ((vnames (let ((v '()))
			    ;; Make up some variable names
			    (dotimes (k (length arglist))
			      (push (intern (format nil "VAR-~D" k)) v))
			    (nreverse v)))
		  ;; Figure out if and how to set the parameters to the returned values.
		  (setters (do ((v nil)
				(vars vnames (rest vars))
				(args arglist (rest args)))
			       ((null args) (remove nil (nreverse v)))
			     (let ((var (first vars))
				   (arg (first args)))
			       (push (cond ((vble-is-array-p arg)
					    nil)
					   ((symbolp arg)
					    `(setf ,arg ,var))
					   ((and (listp arg)
						 (eq (first arg) 'fref))
					    `(fset ,arg ,var))
					   (t
					    nil))
				     v)))))
	     (if setters
		 `((multiple-value-bind ,vnames
		       (,(second x) ,@arglist)
		     (declare (ignorable ,@vnames))
		     ,@setters))
		 `((,(second x) ,@arglist))))))))

;------------------------------------------------------------------------------
(defun parse-do-loop (x)
 (let* ((limits (list-split '|,| (cddddr x))) (step (third limits)))
     `((fdo (,(third x)
        ,(id-expression (first limits))
        (+ ,(third x) ,(if (null step) 1 (id-expression step))))
      ((> ,(third x) ,(id-expression (second limits))) nil)
      ,(read-from-string 
            (concatenate 'string "fdo_body_label" 
                 (princ-to-string (second x))) nil)
))))

; (let* ((limits (list-split '|,| (cddddr x)))
;        (init-val (id-expression (first limits)))
;        (final-val (id-expression (second limits)))
;        (step (if (null (third limits)) 1 (id-expression (third limits))))
;       )
;`((fdo ((iteration-count 
;             (max (truncate (+ (- ,final-val ,init-val) ,step) ,step) 0)
;             (1- iteration-count))
;        (,(third x) ,init-val (+ ,(third x) ,step))
;       )
;       ((zerop iteration-count) nil)
;,(read-from-string 
;            (concatenate 'string "fdo_body_label" 
;                 (princ-to-string (second x))) nil)
;))))

(defun parse-if-then (x) 
   `((if-then ,(id-predicate (butlast (rest x)))  )))

(defun parse-if-goto (bindings) 
   `((if ,(id-predicate (variable-value '%pred bindings) )
         (go ,(read-from-string 
            (concatenate 'string "label" 
                 (princ-to-string (variable-value '%label bindings))) nil)))))

(defun parse-if (x)
   (cond ;arithmetic if
         (;(member '|,| (cddr x))
          (and (eq (length (remove '|,| (cddr x))) 3)
               (not (member-if-not #'numberp (remove '|,| (cddr x)))))
          `((arithmetic-if ,(id-predicate (second x)) 
                           (go ,(read-from-string
                                 (concatenate 'string 
                                              "label" 
                                              (princ-to-string (first (cddr x))))))
                           (go ,(read-from-string
                                 (concatenate 'string 
                                              "label" 
                                              (princ-to-string (third (cddr x))))))
                           (go ,(read-from-string
                                 (concatenate 'string 
                                              "label" 
                                              (princ-to-string (fifth (cddr x)))))))))
         ;logical if
         (t `((if ,(id-predicate (second x)) ,@(translate-line (cddr x)))))))

(defun parse-return (x) x)
(defun parse-parameter (x) 
   (setq x (mapcar #'(lambda (l) (remove '= l))
                   (list-split '|,| (cadr x))))

   (setq *key_params* (append *key_params* 
                            (mapcar #'(lambda (l)
                                           (list (first l) (id-factor (second l))))
                                    x)))
   nil)
; restrict SAVE stmts to
;     SAVE a,b,...
; or  SAVE /label/a,b,..
(defun parse-save (x)
   (setq *save_vbles*
        (append *save_vbles*
                (remove '|,| (if (eq (first (cdr x)) '/) (cddddr x) (cdr x)))))
   nil) 
  
(defun parse-common (x)
   (setq x (mapcar #'(lambda (l) (remove '|,| l))
                   (list-split '|/| (cdr x))))
   (setq x (if (null (car x)) (cdr x) (cons nil x)))
   ;; x now in form (cb nlist cb nlist ... )

   ;; Pick out the common blocks and variables and put them in a hash
   ;; table for later use.  We want to associate the variables with
   ;; the common block.
   (do ((list x (cddr list)))
       ((endp list))
     (let ((varlist (mapcar #'check-reserved-lisp-names (second list)))
	   (block-name (or (caar list)
			   '%BLANK%)))
       ;; Check to see if this is another instance.  If so, the elements
       ;; of the common block should match.
       (multiple-value-bind (val found)
	   (gethash block-name *common-blocks*)
	 (cond ((not found)
		(setf (gethash block-name *common-blocks*)
		      varlist))
	       (t
		;; Found the common block
		(unless (equalp val varlist)
		  (format t "val = ~S~%" val)
		  (format t "lst = ~S~%" varlist)
		  (warn "current common block doesn't match previous occurrence")))))))

   
   ;; pick out lists of vblenames and add to *subprog_common_vars*
   (setq *subprog_common_vars* 
         (append *subprog_common_vars*
                 (do ((list x (cddr list))
                      (ret nil (append (extract-atoms (cadr list)) ret)))
                     ((endp list)
		      (mapcar #'check-reserved-lisp-names ret))
		   ;; look for common arrays and store dimensions of
		   ;; new common vars
		   (do ((nlist (cadr list) (cdr nlist)))
		       ((endp nlist))
		     (cond ((and (cdr nlist) (listp (cadr nlist)))
			    ;; array dimensioned in COMMON stmt
			    (update_cm_array_dims (car nlist) 
						  (remove '|,| (cadr nlist)))
			    (setq nlist (cdr nlist)))
			   ;; check if array dimensioned elsewhere
			   ((member (car nlist) *declared_vbles*)
			    (do ((decls *explicit_vble_decls* (cdr decls)))
				((null decls) nil)
			      (do ((vbles (cdar decls) (cdr vbles)))
				  ((null vbles) nil)
				(if (and (eq (car nlist) (caar vbles))
					 (cdar vbles))
				    (update_cm_array_dims (car nlist)
							  (cdar vbles))))))
			   )))))
;   (print *common_array_dims*)                
   nil)

; append list of vble and dims to *common_array_dims* if vble not already in list
; when vble in list check dims match with that stored
(defun update_cm_array_dims (vble dims)
   (let ((stored-dims (member vble *common_array_dims*)))
      (if stored-dims
          (if (not (equal (cadr stored-dims) dims))
              (error "common array ~A dimensions not equivalent between subprograms" vble))
          (setq *common_array_dims*
		(append (list vble (parse_dimension_specs dims)) *common_array_dims*)))))

(defun extract-atoms (x)
   (do ((l x (cdr l))
        (ret nil (if (atom (car l)) (cons (car l) ret) ret)))
       ((endp l) ret)))

;; Parse Fortran character declarations. These are rather complicated
;; and we need to be able to handle things like:
;;
;; character*10 a, b, c
;; character a*10, b*20, c*30
;; character*10 x(10), y(20), z(3,4)
;; character x(10)*10, y(20)*7, z(3,4)*8
;;
;; By the time we get these, the line looks something like the
;; following, respectively:
;;
;; character * 10 a |,| b |,| c
;; character a * 10 |,| b * 20 |,| c * 30
;; character * 10 x(10) |,| y(20) |,| z(3 |,| 4)
;; character x(10) * 10 |,| y(20) * 7 |,| z(3 |,| 4) * 8

(defun parse-char-decl (x)
  ;; x is the line.
  (cond ((eq (second x) '*)
	 ;; The length was given explicitly as part of the
	 ;; character declaration.
	 (push `((character ,(third x))
		 ,@(mapcar #'(lambda (decl)
			       (let ((dcl (remove '* decl)))
				 ;; (format t "  decl = ~A~%" dcl)
				 (destructuring-bind (name &optional dim-or-len)
				     dcl
				   (if dim-or-len
				       ;; An array of characters
				       `(,name ,@(parse_dimension_specs dim-or-len))
				       ;; A simple character string
				       dcl))))
			   (list-split '|,| (cdddr x))))
	       *explicit_vble_decls*))
	(t
	 ;; The length may have been given as part of the variable.
	 ;; Put the length with the variable type.
	 (mapc #'(lambda (decl)
		   (destructuring-bind (name &optional dim-or-len &rest len)
		       (remove '* decl)
		     (push
		      (if (and dim-or-len (listp dim-or-len))
			  ;; An array
			  `((character ,@len) (,name ,@(parse_dimension_specs dim-or-len)))
			  ;; A simple character string
			  `((character ,(or dim-or-len 1)) (,name)))
		      *explicit_vble_decls*)))
	       (list-split '|,| (cdr x)))))
     
  ;;(format t "explicit_vble_decls* = ~A~%" *explicit_vble_decls*)
  )


; DATA stmts 
;           DATA nlist/clist[,nlist/clist/]...
; restricted to 
;   one vble name per nlist
;   no implied do's
;   clist either a alist or a repetition (not a combination of both)

;; rlt:  also handles
;;	data var1, var2, var3/val1, val2, val3/

(defun parse-data (x)
  ;;(format t "parse-data:  ~S~%" x)
  (setq x (list-split '|/| (cdr x)))
  (do ((list x (cddr list))
       (ret nil (let* ((var (remove '|,| (car list)))
		       (vals (list-split '|,| (cadr list)))
		       (result (parse-data1 var vals)))
		  ;;(format t "list = ~a~%" list)
		  ;;(format t "var = ~a~%" var)
		  ;;(format t "vals = ~a~%" vals)
		  ;;(format t "result = ~a~%" result)
		  (setf *data-init* (append *data-init* (if (consp (first result))
							result
							(list result))))
		  ;;(format t "*data-init*:  ~A~%" *data-init*)
		  )))
      ((null (cdr list)))))

; parse a (vble_name data1 data2 ...) list
;    or a (vble_name n*x) list
(defun parse-data1 (v l)
  ;;(format t "parse-data1:  v = ~S~%" v)
  ;;(format t "parse-data1:  l = ~S~%" l)
  (labels ((fix-up-negative-number (x)
	     (cond ((numberp x)
		    x)
		   (t
		    (let ((ex (id-expression x)))
		      (if (listp ex)
			  (- (second ex))
			  ex)))))
	   (fix-up-data-reps (data)
	     (let ((result '()))
	       (dolist (item data (nreverse result))
		 ;;(format t "data-reps item = ~S~%" item)
		 (cond ((and (>= (length item) 3)
			     (eq (second item) '*))
			;;(format t "~a reps of ~S~%" (first item) (rest (rest item)))
			(let ((num (fix-up-negative-number (rest (rest item)))))
			  (dotimes (k (first item))
			    (push num result))))
		       (t
			(push (fix-up-negative-number item) result))))))
	     (data-var (v)
	       (cond ((atom v)
		      v)
		     ((listp v)
		      (data-var (first v)))
		     (t
		      nil))))
    (cond ((and (listp (car l))
		(eq (second (car l)) '*))
	   `(fill ,(check-reserved-lisp-names (first v))
	          ,(fix-up-negative-number (cddar l))
	          :end ,(1- (first (car l)))))
	  ((and (listp v) (listp l)
		(listp (second v))
		(numberp (first (second v))))
	   ;; Initializing one element of an array.  We assume the
	   ;; lower limit on the array is 1!
	   `(fset (fref ,(check-reserved-lisp-names (first v))
		        ,(remove '|,| (second v)))
	     ,(fix-up-negative-number (first l))))
	  ((and (listp v) (listp l)
		(listp (first v))
		(member '= (first v)))
	   ;; Implied do loop.  Extract out the important parts of the
	   ;; implied do and construct what we need from it.
	     `(data-implied-do ,(first v) ,(data-var v) ,(fix-up-data-reps l)))
	  ((and (listp v) (listp l)
		(= (length v) (length l))
		(> (length v) 1))
	   (mapcar #'(lambda (var val)
		       `(setq ,(check-reserved-lisp-names var)
			      ,(fix-up-negative-number val)))
		   v l))
	  (t
	   (if (vble-is-array-p (check-reserved-lisp-names (first v)))
	       `(replace ,(check-reserved-lisp-names (first v))
		         ',(mapcar #'fix-up-negative-number l))
	       `(setq ,(check-reserved-lisp-names (first v))
		      ,(fix-up-negative-number (first l))))))))

; parse EXTERNAL f1, f2, ...
; by adding the function names to *external_function_names*
(defun parse-external (x)
   (setq *external_function_names*
         (append *external_function_names* (remove '|,| (cdr x))))
   nil)

;=============================================================================
;parsing utilities 

(defun tail-chop (beta lis) 
   (prog (retlist)
      (setq retlist nil)
    loop
      (cond ((or (equal lis nil) (equal (car lis) beta))
             (return retlist))
            (t (setq retlist (append1 retlist (car lis))
                  lis (cdr lis))
             (go loop)))))

(defun head-chop (beta lis) (cdr (member beta lis)))

(defun list-split (beta lis)
     (cond ((equal (member beta lis)  nil) (list lis))
           (t `(,(tail-chop beta lis)
                            ,@(list-split beta (head-chop beta lis))))))

(defun gen-list-split (beta lis)
  (prog (sym ops)
     (cond ((null (remove nil (mapcar 'member beta 
                                           (const lis (length beta)))))
                                            (return (list (list lis) nil)))
           (t (setq sym (gensym))
              (setq ops (extract beta lis))
              (setq lis (tpl-subpair (const sym (length beta)) beta lis))
              (return (list `(,(tail-chop sym lis)
                 ,@(list-split sym (head-chop sym lis))) ops))))))

#+cmu
(defun concat (&rest syms)
  (cond ((null syms)
	 "")
	(t
	 (concatenate 'string (symbol-name (first syms))
		      (apply #'concat (rest syms))))))

(defun list-split-multi (op lis)
  (prog (ret)
    (setq ret (match-separated (list op) lis))
    (cond ((null ret) (princ-reset (concat
'|Senac syntax error: failure to parse an expression 
                  using the operator "| op '|"
Senac syntax: ... | op '| ... | op '| ... OR
              ... | op '| ...|)))
          (t (return (car ret))))))

(defun list-split-bin (op lis)
  (prog (ret)
    (setq ret (match-separated (list op) lis))
    (cond ((null ret) (princ-reset (concat
'|Senac syntax error: failure to parse an expression
                   using the operator "| op '|"
Senac syntax:... | op '| ...|)))
          (t (return  (car ret))))))

(defun list-split-multi-string (op lis)
  (prog (ret)
    (setq ret (match-separated op lis))
    (cond ((null ret) (princ-reset (concat
'|Senac syntax error: failure to parse an expression
                   using the operator "| op '|"
Senac syntax:... | op '| ...|)))
          (t (return  (car ret))))))

(defun concat-operators (x) 
 (prog (next rest done)
  (setq next (car x) rest (cdr x)) 
  loop
  (cond ((null rest) (return (reverse (cons next done)))))
  (cond 
      ((and (eq next '*) (eq (car rest) '*))
       (setq rest (cdr rest)
	     done (cons '^ done))) 
      ((and (eq next '>) (eq (car rest) '=))
       (setq rest (cdr rest)
	     done (cons '>= done))) 
      ((and (eq next '<) (eq (car rest) '=))
       (setq rest (cdr rest)
	     done (cons '<= done))) 
      ((and (eq next '>) (eq (car rest) '<))
       (setq rest (cdr rest)
	     done (cons '>< done))) ; needs to be defined
      ((and (eq next '/) (eq (car rest) '/))
       ;; Fortran string concat operator
       (setq rest (cdr rest)
	     done (cons '// done)))
;---->
      (t 
       (setq done (cons next done))))
   (setq  next (car rest) rest (cdr rest))
   (go loop)))

#+nil
(defun convert-data-type (x)
   (cond ((not (listp x)) 
          (car (convert-data-type (list x))))
         ((equalp (subsequence x 0 2) '(double precision))
          (remove 'precision x))
         ((equalp (subsequence x 0 3) '(real * 8))
          (append '(single-float) (subsequence x 3)))
         ((or (equalp (subsequence x 0 3) '(integer * 2))
              (equalp (subsequence x 0 3) '(integer * 4)))
          (append '(fixnum) (subsequence x 3)))
         ((eq (car x) 'real)   
          (append '(single-float) (cdr x)))
         ((eq (car x) 'integer)
          (append '(fixnum) (cdr x)))
         ((and (listp (car x)) (eq (caar x) 'character))
          (append `(character ,(third (car x))) (cdr x)))
         ((and (listp (car x)) (eq (caar x) 'real))
          (append '(single-float) (cdr x)))
         ((and (listp (car x)) (eq (caar x) 'integer))
          (append '(fixnum) (cdr x)))
         ((eq (car x) 'double) 
          (append '(double-float) (cdr x)))
         ((eq (car x) 'logical) 
          (append t (cdr x)))
         (t x)))

(defun convert-data-type (x)
  (cond ((not (listp x)) 
	 (car (convert-data-type (list x))))
	((equalp (subsequence x 0 2) '(double precision))
	 (remove 'precision x))
	((equalp (subsequence x 0 3) '(real * 8))
	 (append '(double-float) (subsequence x 3)))
	((equalp (subsequence x 0 3) '(integer * 1))
	 (append '(integer1) (subsequence x 3)))
	((equalp (subsequence x 0 3) '(integer * 2))
	 (append '(integer2) (subsequence x 3)))
	((equalp (subsequence x 0 3) '(integer * 4))
	 (append '(integer4) (subsequence x 3)))
	((eq (car x) 'real)   
	 (append '(single-float) (cdr x)))
	((eq (car x) 'integer)
	 (append '(integer4) (cdr x)))
	((equalp (subsequence x 0 3) '(complex * 8))
	 (append '(complex8) (subsequence x 3)))
	((equalp (subsequence x 0 3) '(complex * 16))
	 (append '(complex16) (subsequence x 3)))
	((and (listp (car x)) (eq (caar x) 'complex))
	 (append '(complex8) (cdr x)))
	((and (listp (car x)) (eq (caar x) 'character))
	 (append `(character ,(third (car x))) (cdr x)))
	((and (listp (car x)) (eq (caar x) 'real))
	 (append '(single-float) (cdr x)))
	((and (listp (car x)) (eq (caar x) 'integer))
	 (append '(fixnum) (cdr x)))
	((eq (car x) 'double) 
	 (append '(double-float) (cdr x)))
	((eq (car x) 'logical) 
	 (append t (cdr x)))
	(t x)))

#+nil
(defun reduce-data-type (x)
   (cond ((equalp (subsequence x 0 2) '(double precision))
          (remove 'precision x))
         ((equalp (subsequence x 0 3) '(real * 8))
          (append '(real) (subsequence x 3)))
         ((equalp (subsequence x 0 3) '(complex * 16))
          (append '(complex) (subsequence x 3)))
         ((or (equalp (subsequence x 0 3) '(integer * 2))
              (equalp (subsequence x 0 3) '(integer * 4)))
          (append '(integer) (subsequence x 3)))
         (t x)))
(defun reduce-data-type (x)
  (let ((subseq3 (subsequence x 0 3)))
    (cond ((equalp (subsequence x 0 2) '(double precision))
	   (remove 'precision x))
	  ((equalp subseq3 '(real * 8))
	   (append '(double) (subsequence x 3)))
	  ((equalp subseq3 '(complex * 16))
	   (append '(complex16) (subsequence x 3)))
	  ((equalp subseq3 '(integer * 4))
	   (append '(integer) (subsequence x 3)))
	  ((equalp subseq3 '(integer * 2))
	   (append '(integer2) (subsequence x 3)))
	  ((equalp subseq3 '(integer * 1))
	   (append '(integer1) (subsequence x 3)))
	  (t x))))
  

;=============================================================================

; unification pattern matcher

(defun binding-value (binding)
 (cdr binding))

(defun variablep (v)
 (and (symbolp v)
      (char= (schar (symbol-name v) 0) #\%)))

(defun variable-value (variable bindings)
  (let ((binding (assoc variable bindings)))
    (values (binding-value binding)
            (not (null binding)))))

(defun unify (term-1 term-2 bindings)
  (cond 
    ((eq bindings 'fail) bindings)
    ((variablep term-1) 
     (maybe-extend-bindings term-1 term-2 bindings))
    ((variablep term-2)
     (maybe-extend-bindings term-2 term-1 bindings))
    ((or (atom term-1)
         (atom term-2))
     (if (equal term-1 term-2)
         bindings
         'fail))
    (t (unify (cdr term-1)
              (cdr term-2)
              (unify (car term-1)
                     (car term-2)
                     bindings)))))

(defun match (term-1 term-2 bindings)
 (let ((new-bindings (unify  term-1 term-2 bindings)))
  (if (eq 'fail new-bindings)
      nil
      (values t  new-bindings))))

(defun maybe-extend-bindings (variable value bindings)
  (multiple-value-bind (present-value found?)
    (variable-value variable bindings)
      (if found?
       (unify present-value
              value
              bindings) 
; no occurs check !
       (acons variable value bindings)))) 

;---------------------------------------------------------------------------
(defun setsyntax (x y) 
 (declare (ignore y))
 (set-macro-character x #'single-macro-character))

(defun single-macro-character (stream char)
  (declare (ignore stream))
  (let ((x (intern (string char)))) x))

(defun set-fortran-read nil
  (prog (vcs)
     (setq vcs 'vsingle-character-symbol)
     (setsyntax #\, vcs)
     (setsyntax #\: vcs)
     (setsyntax #\*  vcs)
     (setsyntax #\=  vcs)
     (setsyntax #\/  vcs)
     (setsyntax #\+  vcs)
     (setsyntax #\-  vcs)
     (setsyntax #\^  vcs)
     (setsyntax #\<  vcs)
     (setsyntax #\>  vcs)
     (setsyntax #\[  vcs)
     (setsyntax #\]  vcs)
     (setsyntax #\(  vcs)
     (setsyntax #\)  vcs)
     (setsyntax #\{  vcs)
     (setsyntax #\}  vcs)
     (setsyntax #\!  vcs)
     ;; reserved characters:
     (setsyntax #\$  vcs)
     (setsyntax #\@  vcs) 
     (setsyntax #\&  vcs) 
     (setsyntax #\~  vcs) 
     (setsyntax #\' vcs)
     (setsyntax #\| vcs)
     (setsyntax #\` vcs)
     (setsyntax #\\ vcs)
     (return t)))

(defun setlispread nil (setq *readtable* (copy-readtable nil)))
;-----------------------------------------------------------------------------
(defun lineread (stream)
  (prog (ans rans next-char)
loop1 
    (setq ans (cons 
	       (read-preserving-whitespace stream nil 'EOF nil) ans))
loop2  
    (setq next-char (peek-char nil stream nil 'EOF nil)) 
    (cond ((eql next-char #\Space)
	   (read-char stream nil 'EOF t) (go loop2)))
    (cond ((member next-char '(#\Newline EOF) :test #'eql)
	   (setq rans (nreverse ans))
	   (return rans)))
    (go loop1)))
;------------------------------------------------------------------------------
(defun read-six-chars (stream)
; (make-array '(6) :element-type 'string-char :initial-contents
    (list
       (read-char stream nil 'EOF t)
       (read-char stream nil 'EOF t)
       (read-char stream nil 'EOF t)
       (read-char stream nil 'EOF t)
       (read-char stream nil 'EOF t)
       (read-char stream nil 'EOF t)))

;------------------------------------------------------------------------------	

#+nil
(defun const (x n)
  (prog (lis)
     (setq lis ())
     (do ((i 1 (1+ i))) ((> i n)) (setq lis (cons x lis)))
     (return lis)
     ))
(defun const (x n)
  (make-list n :initial-element x))
;------------------------------------------------------------------------------	
(defun brackets-check (x)
  (prog (path-stack ce check-list ret-list)
 
     (cond ((and 
             (not (member '|(| x)) (not (member '|)| x))
             (not (member '|[| x)) (not (member '|[| x))
             (not (member '|{| x)) (not (member '|}| x))) (return x)))
     (setq path-stack '((0 0 0)) ;;;stack-top '(0 0 0)
           check-list x ce (car x) ret-list nil)
     loop
(cond ((null check-list)
       (cond 
         ((greaterp (caar path-stack) 0)
              (princ-reset 
'|Syntax error: missing right parenthesis ")"|) )
         ((greaterp (cadar path-stack) 0)
              (princ-reset 
'|Syntax error: missing right bracket "]"|) )
         ((greaterp (caddar path-stack) 0)
              (princ-reset 
'|Syntax error: missing right brace "}"|))
         (t (return ret-list)))))

(cond 
   ((equal ce '|(|) 
    (push (mapcar 'plus '(1 0 0) (car path-stack)) path-stack)
    (setq ret-list (gen-append ret-list nil (caadr path-stack))))
   ((equal ce '|[|) 
    (push (mapcar 'plus '(0 1 0) (car path-stack)) path-stack)
    (setq ret-list (gen-append ret-list '|[| (caar path-stack))))
   ((equal ce '|{|) 
    (push (mapcar 'plus '(0 0 1) (car path-stack)) path-stack)
    (setq ret-list (gen-append ret-list '|{| (caar path-stack))))
   ((and (equal ce '|)|) (or (nequal (mapcar 'diff (pop path-stack) '(1 0 0))
                               (car path-stack))
                              (lessp (caar path-stack) 0)))
                          (princ-reset 
'|Syntax error: right parenthesis ")" in an invalid position or unmatched|))
   ((equal ce '|)|))
   ((and (equal ce '|]|) (or (nequal (mapcar 'diff (pop path-stack) '(0 1 0))
                               (car path-stack))
                              (lessp (caar path-stack) 0)))
                          (princ-reset 
'|Syntax error: right bracket "]" in an invalid position or unmatched|))
   ((equal ce '|]|)
    (setq ret-list (gen-append ret-list ce (caar path-stack))))
   ((and (equal ce '|}|) (or (nequal (mapcar 'diff (pop path-stack) '(0 0 1))
                               (car path-stack))
                              (lessp (caar path-stack) 0)))
                          (princ-reset 
'|Syntax error: right brace "}" in an invalid position or unmatched|))
   ((equal ce '|]|)
    (setq ret-list (gen-append ret-list ce (caar path-stack))))
   (t (setq ret-list (gen-append ret-list ce (caar path-stack))))

)
      
    (setq check-list (cdr check-list) ce (car check-list))
    (go loop)))

;-----------------------------------------------------------------------------

(defun subsequence (seq start &optional (end (length seq)))
  (if (null seq) seq
                 (subseq seq start (min end (length seq)))))
;-----------------------------------------------------------------------------

(defun gen-append (lis x n) 
(cond ((equal n 0) (append1 lis x))
      (t (append1 (end-cdr lis) (gen-append (car (last lis)) x
                        (sub1 n))))))

(defun append1 (l x) (append l (list x)))
(defun plus (&rest args) (apply #'+ args))
(defun end-cdr (x) (butlast x))
(defun sub1 (x) (1- x))
(defun nequal (x y) (not (equal x y)))
(defun diff (x y) (- x y))
(defun lessp (x y) (< x y))
(defun greaterp (x y) (> x y))
;==============================================================================

(defun f-to-l (file)
   (fortran-to-lisp (preprocess file) "temp"))

;-----------------------------------------------------------------------------
;end of f2cl1.l
