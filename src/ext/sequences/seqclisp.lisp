; Dummy-Package für Sequences in CLISP
; Bruno Haible 24.04.1992

(provide 'sequences)

(in-package "SEQUENCES" :nicknames '("SEQ"))

(export '(elt subseq copy-seq length list-reverse reverse list-nreverse
          nreverse make-sequence concatenate map some every notany notevery
          reduce fill replace remove remove-if remove-if-not delete delete-if
          delete-if-not remove-duplicates delete-duplicates substitute
          substitute-if substitute-if-not nsubstitute nsubstitute-if
          nsubstitute-if-not find find-if find-if-not position position-if
          position-if-not count count-if count-if-not mismatch search sort
          stable-sort merge coerce do-sequence define-sequence))

(setf (symbol-function 'list-reverse) (symbol-function 'reverse))
(setf (symbol-function 'list-nreverse) (symbol-function 'nreverse))

; (do-sequence (var sequenceform [resultform]) {declaration}* {tag|statement}*)
(defmacro do-sequence ((var sequenceform &optional (resultform nil))
                       &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (system::parse-body body nil env)
    (setq declarations (if declarations `((DECLARE ,declarations)) '()))
    (let ((seqvar (gensym)))
      `(BLOCK NIL
         (LET ((,seqvar ,sequenceform))
           (LET ((,var NIL))
             ,@declarations
             ,var ; var wird nur zum Schein ausgewertet
             (MAP NIL
                  #'(LAMBDA (,var) ,@declarations (TAGBODY ,@body-rest))
                  ,seqvar
             )
             ,resultform
       ) ) )
) ) )

(defmacro define-sequence-1 (name &rest functions)
  `(SYSTEM::%DEFSEQ
     (VECTOR
       ',name
       ,@(mapcar #'(lambda (function)
                     (if (and (consp function)
                              (eq (first function) 'FUNCTION)
                              (consp (second function))
                              (eq (first (second function)) 'LAMBDA)
                         )
                       `(FUNCTION
                          (LAMBDA (SEQ ,@(second (second function)))
                            ,@(cddr (second function))
                        ) )
                       `(FUNCTION
                          (LAMBDA (SEQ &REST SEQ-ARGS)
                            (APPLY ,function SEQ-ARGS)
                        ) )
                   ) )
                 functions
         )
   ) )
)

(defmacro define-sequence
  (name &key init upd endtest fe-init fe-upd fe-endtest access access-set
             copy length make elt set-elt init-start fe-init-end
  )
  (unless upd (error ":UPD muß angegeben werden."))
  (unless fe-upd (error ":FE-UPD muß angegeben werden."))
  (unless access (error ":ACCESS muß angegeben werden."))
  (unless access-set (error ":ACCESS-SET muß angegeben werden."))
  (unless endtest (error ":ENDTEST muß angegeben werden."))
  (unless fe-endtest (error ":FE-ENDTEST muß angegeben werden."))
  (unless make (error ":MAKE muß angegeben werden."))
  (unless (or init init-start)
    (error "Es muß :INIT oder :INIT-START angegeben werden.")
  )
  (cond ((and init (not init-start))
         (setq init-start
           `#'(lambda (index)
                (let ((pointer (funcall ,init)))
                  (dotimes (i index) (setq pointer (funcall ,upd pointer)))
                  pointer
              ) )
        ))
        ((and init-start (not init))
         (setq init
           `#'(lambda () (funcall ,init-start 0))
  )     ))
  (unless elt
    (setq elt
      `#'(lambda (index) (funcall ,access (funcall ,init-start index)))
  ) )
  (unless set-elt
    (setq set-elt
      `#'(lambda (index value)
           (funcall ,access-set (funcall ,init-start index) value)
         )
  ) )
  (unless length
    (setq length
      `#'(lambda ()
           (do ((pointer (funcall ,init) (funcall ,upd pointer))
                (i 0 (1+ i)))
               ((funcall ,endtest pointer) i)
         ) )
  ) )
  (unless (or fe-init fe-init-end)
    (error "Es muß :FE-INIT oder :FE-INIT-END angegeben werden.")
  )
  (cond ((and fe-init (not fe-init-end))
         (setq fe-init-end
           `#'(lambda (index)
                (let ((pointer (funcall ,fe-init)))
                  (dotimes (i (- (funcall ,length) index))
                    (setq pointer (funcall ,fe-upd pointer))
                  )
                  pointer
              ) )
        ))
        ((and fe-init-end (not fe-init))
         (setq fe-init
           `#'(lambda () (funcall ,fe-init-end (funcall ,length)))
  )     ))
  (unless copy
    (warn ":COPY nicht angegeben, interpretiere als #'IDENTITY")
    (setq copy '#'identity)
  )
  `(DEFINE-SEQUENCE-1 ,name
     ,init ,upd ,endtest ,fe-init ,fe-upd ,fe-endtest ,access ,access-set
     ,copy ,length ,make ,elt ,set-elt ,init-start ,fe-init-end
   )
)

