; Funktionen für Sequences
; Bruno Haible 16.09.1988, 29.12.1988, 24.04.1992

(provide 'sequences)

(in-package "SEQUENCES" :nicknames '("SEQ"))

(eval-when (load compile eval)
  (defvar *seq-package* (find-package "SEQ"))
)

(shadow '(elt subseq copy-seq length list-reverse reverse list-nreverse
          nreverse make-sequence concatenate map some every notany notevery
          reduce fill replace remove remove-if remove-if-not delete delete-if
          delete-if-not remove-duplicates delete-duplicates substitute
          substitute-if substitute-if-not nsubstitute nsubstitute-if
          nsubstitute-if-not find find-if find-if-not position position-if
          position-if-not count count-if count-if-not mismatch search sort
          stable-sort merge coerce))

(export '(elt subseq copy-seq length list-reverse reverse list-nreverse
          nreverse make-sequence concatenate map some every notany notevery
          reduce fill replace remove remove-if remove-if-not delete delete-if
          delete-if-not remove-duplicates delete-duplicates substitute
          substitute-if substitute-if-not nsubstitute nsubstitute-if
          nsubstitute-if-not find find-if find-if-not position position-if
          position-if-not count count-if count-if-not mismatch search sort
          stable-sort merge coerce do-sequence define-sequence))

#|
   Ziele:
1. Mehrere Typen (Listen, Vektoren, Bäume, Strings, ...)
   dienen als Sequences.
2. Das Ganze ist compilierbar.
   Verwende daher keine Macros, die vom Typ der Sequence abhängen.
3. Benutzerdefinierbare Sequences
4. Keine Closures, nur globale Variable. (Zur Effizienzsteigerung)

   Zusätze zu COMMON LISP:
- Benutzerdefinierbare Sequence-Typen
- Keywords :START und :END bei SORT und STABLE-SORT
- NREVERSE liefert stets ein Objekt, das EQ zum Argument ist
- Keyword :UPDATE zu MAKE-SEQUENCE,
  z.B. (make-sequence 'list 4 :initial-element 1 :update #'1+) ==> (1 2 3 4)
|#

(proclaim '(special
  SEQ1             SEQ2             ; die fragliche Sequence
  SEQ1-TYPE        SEQ2-TYPE        ; der Typ der Sequence, ein Symbol
  ; Funktionen:
  SEQ1-ACCESS      SEQ2-ACCESS
  SEQ1-ACCESS-SET  SEQ2-ACCESS-SET
  SEQ1-COPY        SEQ2-COPY
  SEQ1-INIT        SEQ2-INIT
  SEQ1-UPD         SEQ2-UPD
  SEQ1-ENDTEST     SEQ2-ENDTEST
  SEQ1-FE-INIT     SEQ2-FE-INIT
  SEQ1-FE-UPD      SEQ2-FE-UPD
  SEQ1-FE-ENDTEST  SEQ2-FE-ENDTEST
  SEQ1-LENGTH      SEQ2-LENGTH
  SEQ1-MAKE        SEQ2-MAKE
  SEQ1-ELT         SEQ2-ELT
  SEQ1-SET-ELT     SEQ2-SET-ELT
  SEQ1-INIT-START  SEQ2-INIT-START
  SEQ1-FE-INIT-END SEQ2-FE-INIT-END
))

#| (with-sequence (SEQi (TYPE UPD MAKE) form) . body)
   bewirkt, daß form ausgewertet wird, SEQi an das Ergebnis gebunden
   wird, alle Operationen XXX=TYPE,UPD,... der Reihe nach initialisiert werden:
   Die Variable SEQi-XXX wird dynamisch an den Wert von (SEQi-XXX) gebunden.
   i=1 oder 2.
|#
(defmacro with-sequence (&whole w (name operationen &optional (form nil svar))
                         &body body)
  (if (and (member name '(SEQ1 SEQ2))
           (dolist (op operationen t)
             (unless (member op '(TYPE ACCESS ACCESS-SET COPY INIT UPD ENDTEST
                                  FE-INIT FE-UPD FE-ENDTEST LENGTH MAKE
                                  ELT SET-ELT INIT-START FE-INIT-END))
               (return nil)
      )    ) )
    (let ((bindlist nil))
      (when svar (push `(,name ,form) bindlist))
      (dolist (op operationen)
        (let* ((sym (intern (lisp:concatenate 'string
                              (symbol-name name) "-" (symbol-name op))
                            *seq-package*
              ))    )
          (push `(,sym (,sym)) bindlist)
      ) )
      `(LET* ,(lisp:nreverse bindlist) ,@body)
    )
    (error "Falscher Gebrauch von WITH-SEQUENCE: ~S" w)
) )

#| Erklärung der Einzelfunktionen SEQi-XXX:

Ein "Pointer" ist etwas, was durch die Sequence durchlaufen kann.
Es gibt Pointer, die von links nach rechts laufen;
  sie werden mit INIT oder INIT-START kreiert, mit COPY kopiert,
             mit UPD um eine Stelle weitergerückt,
             mit ENDTEST getestet, ob sie am Ende der Sequence angelangt sind,
             mit ACCESS wird das Element, worauf der Pointer zeigt, geholt,
             mit ACCESS-SET wird das Element, worauf der Pointer zeigt, gesetzt.
Es gibt auch Pointer, die von links nach rechts laufen;
  sie werden mit FE-INIT oder FE-INIT-END kreiert, mit COPY kopiert,
             mit FE-UPD um eine Stelle nach links weitergerückt,
             mit FE-ENDTEST getestet, ob sie am Ende der Sequence angelangt sind,
             mit ACCESS wird das Element, worauf der Pointer zeigt, geholt.
  Für sie funktioniert ACCESS-SET nicht.

Durchlaufe-Operationen:
INIT          (lambda () ...) -> pointer
              liefert den Pointer zu SEQi, der ganz links steht.
UPD           (lambda (pointer) ...) -> pointer
              liefert zu einem Pointer den Pointer eins weiter rechts.
              SEQi-UPD kann voraussetzen, daß dabei der rechte Rand von
              SEQi nicht überschritten wird.
ENDTEST       (lambda (pointer) ...) -> boolean
              testet, ob dieser Pointer am rechten Rand von SEQi steht.
Dasselbe "FROM END" :
FE-INIT       (lambda () ...) -> pointer
              liefert den Pointer zu SEQi, der ganz rechts steht.
FE-UPD        (lambda (pointer) ...) -> pointer
              liefert zu einem Pointer den Pointer eins weiter links.
              SEQi-FE-UPD kann voraussetzen, daß dabei der linke Rand von
              SEQi nicht überschritten wird.
FE-ENDTEST    (lambda (pointer) ...) -> boolean
              testet, ob dieser Pointer am linken Rand von SEQi steht.
Zugriff mit Pointer:
ACCESS        (lambda (pointer) ...) -> value
              liefert zu einem Pointer in SEQi das entsprechende Element an
              dieser Stelle.
ACCESS-SET    (lambda (pointer value) ...) ->
              setzt das Element in SEQi, auf das der Pointer zeigt, auf den
              gegebenen Wert. Nur bei von links nach rechts laufenden Pointern!
COPY          (lambda (pointer) ...) -> pointer
              liefert eine Kopie des Pointers zu SEQi (denn UPD und FE-UPD
              können destruktiv auf den Pointern arbeiten)
Gesamtlänge:
LENGTH        (lambda () ...) -> size
              liefert die (aktive) Länge der Sequence SEQi.
MAKE          (lambda (size) ...) -> sequence
              liefert eine neu allozierte, leere Sequence, die vom Typ
              SEQi-TYPE ist und die angegebene Länge hat.
Zugriff über Index (meist ineffizienter als über Pointer):
ELT           (lambda (index) ...) -> value
              liefert (ELT SEQi index)
SET-ELT       (lambda (index value) ...) ->
              setzt (ELT SEQi index) auf value.
INIT-START    (lambda (index) ...) -> pointer
              liefert einen nach rechts laufenden Pointer in SEQi
              ab Position index. Muß den Range-test selbst durchführen.
FE-INIT-END   (lambda (index) ...) -> pointer
              liefert einen nach links laufenden Pointer in SEQi
              an Position index. Muß den Range-test selbst durchführen.
|#

(defmacro define-sequence-function (name . body)
  `(progn
     (defun ,(intern (lisp:concatenate 'string "SEQ1-" (symbol-name name))
                     *seq-package*
             )
            ,@(subst 'seq1 'seq body)
     )
     (defun ,(intern (lisp:concatenate 'string "SEQ2-" (symbol-name name))
                     *seq-package*
             )
            ,@(subst 'seq2 'seq body)
   ) )
)

; Die Zugriffsmethode auf einen Sequence-Typ:
(defstruct (sequence-definition (:conc-name "SD-"))
  init upd endtest fe-init fe-upd fe-endtest access access-set copy length make
  elt set-elt init-start fe-init-end
)
; So eine sequence-definition steht unter dem Namen auf der Propertyliste
; unter den Properties SEQUENCE-DEFINITION-1 und SEQUENCE-DEFINITION-2,
; unter Property SEQUENCE-DEFINITION steht T.

(defmacro define-sequence-1 (name &rest args)
  `(progn
     (setf (get ',name 'sequence-definition) T)
     (setf (get ',name 'sequence-definition-1)
           (make-sequence-definition ,@(subst 'seq1 'seq args))
     )
     (setf (get ',name 'sequence-definition-2)
           (make-sequence-definition ,@(subst 'seq2 'seq args))
     )
   )
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
     :init ,init :upd ,upd :endtest ,endtest :fe-init ,fe-init :fe-upd ,fe-upd
     :fe-endtest ,fe-endtest :access ,access :access-set ,access-set :copy ,copy
     :length ,length :make ,make :elt ,elt :set-elt ,set-elt
     :init-start ,init-start :fe-init-end ,fe-init-end
   )
)

(define-sequence LIST
  :init        #'(lambda () seq)
  :upd         #'(lambda (pointer) (cdr pointer))
  :endtest     #'(lambda (pointer) (atom pointer))
  :fe-init     #'(lambda () (list-reverse seq))
  :fe-upd      #'(lambda (pointer) (cdr pointer))
  :fe-endtest  #'(lambda (pointer) (atom pointer))
  :access      #'(lambda (pointer) (car pointer))
  :access-set  #'(lambda (pointer value) (rplaca pointer value))
  :copy        #'(lambda (pointer) pointer)
  :length      #'(lambda () (do ((L seq (cdr L)) (N 0 (1+ N))) ((atom L) N)))
  :make        #'(lambda (size) (make-list size))
  :elt         #'(lambda (index)
                   (do ((L seq (cdr L)) (N 0 (1+ N)))
                       (nil)
                     (if (atom L) (error "Zu großer Index in ELT: ~S" index))
                     (if (= N index) (return (car L)))
                 ) )
  :set-elt     #'(lambda (index value)
                   (do ((L seq (cdr L)) (N 0 (1+ N)))
                       (nil)
                     (if (atom L) (error "Zu großer Index in ELT: ~S" index))
                     (if (= N index) (return (rplaca L value)))
                 ) )
  :init-start  #'(lambda (index)
                   (do ((L seq (cdr L)) (N 0 (1+ N)))
                       ((= N index) (return L))
                     (if (atom L) (error "Unzulässiger :START - Index : ~S" index))
                 ) )
  :fe-init-end #'(lambda (index)
                   (if (<= 0 index)
                     (do* ((L1 nil (cons (car L2) L1))
                           (L2 seq (cdr L2))
                           (i index (1- i)))
                          ((zerop i) L1)
                       (if (atom L2)
                         (error "Unzulässiger :END - Index : ~S" index)
                     ) )
                     (error "Unzulässiger :END - Index : ~S" index)
                 ) )
)

(defun active-length (vector)
  (if (array-has-fill-pointer-p vector)
    (fill-pointer vector)
    (array-dimension vector 0)
) )

(define-sequence vector ; VECTOR steht für GENERAL-VECTOR
  :init        #'(lambda () 0)
  :upd         #'(lambda (pointer) (1+ pointer))
  :endtest     #'(lambda (pointer) (= pointer (active-length seq)))
  :fe-init     #'(lambda () (1- (active-length seq)))
  :fe-upd      #'(lambda (pointer) (1- pointer))
  :fe-endtest  #'(lambda (pointer) (minusp pointer))
  :access      #'(lambda (pointer) (aref seq pointer))
  :access-set  #'(lambda (pointer value) (setf (aref seq pointer) value))
  :copy        #'(lambda (pointer) pointer)
  :length      #'(lambda () (active-length seq))
  :make        #'(lambda (size) (make-array size))
  :elt         #'(lambda (index) (aref seq index))
  :set-elt     #'(lambda (index value) (setf (aref seq index) value))
  :init-start  #'(lambda (index)
                   (if (<= 0 index (active-length seq))
                     index
                     (error "Unzulässiger :START - Index : ~S" index)
                 ) )
  :fe-init-end #'(lambda (index)
                   (if (<= 0 index (active-length seq))
                     (1- index)
                     (error "Unzulässiger :END - Index : ~S" index)
                 ) )
)

(define-sequence string
  :init        #'(lambda () 0)
  :upd         #'(lambda (pointer) (1+ pointer))
  :endtest     #'(lambda (pointer) (= pointer (active-length seq)))
  :fe-init     #'(lambda () (1- (active-length seq)))
  :fe-upd      #'(lambda (pointer) (1- pointer))
  :fe-endtest  #'(lambda (pointer) (minusp pointer))
  :access      #'(lambda (pointer) (char seq pointer))
  :access-set  #'(lambda (pointer value) (setf (char seq pointer) value))
  :copy        #'(lambda (pointer) pointer)
  :length      #'(lambda () (active-length seq))
  :make        #'(lambda (size) (make-string size))
  :elt         #'(lambda (index) (char seq index))
  :set-elt     #'(lambda (index value) (setf (char seq index) value))
  :init-start  #'(lambda (index)
                   (if (<= 0 index (active-length seq))
                     index
                     (error "Unzulässiger :START - Index : ~S" index)
                 ) )
  :fe-init-end #'(lambda (index)
                   (if (<= 0 index (active-length seq))
                     (1- index)
                     (error "Unzulässiger :END - Index : ~S" index)
                 ) )
)

(define-sequence bit-vector
  :init        #'(lambda () 0)
  :upd         #'(lambda (pointer) (1+ pointer))
  :endtest     #'(lambda (pointer) (= pointer (active-length seq)))
  :fe-init     #'(lambda () (1- (active-length seq)))
  :fe-upd      #'(lambda (pointer) (1- pointer))
  :fe-endtest  #'(lambda (pointer) (minusp pointer))
  :access      #'(lambda (pointer) (bit seq pointer))
  :access-set  #'(lambda (pointer value) (setf (bit seq pointer) value))
  :copy        #'(lambda (pointer) pointer)
  :length      #'(lambda () (active-length seq))
  :make        #'(lambda (size) (make-array size :element-type 'bit))
  :elt         #'(lambda (index) (bit seq index))
  :set-elt     #'(lambda (index value) (setf (aref seq index) value))
  :init-start  #'(lambda (index)
                   (if (<= 0 index (active-length seq))
                     index
                     (error "Unzulässiger :START - Index : ~S" index)
                 ) )
  :fe-init-end #'(lambda (index)
                   (if (<= 0 index (active-length seq))
                     (1- index)
                     (error "Unzulässiger :END - Index : ~S" index)
                 ) )
)

(require 'avlseq) ; (define-sequence avl-tree ...)


; (valid-sequence name) liefert
; einen Error, falls name kein gültiger Sequence-Typ-Bezeichner ist,
; oder einen zu name äquivalenten gültigen Sequence-Typ-Bezeichner.
(defun valid-sequence (name)
  (or
    (and (symbolp name)
         (cond ((eq name 'simple-vector)     'vector)
               ((eq name 'simple-string)     'string)
               ((eq name 'simple-bit-vector) 'bit-vector)
               ((eq name 'array)             'vector)
               ((eq name 'simple-array)      'vector)
               ((get name 'sequence-definition) name)
    )    )
    (error "Es gibt keine Sequences vom Typ ~S." name)
) )

(define-sequence-function TYPE ()
  (or
    (cond ((listp seq) 'LIST)
          ((stringp seq) 'STRING)
          ((bit-vector-p seq) 'BIT-VECTOR)
          (#+VAX (sys::structurep seq)
           #-VAX (typep seq 'structure)
           (let ((name (type-of seq)))
             (if (get name 'sequence-definition) name nil)
          ))
          ((vectorp seq) 'VECTOR)
          (t nil)
    )
    (error "Das ist keine Sequence: ~S" seq)
) )

(defun seq1-init () (sd-init (get seq1-type 'sequence-definition-1)))
(defun seq1-upd () (sd-upd (get seq1-type 'sequence-definition-1)))
(defun seq1-endtest () (sd-endtest (get seq1-type 'sequence-definition-1)))
(defun seq1-fe-init () (sd-fe-init (get seq1-type 'sequence-definition-1)))
(defun seq1-fe-upd () (sd-fe-upd (get seq1-type 'sequence-definition-1)))
(defun seq1-fe-endtest () (sd-fe-endtest (get seq1-type 'sequence-definition-1)))
(defun seq1-access () (sd-access (get seq1-type 'sequence-definition-1)))
(defun seq1-access-set () (sd-access-set (get seq1-type 'sequence-definition-1)))
(defun seq1-copy () (sd-copy (get seq1-type 'sequence-definition-1)))
(defun seq1-length () (sd-length (get seq1-type 'sequence-definition-1)))
(defun seq1-make () (sd-make (get seq1-type 'sequence-definition-1)))
(defun seq1-elt () (sd-elt (get seq1-type 'sequence-definition-1)))
(defun seq1-set-elt () (sd-set-elt (get seq1-type 'sequence-definition-1)))
(defun seq1-init-start () (sd-init-start (get seq1-type 'sequence-definition-1)))
(defun seq1-fe-init-end () (sd-fe-init-end (get seq1-type 'sequence-definition-1)))

(defun seq2-init () (sd-init (get seq2-type 'sequence-definition-2)))
(defun seq2-upd () (sd-upd (get seq2-type 'sequence-definition-2)))
(defun seq2-endtest () (sd-endtest (get seq2-type 'sequence-definition-2)))
(defun seq2-fe-init () (sd-fe-init (get seq2-type 'sequence-definition-2)))
(defun seq2-fe-upd () (sd-fe-upd (get seq2-type 'sequence-definition-2)))
(defun seq2-fe-endtest () (sd-fe-endtest (get seq2-type 'sequence-definition-2)))
(defun seq2-access () (sd-access (get seq2-type 'sequence-definition-2)))
(defun seq2-access-set () (sd-access-set (get seq2-type 'sequence-definition-2)))
(defun seq2-copy () (sd-copy (get seq2-type 'sequence-definition-2)))
(defun seq2-length () (sd-length (get seq2-type 'sequence-definition-2)))
(defun seq2-make () (sd-make (get seq2-type 'sequence-definition-2)))
(defun seq2-elt () (sd-elt (get seq2-type 'sequence-definition-2)))
(defun seq2-set-elt () (sd-set-elt (get seq2-type 'sequence-definition-2)))
(defun seq2-init-start () (sd-init-start (get seq2-type 'sequence-definition-2)))
(defun seq2-fe-init-end () (sd-fe-init-end (get seq2-type 'sequence-definition-2)))


(defun elt (sequence index)
  (with-sequence (seq1 (TYPE ELT) sequence)
    (funcall seq1-elt index)
) )

(defun set-elt (sequence index value)
  (with-sequence (seq1 (TYPE SET-ELT) sequence)
    (funcall seq1-set-elt index value)
  )
  value
)

(defsetf elt set-elt)
#| ; oder äquivalent:
(defsetf elt (sequence index) (value)
  `(set-elt ,sequence ,index ,value)
)
|#

(defmacro test-start-end (startvar endvar)
  (let ((startkw (intern (symbol-name startvar) (find-package "KEYWORD")))
        (endkw (intern (symbol-name endvar) (find-package "KEYWORD"))))
    `(PROGN
       (UNLESS (AND (INTEGERP ,startvar) (>= ,startvar 0))
         (ERROR "~S muß ein Integer >= 0 sein, nicht ~S" ',startkw ,startvar)
       )
       (UNLESS (AND (INTEGERP ,endvar) (>= ,endvar 0))
         (ERROR "~S muß ein Integer >= 0 sein, nicht ~S" ',endkw ,endvar)
       )
       (UNLESS (>= ,endvar ,startvar)
         (ERROR "~S = ~S darf ~S = ~S nicht übersteigen."
                ',startkw ,startvar ',endkw ,endvar
     ) ) )
) )

(defmacro test-start-end-1 (startvar endvar)
  (let ((startkw (intern (symbol-name startvar) (find-package "KEYWORD")))
        (endkw (intern (symbol-name endvar) (find-package "KEYWORD"))))
    `(PROGN
       (UNLESS (AND (INTEGERP ,startvar) (>= ,startvar 0))
         (ERROR "~S muß ein Integer >= 0 sein, nicht ~S" ',startkw ,startvar)
       )
       (WHEN ,endvar
         (UNLESS (AND (INTEGERP ,endvar) (>= ,endvar 0))
           (ERROR "~S muß ein Integer >= 0 sein, nicht ~S" ',endkw ,endvar)
         )
         (UNLESS (>= ,endvar ,startvar)
           (ERROR "~S = ~S darf ~S = ~S nicht übersteigen."
                  ',startkw ,startvar ',endkw ,endvar
     ) ) ) )
) )


(defun subseq (sequence start &optional end)
  (with-sequence (seq1 (TYPE LENGTH INIT-START UPD ACCESS MAKE) sequence)
    (unless end (setq end (funcall seq1-length)))
    (test-start-end start end)
    (with-sequence (seq2 (TYPE INIT UPD ACCESS-SET)
                         (funcall seq1-make (- end start))
                   )
      (do ((pointer1 (funcall seq1-init-start start) (funcall seq1-upd pointer1))
           (pointer2 (funcall seq2-init) (funcall seq2-upd pointer2))
           (count (- end start) (1- count)))
          ((zerop count))
        (funcall seq2-access-set pointer2 (funcall seq1-access pointer1))
      )
      seq2
) ) )

(defsetf subseq (sequence start &optional end) (new-sequence)
  `(progn (replace ,sequence ,new-sequence :start1 ,start :end1 ,end)
          ,new-sequence
)  )

(defun copy-seq (sequence)
  (with-sequence (seq1 (TYPE LENGTH MAKE INIT UPD ACCESS) sequence)
    (let ((l (funcall seq1-length)))
      (with-sequence (seq2 (TYPE INIT UPD ACCESS-SET) (funcall seq1-make l))
        (do ((pointer1 (funcall seq1-init) (funcall seq1-upd pointer1))
             (pointer2 (funcall seq2-init) (funcall seq2-upd pointer2))
             (count l (1- count)))
            ((zerop count))
          (funcall seq2-access-set pointer2 (funcall seq1-access pointer1))
        )
        seq2
) ) ) )

(defun length (sequence)
  (with-sequence (seq1 (TYPE LENGTH) sequence)
    (funcall seq1-length)
) )

(defun list-reverse (L)
  (do ((L1 L (cdr L1))
       (L2 nil (cons (car L1) L2)))
      ((atom L1) L2)
) )

(defun reverse (sequence)
  (if (listp sequence)
    (list-reverse sequence)
    (with-sequence (seq1 (TYPE LENGTH FE-INIT FE-UPD ACCESS MAKE) sequence)
      (let ((l (funcall seq1-length)))
        (with-sequence (seq2 (TYPE INIT UPD ACCESS-SET) (funcall seq1-make l))
          (do ((pointer1 (funcall seq1-fe-init) (funcall seq1-fe-upd pointer1))
               (pointer2 (funcall seq2-init) (funcall seq2-upd pointer2))
               (count l (1- count)))
              ((zerop count))
            (funcall seq2-access-set pointer2 (funcall seq1-access pointer1))
          )
          seq2
) ) ) ) )

#|; einfache Version:
(defun list-nreverse (L)
  (do ((L1 L (cdr L1))
       (L2 nil (rplacd L1 L2)))
      ((atom L1) L2)
) )
|#
; kompliziertere Version, erfüllt (eq x (list-nreverse x)) :
(defun list-nreverse (L)
  (cond ((atom L) L)
        ((atom (cdr L)) L)
        ((atom (cddr L)) (rotatef (car L) (cadr L)) L)
        (t ; Liste mit mindestens 3 Elementen:
          (let ((L1 (cdr L)))
            (do ((L3 L1 (cdr L3))
                 (L2 nil (rplacd L3 L2)))
                ((atom (cdr L3))
                 (setf (cdr L) L2)
                 (setf (cdr L1) L3)
                 (rotatef (car L) (car L3))
            )   )
            L
) )     ) )

(defun nreverse (sequence)
  (if (listp sequence)
    (list-nreverse sequence)
    (with-sequence (seq1 (TYPE) sequence)
      (if (or (eq seq1-type 'VECTOR) (eq seq1-type 'STRING) (eq seq1-type 'BIT-VECTOR))
        (with-sequence (seq1 (LENGTH INIT UPD FE-INIT FE-UPD ACCESS ACCESS-SET))
          (do ((count (floor (funcall seq1-length) 2) (1- count))
               (pointer1 (funcall seq1-init) (funcall seq1-upd pointer1))
               (pointer2 (funcall seq1-fe-init) (funcall seq1-fe-upd pointer2)))
              ((zerop count))
            (let ((value1 (funcall seq1-access pointer1))
                  (value2 (funcall seq1-access pointer2)))
              (funcall seq1-access-set pointer1 value2)
              (funcall seq1-access-set pointer2 value1)
          ) )
          seq1
        )
        ; allgemeine Sequence
        ; allgemeine Methode für Sequence der Länge l :
        ; nreverse auf die ersten (floor l 2) Elemente,
        ; nreverse auf die letzte (floor l 2) Elemente,
        ; vertausche beide Blöcke.
        ; (Falls l ungerade ist, bleibt das mittlere Element unverändert.)
#|      (with-sequence (LENGTH INIT UPD ACCESS ACCESS-SET)
          (labels
            ((rev-part (pointer k)
             ; dreht ab Pointer genau k>=1 Elemente um,
             ; liefert den Pointer danach.
               (if (= k 1)
                 (funcall seq1-upd pointer)
                 (let* ((k2 (floor k 2))
                        (pointer-mid (rev-part (funcall seq1-copy pointer) k2))
                        (pointer-mid
                          (if (oddp k) (funcall seq1-upd pointer-mid) pointer-mid)
                        )
                        (pointer-right (rev-part (funcall seq1-copy pointer-mid) k2))
                       )
                   (do ((pointer1 pointer-left (funcall seq1-upd pointer1))
                        (pointer2 pointer-mid (funcall seq1-upd pointer2))
                        (i k2 (1- i)))
                       ((zerop i))
                     (let ((value1 (funcall seq1-access pointer1))
                           (value2 (funcall seq1-access pointer2)))
                       (funcall seq1-access-set pointer1 value2)
                       (funcall seq1-access-set pointer2 value1)
                   ) )
                   pointer-right
            )) ) )
            (rev-part (funcall seq1-init) (funcall seq1-length))
            seq1
        ) )
|#      ; dasselbe iterativ statt rekursiv:
        (with-sequence (seq1 (LENGTH INIT INIT-START UPD ACCESS ACCESS-SET))
          (let* ((l (funcall seq1-length))
                 (k l))
            (dotimes (j (1- l))
              ; k = (floor l 2^j)
              ; vertausche 2^j mal
              ;   einen Block der Länge (floor l 2^(j+1)) mit dem nächsten:
              (let* ((stack 0)
                     (k1 (- k (setq k (ash k -1))))
                     (pointer1 (funcall seq1-init))
                     (pointer2 (funcall seq1-init-start k1)))
                ; pointer1 und pointer2 laufen gemeinsam nach rechts, dabei hat
                ; pointer2 einen Vorsprung von
                ; k1 = (- (floor l 2^j) (floor l 2^(j+1)))
                (loop
                  ; zwei Blöcke der Länge k vertauschen:
                  (dotimes (i k) ; k = (floor l 2^(j+1))
                    (let ((value1 (funcall seq1-access pointer1))
                          (value2 (funcall seq1-access pointer2)))
                      (funcall seq1-access-set pointer1 value2)
                      (funcall seq1-access-set pointer2 value1)
                    )
                    (setq pointer1 (funcall seq1-upd pointer1))
                    (setq pointer2 (funcall seq1-upd pointer2))
                  )
                  (setq stack (+ stack 1))
                  (if (logbitp j stack) (return)) ; bei stack=2^j war's das
                  ; pointer1 und pointer2 um k1 Stellen weiterrücken:
                  (dotimes (i k1)
                    (setq pointer1 (funcall seq1-upd pointer1))
                    (setq pointer2 (funcall seq1-upd pointer2))
                  )
                  ; Es ende (- stack 1) auf genau r Einsen und stack auf genau r
                  ; Nullen (r>=0). Beide Pointer müssen genau dann um eins
                  ; weitergerückt werden, wenn (logbitp (- j 1 r) l) gilt.
                  (when (logbitp l (- j (integer-length (logxor stack (1- stack)))))
                    (setq pointer1 (funcall seq1-upd pointer1))
                    (setq pointer2 (funcall seq1-upd pointer2))
          ) ) ) ) )
          seq1
        )
) ) ) )

(defun make-sequence (type size
                      &key (initial-element nil given) (update #'identity))
  (setq type (valid-sequence type))
  (let ((seq1-type type))
    (with-sequence (seq1 (MAKE))
      (with-sequence (seq1 () (funcall seq1-make size))
        (if given
          (with-sequence (seq1 (INIT UPD ACCESS-SET))
            (do ((pointer (funcall seq1-init) (funcall seq1-upd pointer))
                 (element initial-element (funcall update element))
                 (count size (1- count)))
                ((zerop count))
              (funcall seq1-access-set pointer element)
        ) ) )
        seq1
) ) ) )

(defun coerce (sequence result-type) ; Nur für Sequences!
  (setq result-type (valid-sequence result-type))
  (with-sequence (seq1 (TYPE LENGTH INIT UPD ACCESS) sequence)
    (if (eq seq1-type result-type)
      sequence
      (let ((size (funcall seq1-length))
            (seq2-type result-type))
        (with-sequence (seq2 (MAKE))
          (with-sequence (seq2 (INIT UPD ACCESS-SET) (funcall seq2-make size))
            (do ((pointer1 (funcall seq1-init) (funcall seq1-upd pointer1))
                 (pointer2 (funcall seq2-init) (funcall seq2-upd pointer2))
                 (i size (1- i)))
                ((zerop i))
              (funcall seq2-access-set pointer2 (funcall seq1-access pointer1))
            )
            seq2
      ) ) )
) ) )

(defun concatenate (result-type &rest sequences)
  (setq result-type (valid-sequence result-type))
  (let* ((lengths (mapcar #'length sequences))
         (size (apply #'+ lengths))
         (seq1-type result-type))
    (with-sequence (seq1 (MAKE))
      (with-sequence (seq1 (INIT UPD ACCESS-SET) (funcall seq1-make size))
        (let ((pointer1 (funcall seq1-init)))
          (do ((sequencesr sequences (cdr sequencesr))
               (lengthsr lengths (cdr lengthsr)))
              ((null lengthsr))
            (with-sequence (seq2 (TYPE INIT UPD ACCESS) (car sequencesr))
              (do ((pointer2 (funcall seq2-init) (funcall seq2-upd pointer2))
                   (count (car lengthsr) (1- count)))
                  ((zerop count))
                (funcall seq1-access-set pointer1
                                         (funcall seq2-access pointer2)
                )
                (setq pointer1 (funcall seq1-upd pointer1))
        ) ) ) )
        seq1
) ) ) )

(defun map (result-type fun &rest sequences)
  (if (null sequences)
    (error "MAP ohne Sequence-Argument aufgerufen.")
    (let ((seq-descriptors
            (mapcar #'(lambda (seq)
                        (with-sequence (seq1 (TYPE INIT UPD ENDTEST ACCESS)
                                             seq)
                          (list (cons (funcall seq1-init) (funcall seq1-init))
                                seq1-type seq1-upd seq1-endtest seq1-access
                      ) ) )
                    sequences
         )) )
      (cond ((null result-type) ; nur Seiteneffekt erwünscht
             (loop
               (do ((seq-r sequences (cdr seq-r))
                    (seq-desc-r seq-descriptors (cdr seq-desc-r))
                    (arglist nil))
                   ((null seq-r) (apply fun (list-nreverse arglist)))
                 (let ((seq1 (car seq-r))
                       (seq1-type (second (car seq-desc-r)))
                       (seq1-upd (third (car seq-desc-r)))
                       (seq1-endtest (fourth (car seq-desc-r)))
                       (seq1-access (fifth (car seq-desc-r)))
                       (pointer (caar (car seq-desc-r))))
                   (if (funcall seq1-endtest pointer) (return-from map nil))
                   (push (funcall seq1-access pointer) arglist)
                   (setf (caar (car seq-desc-r)) (funcall seq1-upd pointer))
            )) ) )
            ((setq result-type (valid-sequence result-type))
             ; erst Länge des Ergebnisses bestimmen, dann erst anwenden:
             (let ((size
                     (do ((count 0 (1+ count)))
                         ((do ((seq-r sequences (cdr seq-r))
                               (seq-desc-r seq-descriptors (cdr seq-desc-r)))
                              ((null seq-r) nil)
                            (let ((seq1 (car seq-r))
                                  (seq1-type (second (car seq-desc-r)))
                                  (seq1-upd (third (car seq-desc-r)))
                                  (seq1-endtest (fourth (car seq-desc-r)))
                                  (seq1-access (fifth (car seq-desc-r)))
                                  (pointer (cdar (car seq-desc-r))))
                              (if (funcall seq1-endtest pointer) (return t))
                              (setf (cdar (car seq-desc-r))
                                    (funcall seq1-upd pointer)
                          ) ) )
                          count
                    )) ) )
               (let ((seq2-type result-type))
                 (with-sequence (seq2 (MAKE))
                   (with-sequence (seq2 (INIT UPD ACCESS-SET)
                                        (funcall seq2-make size))
                     (let ((pointer2 (funcall seq2-init)))
                       (dotimes (count size)
                         (do ((seq-r sequences (cdr seq-r))
                              (seq-desc-r seq-descriptors (cdr seq-desc-r))
                              (arglist nil))
                             ((null seq-r)
                              (funcall seq2-access-set pointer2
                                       (apply fun (list-nreverse arglist))
                              )
                              (setq pointer2 (funcall seq2-upd pointer2))
                             )
                           (let ((seq1 (car seq-r))
                                 (seq1-type (second (car seq-desc-r)))
                                 (seq1-upd (third (car seq-desc-r)))
                                 (seq1-endtest (fourth (car seq-desc-r)))
                                 (seq1-access (fifth (car seq-desc-r)))
                                 (pointer1 (caar (car seq-desc-r))))
                             (push (funcall seq1-access pointer1) arglist)
                             (setf (caar (car seq-desc-r))
                                   (funcall seq1-upd pointer1)
                         ) ) )
                       )
                       seq2
               ) ) ) )
            ))
) ) ) )

; boolescher Operator für Sequences
(defmacro def-seq-boolop (name invokeform resultform)
  `(defun ,name (pred &rest sequences)
     (if (null sequences)
       (error "~S ohne Sequence-Argument aufgerufen." ',name)
       (let ((seq-descriptors
               (mapcar #'(lambda (seq)
                           (with-sequence (seq1 (TYPE INIT UPD ENDTEST ACCESS)
                                                seq)
                             (list (funcall seq1-init)
                                   seq1-type seq1-upd seq1-endtest seq1-access
                         ) ) )
                       sequences
            )) )
         (loop
           (do ((seq-r sequences (cdr seq-r))
                (seq-desc-r seq-descriptors (cdr seq-desc-r))
                (arglist nil))
               ((null seq-r)
                ,(subst '(apply pred (list-nreverse arglist)) '(invoke)
                        invokeform :test #'equal
               ) )
             (let ((seq1 (car seq-r))
                   (seq1-type (second (car seq-desc-r)))
                   (seq1-upd (third (car seq-desc-r)))
                   (seq1-endtest (fourth (car seq-desc-r)))
                   (seq1-access (fifth (car seq-desc-r)))
                   (pointer (first (car seq-desc-r))))
               (if (funcall seq1-endtest pointer)
                 (return-from ,name ,resultform)
               )
               (push (funcall seq1-access pointer) arglist)
               (setf (first (car seq-desc-r))
                     (funcall seq1-upd pointer)
   ) ) ) ) ) ) )
)

(def-seq-boolop some     (let ((h (invoke))) (if h (return-from some h))) nil)
(def-seq-boolop every    (if (null (invoke)) (return-from every nil))     t)
(def-seq-boolop notany   (if (invoke) (return-from notany nil))           t)
(def-seq-boolop notevery (if (null (invoke)) (return-from notevery t))    nil)
#| ; Um Speicher zu sparen:
(defun notany (&rest args) (not (apply #'some args)))
(defun notevery (&rest args) (not (apply #'every args)))
|#

(defun reduce (fun sequence &key (from-end nil) (start 0) (end nil)
                                 (initial-value nil init-given))
  (if from-end
    (with-sequence (seq1 (TYPE LENGTH FE-INIT-END FE-UPD ACCESS) sequence)
      (unless end (setq end (funcall seq1-length)))
      (test-start-end start end)
      (if (= start end)
        ; leere Sequence
        (if init-given initial-value (funcall fun))
        ; nichtleere Sequence
        (let ((pointer (funcall seq1-fe-init-end end)))
          (if init-given
            (do* ((value initial-value
                         (funcall fun (funcall seq1-access pointer) value) )
                  (pointer pointer (funcall seq1-fe-upd pointer))
                  (count (- end start) (1- count)))
                 ((zerop count) value)
            )
            (do ((value (funcall seq1-access pointer)
                        (funcall fun (funcall seq1-access pointer) value) )
                 (count (- end start 1) (1- count)))
                ((zerop count) value)
              (setq pointer (funcall seq1-fe-upd pointer))
    ) ) ) ) )
    (with-sequence (seq1 (TYPE LENGTH INIT-START UPD ACCESS) sequence)
      (unless end (setq end (funcall seq1-length)))
      (test-start-end start end)
      (if (= start end)
        ; leere Sequence
        (if init-given initial-value (funcall fun))
        ; nichtleere Sequence
        (let ((pointer (funcall seq1-init-start start)))
          (if init-given
            (do* ((value initial-value
                         (funcall fun value (funcall seq1-access pointer)) )
                  (pointer pointer (funcall seq1-upd pointer))
                  (count (- end start) (1- count)))
                 ((zerop count) value)
            )
            (do ((value (funcall seq1-access pointer)
                        (funcall fun value (funcall seq1-access pointer)) )
                 (count (- end start 1) (1- count)))
                ((zerop count) value)
              (setq pointer (funcall seq1-upd pointer))
    ) ) ) ) )
) )

(defun fill (sequence item &key (start 0) (end nil))
  (with-sequence (seq1 (TYPE LENGTH INIT-START UPD ACCESS-SET) sequence)
    (unless end (setq end (funcall seq1-length)))
    (test-start-end start end)
    (do ((pointer (funcall seq1-init-start start) (funcall seq1-upd pointer))
         (count (- end start) (1- count)))
        ((zerop count) sequence)
      (funcall seq1-access-set pointer item)
) ) )

(defun replace (sequence1 sequence2
                &key (start1 0) (end1 nil) (start2 0) (end2 nil))
  (with-sequence (seq1 (TYPE LENGTH INIT-START UPD ACCESS-SET) sequence1)
    (with-sequence (seq2 (TYPE LENGTH INIT-START UPD ACCESS) sequence2)
      (unless end1 (setq end1 (funcall seq1-length)))
      (unless end2 (setq end2 (funcall seq2-length)))
      (test-start-end start1 end1)
      (test-start-end start2 end2)
      (let* ((count1 (- end1 start1))
             (count2 (- end2 start2))
             (count (min count1 count2)))
        #|(when (< count count1) (setq end1 (+ start1 count)))|# ; end1 wird nicht mehr benutzt
        (when (< count count2) (setq end2 (+ start2 count)))
        ; Es sind genau count Elemente zu kopieren, und end2=start2+count.
        (when (and (eq sequence1 sequence2) (> end2 start1) (> start1 start2))
          (setq seq2 (setq sequence2 (subseq sequence2 start2 end2)))
          (psetq start2 0 #|end2 count|#) ; end2 wird nicht mehr benutzt
        )
        (do ((pointer1 (funcall seq1-init-start start1)
                       (funcall seq1-upd pointer1))
             (pointer2 (funcall seq2-init-start start2)
                       (funcall seq2-upd pointer2))
             (count count (1- count)))
            ((zerop count) sequence1)
          (funcall seq1-access-set pointer1 (funcall seq2-access pointer2))
) ) ) ) )

(defmacro def-seq-testop
          (name lambdalist &body body)
  (setq lambdalist (append lambdalist '((key #'identity))))
  (let ((name1 name)
        (name2 (intern (lisp:concatenate 'simple-string (symbol-name name) "-IF")
                       (symbol-package name) ))
        (name3 (intern (lisp:concatenate 'simple-string (symbol-name name) "-IF-NOT")
                       (symbol-package name) ))
        (lambdalist1 (append '(item) lambdalist '((test nil) (test-not nil))))
        (lambdalist2 (append '(test) lambdalist))
        (lambdalist3 (append '(test) lambdalist))
        (body1 `(macrolet ((test (arg)
                             `(if test
                                (funcall test item (funcall key ,arg))
                                (not (funcall test-not item (funcall key ,arg)))
                          ))  )
                  (if (and test test-not)
                    (error ":TEST und :TEST-NOT dürfen nicht beide angegeben werden.")
                  )
                  (unless (or test test-not) (setq test #'eql))
                  ,@body
        )       )
        (body2 `(macrolet ((test (arg)
                             `(funcall test (funcall key ,arg))
                          ))
                  ,@body
        )       )
        (body3 `(macrolet ((test (arg)
                             `(not (funcall test (funcall key ,arg)))
                          ))
                  ,@body
        )       )
       )
    (when (or (eq name 'substitute) (eq name 'nsubstitute))
      (push 'newitem lambdalist1)
      (push 'newitem lambdalist2)
      (push 'newitem lambdalist3)
    )
    `(progn
       (defun ,name1 ,lambdalist1 ,body1)
       (defun ,name2 ,lambdalist2 ,body2)
       (defun ,name3 ,lambdalist3 ,body3)
     )
) )

; bildet unter Ausnutzung von SEQ1, SEQ1-TYPE, SEQ1-MAKE, SEQ1-INIT, SEQ1-UPD,
; SEQ1-ACCESS eine neue Sequence zu SEQ1 (hat Länge l), wo die dl Elemente
; fehlen, die im Bitvektor bv (hat Länge l = end - start) mit 1 markiert sind.
(defun remove-help (l dl bv bvl start end)
  (with-sequence (seq2 (TYPE INIT UPD ACCESS-SET) (funcall seq1-make (- l dl)))
    (let ((pointer1 (funcall seq1-init))
          (pointer2 (funcall seq2-init)))
      (dotimes (i start) ; vorderes Teilstück unverändert übernehmen
        (funcall seq2-access-set pointer2 (funcall seq1-access pointer1))
        (setq pointer1 (funcall seq1-upd pointer1))
        (setq pointer2 (funcall seq2-upd pointer2))
      )
      (dotimes (bvi bvl) ; mittleres Teilstück: sieben
        (when (zerop (bit bv bvi))
          (funcall seq2-access-set pointer2 (funcall seq1-access pointer1))
          (setq pointer2 (funcall seq2-upd pointer2))
        )
        (setq pointer1 (funcall seq1-upd pointer1))
      )
      (dotimes (i (- l end)) ; hinteres Teilstück unverändert übernehmen
        (funcall seq2-access-set pointer2 (funcall seq1-access pointer1))
        (setq pointer1 (funcall seq1-upd pointer1))
        (setq pointer2 (funcall seq2-upd pointer2))
      )
    )
    seq2
) )

; bildet unter Ausnutzung von SEQ1, SEQ1-TYPE, SEQ1-MAKE, SEQ1-INIT, SEQ1-UPD,
; SEQ1-ACCESS eine neue Sequence zu SEQ1 (hat Länge l), wo die dl Elemente
; fehlen, die im Bitvektor bv (hat Länge l = end - start) mit 1 markiert sind,
; oder verändert SEQ1 entsprechend (destruktiv).
(defun delete-help (l dl bv bvl start end)
  (if (zerop dl) ; nichts zu entfernen -> direkt zurück
    seq1
    (if (eq seq1-type 'LIST) ; bei Listen ein paar Conses herausnehmen
      (let ((L1 seq1) (L2 nil))
           ; (cdr L2) = L1 oder L2=nil,L1=seq1
        (dotimes (i start) (setq L2 L1 L1 (cdr L1)))
        (dotimes (bvi bvl)
          (if (zerop (bit bv bvi))
            (setq L2 L1 L1 (cdr L1))
            (progn ; Cons bei L1 herausnehmen:
              (setq L1 (cdr L1))
              (if L2 (rplacd L2 L1) (setq seq1 L1))
        ) ) )
        seq1
      )
      (if (and (or (eq seq1-type 'VECTOR) (eq seq1-type 'STRING)
                   (eq seq1-type 'BIT-VECTOR)
               )
               (array-has-fill-pointer-p seq1)
          ) ; bei Vektoren mit Fill-Pointer: Fill-Pointer erniedrigen
        (let ((i start)) ; i = Index beim Neueintrag der Elemente
          (dotimes (bvi bvl)
            (when (zerop (bit bv bvi))
              (setf (aref seq1 i) (aref seq1 (+ start bvi))) (incf i)
          ) )
          (do ((j end))
              ((= j l))
            (setf (aref seq1 i) (aref seq1 j)) (incf i) (incf j)
          )
          (setf (fill-pointer seq1) i)
          seq1
        )
        (remove-help l dl bv bvl start end)
) ) ) )

(defmacro def-seq-testop-remove/delete (name helpfun . helpfunargs)
  `(def-seq-testop ,name (sequence
                          &key (from-end nil) (start 0) (end nil) (count nil))
     (with-sequence (seq1 (TYPE LENGTH INIT UPD ACCESS MAKE) sequence)
       (let ((l (funcall seq1-length))
             (dl 0)) ; zählt die Zahl der zu entfernenden Elemente
         (unless end (setq end l))
         (test-start-end start end)
         (let* ((bvl (- end start))
                (bv (make-array bvl :element-type 'bit :initial-element 0)))
           ; Bit-Vektor bv[i] speichert,
           ; ob Element seq1[start+i] gestrichen werden soll.
           (if from-end
             (with-sequence (seq1 (FE-INIT-END FE-UPD))
               (do ((pointer1 (funcall seq1-fe-init-end end)
                              (funcall seq1-fe-upd pointer1))
                    (bvi (1- bvl) (1- bvi))
                    (countdown count))
                   ((minusp bvi))
                 (when (or (null count) (plusp countdown))
                   (when (test (funcall seq1-access pointer1))
                     (setf (bit bv bvi) 1)
                     (incf dl)
                     (when count (decf countdown))
             ) ) ) )
             (with-sequence (seq1 (INIT-START))
               (do ((pointer1 (funcall seq1-init-start start)
                              (funcall seq1-upd pointer1))
                    (bvi 0 (1+ bvi))
                    (countdown count))
                   ((= bvi bvl))
                 (when (or (null count) (plusp countdown))
                   (when (test (funcall seq1-access pointer1))
                     (setf (bit bv bvi) 1)
                     (incf dl)
                     (when count (decf countdown))
             ) ) ) )
           )
           (,helpfun l dl bv bvl start end ,@helpfunargs)
   ) ) ) )
)

(def-seq-testop-remove/delete remove remove-help)
(def-seq-testop-remove/delete delete delete-help)

(defmacro def-seq-remove/delete-duplicates (name helpfun)
  `(defun ,name (sequence &key (from-end nil) (start 0) (end nil)
                               (test nil) (test-not nil) (key #'identity))
     (macrolet ((test (arg1 arg2)
                  `(if test
                     (funcall test ,arg1 ,arg2)
                     (not (funcall test-not ,arg1 ,arg2))
               ))  )
       (if (and test test-not)
         (error ":TEST und :TEST-NOT dürfen nicht beide angegeben werden.")
       )
       (unless (or test test-not) (setq test #'eql))
       (with-sequence (seq1 (TYPE LENGTH INIT INIT-START UPD ACCESS COPY MAKE)
                            sequence)
         (let ((l (funcall seq1-length))
               (dl 0)) ; zählt die Zahl der zu entfernenden Elemente
           (unless end (setq end l))
           (test-start-end start end)
           (let* ((bvl (- end start))
                  (bv (make-array bvl :element-type 'bit :initial-element 0)))
             ; Bit-Vektor bv[i] speichert,
             ; ob Element seq1[start+i] gestrichen werden soll.
             (if (not (and (>= bvl 10)
                           test
                           (or (eql test #'eq) (eql test #'eql) (eql test #'equal)
                               (eql test 'eq) (eql test 'eql) (eql test 'equal)
                 )    )    )
               ; Standard-Methode
               (if from-end
                 ; pointer1 läuft von links nach rechts,
                 ; pointer2 läuft von pointer1 nach rechts,
                 ; falls der Test zutrifft, wird bei pointer2 gestrichen.
                 (do ((pointer1 (funcall seq1-init-start start)
                                (funcall seq1-upd pointer1))
                      (bvi1 0 (1+ bvi1)))
                     ((= bvi1 bvl))
                   (if (zerop (bit bv bvi1))
                     (let ((item1 (funcall key (funcall seq1-access pointer1))))
                       (do ((pointer2 (funcall seq1-copy pointer1))
                            (bvi2 bvi1))
                           (nil)
                         (incf bvi2)
                         (if (= bvi2 bvl) (return))
                         (setq pointer2 (funcall seq1-upd pointer2))
                         (if (zerop (bit bv bvi2))
                           (when (test item1 (funcall key
                                               (funcall seq1-access pointer2)
                                 )           )
                             (setf (bit bv bvi2) 1) ; Item rechts wird gestrichen
                             (incf dl)
                 ) ) ) ) ) )
                 ; pointer0 steht links,
                 ; pointer2 läuft von links nach rechts,
                 ; pointer1 läuft von links bis pointer2,
                 ; falls der Test zutrifft, wird bei pointer1 gestrichen.
                 (do* ((pointer0 (funcall seq1-init-start start))
                       (pointer2 (funcall seq1-copy pointer0)
                                 (funcall seq1-upd pointer2))
                       (bvi2 0 (1+ bvi2)))
                      ((= bvi2 bvl))
                   (if (zerop (bit bv bvi2))
                     (let ((item2 (funcall key (funcall seq1-access pointer2))))
                       (do ((pointer1 (funcall seq1-copy pointer0)
                                      (funcall seq1-upd pointer1))
                            (bvi1 0 (1+ bvi1)))
                           ((= bvi1 bvi2))
                         (if (zerop (bit bv bvi1))
                           (when (test (funcall key (funcall seq1-access pointer1))
                                       item2
                                 )
                             (setf (bit bv bvi1) 1) ; Item links wird gestrichen
                             (incf dl)
                 ) ) ) ) ) )
               )
               ; Methode mit Hashtabelle
               (let ((ht (make-hash-table :test test))) ; leere Hashtabelle
                 (if from-end
                   ; pointer läuft von links nach rechts,
                   ; item wird in die Tabelle gesteckt; war es schon
                   ; drin, wird bei pointer gestrichen.
                   (do ((pointer (funcall seq1-init-start start)
                                 (funcall seq1-upd pointer))
                        (bvi 0 (1+ bvi)))
                       ((= bvi bvl))
                     (let ((item (funcall key (funcall seq1-access pointer))))
                       (if (gethash item ht)
                         ; item war schon in ht -> wird jetzt gestrichen
                         (progn (setf (bit bv bvi) 1) (incf dl))
                         (setf (gethash item ht) t) ; sonst wird es in ht gesteckt
                   ) ) )
                   ; pointer läuft von links nach rechts,
                   ; item wird in die Tabelle gesteckt; war es schon
                   ; drin, wird an der vorigen Position gestrichen.
                   (do ((pointer (funcall seq1-init-start start)
                                 (funcall seq1-upd pointer))
                        (bvi 0 (1+ bvi)))
                       ((= bvi bvl))
                     (let ((item (funcall key (funcall seq1-access pointer))))
                       (let ((i (gethash item ht)))
                         (when i (setf (bit bv i) 1) (incf dl))
                       )
                       (setf (gethash item ht) bvi)
                   ) )
               ) )
             )
             (,helpfun l dl bv bvl start end)
   ) ) ) ) )
)

(def-seq-remove/delete-duplicates remove-duplicates remove-help)
(def-seq-remove/delete-duplicates delete-duplicates delete-help)

; bildet unter Ausnutzung von SEQ1, SEQ1-TYPE, SEQ1-MAKE, SEQ1-INIT, SEQ1-UPD,
; SEQ1-ACCESS eine neue Sequence zu SEQ1 (hat Länge l), wo die dl Elemente
; durch newitem ersetzt sind, die im Bitvektor bv (hat Länge l = end - start)
; mit 1 markiert sind.
(defun substitute-help (l dl bv bvl start end newitem)
  (if (zerop dl)
    seq1
    (if (eq seq1-type 'LIST)
      (let ((L1 nil) (L2 seq1)) ; (revappend L1 L2) = seq1
        ; erste start Conses kopieren:
        (dotimes (i start) (setq L1 (cons (car L2) L1) L2 (cdr L2)))
        ; bvl bis über die letzte 1 im Bitvektor erniedrigen:
        (do () ((plusp (bit bv (1- bvl)))) (decf bvl))
        ; Teilabschnitt kopieren/mit newitems füllen:
        (dotimes (bvi bvl)
          (setq L1 (cons (if (zerop (bit bv bvi)) (car L2) newitem) L1))
          (setq L2 (cdr L2))
        )
        ; an alte Liste wieder anschließen:
        (nreconc L1 L2)
      )
      (with-sequence (seq2 (TYPE INIT UPD ACCESS-SET) (funcall seq1-make l))
        (let ((pointer1 (funcall seq1-init))
              (pointer2 (funcall seq2-init)))
          (dotimes (i start)
            (funcall seq2-access-set pointer2 (funcall seq1-access pointer1))
            (setq pointer1 (funcall seq1-upd pointer1))
            (setq pointer2 (funcall seq2-upd pointer2))
          )
          (dotimes (bvi bvl)
            (funcall seq2-access-set pointer2
              (if (zerop (bit bv bvi)) (funcall seq1-access pointer1) newitem)
            )
            (setq pointer1 (funcall seq1-upd pointer1))
            (setq pointer2 (funcall seq2-upd pointer2))
          )
          (dotimes (i (- l end))
            (funcall seq2-access-set pointer2 (funcall seq1-access pointer1))
            (setq pointer1 (funcall seq1-upd pointer1))
            (setq pointer2 (funcall seq2-upd pointer2))
          )
        )
        seq2
) ) ) )

(def-seq-testop-remove/delete substitute substitute-help newitem)

(def-seq-testop nsubstitute (sequence
                            &key (from-end nil) (start 0) (end nil) (count nil))
  (if from-end
    (with-sequence (seq1 (TYPE LENGTH FE-INIT-END FE-UPD ACCESS
                          INIT-START UPD ACCESS-SET) sequence)
      (unless end (setq end (funcall seq1-length)))
      (test-start-end start end)
      (let* ((bvl (- end start))
             (bv (make-array bvl :element-type 'bit :initial-element 0)))
        ; Bit-Vektor bv[i] speichert,
        ; ob Element seq1[start+i] ersetzt werden soll.
        (do ((pointer (funcall seq1-fe-init-end end)
                      (funcall seq1-fe-upd pointer))
             (bvi (1- bvl) (1- bvi))
             (countdown count))
            ((minusp bvi))
          (when (or (null count) (plusp countdown))
            (when (test (funcall seq1-access pointer))
              (setf (bit bv bvi) 1)
              (when count (decf countdown))
        ) ) )
        (do ((pointer (funcall seq1-init-start start)
                      (funcall seq1-upd pointer))
             (bvi 0 (1+ bvi)))
            ((= bvi bvl))
          (unless (zerop (bit bv bvi))
            (funcall seq1-access-set pointer newitem)
        ) )
        seq1
    ) )
    (with-sequence (seq1 (TYPE INIT-START UPD ACCESS ACCESS-SET ENDTEST) sequence)
      (do ((pointer (funcall seq1-init-start start) (funcall seq1-upd pointer))
           (i (and end (- end start)) (and end (1- i)))
           (countdown count))
          ((or (and end (zerop i)) (funcall seq1-endtest pointer)))
        (when (or (null count) (plusp countdown))
          (when (test (funcall seq1-access pointer))
            (funcall seq1-access-set pointer newitem)
            (when count (decf countdown))
      ) ) )
      seq1
    )
) )

(def-seq-testop find (sequence &key (from-end nil) (start 0) (end nil))
  (if from-end
    (with-sequence (seq1 (TYPE LENGTH FE-INIT-END FE-UPD ACCESS) sequence)
      (unless end (setq end (funcall seq1-length)))
      (test-start-end start end)
      (do ((pointer (funcall seq1-fe-init-end end) (funcall seq1-fe-upd pointer))
           (count (- end start) (1- count)))
          ((zerop count) nil)
        (let ((h (funcall seq1-access pointer)))
          (if (test h) (return h))
    ) ) )
    (with-sequence (seq1 (TYPE INIT-START UPD ACCESS ENDTEST) sequence)
      (test-start-end-1 start end)
      (do ((pointer (funcall seq1-init-start start) (funcall seq1-upd pointer))
           (count (and end (- end start)) (and end (1- count))))
          ((or (and end (zerop count)) (funcall seq1-endtest pointer)) nil)
        (let ((h (funcall seq1-access pointer)))
          (if (test h) (return h))
    ) ) )
) )

(def-seq-testop position (sequence &key (from-end nil) (start 0) (end nil))
  (if from-end
    (with-sequence (seq1 (TYPE LENGTH FE-INIT-END FE-UPD ACCESS) sequence)
      (unless end (setq end (funcall seq1-length)))
      (test-start-end start end)
      (do ((pointer (funcall seq1-fe-init-end end) (funcall seq1-fe-upd pointer))
           (index (1- end) (1- index))
           (count (- end start) (1- count)))
          ((zerop count) nil)
        (if (test (funcall seq1-access pointer)) (return index))
    ) )
    (with-sequence (seq1 (TYPE INIT-START UPD ACCESS ENDTEST) sequence)
      (test-start-end-1 start end)
      (do ((pointer (funcall seq1-init-start start) (funcall seq1-upd pointer))
           (index start (1+ index))
           (count (and end (- end start)) (and end (1- count))))
          ((or (and end (zerop count)) (funcall seq1-endtest pointer)) nil)
        (if (test (funcall seq1-access pointer)) (return index))
    ) )
) )

(def-seq-testop count (sequence &key (from-end nil) (start 0) (end nil))
  (if from-end
    (with-sequence (seq1 (TYPE LENGTH FE-INIT-END FE-UPD ACCESS) sequence)
      (unless end (setq end (funcall seq1-length)))
      (test-start-end start end)
      (do ((pointer (funcall seq1-fe-init-end end)
                    (funcall seq1-fe-upd pointer))
           (i (- end start) (1- i))
           (count 0))
          ((zerop i) count)
        (when (test (funcall seq1-access pointer)) (incf count))
    ) )
    (with-sequence (seq1 (TYPE INIT-START UPD ACCESS ENDTEST) sequence)
      (do ((pointer (funcall seq1-init-start start) (funcall seq1-upd pointer))
           (i (and end (- end start)) (and end (1- i)))
           (count 0))
          ((or (and end (zerop i)) (funcall seq1-endtest pointer)) count)
        (when (test (funcall seq1-access pointer)) (incf count))
    ) )
) )

(defun mismatch (sequence1 sequence2
                 &key (from-end nil) (test nil) (test-not nil) (key #'identity)
                      (start1 0) (end1 nil) (start2 0) (end2 nil) )
  (macrolet ((test (arg1 arg2)
               `(if test
                  (funcall test ,arg1 ,arg2)
                  (not (funcall test-not ,arg1 ,arg2))
            ))  )
    (if (and test test-not)
      (error ":TEST und :TEST-NOT dürfen nicht beide angegeben werden.")
    )
    (unless (or test test-not) (setq test #'eql))
    (if from-end
      (with-sequence (seq1 (TYPE LENGTH FE-INIT-END FE-UPD FE-ENDTEST ACCESS)
                           sequence1)
        (with-sequence (seq2 (TYPE LENGTH FE-INIT-END FE-UPD FE-ENDTEST ACCESS)
                             sequence2)
          (unless end1 (setq end1 (funcall seq1-length)))
          (unless end2 (setq end2 (funcall seq2-length)))
          (test-start-end start1 end1)
          (test-start-end start2 end2)
          (do ((pointer1 (funcall seq1-fe-init-end end1)
                         (funcall seq1-fe-upd pointer1))
               (pointer2 (funcall seq2-fe-init-end end2)
                         (funcall seq2-fe-upd pointer2))
               (index end1 (1- index))
               (count (min (- end1 start1) (- end2 start2)) (1- count)))
              ((zerop count)
               (if (= (- end1 start1) (- end2 start2)) nil index)
              )
            (let ((value1 (funcall key (funcall seq1-access pointer1)))
                  (value2 (funcall key (funcall seq2-access pointer2))))
              (unless (test value1 value2) (return index))
      ) ) ) )
      (with-sequence (seq1 (TYPE INIT-START UPD ENDTEST ACCESS) sequence1)
        (with-sequence (seq2 (TYPE INIT-START UPD ENDTEST ACCESS) sequence2)
          (do ((pointer1 (funcall seq1-init-start start1)
                         (funcall seq1-upd pointer1))
               (pointer2 (funcall seq2-init-start start2)
                         (funcall seq2-upd pointer2))
               (index start1 (1+ index))
               (count1 (and end1 (- end1 start1)) (and end1 (1- count1)))
               (count2 (and end2 (- end2 start2)) (and end2 (1- count2))))
              (nil)
            (let ((1-fertig (or (and end1 (zerop count1))
                                (funcall seq1-endtest pointer1)
                  )         )
                  (2-fertig (or (and end2 (zerop count2))
                                (funcall seq2-endtest pointer2)
                 ))         )
              (if (or 1-fertig 2-fertig)
                (return (if (and 1-fertig 2-fertig) nil index))
            ) )
            (let ((value1 (funcall key (funcall seq1-access pointer1)))
                  (value2 (funcall key (funcall seq2-access pointer2))))
              (unless (test value1 value2) (return index))
      ) ) ) )
) ) )

#| ; Primitiv-Algorithmus:
; Rücke immer in sequence2 um 1 weiter und teste, ob dann sequence1 kommt.
(defun search (sequence1 sequence2
               &key (from-end nil) (test nil) (test-not nil) (key #'identity)
                    (start1 0) (end1 nil) (start2 0) (end2 nil) )
  (macrolet ((test (arg1 arg2)
               `(if test
                  (funcall test ,arg1 ,arg2)
                  (not (funcall test-not ,arg1 ,arg2))
            ))  )
    (if (and test test-not)
      (error ":TEST und :TEST-NOT dürfen nicht beide angegeben werden.")
    )
    (unless (or test test-not) (setq test #'eql))
    (if from-end
      (with-sequence (seq1 (TYPE LENGTH FE-INIT-END FE-UPD FE-ENDTEST ACCESS COPY)
                           sequence1)
        (with-sequence (seq2 (TYPE LENGTH FE-INIT-END FE-UPD FE-ENDTEST ACCESS COPY)
                             sequence2)
          (unless end1 (setq end1 (funcall seq1-length)))
          (unless end2 (setq end2 (funcall seq2-length)))
          (test-start-end start1 end1)
          (test-start-end start2 end2)
          (do* ((pointer10 (funcall seq1-fe-init-end end1))
                (pointer20 (funcall seq2-fe-init-end end2)
                           (funcall seq2-fe-upd pointer20))
                (count10 (- end1 start1))
                (count20 (- end2 start2) (1- count20))
                (index (- end2 count10) (1- index)))
               (nil)
            ; Kommt vor pointer20 die Sequence vor pointer10 ?
            (do ((pointer1 (funcall seq1-copy pointer10)
                           (funcall seq1-fe-upd pointer1))
                 (pointer2 (funcall seq2-copy pointer20)
                           (funcall seq2-fe-upd pointer2))
                 (count1 count10 (1- count1))
                 (count2 count20 (1- count2)))
                (nil)
              (when (zerop count1) ; Sequence1 fertig ?
                (return-from search index) ; gefunden!
              )
              (when (zerop count2) ; Sequence2 fertig ?
                (return-from search nil) ; sequence1 ab jetzt zu lang
              )
              (let ((value1 (funcall key (funcall seq1-access pointer1)))
                    (value2 (funcall key (funcall seq2-access pointer2))))
                (unless (test value1 value2) (return)) ; Vergleich abbrechen
      ) ) ) ) )
      (with-sequence (seq1 (TYPE INIT-START UPD ENDTEST ACCESS COPY) sequence1)
        (with-sequence (seq2 (TYPE INIT-START UPD ENDTEST ACCESS COPY) sequence2)
          (do ((pointer10 (funcall seq1-init-start start1))
               (pointer20 (funcall seq2-init-start start2)
                          (funcall seq2-upd pointer20))
               (count20 (and end2 (- end2 start2)) (and end2 (1- count20)))
               (index start2 (1+ index)))
              (nil)
            ; Kommt ab pointer20 die Sequence ab pointer10 ?
            (do ((pointer1 (funcall seq1-copy pointer10)
                           (funcall seq1-upd pointer1))
                 (pointer2 (funcall seq2-copy pointer20)
                           (funcall seq2-upd pointer2))
                 (count1 (and end1 (- end1 start1)) (and end1 (1- count1)))
                 (count2 count20 (and end2 (1- count2))))
                (nil)
              (when ; Sequence1 fertig ?
                (or (and end1 (zerop count1)) (funcall seq1-endtest pointer1))
                (return-from search index) ; gefunden!
              )
              (when ; Sequence2 fertig ?
                (or (and end2 (zerop count2)) (funcall seq2-endtest pointer2))
                (return-from search nil) ; sequence1 ab jetzt zu lang
              )
              (let ((value1 (funcall key (funcall seq1-access pointer1)))
                    (value2 (funcall key (funcall seq2-access pointer2))))
                (unless (test value1 value2) (return)) ; Vergleich abbrechen
      ) ) ) ) )
) ) )
|#
#| ; Knuth's Algorithmus:
Formuliert für die Suche von S[0]...S[m-1] in T[0]...T[n-1] von vorne:
1. if m=0 then (gefunden bei Index 0, return).
2. i:=0, l:=0. Benötige einen Array l[1]...l[m-1].
3.   i:=i+1.
4.   if i=m then goto 10.
5.   l[i]:=l.
6.   if S[i]=S[l] then (l:=l+1, goto 3).
7.   if l=0 then goto 3.
8.   l:=l[l].
9.   goto 6.
10. j:=0.
11.  if j=n then (nicht gefunden, return).
12.  if not T[j]=S[0] then (j:=j+1, goto 11).
13.  i:=0.
14.   i:=i+1, if i=m then (gefunden bei Index j-i, return).
15.   j:=j+1, if j=n then (nicht gefunden, return).
16.   if S[i]=T[j] then goto 14.
17.   i:=l[i].
18.   if i=0 then goto 12 else goto 16.
|#
(defun search (sequence1 sequence2
               &key (from-end nil) (test nil) (test-not nil) (key #'identity)
                    (start1 0) (end1 nil) (start2 0) (end2 nil) )
  (macrolet ((test (arg1 arg2)
               `(if test
                  (funcall test ,arg1 ,arg2)
                  (not (funcall test-not ,arg1 ,arg2))
            ))  )
    (if (and test test-not)
      (error ":TEST und :TEST-NOT dürfen nicht beide angegeben werden.")
    )
    (unless (or test test-not) (setq test #'eql))
    (with-sequence (seq1 (TYPE LENGTH ACCESS COPY) sequence1)
      (with-sequence (seq2 (TYPE LENGTH ACCESS COPY) sequence2)
        (unless end1 (setq end1 (funcall seq1-length)))
        (unless end2 (setq end2 (funcall seq2-length)))
        (test-start-end start1 end1)
        (test-start-end start2 end2)
        (let ((m (- end1 start1))
              (n (- end2 start2)))
          (if from-end
            (if (zerop m) end2
              (with-sequence (seq1 (FE-INIT-END FE-UPD))
                (with-sequence (seq2 (FE-INIT-END FE-UPD))
                  (let ((lv (make-array m)) (Slv (make-array m))
                        (S0 (funcall seq1-fe-init-end end1)))
                    (prog ((i 0) (Si (funcall seq1-copy S0))
                           (l 0) (Sl (funcall seq1-copy S0)))
                      3
                      (incf i) (setq Si (funcall seq1-fe-upd Si))
                      (if (= i m) (return))
                      (setf (svref lv i) l)
                      (setf (svref Slv i) (funcall seq1-copy Sl))
                      6
                      (when (test (funcall key (funcall seq1-access Si))
                                  (funcall key (funcall seq1-access Sl))
                            )
                        (incf l) (setq Sl (funcall seq1-fe-upd Sl))
                        (go 3)
                      )
                      (if (zerop l) (go 3))
                      (psetq l (svref lv l) Sl (svref Slv l))
                      (go 6)
                    )
                    (prog ((i 0) (Si (funcall seq1-copy S0))
                           (j 0) (Tj (funcall seq2-fe-init-end end2)))
                      11
                      (if (= j n) (return nil))
                      12
                      (unless (test (funcall key (funcall seq1-access Si))
                                    (funcall key (funcall seq2-access Tj))
                              )
                        (incf j) (setq Tj (funcall seq2-fe-upd Tj))
                        (go 11)
                      )
                      14
                      (incf i) (setq Si (funcall seq1-fe-upd Si))
                      (incf j) (setq Tj (funcall seq2-fe-upd Tj))
                      (if (= i m) (return (- end2 j)))
                      (if (= j n) (return nil))
                      16
                      (if (test (funcall key (funcall seq1-access Si))
                                (funcall key (funcall seq2-access Tj))
                          )
                        (go 14)
                      )
                      (psetq i (svref lv i) Si (svref Slv i))
                      (if (zerop i) (go 12) (go 16))
            ) ) ) ) )
            (if (zerop m) start2
              (with-sequence (seq1 (INIT-START UPD))
                (with-sequence (seq2 (INIT-START UPD))
                  (let ((lv (make-array m)) (Slv (make-array m))
                        (S0 (funcall seq1-init-start start1)))
                    (prog ((i 0) (Si (funcall seq1-copy S0))
                           (l 0) (Sl (funcall seq1-copy S0)))
                      3
                      (incf i) (setq Si (funcall seq1-upd Si))
                      (if (= i m) (return))
                      (setf (svref lv i) l)
                      (setf (svref Slv i) (funcall seq1-copy Sl))
                      6
                      (when (test (funcall key (funcall seq1-access Si))
                                  (funcall key (funcall seq1-access Sl))
                            )
                        (incf l) (setq Sl (funcall seq1-upd Sl))
                        (go 3)
                      )
                      (if (zerop l) (go 3))
                      (psetq l (svref lv l) Sl (svref Slv l))
                      (go 6)
                    )
                    (prog ((i 0) (Si (funcall seq1-copy S0))
                           (j 0) (Tj (funcall seq2-init-start start2)))
                      11
                      (if (= j n) (return nil))
                      12
                      (unless (test (funcall key (funcall seq1-access Si))
                                    (funcall key (funcall seq2-access Tj))
                              )
                        (incf j) (setq Tj (funcall seq2-upd Tj))
                        (go 11)
                      )
                      14
                      (incf i) (setq Si (funcall seq1-upd Si))
                      (incf j) (setq Tj (funcall seq2-upd Tj))
                      (if (= i m) (return (+ start2 (- j i))))
                      (if (= j n) (return nil))
                      16
                      (if (test (funcall key (funcall seq1-access Si))
                                (funcall key (funcall seq2-access Tj))
                          )
                        (go 14)
                      )
                      (psetq i (svref lv i) Si (svref Slv i))
                      (if (zerop i) (go 12) (go 16))
            ) ) ) ) )
          )
) ) ) ) )

(defun sort (sequence predicate &key (key #'identity) (start 0) (end nil))
  (stable-sort sequence predicate :key key :start start :end end)
)

(defun stable-sort (sequence predicate &key (key #'identity) (start 0) (end nil))
  (with-sequence (seq1 (TYPE LENGTH INIT-START UPD ACCESS ACCESS-SET COPY MAKE)
                       sequence)
    (unless end (setq end (funcall seq1-length)))
    (test-start-end start end)
    (let ((l (- end start)))
      (with-sequence (seq2 (TYPE INIT UPD ACCESS ACCESS-SET)
                           (funcall seq1-make (floor l 2))
                     ) ; Hilfssequence während des Mergens
        (labels
          ((sort-part (pointer-left k)
             ; sortiert in sequence1 ab pointer-left genau k Elemente, k>=1,
             ; und liefert einen Pointer nach diesen k Elementen.
             ; pointer-left wird destruktiv verändert.
             (if (= k 1)
               (funcall seq1-upd pointer-left)
               (let* ((kl (floor k 2)) ; linker Teil
                      (kr (ceiling k 2)) ; rechter Teil
                      (pointer-mid ; linken Teil sortieren:
                        (sort-part (funcall seq1-copy pointer-left) kl)
                      )
                      (pointer-right ; rechten Teil sortieren:
                        (sort-part (funcall seq1-copy pointer-mid) kr)
                     ))
                 ; linken Teil nach seq2 kopieren:
                 (do ((pointer1 (funcall seq1-copy pointer-left)
                                (funcall seq1-upd pointer1))
                      (pointer2 (funcall seq2-init) (funcall seq2-upd pointer2))
                      (k kl (1- k)))
                     ((zerop k))
                   (funcall seq2-access-set pointer2 (funcall seq1-access pointer1))
                 )
                 ; beide Teile zusammenmergen (von seq2 nach seq1):
                 (do ((pointer0 pointer-left)
                      (pointer1 pointer-mid) (i1 kr)
                      (pointer2 (funcall seq2-init)) (i2 kl)
                     )
                     ((zerop i2))
                   (when (zerop i1)
                     (loop
                       (funcall seq1-access-set pointer0
                                (funcall seq2-access pointer2)
                       )
                       (setq pointer0 (funcall seq1-upd pointer0))
                       (setq pointer2 (funcall seq2-upd pointer2))
                       (decf i2)
                       (when (zerop i2) (return))
                     )
                     (return)
                   )
                   (if (funcall predicate
                         (funcall key (funcall seq1-access pointer1))
                         (funcall key (funcall seq2-access pointer2))
                       )
                     (progn
                       (funcall seq1-access-set pointer0
                                (funcall seq1-access pointer1)
                       )
                       (setq pointer0 (funcall seq1-upd pointer0))
                       (setq pointer1 (funcall seq1-upd pointer1))
                       (decf i1)
                     )
                     (progn
                       (funcall seq1-access-set pointer0
                                (funcall seq2-access pointer2)
                       )
                       (setq pointer0 (funcall seq1-upd pointer0))
                       (setq pointer2 (funcall seq2-upd pointer2))
                       (decf i2)
                 ) ) )
                 pointer-right
          )) ) )
          (if (>= l 1) (sort-part (funcall seq1-init-start start) l))
          sequence
) ) ) ) )

(defun merge (result-type sequence1 sequence2 predicate &key (key #'identity))
  (setq result-type (valid-sequence result-type))
  (with-sequence (seq1 (TYPE LENGTH INIT UPD ACCESS) sequence1)
    (with-sequence (seq2 (TYPE LENGTH INIT UPD ACCESS) sequence2)
      (let* ((l1 (funcall seq1-length))
             (l2 (funcall seq2-length))
             (l (+ l1 l2))
             (type (if (and (eq seq1-type seq2-type)
                            (or (eq seq1-type 'bit-vector) (eq seq1-type 'string))
                       )
                       seq1-type
                       'vector
             )     )
             (result (make-sequence type l)))
        (do ((pointer1 (funcall seq1-init)) (i1 l1)
             (pointer2 (funcall seq2-init)) (i2 l2)
             (i 0 (1+ i))
            )
            (nil)
          ; i1 >=0, i2 >=0.
          (when (zerop i1)
            (loop
              (when (zerop i2) (return))
              (setf (aref result i) (funcall seq2-access pointer2))
              (setq pointer2 (funcall seq2-upd pointer2))
              (incf i) (decf i2)
            )
            (return)
          )
          ; i1 >0, i2>=0.
          (when (zerop i2)
            (loop
              (setf (aref result i) (funcall seq1-access pointer1))
              (setq pointer1 (funcall seq1-upd pointer1))
              (incf i) (decf i1)
              (when (zerop i1) (return))
            )
            (return)
          )
          ; i1 >0, i2 >0.
          (if (funcall predicate
                (funcall key (funcall seq2-access pointer2))
                (funcall key (funcall seq1-access pointer1))
              )
            ; Element aus Sequence2 übernehmen:
            (progn
              (setf (aref result i) (funcall seq2-access pointer2))
              (setq pointer2 (funcall seq2-upd pointer2))
              (decf i2)
            )
            ; Element aus Sequence1 übernehmen:
            (progn
              (setf (aref result i) (funcall seq1-access pointer1))
              (setq pointer1 (funcall seq1-upd pointer1))
              (decf i1)
            )
        ) )
        ; der Vektor result ist fertig.
        (if (eq type result-type)
          result
          (coerce result result-type)
        )
) ) ) )

; (do-sequence (var sequenceform [resultform]) {declaration}* {tag|statement}*)
#+CLISP ; benutzt sys::parse-body
(defmacro do-sequence ((var sequenceform &optional (resultform nil))
                       &body body &environment env)
  (multiple-value-bind (body-rest declarations)
      (system::parse-body body nil env)
    (setq declarations (if declarations `((DECLARE ,declarations)) '()))
    (let ((seqvar (gensym)) (pointer (gensym)))
      `(BLOCK NIL
         (LET ((,seqvar ,sequenceform))
           (LET ((,var NIL))
             ,@declarations
             (WITH-SEQUENCE (SEQ1 (TYPE INIT UPD ENDTEST ACCESS) ,seqvar)
               (DO ((,pointer (FUNCALL SEQ1-INIT) (FUNCALL SEQ1-UPD ,pointer)))
                   ((FUNCALL SEQ1-ENDTEST ,pointer))
                 (LET ((,var (FUNCALL SEQ1-ACCESS ,pointer)))
                   ,@declarations
                   (TAGBODY ,@body-rest)
             ) ) )
             ,resultform
       ) ) )
) ) )

