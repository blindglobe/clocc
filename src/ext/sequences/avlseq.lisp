; AVL-Bäume, implementiert in COMMON LISP, als Sequences
; Bruno Haible, 22.09.1988
; CLISP-Version 29.12.1988
; Version mit vielen Deklarationen,
;             Buchführung über die Länge jedes Teilbaums,
;             Definition eines Sequence-Untertyps AVL-TREE.

; Ein AVL-Baum ist ein Binärbaum, bei dem in jedem Knoten ein Datum
; (value) sitzt. Der Baum ist stets balanciert, in der Weise, daß die Höhen
; zweier linker und rechter Teilbäume sich um höchstens 1 unterscheiden.
; Die Ordnungsrelation auf den values ist durch eine Vergleichsfunktion comp
; festgelegt, die feststellt, wann x<y ist (eine fest gewählte
; Ordnungsrelation). Bei (not (or (comp x y) (comp y x))) gelten x und y als
; gleich.
; Der Baum kann wie eine Sequence von links nach rechts oder von rechts nach
; links durchlaufen werden, alle Sequence-Funktionen operieren somit auch auf
; AVL-Bäumen.

(provide 'avlseq)
(in-package 'avl)
(shadow '(length member delete copy merge))
(export '(treep member insert delete do-avl avl-to-seq seq-to-avl copy merge))
(require 'sequences)
(import '(seq::avl-tree seq::seq))

; Datenstruktur eines Baumes: leerer Baum=nil, sonst Knoten ("node")

(deftype tree ()
  '(or null node))

(defstruct node
  (level 0 :type fixnum)
  (left nil :type tree)
  (right nil :type tree)
  (value nil)
  (length 1 :type (integer 1 *))
)
(proclaim '(inline node-level node-left node-right node-value node-length))

; (level tree) ergibt die Höhe eines Baumes
(proclaim '(function level (node) fixnum))
(proclaim '(inline level))
(defun level (tr)
  (if tr (locally (declare (type node tr)) (node-level tr)) 0)
)

; (length tree) ergibt die Breite (= Anzahl der Nodes) eines Baumes
(proclaim '(function length (tree) integer))
(proclaim '(inline length))
(defun length (tr)
  (if tr (locally (declare (type node tr)) (node-length tr)) 0)
)

; (recalc-length node) berechnet (node-length node) neu
(proclaim '(function recalc-length (node) integer))
(proclaim '(inline recalc-length))
(defun recalc-length (tr)
  (declare (type node tr))
  (setf (node-length tr) (+ (length (node-left tr)) 1 (length (node-right tr))))
)

; (deftype avl-tree (comp) ...) funktioniert nicht.

; (treep tr comp) stellt fest, ob ein AVL-Baum vorliegt.
(proclaim '(function treep (tree function &optional t) symbol))
(defun treep (tr comp &optional (el-type t))
  (or (null tr)
      (and
        (typep tr 'node)
        (locally (declare (type node tr))
          (and
            (typep (node-value tr) el-type)
            (let ((trl (node-left tr))
                  (trr (node-right tr)))
              (declare (type tree trl trr) (type node tr))
              (and (= (level tr)
                      (1+ (the fixnum
                            (max (the fixnum (level trl))
                                 (the fixnum (level trr))
                   )  )   ) )
                   (<= (the fixnum
                         (abs (the fixnum
                                (- (the fixnum (level trl))
                                   (the fixnum (level trr))
                       ) )    ) )
                       1
                   )
                   (or (null trl)
                       (locally (declare (type node tr trl))
                         (funcall comp (node-value trl) (node-value tr))
                   )   )
                   (or (null trr)
                       (locally (declare (type node tr trr))
                         (funcall comp (node-value tr) (node-value trr))
                   )   )
                   (treep trl comp el-type)
                   (treep trr comp el-type)
) )   ) ) ) ) )


; (ganzrechts tr) liefert das "größte" Element eines nichtleeren Baumes
(proclaim '(function ganzrechts (node) node))
(defun ganzrechts (tr)
  (declare (type node tr))
  (if (node-right tr) (ganzrechts (node-right tr)) (node-value tr)))

(proclaim '(function ganzlinks (node) node))
; (ganzlinks tr) liefert das "kleinste" Element eines nichtleeren Baumes
(defun ganzlinks (tr)
  (declare (type node tr))
  (if (node-left tr) (ganzlinks (node-left tr)) (node-value tr)))


; (member item tree comp) testet, ob item ein Element des Baumes tree ist.
; Durch Angabe eines Gleichheitstests eq-test kann geprüft werden, ob die
; beiden Werte (item und der Wert im Baum) in einem engeren Sinne gleich sind.
; Trick: Falls man im Baum keine values mit dem Wert NIL abspeichert, kann man
; sich durch eq-test = #'(lambda (it val) (and ("=" it val) val)) den im Baum
; stehenden Wert val zurückgeben lassen.
(proclaim '(function member (t tree function &optional function) t))
(defun member (item tr comp &optional (eq-test #'equal))
  (if (null tr) nil
    (locally (declare (type node tr))
      (cond ((funcall eq-test item (node-value tr)))
            ((funcall comp item (node-value tr))
             (member item (node-left tr) comp eq-test))
            ((funcall comp (node-value tr) item)
             (member item (node-right tr) comp eq-test))
) ) ) ) ; sonst NIL


; (balance tree) balanciert einen nichtleeren Baum tree aus. Voraussetzung
; ist, daß höchstens ein Element den Baum aus der Balance gebracht hat.
; tree selbst wird verändert!
(proclaim '(function balance (node) node))
(defun balance (b)
  (let ((l (level (node-left b)))
        (r (level (node-right b))))
    (declare (fixnum l r) (type node b c d))
    (setf (node-level b) (the fixnum (1+ (the fixnum (max l r)))))
    (case (the fixnum (- r l))
      ((-2)(let ((c (node-left b))
                 (d nil))
             (cond ((< (the fixnum (level (node-left c)))
                       (the fixnum (level (node-right c))))
                    (setq d (node-right c))
                    (setf (node-right c) (node-left d))
                    (setf (node-left b) (node-right d))
                    (setf (node-left d) c)
                    (setf (node-right d) b)
                    (setf (node-level b) (node-level d))
                    (setf (node-level d) (node-level c))
                    (setf (node-level c) (node-level b))
                    (recalc-length b)
                    (recalc-length c)
                    (recalc-length d)
                    d
                    )
                    (t
                      (setf (node-left b) (node-right c))
                      (setf (node-right c) b)
                      (setf (node-level b)
                        (the fixnum (1+ (the fixnum (level (node-left b))))))
                      (setf (node-level c)
                        (the fixnum (1+ (the fixnum (node-level b)))))
                      (recalc-length b)
                      (recalc-length c)
                      c
      )    ) )     )
      ((2) (let ((c (node-right b))
                 (d nil))
             (cond ((< (the fixnum (level (node-right c)))
                       (the fixnum (level (node-left c))))
                    (setq d (node-left c))
                    (setf (node-left c) (node-right d))
                    (setf (node-right b) (node-left d))
                    (setf (node-right d) c)
                    (setf (node-left d) b)
                    (setf (node-level b) (node-level d))
                    (setf (node-level d) (node-level c))
                    (setf (node-level c) (node-level b))
                    (recalc-length b)
                    (recalc-length c)
                    (recalc-length d)
                    d
                   )
                   (t
                      (setf (node-right b) (node-left c))
                      (setf (node-left c) b)
                      (setf (node-level b)
                        (the fixnum (1+ (the fixnum (level (node-right b))))))
                      (setf (node-level c)
                        (the fixnum (1+ (the fixnum (node-level b)))))
                      (recalc-length b)
                      (recalc-length c)
                      c
      )    ) )     )
      ((-1 0 1) (recalc-length b) b)
) ) )


; (insert item tree comp) fügt item zusätzlich in tree ein.
; Das Ergebnis ist ebenfalls ein AVL-Baum. Falls item bereits vorkommt,
; wird item an dessen Stelle eingesetzt.
; Durch Angabe eines Gleichheitstest eq-test kann angegeben werden, was
; für Elemente als gleich zu gelten haben. (Das muß diejenigen Elemente
; umfassen, die nicht vergleichbar sind: stets x<y oder y<x oder (eq-test x y).)
; tree selbst wird verändert!
(proclaim '(function insert (t tree function &optional function) node))
(defun insert (item tr comp &optional (eq-test #'equal))
  (if (null tr) (make-node :level 1 :value item)
    (locally (declare (type node tr))
      (cond
        ((funcall eq-test item (node-value tr))
         (setf (node-value tr) item)
         tr)
        (t
           (cond
             ((funcall comp item (node-value tr))
              (setf (node-left tr) (insert item (node-left tr) comp eq-test)))
             ((funcall comp (node-value tr) item)
              (setf (node-right tr) (insert item (node-right tr) comp eq-test)))
             (t (error "Element paßt nicht in AVL-Baum-Ordnung!"))
           )
           (balance tr)
) ) ) ) )


; (delete item tree comp) entfernt item aus tree und liefert das
; verkleinerte tree zurück.
(proclaim '(function delete (t tree function &optional function) tree))
(defun delete (item tr comp &optional (eq-test #'equal))
  (if (null tr) tr
    (locally (declare (type node tr))
      (cond
        ((funcall eq-test item (node-value tr))
         (let ((r (node-right tr)))
           (declare (type node tr))
           (if (null r)
               (node-left tr)
               (multiple-value-bind (rest del) (delete-ganzlinks r)
                    (declare (type node del))
                    (setf (node-left del) (node-left tr))
                    (setf (node-right del) rest)
                    (balance del)
        )) )   )
        ((funcall comp item (node-value tr))
         (setf (node-left tr) (delete item (node-left tr) comp eq-test))
         (balance tr))
        ((funcall comp (node-value tr) item)
         (setf (node-right tr) (delete item (node-right tr) comp eq-test))
         (balance tr))
        (t (error "Element paßt nicht in AVL-Baum-Ordnung!"))
) ) ) )

; (delete-ganzlinks tree) entfernt aus dem nichtleeren tree das "kleinste"
; Element und gibt den Restbaum zurück. Das entfernte Element erscheint als
; zweiter Wert (als Knoten, zur Vermeidung von Garbage Produktion).
(proclaim '(function delete-ganzlinks (node) tree))
(defun delete-ganzlinks (tr)
  (declare (type node tr))
  (if (null (node-left tr))
      (values (node-right tr) tr)
      (multiple-value-bind (tl el) (delete-ganzlinks (node-left tr))
        (setf (node-left tr) tl)
        (decf (node-length tr))
        (values tr el)
) )   )


; (do-avl (var treeform [resultform]) {declaration}* {tag|statement}* )
; ist ein Macro wie dolist: Für alle var aus dem AVL-Baum, der bei
; treeform herauskommt, wird der Rest ausgeführt.
(defmacro do-avl (varform &rest body)
  `(progn
     (traverse ,(second varform)
               #'(lambda (,(first varform)) ,@body)
     )
     ,(if (third varform)
        `(let ((,(first varform) nil)) ,(first varform) ,(third varform))
        'nil
      )
)  )

(defmacro do-avl-1 ((var treeform &optional resultform) &body body)
  (let ((abstieg (gensym)) ; Labels
        (aufstieg (gensym))
        (ende (gensym))
        (stack (gensym)) ; (cons ,top ,stack) ist ein "Stack"
        (top (gensym)))
    `(prog ((,stack nil) (,top ,treeform))
        ,abstieg
        (if (null ,top) (go ,aufstieg))
        (push ,top ,stack) (setq ,top (node-left (the node ,top)))
        (go ,abstieg)
        ,aufstieg
        (if (null ,stack) (go ,ende))
        (if (eq ,top (node-right (the node (setq ,top (pop ,stack)))))
            (go ,aufstieg))
        (let ((,var (node-value (the node ,top)))) ,@body)
        (push ,top ,stack) (setq ,top (node-right (the node ,top)))
        (go ,aufstieg)
        ,ende
        (let ((,var nil)) (return ,resultform))
     )
) )

(proclaim '(function traverse (tree (function (t) t)) null))
(defun traverse (tr fun)
  (if (null tr) nil
      (locally (declare (type node tr))
        (traverse (node-left tr) fun)
        (funcall fun (node-value tr))
        (traverse (node-right tr) fun)
) )   )


; (avl-to-seq tree) ergibt eine sortierte Liste aller values des Baumes tree.
; (avl-to-seq tree seq-type) ergibt eine sortierte Sequence des angegebenen
; Typs aus allen Werten des Baumes tree.
(proclaim '(function avl-to-seq (tree &optional t) sequence))
(defun avl-to-seq (tr &optional (result-type 'list))
  (if (null tr)
      (make-sequence result-type 0)
      (locally (declare (type node tr))
        (concatenate result-type
          (avl-to-seq (node-left tr))
          (make-sequence result-type 1 :initial-element (node-value tr))
          (avl-to-seq (node-right tr))
) )   ) )

; (seq-to-avl l comp) ergibt aus einer (unsortierten) sequence l von Elementen
; einen AVL-Baum.
(proclaim '(function seq-to-avl (sequence function &optional function) tree))
(defun seq-to-avl (l comp &optional (eq-test #'equal))
  (reduce #'(lambda (tr item) (insert item tr comp eq-test))
          l :initial-value nil
) )


; (copy tree) ergibt eine Kopie des AVL-Baumes tree.
; Nur die Baumstruktur wird kopiert, die Werte werden übernommen.
; insert und delete sind jetzt auf dem Original und auf der Kopie unabhängig
; voneinander durchführbar.
(proclaim '(function copy (tree) tree))
(defun copy (tr)
  (if (null tr) nil
      (locally (declare (type node tr))
        (make-node :level (node-level tr)
                   :left (copy (node-left tr))
                   :right (copy (node-right tr))
                   :value (node-value tr)
) )   ) )


; (merge tree1 tree2 comp) ergibt einen neuen AVL-Baum, der aus den Elementen
; der Bäume tree1 und tree2 besteht.
; Durch Angabe eines Gleichheitstests kann spezifiert werden, was für
; Elemente (weil gleich) nicht doppelt in den neuen AVL-Baum übernommen zu
; werden brauchen. (Je zwei nicht vergleichbare Elemente müssen in diesem
; Sinne gleich sein.)
(proclaim '(function merge (tree tree function &optional function) tree))
(defun merge (tr1 tr2 comp &optional (eq-test #'equal))
  (if (< (the fixnum (level tr1)) (the fixnum (level tr2))) (rotatef tr1 tr2))
  ; jetzt ist tr1 der größere der Bäume
  (let ((tr (copy tr1)))
    (do-avl (x tr2 tr) (setq tr (insert x tr comp eq-test)))
) )

; AVL-Bäume als Sequences:

; Ausgabefunktion:
(defun print-avl-tree (seq stream depth)
  (declare (ignore depth))
  (format stream #+VAX "~@!#S(~;~S ~:_~S ~:_~S ~:_~S ~:_~S~;)~."
                 #-VAX "#S(~S ~S ~S ~S ~S)"
    'avl-tree ':contents (seq:coerce seq 'list) ':length (seq:length seq)
) )

; als solcher erkennbarer AVL-Baum:
(defstruct (avl-tree (:constructor box-tree (tree))
                     (:print-function print-avl-tree))
  (tree nil)
)
(proclaim '(inline box-tree avl-tree-tree))

; neue Konstruktorfunktion (für den Reader):
(defun new-avl-tree-constructor (&key contents &allow-other-keys)
  (seq:coerce contents 'avl-tree)
)
#+CLISP
(setf (svref (get 'avl-tree 'SYSTEM::DEFSTRUCT-DESCRIPTION) 3)
      'new-avl-tree-constructor
)

(defmacro unbox (seq) `(avl-tree-tree ,seq))

; (make-tree size) kreiert einen leeren AVL-Baum mit size Knoten.
(defun make-tree (size)
  (if (zerop size)
    nil
    (let ((left (make-tree (floor (1- size) 2)))
          (right (make-tree (ceiling (1- size) 2))))
      (make-node :level (1+ (level right)) :left left :right right :length size)
) ) )

(defun make-avl-tree (size)
  (box-tree (make-tree size))
)

; AVL-Baum als Sequence vom Typ AVL-TREE :
; Pointer ist ein Vektor mit Fill-Pointer.
; Fill-Pointer=0 -> am Ende angelangt.
; Fill-Pointer=2*k+1 -> Der Vektor enthält den Pfad zum nächsten NODE,
;   abwechselnd ein NODE und ein Richtungsindikator (LEFT, RIGHT).
(seq:define-sequence AVL-TREE
  :init        #'(lambda ()
                   (let* ((tr (unbox seq))
                          (l (level tr))
                          (pointer (make-array (* 2 l) :fill-pointer 0)))
                     (if tr
                       (locally (declare (type node tr))
                         (loop
                           (vector-push tr pointer)
                           (if (null (node-left tr)) (return))
                           (vector-push 'LEFT pointer)
                           (setq tr (node-left tr))
                     ) ) )
                     pointer
                 ) )
  :upd         #'(lambda (pointer)
                   (if (zerop (fill-pointer pointer))
                     (error "Am rechten Ende eines ~S angelangt." 'avl-tree)
                     (let ((tr (aref pointer (1- (fill-pointer pointer)))))
                       (declare (type node tr))
                       (if (node-right tr)
                         (progn
                           (setq tr (node-right tr))
                           (vector-push 'RIGHT pointer)
                           (loop
                             (vector-push tr pointer)
                             (if (null (node-left tr)) (return))
                             (setq tr (node-left tr))
                             (vector-push 'LEFT pointer)
                         ) )
                         (loop
                           (vector-pop pointer)
                           (if (zerop (fill-pointer pointer)) (return))
                           (if (eq (vector-pop pointer) 'LEFT) (return))
                       ) )
                       pointer
                 ) ) )
  :endtest     #'(lambda (pointer) (zerop (fill-pointer pointer)))
  :fe-init     #'(lambda ()
                   (let* ((tr (unbox seq))
                          (l (level tr))
                          (pointer (make-array (* 2 l) :fill-pointer 0)))
                     (if tr
                       (locally (declare (type node tr))
                         (loop
                           (vector-push tr pointer)
                           (if (null (node-right tr)) (return))
                           (vector-push 'RIGHT pointer)
                           (setq tr (node-right tr))
                     ) ) )
                     pointer
                 ) )
  :fe-upd      #'(lambda (pointer)
                   (if (zerop (fill-pointer pointer))
                     (error "Am linken Ende eines ~S angelangt." 'avl-tree)
                     (let ((tr (aref pointer (1- (fill-pointer pointer)))))
                       (declare (type node tr))
                       (if (node-left tr)
                         (progn
                           (setq tr (node-left tr))
                           (vector-push 'LEFT pointer)
                           (loop
                             (vector-push tr pointer)
                             (if (null (node-right tr)) (return))
                             (setq tr (node-right tr))
                             (vector-push 'RIGHT pointer)
                         ) )
                         (loop
                           (vector-pop pointer)
                           (if (zerop (fill-pointer pointer)) (return))
                           (if (eq (vector-pop pointer) 'RIGHT) (return))
                       ) )
                       pointer
                 ) ) )
  :fe-endtest  #'(lambda (pointer) (zerop (fill-pointer pointer)))
  :access      #'(lambda (pointer)
                   (if (zerop (fill-pointer pointer))
                     (error "Am Ende eines ~S angelangt." 'avl-tree)
                     (node-value (aref pointer (1- (fill-pointer pointer))))
                 ) )
  :access-set  #'(lambda (pointer value)
                   (if (zerop (fill-pointer pointer))
                     (error "Am Ende eines ~S angelangt." 'avl-tree)
                     (setf (node-value (aref pointer (1- (fill-pointer pointer))))
                           value
                 ) ) )
  :copy        #'(lambda (pointer)
                   (make-array (array-dimensions pointer)
                               :initial-contents pointer
                               :fill-pointer (fill-pointer pointer)
                 ) )
  :length      #'(lambda ()
                   (let ((tr (unbox seq)))
                     (if tr (node-length tr) 0)
                 ) )
  :make        #'make-avl-tree
  :elt         #'(lambda (index)
                   (let ((tr (unbox seq)))
                     (if (and tr (< index (node-length tr)))
                       (locally (declare (type node tr))
                         (loop ; Hier ist 0 <= index < (node-length tr)
                           (let ((lleft (length (node-left tr))))
                             (cond ((< index lleft)
                                    (setq tr (node-left tr))
                                   )
                                   ((= index lleft) (return (node-value tr)))
                                   (t ; (> index lleft)
                                    (setq index (- index (+ lleft 1)))
                                    (setq tr (node-right tr))
                       ) ) ) )     )
                       (error "Unzulässiger Index: ~S" index)
                 ) ) )
  :set-elt     #'(lambda (index value)
                   (let ((tr (unbox seq)))
                     (if (and tr (< index (node-length tr)))
                       (locally (declare (type node tr))
                         (loop ; Hier ist 0 <= index < (node-length tr)
                           (let ((lleft (length (node-left tr))))
                             (cond ((< index lleft)
                                    (setq tr (node-left tr))
                                   )
                                   ((= index lleft)
                                    (return (setf (node-value tr) value))
                                   )
                                   (t ; (> index lleft)
                                    (setq index (- index (+ lleft 1)))
                                    (setq tr (node-right tr))
                       ) ) ) )     )
                       (error "Unzulässiger Index: ~S" index)
                 ) ) )
  :init-start  #'(lambda (index)
                   (let* ((tr (unbox seq))
                          (l (level tr))
                          (pointer (make-array (* 2 l) :fill-pointer 0)))
                     (if (<= 0 index (length tr))
                       (if (and tr (< index (node-length tr)))
                         (locally (declare (type node tr))
                           (loop ; Hier ist 0 <= index < (node-length tr)
                             (vector-push tr pointer)
                             (let ((lleft (length (node-left tr))))
                               (cond ((< index lleft)
                                      (vector-push 'LEFT pointer)
                                      (setq tr (node-left tr))
                                     )
                                     ((= index lleft) (return))
                                     (t ; (> index lleft)
                                      (vector-push 'RIGHT pointer)
                                      (setq index (- index (+ lleft 1)))
                                      (setq tr (node-right tr))
                       ) ) ) ) )     )
                       (error "Unzulässiger Index: ~S" index)
                     )
                     pointer
                 ) )
  :fe-init-end   #'(lambda (index)
                   (let* ((tr (unbox seq))
                          (l (level tr))
                          (pointer (make-array (* 2 l) :fill-pointer 0)))
                     (if (<= 0 index (length tr))
                       (if (and tr (plusp index))
                         (locally (declare (type node tr))
                           (loop ; Hier ist 0 < index <= (node-length tr)
                             (vector-push tr pointer)
                             (let ((lleft (length (node-left tr))))
                               (cond ((<= index lleft)
                                      (vector-push 'LEFT pointer)
                                      (setq tr (node-left tr))
                                     )
                                     ((plusp (setq index (- index (1+ lleft))))
                                      (vector-push 'RIGHT pointer)
                                      (setq tr (node-right tr))
                                     )
                                     (t ; (= index (1+ lleft))
                                      (return)
                       ) ) ) ) )     )
                       (error "Unzulässiger Index: ~S" index)
                     )
                     pointer
                 ) )
)

