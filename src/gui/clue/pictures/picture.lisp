;;;-*- Mode:Common-Lisp; Package:PICTURES; Fonts:(CPTFONT HL12B HL12BI HL12BI CPTFONTB); Base:10 -*-
;1;;*
;1;; Description:*
;1;;*	1Picture objects allow graphic objects to be arranged in hierarchical structures.*
;1;;*	1A graphic is a "child" of a picture (its "parent") if the graphic is a*
;1;;*	1member of the picture's list of elements. A picture is an "ancestor" of a*
;1;;*	1graphic (its "descendant") if it is the graphic's parent or an ancestor of its*
;1;;*	1parent. A "top-level" graphic is one whose parent is nil.*

;1;;*	1Read-access to the list of picture elements is given by the picture-elements*
;1;;*	1method.  Modification of the list of picture elements is supported by the*
;1;;*	1picture-insert/delete methods. These methods are responsible for reporting the*
;1;;*	1appropriate damaged regions to any view containing the picture.*
;1;;*
;1;; History:*
;1;;*	18/24/89*	1  Jim Dutton*	1Created*
;1;;*	19/05/89*	1  Jim Dutton*	1Added graphic-normalize method*
;1;;*	19/14/89*	1  Jim Dutton      Reversed order of compose in graphic-normalize*
;1;;*	19/19/89*	1  Jim Dutton*	1Changed graphic-draw to only draw children*
;1;;*				1that intersect the optional limit rectangle.*
;1;;*				
;1;; *
;1;; *			1  RESTRICTED RIGHTS LEGEND *
;1;; *
;1;; Use, duplication, or disclosure by the Government is subject to restrictions as  set*
;1;; forth in  subdivision  (b)(3)(ii)  of  the  Rights  in  Technical  Data and Computer*
;1;; Software clause at 52.227-7013. *
;1;; *
;1;; TEXAS INSTRUMENTS INCORPORATED, P.O. BOX 149149 AUSTIN, TEXAS 78714-9149*
;1;; Copyright (C) 1989, Texas Instruments Incorporated.  All rights reserved.*
;1;; *

(in-package "2PICTURES*")
(USE-PACKAGE  '(common-lisp clos))


;1Picture Class Definition:*

(defclass 4picture *(extent-cache graphic)
  (
   (elements		:type		list
                        :initarg	:elements
                        :initform	nil
                        :documentation	"3The graphic objects in the picture(mR)*")

   (views		:type		list
			:initform	nil
                        :documentation	"3The views to which the picture is attached(mR)*")
   )
  (:documentation "3Base class for composite graphical objects*"))


;1Type: picture-position*
(deftype 4picture-position *() '(or graphic (member :first :last)))


;1Function: make-picture*
;1  Return a new picture object with no elements.*

(defun 4make-picture* (&rest initargs
		     &key
		     &allow-other-keys)
  (declare (values picture))

  (apply #'make-instance 'picture initargs))


;1Method: graphic-draw*
;1  Draw the PICTURE object in the given VIEW. If MIN-X, MIN-Y, WIDTH, and HEIGHT*
;1  are given, then only parts of the object that lie within the given rectangle*
;1  need to be drawn.*
;1  For pictures, just draw each of the child graphics.*

(defmethod 4graphic-draw* ((picture picture) (view view)
                           &optional min-x min-y width height) 
  (declare (type (or null wcoord) min-x min-y width height))

  (with-slots (elements sensitivity) picture
    (world-transform picture)				;1 Cache our transform*
    (combined-gstate picture)                           ;1 Cache our gstate*
    (UNLESS (EQUAL sensitivity :hidden)
      (if (and min-x min-y width height)			;1 Was optional rect is given*
	  (dolist (child elements)		;1 Yes, conditionally draw children*
	    (when (graphic-intersects-p child min-x min-y width height)
	      (graphic-draw child view min-x min-y width height)))
	  (dolist (child elements)		;1 No, unconditionally draw children*
	    (graphic-draw child view))))))


;1Method: graphic-normalize*
;1  Normalize the PICTURE by applying its transform and then calling*
;1  graphic-normalize on each of its children.*
;1  Nothing of value is returned.*

(defmethod 4graphic-normalize *:before ((picture picture))

  (with-slots ((parent-transform transform) elements) picture
    (dolist (child elements)
      (with-slots ((child-transform transform)) child
        (setf child-transform
              (compose-transform child-transform
                                 parent-transform
                                 child-transform)))
      (graphic-normalize child))))


;1Method: graphic-views*
;1  Return a list of all the views to which the GRAPHIC is attached.*

(defmethod 4graphic-views *((picture picture))

  (with-slots (views) picture
    views))


;1Method: extent-compute*
;1  Compute the extent rectangle for the PICTURE.  This is done by combining the extents*
;1  of each of the elements in PICTURE and then applying the PICTURE's transform to the*
;1  resulting extent.*

(defmethod 4extent-compute *((picture picture))
  (declare (values (or null extent-rect)))

  (with-slots (elements) picture
    (when elements					;1 Is this an empty picture?*
      (let ((first-child-extent				;1 No, get the first child's extent*
              (extent-rectangle (first elements))))
        (when first-child-extent			;1 Is it defined?*
          (let ((temp-extent				;1 Yes, make a copy in a temp*
                  (extent-copy first-child-extent
                               (make-extent-rect))))
            (dolist (child (cdr elements))		;1 For all the other children...*
              (extent-combine (extent-rectangle child)	;1  Combine their extents*
                              temp-extent))
            (extent-transform (graphic-transform picture)	;1 Apply our transform to the result*
                              temp-extent
                              temp-extent)
            temp-extent))))))				;1 Return the computed extent*

  
;1Method: picture-delete*
;1  Removes the graphic at the given POSition from the PICTURE.*
;1  If POS is :FIRST or :LAST, the first or last object in the PICTURE is*
;1  deleted.  If POS is a graphic, then that graphic is deleted.*
;1  The deleted graphic is returned with its parent slot set to nil.*

(defmethod 4picture-delete* ((picture picture) pos)
  (declare (type picture-position pos))
  (declare (values graphic))

  (with-slots (elements) picture
    (when elements
      (let ((dead-graphic					;1 Remember who was killed*
              (case pos
                (:first	(pop elements))				;1 Delete first graphic on list*
                (:last  (prog1 (car (last elements))		;1 Delete last graphic*
                               (nbutlast elements)))
                (t	(progn (setf elements	    		;1 Delete the given graphic*
                                     (delete pos elements :test 'eq))
                               pos)))))
        (when dead-graphic					;1 If we killed something,*
          (graphic-stack-purge *transform-stack* dead-graphic)	;1 Notify the transform stack*
          (graphic-stack-purge *gstate-stack* dead-graphic)	;1 Notify the gstate stack*
          (setf (graphic-parent dead-graphic) nil))		;1 Clear its parent slot*

        dead-graphic))))					;1 Return the dead guy*


;1Method: picture-elements*
;1  Return the list of elements contained by PICTURE.*

(defmethod 4picture-elements* ((picture picture))
  (declare (values elements))

  (slot-value picture 'elements))


;1Function: picture-group*
;1  Create a new picture and reparent the given elements to it.*
;1  Return the new picture.*

(defun 4picture-group* (&rest elements)
  (declare (type list elements))

  (picture-reparent (make-picture) elements))		;1 Reparent all elements using a new picture*


;1Method: picture-insert*
;1  Inserts the GRAPHIC at the given POSITION in the PICTURE.  If POSITION is a*
;1  graphic, then GRAPHIC is inserted immediately after it.  If POSITION is :FIRST*
;1  or :LAST, then GRAPHIC is inserted at the beginning or the end of the elements*
;1  list respectively.  The parent slot of GRAPHIC is changed to point to PICTURE*
;1  and the GRAPHIC is returned.*

(defmethod 4picture-insert* ((picture picture) graphic &optional (pos :last))
  (declare (type picture-position pos))
  (declare (values graphic))

  (with-slots (elements) picture
    (case pos
      (:first (push graphic elements))				;1 Push graphic onto front of list*
      (:last  (if elements					;1 Glue graphic onto the end*
                  (rplacd (last elements) (list graphic))
                  (push graphic elements)))
      (t      (let ((pos-n (position pos elements)))		;1 Insert after pos on the list*
                (if pos-n
		    (LET ((a (nthcdr (+ pos-n 1) elements)))
		      (push graphic a))
                    (error "3Graphic not found in picture*"))))))

  (setf (graphic-parent graphic) picture)			;1 Set its new parent*
  graphic)							;1 Return the inserted graphic*


;1Method: picture-reparent*
;1  Move each of the ELEMENTS into the PICTURE.*
;1  Return the new parent picture.*

(defmethod 4picture-reparent* ((picture picture) elements)
  (declare (type list elements))

  (dolist (child elements)				;1 Go through the list*
    (when (graphic-parent child)			;1 If graphic has a parent*
      (picture-delete (graphic-parent child) child))	;1   remove it from its parent's list*
    (picture-insert picture child))			;1 Add it to the new picture*
  picture)						;1 Return the picture now containing graphic*


;1Method: picture-restack*
;1  For the given PICTURE, delete the graphic in OLD-POSITION and re-insert it*
;1  in NEW-POSITION.*

(defmethod 4picture-restack* ((picture picture) old-position new-position)
  (declare (type picture-position old-position new-position))

  (picture-insert picture (picture-delete picture old-position) new-position))


;1Method: picture-ungroup*
;1  Reparent all elements of PICTURE to the parent of PICTURE.*
;1  Delete PICTURE from its parent.*

(defmethod 4picture-ungroup* ((picture picture))

  (with-slots (elements) picture
    (let ((grand-parent (graphic-parent picture)))
      (if grand-parent					;1 Is there a grand parent?*
          (prog1
            (picture-reparent grand-parent elements)	;1 Yes, make it the parent*
            (picture-delete grand-parent picture))	;1  and delete the picture*
          (dolist (child elements)			;1 No, make them all independent*
            (picture-delete picture child))))))		;1  i.e., they have no parent








