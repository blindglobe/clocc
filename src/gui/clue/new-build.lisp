(load "library:subsystems/clx-library.x86f")

(defparameter *here* (make-pathname
		      :directory (pathname-directory *load-truename*)))

#+pcl
(progn
  (pushnew 'compile pcl::*defclass-times*)
  (pushnew 'compile pcl::*defgeneric-times*))

(defvar *clx-directory*  "target:clx/*")
(defvar *clue-directory* (merge-pathnames "clue/" *here*))
#+cmu
(setf (search-list "clue:") (list *here*))

;; Ensure VALUES is a legal declaration
#-cmu17
(proclaim '(declaration values))

#+cmu17 ;; Don't warn about botched values decls
(setf c:*suppress-values-declaration* t)

#+pcl ;; This is now in current sources
(pushnew 'values pcl::*non-variable-declarations*)

;; Ensure *features* knows about CLOS and PCL
(when (find-package 'pcl)
  (pushnew :pcl  *features*)
  (pushnew :clos *features*)
  (unless (find-package :clos)
    (rename-package :pcl :pcl '(:clos))))

(when (find-package 'clos)
  (pushnew :clos *features*))


;; Ensure *features* knows about the Common Lisp Error Handler
(when (find-package 'conditions)
  (pushnew :cleh *features*))

(defvar *clue-precom* "clue:clue/precom")
(defvar *pict-precom* "clue:pictures/precom")

(load "clue:patch.lisp")
(compile-file "target:clx/macros.lisp")
(load "target:clx/macros.x86f")
(compile-file "target:clx/bufmac.lisp")
(load "target:clx/bufmac.x86f")
(compile-file "clue:clue/clue.lisp")
(load "clue:clue/clue.x86f")
(compile-file "clue:clue/defgeneric.lisp")
(load "clue:clue/defgeneric.x86f")
(compile-file "clue:clue/event-parse.lisp")
(load "clue:clue/event-parse.x86f")
(compile-file "clue:clue/defcontact.lisp")
(load "clue:clue/defcontact.x86f")
(compile-file "clue:clue/intrinsics.lisp")
(load "clue:clue/intrinsics.x86f")
(compile-file "clue:clue/caches.lisp")
(load "clue:clue/caches.x86f")
(compile-file "clue:clue/resource.lisp")
(load "clue:clue/resource.x86f")
(compile-file "clue:clue/gray.lisp")
(load "clue:clue/gray.x86f")
(compile-file "clue:clue/cursor.lisp")
(load "clue:clue/cursor.x86f")
(compile-file "clue:clue/events.lisp")
(load "clue:clue/events.x86f")
(compile-file "clue:clue/virtual.lisp")
(load "clue:clue/virtual.x86f")
(compile-file "clue:clue/shells.lisp")
(load "clue:clue/shells.x86f")
(compile-file "clue:clue/root-gmgmt.lisp")
(load "clue:clue/root-gmgmt.x86f")
(compile-file "clue:clue/package.lisp")
(load "clue:clue/package.x86f")
(compile-file "clue:clue/precom.lisp")
(compile-file "clue:clio/clio.lisp")
(load "clue:clio/clio.x86f")
(compile-file "clue:clio/defgeneric.lisp")
(load "clue:clio/defgeneric.x86f")
(compile-file "clue:clio/ol-defs.lisp")
(load "clue:clio/ol-defs.x86f")
(compile-file "clue:clio/utility.lisp")
(load "clue:clio/utility.x86f")
(compile-file "clue:clio/core-mixins.lisp")
(load "clue:clio/core-mixins.x86f")
(compile-file "clue:clio/gravity.lisp")
(load "clue:clio/gravity.x86f")
(compile-file "clue:clio/ol-images.lisp")
(load "clue:clio/ol-images.x86f")
(compile-file "clue:clio/buttons.lisp")
(load "clue:clio/buttons.x86f")
(compile-file "clue:clio/form.lisp")
(load "clue:clio/form.x86f")
(compile-file "clue:clio/table.lisp")
(load "clue:clio/table.x86f")
(compile-file "clue:clio/choices.lisp")
(load "clue:clio/choices.x86f")
(compile-file "clue:clio/scroller.lisp")
(load "clue:clio/scroller.x86f")
(compile-file "clue:clio/slider.lisp")
(load "clue:clio/slider.x86f")
(compile-file "clue:clio/scroll-frame.lisp")
(load "clue:clio/scroll-frame.x86f")
(compile-file "clue:clio/mchoices.lisp")
(load "clue:clio/mchoices.x86f")
(compile-file "clue:clio/menu.lisp")
(load "clue:clio/menu.x86f")
(compile-file "clue:clio/psheet.lisp")
(load "clue:clio/psheet.x86f")
(compile-file "clue:clio/command.lisp")
(load "clue:clio/command.x86f")
(compile-file "clue:clio/confirm.lisp")
(load "clue:clio/confirm.x86f")
(compile-file "clue:clio/buffer.lisp")
(load "clue:clio/buffer.x86f")
(compile-file "clue:clio/text-command.lisp")
(load "clue:clio/text-command.x86f")
(compile-file "clue:clio/display-text.lisp")
(load "clue:clio/display-text.x86f")
(compile-file "clue:clio/edit-text.lisp")
(load "clue:clio/edit-text.x86f")
(compile-file "clue:clio/display-imag.lisp")
(load "clue:clio/display-imag.x86f")
(compile-file "clue:clio/dlog-button.lisp")
(load "clue:clio/dlog-button.x86f")
(compile-file "clue:clio/precom.lisp")
(compile-file "clue:pictures/package.lisp")
(load "clue:pictures/package.x86f")
(compile-file "clue:pictures/defgeneric.lisp")
(load "clue:pictures/defgeneric.x86f")
(compile-file "clue:pictures/types.lisp")
(load "clue:pictures/types.x86f")
(compile-file "clue:pictures/macros.lisp")
(load "clue:pictures/macros.x86f")
(compile-file "clue:pictures/sequence.lisp")
(load "clue:pictures/sequence.x86f")
(compile-file "clue:pictures/transform.lisp")
(load "clue:pictures/transform.x86f")
(compile-file "clue:pictures/extent.lisp")
(load "clue:pictures/extent.x86f")
(compile-file "clue:pictures/edge.lisp")
(load "clue:pictures/edge.x86f")
(compile-file "clue:pictures/class-def.lisp")
(load "clue:pictures/class-def.x86f")
(compile-file "clue:pictures/gstate.lisp")
(load "clue:pictures/gstate.x86f")
(compile-file "clue:pictures/gstack.lisp")
(load "clue:pictures/gstack.x86f")
(compile-file "clue:pictures/graphic.lisp")
(load "clue:pictures/graphic.x86f")
(compile-file "clue:pictures/font-family.lisp")
(load "clue:pictures/font-family.x86f")
(compile-file "clue:pictures/view-draw.lisp")
(load "clue:pictures/view-draw.x86f")
(compile-file "clue:pictures/scene.lisp")
(load "clue:pictures/scene.x86f")
(compile-file "clue:pictures/line.lisp")
(load "clue:pictures/line.x86f")
(compile-file "clue:pictures/circle.lisp")
(load "clue:pictures/circle.x86f")
(compile-file "clue:pictures/polypoint.lisp")
(load "clue:pictures/polypoint.x86f")
(compile-file "clue:pictures/polygon.lisp")
(load "clue:pictures/polygon.x86f")
(compile-file "clue:pictures/rectangle.lisp")
(load "clue:pictures/rectangle.x86f")
(compile-file "clue:pictures/bspline.lisp")
(load "clue:pictures/bspline.x86f")
(compile-file "clue:pictures/ellipse.lisp")
(load "clue:pictures/ellipse.x86f")
(compile-file "clue:pictures/label.lisp")
(load "clue:pictures/label.x86f")
(compile-file "clue:pictures/gimage.lisp")
(load "clue:pictures/gimage.x86f")
(compile-file "clue:pictures/gevents.lisp")
(load "clue:pictures/gevents.x86f")
(compile-file "clue:pictures/grabber.lisp")
(load "clue:pictures/grabber.x86f")
(compile-file "clue:pictures/view.lisp")
(load "clue:pictures/view.x86f")
(compile-file "clue:pictures/view-events.lisp")
(load "clue:pictures/view-events.x86f")
(compile-file "clue:pictures/view-select.lisp")
(load "clue:pictures/view-select.x86f")
(compile-file "clue:pictures/view-zoom.lisp")
(load "clue:pictures/view-zoom.x86f")
(compile-file "clue:pictures/view-pan.lisp")
(load "clue:pictures/view-pan.x86f")
(compile-file "clue:pictures/utilities.lisp")
(load "clue:pictures/utilities.x86f")
(compile-file "clue:pictures/save.lisp")
(load "clue:pictures/save.x86f")
(compile-file "clue:pictures/restore.lisp")
(load "clue:pictures/restore.x86f")
(compile-file "clue:pictures/precom.lisp")

(load "target:tools/setup")
(load "make-subsystems")
(quit)
