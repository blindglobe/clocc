;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

;;; Copyright (C) 1976-2003 
;;;     Drew McDermott and Yale University.  All rights reserved
;;; This software is released under the terms of the Modified BSD
;;; License.  See file COPYING for details.

(eval-when (:load-toplevel)
   (export '(make-Pathname is-Pathname
	     Pathname-host Pathname-device Pathname-directory
	     Pathname-name Pathname-type Pathname-version
	     pathname->string pathname-equal pathname-get
	     ->pathname def-ytools-logical-pathname ytools
	     filespecs->pathnames filespecs->ytools-pathnames)))

;;; Some logical names really name special modes of "file" handling.

;;;;(defvar pathname-traps* !())

;;; The slots are functions that obey the same calling conventions
;;; as the operations (slurping, etc.) that they implement
;;; The 'parser' is a function that takes a list of "operands" from
;;; a filespec, and the default pathname extracted from filespecs to 
;;; the left of the point where the File-op-vector's name occurred.
;;; The 'parser' returns a list of pathnames, a new default, and
;;; the operands it didn't want.
;;; The 'expander', if non-false, is a function that extracts from a "name"
;;; the forms to be inserted into a file when it contains
;;; (depends-on ... %foo/ name), where %foo/ is the control-pathname
;;; associated with this File-op-vector.
(defstruct (File-op-vector (:predicate is-File-op-vector))
   parser
   slurper 
   loader
   compiler
   expander)

;;; The control is a symbol associated with a File-op-vector.
(defstruct (Pseudo-pathname (:predicate is-Pseudo-pathname))
   control
   opvec ;; a File-op-vector
   )
;;; Most Pseudo-pathname are actually subclasses of this "class."

(defun place-pseudo-pathname-opvec (pspn)
   (or (Pseudo-pathname-opvec pspn)
       (let ((opvec (lookup-ytools-logical-pathname
		        (Pseudo-pathname-control pspn))))
	  (cond ((is-File-op-vector opvec)
		 (setf (Pseudo-pathname-opvec pspn)
		       opvec)
		 opvec)
		(t
		 (error "Pseudo-pathname control ~s is not associated with a File-op-vector [~s]"
			(Pseudo-pathname-control pspn)
			opvec))))))

(defun Pseudo-pathname-parser (pspn)
   (File-op-vector-parser (place-pseudo-pathname-opvec pspn)))

(defun Pseudo-pathname-slurper (pspn)
   (File-op-vector-slurper (place-pseudo-pathname-opvec pspn)))

(defun Pseudo-pathname-loader (pspn)
   (File-op-vector-loader (place-pseudo-pathname-opvec pspn)))

(defun Pseudo-pathname-compiler (pspn)
  (File-op-vector-compiler (place-pseudo-pathname-opvec pspn)))

(defun Pseudo-pathname-expander (pspn)
   (File-op-vector-expander (place-pseudo-pathname-opvec pspn)))

(defmethod make-load-form ((pspn Pseudo-pathname) &optional env)
   (declare (ignore env))
   `(make-Pseudo-pathname :control ',(Pseudo-pathname-control pspn)
			  :opvec false))
;;; The opvec will be reconstructed later
;;; VERY IMPORTANT that all subclasses of Pseudo-pathname have their
;;; own make-load-forms.

(defstruct (YTools-pathname
	       (:predicate is-YTools-pathname)
	       (:print-object
		  (lambda (ytp srm)
		     (format srm "YTp\"")
		     (format srm "~a"
			     (cond ((YTools-pathname-is-absolute ytp)
				    "!")
				   (t "~")))
		     (do ((dl (YTools-pathname-directory ytp) (cdr dl))
			  (firstiter t nil))
			 ((null dl))
		        (cond ((symbolp (car dl))
			       (format srm "%~s" (car dl)))
			      (t
			       (cond ((not firstiter)
				      (format srm "/")))
			       (format srm "~a" (car dl)))))
		     (let ((ytfile (YTools-pathname-file ytp)))
		       (cond (ytfile
			      (cond ((is-Symbol ytfile)
				     (format srm "+%~s" ytfile))
				    (t
				     (format srm "+~a" ytfile))))))
		     (format srm "\""))))
			   
;;;   name      ;; Optional sym this YT-pathname is stored under
   is-absolute
   directory  ;; A list of logical names and strings, containing at least one 
             ;; logical name.  If of length > 1, the
             ;; ones to the right are relative to the ones to the left
   file      ;; Either false; or a logical-name symbol;
             ;; or a pathname specifying only a name, optional type,
             ;; optional version
;;;;   filename  ;; Either a symbol or false.  The symbol that was used to
;;;;             ;; generate the file, if any.
)
;;; Hmmm.  Host and device are supposed to be hidden in the 'directory'

(defmethod make-load-form ((ytpn YTools-pathname) &optional env)
   (declare (ignore env))
   `(make-YTools-pathname 
       :is-absolute ',(YTools-pathname-is-absolute ytpn)
       :directory ',(YTools-pathname-directory ytpn)
       :file ',(YTools-pathname-file ytpn)))

;;; To avoid messy conds (or move them around, anyway), 
;;; we access YTools-pathnames with generic functions
;;; that work equally well on ordinary Pathnames.

(defgeneric pn-is-absolute (pn)

   (:method ((pn pathname))
      (let ((dir (pathname-directory pn)))
	 (and (consp dir)
	      (eq (car dir) ':absolute))))

   (:method ((pn YTools-pathname))
      (YTools-pathname-is-absolute pn)))

;;; List of strings, with :relative or :absolute removed.
;;; (That's handled elsewhere.)
(defgeneric pn-directory (pn)
 
  (:method ((pn pathname))
      (pathname-YT-dir pn))

   (:method ((pn YTools-pathname))
      (YTools-pathname-directory pn)))

(defgeneric pn-has-name (pn)
   (:method ((pn pathname))
      (not (not (pathname-name pn))))
   (:method ((pn YTools-pathname))
      (not (not (YTools-pathname-file pn)))))

(defgeneric pn-merge (pn1 pn2)

   (:method ((pn1 pathname) (pn2 pathname))
      (merge-pathnames pn1 pn2))

   (:method ((pn1 pathname) (pn2 YTools-pathname))
      (multiple-value-bind (name type version)
			   (cond ((pathname-name pn1)
				  (values (pathname-name pn1)
					  (pathname-type pn1)
					  (pathname-version pn1)))
				 ((YTools-pathname-file pn2)
				  (let ((pn2-fpn (YTools-pathname-file pn2)))
				     (cond ((or (not pn2-fpn)
						(is-Symbol pn2-fpn))
					    (values pn2-fpn false false))
					   (t
					    (values (pathname-name pn2-fpn)
						    (pathname-type pn2-fpn)
						    (pathname-version pn2-fpn))))))
				 (t
				  (values false false false)))
	 (cond ((pn-is-absolute pn1)
		(cond ((and name (is-Symbol name))
		       (make-YTools-pathname
			  ;;;;:name false
			  :is-absolute true
			  :directory (pathname-YT-dir pn1)
			  :file name))
		      (t
		       (make-pathname
			  :host (pathname-host pn1)
			  :device (pathname-device pn1)
			  :directory (pathname-directory pn1)
			  :name name
			  :type type
			  :version version))))
	       (t
		(make-YTools-pathname
	           ;;;;:name false
		   :is-absolute (pn-is-absolute pn2)
		   :directory (append (YTools-pathname-directory pn2)
				      (cdr (pathname-directory pn1)))
		   :file (cond ((or name type version)
				(make-pathname
				   :name name :type type :version version))
			       (t false)))))))

   (:method ((pn1 YTools-pathname) (pn2 pathname))
      (multiple-value-bind (name type version)
			   (cond ((YTools-pathname-file pn1)
				  (let ((pn1-fpn (YTools-pathname-file pn1)))
				     (cond ((or (not pn1-fpn)
						(is-Symbol pn1-fpn))
					    pn1-fpn)
					   (t
					    (values (pathname-name pn1-fpn)
						    (pathname-type pn1-fpn)
						    (pathname-version pn1-fpn))))))
				 ((pathname-name pn2)
				  (values (pathname-name pn2)
					  (pathname-type pn2)
					  (pathname-version pn2)))
				 (t
				  (values false false false)))
	 (let ((filepart (cond ((or (and name (is-Symbol name))
				    (not (or name type version)))
				name)
			       (t
				(make-pathname
				   :name name :type type :version version)))))
	    (cond ((pn-is-absolute pn1)
		   (make-YTools-pathname
		      ;;;;:name false
		      :is-absolute true
		      :directory (YTools-pathname-directory pn1)
		      :file filepart))
		  (t
		   (make-YTools-pathname
		      ;;;;:name false
		      :is-absolute (pn-is-absolute pn2)
		      :directory (append (pathname-YT-dir pn2)
					 (YTools-pathname-directory pn1))
		      :file filepart))))))

   (:method ((pn1 YTools-pathname) (pn2 YTools-pathname))
      (let ((file (or (YTools-pathname-file pn1)
		      (YTools-pathname-file pn2))))
	 (cond ((pn-is-absolute pn1)
		(make-YTools-pathname
		   ;;;;:name false
		   :is-absolute true
		   :directory (YTools-pathname-directory pn1)
		   :file file))
	       (t
		(make-YTools-pathname
		   ;;;;:name false
		   :is-absolute (pn-is-absolute pn2)
		   :directory (append (YTools-pathname-directory pn2)
				      (YTools-pathname-directory pn1))
		   :file file))))))

(defun pathname-YT-dir (pn)
   (nconc (cond ((pathname-host pn)
		 (list `(:host ,(pathname-host pn))))
		(t '()))
	  (cond ((pathname-device pn)
		 (list `(:device ,(pathname-device pn))))
		(t '()))
	  (let ((pndir (pathname-directory pn)))
	     (cond ((consp pndir)
		    (cdr pndir))
		   ((null pndir) pndir)
		   (t (list pndir))))))

(defgeneric pn-equal (pn1 pn2)
  (:method ((pn1 t) (pn2 t))
      (eq pn1 pn2)))

(defmethod pn-equal ((pn1 pathname) (pn2 pathname))
   (equal pn1 pn2))

(defmethod pn-equal ((pn1 YTools-pathname) (pn2 YTools-pathname))
   (and (eq (YTools-pathname-is-absolute pn1)
	    (YTools-pathname-is-absolute pn2))
	(equal (YTools-pathname-directory pn1)
	       (YTools-pathname-directory pn2))
	(equal (YTools-pathname-file pn1)
	       (YTools-pathname-file pn2))))

(declaim (special lisp-object-extn* lisp-source-extn* dir-delim*))

(eval-when (:compile-toplevel :load-toplevel)
   (subr-synonym is-Pathname pathnamep)
   (subr-synonym make-Pathname make-pathname)
   (subr-synonym Pathname-host pathname-host)
   (subr-synonym Pathname-device pathname-device)
   (subr-synonym Pathname-directory pathname-directory)
   (subr-synonym Pathname-name pathname-name)
   (subr-synonym Pathname-type pathname-type)
   (subr-synonym Pathname-version pathname-version))

(subr-synonym pathname->string namestring)
(subr-synonym pathname-equal equal)

(defun ->pathname (x)
   (pathname-resolve
     (->ytools-pathname x)
     false))

(defvar vacuous-pathname*
        (make-Pathname
	   :host nil
	   :device nil
	   :directory nil
	   :name nil
	   :type nil
	   :version nil))

;;;;(defvar ytools-logical* "YTOOLS")

(defvar ytools-logical-names-table* (make-eq-hash-table :size 10))
;;; -- Each entry is the pathname the logical-name names.  Logical
;;; name is just a symbol at this point.
;;; Reserved logical names like 'module' have non-pn definitions, which
;;; cause an error if someone tries to resolve them into actual pns.

(defun Pseudo-pathname-op-vector (pspn)
   (or (table-entry ytools-logical-names-table*
		    (Pseudo-pathname-control pspn))
       (error "Fumbled File-op-vector for pathname control ~s in ~s"
	      (Pseudo-pathname-control pspn)
	      pspn)))

(defvar pathname-prop-table* (make-hash-table :test #'equalp :size 100))

(defun pathname-get (pn prop)
   (cond ((is-Pseudo-pathname pn)
	  (error "Pseudo-pathnames have no props; ergo, ~s doesn't have prop ~s"
		 pn prop))
	 (t
	  (let ((e (table-entry pathname-prop-table* pn)))
	     (cond (e
		    (alist-entry prop e false))
		   (t false))))))

(defun pathname-put (pn prop value)
   (cond ((is-Pseudo-pathname pn)
	  (error "Pseudo-pathnames have no props; ergo, you can't set prop ~s of ~s to ~s"
		 prop pn value))
	 (t
	  (let ((e (table-entry pathname-prop-table* pn)))
	     (cond ((not e)
		    (setf (table-entry pathname-prop-table* pn) !())))
	     (setf (alist-entry
		      prop
		      (table-entry pathname-prop-table* pn)
		      false)
	           value)))))

(defun (setf pathname-get) (v pn prop)
   (pathname-put pn prop v))

(defun pathname-prop (prop pn)
  (pathname-get (->pathname pn) prop))

(defun set-pathname-prop (prop pn value)
  (pathname-put (->pathname pn) prop value))

(defsetf pathname-prop set-pathname-prop)

(defmacro def-ytools-logical-pathname (name ^pn &optional ^obj-version)
   `(define-ytools-log-pname ',name ,^pn ,^obj-version))
 
(defun define-ytools-log-pname (name pn &optional obj-version)
   (cond ((stringp pn)
	  (setq pn (->pathname pn))))
   (cond ((stringp obj-version)
	  (setq obj-version (->pathname obj-version))
	  (setq obj-version
		(let ((obj-dir (Pathname-directory obj-version)))
		   (cond ((and (not (atom obj-dir))
			       (eq (car obj-dir)
				   ':relative))
			  (place-relative-pathname
			     pn (cdr obj-dir) lisp-object-extn* true))
			 (t
			  (make-Pathname
			     :directory obj-dir
			     :type lisp-object-extn*)))))))
   ;;;;(format t "name = ~s~%" name)
   (setf (table-entry ytools-logical-names-table*
		      name)
         pn)
   (cond (obj-version
	  (setf (pathname-prop 'obj-version pn)
	        (pathname-resolve
		     (merge-pathnames
			obj-version pn)
		     true))))
   name)

;;; Produce pathname that bears relation 'dir-list' to 'pn'.  
(defun place-relative-pathname (pn dir-list suff ensure-existence)
   (cond ((stringp dir-list)
	  (setq dir-list (Pathname-directory (parse-namestring dir-list)))))
   (cond ((not (listp dir-list))
	  (error "place-relative-pathname can't handle string directory: ~s~%"
		 dir-list)))
   (let ((start (Pathname-directory pn)))
      (cond ((and (consp start) (eq (car start) ':absolute))
	     (let ((pn-dir (reverse (cdr start)))
		   (resid !())
		   (res !()))
		(dolist (d dir-list)
		   (cond ((or (memq d '(:back :up)) (equal d ".."))
			  (cond ((null res)
				 (cond ((null pn-dir)
					(error "Relative directory ~s undefined wrt ~s"
					       dir-list pn))
				       (t
					;;(setq res (cons ':up res))
					(setq resid
					      (cons (car pn-dir) resid))
					(setq pn-dir (cdr pn-dir)))))
				((null pn-dir)
				 (setq res (cdr res)))
				(t
				 (setq pn-dir (cdr pn-dir)))))
			 (t
			  (setq res (cons d res)))))
;;;;		(format t "pn-dir = ~s~%res = ~s~%resid = ~s~%"
;;;;			pn-dir res resid)
		(let ((res-pn
		          (make-Pathname
			      :directory
			         `(:absolute,@(reverse pn-dir)
					     ,@(reverse res) ,@resid)
			      :type suff)))
		   (cond (ensure-existence
			  (ensure-directories-exist res-pn)))
		   res-pn)))
	    (t
	     (error "Can't take relative directory with respect to relative directory ~s"
		    pn)))))

(defun ups-to-updowns (dir-list)
   (do ((dl (reverse dir-list) (cdr dl))
	(updnl !() (cons ':updown updnl)))
       ((or (null dl)
	    (not (memq (car dl) '(:up :back))))
	(reverse (nconc updnl dl)))))

(defmacro def-ytools-pathname-control (sym
				       &key slurper loader compiler expander parser)
   (cond ((and slurper loader compiler)
	  `(setf (table-entry ytools-logical-names-table* ',sym)
	         (make-File-op-vector
		     :parser ,parser
		     :slurper ,slurper
		     :loader ,loader
		     :compiler ,compiler
		     :expander ,expander)))
	 (t
	  (error "Must specify slurper, loader, and compiler in 'def-ytools-pathname-control' ~s"
		 sym))))

(defun lookup-ytools-logical-pathname (sym)
   (table-entry ytools-logical-names-table* sym))

;;;======================================================================
;;; turn a list of fload-style filespecs into a list of pathnames. the
;;; file extension will be added later if necessary.
;;;======================================================================

(defun filespecs->pathnames (specs)
  (mapcar (\\ (pn) (pathname-resolve pn true))
	  (filespecs->ytools-pathnames specs)))

(defun filespecs->ytools-pathnames (specs)
   (let ((default (make-Pathname))
	 (pathnames !())
	 (specl specs)
	 spec)
      (labels (
	       )
	 (loop 
;;;;	    (out "specl = " specl 3 "pathnames = " pathnames :%)

	    (cond ((null specl)
		   (return (reverse pathnames))))
	    (setq spec (car specl))
	    (setq specl (cdr specl))
	    (let ((pnx (cond ((is-Symbol spec)
			      (let ((symname (symbol-name spec)))
				 (cond ((and (> (length symname) 0)
					     (char= (elt symname 0) #\%))
					(multiple-value-bind 
					               (sym remainder)
					               (%-factor
							  symname spec specl specs)
					   (setq specl remainder)
					   sym))
				       (t
					(->ytools-pathname spec)))))
			     (t
			      (->ytools-pathname spec)))))
	       (cond (pnx
		      (cond ((is-Symbol pnx)
			     ;; It's a %-thingie, although we don't know yet
			     ;; if it's a logical name or a control name
			     (let (pnl)
				(multiple-value-setq (pnl default specl)
						     (let-logical-name-control
							pnx specl default))
				(setq pathnames (append pnl pathnames))
				(cond ((not default)
				       (setq default (make-Pathname))))))
			    ((pn-has-name pnx)
			     ;; If name is supplied, it generates one new
			     ;; pathname.  The directory of pnx does not
			     ;; affect the default, but if it's absolute
			     ;; it wipes out the previous default.
			     (cond ((pn-is-absolute pnx)
				    (setq pathnames (cons pnx pathnames))
				    (setq default (make-Pathname)))
				   (t
				    (on-list
				       (merge-with-default-given-name pnx default)
				       pathnames))))
			    ((pn-is-absolute pnx)
			     (setq default pnx))
			    (t
			     (setq default
			           (pn-merge pnx default)))))))))))

;;; 'symname' is the name of 'sym', a string starting with #\%.  
;;; Extract actual intended symbol by flushing the %.  To allow
;;; packages be made explicit, the '%' may occur alone, in which
;;; case it is followed by the actual symbol, as in ... % ydecl::baz ...
;;; Return the intended symbol, plus remaining elements of 'specl'.
(defun %-factor (symname sym specl specs)
      (cond ((= (length symname) 1)
	     ;; sym '%' 
	     ;; Next item must be a symbol
	     (cond ((and (not (null specl))
			 (is-Symbol (car specl)))
		    (values
		       (car specl)
		       (cdr specl)))
		   (t
		    (cerror "I'll ignore it"
			    "Bare '%' found before nonsymbol in filespecs ~s"
			    specs)
		    (values false specl))))
	    (t
	     (values (intern (subseq symname 1) (symbol-package sym))
		     specl))))

;;; Returns <pathnames, new-default, remainder>
;;; where 'pathnames' are (possibly Pseudo-) pathnames extracted from 'operands',
;;; and 'remainder' is a list of everything after the last element of 'operands'
;;; that 'sym' was interested in.
;;; If 'new-default' is false, that means reset to blank pathname.
;;; 'default' is pathname from stuff to the left of the logical name.
;;; 'new-default' is the new default to pass on to the right.
;;; 'sym' is a symbol whose name was extracted by dropping the '%' at the
;;; front of the symbol that actually occurred.     
(defun let-logical-name-control (sym operands default)
   (let ((name (Symbol-name sym)))
      (let ((namelen (length name)))
	 (let ((sym-kernel
		  (cond ((char= (elt name (- namelen 1))
				#\/)
			 (intern (subseq name 0 (- namelen 1))
				 (symbol-package sym)))
			(t sym))))
	    (let ((opvec-or-pn (lookup-ytools-logical-pathname sym-kernel)))
	       (cond ((is-File-op-vector opvec-or-pn)
		      (funcall (File-op-vector-parser opvec-or-pn)
			       operands default))
		     (t
		      ;; Just go back to thinking of it as a YTools-pathname
		      (let ((ytpn
			       (string->ytools-pathname
				   (concatenate 'string "%" name)
				   (symbol-package sym))))
			 (cond ((pn-has-name ytpn)
				;; If name is supplied, it generates one new
				;; pathname.  The directory of ytpn does not
				;; affect the default, but if it's absolute
				;; it wipes out the previous default.
				(cond ((pn-is-absolute ytpn)
				       (values (list ytpn)
					       false
					       operands))
				      (t
				       (values (list (merge-with-default-given-name
							ytpn default))
					       default
					       operands))))
			       ((pn-is-absolute ytpn)
				(values !()
					ytpn
					operands))
			       (t
				(values !()
					(pn-merge ytpn default)
					operands)))))))))))

(defun merge-with-default-given-name (pnx default)
		  ;; 'pnx' supplies a name.  Combine pnx with default to get
		  ;; resulting pathnamoid.
		  (cond ((or (is-YTools-pathname default)
			     (is-YTools-pathname pnx))
			 (make-YTools-pathname
			    :is-absolute (pn-is-absolute default)
			    :directory (append (pn-directory default)
					       (pn-directory pnx))
			    :file (cond ((is-YTools-pathname pnx)
					 (YTools-pathname-file pnx))
					(t
					 (make-pathname
					    :name (pathname-name pnx)
					    :type (pathname-type pnx)
					    :version (pathname-version pnx))))))
			(t
			 (merge-pathnames pnx default))))

(defun pathnames-merge-but-not-dirs (new-pn default-pn)
   (let ((new-dir (Pathname-directory new-pn)))
      (cond ((and new-dir
		  (not (equal new-dir '(:relative))))
	     new-pn)
	    (t
	     (merge-pathnames
	        new-pn default-pn)))))

(defun pathname-resolve-if-pseudo (pn)
   (let ((rpn (pathname-resolve pn false)))
      (cond ((is-Pseudo-pathname rpn) rpn)
	    (t pn))))

;;; Wring out the ytools logical names, replacing them with their
;;; their definitions.
(defun pathname-resolve (pn ensure-existence)
   (cond ((is-YTools-pathname pn)
	  (let ((fspec (YTools-pathname-file pn)))
	     (let ((new-pn (cond ((is-Pathname fspec) fspec)
				 (fspec
				  (log-name-as-pathname fspec))
				 (t vacuous-pathname*))))
	        (labels ((dir-resolve (dirlist)
			    (cond ((null dirlist) new-pn)
				  (t
				   (let ((dir (car dirlist))
					 (dres (dir-resolve (cdr dirlist))))
				      (pn-merge
				         dres
					 (cond ((is-Symbol dir)
						(log-name-as-pathname dir))
					       ((consp dir)
					        (make-pathname (car dir) (cadr dir)))
					       (t
						(make-pathname
						   :directory
						      `(:relative ,dir))))))))))
		   (let ((dir-pn (dir-resolve (YTools-pathname-directory pn))))
		      (let ((input-was-dir (not fspec))
			    (output-is-dir
			       (not (or (pathname-name dir-pn)
					(pathname-type dir-pn)
					(not (memq (pathname-version dir-pn)
						   '(nil :unspecific :newest)))))))
			 (cond ((not (eq input-was-dir output-is-dir))
				(cerror "I'll ignore the discrepancy"
					"Resolving YTools pathname ~s~% changes syntactic status in result~% ~s"
					pn dir-pn))))
		      (maybe-ensure-dirs
			 (cond ((and (YTools-pathname-is-absolute pn)
				     (not (pn-is-absolute dir-pn)))
				(make-pathname
				   :host (pathname-host dir-pn)
				   :device (pathname-device dir-pn)
				   :directory `(:absolute ,@(cdr dir-pn))
				   :name (pathname-name dir-pn)
				   :type (pathname-type dir-pn)
				   :version (pathname-version dir-pn)))
			       (t
				dir-pn))
			 ensure-existence))))))
	 (t pn)))

(defun log-name-as-pathname (log-pn)
   (let ((x (lookup-ytools-logical-pathname log-pn)))
      (cond ((and x (pathnamep x))
	     x)
	    (x
	     (error "YTools logical pathname ~s defined as non-pathname ~s"
		    log-pn x))
	    (t
	     (error "Undefined logical pathname ~s"
		    log-pn)))))

(defun maybe-ensure-dirs (pn ensure)
   (cond (ensure
	  ;;;;(format t "Device ~s for ~s~%" (Pathname-device pn) pn)
	  (ensure-directories-exist pn)))
   pn)

(defun dirstring-as-logname (dirstring)
   (declare (type string dirstring))
   (cond ((and (> (length dirstring) 1)
	       (char= (elt dirstring 0)
		      #\%))
	  (multiple-value-bind (sym pos)
			       (read-from-string dirstring false false
						 :start 1)
	     (cond ((and sym
			 (symbolp sym)
			 (= pos (length dirstring)))
		    sym)
		   (t false))))
	 (t false)))

(defun ->ytools-pathname (x)
   (cond ((or (is-Pathname x)
	      (is-Pseudo-pathname x)
	      (is-YTools-pathname x))
	  x)
	 ((is-Symbol x)
	  (string->ytools-pathname
	      (symbol-name-as-file-name (symbol-name x))
	      (symbol-package x))
;;;;	  (symbol->ytools-pathname x)
	  )
	 ((is-String x)
	  (string->ytools-pathname x false))
	 ((is-Pair x)
	  (let ((fspecs (filespecs->ytools-pathnames x)))
	     (cond ((= (len fspecs) 1)
		    (car fspecs))
		   (t
		    (error "Attempt to coerce ~% ~s ~% to single YTools pathname yields~% ~s"
			   x fspecs)))))
	 (t
	  (error "Can't coerce ~s to YTools pathname"
		 x))))

(defconstant +empty-pathname+ (parse-namestring ""))

;;; In spite of its name, this can return an ordinary pathname if no 
;;; %'s are encountered in 'x'.
(defun string->ytools-pathname (x pkg)
   (declare (string x))
   (let (segposl
	 ;; -- list of positions of substring boundaries
	 (pos -1)
	 (l (length x))
	 (abs false)
	 (ddl (elt directory-delimiter* 0)))
      (cond ((and (> l 0)
		  (or (char= (elt x 0) #\/)
		      (char= (elt x 0) ddl)))
	     (setq abs true)
	     (setq pos 0)))
      (cond ((= pos (- l 1))
	     (make-Pathname
		:directory (cond (abs '(:absolute))
				 (t '(:relative)))))
	    (t
	     (setq segposl (list pos))
	     (loop
		(setq pos (position-if (lambda (ch)
					  (or (char= ch #\%)
					      (char= ch #\/)
					      (char= ch ddl)))
				       x
				       :start (+ pos 1)))
		(cond (pos
		       (setq segposl (cons pos segposl)))
		      (t
;;;;		       (format t "segposl = ~s~%" segposl)
		       (return 
			  (string-segs->ytools-pathname
			     segposl x l abs pkg)))))))))

;;; 'segposl' is list (in reverse order) of positions of all directory
;;; delimiters in 'str', ending in -1 if str does not start with a delimiter,
;;; or 0 if it does.
(defun string-segs->ytools-pathname (segposl str strlen abs pkg)
   ;;;;(format t "At start, segposl = ~s~%" segposl)
   (multiple-value-bind (name-pn segposl)
			(cond ((= (car segposl) (- strlen 1))
			       ;; ends in '/'
			       (values +empty-pathname+
				       segposl))
			      (t
			       (values
				  (cond ((and (>= (car segposl) 0)
					      (char= (elt str (car segposl))
						     #\%))
					 (let ((logname
						  (string-logname-sym
						     (subseq str (+ (car segposl) 1))
						     pkg)))
					    (make-YTools-pathname
					       :is-absolute false
					       :directory !()
					       :file logname)))
					(t
					 (let ((name-part
						  (subseq str
							  (+ (car segposl)
							     1))))
					    (parse-namestring name-part))))
				  segposl)))
      (labels ((mergem (pnlist)
		  (cond ((null pnlist) name-pn)
			(t
			 (let ((md (mergem (cdr pnlist))))
;;;;			    (format t "< version = ~s > version = ~s~%"
;;;;				    (pathname-version md)
;;;;				    (pathname-version (car pnlist)))
			    (pn-merge
			        md (car pnlist)))))))
	 (setq segposl (nreverse segposl))
	 ;; segposl now ends in pos of last character before "name part."
	 (let ((dirsegs (mapcar (lambda (b e)
				   (subseq str
					   (cond ((and (>= b 0)
						       (char= (elt str b) #\%))
						  b)
						 (t (+ b 1)))
					   e))
				segposl
				(cdr segposl))))
	    (setq dirsegs
	          (remove-if (\\ (s) (string= s ""))
			     dirsegs))
	    (let ((ytpn (mergem (strings->ytools-pathnames dirsegs pkg))))
;;;;	       (format t "ytpn = ~s~%" ytpn)
	       (cond (abs
		      (cond ((is-YTools-pathname ytpn)
			     (make-YTools-pathname
				:is-absolute true
				:directory (YTools-pathname-directory ytpn)
				:file (YTools-pathname-file ytpn)))
			    (t
			     (let ((dir (pathname-directory ytpn)))
			        (cond ((car-eq dir ':relative)
				       (make-pathname
					  :host (pathname-host ytpn)
					  :device (pathname-device ytpn)
					  :directory
					     `(:absolute ,@(cdr dir))
					  :name (pathname-name ytpn)
					  :type (pathname-type ytpn)
					  :version (pathname-version ytpn)))
				      (t ytpn))))))
		     (t ytpn)))))))

(defun strings->ytools-pathnames (dirsegs pkg)
   (mapcar (lambda (seg)
	      (declare (string seg))
	      (cond ((char= (elt seg 0) #\%)
		     (let ((sym (string-logname-sym (subseq seg 1) pkg)))
		        (make-YTools-pathname
			   :is-absolute false
			   :directory (list sym)
			   :file false)))
		    (t
		     (parse-namestring (concatenate 'string
					  seg directory-delimiter*)))))
	   dirsegs))

;;; spkg is home package of symbol from whose name 'name' was extracted,
;;; or false if name was encountered as a string to begin with
;;; 
(defun string-logname-sym (name spkg)
   (cond ((string= name "")
	  (error "%/ illegal as YTools pathname"))
	 (t
	  ;; Convert to directory component
	  (let ((nanal (string-case-analyze name)))
	     (let ((trim-sym
		      (intern (cond ((and (eq lisp-preferred-case* ':upper)
					  (eq nanal ':lower))
				     (string-upcase name))
				    ((and (eq lisp-preferred-case* ':lower)
					  (eq nanal ':upper))
				     (string-downcase name))
				    (t name))
			      (or spkg *package*))))
		trim-sym)))))

#|
;;; This doesn't care what the value of the logical name is, it just
;;; prints it back out to a string as '%pkg::name' .
(defun sym-logname (sym spkg)
   (cond ((lookup-ytools-logical-pathname sym)
	  (concatenate 'string "%" (sym-print-with-package sym spkg)))
	 (t false)))
|#

;;;;(defun symbol-as-file-name (sym spkg)
;;;;   (symbol-name-as-file-name (symbol-name sym) spkg))

(defun symbol-name-as-file-name (name)
   (let ((an (string-case-analyze name)))
      (cond ((or (eq an ':mixed)
		 (eq filename-case* an))
	     name)
	    ((eq filename-case* ':lower)
	     (string-downcase name))
	    (t
	     (string-upcase name)))))
	
(defun name-all-preferred-case (name)
      (let ((nlen (length name))
	    (want-upper (eq lisp-preferred-case* ':upper)))
	 (do ((i 0 (+ i 1)))
	     ((or (= i nlen)
		  (not (eq (upper-case-p (elt name i))
			   want-upper)))
	      (= i nlen)))))

(defpackage empty-package)

;;; Print sym to string in such a way as to force the package to 
;;; be explicit.  We do it this way to try to anticipate any
;;; odd case conversion the host Lisp wants to do on writes and reads. 
(defun sym-print-with-package (sym spkg)
                              (ignore spkg)
	  ;; Force print with explicit package
	  (let ((*package* (find-package :empty-package)))
;;;;		    (cond ((eq spkg ytools-package*)
;;;;			   cl-user-package*)
;;;;			  (t ytools-package*))))
	     (format nil "~s" sym)))

;;; 'dir-rel' is relative directory; as we pop subdirs off 'dir-to-work-on',
;;; we push them back on relative directory.  Return 
;;; < new-rel, new-work-on >
;;; ('dir-to-work-on' is in reverse of normal direction, so lowest
;;; subdir is first.)
(defun directory-updown-append (dir-rel dir-to-work-on) 
   (let ((add-ons !()))
      (loop
	 (cond ((or (null dir-rel)
		    (not (eq (car dir-rel) ':updown)))
		(return (values (append dir-rel add-ons)
				dir-to-work-on)))
	       ((null dir-to-work-on)
		(error "Impossible relative-directory extension: ~s .. ~s"
		       (dirs-to-string (reverse dir-to-work-on))
		       (dirs-to-string dir-rel))))
	 (setq add-ons (cons (car dir-to-work-on)
			     add-ons))
	 (setq dir-to-work-on (cdr dir-to-work-on))
	 (setq dir-rel (cdr dir-rel)))))

(defun dirs-to-string (dirs)
   (namestring
      (make-Pathname
         :directory
	     (cond ((or (null dirs)
			(not (memq (car dirs) '(:absolute :relative))))
		    (cons ':relative dirs))
		   (t dirs)))))

(defun is-null-or-empty-string (str)
  (or (null str)
      (and (is-String str) (= 0 (length (the string str))))))

#|
(defun pathname-file-handler (pn)
   (let ((dir (Pathname-directory pn)))
      (and (= (length dir) 2)
	   (eq (car dir) ':relative)
	   (is-String (cadr dir))
	   (> (length (cadr dir)) 1)
	   (char= (elt (cadr dir) 0) #\%) 
	   (let ((sym (string-read-as-sym (cadr dir) 1)))
	      (let ((h (table-entry ytools-logical-names-table*
				    sym)))
		 (and (File-op-vector-p h)
		      h))))))

;;;;	      (let ((p (assq sym pathname-traps*)))
;;;;		 (and p (cadr p)))))))

(defun (setf pathname-file-handler) (h sym)
   (setf (table-entry ytools-logical-names-table* sym)
         ;;;;(alist-entry sym pathname-traps* false)
         h))

(defun string-read-as-sym (str start)
   (declare (string str))
   (let ((l (length str)))
      (and (> l (+ start 1))
	   (multiple-value-bind
			    (sym pos)
			    (read-from-string
			       str false false :start start :end l)
	      (cond ((and sym
			  (is-Symbol sym)
			  (= pos (length str)))
		     sym)
	   	    (t false))))))

|#

;;;;(datafun attach-datafun pathname-file-handler
;;;;   (defun :^ (_ sym fun-name)
;;;;      (setf (alist-entry sym pathname-traps* false)
;;;;	    (symbol-function fun-name))))

;; Find, e.g., .fasl file for .lisp file, or .chk file for .opt file .
;; assoc-prop is a symbol used to record and retrieve pathname properties
;;  that say where the associated file is, or where associated files
;;  in this directory normally are.  
;; If such a property is found, and only-if-exists=false, then the
;;  corresponding pathname is returned.  If the property is not found, or
;;  the property is there but the file is not, and only-if-exists=true,
;;  then the pathname of the first file found in the same directory as pn with
;;  a suffix in suffixes is returned.  
;; E.g., suppose we call 
;;   (pathname-find-associate #p"high/low/fff.aaa" 'bbb-file '("bbb") true)
;; and the pathname has a bbb-file prop, to wit #p"high/low/f3.bbb", but
;; this file doesn't exist.  And #p"high/low/" has a bbb-file prop, namely
;; #p"high/bbb/low/???.bbb"  (This is a pathname with no :name field, and
;; is probably unprintable in most Lisps.)
;; And there is a file high/bbb/low/fff.bbb.
;; Then pathname-find-associate returns its pathname, i.e.,
;; #p"high/bbb/low/fff.bbb"

(defun pathname-find-associate (pn assoc-prop suffix only-if-exists)
   (labels ((check-existence (opn)
	       (cond ((and opn only-if-exists (not (probe-file opn)))
		      false)
		     (t opn))))
      (setq pn (->pathname pn))
      (and (is-Pathname pn)
	   (or (check-existence (pathname-prop assoc-prop pn))
	       (let ((dir-pn (dir-pn pn)))
		  (let ((ov (pathname-prop assoc-prop dir-pn)))
		     (cond (ov
			    (check-existence
			       (make-Pathname :host (Pathname-host ov)
					      :device (Pathname-device ov)
					      :directory (Pathname-directory ov)
					      :name (Pathname-name pn)
					      :type (or suffix
							(Pathname-type pn)))))
			   (t
			    (check-existence 
			       (make-Pathname :host (Pathname-host pn)
					      :device (Pathname-device pn)
					      :directory (Pathname-directory pn)
					      :name (Pathname-name pn)
					      :type suffix))))))))))

;;E.g., (declare-pathname-associate 'checked "C:/prog/opt/" "../chk" ".chk")
(defun declare-pathname-associate (assoc-prop pn where suff)
   (setq pn (->pathname pn))
   (let ((whdir
	    (Pathname-directory (->pathname where))))
      (setf (pathname-prop assoc-prop pn)
	    (cond ((car-eq whdir ':absolute)
		   (make-Pathname 
		      :directory whdir
		      :type suff))
		  (t
		   (place-relative-pathname
		       pn (cdr whdir) suff true))))))

(defun string-case-analyze (str)
   (declare (string str))
      (let ((l (length str))
	    (some-upper false)
	    (some-lower false))
	 (do ((i 0 (+ i 1)))
	     ((= i l))
	    (cond ((lower-case-p (elt str i))
		   (setq some-lower true))
		  (t
		   (setq some-upper true))))
	 (cond ((and some-lower some-upper) ':mixed)
	       (some-upper ':upper)
	       (t ':lower))))

;;;;(define-ytools-log-pname 'ytools (->pathname ytools-home-dir*) "../bin/")

(defparameter ytools-bin-path* (strings-concat standard-bin-path* bin-idio*))

(def-ytools-logical-pathname ytools  (->pathname ytools-home-dir*) ytools-bin-path*)

(defstruct (Perform-pseudo-pn (:include Pseudo-pathname)
	                       (:predicate is-Perform-pseudo-pn)
	      (:print-object
	         (lambda (pf-pspn srm)
		    (format srm "#<Perform-pseudo-pn ~s>"
			    (mapcar #'condense (Perform-pseudo-pn-actions pf-pspn))))))
   actions)

(defmethod make-load-form ((perform-pspn Perform-pseudo-pn) &optional env)
   (declare (ignore env))
   `(make-Perform-pseudo-pn
        :control ',(Pseudo-pathname-control perform-pspn)
	:opvec false
	:actions ',(Perform-pseudo-pn-actions perform-pspn)))

(defmethod pn-equal ((pn1 Perform-pseudo-pn) (pn2 Perform-pseudo-pn))
   (equal (Perform-pseudo-pn-actions pn1)
	  (Perform-pseudo-pn-actions pn2)))

(defun perform-pspn-parse (operands _)
   (let ((remainder (or (member-if #'atom operands)
			!())))
      (values (list (make-Perform-pseudo-pn
		       :control 'Perform
		       :actions (ldiff operands remainder)))
	      false
	      remainder)))

(defun perform-pspn-execute (perform-pspn &rest _)
  (dolist (a (Perform-pseudo-pn-actions perform-pspn))
     (eval a)))

(def-ytools-pathname-control Perform
   :parser #'perform-pspn-parse
   :slurper #'perform-pspn-execute
   :loader #'perform-pspn-execute
   :compiler #'perform-pspn-execute
   :expander false)

(defun dir-pn (pn)
   (make-Pathname :host (Pathname-host pn)
		  :device (Pathname-device pn)
		  :directory (Pathname-directory pn)
		  :version #+allegro ':unspecific #-allegro ':newest))
		  