;-*- Mode: Common-lisp; Package: ytools; Readtable: ytools; -*-
(in-package :ytools)

(depends-on %module/ ytools)

(defvar really-copy-ytools* true)

(defun ytload-copy (source-dir target-dir)
   (let ((source-pn (->pathname source-dir))
	 (target-pn (->pathname target-dir)))
      (repeat :for ((fname :in '("ytload" "ytfm.lmd" "ytools.lmd")))
	 (dirs-file-copy source-pn target-pn fname "lmd"))))

(defun extra-lmds-copy (source-dir target-dir)
   (let ((source-pn (->pathname source-dir))
	 (target-pn (->pathname target-dir)))
      (repeat :for ((fname :in '("ynisp" "lisplang" "opt" "optop")))
	 (dirs-file-copy source-pn target-pn fname "lmd"))))

(defun ytools-copy (source-dir target-dir)
   (let ((source-pn (->pathname source-dir))
	 (target-pn (->pathname target-dir)))
      (repeat :for ((fname :in '("base" "datafun" "pathname" "module"
				 "slurp" "files" "depend"
				 "binders" "bq" "fileutils" "mapper"
				 "misc" "multilet" "nilscompat"
				 "object" "outin" "repeat" "setter"
				 "signal" "debug" "tracearound"
				 "ytools.system" "ytools.lsy")))
	 (dirs-file-copy source-pn target-pn fname "lisp"))
      (dir-pns-file-copy source-pn target-pn "CHANGELOG")))

(defun dirs-file-copy (source-dir target-dir fname type)
   (let ((source-dpn (merge-pathnames
			(make-pathname :type type)
			source-dir))
	 (target-dpn (merge-pathnames
			(make-pathname :type type)
			target-dir)))
      (dir-pns-file-copy source-dpn target-dpn fname)))
	   
(defun dir-pns-file-copy (source-dpn target-dpn fname)
   (let ((file-pn (parse-namestring fname)))
      (let ((from-pn (merge-pathnames file-pn source-dpn))
	    (to-pn (merge-pathnames file-pn target-dpn)))
	 (cond (really-copy-ytools*
		(sys::copy-file from-pn to-pn :overwrite true))
	       (t
		(out "Copy: " from-pn
		     :% " => " to-pn :%))))))
