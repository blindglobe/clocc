;; GENERIC-FUNCTION-CLASSES

(in-package "MK4")

(defclass mk4-generic-function-class (standard-generic-function)
  ()
  (:metaclass #+lispworks hcl:funcallable-standard-class
	      #+allegro mop:funcallable-standard-class
	      #+cmu pcl:funcallable-standard-class
	      #-(or lispworks allegro cmu) funcallable-standard-class)
  (:documentation "Generic function class so that EQL specializer can be
used with NO-APPLICABLE-METHOD"))


