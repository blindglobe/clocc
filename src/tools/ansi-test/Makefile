all: normal safe sbcl clisp
	echo ok

.PHONY: clisp sbcl normal safe

clisp:
	rm *.erg clisp/* ; clisp -a -i tests.lisp 
	mv *.erg clisp ; true

sbcl:
	mv *.erg sbcl/ ; echo '(load "tests")' | sbcl 
	mv *.erg sbcl ; true


normal:
	rm *.erg cmucl-normal/* ; lisp -core /usr/lib/cmucl/lisp-normal.core -load tests.lisp 
	mv *.erg cmucl-normal ; true

safe:
	rm *.erg cmucl-safe/* ; lisp -core /usr/lib/cmucl/lisp-safe.core -load tests.lisp 
	mv *.erg cmucl-safe ; true


