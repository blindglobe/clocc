all: normal safe sbcl clisp
	echo ok

.PHONY: clisp sbcl normal safe

clisp:
	mkdir clisp ; rm *.erg clisp/* ; clisp -a -i tests.lisp 
	mv *.erg clisp ; rm *.fas ; true

sbcl:
	mkdir sbcl ; mv *.erg sbcl/ ; echo '(load "tests")' | sbcl 
	mv *.erg sbcl ; rm *.x86f ; true


normal:
	mkdir cmucl-normal ; rm *.erg cmucl-normal/* ; lisp -core /usr/lib/cmucl/lisp-normal.core -load tests.lisp 
	mv *.erg cmucl-normal ; rm *.x86f ; true

safe:
	mkdir cmucl-safe ; rm *.erg cmucl-safe/* ; lisp -core /usr/lib/cmucl/lisp-safe.core -load tests.lisp 
	mv *.erg cmucl-safe ; rm *.x86f ; true


