CLISP=clisp -a -norc
CMUCL_SAFE=/usr/lib/cmucl/lisp-safe.core
CMUCL_NORMAL=/usr/lib/cmucl/lisp-normal.core
RM=rm -f
MV=mv -f
MKDIR=mkdir -p

all: normal safe sbcl clisp
	echo ok

.PHONY: clisp sbcl normal safe

clisp:
	$(MKDIR) clisp ; $(RM) *.erg clisp/* ;
	$(CLISP) -i tests.lisp 
	$(MV) *.erg clisp ; $(RM) *.fas *.lib ;
	ls $@ ;
	true

sbcl:
	$(MKDIR) sbcl ; $(RM) *.erg sbcl/* ;
	echo '(load "tests")' | sbcl 
	$(MV) *.erg sbcl ; $(RM) *.x86f ;
	ls $@ ;
	true

normal:
	$(MKDIR) cmucl-normal ; $(RM) *.erg cmucl-normal/* ;
	lisp -core $(CMUCL_NORMAL) -load tests.lisp 
	$(MV) *.erg cmucl-normal ; $(RM) *.x86f ;
	ls $@ ;
	true

safe:
	$(MKDIR) cmucl-safe ; $(RM) *.erg cmucl-safe/* ;
	lisp -core $(CMUCL_SAFE) -load tests.lisp 
	$(MV) *.erg cmucl-safe ; $(RM) *.x86f ;
	ls $@ ;
	true


