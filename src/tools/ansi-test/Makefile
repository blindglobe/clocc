CLISP=clisp -ansi -norc
CMUCL_SAFE=/usr/lib/cmucl/lisp-safe.core
CMUCL_NORMAL=/usr/lib/cmucl/lisp-normal.core
RM=rm -fr
MV=mv -f
MKDIR=mkdir -p

all: normal safe sbcl clisp
	echo ok

.PHONY: clisp sbcl normal safe

clisp:
	$(MKDIR) clisp ;
	$(RM) *.erg clisp/* ;
	time $(CLISP) -i tests.lisp
	$(MV) *.erg clisp ;
	$(RM) *.fas *.lib ;
	ls -l $@ ;
	true

sbcl:
	$(MKDIR) sbcl ;
	$(RM) *.erg sbcl/* ;
	time echo '(load "tests")' | sbcl
	$(MV) *.erg sbcl ;
	$(RM) *.x86f ;
	ls -l $@ ;
	true

normal:
	$(MKDIR) cmucl-normal ;
	$(RM) *.erg cmucl-normal/* ;
	time lisp -core $(CMUCL_NORMAL) -load tests.lisp
	$(MV) *.erg cmucl-normal ;
	$(RM) *.x86f ;
	ls -l $@ ;
	true

safe:
	$(MKDIR) cmucl-safe ;
	$(RM) *.erg cmucl-safe/* ;
	time lisp -core $(CMUCL_SAFE) -load tests.lisp
	$(MV) *.erg cmucl-safe ;
	$(RM) *.x86f ;
	ls -l $@ ;
	true


