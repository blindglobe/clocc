CLISP=clisp -ansi -norc
CMUCL_SAFE=/usr/lib/cmucl/lisp-safe.core
CMUCL_NORMAL=/usr/lib/cmucl/lisp-normal.core
CMUCL_SMALL=/usr/lib/cmucl/lisp-small.core
SBCL=sbcl --sysinit /dev/null --userinit /dev/null --noprogrammer
CMUCL=lisp -nositeinit -noinit -batch
RM=rm -fr
MV=mv -f
MKDIR=mkdir -p

all: cmucl-normal cmucl-safe cmucl-small sbcl clisp
	echo ok

.PHONY: clisp sbcl cmucl-normal cmucl-safe cmucl-small clean

clean:
	$(RM) -rf clisp sbcl cmucl-normal cmucl-small cmucl-safe

clisp:
	$(MKDIR) clisp ;
	$(RM) *.erg clisp/* ;
	time $(CLISP) -i tests.lisp < /dev/null && true
	$(MV) *.erg clisp ;
	$(RM) *.fas *.lib ;
	ls -l $@ ;
	cat clisp/state-info.erg > clisp-ERROR-IDS
	grep -h '^Bugid: ' clisp/*.erg  | cut -d ' ' -f 2 >> clisp-ERROR-IDS
	true

sbcl:
	$(MKDIR) sbcl ;
	$(RM) *.erg sbcl/* ;
	time $(SBCL) --eval '(load "tests")'
	$(MV) *.erg sbcl ;
	$(RM) *.x86f ;
	ls -l $@ ;
	cat sbcl/state-info.erg > sbcl-ERROR-IDS
	grep -h '^Bugid: ' sbcl/*.erg  | cut -d ' ' -f 2 >> sbcl-ERROR-IDS
	true

cmucl-normal:
	$(MKDIR) cmucl-normal ;
	$(RM) *.erg cmucl-normal/* ;
	time $(CMUCL) -core $(CMUCL_NORMAL) -load tests.lisp
	$(MV) *.erg cmucl-normal ;
	$(RM) *.x86f ;
	ls -l $@ ;
	cat cmucl-normal/state-info.erg > cmucl-normal-ERROR-IDS
	grep -h '^Bugid: ' cmucl-normal/*.erg  | cut -d ' ' -f 2 >> cmucl-normal-ERROR-IDS
	true

cmucl-small:
	$(MKDIR) cmucl-small ;
	$(RM) *.erg cmucl-small/* ;
	time $(CMUCL) -core $(CMUCL_SMALL) -load tests.lisp
	$(MV) *.erg cmucl-small ;
	$(RM) *.x86f ;
	ls -l $@ ;
	cat cmucl-small/state-info.erg > cmucl-small-ERROR-IDS
	grep -h '^Bugid: ' cmucl-small/*.erg  | cut -d ' ' -f 2 >> cmucl-small-ERROR-IDS
	true


cmucl-safe:
	$(MKDIR) cmucl-safe ;
	$(RM) *.erg cmucl-safe/* ;
	time $(CMUCL) -core $(CMUCL_SAFE) -load tests.lisp
	$(MV) *.erg cmucl-safe ;
	$(RM) *.x86f ;
	ls -l $@ ;
	cat cmucl-safe/state-info.erg > cmucl-safe-ERROR-IDS
	grep -h '^Bugid: ' cmucl-safe/*.erg  | cut -d ' ' -f 2 >> cmucl-safe-ERROR-IDS
	true


