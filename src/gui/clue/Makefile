CLISP=clisp

install:
	install --owner=root --group=root --mode=644 clue.system clio.system clio-examples.system pictures.system \
	  $(DESTDIR)/tmp/usr/share/common-lisp/systems
	cp -r Makefile  README README.pw pictures clue clio   \
		$(DESTDIR)/tmp/usr/share/common-lisp/repositories/clue

clue-clisp:
	$(CLISP) -norc -K full -i ../../../clocc-top.fas \
		-x '(mk:compile-system "clue")'

clio-clisp:
	$(CLISP) -norc -K full -i ../../../clocc-top.fas \
		-x '(mk:compile-system "clio")'

clio-ex-clisp:
	$(CLISP) -norc -K full -i ../../../clocc-top.fas \
		-x '(mk:compile-system "clio-examples")'

pictures-clisp:
	$(CLISP) -norc -K full -i ../../../clocc-top.fas \
		-x '(mk:compile-system "pictures")'

clean-clisp:
	rm -fv `find . -name \*.fas -o -name \*.lib`
