# -*- Makefile -*-
# Common Makefile rules.
# The variable TOP must already have been set.

RUNLISP := $(TOP)/bin/run-lisp
LISPFILE := $(TOP)/bin/lisp-file
FASLEXT := $(shell $(RUNLISP) -faslext)
FASLFILES = *.fas *.lib *.axpf *.x86f *.hpf *.sgif *.sparcf *.fasl \
	*.o *.data *.ufsl

default: force
	@echo " * you must specify a target, e.g., 'all'."

%.$(FASLEXT): $(shell $(LISPFILE) %)
	$(RUNLISP) -c $<

clean: force
	rm -f $(FASLFILES)

force:
