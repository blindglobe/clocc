# -*- Makefile -*-
# Common Makefile rules.
# The variable TOP must already have been set.

RUNLISP := $(TOP)/bin/run-lisp
FASLEXT := $(shell $(RUNLISP) -faslext)
FASLFILES = *.fas *.lib *.axpf *.x86f *.hpf *.sgif *.sparcf *.fasl *.o *.data

default: force
	@echo " * you must specify a target, e.g., 'all'";

clean: force
	rm -f $(FASLFILES)

force:
