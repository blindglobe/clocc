# -*- Makefile -*-
# Common Makefile rules.
# The variables TOP, SOURCES and LISPEXT must already have been set.

RUNLISP := $(TOP)/bin/run-lisp
LISPFILE := $(TOP)/bin/lisp-file
FASLEXT := $(shell $(RUNLISP) -faslext)
FASLFILES = *.fas *.lib *.axpf *.x86f *.hpf *.sgif *.sparcf *.fasl \
	*.o *.data *.ufsl

default: force
	@echo " * you must specify a target, e.g., 'all' or 'ChangeLog'."

all: $(addsuffix .$(FASLEXT),$(SOURCES))

%.$(FASLEXT): %.$(LISPEXT)
	$(RUNLISP) $(patsubst %,-i %,$(filter-out $<,$^)) -c $<

ChangeLog: $(addsuffix .$(LISPEXT),$(SOURCES))
	rcs2log $^ > $@

#.DEFAULT:
#	@echo "no rule for '$@'; FASLEXT: '$(FASLEXT)'; LISPEXT: '$(LISPEXT)'; LISPFILE: '$(shell $(LISPFILE) $@)'";

clean: force
	rm -f $(FASLFILES)

force:
