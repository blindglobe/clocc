# -*- Makefile -*-
# Common Makefile rules.
# The variables TOP, SOURCES and LISPEXT must already have been set.

RUNLISP := $(TOP)/bin/run-lisp
LISPFILE := $(TOP)/bin/lisp-file
FASLEXT := $(shell $(RUNLISP) -faslext)
CLOCCTOP := $(TOP)/clocc
FASLFILES = *.fas *.lib *.axpf *.x86f *.hpf *.sgif *.sparcf *.fasl \
	*.o *.data *.ufsl
LISPFILES = $(addsuffix .$(LISPEXT),$(SOURCES))

default: force
	@echo " * you must specify a target, such as..."
	@echo " + all - compile all files in SOURCES ($(SOURCES)) one by one"
	@echo " + system - run mk:compile-file on SYSTEM ($(SYSTEM))"
	@echo " + ChangeLog - create the ChangeLog file using rcs2log"

system: $(SYSTEM).system
	$(RUNLISP) -i $(CLOCCTOP) -x '(mk:compile-system "$(SYSTEM)")'

all: $(addsuffix .$(FASLEXT),$(SOURCES))

%.$(FASLEXT): %.$(LISPEXT)
	$(RUNLISP) $(patsubst %,-i %,$(filter-out $<,$^)) -c $<

ChangeLog: $(LISPFILES)
	rcs2log $^ > $@

TAGS:	 $(LISPFILES)
	etags $^

clean: force
	rm -f $(FASLFILES)

force:
