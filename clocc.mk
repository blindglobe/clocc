# -*- Makefile -*-
# Common Makefile rules.
# The variables TOP, SYSTEM, SOURCES and LISPEXT must already have been set.
# This file requires GNU Make
#
# $Id: clocc.mk,v 1.9 2000/04/19 17:00:09 sds Exp $
# $Source: /cvsroot/clocc/clocc/clocc.mk,v $

RUNLISP := $(TOP)/bin/run-lisp
LISPFILE := $(TOP)/bin/lisp-file
FASLEXT := $(shell $(RUNLISP) -faslext)
CLOCCIMAGE := $(TOP)/clocc-image
FASLFILES = *.fas *.lib *.axpf *.x86f *.hpf *.sgif *.sparcf *.fasl \
	*.o *.data *.ufsl
LISPFILES = $(addsuffix .$(LISPEXT),$(SOURCES))
DOCFILES += ChangeLog $(SYSTEM).list
MAKEFILES = Makefile $(SYSTEM).system
ZIPEXTRA += $(TOP)/clocc.mk $(TOP)/clocc.lisp
RM  = /bin/rm -f
LN  = /bin/ln
ZIP = /usr/local/bin/zip -9uD

default: force
	@echo " * you must specify a target, such as..."
	@echo " + all - compile all files in SOURCES ($(SOURCES)) one by one"
	@echo " + system - run mk:compile-file on SYSTEM ($(SYSTEM))"
	@echo " + ChangeLog - create the ChangeLog file using rcs2log"
	@echo " + $(SYSTEM).list - the list of all functons and variables defined by this system"
	@echo " + TAGS - Emacs tags"
	@echo " + $(SYSTEM).zip - the archive of SOURCES, DOCFILES ($(DOCFILES)), MAKEFILES ($(MAKEFILES)) and ZIPEXTRA ($(ZIPEXTRA))"

system: $(SYSTEM).system
	$(RUNLISP) -I $(CLOCCIMAGE) -x '(mk:compile-system "$(SYSTEM)")'

all: $(addsuffix .$(FASLEXT),$(SOURCES))

%.$(FASLEXT): %.$(LISPEXT)
	$(RUNLISP) $(patsubst %,-i %,$(filter-out $<,$^)) -c $<

ChangeLog: $(LISPFILES)
	rcs2log $^ > $@

TAGS:	 $(LISPFILES)
	etags $^

$(SYSTEM).list: TAGS
	sed -e 's?^/.*/??' -e 's/ *.*/ ...)/' -e 's/,[0-9]*$$//' TAGS > $@

$(SYSTEM).zip: $(DOCFILES) $(LISPFILES) $(MAKEFILES)
	@$(RM) $(SYSTEM);
	@$(LN) -s . $(SYSTEM);
	@$(LN) -s . extra;
	@$(LN) -s $(ZIPEXTRA) .;
	@echo ...updating zip file $@...;
	@$(ZIP) $@ $(patsubst %,$(SYSTEM)/%,$^) \
		$(patsubst %,$(SYSTEM)/extra/%,$(notdir $(ZIPEXTRA)));
	@$(RM) $(SYSTEM) extra $(notdir $(ZIPEXTRA));

clean: force
	rm -f $(FASLFILES)

force:
