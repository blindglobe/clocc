TOP := $(shell cd ../..; pwd)
LISPEXT := lisp
SOURCES := animals base card check clhs closio date datedl doall elisp fileio \
	fin geo gnuplot gq h2lisp html iter laser list log math miscprint \
	octave rpm server simple sorted stat string symb tilsla url withtype
BASE = base
NET = base url gq
SYSTEM := cllib
include $(TOP)/clocc.mk
RM  = /bin/rm -f
LN  = /bin/ln
ZIP = /usr/local/bin/zip -9uD

cllib.zip:	$(LISPFILES) manifest README.html Makefile ChangeLog
	@${RM} cllib
	@$(LN) -s . cllib
	@echo ...updating zip file $@...;
	@$(ZIP) $@ $(patsubst %,cllib/%,$^)
	@$(RM) cllib

manifest:	$(LISPFILES)
	etags $^
	sed "s?^/.*/\([^/]*\),?\1?" TAGS | cut -d" " -f2 > $@

image:		$(addsuffix .fas,$(BASE))
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$^) -d image

image-net:	$(addsuffix .fas,$(NET))
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$^) -d image-net
