# $Id: Makefile,v 1.9 2000/05/23 17:28:28 sds Exp $
# $Source: /cvsroot/clocc/clocc/Makefile,v $

TOP := $(shell pwd)
LISPEXT := lisp
SOURCES := clocc
include $(TOP)/clocc.mk

TOP_DEP = clocc.$(FASLEXT) src/defsystem-3.x/defsystem.$(FASLEXT)

ifneq ($(DO_DUMP),)
ifneq ($(DUMPEXT),)
clocc-top: clocc-top$(DUMPEXT)
endif

clocc-top$(DUMPEXT): $(TOP_DEP)
	$(RUNLISP) $(patsubst %,-i %,$^) -d clocc-top

else

clocc-top: clocc-top.$(FASLEXT)

clocc-top.$(FASLEXT): $(TOP_DEP)
	$(RUNLISP) -cat $^ > $@

endif

cvs.log: force
	cvs log > $@ 2>/dev/null
	@fgrep "author:" $@ | sed 's/^.*author: \([^;]*\);.*$$/\1/' | \
		sort | uniq -c | sort | sed 's/^/    /';
	@fgrep "author:" $@ | wc -l;
