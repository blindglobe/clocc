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

Change.log:
	@rcs2log > $@ 2>/dev/null
	@egrep "^[a-zA-Z0-9]" $@ | cut -d'<' -f2 | cut -d@ -f1 | \
		sort | uniq -c | sort | sed 's/^/    /';
	@egrep "^[a-zA-Z0-9]" $@ | wc -l;
