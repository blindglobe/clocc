TOP := $(shell cd ../..; pwd)
LISPEXT := lisp
SOURCES := base math print list util date octave gnuplot url gq geo \
	rpm animals h2lisp clhs elisp card fin # matrix
include $(TOP)/clocc.mk

deps:	$(addsuffix .$(LISPEXT),$(SOURCES))
	@echo " *** making dependencies...";
	@rm -f deps;
	@for fl in $(SOURCES); do \
		DEPS=`sed -n -e "s/;.*$$//" -e "s/\\#-.*$$//" -e "s/(eval-when ([a-z ]*) //" -e "s/))/)/" -e "s/(sds-require \"\\([a-z0-9A-Z]*\\)\")/\\1.$$\(FASLEXT)/gp" $${fl}.$(LISPEXT)`; \
		echo $${fl}.'$$(FASLEXT): '$${DEPS} >> deps; \
		echo >> deps; \
	done;

include deps
