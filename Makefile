TOP := $(shell pwd)
LISPEXT := lisp
SOURCES := clocc
include $(TOP)/clocc.mk

# "make clocc-image" create a memory image for use with CLOCC under the name
# clocc-image$(DUMPEXT).

ifneq ($(DUMPEXT),)
clocc-image: clocc-image$(DUMPEXT)
endif

clocc-image$(DUMPEXT): clocc.$(FASLEXT) src/defsystem-3.x/defsystem.$(FASLEXT)
	$(RUNLISP) $(patsubst %,-i %,$^) -d clocc-image

