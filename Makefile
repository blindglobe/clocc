TOP := $(shell pwd)
LISPEXT := lisp
SOURCES := clocc
include $(TOP)/clocc.mk

clocc-image: clocc.$(FASLEXT) src/defsystem-3.x/defsystem.$(FASLEXT)
	$(RUNLISP) $(patsubst %,-i %,$^) -d clocc-image
