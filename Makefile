TOP := $(shell pwd)
LISPEXT := lisp
SOURCES := clocc
include $(TOP)/clocc.mk

clocc-image: clocc.$(FASLEXT)
	$(RUNLISP) -c src/defsystem-3.x/defsystem.$(LISPEXT)
	$(RUNLISP) -i $^ -i src/defsystem-3.x/defsystem -d clocc-image
