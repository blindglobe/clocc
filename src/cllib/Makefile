# $Id: Makefile,v 2.13 2000/05/11 17:07:34 sds Exp $
# $Source: /cvsroot/clocc/clocc/src/cllib/Makefile,v $
TOP := $(shell cd ../..; pwd)
LISPEXT := lisp
SOURCES := animals base card check clhs closio date datedl doall elisp fileio \
	fin geo gnuplot gq h2lisp html iter laser list log math miscprint \
	octave rpm server simple sorted stat string symb tilsla url withtype \
	inspect autoload htmlgen xml prompt matrix getopt
BASE = $(TOP)/clocc base auto
NET = $(TOP)/clocc base url gq
SCRIPT = $(TOP)/clocc base getopt
SYSTEM := cllib
ZIPEXTRA = ../defsystem-3.x/defsystem.lisp ../tools/metering/metering.lisp \
	$(TOP)/etc/GNU-GPL
DOCFILES = $(SYSTEM).html entities.xml
include $(TOP)/clocc.mk
DUMPEXT := $(shell $(RUNLISP) -dumpext)

ifneq ($(DUMPEXT),)
image:		image$(DUMPEXT)
image-net:	image-net$(DUMPEXT)
image-script:	image-script$(DUMPEXT)
endif

image$(DUMPEXT):	$(addsuffix .$(FASLEXT),$(BASE))
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$^) -d image

image-net$(DUMPEXT):	$(addsuffix .$(FASLEXT),$(NET))
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$^) -d image-net

image-script$(DUMPEXT):	$(addsuffix .$(FASLEXT),$(SCRIPT))
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$^) -d image-script
