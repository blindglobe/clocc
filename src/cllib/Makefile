# $Id: Makefile,v 2.12 2000/05/04 18:13:48 sds Exp $
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

image:		$(addsuffix .$(FASLEXT),$(BASE))
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$^) -d image

image-net:	$(addsuffix .$(FASLEXT),$(NET))
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$^) -d image-net

image-script:	$(addsuffix .$(FASLEXT),$(SCRIPT))
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$^) -d image-script
