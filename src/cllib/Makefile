# $Id: Makefile,v 2.9 2000/03/23 00:09:15 sds Exp $
# $Source: /cvsroot/clocc/clocc/src/cllib/Makefile,v $
TOP := $(shell cd ../..; pwd)
LISPEXT := lisp
SOURCES := animals base card check clhs closio date datedl doall elisp fileio \
	fin geo gnuplot gq h2lisp html iter laser list log math miscprint \
	octave rpm server simple sorted stat string symb tilsla url withtype \
	inspect autoload htmlgen xml prompt
BASE = base auto
NET = base url gq
SYSTEM := cllib
ZIPEXTRA = ../defsystem-3.x/defsystem.lisp ../tools/metering/metering.lisp \
	../math/clmath/matrix.lisp
DOCFILES = $(SYSTEM).html
include $(TOP)/clocc.mk

image:		$(addsuffix .fas,$(BASE))
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$^) -d image

image-net:	$(addsuffix .fas,$(NET))
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$^) -d image-net
