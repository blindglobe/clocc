# CLLIB Makefile
# LISPTYPE should be set before using this file
#
# $Id: Makefile,v 2.24 2004/11/11 22:17:35 sds Exp $
# $Source: /cvsroot/clocc/clocc/src/cllib/Makefile,v $

TOP := $(shell cd ../..; pwd)
LISPEXT := lisp
SOURCES := animals base card check clhs closio date datedl doall elisp fileio \
	fin geo gnuplot gq h2lisp html iter laser list log math miscprint \
	octave rpm server simple sorted stat string symb tilsla url withtype \
	inspect autoload htmlgen xml prompt matrix getopt tests cvs rng csv \
	base64
BASE = $(TOP)/clocc base auto
NET = $(TOP)/clocc base url gq rpm
SCRIPT = $(TOP)/clocc base getopt
SYSTEM := cllib
ZIPEXTRA = ../defsystem-3.x/defsystem.lisp ../tools/metering/metering.lisp \
	$(TOP)/etc/GNU-GPL $(TOP)/bin/run-lisp
ZIPEXTRANOLINK = cllib/port/Makefile cllib/port/Changelog \
	cllib/port/port.system cllib/port/port.html cllib/port/*.lisp
ZIPEXTRALINK = ../port
DOCFILES = $(SYSTEM).html entities.xml
include $(TOP)/clocc.mk

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

auto.$(LISPEXT):	$(addsuffix .$(LISPEXT),$(SOURCES))
	touch $@
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$(BASE)) -i autoload -x \
		"(funcall (intern \"AUTOLOAD-GENERATE\" \"CLLIB\") \
			  '($(patsubst %,\"%\",$^)) \"$@\")"

auto.$(FASLEXT):	auto.$(LISPEXT)
	$(TOP)/bin/run-lisp $(patsubst %,-i %,$(BASE)) -c $^
