TOP := $(shell cd ../../.. ; pwd)
include $(TOP)/clocc.mk

all: metering.$(FASLEXT)

%.$(FASLEXT) : %.lisp
	$(RUNLISP) -c $<
