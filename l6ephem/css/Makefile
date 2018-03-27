##############################################################################
# Generic definitions for: Hosted Python Library

ifeq ($(PREFIX),)
ifeq ($(HOME),)
PREFIX=$(USERPROFILE)/cassini
else
PREFIX=$(HOME)/cassini
endif
endif

ifeq ($(INST_ETC),)
INST_ETC=$(PREFIX)/etc
endif

ifeq ($(INST_SHARE),)
INST_SHARE=$(PREFIX)/share
endif

ifeq ($(INST_DOC),)
INST_DOC=$(INST_SHARE)/doc
endif

ifeq ($(INST_INC),)
INST_INC=$(PREFIX)/include
endif

ifeq ($(N_ARCH),)
N_ARCH=$(shell uname -s).$(shell uname -p)
endif

ifeq ($(PYVER),)
PYVER:=$(shell python -c "import sys; print '.'.join( sys.version.split()[0].split('.')[:2] )")
endif

ifeq ($(H_ARCH),)
H_ARCH=python$(PYVER)
endif

ifeq ($(INST_NAT_BIN),)
INST_NAT_BIN=$(PREFIX)/bin/$(N_ARCH)
endif

ifeq ($(INST_HOST_LIB),)
INST_HOST_LIB=$(PREFIX)/lib/$(H_ARCH)
endif

BUILD_DIR:=build.$(N_ARCH)

##############################################################################
# Since this is a pure python module, using same makefile for Solaris and
# Linux.  May need to split it up later if MacOS or Windows support is added.

#UNAME = $(shell uname)

#include $(UNAME).mak


export INST_ETC

# Explicit Rules #############################################################


build: $(BUILD_DIR) build_pylib


$(BUILD_DIR):
	@if [ ! -d "$(BUILD_DIR)" ]; then mkdir $(BUILD_DIR); fi

build_pylib:
	python$(PYVER) setup.py build -b $(BUILD_DIR)

install:$(INST_ETC)/invoke.sh  install_pylib

$(INST_ETC)/invoke.sh:$(THIS_FILE)
	@echo \#!/usr/bin/env bash > $@
	@echo >> $@
	@echo "source $(INST_ETC)/setup.sh" >> $@
	@echo >> $@
	@echo -n \"$$  >> $@
	@echo "@\"" >> $@
	@echo >> $@
	@echo -n "exit $$" >> $@
	@echo "?" >> $@
	@chmod a+x $@
	@echo $@ generated

install_pylib:
	python$(PYVER) setup.py install_lib --skip-build -b $(BUILD_DIR)/lib -d $(INST_HOST_LIB)
	 
$(INST_NAT_BIN)/css_invoke.sh:$(BUILD_DIR)/css_invoke.sh
	install -D -m 775 $< $@

clean:
	rm -r -f build $(BUILD_DIR)

distclean:
	rm -r -f build $(BUILD_DIR)

test: 
	echo "Unit tests have not been defined"
