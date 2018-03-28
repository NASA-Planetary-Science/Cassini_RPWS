#############################################################################
# Generic definitions for Native builds + Hosted IDL + Hosted Python

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

ifeq ($(INST_INC),)
INST_INC=$(PREFIX)/include
endif

ifeq ($(N_ARCH),)
N_ARCH=$(shell uname -s).$(shell uname -p)
endif

ifeq ($(INST_NAT_BIN),)
INST_NAT_BIN=$(PREFIX)/bin/$(N_ARCH)
endif

ifeq ($(INST_NAT_LIB),)
INST_NAT_LIB=$(PREFIX)/lib/$(N_ARCH)
endif

ifeq ($(RPWS_TEMP),)
RPWS_TEMP=$(PREFIX)/tmp
endif

ifeq ($(H_ARCH),)
H_ARCH=idl$(IDL_RELEASE)
endif

INST_EXT_LIB=$(INST_NAT_LIB)/$(H_ARCH)

ifeq ($(PY_ARCH),)
PYVER:=$(shell python -c "import sys; print '.'.join( sys.version.split()[0].split('.')[:2] )")
PY_ARCH=python$(PYVER)
endif

ifeq ($(INST_PY_LIB),)
INST_PY_LIB=$(PREFIX)/lib/$(PY_ARCH)
endif

ifeq ($(SETUP_FILE),)
$(error SETUP_FILE needs to provide the location of RPWS processing environment setup file)
endif

##############################################################################
# Where to put stuff when it's built:

BUILD_DIR:=build.$(N_ARCH)

##############################################################################
# Native Platform specific include

UNAME = $(shell uname)

include $(UNAME).mak
