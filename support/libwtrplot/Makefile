# Just execute the platform specific makefile
# NOTE: This is a gmake file.  It uses features specific to GNU make, since
# gmake is available everywhere, were as nmake and SUN make are not.

##############################################################################
# Generic definitions for: Native

ifeq ($(PREFIX),)
ifeq ($(HOME),)
PREFIX=$(USERPROFILE)
else
PREFIX=$(HOME)
endif
endif

ifeq ($(INST_ETC),)
INST_ETC=$(PREFIX)/etc
endif

ifeq ($(INST_SHARE),)
INST_SHARE=$(PREFIX)/share
endif

ifeq ($(INST_DOC),)
INST_DOC=$(PREFIX)/doc
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

BUILD_DIR:=build.$(N_ARCH)

##############################################################################
# Native Platform specific include

ifeq ($(PLATFORM_MAKE),)
MKPREFIX = $(shell uname)
else
MKPREFIX := $(PLATFORM_MAKE)
endif

$(info loading $(MKPREFIX).mak)

include $(MKPREFIX).mak


