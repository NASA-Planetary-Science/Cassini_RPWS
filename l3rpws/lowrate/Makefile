#############################################################################
# Generic definitions for Native builds only

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

ifeq ($(INST_MAN),)
INST_MAN=$(PREFIX)/man
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


export PREFIX INST_ETC INST_BIN

##############################################################################
# Native Platform specific include

UNAME = $(shell uname)

include $(UNAME).mak
