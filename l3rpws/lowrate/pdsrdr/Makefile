#
# NOTE: This is a gmake file.  It uses features specific to GNU make, since
# gmake is available everywhere, were as nmake and SUN make are not.
#

#############################################################################
# Generic definitions for Native builds

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
INST_DOC=$(INST_SHARE)/doc
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

# Extra dependency for make test 
ifeq ($(RPWS_SUPERVOL),)
RPWS_SUPERVOL=$(PREFIX)/pds
endif

BUILD_DIR:=../build.$(N_ARCH)
TEST_DIR:=$(BUILD_DIR)/test
OBJ_DIR=$(BUILD_DIR)/obj

export INST_ETC INST_NAT_BIN RPWS_SUPERVOL

##############################################################################
# Native Platform specific include

UNAME = $(shell uname)

include $(UNAME).mak
