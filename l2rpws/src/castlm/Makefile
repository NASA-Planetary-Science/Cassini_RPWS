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

ifeq ($(N_ARCH),)
N_ARCH=$(shell uname -s).$(shell uname -p)
endif

ifeq ($(INST_NAT_BIN),)
INST_NAT_BIN=$(PREFIX)/bin/$(N_ARCH)
endif

ifeq ($(INST_NAT_LIB),)
INST_NAT_LIB=$(PREFIX)/lib/$(N_ARCH)
endif


# This lib needs C-Spice
ifeq ($(CSPICE_INC),)
$(error Defined the path to the C-Spice include directory in CSPICE_INC)
endif

##############################################################################
# Where to put stuff when it's built:

TOP=$(realpath ../../)

BUILD_DIR:=build.$(N_ARCH)

BUILD_INC=$(TOP)/include
BUILD_LIB=$(TOP)/$(BUILD_DIR)
BUILD_BIN=$(TOP)/$(BUILD_DIR)
BUILD_OBJ=$(TOP)/$(BUILD_DIR)/obj

##############################################################################
# Native Platform specific include

UNAME = $(shell uname)

include $(UNAME).mak
