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

# ASSUME (always dangerous) that the giferator.sav can be found 
# as the first item on the IDL path.
GIFER_PATH=$(firstword $(subst :, ,$(IDL_PATH)) )
export GIFER_PATH

#TOP=$(realpath ../)

BUILD_DIR:=../build.$(N_ARCH)
TEST_DIR:=$(BUILD_DIR)/test

export INST_ETC INST_NAT_BIN

UNAME = $(shell uname)

############################################################################
# This is an unusual Makefile in that in handles all the
# Operation Systems we deal with in on file instead of 
# splitting out into os-specific sub-makes.

# Check for gmake on Sunos
ifeq ($(UNAME),SunOS)
ifneq ($(MAKE),gmake)
$(error Use gmake when building on SunOS)
endif
endif

##############################################################################
# Stuff to build/install

ifeq ($(UNAME),SunOS)
LRFC_SCRIPT_PATH=$(INST_NAT_BIN):/usr/bin:/opt/csw/bin
else
LRFC_SCRIPT_PATH=$(INST_NAT_BIN):/usr/bin
endif

export LRFC_SCRIPT_PATH

SCRIPT_SRCS=rpws_lr_rtinf.ksh.in rpws_lr_rtfc.ksh.in rpws_lr_ppfc.ksh.in \
  rpws_lr_u2dat.ksh.in rpws_lr_mrkinf.ksh.in rpws_lr_rtmrkinf.ksh.in \
  rpws_lr_datlbl.ksh.in rpws_lr_make.ksh.in rpws_lr_listinf.ksh.in \
  rpws_lr_mkcalpds.ksh.in rpws_lr_callbl.ksh.in rpws_lr_calmv.ksh.in \
  rpws_lr_dat2png.ksh.in rpws_lr_das_lrfc.pl.in rpws_lr_png2htm.ksh.in \
  rpws_lr_thumb.ksh.in rpws_lr_htm2brws.ksh.in rpws_lr_brwspnglbl.ksh.in \
  rpws_lr_htmlbl.ksh.in rpws_lr_pnglbl.ksh.in
  
ETC_SRCS=LRF_CAL_MASTER.LBL

##############################################################################
# Automatic Variables

BUILD_SCRIPTS = $(patsubst %.in, $(BUILD_DIR)/%, $(SCRIPT_SRCS))
BUILD_ETCS = $(patsubst %, $(BUILD_DIR)/%, $(ETC_SRCS))

##############################################################################
# Pattern Rules

$(BUILD_DIR)/%.ksh:%.ksh.in | $(BUILD_DIR)
	./envsubst.py $< $@
	chmod +x $@
	
$(BUILD_DIR)/%.pl:%.pl.in | $(BUILD_DIR)
	./envsubst.py $< $@
	chmod +x $@
	
$(BUILD_DIR)/%.LBL:etc/%.LBL | $(BUILD_DIR)
	cp $< $@
	
##############################################################################
# Explicit rules

all: $(BUILD_DIR) $(BUILD_SCRIPTS) $(BUILD_ETCS)


$(BUILD_DIR):
	@if [ ! -d $(BUILD_DIR) ] ; then mkdir $(BUILD_DIR); fi

clean:
	rm $(BUILD_SCRIPTS)

test:
	@echo "Unit tests not defined for scripts area"

