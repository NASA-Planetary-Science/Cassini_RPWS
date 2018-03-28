UNAME = $(shell uname)

# Check for gmake on Sunos
ifeq ($(UNAME),SunOS)
ifneq ($(MAKE),gmake)
$(error Use gmake when building on SunOS)
endif
endif

#############################################################################
# Generic definitions for scripts with substitutions handled as native 
# programs

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

ifeq ($(N_ARCH),)
N_ARCH=$(shell uname -s).$(shell uname -p)
endif

ifeq ($(INST_NAT_BIN),)
INST_NAT_BIN=$(PREFIX)/bin/$(N_ARCH)
endif

BD:=build.$(N_ARCH)

# ASSUME (always dangerous) that the giferator.sav can be found 
# as the first item on the IDL path.
GIFER_PATH=$(firstword $(subst :, ,$(IDL_PATH)) )
export GIFER_PATH


##############################################################################
# Stuff to build/install

ifeq ($(UNAME),SunOS)
KP_SCRIPT_PATH=$(INST_NAT_BIN):/usr/bin:/opt/csw/bin
else
KP_SCRIPT_PATH=$(INST_NAT_BIN):/usr/bin
endif

export KP_SCRIPT_PATH

PARSE_SRCS=rpws_kp_dolots.sh.in rpws_kp_dat2png.ksh.in rpws_kp_dat2png.pl.in\
 rpws_kp_pnglbl.ksh.in rpws_kp_static_html.ksh.in rpws_kp_htm2brws.ksh.in\
 rpws_kp_htmlbl.ksh.in rpws_kp_daily.sh.in
 
COPY_SRCS=rpws_kp_fud.ksh

##############################################################################
# Automatic Variables

BUILD_SCRIPTS = $(patsubst %.in, $(BD)/%, $(PARSE_SRCS)) 
BUILD_SCRIPTS += $(patsubst %, $(BD)/%, $(COPY_SRCS))

INST_SCRIPTS = $(patsubst $(BD)/%,$(INST_NAT_BIN)/%,$(BUILD_SCRIPTS))

##############################################################################
# Pattern Rules

$(BD)/%:%.in | $(BD)
	./envsubst.py $< $@
	-chmod +x $@

$(BD)/%:% | $(BD)
	cp $< $@
	-chmod +x $@

$(INST_NAT_BIN)/%:$(BD)/%
	install -D -m 755 $< $@
	
##############################################################################
# Explicit rules

all: $(BD) $(BUILD_SCRIPTS) $(BUILD_ETCS)


$(BD):
	@if [ ! -d $(BD) ] ; then mkdir $(BD); fi

clean:
	rm $(BUILD_SCRIPTS)
	
distclean:
	rm -r $(BD)

test:
	@echo "Unit tests not defined for scripts area"

install:$(INST_SCRIPTS)
