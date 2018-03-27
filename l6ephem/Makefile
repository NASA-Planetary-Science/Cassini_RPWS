#############################################################################
# Generic definitions for Native builds + Das1 

ifeq ($(PREFIX),)
ifeq ($(HOME),)
PREFIX=$(USERPROFILE)
else
PREFIX=$(HOME)
endif
endif

ifeq ($(INST_ETC),)
INST_ETC=$(PREFIX)/cfg
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

# Traditionally this has been /opt/project/das/datasetroot/cassini
ifeq ($(INST_DAS_DSDF),)
INST_DAS_DSDF=$(INST_ETC)/datasetroot
endif

BUILD_DIR:=build.$(N_ARCH)

##############################################################################
# Build the code using individual makes

build:
	cd lib && $(MAKE)
	cd tools && $(MAKE)
	cd cephemrdr && $(MAKE)
	cd casorb && $(MAKE)
	cd css && $(MAKE)

test:
	cd lib && $(MAKE) test
	cd tools && $(MAKE) test
	cd cephemrdr && $(MAKE) test
	cd casorb && $(MAKE) test
	cd css && $(MAKE)

install:
	cd lib && $(MAKE) install
	cd tools && $(MAKE) install
	cd cephemrdr && $(MAKE) install
	cd casorb && $(MAKE) install
	cd css && $(MAKE)

clean:
	cd lib && $(MAKE) clean
	cd tools && $(MAKE) clean
	cd cephemrdr && $(MAKE) clean
	cd casorb && $(MAKE) clean
	cd css && $(MAKE)
	
distclean:
	rm -r $(BUILD_DIR)

