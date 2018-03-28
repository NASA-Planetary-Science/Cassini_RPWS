##############################################################################
# Short hand for use in this makefile alone included programs
IB=$(INST_NAT_BIN)
IE=$(INST_ETC)
II=$(INST_INC)
IL=$(INST_NAT_LIB)


# Pick defaults for das integration, to autofind the dsdfs already setup
# by Larry and Co, set DAS_DATASETROOT=/home/Web/das/datasetroot
ifeq ($(DAS_DATASETROOT),)
DAS_DATASETROOT=$(INST_ETC)/datasetroot
endif


#############################################################################
# Explicit targets

PROGS = rpws_lr_list \
 rpws_lr_cal rpws_lr_mark rpws_lr_label \
 rpws_lr_pdsrdr rpws_lr_das2rdr.sh \
 rpws_lr_int rpws_lr_hfrcp rpws_mpflist \
 rpws_lr_rtinf.ksh rpws_lr_rtfc.ksh rpws_lr_ppfc.ksh rpws_lr_u2dat.ksh \
 rpws_lr_mrkinf.ksh rpws_lr_datlbl.ksh \
 rpws_lr_make.ksh rpws_lr_listinf.ksh rpws_lr_mkcalpds.ksh rpws_lr_callbl.ksh \
 rpws_lr_calmv.ksh rpws_lr_dat2png.ksh rpws_lr_das_lrfc.pl rpws_lr_png2htm.ksh \
 rpws_lr_thumb.ksh rpws_lr_htm2brws.ksh rpws_lr_brwspnglbl.ksh \
 rpws_lr_htmlbl.ksh rpws_lr_pnglbl.ksh
 #rpws_lr_rtmrkinf.ksh
 
ETCS = LRFC_MASTER.LBL LRF_CAL_MASTER.LBL
DSDFS = rpws_pds.dsdf
D2_DSDFS = lowrate_calibrated.dsdf

#############################################################################
# Pattern targets

BD=$(BUILD_DIR)

BUILD_PROGS = $(patsubst %, $(BD)/%, $(PROGS))
BUILD_ETCS = $(patsubst %, $(BD)/%, $(ETCS))
BUILD_ETCS = $(patsubst %, $(BD)/%, $(DSDFS))

INSTALL_PROGS = $(patsubst %, $(IB)/%, $(PROGS))
INSTALL_ETCS = $(patsubst %, $(IE)/pds/%, $(ETCS))
INSTALL_DSDFS = $(patsubst %, $(DAS_DATASETROOT)/rpws/%, $(DSDFS))
#INSTALL_D2_DSDFS = $(patsubst %, $(DAS2_DATASETROOT)/cassini/rpws/%, $(D2_DSDFS))

#############################################################################
# Pattern Rules

$(IB)/%:$(BD)/%
	umask 0002 && install -D -m 775 $< $@

$(IE)/%:$(BD)/%
	@if [ ! -f $@ ] ; then \
	  install -D -m 644 $< $@ ; echo "Starter Config -> $@"; \
	else \
	  echo "Old config retained: $@" ; \
	fi

$(IE)/pds/%:$(BD)/%
	@if [ ! -f $@ ] ; then \
	  install -D -m 644 $< $@ ; echo "Starter Config -> $@"; \
	else \
	  echo "Old config retained: $@" ; \
	fi
	
$(II)/%:$(BD)/%
	umask 0002 && install -D -m 664 $< $@

$(IL)%:$(BD)/%
	umask 0002 && install -D -m 664 $< $@
	
$(DAS_DATASETROOT)/rpws/%.dsdf:$(BD)/%.dsdf
	umask 0002 && install -D -m 664 $< $@

#$(DAS2_DATASETROOT)/cassini/rpws/%.dsdf:$(BD)/%.dsdf
#	umask 0002 && install -D -m 664 $< $@

	
#############################################################################
# Explicit Rules

.PHONY: scripts

# Building is complicated so it deligated to sub-makes, installation is
# simpler and is thus included here

all:$(BD) $(BUILD_PROGS) $(BUILD_ETCS)

$(BD):
	@mkdir -p $(BD)/obj $(BD)/test

install:$(INSTALL_PROGS) $(INSTALL_ETCS) $(INSTALL_DSDFS)


# Building individual units
$(BD)/rpws_lr_list:
	@cd lrf_list && $(MAKE)

$(BD)/rpws_mpflist:
	@cd mpflist && $(MAKE)
		
$(BD)/rpws_lr_cal:
	@cd lrscal && $(MAKE)

$(BD)/rpws_lr_int:
	@cd lrsint && $(MAKE)
	
$(BD)/rpws_lr_mark:
	@cd lrsmark && $(MAKE)

$(BD)/LRFC_MASTER.LBL $(BD)/rpws_lr_label \
$(BD)/CasMiss.h $(BD)/libCasMiss.a:
	@cd pdslbl && $(MAKE)
	
$(BD)/rpws_lr_hfrcp:
	@cd hfrcp && $(MAKE)
	
$(BD)/rpws_lr_pdsrdr $(BD)/rpws_pds.dsdf $(BD)/rpws_lr_pdsrdr.ksh:
	@cd pdsrdr && $(MAKE)

$(BD)/rpws_lr_datlbl.ksh $(BD)/rpws_lr_mrkinf.ksh  \
$(BD)/rpws_lr_listinf.ksh \
$(BD)/rpws_lr_ppfc.ksh $(BD)/rpws_lr_rtfc.ksh  \
$(BD)/rpws_lr_rtinf.ksh  $(BD)/rpws_lr_u2dat.ksh \
$(BD)/rpws_lr_mkcalpds.ksh $(BD)/rpws_lr_callbl.ksh \
$(BD)/rpws_lr_calmv.ksh $(BD)/rpws_lr_das_lrfc.pl \
$(BD)/rpws_lr_png2htm.ksh $(BD)/rpws_lr_thumb.ksh \
$(BD)/rpws_lr_htm2brws.ksh $(BD)/rpws_lr_brwspnglbl.ksh \
$(BD)/rpws_lr_htmlbl.ksh $(BD)/rpws_lr_pnglbl.ksh \
$(BD)/rpws_lr_make.ksh $(BD)/rpws_lr_dat2png.ksh $(BD)/LRF_CAL_MASTER.LBL:
	@cd scripts && $(MAKE)

#scripts:
#	@cd scripts && $(MAKE)

test:
	@cd lrf_list && $(MAKE) test
	@cd mpflist && $(MAKE) test
	@cd lrscal && $(MAKE) test
	@cd lrsint && $(MAKE) test
	@cd lrsmark && $(MAKE) test
	@cd pdslbl && $(MAKE) test
	@cd hfrcp && $(MAKE) test
	@cd pdsrdr && $(MAKE) test
	
clean:
	@cd lrf_list && $(MAKE) clean
	@cd mpflist && $(MAKE) clean
	@cd lrscal && $(MAKE) clean
	@cd lrsint && $(MAKE) clean
	@cd lrsmark && $(MAKE) clean
	@cd scripts && $(MAKE) clean
	@cd pdslbl && $(MAKE) clean
	@cd hfrcp && $(MAKE) clean
	@cd pdsrdr && $(MAKE) clean

nuke:
	rm -r $(BD)

distclean:
	rm -r $(BD)
