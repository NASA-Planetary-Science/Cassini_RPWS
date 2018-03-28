##############################################################################
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

export PREFIX INST_ETC INST_BIN

# Short hand for use in this makefile alone included programs
IB=$(INST_NAT_BIN)
IE=$(INST_ETC)
II=$(INST_INC)
IM=$(INST_MAN)
IL=$(INST_NAT_LIB)

ID=$(INST_ETC)/datasetroot

BD=$(BUILD_DIR)

#############################################################################
# Pattern Rules

$(INST_NAT_BIN)/%:$(BD)/%
	install -D -m 775 $< $@

$(INST_ETC)/%:$(BD)/%
	@if [ ! -f $@ ] ; then \
	  install -D -m 644 $< $@ ; echo "Starter Config -> $@"; \
	else \
	  echo "Old config retained: $@" ; \
	fi

$(ID)/rpws/%:$(BD)/%
	install -D -m 664 $< $@

$(INST_MAN)/%:$(BD)/man/%
	install -D -m 664 $< $@

$(INST_INC)/%:include/%
	install -D -m 664 $< $@

$(INST_NAT_LIB)/%:$(BD)/%
	install -D -m 664 $< $@

#############################################################################
# Explicit Rules

.PHONY: scripts

# Building is complicated so it deligated to sub-makes, installation is
# simpler and is thus included here

all:  $(BD) util telemetry castlm mdb rpws_chdo rpws_chdo_client rpws_file \
    rtiu rpws_meander rpws_mpus sort_merge db rpws_mpii rpws_decomp \
	 rpws_lfdr_fake rpws_usdc cas_quat_mkdb cas_cgi_attitude dsp \
	 archive scripts das_wfr das_wbr
   
# Needed by ANA
f_all: telemetry  

$(BD):
	mkdir -p $(BD)/obj $(BD)/test $(BD)/etc
	

# Install Targets, you have to run gmake (with no targets) as a seperate step
# before make install will work
install:  $(II)/Cext.h $(II)/rpwstlm/CasCmdParse.h $(II)/rpwstlm/CasHfr.h \
 $(II)/rpwstlm/CasLp.h $(II)/rpwstlm/CasMfr.h $(II)/rpwstlm/CasMiniPacket.h \
 $(II)/rpwstlm/CasPds.h $(II)/rpwstlm/CasRecord.h \
 $(II)/rpwstlm/CasSpice.h $(II)/rpwstlm/CasType.h $(II)/rpwstlm/CasWfdr.h \
 $(II)/rpwstlm/RecordFile.h \
 $(IL)/librpwstlm.a  \
 $(II)/instrument.h $(II)/telemetry.h $(IL)/libtelemetry.a \
 $(IB)/rpws_mdb_list $(IB)/rpws_mdb_wrap $(IB)/rpws_chdo \
 $(IB)/rpws_chdo_listener $(IB)/rpws_file $(IB)/rtiu $(IB)/biucmd \
 $(IB)/sim_bce_rcv $(IB)/rpws_meander $(IB)/rpws_mpus $(IB)/rpws_sort_merge \
 $(IM)/man1/rpws_sort_merge.1 $(IB)/rpws_db_update $(IB)/rpws_db_patch \
 $(IB)/rpws_db_lock $(IB)/rpws_mpii  $(IB)/rpws_decomp $(IB)/rpws_lfdr_fake \
 $(IB)/rpws_usdc  $(IB)/cas_quat_mkdb $(IB)/cas_cgi_attitude \
 $(IB)/rpws_dsp_4 $(IB)/rpws_dsp_5 $(IB)/rpws_dsp_8 $(IB)/rpws_dsp_9 \
 $(IB)/rpws_dsp_9a $(IB)/rpws_dsp_r $(IB)/rpws_dsp_q $(IB)/rpws_dsp_hkanal \
 $(IB)/rpws_dsp_hkrom \
 $(IB)/rpws_archive $(IB)/rpws_housekeeping $(IE)/names.tab \
 $(IB)/rpws_push_sopc.sh $(IB)/rpws_push_wcron.sh  \
 $(IB)/rpws_push_wkill.sh  $(IB)/rpws_push_wpatch.sh \
 $(IB)/rpws_push_wshow.sh $(IB)/rpws_push_autorun.sh \
 $(IB)/rpws_sfdu2mp.sh \
 $(IB)/rpws_l2wfr_hirdr $(ID)/rpws/RPWShfwr0.dsdf $(ID)/rpws/RPWShfwr1.dsdf \
 $(ID)/rpws/RPWShfwr2.dsdf $(ID)/rpws/RPWShfwr3.dsdf $(ID)/rpws/RPWShfwr4.dsdf \
 $(IB)/rpws_l2wfr_lordr $(ID)/rpws/RPWSlfwr0.dsdf $(ID)/rpws/RPWSlfwr1.dsdf \
 $(ID)/rpws/RPWSlfwr2.dsdf $(ID)/rpws/RPWSlfwr3.dsdf $(ID)/rpws/RPWSlfwr4.dsdf \
 $(IB)/rpws_l2wbr_gainrdr $(IB)/rpws_l2wbr_80rdr $(IB)/rpws_l2wbr_80jamitrdr \
 $(IB)/rpws_l2wbr_80hfrdr  $(IB)/rpws_l2wbr_10rdr $(IB)/rpws_l2hfr_gainrdr \
 $(ID)/rpws/RPWShfrmsagc.dsdf $(ID)/rpws/RPWSwb10_V4.dsdf \
 $(ID)/rpws/RPWSwb80_HF.dsdf  $(ID)/rpws/RPWSwb80_V4.dsdf \
 $(ID)/rpws/RPWSwbgain.dsdf \
 $(IB)/rpws_l2wfr_waveform_hirdr $(IB)/rpws_l2wfr_waveform_lordr \
 $(IB)/rpws_l2wbr_waveform


 
# $(IB)/cas_attitude


# Building individual units

util: $(BD)
	@cd src/util && $(MAKE)

# Robert's telemetry library, may be endian agnostic, needed by ANA
telemetry:
	@cd src/telemetry && $(MAKE)

castlm: $(BD) 
	@cd src/castlm && $(MAKE)

sclk_scet:
	@cd src/sclk_scet && $(MAKE)

db:
	@cd src/db && $(MAKE)

rtiu: util
	@cd src/rtiu && $(MAKE)

mdb: util
	@cd src/mdb && $(MAKE)
		
rpws_file: util castlm
	@cd src/file && $(MAKE)

rpws_chdo: util
	@cd src/chdo && $(MAKE)

rpws_chdo_client: util
	@cd src/chdo_client && $(MAKE)
			
rpws_mpii: util
	@cd src/mpii && $(MAKE)

rpws_decomp: util
	@cd src/decomp && $(MAKE)

rpws_mpus: castlm
	@cd src/mpus && $(MAKE)

rpws_meander: castlm
	@cd src/meander && $(MAKE)

rpws_lfdr_fake: util
	@cd src/lfdr_fake && $(MAKE)
	
rpws_usdc: castlm util
	@cd src/usdc && $(MAKE)
	
cas_quat_mkdb cas_cgi_attitude: util
	@cd src/aacs && $(MAKE)
		
archive: util
	@cd src/archive && $(MAKE)

sort_merge: util mdb
	@cd src/sort_merge && $(MAKE)

dsp: util mdb sort_merge
	@cd src/dsp && $(MAKE)
		
das_wfr: util
	@cd src/das_wfr && $(MAKE)
	
das_wbr: util
	@cd src/das_wbr && $(MAKE)
	
# Overall operating scripts, these call many of the programs build earlier
scripts:
	@cd scripts && $(MAKE)
	
	
# Testing or cleaning everything

test: 
	@cd src/util && $(MAKE) test
	@cd src/castlm && $(MAKE) test
	@cd src/mdb && $(MAKE) test
	@cd sort_merge && $(MAKE) test
	#@cd src/raj/lib/Cext && $(MAKE) test
	@cd src/chdo_client && $(MAKE) test
	@cd src/chdo && $(MAKE) test
	@cd src/file && $(MAKE) test
	@cd src/dsp && $(MAKE) test
	@cd src/mpii && $(MAKE) test
	@cd src/decomp && $(MAKE) test
	@cd src/mpus && $(MAKE) test
	@cd src/lfdr_fake && $(MAKE) test
	@cd src/usdc && $(MAKE) test
	@cd src/meander && $(MAKE) test
	@cd src/db && $(MAKE) test
	
clean:
	@cd src/util && $(MAKE) clean
	@cd src/castlm && $(MAKE) clean
	@cd src/mdb && $(MAKE) clean
	@cd sort_merge && $(MAKE) test
	#@cd src/raj/lib/Cext && $(MAKE) clean
	@cd src/chdo_client && $(MAKE) clean
	@cd src/chdo && $(MAKE) clean
	@cd src/file && $(MAKE) clean
	@cd src/dsp && $(MAKE) clean
	@cd src/telemetry && $(MAKE) clean
	@cd src/mpii && $(MAKE) clean
	@cd src/decomp && $(MAKE) clean
	@cd src/mpus && $(MAKE) clean
	@cd src/lfdr_fake && $(MAKE) clean
	@cd src/usdc && $(MAKE) clean
	@cd src/meander && $(MAKE) clean
	@cd src/db && $(MAKE) clean

nuke:
	rm -r $(BD)

distclean:
	rm -r $(BD)
