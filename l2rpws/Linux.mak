##############################################################################
# Note: Many of the Cassini programs can't be run on Linux because they
#       assume a 32-bit word-size, big-endian machine.  Some of the stuff
#       written by Robert can be run on linux and only those programs are
#       included in this make file

export PREFIX INST_ETC INST_BIN

# Short hand for use in this makefile alone included programs
IB=$(INST_NAT_BIN)
IE=$(INST_ETC)
II=$(INST_INC)
IL=$(INST_NAT_LIB)

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
	
$(INST_INC)/%:include/%
	install -D -m 664 $< $@

$(INST_NAT_LIB)/%:$(BD)/%
	install -D -m 664 $< $@

#############################################################################
# Explicit Rules

.PHONY: scripts

# Building is complicated so it deligated to sub-makes, installation is
# simpler and is thus included here

# Not very much of the Cassini program set is portable due to the 
# use of structure packing
all:  util castlm mdb rpws_meander rpws_mpus db

$(BD):
	mkdir -p $(BD)/obj $(BD)/test
	

# Install Targets, you have to run gmake (with no targets) as a seperate step
# before make install will work
# Install Targets, you have to run gmake (with no targets) as a seperate step
# before make install will work
install:  $(II)/Cext.h $(II)/instrument.h $(II)/telemetry.h $(IL)/libtelemetry.a\
 $(II)/rpwstlm/CasCmdParse.h $(II)/rpwstlm/CasHfr.h \
 $(II)/rpwstlm/CasLp.h $(II)/rpwstlm/CasMfr.h $(II)/rpwstlm/CasMiniPacket.h \
 $(II)/rpwstlm/CasPds.h $(II)/rpwstlm/CasRecord.h \
 $(II)/rpwstlm/CasSpice.h $(II)/rpwstlm/CasType.h $(II)/rpwstlm/CasWfdr.h \
 $(II)/rpwstlm/RecordFile.h \
 $(IL)/librpwstlm.a $(IB)/rpws_meander $(IB)/rpws_mpus \
 $(IB)/rpws_db_update $(IB)/rpws_db_patch  $(IB)/rpws_db_lock


# libutil.a in general is not portable, but some of the stuff used by
# simple programs works okay on linux.
util: $(BD)
	@cd src/util && $(MAKE)
	
# Robert's telemetry library, may be endian agnostic, needed by ANA
telemetry:
	@cd src/telemetry && $(MAKE)

mdb: util
	@cd src/mdb && $(MAKE)
	
db:
	@cd src/db && $(MAKE)

castlm: $(BD) 
	@cd src/castlm && $(MAKE)
	
rpws_meander: castlm
	@cd src/meander && $(MAKE)

rpws_mpus: castlm
	@cd src/mpus && $(MAKE)
	
# Overall operating scripts, these call many of the programs build earlier
scripts:
	@cd scripts && $(MAKE)
	
	
# Testing or cleaning everything
test: 
	@cd src/castlm && $(MAKE) test
	
clean:
	@cd src/castlm && $(MAKE) clean

nuke:
	rm -r $(BD)

distclean:
	rm -r $(BD)
