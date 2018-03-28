# Export locations needed by the setup.py script 

export INST_ETC
export INST_NAT_BIN

##############################################################################
# Platform Variables

CC= gcc 
CFLAGS= -Wall -Werror -std=c99 -g -I$(INST_INC) '-DMAG_DATA=$(MAG_DATA)'
LFLAGS= -L$(INST_NAT_LIB) -ldas2rdr -ldas2 -lexpat -largtable2 -lm

DC= dmd -m64 -g 
DLIBS= $(INST_NAT_LIB)/libdas2.a


# Pattern Rules ##############################################################

# DSDF's
$(BD)/%.dsdf:etc/%.dsdf.in
	./envsubst.py $< $@   

# Object Files
$(BD)/%.o:src/%.c  | $(BD)
	$(CC) $(CFLAGS) -c $< -o $@

# Binary program install
$(INST_NAT_BIN)/%:$(BD)/%
	install -D -m 775 $< $@

$(INST_DAS1_DSDF)/mag/%.dsdf:$(BD)/%.dsdf
	install -D -m 664 $< $@

##############################################################################
# Explicit Rules

build: $(BD)/mag_l4pds_rdr $(BD)/mag_vector_rdr_c $(BD)/mag_fce_rdr_c \
 $(BD)/fce.dsdf $(BD)/mag_sync2.sh build_pylib 

$(BD):
	@if [ ! -e "$(BD)" ]; then mkdir $(BD); fi

$(BD)/mag_l4pds_rdr:src/mag_l4pds_rdr.d src/mag.d src/dasmini.d
	# DMD is soooo brain-dead when it comes to out of source tree builds
	cd src && $(DC) -od=../$(BD) -of=../$(BD)/mag_l4pds_rdr mag_l4pds_rdr.d mag.d dasmini.d $(DLIBS)

$(BD)/mag_vector_rdr_c:$(BD)/cassini_mag.o
	$(CC) $(CFLAGS) -o $@ $< $(LFLAGS)

$(BD)/mag_fce_rdr_c:$(BD)/cassini_fce.o
	$(CC) $(CFLAGS) -o $@ $< $(LFLAGS)

$(BD)/mag_sync2.sh:scripts/mag_sync2.sh.$(UNAME).in
	./envsubst.py $< $@

build_pylib:
	python$(PYVER) setup.py build -b $(BD)

install: $(INST_NAT_BIN)/mag_l4pds_rdr $(INST_NAT_BIN)/mag_vector_rdr_c \
  $(INST_NAT_BIN)/mag_fce_rdr_c $(INST_NAT_BIN)/mag_sync2.sh \
  $(INST_DAS1_DSDF)/mag/fce.dsdf install_pylib  

install_pylib:
	python$(PYVER) setup.py install_scripts --skip-build -b $(BD)/scripts-$(PYVER) -d $(INST_HOST_BIN)
	python$(PYVER) setup.py install_lib --skip-build -b $(BD)/lib* -d $(INST_HOST_LIB)
		  
clean:
	rm -r -f $(BD) build
	
distclean:
	rm -r -f $(BD) build

test:
	echo "Unit tests have not been defined"

