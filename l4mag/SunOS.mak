# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

# Export locations needed by the setup.py script 

export INST_ETC
export INST_NAT_BIN

##############################################################################
# Platform Variables

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CC= cc 

CFLAGS= -errwarn=%all -xc99 -g -I/opt/csw/include -I$(INST_INC) \
 '-DMAG_DATA=$(MAG_DATA)' $(LFS_CFLAGS)
 
LFLAGS= -L$(INST_NAT_LIB) -ldas2 -L/opt/csw/lib -lexpat -largtable2 -lm

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

build: $(BD)/mag_vector_rdr_c $(BD)/mag_fce_rdr_c $(BD)/fce.dsdf \
 $(BD)/mag_sync2.sh build_pylib 

$(BD):
	@if [ ! -d "$(BD)" ]; then mkdir $(BD); fi

$(BD)/mag_vector_rdr_c:$(BD)/cassini_mag.o
	$(CC) $(CFLAGS) -o $@ $< $(LFLAGS)

$(BD)/mag_fce_rdr_c:$(BD)/cassini_fce.o
	$(CC) $(CFLAGS) -o $@ $< $(LFLAGS)

$(BD)/mag_sync2.sh:scripts/mag_sync2.sh.$(UNAME).in
	./envsubst.py $< $@

build_pylib:
	python$(PYVER) setup.py build -b $(BD)

install: $(INST_NAT_BIN)/mag_vector_rdr_c $(INST_NAT_BIN)/mag_fce_rdr_c \
  $(INST_NAT_BIN)/mag_sync2.sh $(INST_DAS1_DSDF)/mag/fce.dsdf install_pylib  

install_pylib:
	python$(PYVER) setup.py install_scripts --skip-build -b $(BD)/scripts-$(PYVER) -d $(INST_HOST_BIN)
	python$(PYVER) setup.py install_lib --skip-build -b $(BD)/lib* -d $(INST_HOST_LIB)
		  
clean:
	rm -r -f $(BD) build
	
distclean:
	rm -r -f $(BD) build

test:
	echo "Unit tests have not been defined"

