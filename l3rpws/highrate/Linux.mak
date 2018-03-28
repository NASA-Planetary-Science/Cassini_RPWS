BD=build.$(N_ARCH)

export INST_NAT_BIN
export INST_ETC
export RPWS_TEMP

export INST_EXT_LIB

# ASSUME (always dangerous) that the giferator.sav can be found 
# as the first item on the IDL path.
GIFER_PATH=$(firstword $(subst :, ,$(IDL_PATH)) )
export GIFER_PATH

##############################################################################
# Stuff to build/install

# Most of these rely ultimatly on L2 data readers and so maybe they should be
# in that area, I don't know.   Anyway, they don't work on linux as is
#SCRIPTS=rpws_hr_make.sh.in rpws_hr_build_lbl.ksh.in rpws_hr_html_lbl.pl.in \
# rpws_hr_static_html.ksh.in rpws_hr_prune.pl.in rpws_hr_browse_wbr_HFR.pl.in \
# rpws_hr_browse_wbr_10Khz.pl.in rpws_hr_browse_wbr_75Khz.pl.in \
# rpws_hr_browse_wfr_25Hz.pl.in rpws_hr_browse_wfr_2_5Khz.pl.in \
# rpws_hr_thumb.sh.in rpws_l3wbr_spec_rdr.sh.in

SCRIPTS=rpws_l3wbr_spec_rdr.sh.in rpws_l3hfr_spec_rdr.sh.in \
  rpws_l3wfr_spec_rdr.sh.in

 
PY_SCRIPTS=rpws_l3wbr_avail rpws_l3wbr_das2rdr rpws_l3wbr_print \
 rpws_l3wfr_das2rdr rpws_l3wfr_print 
 
# Note, these are used by the programs above that don't work on linux and
# so are not installed
#STARTER_CFGS= \
# vol_template/BROWSE/BROWSE.HTM \
# vol_template/BROWSE/BROWSE.LBL \
# vol_template/BROWSE/ANCILLARY/ICONS.LBL \
# vol_template/BROWSE/ANCILLARY/KEY.PNG  \
# vol_template/BROWSE/ANCILLARY/LRFC.PNG \
# vol_template/BROWSE/ANCILLARY/VALID_HTML32.PNG \
# vol_template/BROWSE/ANCILLARY/VALID_HTML.LBL \
# vol_template/BROWSE/ANCILLARY/WBR.PNG \
# vol_template/BROWSE/ANCILLARY/WFR.PNG \
# vol_template/BROWSE/RPWS_WAVEFORM_FULL/BROWSE.HTM \
# vol_template/BROWSE/RPWS_WAVEFORM_FULL/BROWSE.LBL \
# vol_template/BROWSE/RPWS_WIDEBAND_FULL/BROWSE.HTM \
# vol_template/BROWSE/RPWS_WIDEBAND_FULL/BROWSE.LBL \
# nodata.png noplot.png
 
C_SRCS= rpws_hrpds_list.c


##############################################################################
# Building C sources and binaries

CC = gcc

DEFINES= '-DCAS_TIME_KERNELS="$(CAS_TIME_KERNELS)"'

CFLAGS=-Wall -ggdb $(DEFINES) -I$(CSPICE_INC)

DC = dmd
DFLAGS = 
DLIBS = $(INST_NAT_LIB)/libdas2.a -L-lfftw3 
SPICE_LIB=/usr/local/lib/libcspice.a


##############################################################################
# Automatic Variables

SCRIPT_SRCS = $(patsubst %, scripts/%, $(SCRIPTS))
BUILD_SCRIPTS = $(patsubst %.in, $(BD)/%, $(SCRIPTS))

BUILD_OBJS = $(patsubst %.c, $(BD)/%.o, $(C_SRCS))

INST_SCRIPTS = $(patsubst %.in, $(INST_NAT_BIN)/%, $(SCRIPTS)) 

PY_INST_SCRIPTS = $(patsubst %, $(INST_NAT_BIN)/%, $(PY_SCRIPTS))

INST_CFG = $(patsubst %, $(INST_ETC)/%, $(STARTER_CFGS))

##############################################################################
# Pattern Rules

$(BD)/%.sh:scripts/%.sh.in
	./envsubst.py $< $@
	chmod +x $@

$(INST_NAT_BIN)/%:$(BD)/%
	install -m 775 $< $@

$(BD)/%.o:src/%.c
	$(CC) $(CFLAGS) -c $< -o $@

##############################################################################
# Explicit rules

build: $(BD) $(BD)/rpws_hrpds_list $(BUILD_SCRIPTS) py_build \
      $(BD)/rpws_l3wbr_das2rdr

$(BD):
	mkdir -p $(BD)

$(BD)/rpws_hrpds_list:$(BD)/rpws_hrpds_list.o
	$(CC) -g -o $@ $< $(CSPICE_LIB) -lm

py_build:
	python$(PYVER) setup.py build -b $(BD)
	
$(BD)/rpws_l3wbr_das2rdr:src/rpws_l3wbr_das2rdr.d src/wbr.d src/dasmini.d | $(BD)
	$(DC) -O  -od=$(BD) -of=$@ $^ $(DLIBS) $(SPICE_LIB)


install: $(INST_SCRIPTS) $(INST_CFG) $(INST_NAT_BIN)/rpws_hrpds_list py_install \
      $(INST_NAT_BIN)/rpws_l3wbr_das2rdr
	
py_install:
	python$(PYVER) setup.py install_lib --skip-build -b $(BD)/lib -d $(INST_PY_LIB)
	python$(PYVER) setup.py install_scripts --skip-build -b $(BD)/scripts-$(PYVER) -d $(INST_NAT_BIN)


$(INST_BIN)/rpws_hrpds_list:$(BD)/rpws_hrpds_list
	install -m 775 $< $@





distclean:
	rm -r -f $(BD)
	
clean:
	rm $(BUILD_OBJS)
	
