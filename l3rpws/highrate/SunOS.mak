# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

export INST_NAT_BIN
export INST_ETC
export RPWS_TEMP
export SETUP_FILE

# Used to affect giferator scripts
export INST_EXT_LIB

# ASSUME (always dangerous) that the giferator.sav can be found 
# as the first item on the IDL path.
GIFER_PATH=$(firstword $(subst :, ,$(IDL_PATH)) )
export GIFER_PATH

##############################################################################
# Stuff to build/install

SCRIPT_SRCS=rpws_hr_make.sh.in rpws_hr_quicklook.sh.in rpws_hr_build_lbl.ksh.in \
 rpws_hr_html_lbl.pl.in rpws_hr_static_html.ksh.in rpws_hr_prune.pl.in \
 rpws_hr_browse_wbr_HFR.pl.in rpws_hr_browse_wbr_10Khz.pl.in \
 rpws_hr_browse_wbr_75Khz.pl.in rpws_hr_browse_wfr_25Hz.pl.in \
 rpws_hr_browse_wfr_2_5Khz.pl.in rpws_hr_thumb.sh.in rpws_l3wbr_spec_rdr.sh.in \
 rpws_l3wfr_spec_rdr.sh.in rpws_l3hfr_spec_rdr.sh.in
 
PY_SCRIPTS=rpws_l3wbr_avail rpws_l3wbr_das2rdr rpws_l3wbr_print \
 rpws_l3wfr_das2rdr rpws_l3wfr_print 
 
# Note, these are only installed if a corresponding file is not already in
# the config directory.  Use the special target gmake config
STARTER_CFGS= \
 vol_template/BROWSE/BROWSE.HTM \
 vol_template/BROWSE/BROWSE.LBL \
 vol_template/BROWSE/ANCILLARY/ICONS.LBL \
 vol_template/BROWSE/ANCILLARY/KEY.PNG  \
 vol_template/BROWSE/ANCILLARY/LRFC.PNG \
 vol_template/BROWSE/ANCILLARY/VALID_HTML32.PNG \
 vol_template/BROWSE/ANCILLARY/VALID_HTML.LBL \
 vol_template/BROWSE/ANCILLARY/WBR.PNG \
 vol_template/BROWSE/ANCILLARY/WFR.PNG \
 vol_template/BROWSE/RPWS_WAVEFORM_FULL/BROWSE.HTM \
 vol_template/BROWSE/RPWS_WAVEFORM_FULL/BROWSE.LBL \
 vol_template/BROWSE/RPWS_WIDEBAND_FULL/BROWSE.HTM \
 vol_template/BROWSE/RPWS_WIDEBAND_FULL/BROWSE.LBL \
 nodata.png noplot.png
 
C_SRCS= rpws_hrpds_list.c


##############################################################################
# Building C sources and binaries

CC = cc

DEFINES= '-DCAS_TIME_KERNELS="$(CAS_TIME_KERNELS)"'

LFS_CFLAGS=$(shell getconf LFS_CFLAGS) # Add large file support

CFLAGS=-Ae -g $(DEFINES) -I$(CSPICE_INC) $(LFS_CFLAGS)


##############################################################################
# Automatic Variables

BUILD_SCRIPTS = $(patsubst %.in, $(BUILD_DIR)/%, $(SCRIPT_SRCS))
BUILD_OBJS = $(patsubst %.c, $(BUILD_DIR)/%.o, $(C_SRCS))

BUILD_TARGS = $(BUILD_SCRIPTS) $(BUILD_OBJS)


inst_scripts_ex = $(patsubst $(BUILD_DIR)/%, $(INST_NAT_BIN)/%, $(BUILD_SCRIPTS))
INST_SCRIPTS = $(basename $(inst_scripts_ex))

INST_TARGS = $(INST_SCRIPTS) 

INST_CFG = $(patsubst %, $(INST_ETC)/%, $(STARTER_CFGS))

##############################################################################
# Pattern Rules

$(BUILD_DIR)/%.sh:scripts/%.sh.in
	./envsubst.py $< $@
	chmod +x $@

$(BUILD_DIR)/%.ksh:scripts/%.ksh.in
	./envsubst.py $< $@
	chmod +x $@

$(BUILD_DIR)/%.pl:scripts/%.pl.in
	./envsubst.py $< $@
	chmod +x $@

$(INST_NAT_BIN)/%:$(BUILD_DIR)/%.sh
	install -m 775 $< $@

$(INST_NAT_BIN)/%:$(BUILD_DIR)/%.ksh
	install -m 775 $< $@

$(INST_NAT_BIN)/%:$(BUILD_DIR)/%.pl
	install -m 775 $< $@
	
$(INST_ETC)/%:etc/%
	@if [ ! -f $@ ] ; then \
	  install -D -m 644 $< $@ ; echo "Starter Config -> $@"; \
	else \
	  echo "Old config retained: $@" ; \
	fi

$(BUILD_DIR)/%.o:src/%.c
	$(CC) $(CFLAGS) -c $< -o $@

##############################################################################
# Explicit rules

build: $(BUILD_DIR) $(BUILD_TARGS) $(BUILD_DIR)/rpws_hrpds_list

py_build:
	python$(PYVER) setup.py build -b $(BUILD_DIR)


install: $(INST_TARGS) $(INST_CFG) $(INST_NAT_BIN)/rpws_hrpds_list py_install

py_install:
	python$(PYVER) setup.py install_lib --skip-build -b $(BUILD_DIR)/lib -d $(INST_PY_LIB)
	python$(PYVER) setup.py install_scripts --skip-build -b $(BUILD_DIR)/scripts-$(PYVER) -d $(INST_NAT_BIN)

$(INST_NAT_BIN)/rpws_hrpds_list:$(BUILD_DIR)/rpws_hrpds_list
	install -m 775 $< $@

$(BUILD_DIR)/rpws_hrpds_list:$(BUILD_DIR)/rpws_hrpds_list.o
	$(CC) -g -o $@ $< $(CSPICE_LIB) -lm

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

distclean:
	rm -r -f $(BUILD_DIR)
	
clean:
	rm $(BUILD_OBJS)
	
