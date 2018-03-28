#############################################################################
# Use GNU make 

ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Architecture Specific Info

CC=cc 

LFS_CFLAGS=$(shell getconf LFS_CFLAGS) # Add large file support

CFLAGS=-xc99 -errwarn '-DINST_ETC="$(INST_ETC)"' -I$(INST_INC) \
 -I$(CSPICE_INC) $(LFS_CFLAGS)

LFLAGS=-L$(INST_NAT_LIB) -lcasephem -ldas2 $(CSPICE_LIB) -lm

##############################################################################
# Explicit targets

build: $(BUILD_DIR) $(BUILD_DIR)/rpws_kp_label

$(BUILD_DIR):
	mkdir -p $@

# NOTE:  Build/Install ../l6epmem first, libcasephem.a comes from there

$(BUILD_DIR)/rpws_kp_label:rpws_l4kp_label.c
	$(CC) $< $(CFLAGS) $(LFLAGS) -o $@
	
test:
	@echo "Unit test not yet defined"
	
install:$(INST_NAT_BIN)/rpws_kp_label $(INST_ETC)/pds/KEY_MASTER.LBL

$(INST_NAT_BIN)/rpws_kp_label:$(BUILD_DIR)/rpws_kp_label
	install -D -m 775 $< $@

$(INST_ETC)/pds/KEY_MASTER.LBL:KEY_MASTER.LBL
	install -D -m 664 $< $@
	
clean:
	rm $(BUILD_DIR)/rpws_kp_label $(BUILD_DIR)/KEY_MASTER.LBL

distclean:
	-rm -r $(BUILD_DIR)

