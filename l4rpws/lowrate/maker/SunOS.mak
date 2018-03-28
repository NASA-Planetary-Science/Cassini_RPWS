#############################################################################
# Use GNU make 

ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif


##############################################################################
# Architecture Specific Info

CC=cc 

LFS_CFLAGS=$(shell getconf LFS_CFLAGS) # Add large file support

CFLAGS=-xc99 -errwarn -I$(INST_INC) $(LFS_CFLAGS)

LFLAGS=-L$(INST_NAT_LIB) -ldas2 -lm

##############################################################################
# Explicit targets

build: $(BUILD_DIR) $(BUILD_DIR)/rpws_kp_make

$(BUILD_DIR):
	mkdir -p $@

$(BUILD_DIR)/rpws_kp_make:rpws_l4kp_make.c
	$(CC) $< $(CFLAGS) $(LFLAGS) -o $@

test:
	@echo "Unit test not yet defined"
	
install:$(INST_NAT_BIN)/rpws_kp_make

$(INST_NAT_BIN)/rpws_kp_make:$(BUILD_DIR)/rpws_kp_make
	install -D -m 775 $< $@
	
clean:
	rm $(BUILD_DIR)/rpws_kp_make

distclean:
	-rm -r $(BUILD_DIR)

