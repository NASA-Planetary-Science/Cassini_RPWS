# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Specific Defs

CC = cc

# Add large file support to be compatible with NFS 4
LFS_CFLAGS=$(shell getconf LFS_CFLAGS)

CFLAGS = -xc99 -g $(LFS_CFLAGS) -I$(INST_INC)

LFLAGS= -L$(INST_NAT_LIB) -lrpwstlm -lcasephem -ldas2 $(CSPICE_LIB) -lm 


##############################################################################
# Explicit Rules and dependencies

$(BUILD_DIR)/rpws_lr_hfrcp:hfrcp.c
	$(CC) $< $(CFLAGS) $(LFLAGS) -o $@

test:
	@echo "Unit test not yet defined"
	
clean:
	rm $(BUILD_DIR)/rpws_lr_hfrcp
