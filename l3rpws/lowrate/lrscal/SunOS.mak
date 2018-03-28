
# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif


##############################################################################
# Platform Defs

CC = cc -xc99=all -errwarn

INC= -I$(INST_INC)
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) # Add large file support
CFLAGS = -xs -Ae -xc99 -g $(INC) $(LFS_CFLAGS)

LFLAGS= -L$(INST_NAT_LIB) -lrpwstlm -lcasephem -ldas2 $(CSPICE_LIB) -lm 


##############################################################################
# Explicit Rules and dependencies

$(BUILD_DIR)/rpws_lr_cal:lrscal.c lrscal.h
	$(CC) $< $(CFLAGS) $(LFLAGS) -o $@

test:
	@echo "Unit test not yet defined"
	
clean:
	rm $(BUILD_DIR)/rpws_lr_cal
