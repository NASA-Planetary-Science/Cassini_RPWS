##############################################################################
# Platform Defs

CC = gcc -std=c99 -Wall -Werror

CFLAGS = -I$(INST_INC)
LFLAGS= -L$(INST_NAT_LIB) -lrpwstlm -lcasephem -ldas2 $(CSPICE_LIB) -lm 

##############################################################################
# Explicit Rules and dependencies

$(BUILD_DIR)/rpws_lr_mark:lrsmark.c
	$(CC) $< $(CFLAGS) $(LFLAGS) -o $@

test:
	@echo "Unit test not yet defined"


clean:
	rm $(BUILD_DIR)/rpws_lr_mark

