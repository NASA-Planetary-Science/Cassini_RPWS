##############################################################################
# Specific Defs

CC = gcc

CFLAGS = -g -std=c99 -Wall -Werror -I$(INST_INC)

LFLAGS= -L$(INST_NAT_LIB) -lrpwstlm -lcasephem -ldas2 $(CSPICE_LIB) -lm 


##############################################################################
# Explicit Rules and dependencies

$(BUILD_DIR)/rpws_lr_hfrcp:hfrcp.c
	$(CC) $< $(CFLAGS) $(LFLAGS) -o $@

test:
	@echo "Unit test not yet defined"
	
clean:
	rm $(BUILD_DIR)/rpws_lr_hfrcp
