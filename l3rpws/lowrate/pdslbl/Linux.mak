##############################################################################
# Architecture specific definitions

CC = gcc

CFLAGS = -g -std=c99 -Wall -Werror -I$(INST_INC) '-DINST_ETC="$(INST_ETC)"'

LFLAGS= -L$(INST_NAT_LIB) -lrpwstlm -lcasephem $(CSPICE_LIB) -lm

##############################################################################
# Patter Rules

$(OBJ_DIR)/%.o:%.c
	$(CC) -c $< $(CFLAGS) -o $@

##############################################################################
# Explicit Rules and dependencies

all:$(BUILD_DIR) $(BUILD_DIR)/rpws_lr_label $(BUILD_DIR)/LRFC_MASTER.LBL

$(BUILD_DIR)/rpws_lr_label: $(OBJ_DIR)/pdslbl.o
	$(CC) $^ $(CFLAGS) $(LFLAGS) -o $@

$(BUILD_DIR)/LRFC_MASTER.LBL:LRFC_MASTER.LBL
	cp -p $< $@
	chmod 664 $@

clean:
	rm -f $(BUILD_DIR)/LRFC_MASTER.LBL $(BUILD_DIR)/rpws_lr_label

test:
	@echo "Unit test not yet defined"

$(BUILD_DIR):
	@if [ ! -d $(BUILD_DIR) ] ; then mkdir $(BUILD_DIR); fi
