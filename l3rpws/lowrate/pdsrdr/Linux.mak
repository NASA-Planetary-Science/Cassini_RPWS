##############################################################################
# Generics

PDSRDR_SCRIPT_PATH=/usr/bin
export PDSRDR_SCRIPT_PATH

##############################################################################
# Files

# Don't know why casdfr.c is not included here
SRCS=pdsrdr.c pdshist.c

SCRIPTS=rpws_lr_pdsrdr.sh.in rpws_lr_das2rdr.sh.in

DSDFS=rpws_pds.dsdf.in

D2_DSDFS=LowRateScience.dsdf.in

##############################################################################
# Specific Defs

CC = gcc

CFLAGS = -g -std=c99 -Wall -Werror $(LFS_CFLAGS) -I$(INST_INC)

LFLAGS= -L$(INST_NAT_LIB) -lrpwstlm -lcasephem -ldas2 $(CSPICE_LIB) -lm 

##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %.c,$(OBJ_DIR)/%.o,$(SRCS))
BUILD_SCRIPTS=$(patsubst %.sh.in,$(BUILD_DIR)/%.sh,$(SCRIPTS))
BUILD_DSDFS=$(patsubst %.dsdf.in,$(BUILD_DIR)/%.dsdf,$(DSDFS))

BUILD_D2_DSDFS=$(patsubst %.dsdf.in,$(BUILD_DIR)/%.dsdf,$(D2_DSDFS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(OBJ_DIR)/%.o:%.c  | $(OBJ_DIR)
	$(CC) $(CFLAGS) -c $< -o $@
	
$(BUILD_DIR)/%.dsdf:%.dsdf.in | $(BUILD_DIR)
	./envsubst.py $< $@
	chmod +x $@

$(BUILD_DIR)/%.sh:%.sh.in | $(BUILD_DIR)
	./envsubst.py $< $@
	chmod +x $@

##############################################################################
# Explicit Rules and dependencies

build: $(BUILD_DIR)/rpws_lr_pdsrdr $(BUILD_SCRIPTS) $(BUILD_DSDFS) \
 $(BUILD_D2_DSDFS)


$(BUILD_DIR):
	@if [ ! -d $(BUILD_DIR) ] ; then mkdir $(BUILD_DIR); fi
	
$(OBJ_DIR):
	@if [ ! -d $(OBJ_DIR) ] ; then mkdir -p $(OBJ_DIR); fi

$(BUILD_DIR)/rpws_lr_pdsrdr: $(BUILD_OBJS)
	$(CC) $^ $(LFLAGS) -o $@	

clean:
	rm $(BUILD_OBJS)
	
distclean:
	rm $(BUILD_DIR)/rpws_lr_pdsrdr $(BUILD_SCRIPTS) $(BUILD_DSDFS) \
 $(BUILD_D2_DSDFS) $(BUILD_OBJS)

test:
	@echo "No unit tests defined for this program"



