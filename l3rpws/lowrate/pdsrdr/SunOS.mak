##############################################################################
# Generics

# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

PDSRDR_SCRIPT_PATH=/opt/csw/bin:/usr/bin
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

CC = cc

INC= -I$(INST_INC)

# Add large file support to be compatible with NFS 4
LFS_CFLAGS=$(shell getconf LFS_CFLAGS)

CFLAGS = -xc99 -errwarn $(LFS_CFLAGS) -g $(INC)

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

$(OBJ_DIR)/%.o:%.c
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

$(BUILD_DIR)/rpws_lr_pdsrdr: $(BUILD_OBJS)
	$(CC) $^ $(LFLAGS) -o $@

clean:
	rm $(BUILD_OBJS)
	
distclean:
	rm $(BUILD_DIR)/rpws_lr_pdsrdr $(BUILD_SCRIPTS) $(BUILD_DSDFS) \
 $(BUILD_D2_DSDFS) $(BUILD_OBJS)

test:
	@echo "No unit tests defined for this program"



