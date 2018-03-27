# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

export INST_ETC

##############################################################################
# Platform Defs

CC = cc 

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS)
INCLUDES = -I$(INC_DIR) -I$(INST_INC) 
DEFINES  = -DRPWS_MPDB=$(RPWS_MPDB)

CFLAGS = -xc99 -g $(LFS_CFLAGS) #-errwarn=%all

FC = f90 -f77 -ftrap=%none
FFLAGS = -g

LFLAGS = -L $(LIB_DIR) -lutil -L$(INST_NAT_LIB) -ldas2 -lm

# General objects used by both programs
OBJS = fft.o window.o file_list.o

DSDFS = RPWShfwr0.dsdf RPWShfwr1.dsdf RPWShfwr2.dsdf RPWShfwr3.dsdf \
 RPWShfwr4.dsdf RPWSlfwr0.dsdf RPWSlfwr1.dsdf RPWSlfwr2.dsdf \
 RPWSlfwr3.dsdf RPWSlfwr4.dsdf

##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %,$(OBJ_DIR)/%,$(OBJS))

BUILD_DSDFS=$(patsubst %,$(ETC_DIR)/%,$(DSDFS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(OBJ_DIR)/%.o:%.c | $(OBJ_DIR)
	$(CC) $(CFLAGS) $(DEFINES) $(INCLUDES) -c $< -o $@

$(ETC_DIR)/%:%.in | $(ETC_DIR)
	./envsubst.py $< $@

##############################################################################
# Explicit Rules and dependencies

all: $(BIN_DIR)/rpws_l2wfr_hirdr $(BIN_DIR)/rpws_l2wfr_lordr \
 $(BIN_DIR)/rpws_l2wfr_waveform_lordr $(BIN_DIR)/rpws_l2wfr_waveform_hirdr \
 $(BUILD_DSDFS)

$(OBJ_DIR):
	-mkdir -p $(OBJ_DIR)

$(BIN_DIR)/rpws_l2wfr_hirdr:$(OBJ_DIR)/hfwr.o $(BUILD_OBJS)	
	$(CC) $^ $(LFLAGS) -o $@ 

$(BIN_DIR)/rpws_l2wfr_lordr:$(OBJ_DIR)/lfwr.o $(BUILD_OBJS)
	$(CC) $^ $(LFLAGS) -o $@ 

$(BIN_DIR)/rpws_l2wfr_waveform_lordr:$(OBJ_DIR)/lfwr_waveform.o $(BUILD_OBJS)
	$(CC) $^ $(LFLAGS) -o $@

$(BIN_DIR)/rpws_l2wfr_waveform_hirdr:$(OBJ_DIR)/hfwr_waveform.o $(BUILD_OBJS)
	$(CC) $^ $(LFLAGS) -o $@

clean:
	rm -f $(BUILD_OBJS) $(OBJ_DIR)/hfwr.o $(OBJ_DIR)/lfwr.o $(BUILD_DSDFS)

test:
	@echo "No unit test defined for Das WFR readers"
