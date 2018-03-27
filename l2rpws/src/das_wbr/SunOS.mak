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
DEFINES  = -DRPWS_MPDB=$(RPWS_MPDB) \
 -DRPWS_HFR_CALDIR=$(PREFIX)/cal/version1.0/hfr

CFLAGS = -xc99 -g $(LFS_CFLAGS) #-errwarn=%all

FC = f90 -f77 -ftrap=%none
FFLAGS = -g

LFLAGS = -L $(LIB_DIR) -lutil -L$(INST_NAT_LIB) -lfftpack -ldas2 -lfg -lm

OBJS = wbrgain.o new10k_V4.o new80k_V4.o new80k_V5.o file_list.o find_mfr.o\
 window.o hfr_msagc.o fft.o wbr80hfb.c hfr_cal.c

DSDFS =RPWShfrmsagc.dsdf RPWSwb10_V4.dsdf RPWSwb80_HF.dsdf \
 RPWSwb80_V4.dsdf RPWSwbgain.dsdf

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

all: $(BIN_DIR)/rpws_l2wbr_gainrdr $(BIN_DIR)/rpws_l2wbr_80rdr \
$(BIN_DIR)/rpws_l2wbr_80jamitrdr $(BIN_DIR)/rpws_l2wbr_80hfrdr \
$(BIN_DIR)/rpws_l2wbr_10rdr $(BIN_DIR)/rpws_l2hfr_gainrdr \
$(BIN_DIR)/rpws_l2wbr_waveform $(BUILD_DSDFS)

$(OBJ_DIR):
	-mkdir -p $(OBJ_DIR)

$(BIN_DIR)/rpws_l2wbr_gainrdr:$(OBJ_DIR)/wbrgain.o $(OBJ_DIR)/file_list.o
	$(CC) $^ $(LFLAGS) -o $@ 

$(BIN_DIR)/rpws_l2wbr_80rdr:$(OBJ_DIR)/new80k_V4.o $(OBJ_DIR)/window.o \
  $(OBJ_DIR)/file_list.o $(OBJ_DIR)/find_mfr.o
	$(CC) $^ $(LFLAGS) -o $@ 

$(BIN_DIR)/rpws_l2wbr_80jamitrdr:$(OBJ_DIR)/new80k_V5.o $(OBJ_DIR)/window.o \
  $(OBJ_DIR)/file_list.o $(OBJ_DIR)/find_mfr.o
	$(CC) $^ $(LFLAGS) -o $@ 

$(BIN_DIR)/rpws_l2wbr_10rdr:$(OBJ_DIR)/new10k_V4.o $(OBJ_DIR)/window.o \
  $(OBJ_DIR)/file_list.o $(OBJ_DIR)/find_mfr.o
	$(CC) $^ $(LFLAGS) -o $@ 

$(BIN_DIR)/rpws_l2hfr_gainrdr:$(OBJ_DIR)/hfr_msagc.o $(OBJ_DIR)/hfr_cal.o \
 $(OBJ_DIR)/file_list.o
	$(CC) $^ $(LFLAGS) -o $@ 

$(BIN_DIR)/rpws_l2wbr_80hfrdr:$(OBJ_DIR)/wbr80hfb.o $(OBJ_DIR)/hfr_cal.o \
  $(OBJ_DIR)/file_list.o $(OBJ_DIR)/window.o
	$(CC) $^ $(LFLAGS) -o $@ 

$(BIN_DIR)/rpws_l2wbr_waveform:$(OBJ_DIR)/rpws_l2wbr_waveform.o \
  $(OBJ_DIR)/hfr_cal.o $(OBJ_DIR)/file_list.o $(OBJ_DIR)/find_mfr.o
	$(CC) $^ $(LFLAGS) -o $@

clean:
	rm -f $(BUILD_OBJS) $(BUILD_DSDFS)

test:
	@echo "No unit test defined for Das WBR readers"
