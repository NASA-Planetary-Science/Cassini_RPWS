# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Specific Defs

DEFINES= '-DCFG="$(INST_ETC)"'

LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CFLAGS=-Ae -g $(DEFINES) -I$(BUILD_INC) -I$(INST_INC) $(LFS_CFLAGS)

CC = cc

LFLAGS=-g -L$(BUILD_LIB) -lutil -lfg -lsocket -lnsl


OBJS = biucmd5.o rtiu.o rtiuh5.o sim_bce_rcv.o

STARTER_CFGS = rpws2.trs rtiuh.trs


##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %,$(BUILD_OBJ)/%,$(OBJS))

BULID_CFG=$(patsubst %,$(BUILD_ETC)/%,$(STARTER_CFGS))


##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(BUILD_OBJ)/%.o:%.c
	$(CC) $(CFLAGS) -c $< -o $@
	
$(BUILD_ETC)/%:%
	cp $< $@


##############################################################################

all: $(BUILD_BIN)/biucmd $(BUILD_BIN)/rtiu $(BUILD_BIN)/sim_bce_rcv \
 $(BUILD_ETC) $(BULID_CFG)

$(BUILD_ETC):
	-mkdir -p $(BUILD_ETC)

$(BUILD_BIN)/biucmd:$(BUILD_OBJ)/biucmd5.o $(BUILD_OBJ)/rtiuh5.o
	$(CC) -g $^ $(LFLAGS) -o $@ 


$(BUILD_BIN)/rtiu:$(BUILD_OBJ)/rtiu.o $(BUILD_OBJ)/rtiuh5.o
	$(CC) -g $^ $(LFLAGS) -o $@
	
$(BUILD_BIN)/sim_bce_rcv:$(BUILD_OBJ)/sim_bce_rcv.o
	$(CC) -g $^ $(LFLAGS) -o $@

clean:
	rm $(BUILD_OBJS) $(BULID_CFG)
