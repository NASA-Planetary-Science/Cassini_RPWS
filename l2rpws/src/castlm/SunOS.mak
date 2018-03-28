# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif


##############################################################################
# Specific Defs

# -Xc: Strict conformant ISO C
# -errwarn=%all: Exit the compiler on warnings
# -fd: Report K&R functions


DEFINES= '-DCFG="$(INST_ETC)"'

LFS_CFLAGS=$(shell getconf LFS_CFLAGS)

CFLAGS = -g -xc99 -Xc -errwarn=%all -fd $(DEFINES) -I$(BUILD_INC) \
 -I$(INST_INC) -I$(CSPICE_INC) $(LFS_CFLAGS)

CC = cc

OBJ_TARGS= CasType.o RecordFile.o CasRecord.o CasMiniPacket.o \
      CasHfr.o CasHfrMeander.o CasHfrKronos.o CasHfrCal.o CasMfr.o  \
      CasWfdr.o CasLp.o CasWfr.o CasWbr.o CasPds.o CasCmdParse.o 

##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %, $(BUILD_OBJ)/%, $(OBJ_TARGS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(BUILD_OBJ)/%.o:%.c | $(BUILD_OBJ)
	cc $(CFLAGS) -c $< -o $@
	
##############################################################################
# Explicit Rules and Dependencies

$(BUILD_BIN)/librpwstlm.a: $(BUILD_OBJS)
	ar r $@ $(BUILD_OBJS)

$(BUILD_BIN):
	mkdir -p $(BUILD_BIN)

$(BUILD_OBJ):
	mkdir -p $(BUILD_OBJ)

clean:
	rm $(BUILD_OBJS)
	
test: $(BUILD_BIN)/libCasTlm.a
	@echo "castlm unit test not yet defined!"

CasType.o: CasType.c 
RecordFile.o: RecordFile.c 
CasRecord.o: CasRecord.c 
CasMiniPacket.o: CasMiniPacket.c 
CasSpice.o: CasSpice.c 
CasHfr.o: CasHfr.c 
CasHfrCal.o: CasHfrCal.c 
CasHfrMeander.o: CasHfrMeander.c 
CasHfrKronos.o: CasHfrKronos.c 
CasMfr.o: CasMfr.c 
CasWfdr.o: CasWfdr.c 
CasWfr.o: CasWfr.c 
CasWbr.o: CasWbr.c 
CasPds.o: CasPds.c 
CasCmdParse.o: CasCmdParse.c 
