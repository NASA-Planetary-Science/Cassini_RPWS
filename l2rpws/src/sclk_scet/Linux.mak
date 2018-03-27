##############################################################################
# Generics

# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

TOP=$(realpath ../../)
ARCH=$(shell uname -s).$(shell uname -p)
BUILD:=build.$(ARCH)


INC=$(TOP)/include
LIB=$(TOP)/$(BUILD)
BIN=$(TOP)/$(BUILD)
OBJ_DIR=$(TOP)/$(BUILD)/obj

##############################################################################
# Specific Defs

#CFLAGSx=-Ae -s  -I$(INC) -I$(X11)
#CFLAGS=-Ae -g  -I$(INC)

CFLAGS=-g -I$(INC)

OBJS = file_58.o

#CC = gcc -ansi -pedantic 

GCCOPT = -Wall -Wshadow -Wpointer-arith -Wcast-qual \
         -Wcast-align -Wwrite-strings -Wconversion -Waggregate-return \
         -Wmissing-prototypes -Wnested-externs -Wimplicit \
         -Wstrict-prototypes -Wredundant-decls


INCL= -I/home/raj/project/include -I /home/raj/project/spice/include

CAS_DIR=/opt/project/cassini
INCL= -I$(CAS_DIR)/include -I$(CAS_DIR)/spice/include

OBJs= CasType.o RecordFile.o CasRecord.o CasMiniPacket.o CasSpice.o \
      CasHfr.o CasHfrMeander.o CasHfrKronos.o CasHfrCal.o CasMfr.o  \
      CasWfdr.o CasLp.o CasWfr.o CasWbr.o CasPds.o CasCmdParse.o 


libCasTlm.a: $(OBJs)
	$(CC) $(GCCOPT) -c $(INCL) CasType.c
	ar r libCasTlm.a $(OBJs) 

.c.o:
	$(CC) $(GCCOPT) -c $(INCL)  $<

clean:
	rm -f *.o 

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
