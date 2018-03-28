#
# Cassini telemetry PCRTIU
#
TOP=/opt/project/cassini
LIB=$(TOP)/lib
BIN=$(TOP)/bin
LBIN=$(TOP)/src/mcmd
INC=$(TOP)/include
CFLAGS=-Ae -g -I$(INC) -D__SUN__

MAINS =       mcmd.o mcmd2.o mcmd3.o mtst.o
OBJS =        mcmd_delay.o mcmd_gpib.o mcmd_gpib_1.o mcmd_misc.o
OBJSx =       mcmd_delay.o mcmd_gpib.o mcmd_misc.o
MCMD =        mcmd_sub.o mcmd_gpib.o

.c:           cc $(CFLAGS) -c -g $*.c

mtst:          mtst.o $(MCMD) $(LIB)/libmcmd.a
		cc -o mtst mtst.o $(LIB)/gpib.o -L$(LIB) -lutil -lmcmd
lcl:          $(OBJS) $(MAINS)
		cc -o $(LBIN)/mcmd0  mcmd.o  $(OBJSx) $(LIB)/gpib.o -L$(LIB) -lutil
		cc -o $(LBIN)/mcmd2 mcmd2.o $(OBJSx) $(LIB)/gpib.o -L$(LIB) -lutil
		cc -o $(LBIN)/mcmd3 mcmd3.o $(OBJSx) $(LIB)/gpib.o -L$(LIB) -lutil
lib:          $(OBJS) $(MAINS)
		cc -o $(BIN)/mcmd0  mcmd.o  $(OBJSx) $(LIB)/gpib.o -L$(LIB) -lutil
		cc -o $(BIN)/mcmd2  mcmd2.o $(OBJSx) $(LIB)/gpib.o -L$(LIB) -lutil
		cc -o $(BIN)/mcmd3  mcmd3.o $(OBJSx) $(LIB)/gpib.o -L$(LIB) -lutil
ar:           $(MCMD)
		ar r $(LIB)/libmcmd.a \
			$(MCMD)
		cc -o mtst mtst.o -L$(LIB) -lmcmd
		cp mcmd.h /usr/cassini/include
		
mtst.o:          mtst.c mcmd.h

clean:
		rm *.o
