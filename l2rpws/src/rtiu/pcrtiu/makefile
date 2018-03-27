#
# Cassini telemetry PCRTIU
#
TOP=/usr/cassini
END=/usr/lib/end.o
BIN=$(TOP)/bin
LIB=$(TOP)/lib
INC=$(TOP)/include 
X11=/usr/include/Motif1.2
CFLAGSx=-Ae -s  -I$(INC) -I$(X11)
CFLAGS=-Ae  -g -I$(INC) -I$(X11)

OBJS =      pcpipe.o pcrtiua.o

.c:           cc $(CFLAGS) -c -g $*.c

lcl:	      $(OBJS)
		cc -o rtiupipe pcpipe.o pcrtiua.o -L $(LIB) -l util
lib:          $(OBJS)
		cc -o $(BIN)/rtiupipe pcpipe.o pcrtiua.o -L $(LIB) -l util
clean:
		rm *.o
