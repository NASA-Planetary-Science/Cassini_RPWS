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

OBJS =      stimf.o

.c:           cc $(CFLAGS) -c -g $*.c

lcl:	      $(OBJS)
		cc -o stimf stimf.o  $(END) -L $(LIB) -l util
lib:          $(OBJS)
		cc -o $(BIN)/stimf stimf.o  -L $(LIB) -l util
clean:
		rm *.o
