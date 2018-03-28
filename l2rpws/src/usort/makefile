#
# Cassini telemetry PCRTIU
#
TOP=/usr/cassini
BIN=$(TOP)/bin
LIB=$(TOP)/lib
INC=$(TOP)/include
X11=/usr/include/Motif1.2
CFLAGSx=-Ae -s  -I$(INC) -I$(X11)
CFLAGS=-Ae  -g -I$(INC) -I$(X11)

OBJS =        usort.o

.c:           cc $(CFLAGS) -c -g $*.c

lcl:          $(OBJS)
		cc -o usort usort.o /usr/lib/end.o -L $(LIB) -l util
lib:          $(OBJS)
		cc -o $(BIN)/usort usort.o -L $(LIB) -l util

clean:
		rm *.o
