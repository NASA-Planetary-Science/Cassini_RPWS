#
# Cassini telemetry PCRTIU
#
TOP=/usr/cassini
LIB=$(TOP)/lib
END=/usr/lib/end.o
BIN=$(TOP)/bin
BIN=$(TOP)/src/mcmd
INC=$(TOP)/include
CFLAGS=-Ae -s -I$(INC)

OBJS =        mon.o

.c:           cc $(CFLAGS) -c -g $*.c

lib:          $(OBJS)
		cc -o mon $(OBJS) $(LIB)/gpib.o -L$(LIB) -lutil
		
clean:
		rm *.o
