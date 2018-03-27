#
# Cassini telemetry PCRTIU
#
TOP=/usr/cassini
LIB=$(TOP)/lib
END=/usr/lib/end.o
BIN=$(TOP)/bin
INC=$(TOP)/include
CFLAGS=-Ae -s -I$(INC)

OBJS =        gett.o

.c:           cc $(CFLAGS) -c -g $*.c

lcl:          $(OBJS)
		cc -o gett $(OBJS) -L$(LIB) -lutil
lib:          $(OBJS)
		cc -o $(BIN)/gett $(OBJS) -L$(LIB) -lutil
		
clean:
		rm *.o
