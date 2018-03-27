#
# Cassini telemetry PCRTIU
#
TOP=/opt/project/cassini
BIN=$(TOP)/bin
LIB=$(TOP)/lib
INC=$(TOP)/include 
X11=/usr/include/Motif1.2
CFLAGS=-Ae  -g -I$(INC) -I$(X11)

OBJS =        mp_sort2.o

.c:           cc $(CFLAGS) -c -g $*.c

lcl:          $(OBJS)
		cc -o mp_sort2 mp_sort2.o  -L $(LIB) -l util

clean:
		rm *.o
