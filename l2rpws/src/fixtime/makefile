#
# Cassini telemetry PCRTIU
#
TOP=/opt/project/cassini
BIN=$(TOP)/bin
LIB=$(TOP)/lib
INC=$(TOP)/include
X11=/usr/include/Motif1.2
CFLAGSx=-Ae -s  -I$(INC) -I$(X11)
CFLAGS=-Ae  -g -I$(INC) -I$(X11)
CFLAGS=-g -I$(INC)

OBJS =        sclkscet.o fixtime.o gmt.o

.c:           cc $(CFLAGS) -c -g $*.c

lcl:          $(OBJS)
#		cc -o fixtime fixtime.o sclkscet.o -L $(LIB) -l util /lib/libcurses.sl
		cc -o gmt gmt.o  -L $(LIB) -lutil
		
lib:          $(OBJS)
#		cc -o $(BIN)/fixtime fixtime.o sclkscet.o -L $(LIB) -l util /lib/libcurses.sl
clean:
		rm *.o
