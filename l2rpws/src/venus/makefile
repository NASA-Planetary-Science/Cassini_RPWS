#
# Cassini telemetry PCRTIU
#
TOP=/usr/cassini
END=/opt/langtools/lib/end.o
BIN=$(TOP)/bin
LIB=$(TOP)/lib
INC=$(TOP)/include 
X11=/usr/include/Motif1.2
CFLAGSx=-Ae -s  -I$(INC) -I$(X11)
CFLAGS=-Ae  -g -I$(INC) -I$(X11)

MAINS =      ven1.o
OBJS =      chk3.o time_fix.o rti_fix.o pkt_fix.o

.c:           cc $(CFLAGS) -c -g $*.c

lcl:	      $(OBJS) $(MAINS) xsort.o vsort.o
		cc -o venus ven1.o vsort.o $(OBJS) $(END) -L $(LIB) -l util
		cc -o xvenus ven1.o xsort.o $(OBJS) $(END) -L $(LIB) -l util
lib:          $(OBJS) $(MAINS)
		cc -o $(BIN)/venus22 ven1.o xsort.o $(OBJS) -L $(LIB) -l util

ven1.o:		ven1.c venus.h
chk3.o:		chk3.c venus.h
time_fix.o:	time_fix.c venus.h
rti_fix.o:	rti_fix.c venus.h
pkt_fix.o:	pkt_fix.c venus.h
vsort.o:	vsort.c venus.h
xsort.o:	xsort.c venus.h xvenus.h

clean:
		rm *.o
