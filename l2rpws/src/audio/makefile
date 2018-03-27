#
# Cassini telemetry PCRTIU
#
TOP=/usr/cassini
END=/usr/lib/end.o
BIN=$(TOP)/bin
LIB=$(TOP)/lib
INC=$(TOP)/include 
X11=/usr/include/Motif1.2

OBJS=		player.o autils.o uplay.o

.c:		$(OBJS)
		cc -Ac -c $*.c

player.o:	player.c autils.h
uplay.o:	uplay.c autils.h
autils.o:	autils.c autils.h

taud.o:
		cc -Ae -c taud.c
taud:
		 cc -o taud taud.o uplay.o -l Alib -l m

clean:
		 rm *.o
