#
# Cassini telemetry PCRTIU
#
TOP=/usr/cassini
END=/opt/langtools/lib/end.o
BIN=$(TOP)/bin
LIB=$(TOP)/lib
LIB2=$(TOP)/util
INC=$(TOP)/include 
X11=/usr/include/Motif1.2
CFLAGSx=-Ae -s  -I$(INC) -I$(X11)
CFLAGS=-Ae  -g -I$(INC) -I$(X11)

OBJS =        xwdcolor.o

.c:           cc $(CFLAGS) -c -g $*.c

lcl:          $(OBJS)
		cc -o xwdcolor xwdcolor.o  -L $(LIB) -l util
#
#	Everything past this point will end up in /usr/cassini/bin
#	and requires root to perform the 'make'
#		
lib:		$(OBJS)
		cc -o $(BIN)/xwdcolor xwdcolor.o -L $(LIB) -l util 
		strip $(BIN)/xwdcolor
		chown robison:cassini $(BIN)/xwdcolor
		chmod ugo-rw $(BIN)/xwdcolor
		chmod u+s $(BIN)/xwdcolor
