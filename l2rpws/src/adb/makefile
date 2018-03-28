#
# Cassini telemetry PCRTIU
#
TOP=/opt/project/cassini
BIN=$(TOP)/bin
LIB=$(TOP)/lib
INC=$(TOP)/include
DASLIB=/local/lib
DASINC=/local/include
JOEOBJ= /home/jbg/cassini/web/util.o
CUR=-lcurses
X11=/usr/include/Motif1.2
#
SRCADB=/opt/project/cassini/src/adb
#
NAIF=/local/naif/toolkit/lib/spicelib.a
#
CFLAGS=-Ae -g -I$(INC) -I$(X11)

MAINS    =	 adbwrap.o
OBJS	 =	 adb.o

lcl:    $(MAINS) $(OBJS)
		ar -r libadb.a $(OBJS)
		f77 -g -o adb adbwrap.o -ladb -L $(LIB) -lmdb -lm -L $(LIB) -l util -L $(DASLIB) -l das $(CUR) $(NAIF)
#
#	Everything past this point will end up in /opt/project/cassini/bin
#	and requires "kochab" to perform the 'make'
#
#	Building some CGI stuff as well (cgi subdirectory)
#		
lib:          $(OBJS)
		ar -r $(LIB)/libadb.a $(OBJS)
		f77 -g -o $(BIN)/adb10 adbwrap.o -L $(LIB) -ladb -lmdb -lm -L $(LIB) -l util -L $(DASLIB) -l das $(CUR) $(NAIF)

adb.o:   $(SRCADB)/adb.c $(INC)/adb.h adb_time.h
		cc $(CFLAGS) -c $(SRCADB)/adb.c -I $(INC)

adbwrap.o:   $(SRCADB)/adbwrap.c $(INC)/adb.h
		cc $(CFLAGS) -c $(SRCADB)/adbwrap.c -I $(INC)

clean:
		rm *.o
