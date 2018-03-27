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
SRCMDB=/opt/project/cassini/src/pdsdb
#
NAIF=/local/naif/toolkit/lib/spicelib.a
#
CFLAGS=-Ae -g -I$(INC) -I$(X11)

MAIN	 =	 pdsdbwrap.o
OBJS	 =	 pdsdb.o \
		 pdsdb_help.o

lcl:    $(MAIN) $(OBJS)
		ar -r libpdsdb.a $(OBJS)
		f77 -g -o pdsdb pdsdbwrap.o -L./ -lpdsdb -lm -L $(LIB) -l mdb -l util -L $(DASLIB) -l das $(CUR) $(NAIF)
#
#	Everything past this point will end up in /opt/project/cassini/bin
#	and requires "kochab" to perform the 'make'
#
#	Building some CGI stuff as well (cgi subdirectory)
#		
lib:          $(OBJS)
		ar -r $(LIB)/libpdsdb.a $(OBJS)
		f77 -g -o $(BIN)/pdsdb pdsdbwrap.o -L $(LIB) -lpdsdb -lm -L $(LIB) -l util -L $(DASLIB) -l das $(CUR) $(NAIF)

clean:
		rm *.o

pdsdb.o:      pdsdb.c pdsdb.h
		cc $(CFLAGS) -c pdsdb.c -I $(INC)

pdsdbwrap.o:  pdsdbwrap.c pdsdb.h
		cc $(CFLAGS) -c pdsdbwrap.c -I $(INC)

pdsdb_help.o:	pdsdb_help.c pdsdb.h ../mdb/mdb_time.h
		cc $(CFLAGS) -c pdsdb_help.c -I $(INC)
