#
# Cassini highrate browse images
#
TOP=/net/kochab/opt/project/cassini
TOP=/opt/project/cassini
LIB=$(TOP)/lib
INC=$(TOP)/include
ARCH=$(TOP)/src/archive
MDB=/opt/project/cassini/src/mdb
SRCARCH=/opt/project/cassini/src/browse
SRCMDB=$(MDB)
DASLIB=/local/lib
DASINC=/local/include
JOEOBJ= /home/jbg/cassini/web/util.o
CUR=-lcurses
X11=/usr/include/Motif1.2
CFLAGSx=-Ae -s  -I$(INC) -I$(X11) -g
CFLAGS=-Ae -g -I$(INC) -I$(X11)
CFLAGSx=-Ae -xprofile=tcov -I$(INC) -I$(X11)

OBJS  =         batch.o browse.o browse_wfr.o wbrplot.o wbrplot_mdb.o configure.o

lcl:    $(OBJS)
		f77 -g -o browse browse.o $(ARCH)/rpws_fsw_ver.o $(ARCH)/rpws_status.o -L$(MDB) -lmdb -L$(LIB) -lutil -L$(DAS) -ldas -lm /local/naif/toolkit/lib/spicelib.a
		f77 -g -o browse_wfr browse_wfr.o $(ARCH)/rpws_fsw_ver.o $(ARCH)/rpws_status.o -L$(MDB) -lmdb -L$(LIB) -lutil -L$(DAS) -ldas -lm /local/naif/toolkit/lib/spicelib.a
		f77 -g -o batch batch.o configure.o
##		f77 -g -o wbrplot wbrplot.o -L$(LIB) -lutil -L$(MDB) -lmdb /lib/libm.a -lpgplot -L/opt/local/lib -ldas -lm 
		f77 -g -o wbrplot wbrplot_mdb.o $(ARCH)/rpws_status.o -L$(LIB) -lutil  -L$(MDB) -lmdb /lib/libm.a -lpgplot -L/opt/local/lib -ldas -lm /local/naif/toolkit/lib/spicelib.a
##
#	Everything past this point will end up in /opt/project/cassini/bin
#	and requires "kochab" to perform the 'make'
#
#	Building some CGI stuff as well (cgi subdirectory)
#		
lib:	$(OBJS)

browse.o:	 $(SRCARCH)/browse.c 
		 cc $(CFLAGS) -c $(SRCARCH)/browse.c -I$(DASINC) -I$(ARCH) -I$(INC)
browse_wfr.o:	 $(SRCARCH)/browse_wfr.c 
		 cc $(CFLAGS) -c $(SRCARCH)/browse_wfr.c -I$(DASINC) -I$(ARCH) -I$(INC)
wbrplot.o:	 $(SRCARCH)/wbrplot.c 
		 cc $(CFLAGS) -c $(SRCARCH)/wbrplot.c -I$(DASINC) -I$(ARCH) -I$(INC)
wbrplot_mdb.o:	 $(SRCARCH)/wbrplot_mdb.c 
		 cc $(CFLAGS) -c $(SRCARCH)/wbrplot_mdb.c -I$(DASINC) -I$(ARCH) -I$(INC)

clean:
		rm *.o
