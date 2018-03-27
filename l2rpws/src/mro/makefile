#
# Cassini MRO
#
TOP=/net/kochab/opt/project/cassini
TOP=/opt/project/cassini
LIB=$(TOP)/lib
INC=$(TOP)/include
ARCH=$(TOP)/src/archive
MDB=/opt/project/cassini/src/mdb
SRCARCH=/opt/project/cassini/src/mro
SRCMDB=$(MDB)
DASLIB=/local/lib
DASINC=/local/include
JOEOBJ= /home/jbg/cassini/web/util.o
CUR=-lcurses
X11=/usr/include/Motif1.2
CFLAGSx=-Ae -s  -I$(INC) -I$(X11) -g
CFLAGS=-Ae -g -I$(INC) -I$(X11)
CFLAGSx=-Ae -xprofile=tcov -I$(INC) -I$(X11)

OBJS  =         mro.o

lcl:    $(OBJS)
		f77 -g -o mro mro.o $(ARCH)/rpws_fsw_ver.o $(ARCH)/rpws_status.o -L$(MDB) -lmdb -L$(LIB) -lutil -L$(DAS) -ldas -lm /local/naif/toolkit/lib/spicelib.a

mro.o:	 $(SRCARCH)/mro.c 
		 cc $(CFLAGS) -c $(SRCARCH)/mro.c -I$(DASINC) -I$(ARCH) -I$(INC)

clean:
		rm *.o
