# Cassini BIU command for PC-RTIU
#
TOP=/usr/cassini
INC=$(TOP)/include
UTIL=$(TOP)/util
LIB=$(TOP)/lib
BIN=$(TOP)/bin
CFLAGS=-Ae -I $(INC) -g
OBJS=  biucmd2.o
PCRTIUA=/usr/cassini/src/pcrtiu/pcrtiua.o

.c:           cc $(CFLAGS) -c $*.c

lcl:        $(OBJS)
#		cc -o biucmd biucmd.o $(PCRTIUA) /usr/lib/end.o  -L$(LIB) -lutil
#		cc -o biucmd1 biucmd1.o $(PCRTIUA) $(UTIL)/UTIL_align_on_second.o /usr/lib/end.o  -L$(LIB) -lutil
		cc -o biucmd2 biucmd2.o $(PCRTIUA) $(UTIL)/UTIL_align_on_second.o /usr/lib/end.o  -L$(LIB) -lutil
lib:        $(OBJS)
#		cc -o $(BIN)/biucmd biucmd.o $(PCRTIUA) /usr/lib/end.o  -L$(LIB) -lutil
#		cc -o $(BIN)/biucmd1 biucmd1.o $(PCRTIUA) $(UTIL)/UTIL_align_on_second.o /usr/lib/end.o  -L$(LIB) -lutil
		cc -o $(BIN)/biucmd2 biucmd2.o $(PCRTIUA) $(UTIL)/UTIL_align_on_second.o /usr/lib/end.o  -L$(LIB) -lutil


biucmd2.o:      biucmd2.c $(INC)/pcrtiu.h $(INC)/util.h

clean:
		rm *.o
