#
# Cassini minipacket de-segmentation
TOP=/usr/cassini
LIB=$(TOP)/lib
END=/usr/lib/end.o
BIN=$(TOP)/bin
INC=$(TOP)/include
CFLAGS=-Aa -w1 -s


mpus: mpus_main.o mpus.o $(TOP)/lib/libutil.a 
	cc -o mpus mpus_main.o mpus.o -L$(LIB) -lutil 

lib: mpus_main.o mpus.o $(TOP)/lib/libutil.a 
	cc -o $(TOP)/bin/mpus8 mpus_main.o mpus.o -L$(LIB) -lutil 

mpus.o:		mpus.c
	        cc $(CFLAGS) -c mpus.c -I$(INC)
	
mpus_main.o:	mpus_main.c
	        cc $(CFLAGS) -c mpus_main.c -I$(INC)
			
clean:
	rm *.o
