#
# Cassini minipacket de-segmentation
#TOP=/usr/cassini
TOP=/opt/project/cassini
LIB=$(TOP)/lib
END=/usr/lib/end.o
BIN=$(TOP)/bin
INC=$(TOP)/include
#CFLAGS=-Aa -w1 -s

CASS=/opt/project/cassini

IPATH=$(CASS)/include
INCL=$(IPATH)

LPATH=$(CASS)/lib
LIBs=$(LPATH) -lCasTlm -lCext -lm



wfrsort:wfrsort.c 
	cc -o wfrsort wfrsort.c -I$(INCL) -L$(LIBs)

mpus.o:mpus.c
	cc $(CFLAGS) -c mpus.c -I$(INCL)
	
mpus_main.o:mpus_main.c
	cc $(CFLAGS) -c mpus_main.c -I$(INCL)
			
clean:
	rm *.o
