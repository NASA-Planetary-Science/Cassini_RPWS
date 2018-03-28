#
# Cassini telemetry PCRTIU
#
CFLAGS=-Ae -s 

OBJS  =      dupe.o

lcl:    $(OBJS)
		cc -g -o dupe  dupe.o  

dupe.o:         dupe.c
		cc  -c dupe.c  $(CFLAGS)

clean:
		rm *.o
