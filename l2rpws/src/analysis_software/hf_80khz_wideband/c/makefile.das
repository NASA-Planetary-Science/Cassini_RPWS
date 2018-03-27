TOP=/opt/project/cassini
LIB=$(TOP)/lib/original
INC=$(TOP)/include/original
CFLAGS=-Ae -g -I$(INC) -I/opt/local/include

OBJS =        hfr_msagc.o

.c:           cc $(CFLAGS) -c $*.c

lib:          $(OBJS)
		cc -o hfr_msagc.exe hfr_msagc.o \
                                        $(LIB)/libutil.a  \
                                        -L/opt/local/lib -ldas -lm -lsunmath
		
clean:
		rm *.o
