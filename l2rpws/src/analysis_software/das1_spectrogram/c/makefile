TOP=/opt/project/cassini
LIB=$(TOP)/lib/original
INC=$(TOP)/include/original

CFLAGS=-Ae -g -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64 -I$(INC) -I/opt/local/include

OBJS =        key_reader.o

.c:           cc $(CFLAGS) -I$(INC) -I/opt/local/include -c $*.c

lib:          $(OBJS)
		cc -g -o key_reader.exe key_reader.o \
                                        $(LIB)/libutil.a  \
                                        -L/opt/local/lib -ldas -lm -lsunmath

clean:
		rm *.o
