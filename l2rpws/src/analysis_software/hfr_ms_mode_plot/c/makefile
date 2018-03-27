TOP=/opt/project/cassini
LIB=$(TOP)/lib/original
INC=$(TOP)/include/original
CFLAGS=-Ae -g -I$(INC) -I/opt/local/include

OBJS =        msplot2.o

.c:           cc $(CFLAGS) -c $*.c

lib:          $(OBJS)
		f77 -o msplot2 msplot2.o  \
                                        /home/jbg/cassini/web/util.o \
                                        $(LIB)/libutil.a  \
					 /local/pgplot/libpgplot.a  \
					-L/opt/csw/lib -R/opt/csw/lib -lpng \
					-lm -lsunmath  -lX11 -L/opt/local/lib -ldas
		
clean:
		rm *.o

