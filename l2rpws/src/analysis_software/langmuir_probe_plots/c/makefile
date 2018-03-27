TOP=/opt/project/cassini
LIB=$(TOP)/lib/original
INC=$(TOP)/include/original
CFLAGS=-Ae -g -I$(INC) -I/opt/local/include

OBJS =        lp_plot.o

.c:           cc $(CFLAGS) -c $*.c

lib:          $(OBJS)
		f77 -o lp_plot lp_plot.o  \
                                        /home/jbg/cassini/web/util.o \
					 /local/pgplot/libpgplot.a \
					/local/lib/libpng.a \
                                        -L./ -lm -L/opt/local/lib -L/local/lib \
                                        -L/opt/SUNWspro/lib \
                                        -R/opt/SUNWspro/lib \
                                        -R/opt/local/lib    \
                                        $(LIB)/libutil.a  \
                                        -lX11 \
                                        -lz -ldas -lsunmath

clean:
		rm *.o
