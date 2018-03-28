CC=cc
FC=f77
CFLAGS=-v -O -I/local/include
FFLAGS=-xvector=no -xtarget=ultra -O
PGPLOT_LIB = /opt/local/lib/libpgplot.a
LIBS7=-L/local/lib -lm -ldas -lX11 -lgllspice -lspicelib -R/local/lib

util.o:
	$(CC) $(CFLAGS) -c util.c

plotsat_sub.o:
	$(FC) $(FFLAGS) -c plotsat_sub.f

plotsat.o:
	$(CC) $(CFLAGS) -c plotsat.c

plotsat: plotsat.o
	$(FC) $(FFLAGS) -o plotsat plotsat.o util.o plotsat_sub.o $(PGPLOT_LIB) $(LIBS7)
