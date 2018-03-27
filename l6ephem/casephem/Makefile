CC=cc
FC=f77
CFLAGS=-v -O -I/local/include
FFLAGS=-xvector=no -xtarget=ultra -O
LIBS3=-L/local/lib -L/opt/project/cassini/lib -lm -ldas -lgllspice -lspicelib -R/opt/project/cassini/lib -R/local/lib

casephem_sub.o:
	$(FC) $(FFLAGS) -c casephem_sub.f

util.o:
	$(CC) $(CFLAGS) -c util.c

casephem.o:
	$(CC) $(CFLAGS) -c casephem.c

casephem: casephem.o
	$(FC) $(FFLAGS) -o casephem casephem.o casephem_sub.o util.o $(LIBS3)
