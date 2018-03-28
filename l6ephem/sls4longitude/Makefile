CC=cc
FC=f77
CFLAGS=-v -O -I/local/include
FFLAGS=-xvector=no -xtarget=ultra -O
LIBS3=-L/local/lib -L/opt/project/cassini/lib -lm -ldas -lgllspice -lspicelib -R/opt/project/cassini/lib -R/local/lib

util.o:
	$(CC) $(CFLAGS) -c util.c

sls4longitude.o:
	$(CC) $(CFLAGS) -c sls4longitude.c

sls4longitude: sls4longitude.o
	$(FC) $(FFLAGS) -o sls4longitude sls4longitude.o util.o $(LIBS3)
