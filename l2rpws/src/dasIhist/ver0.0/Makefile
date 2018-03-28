CC = gcc -ansi -pedantic

GCCOPT = -Wall -Wshadow -Wpointer-arith -Wcast-qual \
		-Wcast-align -Wwrite-strings -Wconversion -Waggregate-return \
		-Wmissing-prototypes -Wnested-externs -Wimplicit \
                -Wstrict-prototypes -Wredundant-decls



INCL= -I/usr/include -I/opt/local/include -I/home/raj/project/include
LIBs= -L/opt/project/cassini/lib -ldas

OBJs= dasIhist.o pdshist.o
OBJs= dasIhist.o

INCL= -I/usr/include -I/opt/local/include -I/home/raj/cassini/src/lib/castlm/Ver2.6 -I/home/raj/project/include
LIBs= -L/home/raj/cassini/src/lib/castlm/Ver2.6 -lCasTlm -L/opt/project/cassini/lib -ldas

INCL= -I/home/raj/cassini/src/lib/castlm/ver2.7 -I/home/raj/project/include
LIBs= -L/home/raj/cassini/src/lib/castlm/ver2.7 -lCasTlm \
      -L/home/raj/project/spice/lib -lcspice 




INCL=
LIBs=

OBJs= pdsrdr.o pdshist.o
OBJs= dasIhist.o

pdsrdr: $(OBJs)
	$(CC) $(GCCOPT) -o dasIhist $(OBJs) $(LIBs) -lm -lc

.c.o:
	$(CC) $(GCCOPT) -c $(INCL) $<

relink:
	rm -f dasIhist; make -f Makefile

clean:
	rm -f *.o 


dasIhist.o: dasIhist.c
pdshist.o: pdshist.c

