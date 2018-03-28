CC = gcc -ansi -pedantic

GCCOPT = -Wall -Wshadow -Wpointer-arith -Wcast-qual \
		-Wcast-align -Wwrite-strings -Wconversion -Waggregate-return \
		-Wmissing-prototypes -Wnested-externs -Wimplicit \
                -Wstrict-prototypes -Wredundant-decls




INCL= -I/opt/project/cassini/include -I/opt/project/cassini/spice/include
LIBs= -L/opt/project/cassini/lib -lCasTlm \
      -L/opt/project/cassini/spice/lib -lcspice

OBJs= pdsmark.o


pdsmark: $(OBJs)
	$(CC) $(GCCOPT) -o pdsmark $(OBJs) $(LIBs) -lm -lc

.c.o:
	$(CC) $(GCCOPT) -c $(INCL) $<

relink:
	rm -f pdsmark; make -f Makefile

clean:
	rm -f *.o 


pdsmark.o: pdsmark.c
