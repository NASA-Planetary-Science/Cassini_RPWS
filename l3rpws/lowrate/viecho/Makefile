CC = gcc -ansi -pedantic
GCCOPT = -Wall -Wshadow -Wpointer-arith -Wcast-qual \
		-Wcast-align -Wwrite-strings -Wconversion -Waggregate-return \
		-Wmissing-prototypes -Wnested-externs -Wimplicit \
                -Wstrict-prototypes -Wredundant-decls


INCL= -I/home/raj/project/include \
      -I/home/raj/cassini/src/lib/castlm/Ver2.6 \
      -I/home/raj/project/spice/include
      

LIBs= -L/home/raj/cassini/src/lib/castlm/Ver2.6 -lCasTlm \
      -L/home/raj/project/spice/lib -lcspice 

OBJs= viecho.o 


viecho: $(OBJs)
	$(CC) $(GCCOPT) -o viecho $(OBJs) $(LIBs) -lm -lc

.c.o:
	$(CC) $(GCCOPT) -c $(INCL) $<

relink:
	rm -f viecho; make -f Makefile

clean:
	rm -f *.o


viecho.o: viecho.c

