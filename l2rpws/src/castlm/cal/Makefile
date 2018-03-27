#CC = cc -X c
CC = gcc -ansi -pedantic


GCCOPT = -Wall -Wshadow -Wpointer-arith -Wcast-qual \
		-Wcast-align -Wwrite-strings -Wconversion -Waggregate-return \
		-Wmissing-prototypes -Wnested-externs -Wimplicit \
                -Wstrict-prototypes -Wredundant-decls


INCL= 
LIBs= 

OBJs= main.o 

main: $(OBJs)
	$(CC) $(GCCOPT) -o main $(OBJs) $(LIBs) -lm -lc

.c.o:
	$(CC) $(GCCOPT) -c $(INCL) $<
 
relink:
	rm -f main; make -f Makefile

clean:
	rm -f *.o 


main.o: main.c


#main: $(OBJs)
#	$(CC) $(GCCOPT) main.c -o main $(INCL) $(LIBs) -lm -lc
