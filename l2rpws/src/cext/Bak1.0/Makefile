CC = cc -Ae

CAS_DIR= /usr/cassini

CAS_IPATH= $(CAS_DIR)/include
CAS_INCL= -I$(CAS_IPATH)

CAS_LPATH= $(CAS_DIR)/lib
CAS_LIBs= -L$(CAS_LPATH) -lCext


OBJs= Cext.o Sleep.o

libCext.a: $(OBJs)
	ar r libCext.a $(OBJs) ; mv libCext.a $(CAS_LPATH)

Cext.o: Cext.c
	$(CC) -c Cext.c $(CAS_INCL)

Sleep.o: Sleep.c   # Do not compile to ANSI standards, need different time.h
	$(CC) -c Sleep.c 

test:
	$(CC) -o main main.c $(CAS_INCL) $(CAS_LIBs) 

clean:
	rm -f *.o 

