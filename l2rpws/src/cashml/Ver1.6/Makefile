CC= cc -Aa -g


PGPLOT_IPATH= /usr/local/pgplot
PGPLOT_INCL= -I$(PGPLOT_IPATH)
PGPLOT_LPATH= /usr/local/pgplot
PGPLOT_LIBs= -L$(PGPLOT_LPATH) -lXmPgplot -lcpgplot -lpgplot -lf 

MOTIF_IPATH= /usr/include/Motif1.2
MOTIF_INCL= -I$(MOTIF_IPATH)
MOTIF_LPATH= /usr/lib/Motif1.2
MOTIF_LIBs= -L$(MOTIF_LPATH) -lXm

XWIN_IPATH= /usr/include/X11R5
XWIN_INCL= -I$(XWIN_IPATH)
XWIN_LPATH= /usr/lib/X11R5
XWIN_LIBs= -L$(XWIN_LPATH) -lXt -lX11 -lXext

CAS_DIR= /usr/cassini
CAS_IPATH= $(CAS_DIR)/include
CAS_INCL= -I$(CAS_IPATH)
CAS_LPATH= $(CAS_DIR)/lib
#CAS_LIBs= -L$(CAS_LPATH) -lCasTlm -lCTools -lCext
CAS_LIBs= -L/usr/cassini/src/raj/lib/CasTlm/Ver1.1 -lCasTlm -L$(CAS_LPATH) -lCTools -lCext


OBJs= main_gui.o


cashml: $(OBJs)
	$(CC) -o cashml $(OBJs) $(CAS_LIBs) $(PGPLOT_LIBs) $(MOTIF_LIBs) $(XWIN_LIBs) -lm -lc 

main_gui.o: main_gui.c
	$(CC) -c main_gui.c $(CAS_INCL) $(PGPLOT_INCL) $(MOTIF_INCL) $(XWIN_INCL)

relink:
	rm -f cashml; make -f Makefile

backup:
	cp *.h *.c Bak

clean :
	rm -f *.o
