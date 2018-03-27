# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Specific Defs

DEFINES='-DINST_ETC="$(INST_ETC)"'

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CFLAGS=-Ae -g $(DEFINES) -I$(INC_DIR) -I$(INST_INC) -I$(CSPICE_INC) $(LFS_CFLAGS)

OBJS = dsp4.o dsp5.o dsp5_hfr.o dsp8.o dsp9.o dsp9a.o dsp9b.o dsp9c.o \
 dsp9t.o dsp9x.o dspq.o dspr.o hkrom14.o hkanal.o webutil.o
 # hkrom12.0

# No generic LFLAGS here, programs require very different libraries
LFLAGS=

##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %,$(OBJ_DIR)/%,$(OBJS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(OBJ_DIR)/%.o:%.c
	$(CC) $(CFLAGS) -c $< -o $@

##############################################################################
# Explicit Rules and dependencies

all: $(BIN_DIR)/rpws_dsp_4 $(BIN_DIR)/rpws_dsp_5 $(BIN_DIR)/rpws_dsp_8 \
     $(BIN_DIR)/rpws_dsp_9 $(BIN_DIR)/rpws_dsp_9a $(BIN_DIR)/rpws_dsp_r \
	$(BIN_DIR)/rpws_dsp_q $(BIN_DIR)/rpws_dsp_hkanal $(BIN_DIR)/rpws_dsp_hkrom

$(BIN_DIR)/rpws_dsp_r:$(OBJ_DIR)/dspr.o
	$(CC) -g $< -L$(LIB_DIR) -lcassort -lmdb -lutil -L$(INST_NAT_LIB) -lfg \
	$(CSPICE_LIB) -lm -o $@ 

$(BIN_DIR)/rpws_dsp_q:$(OBJ_DIR)/dspq.o
	$(CC) -g $< -L$(LIB_DIR) -lcassort -lmdb -lutil -L$(INST_NAT_LIB) -lfg \
     $(CSPICE_LIB) -lm -o $@

$(BIN_DIR)/rpws_dsp_hkanal:$(OBJ_DIR)/hkanal.o $(LIB_DIR)/libutil.a $(LIB_DIR)/libmdb.a
	$(CC) $< -L$(LIB_DIR) -lcassort -lmdb -lutil -L$(INST_NAT_LIB) -lfg \
	$(CSPICE_LIB) -lm  -o $@ 

$(BIN_DIR)/rpws_dsp_4:$(OBJ_DIR)/dsp4.o
	$(CC) $< -L$(LIB_DIR) -lcassort -lmdb -lutil -lfg \
	$(CSPICE_LIB) -lm  -lcurses -o $@ 

$(BIN_DIR)/rpws_dsp_5:$(OBJ_DIR)/dsp5.o $(OBJ_DIR)/dsp5_hfr.o
	cc -g $^ -L $(LIB_DIR) -lutil -L$(INST_NAT_LIB) -lfg -lcurses -lm -o $@

$(BIN_DIR)/rpws_dsp_8:$(OBJ_DIR)/dsp8.o
	cc -g $< -L $(LIB_DIR)  -l util -L$(INST_NAT_LIB) -lfg -lcurses -lm -o $@

$(BIN_DIR)/rpws_dsp_9:$(OBJ_DIR)/dsp9.o
	cc -g $< -L $(LIB_DIR) -l util -L$(INST_NAT_LIB) -lfg -o $@
	
$(BIN_DIR)/rpws_dsp_9a:$(OBJ_DIR)/dsp9a.o $(OBJ_DIR)/dsp9b.o $(OBJ_DIR)/dsp9c.o \
  $(OBJ_DIR)/dsp9t.o $(OBJ_DIR)/dsp9x.o $(OBJ_DIR)/webutil.o
	cc -g -o $@ $^ -L$(LIB_DIR) -lutil -L$(INST_NAT_LIB) -lfg -ldas2 \
	-lsocket -lnsl -lm
	
$(BIN_DIR)/rpws_dsp_hkrom:$(OBJ_DIR)/hkrom14.o
	cc -g -o $@ $< -L $(LIB_DIR) -lutil -L$(INST_NAT_LIB) -lfg -lcurses
	
clean:
	rm $(BUILD_OBJS)

test:
	@echo "No unit tests defined for dsp yet!"

##
# $(BIN_DIR)/dspq2:$(OBJ_DIR)/dspq2.o
#	$(CC) -g $< -L$(BIN_DIR) -lcassort -lmdb -lutil $(SPICE) -lm -o $@ 
#	$(CC) -g -o dspq2 dspq2.o $(SRC)/sort_merge/sort.o 
#	$(SRC)/archive/rpws_status.o -L $(LIB) -l mdb -l Util $(NAIF)/spicelib.a

#$(BIN_DIR)/dsph:$(OBJ_DIR)/dsph.o
#	cc -g $< -L$(BIN_DIR) -lUtil -lutil -lcurses $(SPICE) -lm -o $@ 

#$(BIN_DIR)/hkrom12:$(OBJ_DIR)/hkrom12.o
#	cc -g -o $@ $< -L $(LIB) -l util -lcurses


#install_dont_use_yet:$(BUILD_OBJS)
#	$(CC) -o $(BIN_DIR)/dspr dspr.o -L $(LIB) -l mdb -l util $(NAIF)/spicelib.a
#	$(CC) -o $(BIN_DIR)/dspq dspq.o -L $(LIB) -l mdb -l Util $(NAIF)/spicelib.a
#	$(CC) -o $(BIN_DIR)/dspq2 dspq2.o $(SRC)/sort_merge/sort.o \
#     $(SRC)/archive/rpws_status.o  -L $(LIB) -l mdb -l util $(NAIF)/spicelib.a
#	cc  -o $(BIN_DIR)/dsp5 dsp5.o dsp5_hfr.o -L $(LIB) -l util $(CUR)
#	$(CC) -o $(BIN_DIR)/dsp4 dsp4.o -L $(LIB) -l util $(CUR) $(NAIF)/spicelib.a 
#	cc  -o $(BIN_DIR)/dsp8      dsp8.o            -L $(LIB) -l util $(CUR)
##	cc  -o $(BIN_DIR)/cgi/dsp9  dsp9.o            -L $(LIB) -l util
#	cc  -o $(BIN_DIR)/cgi/dsp9a dsp9a.o dsp9b.o dsp9c.o dsp9t.o dsp9x.o $(JOEOBJ) \
#           -L$(DASLIB) -ldas2 -L $(LIB) -l util
#	cp $(BIN_DIR)/cgi/dsp9a $(BIN_DIR)/cgi/dsp9
#	cp $(BIN_DIR)/cgi/dsp9a $(BIN_DIR)/cgi/dsp9a_vc0
#	cp $(BIN_DIR)/cgi/dsp9a $(BIN_DIR)/cgi/dsp9a_vc1
#	cp $(BIN_DIR)/cgi/dsp9a $(BIN_DIR)/cgi/dsp9a_all
#	cp $(BIN_DIR)/cgi/dsp9a $(BIN_DIR)/cgi/dsp9a_key
#	cp $(BIN_DIR)/cgi/dsp9a $(BIN_DIR)/cgi/dsp9a_nert
#	cp $(BIN_DIR)/cgi/dsp9a $(BIN_DIR)/cgi/dsp9a_em
#	cp $(BIN_DIR)/cgi/dsp9a $(BIN_DIR)/cgi/dsp9a_test
#	cc -o $(BIN_DIR)/hkrom14 hkrom14.o   -L $(LIB) -l util $(CUR)
