#$(error These programs woesn't work on linux-64 due to reliance on struct alignment and endianess)

##############################################################################
# Specific Defs

CC=gcc

CFLAGS = -std=c99 -g -Wall -I$(BUILD_INC) -I$(INST_INC) -I$(CSPICE_INC)
       
OBJS = mdb.o\
       mdb_time.o\
       mdb_scan.o\
       mdb_help.o

LFLAGS = -L$(BUILD_BIN) -L$(INST_NAT_LIB) -lmdb -lutil $(CSPICE_LIB) -lm 

##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %,$(BUILD_OBJ)/%,$(OBJS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(BUILD_OBJ)/%.o:%.c
	$(CC) $(CFLAGS) -c $< -o $@

##############################################################################
# Explicit Rules and dependencies

all: $(BUILD_BIN)/libmdb.a $(BUILD_BIN)/rpws_mdb_list #$(BUILD_BIN)/rpws_mdb_wrap

$(BUILD_BIN)/libmdb.a:$(BUILD_OBJS) $(BUILD_INC)/mdb.h $(BUILD_INC)/mdb_time.h
	ar -r $@ $(BUILD_OBJS) 

$(BUILD_BIN)/rpws_mdb_wrap:
	echo "Solve endian and byte alignment problems in rtiu.h and util.a, then build this on linux"
	/bin/false
		
#$(BUILD_BIN)/rpws_mdb_wrap:$(BUILD_OBJ)/mdbwrap.o $(BUILD_BIN)/libmdb.a $(BUILD_INC)/mdb.h $(BUILD_INC)/mdb_time.h
#	$(CC) -g -o $@ $(BUILD_OBJ)/mdbwrap.o $(LFLAGS)

$(BUILD_BIN)/rpws_mdb_list:$(BUILD_OBJ)/mdblist.o $(BUILD_BIN)/libmdb.a $(BUILD_INC)/mdb.h $(BUILD_INC)/mdb_time.h
	$(CC) -g -o $@ $(BUILD_OBJ)/mdblist.o $(LFLAGS)
	

mdb.o:      mdb.c      $(BUILD_INC)/mdb.h $(BUILD_INC)/mdb_time.h

mdbwrap.o:  mdbwrap.c  $(BUILD_INC)/mdb.h $(BUILD_INC)/mdb_time.h

mdb_time.o: mdb_time.c $(BUILD_INC)/mdb.h $(BUILD_INC)/mdb_time.h

mdb_scan.o: mdb_scan.c $(BUILD_INC)/mdb.h $(BUILD_INC)/mdb_time.h

mdb_help.o: mdb_help.c $(BUILD_INC)/mdb.h $(BUILD_INC)/mdb_time.h
		

clean:
	rm $(BUILD_OBJS)
	
test:
	echo "mdb unit test not yet defined!"
