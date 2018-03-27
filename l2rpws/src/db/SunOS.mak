# Use GNU make
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Platform Defs

CC = cc
# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CFLAGS = -xc99 -g -I$(INC_DIR) -I$(INST_INC) $(LFS_CFLAGS)
LFLAGS = -L$(LIB_DIR) -L$(INST_NAT_LIB) -lfg -lm


##############################################################################
# Pattern Defs

SRCS=db_lock.c dblock.o dbpatch.c dbtest.c dbupdate.c

BUILD_OBJS=$(patsubst %.c,$(OBJ_DIR)/%.o,$(SRCS))


##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(OBJ_DIR)/%.o:%.c
	$(CC) $(CFLAGS) -c $< -o $@


##############################################################################
# Explicit Rules and dependencies

#lcl:    $(OBJS)
#		cc -g -o shit shit.o  -lm
#		cc -g -o dbpatch dbpatch.o dblock.o  -L $(LIB) -lm -lutil
#		cc -g -o dbupdate dbupdate.o dblock.o  -L $(LIB) -lm -lutil
#		cc -g -o db_lock db_lock.o dblock.o  -L $(LIB) -lm -lutil
#		cc -g -o dbtest dbtest.o dblock.o  -L $(LIB) -lm -lutil

all: $(BIN_DIR)/rpws_db_update $(BIN_DIR)/rpws_db_patch $(BIN_DIR)/rpws_db_lock
		
$(BIN_DIR)/rpws_db_patch: $(OBJ_DIR)/dbpatch.o $(OBJ_DIR)/dblock.o
	$(CC) $^ $(LFLAGS) -o $@
	
$(BIN_DIR)/rpws_db_update: $(OBJ_DIR)/dbupdate.o $(OBJ_DIR)/dblock.o
	$(CC) $^ $(LFLAGS) -o $@
	
$(BIN_DIR)/rpws_db_lock: $(OBJ_DIR)/db_lock.o $(OBJ_DIR)/dblock.o
	$(CC) $^ $(LFLAGS) -o $@

$(BIN_DIR)/rpws_db_test: $(OBJ_DIR)/dbtest.o $(OBJ_DIR)/dblock.o
	$(CC) $^ $(LFLAGS) -o $@

clean:
	rm $(BUILD_OBJS)

test:
	@echo "No unit test defined by this module"

