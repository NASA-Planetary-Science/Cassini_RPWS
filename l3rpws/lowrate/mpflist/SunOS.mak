##############################################################################
# Generics

# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Specific Defs

CC = cc

LFS_CFLAGS=$(shell getconf LFS_CFLAGS) # Add large file support

CFLAGS = -xc99 -errwarn -g -I$(CSPICE_INC) -I$(INST_INC) $(LFS_CFLAGS)

LFLAGS = -L$(INST_NAT_LIB) -lrpwstlm -lcasephem $(CSPICE_LIB) -lm

OBJS=mpflist.o

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

all:$(OBJ_DIR) $(BUILD_DIR)/rpws_mpflist

$(BUILD_DIR)/rpws_mpflist:$(OBJ_DIR)/mpflist.o $(OBJ_DIR)/rajTime.o 
	$(CC) $^ $(CFLAGS) $(LFLAGS) -ldas2 -lm -o $@ 
	
clean:
	rm -f $(BUILD_OBJS) $(BUILD_DIR)/rpws_mpflist 
	
test:
	echo "TODO: setup mplist unit tests"

$(OBJ_DIR):
	@if [ ! -d $(OBJ_DIR) ] ; then mkdir -p $(OBJ_DIR); fi
	
$(TEST_DIR):
	@if [ ! -d $(TEST_DIR) ] ; then mkdir $(TEST_DIR); fi
	
$(BULID):
	@if [ ! -d $(BUILD_DIR) ] ; then mkdir $(BUILD_DIR); fi

$(OBJ_DIR)/fxtime.o: fxtime.c
$(OBJ_DIR)/rajTime.o: rajTime.c
