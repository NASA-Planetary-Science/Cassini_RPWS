# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Specific Defs

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CFLAGS=-g -errwarn -xc99 -I$(INC_DIR) -I$(INST_INC) $(LFS_CFLAGS)

LFLAGS=-L$(LIB_DIR) -lutil -lfg -L$(INST_NAT_LIB)

OBJS  = decomp1.o

##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %,$(OBJ_DIR)/%,$(OBJS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(OBJ_DIR)/%.o:%.c
	cc $(CFLAGS) -c $< -o $@
	

##############################################################################
# Explicit Rules and Dependencies


$(BIN_DIR)/rpws_decomp:$(BUILD_OBJS)
	cc $(CFLAGS) $(BUILD_OBJS) $(LFLAGS) -o $@
		
clean:
	rm $(BUILD_OBJS)

test:  $(BIN)/rpws_decomp
	@echo "rpws_decomp unit test is not yet defined!"


