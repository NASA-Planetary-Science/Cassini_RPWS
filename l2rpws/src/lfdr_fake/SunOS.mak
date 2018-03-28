# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Platform Defs

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CFLAGS=-g -errwarn -xc99 -I$(INC_DIR) $(LFS_CFLAGS)

LFLAGS=-L$(LIB_DIR) -lutil -lm 

OBJS  = lfdr_fake.o

##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %,$(OBJ_DIR)/%,$(OBJS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(OBJ_DIR)/%.o:%.c | $(OBJ_DIR)
	cc $(CFLAGS) -c $< -o $@
	

##############################################################################
# Explicit Rules and Dependencies

$(BIN_DIR)/rpws_lfdr_fake:$(BUILD_OBJS)
	cc $(CFLAGS) $(BUILD_OBJS) $(LFLAGS) -o $@

clean:
	rm $(BUILD_OBJS)

test:  $(BIN)/rpws_lfdr_fake
	@echo "rpws_lfdr_fake unit test is not yet defined!"
	
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)
				
