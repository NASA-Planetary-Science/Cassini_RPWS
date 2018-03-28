# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Platform Defs


# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CFLAGS=-g -errwarn -xc99 -I$(INST_INC) -I$(CSPICE_INC) $(LFS_CFLAGS)

LFLAGS=-L$(LIB_DIR) -lrpwstlm -L$(INST_NAT_LIB) -lcasephem $(CSPICE_LIB) -lm

##############################################################################
# Pattern Defs

OBJS  = mpus_main.o mpus.o

BUILD_OBJS=$(patsubst %,$(OBJ_DIR)/%,$(OBJS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(OBJ_DIR)/%.o:%.c mpus.h | $(OBJ_DIR)
	cc $(CFLAGS) -c $< -o $@

##############################################################################
# Explicit Rules and Dependencies

$(BIN_DIR)/rpws_mpus:$(BUILD_OBJS)
	cc $(CFLAGS) $(BUILD_OBJS) $(LFLAGS) -o $@

clean:
	rm $(BUILD_OBJS)

test:  $(BIN)/rpws_mpus
	@echo "rpws_mpus unit test is not yet defined!"
	
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)
				
