# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Platform Defs

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CFLAGS=-g -errwarn -xc99 -I$(INC_DIR) -I$(INST_INC) $(LFS_CFLAGS)

LFLAGS=-L$(LIB_DIR) -lrpwstlm -lutil -L$(INST_NAT_LIB) -lcasephem -lfg $(CSPICE_LIB) -lm

OBJS= usdc_main.o usdc.o rice.o fg_filter.o


#CC = gcc -ansi -pedantic

#CAS_DIR=/opt/project/cassini

#INCL= -I$(CAS_DIR)/include
#LIBs= -L$(CAS_DIR)/lib -lCasTlm -L$(CAS_DIR)/spice/lib -lcspice -lutil

##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %,$(OBJ_DIR)/%,$(OBJS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(OBJ_DIR)/%.o:%.c usdc.h rice.h | $(OBJ_DIR)
	cc $(CFLAGS) -c $< -o $@

##############################################################################
# Explicit Rules and Dependencies

$(BIN_DIR)/rpws_usdc:$(BUILD_OBJS)
	cc $(CFLAGS) $(BUILD_OBJS) $(LFLAGS) -o $@

clean:
	rm $(BUILD_OBJS)

test:  $(BIN)/rpws_usdc
	@echo "rpws_usdc unit test is not yet defined!"
	
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)
