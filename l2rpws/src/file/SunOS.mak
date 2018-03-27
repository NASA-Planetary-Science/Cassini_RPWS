# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Specific Defs

#CFLAGSx=-Ae -s  -I$(INC) -I$(X11)
#CFLAGS=-Ae -g  -I$(INC)

# Add large file support to be compatible with NFS 4
LFS_CFLAGS=$(shell getconf LFS_CFLAGS)

CFLAGS=-g -xc99 $(LFS_CFLAGS) -I$(BUILD_INC) -I$(INST_INC) 

OBJS = file_58.o

LFLAGS=-L$(BUILD_LIB) -lutil -lrpwstlm -L$(INST_NAT_LIB) -lfg -lm

##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %,$(BUILD_OBJ)/%,$(OBJS))


##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(BUILD_OBJ)/%.o:%.c
	cc $(CFLAGS) -c $< -o $@

##############################################################################
# Explicit Rules and Dependencies

$(BUILD_BIN)/rpws_file:$(BUILD_OBJS)
	cc -o $(BUILD_BIN)/rpws_file $(BUILD_OBJS) $(LFLAGS)

clean:
	rm $(BUILD_OBJS)

test: $(BIN)/rpws_file58
	@echo "rpws_file unit test not yet defined!"
