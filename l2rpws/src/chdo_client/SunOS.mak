# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Specific Defs

#CFLAGSx=-Ae -s  -I$(INC) -I$(X11)

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CFLAGS=-Ae  -g -I$(BUILD_INC) -I$(INST_INC) $(LFS_CFLAGS)

CC = cc

OBJS =  chdo_client_read.o \
	chdo_client_read_1.o \
	chdo_client_read_2.o \
	chdo_listner.o \
	chdo_common.o \
	socket_calls.o

LFLAGS=-L $(BUILD_LIB) -lutil -L $(INST_NAT_LIB) -lfg -lsocket -lnsl 

##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %,$(BUILD_OBJ)/%,$(OBJS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(BUILD_OBJ)/%.o:%.c
	cc $(CFLAGS) -c $< -o $@

##############################################################################
# Explicit Rules and dependencies

all: $(BUILD_BIN)/rpws_chdo_client_read2 $(BUILD_BIN)/rpws_chdo_listener

$(BUILD_BIN)/rpws_chdo_client_read2:$(BUILD_OBJS)
	cc -o $@ $(BUILD_OBJ)/chdo_client_read_2.o $(BUILD_OBJ)/chdo_common.o $(BUILD_OBJ)/socket_calls.o $(CFLAGS) $(LFLAGS)

$(BUILD_BIN)/rpws_chdo_listener:$(BUILD_OBJS)
	cc -o $@ $(BUILD_OBJ)/chdo_listner.o $(BUILD_OBJ)/chdo_common.o $(BUILD_OBJ)/socket_calls.o $(CFLAGS) $(LFLAGS)
	
clean:
	rm -f $(BUILD_OBJS)

test:
	@echo "No unit test defined for rpws_chdo_listener"

