# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Specific Defs

#CFLAGSx=-Ae -s  -I$(INC) -I$(X11)
#CFLAGS=-Ae  -g -I$(INC) -I$(X11)

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CFLAGS = -Ae -g -I$(BUILD_INC) -I$(INST_INC) $(LFS_CFLAGS)
CC = cc 

#OBJS = chdo1.o chdo2.o chdo3.o chdo5.o chdo6.o chdopipe.o
OBJS = chdo7.o chdopipe.o

LFLAGS = -L $(BUILD_LIB) -L $(INST_NAT_LIB) -lutil -lfg -lcurses

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


$(BUILD_BIN)/rpws_chdo:$(BUILD_OBJS)
	$(CC) -o $@ $(BUILD_OBJ)/chdopipe.o $(BUILD_OBJ)/chdo7.o $(LFLAGS)

clean:
	rm -f $(BUILD_OBJS)

test:
	@echo "No unit test defined for chdo_listener"
