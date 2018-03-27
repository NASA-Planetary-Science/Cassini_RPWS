##############################################################################
# Generics

# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

TOP=$(realpath ../../../../)
ARCH=$(shell uname -s).$(shell uname -p)
BUILD:=build.$(ARCH)


INC=$(TOP)/include
LIB=$(TOP)/$(BUILD)
BIN=$(TOP)/$(BUILD)
OBJ_DIR=$(TOP)/$(BUILD)/obj

##############################################################################
# Specific Defs

# -Xc: Strict conformant ISO C
# -errwarn=%all: Exit the compiler on warnings
# -fd: Report K&R functions

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CFLAGS = -g -Xc -errwarn=%all -fd -I$(INC) $(LFS_CFLAGS) 

CC = cc

OBJS= Cext.o Sleep.o

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

$(BIN)/libCext.a: $(BUILD_OBJS)
	ar r $@ $(BUILD_OBJS)

#$(OBJ_DIR)/Cext.o: Cext.c
	
$(OBJ_DIR)/Sleep.o: Sleep.c   # Do not compile to ANSI standards, need different time.h
	$(CC) -Ae -c $< -o $@

test:
	@echo "No unit test defined for Cext!"

#test:$(BIN)/test_cext

#$(BIN)/test_cext:$(OBJ_DIR)/test_cext.o $(BUILD_OBJS)
#	$(CC) $(CFLAGS) -o $@  $(OBJ_DIR)/test_cext.o $(BUILD_OBJS)
#	$@

clean:
	rm $(BUILD_OBJS)

