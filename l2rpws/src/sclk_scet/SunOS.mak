##############################################################################
# Generics

# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif


# Pick a default installed locations, if user doesn't have them defiend
ifeq ($(PREFIX),)
PREFIX=$(HOME)
endif
ifndef INST_LIB
INST_LIB=$(PREFIX)/lib
endif
ifndef INST_ETC
INST_ETC=$(PREFIX)/etc
endif
ifndef INST_INC
INST_INC=$(PREFIX)/include
endif


TOP=$(realpath ../../)
ARCH=$(shell uname -s).$(shell uname -p)
BUILD:=build.$(ARCH)


BUILD_INC=$(TOP)/include
BUILD_LIB=$(TOP)/$(BUILD)
BUILD_BIN=$(TOP)/$(BUILD)
OBJ_DIR=$(TOP)/$(BUILD)/obj



##############################################################################
# Specific Defs

# -Xc: Strict conformant ISO C
# -errwarn=%all: Exit the compiler on warnings
# -fd: Report K&R functions


DEFINES= '-DCFG="$(INST_ETC)"'

LFS_CFLAGS=$(shell getconf LFS_CFLAGS)

CFLAGS = -g -xc99 -Xc -errwarn=%all -fd $(DEFINES) -I$(BUILD_INC) \
 -I/local/naif/cspice/include $(LFS_CFLAGS)

CC = cc

#INCL= -I/home/raj/project/include -I /home/raj/project/spice/include

#INCL= -I$(CAS_DIR)/include -I$(CAS_DIR)/spice/include

OBJ_TARGS= cssscet.o csssclk.o

##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %, $(OBJ_DIR)/%, $(OBJ_TARGS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(OBJ_DIR)/%.o:%.c
	cc $(CFLAGS) -c $< -o $@
	
##############################################################################
# Explicit Rules and Dependencies


build: $(BUILD_OBJS)


clean:
	rm $(BUILD_OBJS)
	
test: $(BUILD_BIN)/libCasTlm.a
	@echo "castlm unit test not yet defined!"

cssscet.o:cssscet.c
csssclk.o:csssclk.c
