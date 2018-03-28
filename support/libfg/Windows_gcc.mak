##############################################################################
# A native Windows GNU Makefile.
#
# The preferred way to get gnu make for windows is to download as part of a
# mingw distribution.  You can get mingw in many places, if you are not doing
# graphical development you can grab it from:
#
# http://tdm-gcc.tdragon.net/
#
# A better source is the Qt GUI library with bundled python and mingw compiler
# at:
#
# http://qt-project.org/downloads
#
# Always use mingw32-make (or equivalent) never simple make.exe that is supplied
# with MSYS among other places.  This is a NATIVE makefile that uses CMD.EXE
# for the shell, don't run it under Cygwin!

##############################################################################
# Generic Definitions

CC=gcc
SHELL=cmd.exe

##############################################################################
# Specific Definitions

CFLAGS=-g -Wall -std=c99

SRCS=fg.c
HDRS=fg.h

OBJS= $(patsubst %.c, $(BUILD_DIR)/%.obj, $(SRCS))

##############################################################################
# Pattern rules

.SUFFIXES:

# Generic build rule
$(BUILD_DIR)/%.obj:src/%.c |  $(BUILD_DIR)
	$(CC) -c $(CFLAGS) -o $@ $<
	
# Header install rule
$(INST_INC)/%.h:src/%.h 
	install $< $@

# Lib install rule
$(INST_NAT_LIB)/%.a:$(BUILD_DIR)/%.a | $(BUILD_DIR)
	install $< $@


##############################################################################
# Explicit rules

all: $(BUILD_DIR) $(BUILD_DIR)/libfg.a

$(BUILD_DIR):
	if not exist $(BUILD_DIR) mkdir $(BUILD_DIR)
	
$(BUILD_DIR)/libfg.a: $(OBJS)
	ar rv $@ $^

install:$(INST_INC)/fg.h $(INST_NAT_LIB)/libfg.a
	
	
clean:
	del $(OBJS)
	
distclean:
	rmdir /S /Q $(BUILD_DIR)