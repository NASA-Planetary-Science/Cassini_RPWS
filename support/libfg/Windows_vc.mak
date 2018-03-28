# A GNU Makefile.  To get gnu make for windows goto:
# 
#   http://gnuwin32.sourceforge.net/  
#
# and get the 'make' package.  
#
# This makefile expects a working uname command, to get this goto the
# site above and get the 'CoreUtils' package.
#
# makefile for libfg.a on WindowsXP using windows native command line tools
# you can get these by downloading "Visual Studio 2008 Express"
# To get the tools on to you path run the included vcvars32.bat file before
# attempting to run this program.
#
# DO NOT RUN under Cygwin!!!  This expects a native windows compliler
# as well as the CMD.EXE shell.

##############################################################################
# Generic Definitions

CC=cl
SHELL=cmd.exe

# Pick a default install location, if user doesn't have one defined
ifeq ($(PREFIX),)
PREFIX=$(USERPROFILE)
endif

INST_LIB=$(PREFIX)\lib
INST_INC=$(PREFIX)\include

# Have a seperate build directory for each arch so that we won't accidentally
# install libs for the wrong platform
ARCH=$(shell uname -s).$(shell uname -m)
BUILD_DIR:=build.$(ARCH)

##############################################################################
# Specific Definitions

CFLAGS=/Zi /Wall /nologo

SRCS=fg.c
HDRS=fg.h

OBJS= $(patsubst %.c, $(BUILD_DIR)/%.obj, $(SRCS))

##############################################################################
# Pattern rules

.SUFFIXES:

$(BUILD_DIR)/%.obj:src/%.c |  $(BUILD_DIR)
	$(CC) $(CFLAGS) /c $< /Fo$@ /Fd$@.pdb

##############################################################################
# Explicit rules

all: $(BUILD_DIR) $(BUILD_DIR)/fg.lib

$(BUILD_DIR):
	if not exist $(BUILD_DIR) mkdir $(BUILD_DIR)
	
$(BUILD_DIR)/fg.lib: $(OBJS)
	lib.exe /nologo /verbose /out:$@ $(OBJS)

install:
	@if not exist "$(INST_LIB)" mkdir "$(INST_LIB)"
	copy /Y $(BUILD_DIR)\fg.lib "$(INST_LIB)\fg.lib"
	@if not exist "$(INST_INC)" mkdir "$(INST_INC)"
	copy /Y src\fg.h "$(INST_INC)\fg.h"
	
clean:
	del $(OBJS)
	
distclean:
	rmdir /S /Q $(BUILD_DIR)