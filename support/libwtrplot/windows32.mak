# A GNU Makefile.  To get gnu make for windows goto:
# 
#   http://gnuwin32.sourceforge.net/  
#
# and get the 'make' package.  
#
# This makefile expects a working uname command, to get this goto the
# site above and get the 'CoreUtils' package.
#
# makefile for Willy's plotting library on WindowsXP using windows native
# command line tools.  You can get these by downloading "Visual Studio 2008
# Express"  To get the tools on to you path run the included vcvars32.bat
# file before attempting to run this program.
#
# DO NOT RUN under Cygwin!!!  This expects a native windows compliler
# as well as the CMD.EXE shell.

##############################################################################
# Generic Definitions

CC=cl.exe
SHELL=cmd.exe

# Pick a default install location, if user doesn't have one defined
ifeq ($(PREFIX),)
PREFIX=$(USERPROFILE)
endif

INST_BIN=$(PREFIX)\bin
INST_INC=$(PREFIX)\include
INST_LIB=$(PREFIX)\lib

# Have a seperate build directory for each arch so that we won't accidentally
# install libs for the wrong platform
ARCH=$(shell uname -s).$(shell uname -m)
BUILD_DIR:=build.$(ARCH)

##############################################################################
# Specific Definitions

SRCS=plot.c ps_plot.c pcl_plot.c hp_plot.c set.c plotinit.c utility.c \
  symbol.c la.c
  
HDRS=wtrplot.h

TARG=wtrplot.lib

OBJS= $(patsubst %.c, $(BUILD_DIR)/%.obj, $(SRCS))

##############################################################################
# Specific Definitions

CFLAGS=/Zi /nologo /I"$(INST_INC)"

LFLAGS=/link /LIBPATH:"$(INST_LIB)" 

##############################################################################
# Pattern rules

.SUFFIXES:

$(BUILD_DIR)/%.obj:src/%.c |  $(BUILD_DIR)
	$(CC) $(CFLAGS) /c $< /Fo$@ /Fd$@.pdb

##############################################################################
# Explicit rules

all: $(BUILD_DIR) $(BUILD_DIR)/$(TARG)

$(BUILD_DIR):
	if not exist $(BUILD_DIR) mkdir $(BUILD_DIR)

$(BUILD_DIR)/$(TARG): $(OBJS)
	lib.exe /nologo /verbose  /out:$@ $(OBJS)	
	
install:
	@if not exist "$(INST_LIB)" mkdir "$(INST_LIB)"
	copy /Y $(BUILD_DIR)\wtrplot.lib "$(INST_LIB)\wtrplot.lib"
	@if not exist "$(INST_INC)" mkdir "$(INST_INC)"
	copy /Y src\wtrplot.h "$(INST_INC)\wtrplot.h"

clean:
	del $(OBJS)
	
distclean:
	rmdir /S /Q $(BUILD_DIR)	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	