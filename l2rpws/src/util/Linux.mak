##############################################################################
# Specific Defs

CC = gcc

GCCOPT = -std=c99 -g -Wall -Wshadow -Wpointer-arith -Wcast-qual \
         -Wcast-align -Wwrite-strings -Waggregate-return \
         -Wnested-externs -Wimplicit -Wredundant-decls


# Include area has fg.h which we need
CFLAGS= $(GCCOPT) '-DCFG="$(INST_ETC)"' -I$(BUILD_INC) -I$(INST_INC)

BADOBJ =	UTIL_mpus.o UTIL_DCC_decompress.o

OBJS = miniproc.o util.o utild.o utiloffset.o UTIL_MFR_decompress.o \
       UTIL_DUST_decompress.o UTIL_DCP_decompress.o UTIL_event_time.o \
       UTIL_status.o utilfile.o

# I wish they had picked a naming scheme that involved more that changing
# the capitalization of file names and libraries
OBJS2 =         Util.o Util_event_time.o

LIB_TARGS = libutil.a libUtil.a
INC_TARGS = rtiu.h util.h decomp.h UTIL_DCC_decompress.h

##############################################################################
# Pattern Defs

BUILD_LIBS = $(patsubst %,$(BUILD_LIB)/%,$(LIB_TARGS))
#INST_LIBS = $(patsubst $(BUILD_LIB)/%,$(INST_LIB)/%,$(BUILD_LIBS))

BUILD_HDRS = $(patsubst %,$(BUILD_INC)/%,$(INC_TARGS))
#INST_HDRS = $(patsubst $(BUILD_INC)/%,$(INST_INC)/%,$(BUILD_HDRS))

BUILD_OBJS=$(patsubst %,$(BUILD_OBJ)/%,$(OBJS))
BUILD_OBJS2=$(patsubst %,$(BUILD_OBJ)/%,$(OBJS2))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o .a

$(BUILD_OBJ)/%.o:%.c
	$(CC) $(CFLAGS) -c $< -o $@
	
$(INST_LIB)/%.a:$(BUILD_LIB)/%.a
	install -m 664 $< $@

$(INST_INC)/%:$(BUILD_INC)/%
	install -m 664 $< $@


##############################################################################
# Explicit Rules

all: $(BUILD_LIBS)

#install: $(INST_LIBS) $(INST_HDRS)
	
$(BUILD_LIB)/libutil.a:  $(BUILD_OBJS)
	ar -r $@ $^

$(BUILD_LIB)/libUtil.a: $(BUILD_OBJS2)
	ar -r $@ $^
		
$(BUILD_OBJ)/miniproc.o:   miniproc.c $(BUILD_INC)/rtiu.h
$(BUILD_OBJ)/utilfile.o:   utilfile.c $(BUILD_INC)/util.h
$(BUILD_OBJ)/util.o: util.c $(BUILD_INC)/util.h $(BUILD_INC)/decomp.h $(BUILD_INC)/rtiu.h
$(BUILD_OBJ)/utild.o:      utild.c $(BUILD_INC)/util.h $(BUILD_INC)/decomp.h $(BUILD_INC)/rtiu.h
$(BUILD_OBJ)/utiloffset.o:      utiloffset.c
$(BUILD_OBJ)/fg.o:         fg.c
$(BUILD_OBJ)/UTIL_DCP_decompress.o: \
   UTIL_DCP_decompress.c $(BUILD_INC)/rtiu.h $(BUILD_INC)/util.h \
   $(BUILD_INC)/UTIL_DCC_decompress.h

$(BUILD_OBJ)/UTIL_MFR_decompress.o: UTIL_MFR_decompress.c $(BUILD_INC)/rtiu.h

$(BUILD_OBJ)/UTIL_DUST_decompress.o: UTIL_DUST_decompress.c $(BUILD_INC)/rtiu.h

$(BUILD_OBJ)/UTIL_event_time.o: UTIL_event_time.c $(BUILD_INC)/rtiu.h

# UTIL_align_on_second.o: UTIL_align_on_second.c $(BUILD_INC)/rtiu.h

clean:
	rm $(BUILD_OBJS) $(BUILD_OBJS2)

test:
	@echo "libutil.a unit test not yet defined!"
