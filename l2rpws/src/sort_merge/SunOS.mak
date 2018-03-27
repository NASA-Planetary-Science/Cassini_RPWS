# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Platform Defs

DEFINES= '-DCFG="$(INST_ETC)"' '-DINST_BIN="$(INST_BIN)"'

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS)

CFLAGS=-xc99 -g $(DEFINES) -I$(INC_DIR) -I$(INST_INC) -I$(CSPICE_INC) \
 $(LFS_CFLAGS)

LFLAGS=-L$(LIB_DIR) -lcassort -lmdb -lutil -L $(INST_NAT_LIB) -lfg -lcurses \
 -ldas2 $(CSPICE_LIB) -lm

##############################################################################
# Pattern Defs

LIB_OBJS = sort.o
OBJS	= sort_merge.o spice.o read_raw.o write_raw.o help.o

BUILD_LIB_OBJS=$(patsubst %,$(OBJ_DIR)/%,$(LIB_OBJS))
BUILD_OBJS=$(patsubst %,$(OBJ_DIR)/%,$(OBJS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(OBJ_DIR)/%.o:%.c
	$(CC) $(CFLAGS) -c $< -o $@
	
$(MAN_DIR)/man1/%:% | $(MAN_DIR)/man1
	cp $< $@

##############################################################################
# Explicit Rules and dependencies


all:$(BIN_DIR)/libcassort.a $(BIN_DIR)/rpws_sort_merge \
  $(MAN_DIR)/man1/rpws_sort_merge.1

$(MAN_DIR)/man1:
	-mkdir -p $(MAN_DIR)/man1

lib:$(BIN_DIR)/libcassort.a

sort_merge:$(BIN_DIR)/sort_merge_2.2

$(LIB_DIR)/libcassort.a:$(BUILD_LIB_OBJS)
	@echo $^
	ar -r $@ $^

$(BIN_DIR)/rpws_sort_merge:$(BUILD_OBJS)
	@echo $(BUILD_OBJS)
	f77 -g -o $@ $^ $(LFLAGS)

$(OBJ_DIR)/sort.o: sort.c debug.h help.h read_raw.h sort_merge.h spice.h write_raw.h
$(OBJ_DIR)/sort_merge.o:sort_merge.c debug.h help.h read_raw.h sort_merge.h spice.h write_raw.h
$(OBJ_DIR)/read_raw.o:read_raw.c debug.h help.h read_raw.h sort_merge.h spice.h write_raw.h
$(OBJ_DIR)/write_raw.o:write_raw.c debug.h help.h read_raw.h sort_merge.h spice.h write_raw.h
$(OBJ_DIR)/spice.o: spice.c debug.h help.h read_raw.h sort_merge.h spice.h write_raw.h
$(OBJ_DIR)/help.o:help.c debug.h help.h read_raw.h sort_merge.h spice.h write_raw.h

clean:
	rm $(BUILD_OBJS)

