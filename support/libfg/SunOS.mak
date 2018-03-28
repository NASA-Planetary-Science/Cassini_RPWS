# makefile for libfg.a ( -L/local/lib -lfg )

# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

# makefile for libfg.a on Solaris 10, assumes gmake
CC=cc
CFLAGS=-xc99 -errwarn -O

SRCS=fg.c
HDRS=fg.h

OBJS= $(patsubst %.c, $(BUILD_DIR)/%.o, $(SRCS))

##############################################################################
# Pattern rules

.SUFFIXES:

$(BUILD_DIR)/%.o:src/%.c |  $(BUILD_DIR)
	$(CC) -c $(CFLAGS) -o $@ $<


##############################################################################
# Explicit rules

all: $(BUILD_DIR)/libfg.a

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/libfg.a: $(OBJS)
	ar rv $@ $(OBJS)

install: $(INST_NAT_LIB)/libfg.a $(INST_INC)/fg.h

$(INST_NAT_LIB)/libfg.a:$(BUILD_DIR)/libfg.a
	install -D -m 664 $< $@

$(INST_INC)/fg.h:src/fg.h
	install -D -m 664 $< $@

distclean:
	rm -f -r $(BUILD_DIR)


clean:
	rm -f -r $(BUILD_DIR)/*.o
