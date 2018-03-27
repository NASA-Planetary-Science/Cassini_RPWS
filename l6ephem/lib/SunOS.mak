###############################################################################
# Platform Defs

CC = cc -g -xc99=all -errwarn

LFS_CFLAGS=$(shell getconf LFS_CFLAGS) # Add large file support

CFLAGS = -I$(INST_INC) -I$(CSPICE_INC) $(LFS_CFLAGS)

LFLAGS = -L$(INST_NAT_LIB) $(CSPICE_LIB) -lm

##############################################################################
# Pattern Defs

OBJS=CasSpice.o CasTables.o

BUILD_OBJS=$(patsubst %,$(BUILD_DIR)/%,$(OBJS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(BUILD_DIR)/%.o:%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@
	
$(INST_INC)/%.h:%.h
	install -D -m 644 $< $@

$(INST_INC)/casephem/%.h:%.h
	install -D -m 644 $< $@
	
$(INST_NAT_LIB)/%.a:$(BUILD_DIR)/%.a
	install -D -m 644 $< $@

##############################################################################
# Explicit Rules and dependencies

$(BUILD_DIR)/libcasephem.a:$(BUILD_OBJS)
	ar -r $@ $^

$(BUILD_DIR):
	@if [ ! -d $(BUILD_DIR) ] ; then mkdir $(BUILD_DIR); fi


install:$(INST_INC)/Cext.h $(INST_INC)/casephem/CasSpice.h \
  $(INST_INC)/casephem/CasTables.h $(INST_NAT_LIB)/libcasephem.a
 
test:
	@echo "No make tests defined"

clean:
	rm $(BUILD_DIR)/libcasephem.a $(BUILD_OBJS)
