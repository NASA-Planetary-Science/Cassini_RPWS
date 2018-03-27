##############################################################################
# Platform Defs

CC = gcc

CFLAGS = -std=c99 -Wall -Werror -Wno-unused -g -I../lib

LFLAGS = -L$(BUILD_DIR) -lcasephem $(CSPICE_LIB) -lm

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(INST_NAT_BIN)/%:$(BUILD_DIR)/%
	install -D -m 775 $< $@


##############################################################################

build: $(BUILD_DIR)/cas_sclk $(BUILD_DIR)/cas_phase $(BUILD_DIR)/cas_target


$(BULID_DIR):
	@if [ ! -d $(BUILD_DIR) ] ; then mkdir $(BUILD_DIR); fi

$(BUILD_DIR)/cas_sclk:casclk.c
	$(CC) $< $(CFLAGS) $(LFLAGS) -o $@

$(BUILD_DIR)/cas_phase:casmis.c
	$(CC) $^ $(CFLAGS) $(LFLAGS) -o $@ 
	
$(BUILD_DIR)/cas_target:castar.c
	$(CC) $^ $(CFLAGS) $(LFLAGS) -o $@ 


install: $(INST_NAT_BIN)/cas_sclk $(INST_NAT_BIN)/cas_phase \
 $(INST_NAT_BIN)/cas_target

clean:
	rm $(BUILD_DIR)
