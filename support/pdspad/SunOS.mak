
# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Specific Defs

CC = cc
CFLAGS = -xc99 -g
LFLAGS = 

##############################################################################
# Explicit Rules and dependencies

all: $(BUILD_DIR) $(BUILD_DIR)/pdspad

install: $(INST_NAT_BIN)/pdspad

$(BUILD_DIR):
	mkdir $(BUILD_DIR)

$(BUILD_DIR)/pdspad:pdspad.c
	$(CC) $< $(CFLAGS) $(LFLAGS) -o $@

$(INST_NAT_BIN)/pdspad:$(BUILD_DIR)/pdspad
	install -D -m 755 $< $@
	
distclean:
	rm -r $(BUILD_DIR)
	
clean:
	@echo "Nothing to clean"

test:
	@echo "Unit test not yet defined"
