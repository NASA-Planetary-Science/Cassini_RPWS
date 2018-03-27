##############################################################################
# Generics

LRFC_SCRIPT_PATH=$(INST_NAT_BIN):/usr/bin
export LRFC_SCRIPT_PATH


##############################################################################
# Specific Defs

CC = gcc
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) # Add large file support
INC= -I$(BUILD_DIR) -I../lib -I$(INST_INC)  
CFLAGS = -std=c99 -g $(INC) $(LFS_CFLAGS)

LFLAGS= $(BUILD_DIR)/libcasephem.a -L$(INST_NAT_LIB) -ldas2 $(CSPICE_LIB) -lm 

##############################################################################
# Pattern Rules

$(INST_NAT_BIN)/%:$(BUILD_DIR)/%
	install -D -m 775 $< $@

##############################################################################
# Explicit Rules and dependencies

build: $(BUILD_DIR) $(BUILD_DIR)/cas_orbit $(BUILD_DIR)/cas_orbit.ksh

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

$(BUILD_DIR)/cas_orbit:casorb.c $(BUILD_DIR)/casorb.h
	$(CC) $< $(CFLAGS) $(LFLAGS) -o $@
	
$(BUILD_DIR)/cas_orbit.ksh:cas_orbit.ksh.in
	./envsubst.py $< $@
	-chmod +x $@

$(BUILD_DIR)/casorb.h:
	@if test ! -f "$(INST_ETC)/OrbitNames.list" ; then \
	   echo "$(INST_ETC)/OrbitNames.list is missing, can't make casorb.h"; \
		exit 13; \
	else \
		echo "./orb2hdr.ksh > $(BUILD_DIR)/casorb.h" ;\
	   ./orb2hdr.ksh > $(BUILD_DIR)/casorb.h ; \
	fi

test:
	@echo "Unit test not yet defined"
	
install:$(INST_NAT_BIN)/cas_orbit $(INST_NAT_BIN)/cas_orbit.ksh
	
clean:
	rm $(BUILD_DIR)/cas_orbit $(BUILD_DIR)/casorb.h
