# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Compiler Defs

DBGR=dbx
CC=cc 
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) # Add large file support
DEFINES='-DRPWS_SUPERVOL="$(RPWS_SUPERVOL)"'
CFLAGS= -xc99 -errwarn -g $(DEFINES) -I$(INST_INC) $(LFS_CFLAGS)
LDFLAGS= -L$(INST_NAT_LIB) -ldas2 -lz -lexpat -lm


##############################################################################
# Pattern Rules

.SUFFIXES: .c .o
$(BUILD_DIR)/%.o:%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@
	
	
##############################################################################
# Explicit Rules and dependencies

$(BUILD_DIR)/rpws_kp_das2rdr: $(BUILD_DIR)/rpws_l4kp_das2rdr.o $(BUILD_DIR)/dbase.o
	$(CC) $(CFLAGS) $^ $(LDFLAGS) -o $@ 

$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# the KP reader outputs progress information which can happen at indetermined
# time intervals, thus you can't easily compare the output, so testing is 
# broken
# TODO:  Remove progress comments from output
test_cant: $(BUILD_DIR)/out.d2s $(BUILD_DIR)/rpws_kp_das2rdr
	@ls -l $< test/test_out_be_2000-02-06_2000-02-07T02-00.d2s
	@cmp $< test/test_out_be_2000-02-06_2000-02-07T02-00.d2s

$(BUILD_DIR)/out.d2s:$(BUILD_DIR)/rpws_kp_das2rdr
	$< $(RPWS_SUPERVOL)/DATA/RPWS_KEY_PARAMETERS E "2000-02-06" "2000-02-07 02:00" > $@
	

install: $(INST_NAT_BIN)/rpws_kp_das2rdr

$(INST_NAT_BIN)/rpws_kp_das2rdr:$(BUILD_DIR)/rpws_kp_das2rdr
	install -D -m 775 $< $@

checkCore: cassiniKp
	unlimit coredumpsize
	make test
	$(DBGR) cassiniKp core
    
clean:
	rm $(BUILD_DIR)/*.o

distclean:
	rm -r $(BUILD_DIR)
