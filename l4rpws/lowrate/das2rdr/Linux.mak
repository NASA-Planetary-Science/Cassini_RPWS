##############################################################################
# Compiler Defs

CC=gcc 
DBGR=gdb
CFLAGS= -std=c99 -Wall -Werror -g -I$(INST_INC) '-DRPWS_SUPERVOL="$(RPWS_SUPERVOL)"'
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
	@ls -l $< test/test_out_le_2000-02-06_2000-02-07T02-00.d2s
	@diff -a -q $< test/test_out_le_2000-02-06_2000-02-07T02-00.d2s
	@echo Success

$(BUILD_DIR)/out.d2s:$(BUILD_DIR)/rpws_kp_das2rdr
	$< $(RPWS_SUPERVOL)/DATA/RPWS_KEY_PARAMETERS E "2000-02-06" "2000-02-07 02:00" > $@
	


install: $(INST_NAT_BIN)/rpws_kp_das2rdr

$(INST_NAT_BIN)/rpws_kp_das2rdr:$(BUILD_DIR)/rpws_kp_das2rdr
	install -D -m 775 $< $@

checkCore: cassiniKp
	unlimit coredumpsize
	make test
	BDGR cassiniKp core
    
clean:
	-rm $(BUILD_DIR)/*.o

distclean:
	-rm -r $(BUILD_DIR)
