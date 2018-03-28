# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

export INST_NAT_BIN
export SETUP_FILE

##############################################################################
# Stuff to build/install

SCRIPT_SRCS=rpws_push_sopc.sh.in rpws_push_wkill.sh.in \
 rpws_push_wcron.sh.in rpws_push_wpatch.sh.in rpws_push_wshow.sh.in \
 rpws_sfdu2mp.sh.in rpws_push_autorun.sh.in
 
 # rpws_sort_merge_wcron.ksh.in rpws_sort_merge_sopc.sh.in
 # rpws_sort_merge_wkill.ksh.in 

##############################################################################
# Automatic Variables

BUILD_SCRIPTS = $(patsubst %.in, $(BIN_DIR)/%, $(SCRIPT_SRCS))


##############################################################################
# Pattern Rules

$(BIN_DIR)/%.sh:%.sh
	cp $< $@
	chmod +x $@

$(BIN_DIR)/%.sh:%.sh.in
	./envsubst.py $< $@
	chmod +x $@

$(BIN_DIR)/%.ksh:%.ksh.in
	./envsubst.py $< $@
	chmod +x $@

$(BIN_DIR)/%.pl:%.pl.in
	./envsubst.py $< $@
	chmod +x $@

##############################################################################
# Explicit rules

all: $(BIN_DIR) $(BUILD_SCRIPTS)

$(BIN_DIR):
	mkdir -p $(BIN_DIR)
	
clean:
	rm $(BUILD_SCRIPTS)
	
