##############################################################################
# Use GNU make 

ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif


##############################################################################
# Specific Defs

CC = cc -errwarn=%all -xc99=all

LFS_CFLAGS=$(shell getconf LFS_CFLAGS) # Add large file support

CFLAGS = -g $(LFS_CFLAGS)

LFLAGS = -lm


##############################################################################
# Explicit Rules and dependencies

$(BUILD_DIR)/rpws_lr_list: lrf_list.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) $< -o $@ $(CFLAGS) $(LFLAGS)

test: $(TEST_DIR) $(BUILD_DIR)/rpws_lr_list
	@$(BUILD_DIR)/rpws_lr_list TEST_INPUT.DAT > $(TEST_DIR)/rpws_lr_list_test.txt
	@if diff TEST_OUTPUT.TXT $(TEST_DIR)/rpws_lr_list_test.txt > /dev/null ; then \
	  echo rpws_lr_list: Test [  OK  ]; \
	else \
	  echo rpws_lr_list: Test [FAILED] Output not equal to TEST_OUTPUT.TXT; \
	fi

clean:
	-rm $(BUILD_DIR)/rpws_lr_list
	-rm $(TEST_DIR)/rpws_lr_list_test.txt


##############################################################################
# Rules for making $(BUILD) and $(TEST) if they don't exist

$(BUILD_DIR):
	@if [ ! -d $(BUILD_DIR) ] ; then mkdir $(BUILD_DIR); fi

$(TEST_DIR):
	@if [ ! -d $(TEST_DIR) ] ; then mkdir $(TEST_DIR); fi
