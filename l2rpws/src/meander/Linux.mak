##############################################################################
# Platform Defs

CC=gcc

CFLAGS=-g -Wall -std=c99 -I$(INST_INC) -I$(BUILD_INC) -I$(CSPICE_INC) $(LFS_CFLAGS)

LFLAGS=-L$(LIB_DIR) -lrpwstlm -L$(INST_NAT_LIB) -lcasephem $(CSPICE_LIB) -lm

##############################################################################
# Pattern Defs

OBJS= meander.o

BUILD_OBJS=$(patsubst %,$(OBJ_DIR)/%,$(OBJS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(OBJ_DIR)/%.o:%.c | $(OBJ_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

##############################################################################
# Explicit Rules and Dependencies

$(BIN_DIR)/rpws_meander:$(BUILD_OBJS)
	$(CC) $(CFLAGS) $(BUILD_OBJS) $(LFLAGS) -o $@

clean:
	rm $(BUILD_OBJS)

test:  $(BIN_DIR)/rpws_meander
	@echo "rpws_meander unit test is not yet defined!"
	
$(OBJ_DIR):
	mkdir -p $(OBJ_DIR)
