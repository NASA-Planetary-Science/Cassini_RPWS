##############################################################################
# Lib Specific Defs

OBJS= delay.o instrument.o iopacket.o record.o telemetry.o
TARG= libtelemetry.a

##############################################################################
# Sun specific stuff

CC = gcc -std=c99

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CFLAGS = -ggdb -Wall -I$(BUILD_INC) $(LFS_CFLAGS)


##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %,$(BUILD_OBJ)/%,$(OBJS))

$(BUILD_OBJ)/%.o:%.c | $(BUILD_OBJ)
	$(CC) $(CFLAGS) -c $< -o $@

#.c.o:
#	$(CC) -c $(RAJCAS_INCL) $<



##############################################################################
# Explicit Rules

$(BUILD_BIN)/$(TARG): $(BUILD_OBJS)
	ar r $@ $(BUILD_OBJS)  

$(BUILD_OBJ):
	mkdir -p $(BUILD_OBJ)

clean:
	rm -f $(BUILD_OBJS)

$(BUILD_OBJ)/instrument.o: instrument.c
$(BUILD_OBJ)/iopacket.o: iopacket.c
$(BUILD_OBJ)/record.o: record.c
$(BUILD_OBJ)/telemetry.o: telemetry.c

