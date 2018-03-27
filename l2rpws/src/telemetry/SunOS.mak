##############################################################################
# Generics

# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Lib Specific Defs

OBJS= delay.o instrument.o iopacket.o record.o telemetry.o
TARG= libtelemetry.a

##############################################################################
# Sun specific stuff

CC = cc 

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CFLAGS = -Xc -v -I$(BUILD_INC) $(LFS_CFLAGS)

#RAJCAS_INCL='-I/home/raj/project/Cassini/include'


##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %,$(BUILD_OBJ)/%,$(OBJS))

$(BUILD_OBJ)/%.o:%.c
	$(CC) $(CFLAGS) -c $< -o $@

#.c.o:
#	$(CC) -c $(RAJCAS_INCL) $<



##############################################################################
# Explicit Rules

$(BUILD_BIN)/$(TARG): $(BUILD_OBJS)
	ar r $@ $(BUILD_OBJS)  

#delay.o: delay.c	# special case for select( ) system call 
#	$(CC) -c delay.c $(CASRAJ_INCL)

clean:
	rm -f $(BUILD_OBJS)

$(BUILD_OBJ)/instrument.o: instrument.c
$(BUILD_OBJ)/iopacket.o: iopacket.c
$(BUILD_OBJ)/record.o: record.c
$(BUILD_OBJ)/telemetry.o: telemetry.c

