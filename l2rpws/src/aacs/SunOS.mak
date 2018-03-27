# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Platform Defs

CC = cc 

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS)
INCLUDES = -I$(CSPICE_INC) -I$(INST_INC) -I$(INC_DIR)
DEFINES  = -DCAS_EPHEMERIS_KERNELS=$(CAS_EPHEMERIS_KERNELS) \
 -DCAS_QUAT_DB=$(CAS_QUAT_DB)

CFLAGS = -xc99 -g $(LFS_CFLAGS) #-errwarn=%all

FC = f90 -f77 -ftrap=%none
FFLAGS = -g

LFLAGS = -L $(LIB_DIR) -lutil -lgllspice $(CSPICE_LIB) -lm

OBJS = makedbase.o quaternion.o getatt.o webutil.o gei2gse.o

##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %,$(OBJ_DIR)/%,$(OBJS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(OBJ_DIR)/%.o:%.c
	$(CC) $(CFLAGS) $(DEFINES) $(INCLUDES) -c $< -o $@
	
$(OBJ_DIR)/%.o:%.for
	$(FC) $(FFLAGS) -c $< -o $@


##############################################################################
# Explicit Rules and dependencies

all: $(BIN_DIR)/cas_quat_mkdb $(BIN_DIR)/cas_cgi_attitude

$(BIN_DIR)/cas_quat_mkdb:$(OBJ_DIR)/makedbase.o $(OBJ_DIR)/quaternion.o
	$(CC) -o $@ $^ $(LFLAGS)

$(BIN_DIR)/cas_cgi_attitude:$(OBJ_DIR)/getatt.o $(OBJ_DIR)/webutil.o \
 $(OBJ_DIR)/gei2gse.o
	$(CC) $^ $(LFLAGS) -ldas2 -o $@ 

clean:
	rm -f $(BUILD_OBJS)

test:
	@echo "No unit test defined for chdo_listener"
