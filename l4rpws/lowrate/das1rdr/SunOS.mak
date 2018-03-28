##############################################################################
# Use gnu make on the suns

# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Exports needed for substitutions

export INST_ETC
export RPWS_SUPERVOL
export INST_NAT_BIN

##############################################################################
# Files

# Don't know why casdfr.c is not included here
SRCS=rpws_l4kp_das1rdr.c

SCRIPTS=rpws_kp_reader.sh.in

DSDFS=rpws_kp.dsdf.in

##############################################################################
# Specific Defs

CC = cc
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) # Add large file support
CFLAGS = -xc99 -errwarn -g -I$(INST_INC) $(LFS_CFLAGS)
LFLAGS= -L$(INST_NAT_LIB) -ldas2 -lm 


##############################################################################
# Pattern Rules

.SUFFIXES: .c .o
	
$(BD)/%.sh:%.sh.in | $(BD)
	./envsubst.py $< $@
	chmod +x $@

$(BD)/%.dsdf:%.dsdf.in | $(BD)
	./envsubst.py $< $@

# Installing das1 dsdfs
$(DAS_DATASETROOT)/rpws/%.dsdf:$(BD)/%.dsdf
	umask 0002 && install -D -m 664 $< $@

# Installing programs and scripts
$(INST_NAT_BIN)/%:$(BD)/%
	install -D -m 755 $< $@


##############################################################################
# Explicit Rules and dependencies

build: $(BD) $(BD)/rpws_kp_reader $(BD)/rpws_kp_reader.sh $(BD)/rpws_kp.dsdf

$(BD):
	mkdir -p $(BD)

$(BD)/rpws_kp_reader: rpws_l4kp_das1rdr.c
	$(CC) $^ $(CFLAGS) $(LFLAGS) -o $@

test:
	@echo "No unit tests defined for this program"


install:$(DAS_DATASETROOT)/rpws/rpws_kp.dsdf \
 $(INST_NAT_BIN)/rpws_kp_reader $(INST_NAT_BIN)/rpws_kp_reader.sh

clean:
	rm $(BUILD_OBJS)
	
distclean:
	rm $(BD)/rpws_kp_reader $(BD)/rpws_kp_reader.sh $(BD)/rpws_kp.dsdf




