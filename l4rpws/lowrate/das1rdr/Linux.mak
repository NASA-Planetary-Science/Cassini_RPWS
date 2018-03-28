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

CC = gcc
CFLAGS = -std=c99 -Wall -Werror -g -I$(INST_INC)
LFLAGS= -L$(INST_NAT_LIB) -ldas2 -lm 

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o
	
$(BD)/%.sh:%.sh.in | $(BD)
	./envsubst.py $< $@
	chmod +x $@

# Building dsdfs
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

clean:
	-rm $(BUILD_OBJS)
	
install:$(DAS_DATASETROOT)/rpws/rpws_kp.dsdf \
 $(INST_NAT_BIN)/rpws_kp_reader $(INST_NAT_BIN)/rpws_kp_reader.sh


distclean:
	-rm -r $(BD)




