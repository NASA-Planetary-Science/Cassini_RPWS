# Pick a default install location, if user doesn't have one defiend
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Platform Defs

#ARC=$(TOP)/$(BUILD)/archive/script

TEMP=$(TOP)/archive/volume_template

MDB=/opt/project/cassini/src/mdb

VOLTMP=/opt/project/cassini/archive/volume_template/EXTRAS/SOFTWARE

DEFINES= '-DCFG="$(INST_ETC)"' '-DINST_BIN="$(INST_NAT_BIN)"'

#CFLAGSx=-Ae -s  -I$(INC)  -g
#CFLAGSx=-Ae -xprofile=tcov -I$(INC)

# Add large file support for Linux NFS compatibility
LFS_CFLAGS=$(shell getconf LFS_CFLAGS) 

CFLAGS=-xc99 -g $(DEFINES) -I$(INC_DIR) -I$(INST_INC) -I$(CSPICE_INC) $(LFS_CFLAGS)

CC = cc

OBJS = pds_format.o rpws_alarm.o rpws_archive_dump.o rpws_archive_example.o \
  rpws_archive.o rpws_browse.o rpws_duplicate_scan.o rpws_engineering.o \
  rpws_essay.o rpws_fsw.o rpws_fsw_ver.o rpws_help.o rpws_hfr_status.o \
  rpws_housekeeping_help.o rpws_housekeeping.o rpws_image.o \
  rpws_label_graphics.o rpws_label.o rpws_lp_status.o rpws_master_browse.o \
  rpws_time_patch_example.o rpws_time_patch.o rpws_timing.o 
	
STARTER_CFGS = names.tab

##############################################################################
# Pattern Defs

BUILD_OBJS=$(patsubst %,$(OBJ_DIR)/%,$(OBJS))

BULID_CFG=$(patsubst %,$(ETC_DIR)/%,$(STARTER_CFGS))

##############################################################################
# Pattern Rules

.SUFFIXES: .c .o

$(OBJ_DIR)/%.o:%.c
	$(CC) $(CFLAGS) -c $< -o $@
	
$(ETC_DIR)/%:%
	cp $< $@

##############################################################################
# Explicit Rules and dependencies

all: $(BIN_DIR)/pds_format $(BIN_DIR)/rpws_time_patch_example \
     $(BIN_DIR)/rpws_fsw $(BIN_DIR)/rpws_housekeeping $(BIN_DIR)/rpws_archive_dump \
     $(BIN_DIR)/lbl_ver $(BIN_DIR)/rpws_archive_example $(BIN_DIR)/rpws_archive \
     $(BIN_DIR)/rpws_housekeeping_new $(ETC_DIR)/names.tab

$(BIN_DIR)/pds_format:$(OBJ_DIR)/pds_format.o
	$(CC) -g -o $@ $<

$(BIN_DIR)/rpws_time_patch_example:$(OBJ_DIR)/rpws_time_patch_example.o $(OBJ_DIR)/rpws_time_patch.o
	$(CC) -g -o $@ $^ -L$(INST_NAT_LIB) -ldas2 $(CSPICE_LIB) -lm

$(BIN_DIR)/rpws_fsw:$(OBJ_DIR)/rpws_fsw.o $(OBJ_DIR)/rpws_fsw_ver.o $(LIB_DIR)/libmdb.a
	$(CC) -g -o $@ $^ -L$(LIB_DIR) -lmdb -lutil -L$(INST_NAT_LIB) -ldas2 $(CSPICE_LIB) -lm

$(BIN_DIR)/rpws_housekeeping:$(OBJ_DIR)/rpws_housekeeping.o \
 $(OBJ_DIR)/rpws_housekeeping_help.o $(OBJ_DIR)/rpws_engineering.o \
 $(OBJ_DIR)/rpws_alarm.o $(OBJ_DIR)/rpws_fsw_ver.o $(OBJ_DIR)/rpws_label.o \
 $(OBJ_DIR)/rpws_time_patch.o
	$(CC) -g -o $@ $^ -L$(LIB_DIR) -lmdb -lutil -L$(INST_NAT_LIB) -ldas2 $(CSPICE_LIB) -lm

$(BIN_DIR)/rpws_archive_dump:$(OBJ_DIR)/rpws_archive_dump.o 
	$(CC) -g -o $@ $^ -L$(LIB_DIR) -lutil -L$(INST_NAT_LIB) -lfg -ldas2 $(CSPICE_LIB) -lm

$(BIN_DIR)/lbl_ver:$(OBJ_DIR)/lbl_ver.o
	$(CC) -g -o $@ $<
	
$(BIN_DIR)/rpws_archive_example:$(OBJ_DIR)/rpws_archive_example.o
	$(CC) -g -o $@ $< $(CSPICE_LIB) -lm
	
$(BIN_DIR)/rpws_archive:$(OBJ_DIR)/rpws_archive.o $(LIB_DIR)/libmdb.a $(OBJ_DIR)/rpws_essay.o\
 $(OBJ_DIR)/rpws_timing.o $(OBJ_DIR)/rpws_label.o \
 $(OBJ_DIR)/rpws_browse.o $(OBJ_DIR)/rpws_master_browse.o $(OBJ_DIR)/rpws_duplicate_scan.o \
 $(OBJ_DIR)/rpws_fsw_ver.o $(OBJ_DIR)/rpws_lp_status.o $(OBJ_DIR)/rpws_hfr_status.o \
 $(OBJ_DIR)/rpws_label_graphics.o $(OBJ_DIR)/rpws_image.o $(OBJ_DIR)/rpws_time_patch.o \
 $(OBJ_DIR)/rpws_help.o 
	$(CC) -g -o $@ $^  -L$(LIB_DIR) -lmdb -lutil  -L$(INST_NAT_LIB) -lwtrplot -lfg -ldas2 $(CSPICE_LIB) -lm

$(BIN_DIR)/rpws_housekeeping_new: $(OBJ_DIR)/rpws_housekeeping.o \
  $(OBJ_DIR)/rpws_housekeeping_help.o $(OBJ_DIR)/rpws_engineering.o \
  $(OBJ_DIR)/rpws_alarm.o $(OBJ_DIR)/rpws_fsw_ver.o \
  $(OBJ_DIR)/rpws_label.o $(OBJ_DIR)/rpws_time_patch.o
	$(CC) -g -o $@  $^ -L$(LIB_DIR) -lmdb -lutil -L$(INST_NAT_LIB) -ldas2 $(CSPICE_LIB) -lm

# Dependencies
$(OBJ_DIR)/rpws_archive.o:rpws_archive.c rpws_label.c rpws_fsw_ver.h \
  rpws_lp_status.h rpws_hfr_status.h rpws_label.h rpws_archive_cdrom.h \
  rpws_archive_cdrom.h rpws_time_patch.h $(INC_DIR)/archive.h 
  
$(OBJ_DIR)/rpws_archive_dump.o:rpws_archive_dump.c rpws_fsw_ver.h \
  rpws_lp_status.h rpws_hfr_status.h rpws_label.h $(INC_DIR)/archive.h
  
$(OBJ_DIR)/pds_format.o:pds_format.c rpws_fsw_ver.h rpws_lp_status.h \
  rpws_hfr_status.h  rpws_label.h $(INC_DIR)/archive.h
  
$(OBJ_DIR)/rpws_essay.o:rpws_essay.c

$(OBJ_DIR)/rpws_status.o:rpws_status.c rpws_label.c rpws_fsw_ver.h \
 rpws_lp_status.h rpws_hfr_status.h rpws_label.h rpws_archive_cdrom.h \
 $(INC_DIR)/archive.h
 
$(OBJ_DIR)/rpws_timing.o:rpws_timing.c

$(OBJ_DIR)/rpws_time_patch.o:rpws_time_patch.c

$(OBJ_DIR)/rpws_time_patch_example.o:rpws_time_patch_example.c

$(OBJ_DIR)/rpws_image.o:rpws_image.c

$(OBJ_DIR)/rpws_label.o:rpws_label.c rpws_label.h

$(OBJ_DIR)/rpws_browse.o:rpws_browse.c rpws_browse.h

$(OBJ_DIR)/rpws_master_browse.o:rpws_master_browse.c rpws_master_browse.h

$(OBJ_DIR)/rpws_duplicate_scan.o:rpws_duplicate_scan.c rpws_duplicate_scan.h

$(OBJ_DIR)/rpws_label_graphics.o:rpws_label_graphics.c  rpws_label_graphics.h

$(OBJ_DIR)/rpws_fsw_ver.o:rpws_fsw_ver.c rpws_fsw_ver.h $(INC_DIR)/mdb_time.h

$(OBJ_DIR)/rpws_fsw.o:rpws_fsw.c rpws_fsw_ver.h $(INC_DIR)/mdb_time.h

$(OBJ_DIR)/rpws_hfr_status.o:rpws_hfr_status.c rpws_hfr_status.h

$(OBJ_DIR)/rpws_lp_status.o:rpws_lp_status.c rpws_lp_status.h

$(OBJ_DIR)/rpws_help.o:rpws_help.c rpws_help.h

$(OBJ_DIR)/rpws_housekeeping.o:rpws_housekeeping.c rpws_label.h rpws_housekeeping_help.c  

$(OBJ_DIR)/rpws_housekeeping_help.o:rpws_housekeeping_help.c

$(OBJ_DIR)/rpws_engineering.o:rpws_engineering.c rpws_engineering.h

$(OBJ_DIR)/rpws_alarm.o:rpws_alarm.c rpws_alarm.h

$(OBJ_DIR)/lbl_ver.o:lbl_ver.c


clean:
	rm $(BUILD_OBJS)

test:
	@echo "rpws_archive no unit tests defined!"


# Stuff from the previous makefile, needs to be handled at some point

#rpws_archive_example.o:rpws_archive_example.c rpws_archive_example_dust.c \
#   rpws_archive_example_bfdl.c rpws_archive_example_ipc.c rpws_archive_example_types.c \
#   rpws_fsw_ver.h rpws_lp_status.h  rpws_hfr_status.h  rpws_label.h $(INC)/archive.h
#	cc $(CFLAGS) -o rpws_archive_example.o -c $(SRCARCH)/rpws_archive_example.c 
#	./pds_format -s -x -yCURRENT_DATE -i /opt/project/cassini/include -d "rpws_archive_example_" < 
#        /opt/project/cassini/src/archive/rpws_archive_example.c > 
#        /opt/project/cassini/archive/volume_template/EXTRAS/SOFTWARE/WBR_WFR_LIST.C
#	./pds_format -s -c < /opt/project/cassini/archive/volume_template/EXTRAS/SOFTWARE/WBR_WFR_LIST.C > #
#              /opt/project/cassini/src/archive/WBR_WFR_LIST.C
#	gcc -Wall -O -o WBR_WFR_LIST WBR_WFR_LIST.C -lm


######
#	Everything past this point will end up in /opt/project/cassini/bin
#	and requires "kochab" to perform the 'make'
#
#	Building some CGI stuff as well (cgi subdirectory)
#		
#lib:$(OBJS)
#	$(CC)  -o $(ARC)/lbl_ver lbl_ver.c
#	$(CC)  -o $(ARC)/pds_format pds_format.o
#	$(CC)  -o $(BIN)/pds_format pds_format.o
#	$(CC) -o $(ARC)/rpws_fsw_1.2  rpws_fsw.o mdb_time.o rpws_fsw_ver.o $(SPICE) 
#                -L/home/wtr/plot -l plot -L$(DASLIB) -ldas2 -L $(LIB_DIR) -lm -l util
#	$(CC) -o $(ARC)/rpws_archive_4.40 rpws_archive.o mdb_time.o 
#             rpws_essay.o rpws_status.o rpws_timing.o rpws_label.o 
#             rpws_browse.o rpws_master_browse.o rpws_duplicate_scan.o 
#             rpws_fsw_ver.o rpws_lp_status.o rpws_hfr_status.o rpws_label_graphics.o
#             rpws_image.o rpws_time_patch.o rpws_help.o $(SPICE) -L/home/wtr/plot 
#             -l plot -L$(DASLIB) -ldas2 -L $(LIB_DIR) -lm -l util
#	rm -f $(TEMP)/EXTRAS/SOFTWARE/WBR_WFR_LIST.C
#	./pds_format -s -i$(INC) < rpws_archive_example.c > $(TEMP)/EXTRAS/SOFTWARE/WBR_WFR_LIST.C

