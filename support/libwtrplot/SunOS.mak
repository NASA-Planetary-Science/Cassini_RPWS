# makefile for libfg.a ( -L/local/lib -lfg )

# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

CC=cc

#CFLAGS= -O
CFLAGS= -g -Xc -errwarn=%all -fd

SRCS=plot.c ps_plot.c pcl_plot.c hp_plot.c set.c plotinit.c utility.c \
  symbol.c la.c
  
HDRS=wtrplot.h

TARG=libwtrplot.a

OBJS= $(patsubst %.c, $(BUILD_DIR)/%.o, $(SRCS))

##############################################################################
# Pattern rules

.SUFFIXES:

$(BUILD_DIR)/%.o:src/%.c |  $(BUILD_DIR)
	$(CC) -c $(CFLAGS) -o $@ $<


##############################################################################
# Explicit rules

all: $(BUILD_DIR)/$(TARG)

$(BUILD_DIR):
	@if [ ! -d "$(BUILD_DIR)" ] ; then \
	echo mkdir $(BUILD_DIR); \
	mkdir -p $(BUILD_DIR); \
	fi

$(BUILD_DIR)/$(TARG): $(OBJS)
	ar rv $@ $(OBJS)

$(BUILD_DIR)/wtrplot_test:src/test.c $(BUILD_DIR)/libwtrplot.a
	$(CC) $< $(CFLAGS) $(BUILD_DIR)/libwtrplot.a -o $@
	chmod +x $@
	
test: $(BUILD_DIR)/wtrplot_test
	sh -c "./$(BUILD_DIR)/wtrplot_test > $(BUILD_DIR)/wtrplot_test1.txt"
	@if diff test/test_output1.txt $(BUILD_DIR)/wtrplot_test1.txt > /dev/null; then \
	  echo "WTR Plot Test 1 [PASSED]"; \
	  else echo "WTR Plot Test 1 [FAILED]"; return 1; fi

install: $(BUILD_DIR)/$(TARG)
	install -D $(BUILD_DIR)/$(TARG) $(INST_NAT_LIB)/$(TARG)
	install -D src/wtrplot.h $(INST_INC)/wtrplot.h
	
distclean:
	rm -f -r $(BUILD_DIR)

clean:
	rm $(BUILD_DIR)/*.o
