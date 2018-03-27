##############################################################################
# Generics

# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Project Specific Defs

BITMAPS=FastForward.bm PlayForward.bm PlayRewind.bm Rewind.bm StepForward.bm \
      StepRewind.bm Stop.bm

OBJS= ana_main.o ana_gui.o ana_evtlp.o ana_dsply.o ana_class.o \
      CPGPlot.o CLinkedList.o 
		#pggrid.o 
		#pgplotcism.o 
		
TARG= rpws_ana

# Sources for the RPWS motif lib
XM_OBJS= callback.o FileManager.o menue.o printer.o recorder.o ToggleBox.o \
         dialog.o fileselection.o postdialog.o PrintManager.o Text.o \
			XmTools.o
			
# Sources for FFT functions
FFT_OBJS= fft.o hannfft.o Means.o realonlyfft.o RealOnlyFFT.o Tools.o \
          window.o Window.o

##############################################################################
# Sun specific stuff
CC=cc
F77=f77

MOTIF_INC=  -I/usr/dt/include
XWIN_INC=-I/usr/openwin/include

DEFS='-DINST_SHARE="$(INST_SHARE)"' '-DRPWS_DATA="$(RPWS_DATA)"'

CFLAGS=-m32 -Xc -v $(DEFS) -I$(INST_INC) $(PGPLOT_INC) $(MOTIF_INC) $(XWIN_INC)

RPWS_LIB=-L$(INST_NAT_LIB) -ltelemetry
PGPLOT_LIB=-lXmPgplot -lcpgplot -lpgplot -lpng
MOTIF_LIB=  -L/usr/dt/lib/Xm -lXm
XWIN_LIB=-L/usr/openwin/lib -lXt -lX11

FORT_LIBS=-L/opt/SUNWspro/lib -lf77compat -fai2

LFLAGS=-m32 $(RPWS_LIB) $(PGPLOT_LIB) $(MOTIF_LIB) $(XWIN_LIB) -lm


##############################################################################
# Pattern defs and rules

INST_BITMAPS=$(patsubst %,$(INST_SHARE)/ana/%,$(BITMAPS))

BUILD_OBJS= $(patsubst %,$(BUILD_DIR)/%,$(OBJS)) \
            $(patsubst %,$(BUILD_DIR)/%,$(XM_OBJS)) \
				$(patsubst %,$(BUILD_DIR)/%,$(FFT_OBJS))

DEPS = $(patsubst %.o, %.d, $(OBJS))

$(BUILD_DIR)/%.o:src/%.c
	$(CC) $(CFLAGS) -c $< -o $@
	
$(BUILD_DIR)/%.o:lib/XmRPWS/%.c
	$(CC) $(CFLAGS) -c $< -o $@
	
$(BUILD_DIR)/%.o:lib/rpwsdsp/%.c
	$(CC) $(CFLAGS) -c $< -o $@
	
$(INST_SHARE)/ana/%:bitmaps/%
	install -D -m 664 $< $@

##############################################################################
# Explicit rules
#
# Warning! There are no object dependencies listed here, so to be safe
#          you should gmake clean before each build

$(TARG):$(BUILD_DIR) $(BUILD_DIR)/$(TARG)

$(BUILD_DIR):
	if [ ! -d "$(BUILD_DIR)" ]; then echo mkdir $(BUILD_DIR); mkdir -p $(BUILD_DIR); fi
	
$(BUILD_DIR)/$(TARG):$(BUILD_OBJS)
	$(F77) $(BUILD_OBJS) $(LFLAGS) -o $@ 
	
install: $(INST_NAT_BIN)/$(TARG) $(INST_BITMAPS)

$(INST_NAT_BIN)/$(TARG):$(BUILD_DIR)/$(TARG)
	if [ ! -d $(INST_NAT_BIN) ]; then mkdir -p $(INST_NAT_BIN); chmod g+w $(INST_NAT_BIN); fi
	install -p -m 775 $< $@

distclean:
	rm -r -f $(BUILD_DIR)

clean:
	rm $(BUILD_DIR)/*.o $(BUILD_DIR)/*.d
