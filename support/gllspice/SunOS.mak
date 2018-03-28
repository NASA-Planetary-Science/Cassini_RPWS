##############################################################################
# Generics

# Use GNU make 
ifneq ($(MAKE),gmake)
$(error This make file is intended for use with gmake)
endif

##############################################################################
# Files

SRCS=ar2avv_g.f asdiam_g.f asryed_g.f avv2ar_g.f bodtrn_g.f cksr_g.f ddspnt_g.f\
 epdpnt_g.f gsetrn_g.f gsmtrn_g.f hicpnt_g.f illum_g.f irftrn_g.f limang_g.f\
 linrot_g.f nimmp_g.f pxproj_g.f scnoff_g.f smearv_g.f srfint_g.f ssil2i_g.f\
 ssil2x_g.f subpt_g.f subsol_g.f surfpv_g.f

OBJS=$(patsubst %.f,$(BUILD_DIR)/%.o,$(SRCS))

##############################################################################
# Architecture Specific Info

FC=f77

FFLAGS=-O -u -errwarn


##############################################################################
# Pattern Rules

$(BUILD_DIR)/%.o:src/%.f | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@


##############################################################################
# Explicit rules and targets

.PHONY: build install clean test

build: $(BUILD_DIR) $(BUILD_DIR)/libgllspice.a

$(BUILD_DIR):
	if [ ! -d $(BUILD_DIR) ]; then mkdir -p $(BUILD_DIR); fi

$(BUILD_DIR)/libgllspice.a: $(OBJS)
	ar cr $@ $(OBJS)
	ranlib $@

install:$(INST_NAT_LIB)/libgllspice.a $(INST_INC)/gllspice.h

$(INST_NAT_LIB)/libgllspice.a:$(BUILD_DIR)/libgllspice.a
	install -D -m 664 $< $@

$(INST_INC)/gllspice.h:src/gllspice.h
	install -D -m 664 $< $@

test:
	@echo "No unit tests defined"

clean:
	-rm -r $(BUILD_DIR)
