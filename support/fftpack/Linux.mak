SRCS=cfftb.f cfftb1.f cfftf.f cfftf1.f cffti.f cffti1.f cosqb.f cosqb1.f\
 cosqf.f cosqf1.f cosqi.f cost.f costi.f ezfft1.f ezfftb.f ezfftf.f ezffti.f\
 passb.f passb2.f passb3.f passb4.f passb5.f passf.f passf2.f passf3.f \
 passf4.f passf5.f radb2.f radb3.f radb4.f radb5.f radbg.f radf2.f radf3.f\
 radf4.f radf5.f radfg.f rfftb.f rfftb1.f rfftf.f rfftf1.f rffti.f rffti1.f\
 sinqb.f sinqf.f sinqi.f sint.f sint1.f sinti.f

LIB=fftpack

# Pattern Targets ############################################################

BUILD_OBJS=$(patsubst %.f,$(BUILD_DIR)/%.o,$(SRCS))

# Flags ######################################################################

FC=gfortran

FFLAGS=-O -std=legacy

# Pattern Rules ##############################################################

$(BUILD_DIR)/%.o:%.f | $(BUILD_DIR)
	$(FC) $(FFLAGS) -c $< -o $@

$(INST_NAT_LIB)/%.a:$(BUILD_DIR)/%.a
	install -D -m 664 $< $@

$(INST_INC)/%.h:%.h
	install -D -m 664 $< $@

# Explicit Rules #############################################################

$(BUILD_DIR)/lib$(LIB).a:$(BUILD_OBJS)
	ar -rv $@ $^

$(BUILD_DIR):
	@if [ ! -e "$(BUILD_DIR)" ]; then mkdir $(BUILD_DIR); fi

install:$(INST_NAT_LIB)/lib$(LIB).a $(INST_INC)/fftpack.h

test: $(BUILD_DIR)/test.o
	$(FC) test.o $(BUILD_DIR)/lib$(LIB).a -o $(BUILD_DIR)/test_fftpack
	time test_fftpack

	
distclean:
	rm -r $(BUILD_DIR)
