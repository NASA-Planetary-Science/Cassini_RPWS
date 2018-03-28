build:
	@echo Building Cassini RPWS support libraries, remember build and 
	@echo install giferator, libdas2, and pspice as well
	@cd 
	@cd fftpack && $(MAKE) 
	@cd gllspice && $(MAKE)
	@cd libfg && $(MAKE)
	@cd libwtrplot && $(MAKE)
	@cd pdspad && $(MAKE)
	@cd rpwPDS && $(MAKE)


test:
	@echo Not sure which of the sub-modules have a make test target
	@echo Skipping make test for all.

install:
	@echo Installing Cassini RPWS support libraries
	@cd 
	@cd fftpack && $(MAKE) install
	@cd gllspice && $(MAKE) install
	@cd libfg && $(MAKE) install
	@cd libwtrplot && $(MAKE) install
	@cd pdspad && $(MAKE) install
	@cd rpwPDS && $(MAKE) install

distclean:
	@echo Removing all build output
	@cd 
	@cd fftpack && $(MAKE) distclean
	@cd gllspice && $(MAKE) distclean
	@cd libfg && $(MAKE) distclean
	@cd libwtrplot && $(MAKE) distclean
	@cd pdspad && $(MAKE) distclean
	@cd rpwPDS && $(MAKE) distclean

