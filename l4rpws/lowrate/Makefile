build:
	@echo Building Cassini RPWS Key-Parameters programs and scripts
	@echo
	@echo 
	@cd das1rdr && $(MAKE)
	@cd das2rdr && $(MAKE)
	@cd labeler  && $(MAKE)
	@cd maker   && $(MAKE)
	@cd scripts && $(MAKE)
	

test:
	@echo Building Cassini RPWS Key-Parameters programs and scripts
	@echo
	@echo 
	@cd das1rdr && $(MAKE) test
	@cd das2rdr && $(MAKE) test
	@cd labeler  && $(MAKE) test
	@cd maker   && $(MAKE) test
	@cd scripts && $(MAKE) test


# Order doesn't matter, these don't depend on each other for linking
install:
	@echo Installing Cassini RPWS Key-Parameters programs and scripts
	@echo
	@echo 
	@cd das1rdr && $(MAKE) install
	@cd das2rdr && $(MAKE) install
	@cd labeler && $(MAKE) install
	@cd maker   && $(MAKE) install
	@cd scripts && $(MAKE) install


distclean:
	@echo Removing build output
	@echo
	@echo 
	@cd das1rdr && $(MAKE) distclean
	@cd das2rdr && $(MAKE) distclean
	@cd labeler && $(MAKE) distclean
	@cd maker   && $(MAKE) distclean
	@cd scripts && $(MAKE) distclean
	
