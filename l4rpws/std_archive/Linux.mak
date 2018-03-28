# Export locations needed by the setup.py script 

export INST_ETC
export INST_NAT_BIN

# Note if RPWS_DATA and RPWS_SUPERVOL are not defined, setup.py will fail
# they will be defined in the install directions are followed or if one
# of our standard setup scripts has been sourced first.


# Explicit Rules #############################################################


build: $(BUILD_DIR) build_pylib


$(BUILD_DIR):
	@if [ ! -e "$(BUILD_DIR)" ]; then mkdir $(BUILD_DIR); fi

build_pylib:
	python$(PYVER) setup.py build -b $(BUILD_DIR)

$(INST_ETC)/invoke.sh:$(THIS_FILE)
	@echo \#!/bin/bash > $@
	@echo >> $@
	@echo "source $(INST_ETC)/setup.sh" >> $@
	@echo >> $@
	@echo -n \"$$  >> $@
	@echo "@\"" >> $@
	@echo >> $@
	@echo -n "exit $$" >> $@
	@echo "?" >> $@
	@chmod a+x $@
	@echo $@ generated

install:$(INST_ETC)/invoke.sh  install_pylib

install_pylib:
	python$(PYVER) setup.py install_lib --skip-build -b $(BUILD_DIR)/lib -d $(INST_HOST_LIB)
	python$(PYVER) setup.py install_scripts --skip-build -b $(BUILD_DIR)/scripts-$(PYVER) -d $(INST_NAT_BIN)
	python$(PYVER) setup.py install_data -d $(INST_ETC)
	 
clean:
	rm -r -f build $(BUILD_DIR)

distclean:
	rm -r -f build $(BUILD_DIR)

test: 
	echo "Unit tests have not been defined"


