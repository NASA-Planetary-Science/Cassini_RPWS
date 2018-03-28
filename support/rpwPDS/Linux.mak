##############################################################################
# Explicit Rules

build: 
	python$(PYVER) setup.py build --build-base=$(BUILD_DIR)

install:
	python$(PYVER) setup.py install --install-purelib=$(INST_HOST_LIB) \
 --install-scripts=$(INST_HOST_BIN)

clean:
	rm -r -f $(BUILD_DIR) build
	
distclean:
	rm -r -f $(BUILD_DIR) build

test:
	echo "Unit tests have not been defined"

