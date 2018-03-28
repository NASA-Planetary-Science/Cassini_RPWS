# Common Definitions #########################################################

# Pick a default install location, if user doesn't have one defiend
ifeq ($(PREFIX),)
PREFIX=/usr/local/cassini
endif

ETC=$(PREFIX)/etc
DOC=$(PREFIX)/doc
BIN=$(PREFIX)/bin
SRC=$(PREFIX)/src
LIB=$(PREFIX)/lib

PYVER=$(shell python -c "import sys; print '.'.join( sys.version.split()[0].split('.')[:2] )")


PYLIB=$(LIB)/python$(PYVER)

INC=$(PREFIX)/include
SHARE=$(PREFIX)/share

DATE=$(shell date "+%Y-%m-%d")
ARCH=$(shell uname -s).$(shell uname -p)

BUILD=build.$(ARCH)

##############################################################################
# Explicit Rules

build: 
	python setup.py build

install:
	python setup.py install --prefix=$(PREFIX) --install-purelib=$(PYLIB)

clean:
	rm -r -f build

test:
	echo "Unit tests have not been defined"
