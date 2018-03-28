#!/bin/bash

# wrapper script for mag_vector.py


uname=$(uname)

if [ $uname = SunOS ]; then
	python=/opt/csw/bin/python
	PYTHONPATH=/local/lib/python/site-packages
elif [ $uname = Linux ]; then
	python=python
	PYTHONPATH=/usr/local/lib64/python2.4/site-packages
else
	python=python
fi

export PYTHONPATH

exec $python $(dirname $0)/mag_vector.py "$@"
