#!/bin/bash

cd /net/betazed/disk/1/cassini



patchSuper()
{

	echo "Patching using ${1}_${2}"

	if [ "$1" = "" ] ; then
		echo "Nada 1"
		return 11
	fi
	
	if [ "$2" = "" ] ; then
		echo "Nada 2"
		return 11
	fi
	
	if ! mv pds/DATA/RPWS_RAW_COMPLETE/T${1}${2} scratch/pds/DATA/RPWS_RAW_COMPLETE ; then
		echo "Error patching $1 $2 in step 1"
		return 1
	fi
	
	if ! cp -r -p /net/thuban/export/data2/cassini/${1}_${2}/DATA/RPWS_RAW_COMPLETE/T${1}${2} pds/DATA/RPWS_RAW_COMPLETE ; then
		echo "Error patching $1 $2 in step 2"
		return 2
	fi
	
	
	if ! mv pds/DATA/RPWS_WAVEFORM_FULL/T${1}${2} scratch/pds/DATA/RPWS_WAVEFORM_FULL ; then
		echo "Error patching $1 $2 in step 3"
		return 3
	fi
	
	if ! cp -r -p /net/thuban/export/data2/cassini/${1}_${2}/DATA/RPWS_WAVEFORM_FULL/T${1}${2} pds/DATA/RPWS_WAVEFORM_FULL ; then
		echo "Error patching $1 $2 in step 4"
		return 4
	fi
	
	if ! mv pds/DATA/RPWS_WIDEBAND_FULL/T${1}${2} scratch/pds/DATA/RPWS_WIDEBAND_FULL ; then
		echo "Error patching $1 $2 in step 5"
		return 5
	fi
	
	if ! cp -r -p /net/thuban/export/data2/cassini/${1}_${2}/DATA/RPWS_WIDEBAND_FULL/T${1}${2} pds/DATA/RPWS_WIDEBAND_FULL ; then
		echo "Error patching $1 $2 in step 6"
		return 6
	fi
	
	if ! mv pds/BROWSE/RPWS_WAVEFORM_FULL/T${1}${2} scratch/pds/BROWSE/RPWS_WAVEFORM_FULL ; then
		echo "Error patching $1 $2 in step 7"
		return 7
	fi
	
	if ! cp -r -p /net/thuban/export/data2/cassini/${1}_${2}/BROWSE/RPWS_WAVEFORM_FULL/T${1}${2} pds/BROWSE/RPWS_WAVEFORM_FULL ; then
		echo "Error patching $1 $2 in step 8"
		return 8
	fi
	
	if ! mv pds/BROWSE/RPWS_WIDEBAND_FULL/T${1}${2} scratch/pds/BROWSE/RPWS_WIDEBAND_FULL ; then
		echo "Error patching $1 $2 in step 9"
		return 9
	fi
	
	if ! cp -r -p /net/thuban/export/data2/cassini/${1}_${2}/BROWSE/RPWS_WIDEBAND_FULL/T${1}${2} pds/BROWSE/RPWS_WIDEBAND_FULL ; then
		echo "Error patching $1 $2 in step 10"
		return 10
	fi
	
	return 0
}


copyToSuper()
{

	echo "Patching using ${1}_${2}"

	if [ "$1" = "" ] ; then
		echo "Nada 1"
		return 11
	fi
	
	if [ "$2" = "" ] ; then
		echo "Nada 2"
		return 11
	fi
		
	if ! cp -r -p /net/thuban/export/data2/cassini/${1}_${2}/DATA/RPWS_RAW_COMPLETE/T${1}${2} pds/DATA/RPWS_RAW_COMPLETE ; then
		echo "Error patching $1 $2 in step 2"
		return 2
	fi
		
	if ! cp -r -p /net/thuban/export/data2/cassini/${1}_${2}/DATA/RPWS_WAVEFORM_FULL/T${1}${2} pds/DATA/RPWS_WAVEFORM_FULL ; then
		echo "Error patching $1 $2 in step 4"
		return 4
	fi
		
	if ! cp -r -p /net/thuban/export/data2/cassini/${1}_${2}/DATA/RPWS_WIDEBAND_FULL/T${1}${2} pds/DATA/RPWS_WIDEBAND_FULL ; then
		echo "Error patching $1 $2 in step 6"
		return 6
	fi
		
	if ! cp -r -p /net/thuban/export/data2/cassini/${1}_${2}/BROWSE/RPWS_WAVEFORM_FULL/T${1}${2} pds/BROWSE/RPWS_WAVEFORM_FULL ; then
		echo "Error patching $1 $2 in step 8"
		return 8
	fi
		
	if ! cp -r -p /net/thuban/export/data2/cassini/${1}_${2}/BROWSE/RPWS_WIDEBAND_FULL/T${1}${2} pds/BROWSE/RPWS_WIDEBAND_FULL ; then
		echo "Error patching $1 $2 in step 10"
		return 10
	fi
	
	return 0
}

#if ! patchSuper 2009 0XX ; then
#	exit $?
#fi
#exit 0

#if ! patchSuper 2009 1XX ; then
#	exit $?
#fi
#if ! patchSuper 2009 2XX ; then
#	exit $?
#fi
#if ! patchSuper 2009 3XX ; then
#	exit $?
#fi
#
#if ! patchSuper 2010 0XX ; then
#	exit $?
#fi
#if ! patchSuper 2010 1XX ; then
#	exit $?
#fi
#if ! patchSuper 2010 2XX ; then
#	exit $?
#fi
#if ! patchSuper 2010 3XX ; then
#	exit $?
#fi
#
#if ! patchSuper 2011 0XX ; then
#	exit $?
#fi
#if ! patchSuper 2011 1XX ; then
#	exit $?
#fi

if ! copyToSuper 2011 2XX ; then
	exit $?
fi

if ! copyToSuper 2011 3XX ; then
	exit $?
fi

exit 0

