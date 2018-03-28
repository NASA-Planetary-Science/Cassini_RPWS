#!/bin/ksh
#
# fndcal.ksh
# Version 1.0
#   Feburary 15, 2005 
#   Script to find hfr calibration packets
#   
#
# Programs: hfrcp(), version 1.0
#
#           version 1.1, removed hard coded defaults

# Source the cassini path setup file
. %(INST_ETC)s/setup.sh

PATH=%(LRFC_SCRIPT_PATH)s
  export PATH
  
if [ "$RPWS_MPDB" = "" ]; then
	echo "Set RPWS_MPDB"
	exit 13
fi
  
if [ "$RPWS_SUPERVOL" = "" ]; then
	echo "Set RPWS_SUPERVOL"
	exit 13
fi

if [ "$CAS_TIME_KERNELS" = "" ]; then
	echo "Set CAS_TIME_KERNELS"
	exit 13
fi

if [ "$RPWS_TEMP" = "" ]; then
	echo "Set RPWS_TEMP"
	exit 13
fi

CAS_LRS_MARK_DIR=${RPWS_TEMP}/pds/lrsmrk
  export CAS_LRS_MARK_DIR

if [[ $# -eq 0 ]]; then
  printf "fndcal.ksh tbeg [tend]\n"
  exit 1
elif [[ $# -eq 1 ]]; then
  tBeg=$(das_fxtime "$1" -f "%Y-%j")
  tEnd=$(das_fxtime "$tBeg" +j 1 -f "%Y-%j")
else
  tBeg=$(das_fxtime "$1" -f "%Y-%j")
  tEnd=$(das_fxtime "$2" -f "%Y-%j")
fi 

 
echo "fndcal.ksh() $tBeg to $tEnd"
echo "  RPWS_SUPERVOL=$RPWS_SUPERVOL"
echo "  CAS_TIME_KERNELS=$CAS_TIME_KERNELS"
echo "  RPWS_MPDB=$RPWS_MPDB"
echo "  CAS_LRS_MARK_DIR=$CAS_LRS_MARK_DIR"
echo "  PATH=$PATH"


umask 002 

# assume the interference files have been produced
while [[ $tBeg < $tEnd ]]; do  # tBeg and tEnd should look like YYYY-DOY
  printf "$tBeg "
  
  hUcal=$tBeg.u00
  hUfile=$(mpflist $tBeg)

  if [[ $hUfile != "" ]]; then
    ./hfrcp $hUfile 1>$hUcal 2>>log.txt
    nSize=$(ls -l $hUcal | awk '{print $5}')
  else
    nSize=0
  fi

  if [[ $nSize -ne 0 ]]; then
    printf "size=%s\n" $nSize
  else
    printf "removing %s\n" $hUcal
    rm $hUcal
  fi

  tBeg=$(das_fxtime $tBeg +j 1 -f "%Y-%j")
done

exit 0
