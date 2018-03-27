#!/usr/bin/env bash

if [ "$CAS_TIME_KERNELS" = "" ]; then
	echo "Set CAS_TIME_KERNELS 1st" 
	exit 13
fi

sBeg="1999-225"
sEnd="1999-257"
./casorb $sBeg $sEnd

sBeg="1999-225"
sEnd="2005-001"
./casorb $sBeg $sEnd

sBeg="2004-185"
sEnd="2004-200"
./casorb $sBeg $sEnd

sBeg="2004-185"
./casorb $sBeg 

sBeg="2005-186T00:00:03"
echo $sBeg
./casorb $sBeg 

sBeg="2004-185T00:00:03"
echo $sBeg
./casorb $sBeg 

sBeg="2008-190T00:00:00"  # July 8, 2008"
echo $sBeg
./casorb $sBeg 

sBeg="2008-185T00:00:00"  # July 8, 2008"
echo $sBeg
./casorb $sBeg 

sBeg="2008-185T22:04:00"  # July 8, 2008"
echo $sBeg
./casorb $sBeg 

sBeg="2008-178T19:35"  # 73 
echo $sBeg
./casorb $sBeg 
sBeg="2008-178T19:36"  # 73,74
echo $sBeg
./casorb $sBeg 
sBeg="2008-178T19:37"  # 74 
echo $sBeg
./casorb $sBeg 

sBeg="2008-185T22:02"  # 74 
echo $sBeg
./casorb $sBeg 
sBeg="2008-185T22:03"  # 74,75 
echo $sBeg
./casorb $sBeg 
sBeg="2008-185T22:04"  # 75 
echo $sBeg
./casorb $sBeg 

sBeg="2008-185T22:05"  # 75 
echo $sBeg
./casorb $sBeg 
