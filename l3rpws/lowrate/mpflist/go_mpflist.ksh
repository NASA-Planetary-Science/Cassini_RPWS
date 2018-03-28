#!/bin/ksh

#CAS_NAIF_METAKERNEL=/opt/project/cassini/spice/kernels/cas_kernels.txt
#  export CAS_NAIF_METAKERNEL

tBeg=2004-180
tEnd=2004-180T01:00
tBeg=2004-179T23:00
tEnd=2004-181T01:00

tBeg=2004-180
tEnd=2004-180T01:00
tBeg=2004-001

tBeg=2003-279T13:46
tEnd=2003-279T13:51

#./mpflist -b -v -r $tBeg 1>tmp.out 2>tmp.err
./mpflist -r $tBeg $tEnd 1>tmp.out 2>tmp.err

exit 0
echo R files
./mpfind -v -o -u -tBeg $tBeg -tEnd $tEnd 1>tmp.u00
