#!/bin/ksh

#CAS_NAIF_METAKERNEL=/opt/project/cassini/spice/kernels/cas_kernels.txt
#  export CAS_NAIF_METAKERNEL

tBeg=2004-
./mpflist -b -v -r $tBeg 1>tmp.out 2>tmp.err

tBeg=2004-180
./mpflist -b -v -r $tBeg 1>tmp.out 2>>tmp.err

tBeg=2004-180T12:
./mpflist -b -v -r $tBeg 1>tmp.out 2>>tmp.err

tBeg=2004-180T12:30
./mpflist -b -v -r $tBeg 1>tmp.out 2>>tmp.err

tBeg=2004-180T12:30:59.125
./mpflist -b -v -r $tBeg 1>tmp.out 2>>tmp.err

tBeg=2004--180T-12:-30:-59.125
./mpflist -b -v -r $tBeg 1>tmp.out 2>>tmp.err

tBeg=2004-001T12:30:2.125
./mpflist -b -v -r $tBeg 1>tmp.out 2>>tmp.err

more tmp.err
exit 0

#./mpflist -r $tBeg $tEnd 1>tmp.out 2>tmp.err

exit 0
echo R files
./mpfind -v -o -u -tBeg $tBeg -tEnd $tEnd 1>tmp.u00
