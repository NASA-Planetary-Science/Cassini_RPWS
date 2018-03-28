#!/bin/ksh

PATH=/usr/bin:/local/bin:/home/raj/bin

tBeg=1998-364
tEnd=1999-001

tBeg=2000-153
tEnd=2000-154
tBeg=2000-348
tEnd=2001-001

tBeg=1997-001
tEnd=2005-180

tBeg=2004-289
tEnd=2005-001

tBeg=2005-001
tEnd=2005-190

tBeg=2004-183
tEnd=2004-288

tBeg=2004-288
tEnd=2004-289

tBeg=2008-002
tEnd=2008-009

tBeg=2008-182
tEnd=2008-190

dRoot=/net/spica/export/data13/cassini/pds/DATA/RPWS_LOW_RATE_FULL
echo $tBeg $tEnd
while [[ $tBeg < $tEnd ]]; do

  nYear=${tBeg%%-*};  nDoy=${tBeg##*-}
  dDat=$dRoot/T${nYear}${nDoy%[0-9][0-9]}XX/T${nYear}${nDoy}
  for f in $(ls $dDat/*CAL*.TAB 2>/dev/null); do
    tSclk=$(cat ${f%.TAB}.LBL | nixcr | awk '$0 ~ /^START_TIME *=/ {print $3}')
    tSclk=${tSclk%Z}
    echo "${f##*/} $tSclk"
    /opt/project/cassini/archive/script/lrfc/callbl.ksh $f $tSclk
  done

  tBeg=$(das1_fxtime $tBeg +j 1 -f "%Y-%j")
done

exit 0
