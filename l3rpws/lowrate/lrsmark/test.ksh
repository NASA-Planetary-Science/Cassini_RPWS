#!/bin/ksh

CAS_PDS_ROOT_DIR=/net/spica/export/data13/cassini/pds

data_dir=$CAS_PDS_ROOT_DIR/DATA/RPWS_LOW_RATE_FULL/T20042XX/T2004296
dFile=$(ls $data_dir/*LFR0.DAT)
iFile=$CAS_PDS_ROOT_DIR/lrsmrk/t20043xx/t2004317.txt

tYear=2005;  tDoy=035
tYear=2005;  tDoy=062
data_dir=$CAS_PDS_ROOT_DIR/DATA/RPWS_LOW_RATE_FULL/T$tYear${tDoy%%[0-9][0-9]}XX/T$tYear$tDoy
dFile=$(ls $data_dir/*MFDR0.DAT)
iFile=$CAS_PDS_ROOT_DIR/lrsmrk/t$tYear${tDoy%%[0-9][0-9]}xx/t$tYear$tDoy.txt
lfdrFile=$CAS_PDS_ROOT_DIR/lrsmrk/lfdr/t$tYear${tDoy%%[0-9][0-9]}xx/t$tYear$tDoy.txt

echo $dFile
echo $iFile

cp $dFile .

#./lrsmark -m -i $iFile $dFile 1>tmp.out 2>tmp.err

./lrsmark -i $iFile $dFile 1>tmp.out 2>tmp.err
./lrsmark -clean -i $lfdrFile $dFile 1>tmp2.out 2>tmp2.err

echo "exit status=$?"

exit 0

echo "$tYear-$tDoy" 1>tmp.err
for f in $dFile; do
  echo "file=$f" 1>&2
  lrsmark -v -i $iFile ${f##*/} 1>tmp.out 2>>tmp.err
done

exit 0
lrf_list -b *LFR0.DAT | awk '$0 ~ /317T04:23/,/317T04:2[5-9]/{print $0}' |more  
