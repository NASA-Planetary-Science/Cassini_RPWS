#!/bin/ksh


if [ "$CAS_TIME_KERNELS" = "" ]; then
	echo "Set CAS_TIME_KERNELS 1st" 
	exit 13
fi

lrf_mas_lbl=/home/raj/project/Cassini/pds/lbl/LRFC_MASTER.LBL

pds_dir=/net/spica/export/data13/cassini/pds/DATA/RPWS_LOW_RATE_FULL


#pds_dir=$pds_dir/T19992XX/T1999227
pds_dir=$pds_dir/T20082XX/T2008283

./pdslbl -m $lrf_mas_lbl $pds_dir/*MFR0.DAT 1>tmp.out 2>tmp.err

more tmp.err
exit 0


./pdslbl -m $lrf_mas_lbl *.DAT 1>tmp.out 2>tmp.err

exit 0
./pdslbl -p 78 -m LRFC_MASTER.LBL *.DAT 1>tmp.out 2>tmp.err

cp old/*.DAT .
./pdslbl -p 78 -m LRFC_MASTER.LBL *.DAT 1>tmp.out 2>tmp.err

exit 0

# -r use fake day long times

pdslbl -p 67 -m LRFC_MASTER.LBL junk.DAT 1>tmp.out 2>tmp.err


pdslbl -p 77 -m tmp.LBL junk.DAT 1>tmp.out 2>tmp.err
