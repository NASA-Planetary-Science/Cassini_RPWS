#!/usr/bin/ksh
#
#	POST-PASS Query
#		new features in rpws_file allow everything
#		to be combined into a single 'pipe'
#
#	Note about the mpii "-pool" argument:
#		The Micro-sort will eliminate duplicate records
#		ONLY if it is enabled with a value at or above 32
#		(23-APR-2002 set to 32)
#
#
#source /opt/project/cassini/bin/.CASSINIrc
cd /opt/project/cassini/data/sort_merge
#
#
#
umask 113
s2File=`date '+Cassini_Sort_Merge_%Y-%jT%H:%M'`
sFile='Cassini_Sort_Merge'
mv /var/tmp/$sFile.err /var/tmp/$s2File.err
echo 1>&2 Errors Routed to /var/tmp/$sFile.err 
exec 2> /var/tmp/$sFile.err 
echo 1>&2 "sopc_1270.bat POST PASS *****************"
date 1>&2
# touch nert_spool
rm -f nert_spool
#
#	 /opt/project/cassini/bin/rpws_file56a  (this was replaced 7/24/2009 WTR)
#						to get the new K file to include
#						millisecond mode...
#		+eof \
#		-nert \
#
exec \
	/opt/project/cassini/bin/chdo_listner \
		-debug \
		-port 1270 \
    | \
	 /opt/project/cassini/bin/chdo \
		+eof \
		-epup 128 \
    | \
	 /opt/project/cassini/bin/hkrom12 \
		+eof \
		-recent /opt/project/cassini/data/sort_merge/recent_hsk.dat \
		-stdout \
    | \
	 /opt/project/cassini/src/file/rpws_file58 \
		+eof \
		-sciop \
		-strip \
    &
echo 1>&2 Shell $$
echo 1>&2 Process $!
exit 0
