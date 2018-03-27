#!/usr/bin/ksh
#
#	Data Push (real time data push to thuban)
#
#	Log file foolishness...
#		I think we put it here so it disappears
#		if the machine gets rebooted ?!?
#

PUSH_HOME="/u/users/cpiker/push"

umask 0002
s2File=`date '+RPWS_Push_%Y-%jT%H:%M'`
sFile='RPWS_Push'
mv $PUSH_HOME/log/$sFile.err.cur $PUSH_HOME/log/$s2File.err
#
#
#
echo "chdocp starting: Errors Routed to $PUSH_HOME/log/$sFile.err.cur" 1>&2
exec 2> $PUSH_HOME/log/$sFile.err.cur 
echo "sopc.bat PUSH *****************" 1>&2 
date 1>&2
#
#
nTARGET=thuban.physics.uiowa.edu
nSOCKET=1277
nBC=CASTISM1
nFILTER="f:sfdu_cas_rpws_hk or f:sfdu_cas_rpws"

#
#	get ready
#
echo "detach: /sfoc/bin/chdocp -filter $nFILTER -MC -skt $nBC $nSOCKET:$nTARGET &" 1>&2 
/sfoc/bin/chdocp \
	-filter "$nFILTER" \
	-MC \
	-skt \
	$nBC \
	$nSOCKET:$nTARGET \
	&


#	Save important process ID's so we can find & kill

echo "Shell $$" 1>&2
echo "Process $!" 1>&2 

exit 0
