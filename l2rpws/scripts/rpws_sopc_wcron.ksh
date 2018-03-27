#!/usr/bin/ksh
#
#
# sample $PUSH_HOME/log/Cassini_Push_* file
#       Shell Process ID 21972
#       Background Process ID 21974
#       chdo_listner: V2.1
#       CDHO3: eup = 128 -1
#
# sample ps output:
#       wtr 21974     1  0 11:24:05 pts/13   0:00 /opt/project/cassini/bin/rpws_file50 -sciop -split -stpk1 -strip
#       wtr 21978 21974  0 11:24:05 pts/13   0:00 /opt/project/cassini/bin/rpws_file50 -sciop -strip -addstdout
#       wtr 22313 21960  0 11:43:05 pts/13   0:00 grep 21974
#
# Look in the last "$PUSH_HOME/log/Cassin_Push_*" file for the "Background Process ID" number
#  and produce a process list.
#       3 notes.
#               ID xx   valid
#               xx ID   valid
#               xx xx   invalid (grep)
#
PUSH_HOME="/u/users/cpiker/push"
ERR_MAIL="cwp@mercury.physics.uiowa.edu"
CRON_USER=cpiker

# mail is not working from the sopcs so the mail commands have been shut off

umask 0002

# 1st make sure that the log directory exists:

if [ ! -d $PUSH_HOME/log ] ; then
	if ! mkdirs $PUSH_HOME/log ; then
		echo "Couldn't make $PUSH_HOME/log" 2>&1
		exit 3
	fi
fi 

# CD to the log directory
if ! cd $PUSH_HOME/log ; then
	echo "Couldn't CD to $PUSH_HOME/log" 2>&1
	exit 3
fi

#
s2File=$(date '+RPWS_Push_%Y-%jT%H:%M')
s2Year=$(date +%Y)
s2Day=$(date +%j)
s2Hour=$(date +%H)
#
#	remove old files to keep $PUSH_HOME/log from clogging up
#	Fixed up the remove stuff 2/12/2004 WTR
#
s1Day=$(( $(date +%j)-1 ))
if (( ${#s1Day} == 2 )); then
    s1Day=0$(( $(date +%j)-1 ))
  fi 
if (( ${#s1Day} == 1 )); then
    s1Day=00$(( $(date +%j)-1 ))
  fi
s1Files="RPWS_Push_"$s2Year"-"$s1Day"T"$s2Hour"*"
echo "remove $PUSH_HOME/log/$s1Files (A)"
rm $PUSH_HOME/log/$s1Files 2>/dev/null
#
s1Day=$(( $(date +%j)-2 ))
s1Files="RPWS_Push_$s2Year-$s1Day*"
rm $PUSH_HOME/log/$s1Files 2>/dev/null
#
sFile='RPWS_Push'
#
#	output log file
#
mv $PUSH_HOME/log/$sFile.cron.cur $PUSH_HOME/log/$s2File.cron
echo 1>&2 "wcron: Messages to $PUSH_HOME/log/$sFile.cron.cur (B)"
exec 2> $PUSH_HOME/log/$sFile.cron.cur
echo 1>&2 "wcron PUSH ***************** (B)"
date 1>&2 
echo 1>&2 "rm $PUSH_HOME/log/$s1Files (B)"
#
touch wcron.001
touch wcron.002
touch wcron.003
touch wcron.004
touch wcron.005
#
rm wcron.001
rm wcron.002
rm wcron.003
rm wcron.004
rm wcron.005
#
touch wcron.003
#
#	Look for mail message from bcas/spica
#	'wmail' should have a pile of delete commands
#	  followed by a quit command...
#
if mail -e
then
  # mailx < wmail > wcron.001
  grep "RPWS RESTART REQUEST" wcron.001 > wcron.003
  rm wcron.001
fi
#
#      Status messages to temporary files
#
ps -ef | grep "stot" | grep "Cas_opsana" | grep "QueryServer" > wcron.001
awk '{print $5 " " $6}' < wcron.001 > wcron.004
rm wcron.001
sText="Process"
sPID=$(grep "$sText" $PUSH_HOME/log/RPWS_Push.err.cur)
#
#	worker process ID
#
nPID=${sPID:#$sText}
#
#ps -ef > wcron.001
#grep $nPID < wcron.001 > wcron.002
ps -ef | grep $CRON_USER > wcron.001
grep chdocp < wcron.001 > wcron.002
#
rm wcron.001
echo "Data Push " > wcron.001
date >> wcron.001
grep "defunct" < wcron.002 >> wcron.001
#
#	looking for <defunct>
#
nDefunct=$(wc -l < wcron.001)
nActive=$(wc -l < wcron.002)
nRequest=$(wc -l < wcron.003)
nFlag=0
echo 1>&2 "PUSH $nActive process, $nDefunct defunct, $nRequest request (B)"
if (( $nRequest == 0 )); then
    echo  "No external restart request (A)"
    echo 1>&2 "No external restart request (B)"
    cat 1>&2 wcron.002
fi
#
if [ $nActive != 1 ]; then
    nFlag=$nFlag+active
fi
if [ $nDefunct != 2 ]; then
    nFlag=$nFlag+defunct
fi
if [ $nRequest != 0 ]; then
    nFlag=$nFlag+request
fi
#
if [ $nFlag == 0 ]; then
    echo "PUSH seems OK (A)"
    echo 1>&2 "PUSH seems OK (B)"
  else
    echo  "PUSH ($nFlag) restarting $nActive process, $nDefunct defunct, $nRequest request (A)"
    echo 1>&2 "PUSH ($nFlag) restarting $nActive process, $nDefunct defunct, $nRequest request (B)"
    echo 1>&2 "------ wcron.001 -------- $nDefunct!=2 --- defunct list (B)"
    cat wcron.001
    echo 1>&2 "------ wcron.002 -------- $nActive!=1 --- process list (B)"
    cat wcron.002
    echo 1>&2 "------ wcron.003 -------- $nRequest>0 --- restart request list (B)"
    cat wcron.003
    echo 1>&2 "------           -------- (B)"
    $PUSH_HOME/bin/wkill.ksh
#    mailx -s "casrpws1 restart ($nFlag) [chdocp dead?]" wtr@space.physics.uiowa.edu < wcron.002
#    mailx -s "casrpws1 restart ($nFlag)" $ERR_MAIL < $PUSH_HOME/log/RPWS_Push.err.cur
    $PUSH_HOME/bin/sopc.ksh
#
#       Pager notification: Keep the mssg short.  Pager will
#       keep track of date, so short subject line and
#       bried message
#
    touch $PUSH_HOME/log/skymail
#    mailx -s "Data PUSH casrpws1" 1146403@skymail.com < skymail
#    mailx -s "Data PUSH casrpws1" 1143844@skymail.com < skymail
  fi
#
touch wcron.001
touch wcron.002
touch wcron.003
rm wcron.001
rm wcron.002
rm wcron.003
