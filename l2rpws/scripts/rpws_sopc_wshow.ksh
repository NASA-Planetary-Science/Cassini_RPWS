#!/bin/ksh

umask 0002

PUSH_HOME="/u/users/cpiker/push"

sLogFile=$PUSH_HOME/log/RPWS_Push.err.cur

# 1st look using the log file
echo "Process listed in ${sLogFile}:"

if [ ! -e "$sLogFile" ]; then
	echo "INFO: Logfile $sLogFile not present"
else
	sPID=$(grep Process ${sLogFile})
	nPID=${sPID:#Process}
	sTmpFile=$(mktemp /var/tmp/wshow.XXXXXX)
	if [ -z "$sTmpFile" ]; then
		echo "ERROR: Couldn't create temporary file in /var/tmp"
		exit 3
	fi
	ps -ef > $sTmpFile
	grep $nPID $sTmpFile
	if [ "$?" != "0" ]; then
		echo "(none)"
	fi
	echo
	rm $sTmpFile
fi

echo "All chdocp processes:"

sTmpFile=$(mktemp /var/tmp/wshow.XXXXXX)
if [ -z "$sTmpFile" ]; then
	echo "ERROR: Couldn't create temporary file in /var/tmp"
	exit 3
fi
ps -ef > $sTmpFile
grep chdocp $sTmpFile
if [ "$?" != "0" ]; then
	echo "(none)"
fi

echo
rm $sTmpFile

exit 0

