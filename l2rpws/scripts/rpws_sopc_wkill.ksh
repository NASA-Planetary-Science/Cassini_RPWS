#!/usr/bin/env bash
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
#               ID xx   valid i.e. kill this process
#               xx ID   valid i.e. kill these processes
#               xx xx   invalid (grep)
#
#

PUSH_HOME="/u/users/cpiker/push"
CRON_USER=cpiker

umask 0002
touch wkill.001
rm wkill.001
touch wkill.002
rm wkill.002
sText="Process"
sPID=$(grep "$sText" $PUSH_HOME/log/RPWS_Push.err.cur)
#
nPID=$(echo $sPID | cut -d \  -f 2)
#
#ps -ef > wkill.001
#grep $nPID < wkill.001 > wkill.002
ps -ef | grep $CRON_USER > wkill.001
grep chdocp < wkill.001 > wkill.002
#
iPID=`wc -l < wkill.002`
echo $iPID processes
#
if (( $iPID >= 1 )); then
  awk '{print $2}' < wkill.002 | xargs kill -15
  awk '{print $2}' < wkill.002 | cat
fi
#
rm wkill.001
rm wkill.002

