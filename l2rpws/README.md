Most of the code in this area is made to work together to get data from SFDUs
from the SOPC to *.r and *.u files in a data area.  Orginally written by:

   Willy R.
	Robert J.
	Terry A.
	  
The location for much of this software was:

  /opt/project/cassini/src
  
on the Solaris NFS network.


PROGRAMS INSTALLED
==================


rpws_dsp_4
----------
CASSINI CDS data dump

Curses based terminal program to dump information from RPWS mini-packet files.
The -hdr option is useful to see one-line per packet summary skiping curses.  

Help Cmdline:  rpws_dsp_4 -help
Basic Usage:    cat YOUR_R_OR_U_FILE | rpws_dsp4

TODO: Define CDS


rpws_dsp_5
----------
CASSINI MP data dump

Curses based terminal program to dump information from RPTS minipacket files.
Looks to be capable of digging out much more information than rpws_dsp4

Help Cmd Line:  rpws_dsp_5 -help
Basic Usage:    cat YOUR_R_OR_U_FILE | rpws_dsp_5


rpws_dsp_8
----------
CASSINI time dump

No summary

Help Cmdline: rpws_dsp_8 -help
Basic Usage:  Unknown


rpws_dsp_9
----------
CASSINI MP data dump using MP database

No summary

Help Cmdline: rpws_dsp_9 -help
Basic Usage:  Unknown


rpws_dsp_9a
-----------
CASSINI MP data dump CGI program

This program outputs information about the most recently recieved MP data as
HTML text.  It's odd in that calling the program by different names triggers
different operations.  The known argv[0] names are:

rpws_dsp_9a:       Default name, triggers output of Science data from the 
                  minipacket 'database'
            
rpws_dsp_9a_temp:  Reads from $RPWS_DATA/temp/recent_hsk.dat

rpws_dsp_9a_test:  Reads from $RPWS_DATA/bg_test/recent_hsk.dat

rpws_dsp_9a_vc0:   Reads from $RPWS_DATA/bg_push/recent_hsk.dat.vc0

rpws_dsp_9a_vc1:   Reads from $RPWS_DATA/bg_push/recent_hsk.dat.vc1

rpws_dsp_9a_all:   Reads from $RPWS_DATA/bg_push/recent_hsk.dat

rpws_dsp_9a_key:   Reads from $RPWS_DATA/bg_push/recent_hsk.dat

rpws_dsp_9a_nert:  Reads from $RPWS_DATA/bg_nert/recent_hsk.dat

rpws_dsp_9a_em:    Reads from $RPWS_DATA/data_em/recent_hsk.dat

All versions of the program log to:  $RPWS_DATA/temp/dsp9a_web-cgi.log

Help Cmdline: rpws_dsp_9a -help 

Basic Usage:  Unknown


rpws_dsp_r
----------
CASSINI CDS data recovery analysis (DSPR)

Prints diagnostic information to stdout about minipackets in the 'database'

Help Cmdline: rpws_dspr -help
Basic Usage:  rpws_dspr START_TIME STOP_TIME


rpws_dsp_q
----------
CASSINI CDS data recovery analysis (DSPQ)

Prints diagnostic information to stdout about minipackets in the 'database'.
Any reasons to use this program over rpws_dspr are unknown.

Help Cmdline: rpws_dspq -help
Basic Usage:  rpws_dspq START_TIME STOP_TIME


rpws_dsp_hkanal
---------------
Housekeeping analysis

Reads housekeeping packets from the 'database' and does some sort of checking.



rpws_dsp_hkrom
--------------
CASSINI RPWS housekeeping dump

This program looks at a standard input stream and writes HSK information to
the screen.  Alternatly it can write HSK packets to a file for some other
program to read, like rpws_dsp9a.  The standard post-pass query script,
"sopc.bat" uses this program.

Help Cmdline:  rpws_dsp_hkrom -help
Basic Usage:   cat YOUR_R_OR_U_FILES | rpws_dsp_hkrom



