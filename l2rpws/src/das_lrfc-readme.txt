If you want to build the the Level 2 low-rate complete reader you'll need
to get the sources from:

  /home/raj/cassini/src/das/dasCasSpec2.6  (das.c, wtrstf.c)
  /home/raj/cassini/src/mpmode/ver1.1      (mpmode.c)
  /home/raj/cassini/src/lib/castlm/Ver2.5  (CasTlmFrontEnd.c)
  /home/raj/cassini/include                (CasTlmFrontEnd.h)

It is recommended that you don't use that reader as it doesn't reade the
PDS data.  See the code in 

   l3rpws/lowrate/pdsrdr
	
to handle reading the Level 3 (PDS) dataset instead.

by C. Piker on 2017-04-04
