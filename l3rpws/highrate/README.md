Software for making the high-rate portion of the RPWS archive, that's not
needed in production/trunk/GSE.  Before making this software, do a

 source /opt/project/cassini/etc/setup.sh
 gmake
 gmake install
 
on production/trunk/GSE.

C-code
======




Perl and Shell code
===================

scripts - All the control scripts that run rpws_archive and 
  rpws_housekeeping as well as generating all the browse images
