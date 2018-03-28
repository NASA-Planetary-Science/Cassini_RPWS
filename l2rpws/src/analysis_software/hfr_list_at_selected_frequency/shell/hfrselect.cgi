#!/bin/ksh

CASMPDB=/opt/project/cassini/data/database/CassiniJPL.db
  export CASMPDB
CAS_CAL_DIR=/opt/project/cassini/cal/version1.0
  export CAS_CAL_DIR
CAS_DIR_CAL=/opt/project/cassini/cal/version1.0
  export CAS_DIR_CAL
CAS_NAIF_METAKERNEL=/opt/project/cassini/spice/kernels/cas_kernels.txt
  export CAS_NAIF_METAKERNEL
CAS_TIME_KERNELS=/opt/project/cassini/spice/kernels/cas_kernels.txt
  export CAS_TIME_KERNELS
RPWS_SUPERVOL=/opt/project/cassini/pds
  export RPWS_SUPERVOL  

/home/tfa/cassini/hfr/web/hfrselect

exit 0
