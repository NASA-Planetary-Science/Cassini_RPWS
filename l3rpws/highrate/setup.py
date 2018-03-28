#!/usr/bin/env python

import os
import sys
from types import *
import string

from distutils.core import setup


if os.getenv('INST_ETC') == None:
	sys.stderr.write("ERROR: Environment var INST_ETC is not defined\n")
	sys.exit(7)

##############################################################################
setup(
	description="Cassini RPWS WBR/WFR receiver tools for Level 3 data",
   
	name="rpws.highrate.l3",
	
	version="0.1",
		
	author="Chris Piker, Nhan Tran-Phan",
	
	packages=['rpws','rpws.highrate','rpws.highrate.l3'],
	
	scripts=['scripts/rpws_hr_fixorbit', 'scripts/rpws_hr_fixtargs',
	         'scripts/rpws_hr_len0fix', 
	         'scripts/rpws_l3wbr_avail', 
	         'scripts/rpws_l3wbr_print', 'scripts/rpws_l3wfr_das2rdr',
	         'scripts/rpws_l3wfr_print', 'scripts/rpws_l3hr_gainrdr']
)
