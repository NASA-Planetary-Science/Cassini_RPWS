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
# This module depends on the general cassini module, 'css'

setup(

	description="Cassini - RPWS Langmurpprobe data handling scripts",
   
	name="rpws.lp",
	
	version="0.2",
		
	author="Larry Granroth",
	
	scripts=[
		'scripts/rpws_lp_index',
		'scripts/rpws_lp_reform_cntcur',
		'scripts/rpws_lp_reform_enceladus',
		'scripts/rpws_lp_reform_neproxy',
		'scripts/rpws_lp_reform_saturn',
		'scripts/rpws_lp_reform_titan',
		'scripts/rpws_lp_reform_ui'
	],	
	
	data_files=[
		( '%s/pds'%os.getenv('INST_ETC'),
		  [ 'etc/RPWS_LP_CNTCUR_yyyyddd_V1.LBL.template',
		    'etc/RPWS_LP_NEPROXY_yyyyddd_V1.LBL.template',
		    'etc/RPWS_LP_T_yyyyddd_Tnn_V1.LBL.template',
		    'etc/RPWS_LP_UI_yyyyddd_V1.LBL.template' 
		  ]
		)
	]
)





