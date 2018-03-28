#!/usr/bin/env python

import os
import sys
from types import *
import string

from distutils.core import setup


if os.getenv('RPWS_DATA') == None:
	sys.stderr.write("ERROR: Environment var RPWS_DATA is not defined\n")
	sys.exit(7)

if os.getenv('RPWS_SUPERVOL') == None:
	sys.stderr.write("ERROR: Environment var RPWS_SUPERVOL is not defined\n")
	sys.exit(7)

##############################################################################
# Need a custom build_py to write the config file directory into the source.
from distutils.command.build_py import build_py

class post_build_py(build_py):

	def build_module (self, module, module_file, package):
		if type(package) is StringType:
			package = string.split(package, '.')
		elif type(package) not in (ListType, TupleType):
			raise TypeError, \
			    "'package' must be a string (dot-separated), list, or tuple"

		# Now put the module source file into the "build" area -- this is
		# easy, we just copy it somewhere under self.build_lib (the build
		# directory for Python source).
		outfile = self.get_module_outfile(self.build_lib, package, module)
		dir = os.path.dirname(outfile)
		self.mkpath(dir)
		 
		(sDest, bCopied) = self.copy_file(module_file, outfile, preserve_mode=0)
		
		if module_file.endswith('__init__.py'):
			# Move the file
			os.rename(sDest, sDest + ".tmp")
			
			fIn = file(sDest + ".tmp", 'rb')
			fOut = file(sDest, 'wb')
			
			for sLine in fIn:
				if sLine.startswith('g_sSuperVol'):
					fOut.write('g_sSuperVol = "%s"\n'%os.getenv('RPWS_SUPERVOL'))
				
				# Maybe change this to etc/pds directory like all the other 
				# projects
				elif sLine.startswith('g_sTemplateVol'):
					fOut.write('g_sTemplateVol = "%s/database/stdarch"\n'%os.getenv('RPWS_DATA'))
					
				else:
					fOut.write(sLine)
				
			fOut.close()
			fIn.close()
			os.remove(sDest + ".tmp")
			
		return (sDest, bCopied)


##############################################################################

# Maybe this should be under etc/pds like all the other projects, don't know.

sArchTpltDir = '%s/database/stdarch'%os.getenv('RPWS_DATA')
#sArchTpltDir = '/home/cwp/tmp/cas_db'

setup(

	description="Cassini RPWS PDS volume processing and generation",
   
	name="rpws.stdarch",
	
	version="1.2",
		
	author="Chris Piker, Larry Granroth",
	
	packages=['rpws.stdarch'],
	
	scripts=[
	   'scripts/rpws_stdarc_seq2txt',
		'scripts/rpws_stdarc_mkvol',
		'scripts/rpws_stdarc_report',
	],
	
	data_files=[
		( sArchTpltDir,
		  [ 'etc/AAREADME.HTM.r1', 'etc/AAREADME.LBL.r1', 'etc/AAREADME.TXT.r1',
		    'etc/AUTORUN.INF', 'etc/AUTORUN.LBL', 'etc/ERRATA.TXT.r4',
		    'etc/SHRUN.INF','etc/SHRUN.LBL','etc/VOLDESC.CAT.r1'
		  ]
		),
		( '%s/BROWSE'%sArchTpltDir,
		  ['etc/BROWSE/BROWINFO.TXT.r1', 'etc/BROWSE/BROWSE.LBL.r1' ]
		),
		( '%s/DATA'%sArchTpltDir,
		  ['etc/DATA/DATAINFO.TXT.r1']
		),
		( '%s/DATA/ANCILLARY/TXXXXXXX/SXX'%sArchTpltDir,
		  ['etc/DATA/ANCILLARY/TXXXXXXX/SXX/SXX_TOL.LBL.r2']
		),
		( '%s/EXTRAS'%sArchTpltDir,
		  ['etc/EXTRAS/MD5.LBL.r1', 'etc/EXTRAS/RPWS.ICO', 
		   'etc/EXTRAS/SHELLRUN.EXE' ]
		),
		( '%s/INDEX'%sArchTpltDir,
		  ['etc/INDEX/CUMINDEX.LBL.r3', 'etc/INDEX/INDEX.LBL.r2',  
		   'etc/INDEX/INDXINFO.TXT.r1']
		)
	],
	
	cmdclass={"build_py": post_build_py}
)
