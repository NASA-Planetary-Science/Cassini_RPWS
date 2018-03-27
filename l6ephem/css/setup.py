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
				if sLine.startswith('g_sConfDir'):
					fOut.write('g_sConfDir = "%s"\n'%os.getenv('INST_ETC'))
				else:
					fOut.write(sLine)
			fOut.close()
			fIn.close()
			os.remove(sDest + ".tmp")
			
		return (sDest, bCopied)


##############################################################################
setup(

	description="Cassini - general mission ancillary information",
   
	name="css",
	
	version="1.2",
		
	author="Chris Piker, Larry Granroth",
	
	packages=['css'],
		
	cmdclass={"build_py": post_build_py}
)
