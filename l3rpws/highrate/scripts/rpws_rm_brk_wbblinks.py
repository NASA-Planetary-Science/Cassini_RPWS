#!/usr/bin/env python
import sys
import os
import re

from os.path import join as pjoin
import os.path

g_nMinRm = 2  # Use this to fix the ole _CAL0.TAB problem


def rmLines(sPath, lRm):

	if len(lRm) < g_nMinRm:
		print "%s: Skipping fix, less than minimum number of broken links preset."%os.path.basename(sPath)
		return 0
	
	print "%s: Removing lines %s"%(sPath, lRm)
	
	fIn = file(sPath, 'rb')
	
	lIn = []
	for sLine in fIn:
		lIn.append(sLine)
	fIn.close()
	
	fOut = file(sPath, 'wb')
	
	nLine = 0
	
	for i in xrange(0, len(lIn)):
		nLine += 1
		
		if nLine not in lRm:
			fOut.write("%s\r\n"%lIn[i].strip('\r\n'))

	fOut.close()
	return len(lRm)


bFix = False
iArgTop = 1
if len(sys.argv) > 1 and sys.argv[1] == '--fix':
	print "Removing broken links"
	bFix = True
	iArgTop = 2

reLink = re.compile(r'(<A HREF=")(.*)(">)')

#Example sTop = '/opt/project/cassini/pds/BROWSE/RPWS_WIDEBAND_FULL/T20090XX'

if len(sys.argv) == 1 or sys.argv[1] == '-h' or sys.argv[1] == '--help':
	print "This script attempts to remove broken links from Cassini"
	print "PDS browse HTML files.  USE WITH CAUTION.  It has only been "
	print "tested on the directories: "
	print "   BROWSE/RPWS_WIDEBAND_FULL"
	print "   BROWSE/RPWS_LOW_RATE_FULL"
	print "Don't use it anywhere else without testing first.  You have"
	print "been warned!"
	print
	print "Usage %s [--fix] TOP_DIR"%os.path.basename(sys.argv[0])
	sys.exit(0)

sTop = sys.argv[iArgTop]

for (sRoot, lDirs, lFiles) in os.walk(sTop):
	
	for sFile in lFiles:
		if sFile.endswith(".HTM"):
			sPath = pjoin(sRoot, sFile)
			
			fIn = file(sPath, 'rb')
			#sIn = fIn.read()
			#fIn.close()
			
			lRm = []
			
			nLine = 0
			for sLine in fIn:
				nLine += 1
				mLink = reLink.search(sLine)
				if mLink:
					sLink = mLink.group(2)
					if sLink.endswith(".DAT") or sLink.endswith(".LBL") or\
					   sLink.endswith(".TAB"):
					
						sRelLink = pjoin(sRoot, sLink)
						sAbsLink = os.path.abspath(sRelLink)
						
						if not os.path.isfile(sAbsLink):
							lRm.append(nLine)
							print "%s: Broken link: %s"%(os.path.basename(sPath),
							      sAbsLink)
			
			fIn.close()
								
			if len(lRm) > 0:
				if bFix:
					rmLines(sPath, lRm)
				else:
					print "%s: use --fix to rm lines %s"%(sPath, lRm)

sys.exit(0)		
			
