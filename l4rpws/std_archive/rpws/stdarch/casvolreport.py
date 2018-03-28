#!/usr/bin/env python

""" casvolreport.py - report on data files and size by dataset id in
cassini volumes"""

import os
import sys
import os.path
from os.path import join as pjoin

from copy import deepcopy

#module local imports
import ipds.util


def main():
	"""Uses sys.argv"""
	
	if '-h' in sys.argv or '--help' in sys.argv:
		print "Data set ID based report of MegaBytes and number of files"
		print "cassini volumes."
		print
		print "   usage:  %s INDEX.TAB [INDEX.TAB INDEX.TAB ...]"%sys.argv[0]
		print 
		print "where INDEX.TAB is a cassini index table."
		return 0
	
	if len(sys.argv) < 2:
		sys.stderr.write("No cassini index tables specified, use -h for more info\n")
		return 1
	
	for sIndex in sys.argv[1:]:
		
		fIn = file(sIndex, 'rb')
		nLine = 0
		dProducts = {}
		
		sVolRoot = os.path.abspath(sIndex)
		sVolRoot = os.path.dirname(sVolRoot) # up to INDEX level
		sVolRoot = os.path.dirname(sVolRoot) # up to VOLUME Root
		
		print "Report for volume: %s"%os.path.basename(sVolRoot)
		print "========================================"
		
		for sLine in fIn:
			nLine += 1
			if nLine < 2:
				continue
			
			sProduct = sLine[38:78].strip()
			if sProduct not in dProducts.keys():
				dProducts[sProduct] = [0L,0L]
			
			sTmp = sLine[183:256].strip()
			sPath = pjoin(sVolRoot, sTmp)
			
			#Strip extension
			sPath = sPath[:-4]
		
			#1st look for .TAB, then .DAT
			sDataPath = ""
			nFound = 0
			
			lTmp = ["%s.TAB", "%s.DAT", "%s.PKT"]
			for sTmp in lTmp:
				if os.path.exists(sTmp%sPath):
					nFound += 1
					sDataPath = sTmp%sPath				
		
			if nFound > 1:
				sys.stderr.write("ERROR: Found more that one of *.DAT, *.PKT and *.TAB file for label %s.LBL\n"%sPath)
				continue
				
			if nFound < 1:
				sys.stderr.write("ERROR: Couldn't find a *.DAT, *.PKT or *.TAB file for label %s.LBL\n"%sPath)
				continue

			dProducts[sProduct][0] += 1
			
			if os.path.exists(sDataPath):
				dProducts[sProduct][1] += os.path.getsize(sDataPath)
			else:
				sys.stderr.write("ERROR: File %s does not exist!\n"%sDataPath)
				continue
		
		fIn.close()
		# Out of per line loop, print report, use explicit keys to keep
		# the order constant.
		
		lKeys = ['CO-V/E/J/S/SS-RPWS-4-SUMM-KEY60S-V1.0',
		         'CO-V/E/J/S/SS-RPWS-2-REFDR-ALL-V1.0',
		         'CO-V/E/J/S/SS-RPWS-3-RDR-LRFULL-V1.0',
		         'CO-V/E/J/S/SS-RPWS-2-REFDR-WBRFULL-V1.0',
		         'CO-V/E/J/S/SS-RPWS-2-REFDR-WFRFULL-V1.0']
		
		for sKey in lKeys:
			lVal = dProducts[sKey]
			
			print "%-40s: %7s in %4s data files."%(sKey, 
			   ipds.util.bytes2str(lVal[1], "M", True), lVal[0])
				
		print
	
	return 0

if __name__ == "__main__":
	sys.exit(main())
