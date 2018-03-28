#!/usr/bin/env python

"""Troll through all the low-rate complete datasets and make a table 
of all modes encountered.  A mode is defined by:


  1. The Frequency Table
  2. The anteanna

"""

import sys
import os.path
import os
import struct
import hashlib

#g_sTop = "/opt/project/cassini/pds/DATA/RPWS_LOW_RATE_FULL"

g_dAnt = {0:'Ex', 1:'Eu', 2:'Ev', 3:'Ew', 4:'Bx', 5:'By', 6:'Bz', 8:'Hf', 11:'Lp'}

##############################################################################
def main(lArgs):

	perr = sys.stderr.write
	pout = sys.stdout.write
	
	if len(lArgs) < 1:
		perr("ERROR: No starting directory path provided\n"+\
		     "       Usage %s DIRECTORY [PREV_OUTPUT_FILE]\n"%lArgs[0])
		return 4

	# Each entry is the tuple
	# (rBegFreq, rEndFreq, nFreq, sAnt, nRowCount, sFileMnemonic)
	#
	# The file mnemonic is the stuff between _ and the first digit in
	# the filename.
	#
	# Each key is "%s+%s"%(sAnt, md5 hexdigest of lRawFreqBytes)
	# 
	dModes = {}
	
	fIn = None
	nUnknownCount = 0
	for (sDir, lSubDirs, lFiles) in os.walk(lArgs[1], followlinks=True):
		
		for sFile in lFiles:
			if fIn != None:
				fIn.close()
				fIn = None
		
			if not sFile.endswith(".DAT"):
				continue
				
			i = sFile.rfind('_')
			if i == -1:
				perr("ERROR: Unexpected filename pattern %s\n"%sPath)
				continue
			
			j = sFile.rfind('.')
			if j == -1:
				perr("ERROR: Unexpected filename pattern %s\n"%sPath)
				continue
				
			if j <= i+1:
				perr("ERROR: Unexpected filename pattern %s\n"%sPath)
				continue
			
			sTmp = sFile[i+1: j]
			sMnemonic = ""
			for c in sTmp:
				if not c.isdigit():
					sMnemonic += c
			
			sPath = os.path.join(sDir, sFile)
			perr("INFO: Reading %s\n"%sPath)
			fIn = file(sPath, 'rb')
			
			sHdrRow = fIn.read(12)
			if len(sHdrRow) != 12:
				perr("ERROR: Reading %s"%sPath)
				continue
				
			(nRecBytes,) = struct.unpack(">I", sHdrRow[8:12])
			sHdrRow += fIn.read(nRecBytes - 12)
			if len(sHdrRow) != nRecBytes:
				perr("ERROR: Reading %s\n"%sPath)
				continue
			
			for i in xrange(0,2):
				# First row is time offsets, we over-write those here
				sFreqRow = fIn.read(nRecBytes)
				if len(sFreqRow) != nRecBytes:
					perr("ERROR: Reading %s\n"%sPath)
					continue
			
			sFreq = sFreqRow[16:]
			if len(sFreq) < 8:
				perr("ERROR: Less that two frequencies in %s\n"%sPath)
				continue
			
			if len(sFreq) % 4 != 0:
				perr("ERROR: Uneven number of frequency bytes %d in %s\n"%(len(sFreq),sPath))
				continue
			
			nFreq = len(sFreq) / 4
				
			(rBegFreq, rEndFreq) = struct.unpack(">ff", sFreq[:4] + sFreq[-4:])
			
			md5 = hashlib.md5()
			md5.update(sFreq)
			sFreqHash = md5.hexdigest()
			
			# Now start reading each row to get the antennas
			sRow = fIn.read(nRecBytes)
			while len(sRow) == nRecBytes:
				
				(nStatus,) = struct.unpack(">I", sRow[12:16])
				
				nAnt = (nStatus >> 28) & 0xF
				
				if nAnt not in g_dAnt:
					perr("WARNING: Unexpected antenna id: %d, in %s\n"%(nAnt, sPath))
					nUnknownCount += 1
					sAnt = "Ant_%d"%nAnt
				else:
					sAnt = g_dAnt[nAnt]
					
				# Okay, see if we've seen this mode before
				sKey = "%s+%s"%(sAnt, sFreqHash)
				if sKey not in dModes:
					dModes[sKey] = [rBegFreq, rEndFreq, nFreq, sAnt, 1, sMnemonic]
				else:
					dModes[sKey][4] += 1
					
				sRow = fIn.read(nRecBytes)	
	
	# Output the table
	pout('"MODE_KEY","BEGIN_FREQ","END_FREQ","NUM_FREQS","ANTENNA","ROW_COUNT","FILE_TOKEN"\r\n')
	
	for sKey in dModes:
		pout('"%s",'%sKey)
		rec = dModes[sKey]
		pout('%.4e,%.4e,%d,"%s",%d,"%s"\r\n'%tuple(rec))
	
	
	if nUnknownCount > 0:
		perr("%d unknown antenna rows encountered\n"%nUnknownCount)
		return 12
		
	return 0
	
	
if __name__ == "__main__":
	sys.exit(main(sys.argv))
