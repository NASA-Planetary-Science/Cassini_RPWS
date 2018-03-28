import os.path
import shutil
import subprocess
import tempfile
import sys

import warnings  # Get rid of the tempnam admonisment

from os.path import basename as bname
from os.path import join as pjoin

import rpw_pds.util as U
import rpw_pds.parse as P

import das2

##############################################################################
# Check for updates to static volume files

def ckReplace(log, sSrcPath, sDestPath, bOverwrite):
	
	sDirName = os.path.dirname(sDestPath)
	if not os.path.isdir(sDirName):
		os.makedirs(sDirName)
	
	if not os.path.isfile(sSrcPath):
		log.error("%s: Config file doesn't exist"%sSrcPath)
		return False
	
	if not os.path.isfile(sDestPath):
		log.info("Copying in %s"%bname(sDestPath))
		shutil.copy2(sSrcPath, sDestPath)		
		return True
	
	sSrcSum = U.getFileHash(sSrcPath)
	sDestSum = U.getFileHash(sDestPath)
	
	if sSrcSum != sDestSum:
	
		nSrcTm = os.path.getmtime(sSrcPath)
		nDestTm = os.path.getmtime(sDestPath)
		
		if nSrcTm < nDestTm and not bOverwrite:
			log.error("Source %s is older than the destination "%sSrcPath +\
			          "%s (use -w overwrite anyway)"%sDestPath)
			return False
	
		log.info("Updating %s"%bname(sDestPath))
		shutil.copy2(sSrcPath, sDestPath)
	else:
		log.info("%s already up2date", bname(sDestPath))
	
	return True
	
##############################################################################
def getScetRng(log, sVolRoot):
	"""Get the SCET range of data on a volume to day accuracy by reading
	   INDEX.LBL"""
	

	sFirstScet = None
	sLastScet = None

	sIdxLbl = pjoin(sVolRoot, 'INDEX', 'INDEX.LBL')
	
	if not os.path.isfile(sIdxLbl):
		log.error("%s: File is missing, run your volume indexer to fix this"%sIdxLbl)
		return (None, None)


	dTmp = P.extractFromRoot(sIdxLbl, ['START_TIME', 'STOP_TIME'], 3, True)
	sFirstScet = dTmp['START_TIME'].strip('"')[:10]
	sLastScet = dTmp['STOP_TIME'].strip('"')[:10]
	
	if len(sFirstScet) < 10:
		log.error("%s: START_TIME less than 10 chars long"%bname(sIdxLbl))
		return (None, None)
		
	if len(sLastScet) < 10:
		log.error("%s: STOP_TIME less than 10 chars long"%bname(sIdxLbl))
		return (None, None)
	
	if sFirstScet == sLastScet:
		log.error("%s: START_TIME == STOP_TIME to within a day"%bname(sIdxLbl))
		return (None, None)
	
	try:
		i = int(sFirstScet[:4])
		i = int(sLastScet[:4])
		
		i = int(sFirstScet[5:7])
		i = int(sLastScet[5:7])
		
		i = int(sFirstScet[8:10])
		i = int(sLastScet[8:10])
	
	except ValueError, e:
		log.error("%s: Malformed START_TIME or STOP_TIME"%bname(sIdxLbl))
		return (None, None)

	return (sFirstScet, sLastScet)
	
	
##############################################################################
# data verification

def checkLabelProdId(log, sLblPath):
	"""Verify that the release number in a label file's name is the same
	as the PRODUCT_VERSION_ID in the label itself.
	"""
	
	try:
		nFileVer = int(sLblPath[-6:-4], 10)
	except ValueError, e:
		log.error("Can't get version number for file %s"%sLblPath)
		return False
		
		
	fIn = file(sLblPath, 'rb')
	for sLine in fIn:
		lLine = sLine.split()
		if len(lLine) >= 3:
			if lLine[0] == 'PRODUCT_VERSION_ID':
				sVer = lLine[2].strip('"')
				try:
					nLblVer = int(sVer, 10)
				except ValueError, e:
					log.error("Couldn't parse version number '%s' in %s"%(sVer, 
					          sLblPath))
					fIn.close()
					return False
				
				if nLblVer == nFileVer:
					fIn.close()
					return True
	
	fIn.close()
	
	log.error("Couldn't find PRODUCT_VERSION_ID in %s"%sLblPath)
	return False


##############################################################################

def mostRecentMtime(log, sTopDir, lExt):
	"""Get most recent modification time, but skip VOLDESC.CAT"""

	nMtime = 0

	for sRootDir, lSubDirs, lFiles in os.walk(sTopDir):

		for sFile in lFiles:
			if sFile == "VOLDESC.CAT":
				continue

			for sExt in lExt:

				sPath = os.path.join(sRootDir, sFile)

				if sFile.endswith(sExt):
					 nMcur = os.path.getmtime(sPath)
					 if nMcur > nMtime:
					 	nMtime = nMcur


	return nMtime

##############################################################################
# Handle replacements for doc files

def handleFileWithSub(log, sInput, dRep, sOutput):
	
	fInput = file( sInput, 'rb')
	sFmt = fInput.read()
	fInput.close()
	
	# Get rid of the tempnam warning
	warnings.simplefilter('ignore', RuntimeWarning)
	sTest = os.tempnam()
	fOut = file(sTest, 'wb')
	fOut.write(sFmt%dRep)
	fOut.close()
	
	sSrcHash = U.getFileHash(sTest)
	sDestHash = None
	if os.path.isfile( sOutput ):
		sDestHash = U.getFileHash( sOutput )
	
	sBasename = bname(sOutput)

	if sSrcHash != sDestHash:
		log.info("Updating %s"%sBasename)
		shutil.copy2(sTest, sOutput )
	else:
		log.info("%s already up2date"%sBasename)
	
	os.remove(sTest)

	return 0
