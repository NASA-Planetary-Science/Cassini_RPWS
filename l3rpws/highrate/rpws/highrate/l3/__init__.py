import logging
import sys

from wbr import WbrRecord, readWbr, findWbr
from wfr import WfrRecord, readWfr, findWfr

##############################################################################
def setupLogging(sLogLevel, sFile=None, bStderr=True):
	"""Utility to setup standard python logger.
	sLogLevel - Logging level, starts with one of: C,c,E,e,W,w,I,i,D,d 
	           [critical,error,warning,info,debug]
				
	bStderror - Log to standard error, if False don't log to standard error
	
	sFile - If not empty, open the stated file for logging.
	
	Returns: None, can throw IOerror.
	"""

	sLevel = sLogLevel.lower()
	nLevel = logging.WARNING
	
	sDateFmt = '%Y-%m-%d %H:%M:%S'
	sFileFmt = '%(asctime)s %(levelname)-8s: %(message)s'
	sConFmt = '%(levelname)-8s: %(message)s'
	
	if sLevel.startswith("c"):
		nLevel = logging.CRITICAL
	elif sLevel.startswith("e"):
		nLevel = logging.ERROR
	elif sLevel.startswith("i"):
		nLevel = logging.INFO
	elif sLevel.startswith("d"):
		nLevel = logging.DEBUG
		sFileFmt = '%(asctime)s %(name)-12s %(levelname)-8s: %(message)s'
		sConFmt = '%(name)-12s %(levelname)-8s: %(message)s'
	
	#Logging options:  Console,File|File|Console|None(acually console force crit)
	rootLogger = logging.getLogger('')
	rootLogger.setLevel(nLevel)
	
	
	if sFile != None:
		fileHdlr = logging.FileHandler(sFile, 'w')
		formatter = logging.Formatter(sFileFmt, sDateFmt)
		fileHdlr.setFormatter(formatter)
		rootLogger.addHandler(fileHdlr)
	
	#For all cases except file only logging we need a console handler
	if bStderr or (not bStderr and (sFile == "")):
		conHdlr = logging.StreamHandler(sys.stderr)
		formatter = logging.Formatter(sConFmt, sDateFmt)
		conHdlr.setFormatter(formatter)
		rootLogger.addHandler(conHdlr)
	
		if not bStderr and sFile == "":
			#In the special case that we are not supposed to log at all, make a
			#console handler that never reports anything.
			logging.addLevelName(60, 'NEVER')
			rootLogger.setLevel(60)
		
	return rootLogger


##############################################################################

def hrRecSort(rec1, rec2):
	"""Time Sorting function for WBR records.   Returns a comparison by SCLK
	"""
	
	nCmp = cmp(rec1.nSclkPart, rec2.nSclkPart)
	if nCmp != 0:
		return nCmp
	
	nCmp = cmp(rec1.nSclkSec, rec2.nSclkSec)
	if nCmp != 0:
		return nCmp

	return cmp(rec1.nSclkFine, rec2.nSclkFine)
