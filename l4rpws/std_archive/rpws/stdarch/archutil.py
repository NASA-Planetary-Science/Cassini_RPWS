#!/usr/bin/env python

"""Utilities for CASSINI RPWS PDS archiving"""

#Python stdlib imports
import os
import os.path
from os.path import join as pjoin
import logging
from copy import deepcopy
import sys
import shutil

#related module imports
import rpw_pds.util
import rpw_pds.parse

#Module Local imports
import cas_rewrite

import pspice

##############################################################################
# Global, CASSINI and storage location specific information

g_sDataIndexFmt = '"%-11s","%-20s","%-40s","%-30s","%-22s","%-22s",'+\
                  '"%-16s","%-73s","%-10s"\r\n'

##############################################################################

def _get100dir(date):
	"""utility func, get str of type T%sXX given a DOYdate
	"""
	sTmp = str(date)
	if date.nYear < 1000 or date.nYear > 9999:
		log = logging.getLogger('_get100dir')
		sTmp = "Unexpected date range '%s'."%date
		log.critical(sTmp)
		raise ValueError(sTmp)
		
	return "T%sXX"%sTmp[:5]

##############################################################################
def linkMatchingSubs(sSrcTop, sDestTop, sSub):
	"""Link a sub directory in sSrcTop to an equivalent sub directory in
	sDestTop, and return the size of all the files in the linked directory.
	
	MISSING FEATURE: if sSub is more than one level below sSrcTop then
	   this function does not create the required parents in sDestTop.
	"""
	log = logging.getLogger('linkMatchingSubs')
	
	sDir = pjoin(sDestTop, sSub)
	if not os.path.islink(sDir):
		os.symlink(pjoin(sSrcTop, sSub), sDir)
	
	(nSize, nFiles) = rpw_pds.util.getDirSizeLk(sDir)
	
	log.info('Linking: %-50s %s in %3d files.'%(sDir,rpw_pds.util.bytes2str(nSize),
		                                   nFiles))
	return nSize


def linkUnchanged(sTpltVol, sSuperVol, sStageDir):
	"""Create symlinks to the unchanging content and return it's size.
			
	The unchanged content is assumed to be under sSuperVol.  The unchanged
	content consists of (on the output disk):
	 
	   CATALOG/
	   DOUCMENT/
		LABEL/
		BROWSE/ANCILLARY
		
		EXTRAS/WBRWFR.TXT
		EXTRAS/EXTRINFO.TXT
		EXTRAS/SEQUENCE_INFO   <---*except* INDEX.HTM
		EXTRAS/SOFTWARE        <---*except* KEY_BROWSE.TXT (lnDataMkIndex)
		
		
	The following items are assumed to be in the sTpltVol.  And are 
	linked unchanged.
	
		AUTORUN.INF
		AUTORUN.LBL
		SHRUN.INF
		SHRUN.LBL
	
		EXTRAS/SHELLRUN.EXE
		EXTRAS/RPWS.ICO
		
	Returns:  The size of the statically linked objects.
	
	Throws:  Directories and links are expected not to exist when the
	   program is run.  By design, this throws IOerrors etc if they
	   aready exist.
	
	NOTE: See function, makeFromTemplate, and generateIndicies for
	       other functions that return size.
	"""	
	nSize = 0L
	log = logging.getLogger('linkUnchanged')
	
	# root of the volume
	for sFile in ['AUTORUN.INF','AUTORUN.LBL','SHRUN.INF','SHRUN.LBL']:
		sDest = pjoin(sStageDir, sFile)
		#if not os.path.isfile(sDest):
		nSize += rpw_pds.util.symLinkCkSize(pjoin(sTpltVol, sFile), sDest)
		
	
	# CATALOG, DOCUMENT, LABEL
	nSize += linkMatchingSubs(sSuperVol, sStageDir, 'CATALOG')
	nSize += linkMatchingSubs(sSuperVol, sStageDir, 'DOCUMENT')
	nSize += linkMatchingSubs(sSuperVol, sStageDir, 'LABEL')
	
	# BROWSE/ANCILLARY
	sBrowse = pjoin(sStageDir, 'BROWSE')
	#if not os.path.isdir(sBrowse):
	os.mkdir(sBrowse)
	nSize += linkMatchingSubs(pjoin(sSuperVol, 'BROWSE'),
	                          sBrowse, 'ANCILLARY');
									  
	# EXTRAS
	sExtras = pjoin(sStageDir, 'EXTRAS')
	#if not os.path.isdir(sExtras):
	os.mkdir(sExtras)
	
	
	dTmp = { 'WBRWFR.TXT':sSuperVol, 'EXTRINFO.TXT': sSuperVol,
		   'SHELLRUN.EXE':sTpltVol, 'RPWS.ICO':sTpltVol }
	
	for sFile, sTopSrc in dTmp.iteritems():
		sSrc = pjoin(sTopSrc, 'EXTRAS', sFile)
		sDest = pjoin(sStageDir, 'EXTRAS', sFile)
		#if not os.path.isfile(sDest):
		nSize += rpw_pds.util.symLinkCkSize(sSrc, sDest)
	
	
	# EXTRAS/SOFTWARE, EXTRAS/SEQUENCE_INFO
	for sExtraSub in ['SEQUENCE_INFO','SOFTWARE']:
		
		nFiles = 0
		nLocSize = 0
		sOutDir = pjoin(sExtras, sExtraSub)
		os.mkdir(sOutDir)
						
		for sFile in os.listdir(pjoin(sSuperVol, 'EXTRAS', sExtraSub)):
			# isupper returns true if character is cased and is upper case
			# so the string '1' is not upper.

			if not sFile[0].isupper():
				continue
			
			if sExtraSub == 'SOFTWARE' and not sFile[-1].isupper():
				continue
		
			if (sExtraSub == 'SOFTWARE' and  sFile == 'KEY_BROWSE.TXT') or\
			   (sExtraSub == 'SEQUENCE_INFO' and sFile == 'INDEX.HTM'):
				# EXTRAS/SOFTWARE/KEY_BROWSE.TXT and
				# EXTRAS/SEQUENCE_INFO/INDEX.HTM need special processing
				continue
			
			sSrc = pjoin(sSuperVol,'EXTRAS', sExtraSub, sFile)
			sDest = pjoin(sStageDir, 'EXTRAS', sExtraSub, sFile)
	
			os.symlink(sSrc, sDest)
			
			if os.path.isfile(sDest):
				nFiles += 1
				nLocSize += os.path.getsize(sDest)
			else:
				(nSzTmp, nFcTmp) = rpw_pds.util.getDirSizeLk(sDest)
				nFiles += nFcTmp
				nLocSize += nSzTmp
			
		log.info('Linking: %-50s %s in %3d files.'%(
		         sOutDir, rpw_pds.util.bytes2str(nLocSize), nFiles))
		nSize += nLocSize
	return nSize


##############################################################################
def getSizeDirsIn100(sTop, date):
	"""Find out how much data is in a given RPWS data dir for a given day.
	
	   sTop - The subdirectory of to look in, assumes sub dirs are in the
		   format: TyyyydXX/Tyyyyddd
		
		date - a rpw_pds.util.DOYdate object.
		
		NOTE:  This function assues that the given sSubDir contains one
		   folder for each 100 day period (will 3XX is actuall 65 or 66 days)
			and that each 100 day folder contains a day folder.  It is not
			suited for KEY_PARAMETERS data and BROWSE data.
	"""
	
	log = logging.getLogger('getSizeDirsIn100')
	
	sPath = pjoin(sTop, _get100dir(date), "T%s"%date)
	if not os.path.isdir(sPath):
		log.warning('Expected one day data directory: %s does not exist'%sPath)
		return 0L
	
	nSize = 0L
	nFiles = 0
	for sFile in os.listdir(sPath):
		log.debug("Listing: %s"%sPath)
		nSize += os.path.getsize(pjoin(sPath, sFile))
		nFiles += 1
	
	log.debug('%-66s %s in %3d files.'%(sPath, rpw_pds.util.bytes2str(nSize), nFiles))
	
	return nSize


##############################################################################
def getSizeFilesIn100(sTop, date):
	"""Find out how much data is in a given RPWS data directory for a given day
	
		sTop - dir to search
		
		date - a rpw_pds.util.DOYdate obj
	
	   NOTE: This function assumes that the given dir contains one folder for
		  each 100 day period (were 3XX is actually 65 or 66 days) and that
		  the 100 day folder contains *NO* subdirectories.   It is assumed that
		  files for each day are in the format *YYYYDDD*.
	"""
	
	log = logging.getLogger('getSizeFilesIn100')

	sPath = pjoin(sTop, _get100dir(date))
	
	if not os.path.isdir(sPath):
		log.warning('Expected 100 day data dir: %s does not exist'%sPath)
		return 0L
	
	nSize = 0L
	nFiles = 0
	for sFile in os.listdir(sPath):
		
		if sFile.find(str(date)) > -1:
			nSize += os.path.getsize(pjoin(sPath, sFile))
			nFiles += 1
	
	log.debug('%-66s %s in %3d files.'%(sPath, rpw_pds.util.bytes2str(nSize), nFiles))
	
	return nSize

##############################################################################
def daysToFillSize(sSuperVol, doyStart, nMaxBytes = 4400000000L, nMaxDays=60):
	"""Given a start date, and a maximum size, caculate the highest end day
	for cassini PDS data that would not exceed the given size.
	
	sSuperVol -- The location of the Cassini RPWS supervolume
	
	doyStart -- A DOYdate object containing the *inclusive* start day for
	   cassini data.
	
	nSizeMB -- The maximum allowed size in megabytes, NOTE: if making a DVD
	     don't set this to 4700, you're going to need room for CATELOGs and
		  other non-date stuff.
	
	Returns -- List of DOYdate objects that will fit in the given size.
		  The list can be empty if no days will fit.
	"""
	
	log = logging.getLogger('daysToFillSize')
	
	lTwoLvls = []
	lTwoLvls.append(pjoin(sSuperVol, 'DATA', 'RPWS_LOW_RATE_FULL'))
	lTwoLvls.append(pjoin(sSuperVol, 'DATA', 'RPWS_RAW_COMPLETE'))
	lTwoLvls.append(pjoin(sSuperVol, 'DATA', 'RPWS_WAVEFORM_FULL'))
	lTwoLvls.append(pjoin(sSuperVol, 'DATA', 'RPWS_WIDEBAND_FULL'))
	lTwoLvls.append(pjoin(sSuperVol, 'BROWSE', 'RPWS_WAVEFORM_FULL'))
	lTwoLvls.append(pjoin(sSuperVol, 'BROWSE', 'RPWS_WIDEBAND_FULL'))
	
	lOneLvl = []
	lOneLvl.append(pjoin(sSuperVol, 'DATA', 'RPWS_KEY_PARAMETERS'))
	lOneLvl.append(pjoin(sSuperVol, 'BROWSE', 'RPWS_KEY_PARAMETERS'))
	lOneLvl.append(pjoin(sSuperVol, 'BROWSE', 'RPWS_LOW_RATE_FULL'))

	# Find out how many days need to be in this volume, based on size...
	nSize = 0L
	curDay = deepcopy(doyStart)
	lDaysInSet = []
	
	while True:
		
		nDaySize = 0L #One time around this loop per day
		
		# ...in directories where structure is 2 levels deep
		for sPath in lTwoLvls:
			nDaySize += getSizeDirsIn100(sPath, curDay)
		
		# ...in directories where strucuture is 1 level deep		
		for sPath in lOneLvl:
			nDaySize += getSizeFilesIn100(sPath, curDay)
		
		
		log.info("Size Ck: Day %s,  %s"%(curDay, rpw_pds.util.bytes2str(nDaySize)))
		
		nSize += nDaySize
		if nSize <= nMaxBytes:
			
			lDaysInSet.append(rpw_pds.util.DOYdate( curDay.nYear, curDay.nDOY ))
			
			if len(lDaysInSet) >= nMaxDays:
				sTmp = "Terminating data search, insufficient data to fill "
				sTmp += "%s in %d days"%(rpw_pds.util.bytes2str(nMaxBytes, 'M'), nMaxDays)
				raise RuntimeError(sTmp)
				break
				
			curDay.incDay()
		else:
			doyTmp = deepcopy(lDaysInSet[-1])
			doyTmp.incDay()
			log.info("Day %s, brings total to %s, backing off one day."%(
			         doyTmp, rpw_pds.util.bytes2str(nSize, 'M')))
			nSize -= nDaySize
			break
	
	log.info('%s found in %d days'%(rpw_pds.util.bytes2str(nSize, 'M'),
	                                len(lDaysInSet)))
	
	if len(lDaysInSet) < 1:
		sTmp="Starting at %s not even a single day's data will fit."%doyStart
		log.critical(sTmp)
		raise RuntimeError(sTmp)
	
	return lDaysInSet
	
##############################################################################
def dataSizeForDay(sSuperVol, date):
	"""Find the amount of data on the super volume for a date's data
	
	   date - a DOYdate object
	"""
	nSize = 0L
	sV = sSuperVol
	
	nSize += getSizeDirsIn100(pjoin(sV,'DATA','RPWS_LOW_RATE_FULL'), date)
	nSize += getSizeDirsIn100(pjoin(sV, 'DATA', 'RPWS_RAW_COMPLETE'), date)
	nSize += getSizeDirsIn100(pjoin(sV, 'DATA', 'RPWS_WAVEFORM_FULL'), date)
	nSize += getSizeDirsIn100(pjoin(sV, 'DATA', 'RPWS_WIDEBAND_FULL'), date)
	nSize += getSizeDirsIn100(pjoin(sV, 'BROWSE', 'RPWS_WAVEFORM_FULL'), date)
	nSize += getSizeDirsIn100(pjoin(sV, 'BROWSE', 'RPWS_WIDEBAND_FULL'), date)
	
	nSize += getSizeFilesIn100(pjoin(sV, 'DATA', 'RPWS_KEY_PARAMETERS'), date)
	nSize += getSizeFilesIn100(pjoin(sV, 'BROWSE', 'RPWS_KEY_PARAMETERS'), date)
	nSize += getSizeFilesIn100(pjoin(sV, 'BROWSE', 'RPWS_LOW_RATE_FULL'), date)
	
	log = logging.getLogger('daySizeForDay')
	
	log.info("Size Ck: Day %s,  %s"%(date, rpw_pds.util.bytes2str(nSize)))
	return nSize


def mkAndCkFixedDateRange(sSuperVol, doyStart, doyEnd, nMaxBytes):
	"""Given a start day and *exclusive* end day check to see if data will fit.
		
		doyStart - Start Date (DOYdate object)
		doyEnd   - Exclusive End Date (DOYdate object)
		nMaxBytes - Max space that files can use in the date range
		
		Returns:  A list of date values.
	"""
	log = logging.getLogger('mkAndCkFixedDateRange')
	
	doyTmp = deepcopy(doyStart)
			
	nSize = 0L
	
	sErrReport = ""
	lDates = []
	while doyTmp < doyEnd:
		nTmp = dataSizeForDay(sSuperVol, doyTmp)
		sErrReport += "     Day %s:     %s\n"%(doyTmp, rpw_pds.util.bytes2str(nTmp))
		nSize += nTmp
		lDates.append(deepcopy(doyTmp))
		doyTmp.incDay()
				
	if nSize > nMaxBytes:
		sTmp = "Data exceeds max space per volume (overridable with -m).\n"
		sTmp +="Space required %s, space allowed after "%rpw_pds.util.bytes2str(nSize)
		sTmp +="static content %s.\n Details:\n"%rpw_pds.util.bytes2str(nSize)
		
		sTmp += sErrReport
		log.critical(sTmp)
		sTmp = 'Insufficent space for data in days %s to %s'%(doyStart, doyEnd)
		raise RuntimeError(sTmp)
			
	else:
		log.info('%s found in %d days'%(rpw_pds.util.bytes2str(nSize, 'M'),
	                                len(lDates)))
											  
	return lDates

############################################################################
# Some little checking routines for linkDataMkIndex

def checkAssociatedFileExists(sInPath, lExts):
	"""Given a complete path name make sure that another file exist with the
	same dir and basename, but with a different 'extension'.  Only one other
	file should match.
	"""
	i = sInPath.rfind('.')
	if i == -1:
		raise ValueError("sInPath: %s doesn't contain a . character"%sInPath)
	sBase = sInPath[:i]
	
	n = 0
	for sExt in lExts:
		if sExt.rfind('.') == -1:
			raise ValueError("Extenstion: %s doesn't contain a . character"%sExt)
			
		if os.path.isfile("%s%s"%(sBase,sExt)):
			n += 1
	
	if n == 1:
		return None
	else:
		log = logging.getLogger('checkAssociatedFileExists')
		if n == 0:
			log.error("%s: can't find assiociated file with at least one ext in %s."%(
			          sInPath, lExts))
		else:
			log.error("%s: has more than one assiociated file with at least one ext in %s."%(
			          sInPath, lExts))
		
		raise ValueError('Missing file')

def checkTimeRange(doyStart, doyStop, lPDStimes):
	"""check to see if list of PDS times are inclusivly bounded by start and
	stop date objects.
	"""
	
	return None

############################################################################
def linkDataAncillary(sSuperVol, sStageDir, lDates):
	"""Make a link to the proper 100 day directories containing sequence info."""
	
	nSize = 0L
	
	log = logging.getLogger('linkDataAncillary')
	
	sAncPath = pjoin(sStageDir, 'DATA','ANCILLARY')
	if not os.path.isdir(sAncPath):
		os.makedirs(sAncPath)
		
	l100Days = []
	for date in lDates:
		s100day = "T%sXX"%(str(date)[:5])
		if s100day not in l100Days:
			l100Days.append(s100day)
			
	for sDir in l100Days:
		sSrc = pjoin(sSuperVol, 'DATA', 'ANCILLARY', sDir)
		sDest = pjoin(sStageDir, 'DATA','ANCILLARY', sDir)
		log.info('linking: %s'%sDest)
		try:
			os.symlink( sSrc, sDest)
		except OSError, e:
			raise RuntimeError('%s -> %s: symlink error %s.'%(sSrc, sDest, e.strerror))
						
		(nTmp, nFiles) = rpw_pds.util.getDirSizeLk(sSrc)
		nSize += nTmp

	return nSize

############################################################################
# Main data linker and index creator

def linkDataMkIndex(sTpltVol, sSuperVol, sStageDir, nVol, lDates, datePub):
	"""Make data links and generate data indicies.
	
	Parameters:
	   sSuperVol - location of the RPWS supervolume
	   sStageDir - disk mastering directory
		nVol - the volume number
		lDate - a list of DOYdate objects, one for each day to link 
		datePub - the publish date 
	
	Outputs:
	   All links under DATA
	   sStage/INDEX/INDEX.TAB
	   sStage/INDEX/INDEX.LBL
		sStage/EXTRAS/SOFTWARE/KEY_BROWSE.TXT
	   sTpltVol/INDEX/index.0XXX.tab(,n)
      sTpltVol/INDEX/index.0XXX.lbl(,n)
	
	Returns:  (nSize of data created, List of template vol files created)
	Throws:   Varies
	Logs:     Yes
	"""
	global g_sDataIndexFmt
	
	log = logging.getLogger('linkDataMkIndex')
	nSize = 0L

	# Each row of the index items (idx) looks like:
	# DATE_SET_ID, PRODUCT_ID, START_TIME, STOP_TIME, SPACECRAFT_CLOCK, 
	# FILE_SPECIFICATION_NAME, PRODUCT_CR
	
	lSets = [
	   {'name':'RPWS_KEY_PARAMETERS','nest2':False ,'idx':[], 
		 'ext':['.TAB']}, # ext .LBL is implicit
		 
		{'name':'RPWS_LOW_RATE_FULL' ,'nest2':True  ,'idx':[], 
		 'ext':['.DAT','.TAB']}, # ext .LBL is implicit
		 
		{'name':'RPWS_RAW_COMPLETE'  ,'nest2':True  ,'idx':[], 
		 'ext':['.PKT']}, # ext .LBL is implicit
		 
		{'name':'RPWS_WAVEFORM_FULL' ,'nest2':True  ,'idx':[],
		 'ext':['.DAT']}, # ext .LBL is implicit
		 
		{'name':'RPWS_WIDEBAND_FULL' ,'nest2':True  ,'idx':[],
		 'ext':['.DAT']} # ext .LBL is implicit
	]
	
	#I hate shallow copies, (ARRG)
	#doyStop = rpw_pds.util.DOYdate(lDates[-1].nYear, lDates[-1].nDOY)
	doyStop = deepcopy(lDates[-1])
	doyStop.incDay()
	dIdxLbl = {
		'vol-id':"CORPWS_%04i"%nVol,
		'pub-date':datePub.getDOMstr(),
		'num-labels':0,     # Just keep track of all label files
		'index-rows':1,     # One plus the number of labels referenced in the index
		'start-time':"%04d-%03dT00:00"%(lDates[0].nYear, lDates[0].nDOY),
		'stop-time':"%04d-%03dT00:00"%(doyStop.nYear, doyStop.nDOY),
		'TARGS':[],       # Keep all unique while parsing lbls
		'targets':None,   #Str version of TARGS
		'PHASES':[],      # Keep all unique while parsing lbls
		'phases':None,    #Str version of PHASES
		'DATASETS':[],    # Keep all unique while parsing lbls    
		'datasets':None   #Str version of datasets
	}
	
	#Fields to extract from each data label:
	lExtract = ['DATA_SET_ID','PRODUCT_ID','START_TIME','STOP_TIME',
	            'SPACECRAFT_CLOCK_START_COUNT','PRODUCT_CREATION_TIME',
	            'MISSION_PHASE_NAME','TARGET_NAME','DATA_SET_ID']
	
	dKeyBrowse = {}  #Collect all .TAB files in Key Params for KEY_BROWSE.TXT
	
	for dSet in lSets:
		log.info("Linking data for dataset %s."%dSet['name'])
	
		for date in lDates:
			
			sSrcDir = pjoin(sSuperVol, 'DATA', dSet['name'], _get100dir(date))
			
			if dSet['nest2']:
				sSrcDir = pjoin(sSrcDir, "T%s"%date)
				
			if not os.path.isdir(sSrcDir):
				continue
				
			log.info("    Day: %s"%date)
				
			sDestDir = sSrcDir.replace(sSuperVol, sStageDir, 1)
	
			for sFile in os.listdir(sSrcDir):
				sInPath = pjoin(sSrcDir,sFile)
				sOutPath = pjoin(sDestDir, sFile)
				
				if sFile.find(str(date)) < 0:
					continue
				
				if not os.path.isdir(sDestDir):
					os.makedirs(sDestDir)
					
				if sFile.endswith('.LBL'):
					
					checkAssociatedFileExists(sInPath, dSet['ext'])
				
					dLbl = rpw_pds.parse.extractFromRoot(sInPath, lExtract)
					
					dIdxLbl['num-labels'] += 1
					dIdxLbl['index-rows'] += 1
					dIdxLbl['TARGS'] = rpw_pds.util.listMerge(dIdxLbl['TARGS'],
					                                  dLbl['TARGET_NAME'])
					dIdxLbl['PHASES'] = rpw_pds.util.listMerge(dIdxLbl['PHASES'],
					                                   dLbl['MISSION_PHASE_NAME'])
					dIdxLbl['DATASETS'] = rpw_pds.util.listMerge(dIdxLbl['DATASETS'],
					                                     dLbl['DATA_SET_ID'])
					
					checkTimeRange(lDates[0], doyStop,
					               [ dLbl['START_TIME'],dLbl['STOP_TIME'] ] )
						
					# Add the label info to the index for this set,
					# note, the FILE_SPECIFICATION_NAME comes from the path
					# not any lable parsing.
					dSet['idx'].append( 
					  ( dLbl['DATA_SET_ID'].strip('"'), dLbl['PRODUCT_ID'].strip('"'),
					    dLbl['START_TIME'].strip('"'),  dLbl['STOP_TIME'].strip('"'),
						 dLbl['SPACECRAFT_CLOCK_START_COUNT'].strip('"'),
						 sOutPath.replace(sStageDir+os.sep, '', 1),
						 dLbl['PRODUCT_CREATION_TIME'].strip('"')  )
					)
				else:
				
					# if the file doesn't end in .LBL it must end in one of the
					# expected data types for this set, if not then ignore it.
					bIgnore = True
					for sExt in dSet['ext']:
						if sFile.endswith(sExt):
							bIgnore = False
					if bIgnore:
						continue
						
					checkAssociatedFileExists(sInPath, ['.LBL'])
					
					# Special handling for Key Parameters browse data
					if dSet['name'] == 'RPWS_KEY_PARAMETERS' and sFile.endswith('.TAB'):
						sTmp = "%04i-%03i"%(date.nYear, date.nDOY)
						dKeyBrowse[sTmp] = sOutPath.replace(sStageDir, '../..', 1)
						
				
				nSize += rpw_pds.util.symLinkCkSize(sInPath, sOutPath)
	
	#Get EXTRAS/SOFTWARE/KEY_BROWSE.TXT out of the way
	sKeyBrowse = pjoin(sStageDir,'EXTRAS','SOFTWARE','KEY_BROWSE.TXT')
	log.info("Writting '%s'."%sKeyBrowse)
	fKeyBrowse = file(sKeyBrowse, 'wb')
	lTmp = dKeyBrowse.keys()
	lTmp.sort()
	for sKey in lTmp:
		# Unknown extra space at end of line included in the originals.
		fKeyBrowse.write("%s %s \r\n"%(sKey, dKeyBrowse[sKey])) 
	fKeyBrowse.close()
	nSize += os.path.getsize(sKeyBrowse)
	
	
	#Now write the main index label
	dIdxLbl['TARGS'].sort()
	dIdxLbl['PHASES'].sort()
	dIdxLbl['DATASETS'].sort()
	
	dIdxLbl['targets'] =  rpw_pds.util.pdsList(dIdxLbl['TARGS'], 26)
	dIdxLbl['phases'] =   rpw_pds.util.pdsList(dIdxLbl['PHASES'], 26)
	dIdxLbl['datasets'] = rpw_pds.util.pdsList(dIdxLbl['DATASETS'], 26)
	
	nSize += rpw_pds.util.makeFromTemplate(pjoin(sTpltVol, 'INDEX'),
	                               pjoin(sStageDir, 'INDEX'), dIdxLbl, '.r2')
	
	
	#And now the main index file
	sIndexPath = pjoin(sStageDir, 'INDEX', 'INDEX.TAB')
	log.info('Writing INDEX: %s'%sIndexPath)
	fIdx = file(sIndexPath, 'wb')
	
	fIdx.write(g_sDataIndexFmt%(
	  "VOLUME_ID","STANDARD_DATA_PRODUC","DATA_SET_ID","PRODUCT_ID",
	  "START_TIME","STOP_TIME","SPACECRAFT_CLOCK","FILE_SPECIFICATION_NAME",
	  "PRODUCT_CR")
	)
	sVolID = "CORPWS_%04d"%nVol
	
	for dSet in lSets:
		log.info('Sorting dataset %s...'%dSet['name'])
		dSet['idx'].sort()
		for tup in dSet['idx']:
			fIdx.write(g_sDataIndexFmt%(sVolID, dSet['name'], tup[0],tup[1],tup[2],tup[3],
			           tup[4], tup[5], tup[6]))
	
	fIdx.close()
	nSize += os.path.getsize(sIndexPath)
	
	# Now add these index to the list of indicies tracked.
	lBackouts = []
	for tup in [('LBL','lbl'),('TAB','tab')]:
	
		sSrc = pjoin(sStageDir, 'INDEX', 'INDEX.%s'%tup[0])
		sDest = pjoin(sTpltVol, 'INDEX', 'index.%04i.%s'%(nVol, tup[1]))
		
		sDest = rpw_pds.util.mkVerFileName(sDest)
		shutil.copyfile(sSrc, sDest)
		lBackouts.append(sDest)
	
	return (nSize, lBackouts)

############################################################################

def mkCumulativeIndex(sTpltVol, sStageDir, nVol, doyPub, bLowestVer=False):
	"""Make a cumlative index for the current volume.
	
		Copies sStageDir/INDEX/INDEX.??? -> sTpltVol/INDEX/index.[nvol].???
		
		sStageDir - Where the volume is being staged
		
		nVol - This volume's number
		
		doyPub - The publication date for this volume
		
		bRotIndex - If this is true move aside existing indecies for this
		   volume by renaming them as *.0 *.1 *.2 etc.  If bRotIndex is
			false and an index alread exists for the volume an exception is
			thrown.
	"""
	
	log = logging.getLogger('mkCumulativeIndex')
	nSize = 0L
	
	# Doc replace dictionary that will be filled in by this function.
	dCIdxLbl = {
		'vol-id':"CORPWS_%04i"%nVol,  #Current volume ID
		'pub-date':doyPub.getDOMstr(),
		'num-labels':0L,     # Number of data rows in all index files
		'index-rows':1L,     # One plus the number of data rows.
		'start-time':None,  # Earliest start time in first index
		'stop-time':None,   # last stop time in last index
		'targets':None,     # accumulated targets in all index labels
		'phases':None,      # accumulated mission phases in index labels
		'datasets':None     # accumulated datasets in index labels
	}
	
	lExtract = ['DATA_SET_ID','START_TIME','STOP_TIME','TARGET_NAME',
	            'MISSION_PHASE_NAME']
	
	sPathOutTable=pjoin(sStageDir, 'INDEX','CUMINDEX.TAB')
	log.info("Generating: %s"%sPathOutTable)
	
	fOutTbl = file(sPathOutTable, 'wb')
	fOutTbl.write(g_sDataIndexFmt%(
	  "VOLUME_ID","STANDARD_DATA_PRODUC","DATA_SET_ID","PRODUCT_ID",
	  "START_TIME","STOP_TIME","SPACECRAFT_CLOCK","FILE_SPECIFICATION_NAME",
	  "PRODUCT_CR")
	)
	
	#loop over all indicies to include
	lTargs = []
	lPhases = []
	lDatasets = []
	for iVol in range(1, nVol+1):
		sTmp = pjoin(sTpltVol, 'INDEX', 'index.%04i.tab'%iVol)
		sInTbl = rpw_pds.util.getVerFile(sTmp, bLowestVer, True)
		fInTbl = file(sInTbl, 'rb')
		
		nInLine = 0L
		sLine = 'junk'
		lLines = []
		log.info('    Reading and sorting: %s'%sInTbl)
		while len(sLine) > 0:
			sLine = fInTbl.readline()
			nInLine += 1L
			
			if nInLine < 2L: #Ignore the first line
				continue
		
			# Buffer internally for sorting
			if len(sLine.strip()) > 0:
				lLines.append(sLine)
		
		lLines.sort()
		for sLine in lLines:
			fOutTbl.write(sLine)
		
		# Now get the label information
		sTmp = pjoin(sTpltVol, 'INDEX', 'index.%04i.lbl'%iVol)
		sInLbl = rpw_pds.util.getVerFile(sTmp, bLowestVer, True)
		
		dCIdxLbl['num-labels'] += len(lLines)
		dCIdxLbl['index-rows'] += len(lLines)
	
		dLbl = rpw_pds.parse.extractFromRoot(sInLbl, lExtract)
		
		lDatasets = rpw_pds.util.listMerge(lDatasets, dLbl['DATA_SET_ID'])	
		lTargs = rpw_pds.util.listMerge(lTargs, dLbl['TARGET_NAME'])
		lPhases = rpw_pds.util.listMerge(lPhases, dLbl['MISSION_PHASE_NAME'])
		
		#Update earliest start and latest stop
		if dCIdxLbl['start-time'] == None:
			dCIdxLbl['start-time'] = dLbl['START_TIME']
		else:
			if dLbl['START_TIME'] < dCIdxLbl['start-time']:
				dCIdxLbl['start-time'] = dLbl['START_TIME']
		
		if dCIdxLbl['stop-time'] == None:
			dCIdxLbl['stop-time'] = dLbl['STOP_TIME']
		else:
			if dLbl['STOP_TIME'] > dCIdxLbl['stop-time']:
				dCIdxLbl['stop-time'] = dLbl['STOP_TIME']
	
	
	#Close out the tbl file and write the label file
	fOutTbl.close()
	nSize += os.path.getsize(sPathOutTable)
	
	lDatasets.sort()
	lTargs.sort()
	lPhases.sort()
	
	dCIdxLbl['datasets'] = rpw_pds.util.pdsList(lDatasets, 26)
	dCIdxLbl['targets'] =  rpw_pds.util.pdsList(lTargs, 26)
	dCIdxLbl['phases'] =   rpw_pds.util.pdsList(lPhases, 26)
	
	nSize += rpw_pds.util.makeFromTemplate(pjoin(sTpltVol, 'INDEX'),
	                               pjoin(sStageDir, 'INDEX'),
	                               dCIdxLbl, suffix='.r3')					 
	return nSize

############################################################################
def _lnSubStrMatch(sSrcDir, sDestDir, sSubStr, lCrossCh = []):
	nSize = 0L
	
	if not (len(lCrossCh) == 0 or len(lCrossCh) == 2):
		raise ValueError('_lnSubStrMatch: expected lCrossCh to be length 0 or 2')
	
	for sFile in os.listdir(sSrcDir):
	
		sSrcPath = pjoin(sSrcDir, sFile)
		sDestPath = pjoin(sDestDir, sFile)
	
		if os.path.isfile(sSrcPath) and sFile.find(sSubStr) > -1:
		
			if lCrossCh != []:
				if sFile.endswith(lCrossCh[0]):
					checkAssociatedFileExists(sSrcPath, [lCrossCh[1]])
				elif sFile.endswith(lCrossCh[1]):
					checkAssociatedFileExists(sSrcPath, [lCrossCh[0]])
				else:
					raise ValueError('%s: does not end in one of %s'%(sSrcPath, lCrossCh))			
				
			nSize += rpw_pds.util.symLinkCkSize(sSrcPath, sDestPath)
			
	return nSize
					

def lnBrowseMakeIndex(sSuperVol, sStageDir, lDates, pubDate):
	"""Handle the BROWSE directory.
	
	   Files generated by parsing similar file in sSuperVol 
	      BROWSE/BROWSE.HTM 
		  
		   BROWSE/RPWS_KEY_PARAMETERS/TYYYYDXX/BROWSE.HTM
	      BROWSE/RPWS_LOW_RATE_FULL/TYYYYDXX/BROWSE.HTM
			
		   BROWSE/RPWS_WAVEFORM_FULL/TYYYYDXX/BROWSE.LBL 
	      BROWSE/RPWS_WAVEFORM_FULL/TYYYYDXX/BROWSE_25HZ_MA.HTM
	      BROWSE/RPWS_WAVEFORM_FULL/TYYYYDXX/BROWSE_2_5KHZ_MA.HTM
			
			BROWSE/RPWS_WIDEBAND_FULL/TYYYYDXX/BROWSE.LBL
			BROWSE/RPWS_WIDEBAND_FULL/TYYYYDXX/BROWSE_10KHZ_MA.HTM
			BROWSE/RPWS_WIDEBAND_FULL/TYYYYDXX/BROWSE_75KHZ_MA.HTM
	"""
	log = logging.getLogger('lnBrowseMakeIndex')
	
	DAY_ORG_BY_DIR = 1
	DAY_ORG_BY_FILE = 2
	dSet = {'RPWS_KEY_PARAMETERS':(cas_rewrite.keyParamBrowseReWriter, DAY_ORG_BY_FILE),
	        'RPWS_LOW_RATE_FULL':(cas_rewrite.lowRateBrowseReWriter, DAY_ORG_BY_FILE),
			  'RPWS_WAVEFORM_FULL':(cas_rewrite.waveFormBrowseReWriter, DAY_ORG_BY_DIR),
	        'RPWS_WIDEBAND_FULL':(cas_rewrite.wideBandBrowseReWriter, DAY_ORG_BY_DIR)}
			  
	dMstrBrowseLnks = {'RPWS_KEY_PARAMETERS':[], 'RPWS_LOW_RATE_FULL':[],
			             'RPWS_WAVEFORM_FULL': [], 'RPWS_WIDEBAND_FULL':[]}
	
	nSize = 0L
	for sSub in dSet.keys():
		sSrcDir = pjoin(sSuperVol, 'BROWSE',sSub)
		sDestDir = pjoin(sStageDir, 'BROWSE',sSub)
		log.info('Linking: %s'%sDestDir)
		dDest100Dates = {}
		
		for date in lDates:
			sDest100Dir = pjoin(sDestDir, _get100dir(date))
			sSrc100Dir = pjoin(sSrcDir, _get100dir(date))
			
			# For each Hundred day directory, generate a list of dates that are
			# in that hundred day dir, we'll feed this list to a rewriter
			if sDest100Dir not in dDest100Dates.keys():
				dDest100Dates[sDest100Dir] = []
			dDest100Dates[sDest100Dir].append(deepcopy(date))
			
			if not os.path.isdir(sDest100Dir):
				os.makedirs(sDest100Dir)
			
			if dSet[sSub][1] == DAY_ORG_BY_DIR:
				#One dir per day sets, RPWS_WAVEFORM_FULL, RPWS_WIDEBAND_FULL
				sDayLnk = pjoin(sDest100Dir, "T%s"%date)
				
				if not os.path.islink(sDayLnk):
					#Check to see if corresponding source dir exists first
					sSrcDayDir = pjoin(sSrcDir, _get100dir(date), "T%s"%date)
					if os.path.isdir(sSrcDayDir):
						os.symlink(sSrcDayDir, sDayLnk)
						(nTmp, nFiles) = rpw_pds.util.getDirSizeLk(sDayLnk)
						nSize += nTmp
					
			else: 
				#One dir per 100day sets, RPWS_KEY_PARAMETERS, RPWS_LOW_RATE_FULL
				sDay = "T%s"%date
				lCrossCh = []
				if sSub == 'RPWS_LOW_RATE_FULL':
					lCrossCh = ['.PNG','.LBL']
				nSize += _lnSubStrMatch(sSrc100Dir, sDest100Dir, sDay, lCrossCh)
				nSize += _lnSubStrMatch(pjoin(sSrc100Dir, 'HTML'),
				                        pjoin(sDest100Dir, 'HTML'), sDay)
		
					
		
		# Now do the rewriting of the browse html lists, using classes stored
		# in the dataset dictionary (recycles sDest100Dir, and sSrc100Dir from
		# above, does not depend on them).
		
		for sDest100Dir in dDest100Dates.keys():
			sSrc100Dir = sDest100Dir.replace(sDestDir, sSrcDir, 1)
			
			reWriter = dSet[sSub][0](dDest100Dates[sDest100Dir], sSrc100Dir, sDest100Dir)
			(nTmp, lLinks) = reWriter.parse(pubDate, pjoin(sStageDir, 'BROWSE'))
			dMstrBrowseLnks[sSub] += lLinks
			#print "Link Txt: ", lLinks
			nSize += nTmp
	
	#Using the supplied sublinks, make a master browse file.
	nSize += cas_rewrite.mkMasterBrowse(sStageDir, dMstrBrowseLnks, lDates)
	return nSize


############################################################################
def copyUpdateErrata(sTpltVol, sStageDir, doyPub, nVol, nVolVer, sRelNotes, sRelFile):
	"""Copy Errata file to stage dir, update template volume with a new
	ERRATA template.
	
	Returns:  (nSize errata file on stage, new errata template name)
	"""
	
	log = logging.getLogger('copyUpdateErrata')
	log.info('Adding release notes to ERRATA.TXT')
	
	# Get release notes as an after-the-1st-line indented string.
	sNotes = ''
	if sRelFile != '':
		sTmp = file(sRelFile, 'rb').read()
		sNotes = rpw_pds.util.spaceFormat(sTmp, 9, 70)
	else:
		sNotes = rpw_pds.util.spaceFormat(sRelNotes, 9, 70)
	
	# Sub in top-pub-date, vol-int, pub-date, sRelNotes -> stage dir
	sOrigEtaTplt = rpw_pds.util.getVerFile(pjoin(sTpltVol, 'ERRATA.TXT.r4'))
	
	if os.path.getsize(sOrigEtaTplt) < 1L:
		sErr = "%s is a 0 length file, delete it!"%sOrigEtaTplt
		log.error(sErr)
		raise RuntimeError(sErr)
	
	fEtaOrig = file(sOrigEtaTplt,'rb')
	sFmt = fEtaOrig.read()
	fEtaOrig.close()
	
	dRep = {'top-pub-date':doyPub.getDOMstr(), 'vol-int':nVol,
	        'version':"%d"%nVolVer, 'pub-date':doyPub.getDOMstr(),
			  'release-notes': sNotes}
	
	fEtaStg = file(pjoin(sStageDir, 'ERRATA.TXT'), 'wb')
	fEtaStg.write(sFmt%dRep)
	fEtaStg.close()
	
	# Sub in vol-int, pub-date; Append suffix template -> template dir
	dRep['top-pub-date'] = '%(top-pub-date)s' #Put top formatter back in.
	
	sNewEtaTplt = rpw_pds.util.mkVerFileName(pjoin(sTpltVol,'ERRATA.TXT.r4'))
	fEtaTplt = file(sNewEtaTplt,'wb')
	fEtaTplt.write(sFmt%dRep)
	fEtaTplt.write("   %(vol-int)03i  %(pub-date)s  Version %(version)s\r\n")
	fEtaTplt.write("%(release-notes)s\r\n")
	fEtaTplt.close()
	
	return (os.path.getsize(sNewEtaTplt), sNewEtaTplt)


##############################################################################
def scetToSclk(sScet):
	"""parse SCET return SCET, general version takes strings.
		NOTE: Replace this with direct spice call version.
	"""
	
	log = logging.getLogger('scetToSclk')
	
	et = pspice.utc2et(sScet)
	sSclk = pspice.sce2s(-82, et)	
	
	return sSclk
	

##############################################################################
def _getSclkSecFromScet(doy):
	"""parse SCLK return SCET"""
	
	log = logging.getLogger('_getScetFromSclk')
	
	et = pspice.utc2et(doy.getDOMstr())
	sSclk = pspice.sce2s(-82, et)
		
	lOut = sSclk.split('.') 
	if len(lOut[0]) > 12:
		log.error('Unexpectedly long output before the first "." from %s: %s'%(
		          sCasSclk, sOut))
	
	return lOut[0]

def printLblTxt(sStageDir, doyPub, doyStart, doyTo, nVol):
	"""Make a cut-n-paste lable text file for this volume
				
		sStageDir - stage directory, basename of this is used to make name
		   of the text file
			
		doyPub - publication date object
		doyStart - data start day
		doyTo - *exclusive* stop day
	"""
	

	fOut = file("%s_dvd_print.txt"%sStageDir, 'wb')
	fOut.write('Title\r\n')
	fOut.write('-----\r\n')
	fOut.write('USA_NASA_PDS_CORPWS_%04d\r\n\r\n'%nVol)
	
	fOut.write('Dates (with Exclusive end point)\r\n')
	fOut.write('--------------------------------\r\n')
	fOut.write('%s to %s\r\n\r\n'%(doyStart.getDOMpDOYstr(), doyTo.getDOMpDOYstr()))
	
	sStart = _getSclkSecFromScet(doyStart)
	sExclusiveEnd = _getSclkSecFromScet(doyTo)
		
	fOut.write('SCLK Text (bottom)\r\n')
	fOut.write('------------------\r\n')
	fOut.write('%s to %s\r\n'%(sStart, sExclusiveEnd))
	fOut.write('Volume %03d:\r\n'%nVol)
	fOut.write('Cassini Radio and Plasma Wave\r\nStandard Products\r\n')
	fOut.write('%s\r\n'%doyPub.getDOMstr())
	
	fOut.close()
