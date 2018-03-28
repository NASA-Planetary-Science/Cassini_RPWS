"""Cassini HTML re-writers, used when generating output pds volumes from
   the super volume """

#System lib imports
import os
import os.path

from os.path import join as pjoin
from copy import deepcopy

import re
import logging
#import shutil


#Local lib imports
import rpw_pds.parse
import rpw_pds.util

##############################################################################
# Re-write the sequence_info/index file with different background highlighting.

class seqInfoRewrite:
	"""Alters background highlighting of table rows based on sequence info
	date range.  Used to create   EXTRAS/SEQUENCE_INFO/INDEX.HTM
	from the same file in the super volume.
	"""
	def __init__(self, sSuperDir, lDates, sOutDir):
		self.InclusiveStart = deepcopy(lDates[0])
		self.ExclusiveStop = deepcopy(lDates[-1])
		self.ExclusiveStop.incDay()
		self.nSeqInDateRange = 0
		self.sIn = pjoin(sSuperDir, 'EXTRAS','SEQUENCE_INFO','INDEX.HTM')
		self.sOut = pjoin(sOutDir, 'EXTRAS','SEQUENCE_INFO','INDEX.HTM')

	def _rowStartStop(self, sTxt):
		"""Produce start and stop DOY objects given Terry Avercamp's
		   SEQUENCE_INFO/INDEX.HTM row definitions.
		"""
		#Example of matching string is: 25-Jan-1999 (Day 025)
		# Ah, great, Jessica Removed the space, I hate parsing HTML, another of
		# Terry's great ideas.
		reg = re.compile(r'[0-9]{2}-[A-Za-z]{3}-[12][09][0-9]{2}\s*\([Dd][Aa][Yy]\s[0-9]{3}\)')
		lTmp = reg.findall(sTxt)
		#print lTmp
		if len(lTmp) < 2:
			return (False, None, None)
		
		sStart = lTmp[0]
		sEnd = lTmp[1]
		doySeqStart = rpw_pds.util.DOYdate(int(sStart[7:11]), int(sStart[-4:-1]))
		doySeqEnd = rpw_pds.util.DOYdate(int(sEnd[7:11]), int(sEnd[-4:-1]))
		
		#print doySeqStart, doySeqEnd
		return (True, doySeqStart, doySeqEnd)
		
		
	def tableDefOut(self, sIn, sLookAhead):
		""" Intecept table start tag and change BGCOLOR to #999999 """
		if sIn.find('#FFFFFF') > -1:
			sOut = sIn.replace('#FFFFFF','#999999',1)
		else:
			raise ValueError('Expected input table definition to include color #FFFFFF')
			
		return sOut

	
	def rowDefOut(self, sIn, sLookAhead):
		"""Intecept TR tags and break into tuple (Sequence, Start, Stop)
	      - If start or stop of sequence is within our time range
	        output the TR tag with a BGCOLOR="#FFFFFF#
		"""
		(bHasDate, sS, sE) = self._rowStartStop(sIn)
		if not bHasDate:
			return sIn
		
		log = logging.getLogger('seqInfoRewrite')
		log.debug("Found seq. date: %s to %s"%(sS, sE))
	
		bSeqInRange = False
		if sS >= self.InclusiveStart and sS < self.ExclusiveStop:
			# Starts Within range
			bSeqInRange = True
		elif sE >= self.InclusiveStart and sE < self.ExclusiveStop:
			# Ends within range
			bSeqInRange = True
		elif sS < self.InclusiveStart and sE >= self.ExclusiveStop:
			# Starts before and ends after
			bSeqInRange = True
			
			
		if bSeqInRange:
			sOut = '<TR BGCOLOR="#FFFFFF"%s'%sIn[3:]
			self.nSeqInDateRange += 1
			return sOut
		else:
			return sIn
			
	def getMatch(self):
		"""Produce a match list of start and end tags suitable for
		sending to genericTextProc in the parse module.
		"""
		return [ ('<TABLE', '>',      self.tableDefOut),
	            ('<TR',    '</TR>',  self.rowDefOut,    300)   ]

	def parse(self):
		"""Parse the input to the output, and return nBytes of output"""
		fIn = file(self.sIn, 'rb')
		fOut = file(self.sOut, 'wb')
		
		log = logging.getLogger('seqInfoRewrite')
		
		log.info("Parsing: '%s'"%self.sIn)
		log.debug("            --to-> '%s'"%self.sOut)
		
		(lFound, nSize) = rpw_pds.parse.genericTextProc(fIn, fOut, self.getMatch())
	
		#check to see if any sequences were in the date range:
		if self.nSeqInDateRange < 1:
			sTmp = "Could not find any sequences in date range %s to %s."%(
			       self.InclusiveStart, self.ExclusiveStop)
			log.error(sTmp)
			raise ValueError(sTmp)
	
		return nSize


##############################################################################
# Rewriters for WAVEFORM browse html files: BROWSE_2_5KHZ_MA and BROWSE_25HZ_MA

class browseReWritter(object):
	"""Base class for waveForm and wideBand rewritters.
	
	The waveform and wideband browse files are very similar so the similarities
	in processing them have been grouped together in this class to avoid 
	'cut-in-paste' coding.
	"""
	def __init__(self, lDates, sSrcDir, sDestDir):
		"""This is a virtual class.  Derived classes must define sSrcName1 and
		sSrcName2
		"""
		object.__init__(self)
		self.log = logging.getLogger(self.__class__.__name__)
		self.lSrc = [pjoin(sSrcDir, self.sSrcName1), 
		             pjoin(sSrcDir, self.sSrcName2)]
		
		for sSrc in self.lSrc:
			if not os.path.isfile(sSrc):
				sTmp = "Input file %s is missing."%sSrc
				self.log.error(sTmp)
				raise RuntimeError(sTmp)

		self.lDest = [pjoin(sDestDir, self.sSrcName1),
		              pjoin(sDestDir, self.sSrcName2)]

		self.sDestDir = sDestDir
		self.InclusiveStart = deepcopy(lDates[0])
		self.ExclusiveStop = deepcopy(lDates[-1])
		self.ExclusiveStop.incDay()
		self.nDaysFound = 0
		
		self.sThroughStr = ""
		if lDates[0] == lDates[-1]:
			self.sThroughStr = '%s'%lDates[0].getDOMpDOYstr()
		else:
			self.sThroughStr = '%s through %s'%(
		       	   lDates[0].getDOMpDOYstr(), lDates[-1].getDOMpDOYstr())
						

	def writeLabel(self, lLabelRef, doyPub):
		"""Given a list of html files, write a PDS label for them.
		Assumes that the html files are in the same directory.
		
		Label is writen to dir self.sDestDir
		"""
		
		if len(lLabelRef) < 1:
			raise RuntimeError('Are there really no browse files?')
		lPrnRef = []
		for sFile in lLabelRef:
			lPrnRef.append('"%s"'%os.path.basename(sFile) )
		
		sLbl = """PDS_VERSION_ID          = PDS3\r
RECORD_TYPE             = STREAM\r
^HTML_DOCUMENT          = %s\r
\r
OBJECT                  = HTML_DOCUMENT\r
  DOCUMENT_NAME           = "GRAPHICAL DATA BROWSER"\r
  DOCUMENT_TOPIC_TYPE     = "HTML NAVIGATION"\r
  PUBLICATION_DATE        = %s\r
  INTERCHANGE_FORMAT      = ASCII\r
  DOCUMENT_FORMAT         = HTML\r
  DESCRIPTION             = "The referenced documents provide an access point\r
for HTML and web-browser-specific content."\r
END_OBJECT              = HTML_DOCUMENT\r
END\r
"""%(rpw_pds.util.pdsList(lPrnRef, 26), doyPub.getDOMstr())
		
		fLbl = file(pjoin(self.sDestDir, 'BROWSE.LBL'), 'wb')
		fLbl.write(sLbl)
		fLbl.close()
		
		return os.path.getsize(pjoin(self.sDestDir, 'BROWSE.LBL'))
	
	
	def _mkMstrBrowseLinkList(self, sRelTo):
		"""Check to see if src browse items are present, use info to generate
		master browse list of links, or empty list if none present.
		
		sRelTo - A dir that the links should be generated relative to.
		"""
		
		if not os.path.isfile(self.lDest[0]) and not os.path.isfile(self.lDest[1]):
			return []
		
		sTxt = "%s&nbsp;&nbsp;\r\n"%self.sThroughStr
		
		if os.path.isfile(self.lDest[0]):
			sLkPath = rpw_pds.util.relPath(sRelTo, self.lDest[0], '/')
			sTxt += '<A href="%s">%s</A>&nbsp;&nbsp;\r\n'%(sLkPath, self.sSrcLnkTxt1)
		else:
			sTxt += '%s&nbsp;&nbsp;\r\n'%self.sSrcLnkTxt1
		
		if os.path.isfile(self.lDest[1]):
			sLkPath = rpw_pds.util.relPath(sRelTo, self.lDest[1], '/')
			sTxt += '<A href="%s">%s</A>'%(sLkPath, self.sSrcLnkTxt2)
		else:
			sTxt += self.sSrcLnkTxt2
		
		return [sTxt]

##############################################################################

class waveFormBrowseReWriter(browseReWritter):
	"""ReWriter for Cassini RPWS super volume files:
	
		BROWSE/RPWS_WAVEFORM_FULL/TYYYYDXX/BROWSE_25HZ_MA.HTM and
		BROWSE/RPWS_WAVEFORM_FULL/TYYYYDXX/BROWSE_2_5KHZ_MA.HTM
		
		For each call to parse() up to two files are written, based on
		the inputs and the date range.
	"""
	def __init__(self, lDates, sSrcDir, sDestDir):
		self.sSrcName1 = 'BROWSE_25HZ_MA.HTM'
		self.sSrcLnkTxt1 = '25Hz'
		self.sSrcName2 = 'BROWSE_2_5KHZ_MA.HTM'
		self.sSrcLnkTxt2 = '2.5kHz'
		browseReWritter.__init__(self, lDates, sSrcDir, sDestDir)
		self.regField = re.compile(r'[0-9]{4}-[0-9]{2}-[0-9]{2}\([0-9]{3}\)T[0-9]{2}')
				 
		
	def _fieldOut(self, sIn, sLookAhead):
		 
		lTmp = self.regField.findall(sIn)
		
		if len(lTmp) != 1:
			sTmp = "%s: Couldn't find unique date pattern in table field,"%self.sCurFile
			sTmp +=" text block was:\n%s"%sIn
			raise ValueError(sTmp)
		
		doyTmp = rpw_pds.util.DOYdate(int(lTmp[0][:4]), int(lTmp[0][11:14]))
		if doyTmp >= self.InclusiveStart and doyTmp < self.ExclusiveStop:
			self.nDaysFound += 1
			return sIn
		else:
			return ""
	
	
	def _rowOut(self, sIn, sAhead):
		"""Remove empty rows"""
		if sAhead.find('<TR ALIGN="CENTER">') > -1:
			return ""
		if sAhead.find('</TABLE>') > -1:
			return ""
		else:
			return sIn
			
	
	def parse(self, doyPub, sLnkTo):
		nSize = 0L
		lLabelRef = [] #List of files that the label should reference.
		
		for i in range(0, len(self.lSrc) ):
			fIn = file(self.lSrc[i], 'rb')
			
			if not os.path.isdir( os.path.dirname(self.lDest[i]) ):
				os.makedirs( os.path.dirname(self.lDest[i]) )
			sTmpOut = "%s.tmp"%self.lDest[i]
			fOut = file(sTmpOut,  'wb')
			
			
			lMatch = [('  <!-- Plot Start:','\n  <BR>\n', self._fieldOut )]
			
			self.log.info("Parsing: '%s'"%self.lSrc[i])
			self.log.debug("            --to-> '%s'"%sTmpOut)
			
			self.sCurFile = self.lSrc[i]
			self.nDaysFound = 0
			(lFound, nSizeOut) = rpw_pds.parse.genericTextProc(fIn, fOut, lMatch)
			fIn.close()
			fOut.close()
			
			
			if self.nDaysFound > 0:
				lLabelRef.append(self.lSrc[i])
			else:
				os.remove(sTmpOut)
				continue
			
			
			#Now do round two, empty row removal
			fIn = file(sTmpOut, 'rb')
			fOut = file(self.lDest[i], 'wb')
			
			lMatch = [('<TR ALIGN="CENTER">\n','',self._rowOut)]
			
			self.log.info("Parsing: '%s'"%sTmpOut)
			self.log.debug("            --to-> '%s'"%self.lDest[i])
			
			
			(lFound, nSizeOut) = rpw_pds.parse.genericTextProc(fIn, fOut, lMatch, 19)
			fIn.close()
			fOut.close()
			
			os.remove(sTmpOut)
			nSize += nSizeOut
				
		#Now generate the label
		nSize += self.writeLabel(lLabelRef, doyPub)
		return (nSize, self._mkMstrBrowseLinkList(sLnkTo))
		
##############################################################################

class wideBandBrowseReWriter(browseReWritter):
	"""ReWriter for Cassini RPWS super volume files:
	
		BROWSE/RPWS_WIDEBAND_FULL/TYYYYDXX/BROWSE_10KHZ_MA.HTM and
		BROWSE/RPWS_WIDEBAND_FULL/TYYYYDXX/BROWSE_75KHZ_MA.HTM
		
		For each call to parse() up to two files are written, based on
		the inputs and the date range.
	"""
	def __init__(self, lDates, sSrcDir, sDestDir):
		self.sSrcName1 = 'BROWSE_10KHZ_MA.HTM'
		self.sSrcLnkTxt1 = '10kHz'
		self.sSrcName2 = 'BROWSE_75KHZ_MA.HTM'
		self.sSrcLnkTxt2 = '75kHz / HF WBR'
		browseReWritter.__init__(self, lDates, sSrcDir, sDestDir)
		self.regItem = re.compile(r'[0-9]{4}-[0-9]{2}-[0-9]{2} \([0-9]{3}\)')
	
	def _itemOut(self, sIn, sLookAhead):
		lTmp = self.regItem.findall(sIn)
		
		if len(lTmp) != 1:
			sTmp = "%s: Couldn't find unique date pattern in list item,"%self.sCurFile
			sTmp +=" text block was:\n%s"%sIn
			raise ValueError(sTmp)
			
		doyTmp = rpw_pds.util.DOYdate(int(lTmp[0][:4]), int(lTmp[0][12:15]))
		if doyTmp >= self.InclusiveStart and doyTmp < self.ExclusiveStop:
			self.nDaysFound += 1
			return sIn
		else:
			return ""
	
	
	def parse(self, doyPub, sLnkTo):
		"""
		"""
		nSize = 0L
		lLabelRef = [] #List of files that the label should reference.
		
		for i in range(0, len(self.lSrc) ):
			fIn = file(self.lSrc[i], 'rb')
			
			if not os.path.isdir( os.path.dirname(self.lDest[i]) ):
				os.makedirs( os.path.dirname(self.lDest[i]) )
			fOut = file(self.lDest[i],  'wb')
		
			lMatch = [('  <LI>','</a>\n', self._itemOut)]
							  
			self.log.info("Parsing: '%s'"%self.lSrc[i])
			self.log.debug("                  --to-> '%s'"%self.lDest[i])
			
			self.sCurFile = self.lSrc[i]
			self.nDaysFound = 0
			(lFound, nSizeOut) = rpw_pds.parse.genericTextProc(fIn, fOut, lMatch)
			fIn.close()
			fOut.close()
			
			
			if self.nDaysFound > 0:
				lLabelRef.append(self.lSrc[i])
				nSize += nSizeOut
			else:
				self.log.warning("%s: EMPTY WIDEBAND BROWSE LIST CHECK FILE and (maybe) fix hash"%self.lSrc[i])
		
		#Okay, make a lable
		nSize += self.writeLabel(lLabelRef, doyPub)
		return (nSize, self._mkMstrBrowseLinkList(sLnkTo))
	
##############################################################################
class KP_LRbaseReWriter(object):
	"""Base for common functionality for Key Parameter BROWSE index handling
	and Low Rate BROWSE index handling"""
	
	########################################################################
	def __init__(self, lDates, sSrcDir, sDestDir):
		object.__init__(self)
		self.sSrcDir = sSrcDir
		self.sDestDir = sDestDir
		self.lDates = lDates
		self.log = logging.getLogger('KP_LRbaseReWriter')
		self.reDateLine = re.compile(r'PUBLICATION_DATE\s*=.*')
		self.reDate = re.compile(r'[0-9]{4}-[0-9]{2}-[0-9]{2}')
		
		self.sThroughStr = ""
		if lDates[0] == lDates[-1]:
			self.sThroughStr = '%s'%lDates[0].getDOMpDOYstr()
		else:
			self.sThroughStr = '%s through %s'%(
		       	   lDates[0].getDOMpDOYstr(), lDates[-1].getDOMpDOYstr())
	
	########################################################################
	def _getPubDate(self, sSuperLbl):
		fIn = file(sSuperLbl, 'rb')
		s = fIn.read()
		fIn.close()
		lTmp = self.reDateLine.findall(s)
		if len(lTmp) != 1:
			raise RuntimeError("%s: Couldn't find unique PUBLICATION_DATE"%sSuperLbl)
		
		lSub = self.reDate.findall(lTmp[0])
		if len(lSub) != 1:
			raise RuntimeError("%s: Couldn't date formatted as YYYY-MM-DD."%sSuperLbl)
		
		return lSub[0]
	
	########################################################################
	def _genPairedMultiFileLbl(self, sHdr, lFileTups, sFtr, sOutFile):
		fOut = file(sOutFile, 'wb')
		fOut.write(sHdr)
		
		# Get spacing for list items based off length of last line of sHdr
		sSp = ' '*(len(sHdr) - sHdr.rfind('\n')) 

		fOut.write('{')
		
		iLast = len(lFileTups) - 1
		for i in range(0,iLast + 1):
			if i != 0:
				fOut.write(sSp)
				
			fOut.write('"%s","%s"'%(lFileTups[i][0],lFileTups[i][1]))
			
			if i != iLast:
				fOut.write(",\r\n")
			
		
		fOut.write('}\r\n')
		fOut.write(sFtr)
		fOut.close()
		
		return os.path.getsize(sOutFile)
		
	########################################################################	
	def _getPDSFilePairs(self, sPairDir, sFmt1, sFmt2):
		lPairs = []
		
		for date in self.lDates:
			lPaths = [pjoin(sPairDir, sFmt1%date), pjoin(sPairDir, sFmt2%date)]
			lFound = [False, False]
			# If don't find both, just issue warning, if don't find only one
			# then it's error time.
			
			for i in range(0,2):
				if	os.path.isfile( lPaths[i] ):
					lFound[i] = True
					
			if lFound == [True, True]:
				lPairs.append( (os.path.basename(lPaths[0]), os.path.basename(lPaths[1])) )
			elif lFound == [False, False]:
				self.log.warning("in BROWSE tree: Couldn't find files named %s"%lPaths)
			else:
				raise RuntimeError("in BROWSE tree: one of %s is missing."%lPaths)
			
		return lPairs

	########################################################################
	def _checkFound(self, lFound, sSetDir, date):
		nFalse = 0
		nTrue = 0
		nSize = len(lFound)
		
		for bTmp in lFound:
			if bTmp:
				nTrue += 1
			else:
				nFalse += 1
				
		if nTrue == nSize:
			return nSize
		elif nFalse == nSize:
			self.log.warning('No BROWSE files found in %s for day %s'%(sSetDir, date))
			return 0
		else:
			raise RuntimeError('Partial set of BROWSE files found in %s for day %s'%(
			                   sSetDir, date))
		return None
	
	
	def _genBOWSE(self, sSetDir, lFmtTup, sTitle, sOutFile, nExpected=-1):
		"""Look in dir for files with given name patterns and generate 
		   a browse file.  
			
			sSetDir  - Directory to find file sets with formats in tFmt
			
			lFmtTup  - A list of 3-tuples 1st is link file fmt, 2nd is link
			           image fmt, 3rd is the alternate image text.  The
			           formats must have a %s in them where the DOY date
			           will be inserted.
						  
			sTitle   - title of the page
			
			sOutfile - name of output html, will be generated in self.sDest
			
			nExpected - the number of files expected, if -1 the found file
			           count is ignored, if not failure to find the number
			           of files expected throws an RuntimeError
			
			Also uses self.lDates behind the scenes
			
			Returns size of file generated.  NOTE: if there were no files
			matching any of the dates and formats then return will be 0L
			
			Output file:  Is a HTML 3.2 complient file with at most 4 columns.
			in the main table.  The output rows will be grouped so that
			data on the same day but with different formats appear together.
		"""
		
		fOut = file(pjoin(self.sDestDir, sOutFile), 'wb')
		self.log.info('Writing: %s'%pjoin(self.sDestDir, sOutFile))
		
		#Header
		fOut.write("""<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">\r
<HTML>\r
<HEAD>\r
<META HTTP-EQUIV="Content-Type" CONTENT="text/html; charset=ISO-8859-1">\r
<TITLE>%s</TITLE>\r
</HEAD>\r
<BODY>\r
<TABLE>\r
"""%sTitle)
		
		# Get the relative link path to sSetDir from self.sDest, make work on
		# windows and posix (can't use os.path.dirname function).
		#
		
		# HACK start --->
		# Technically this is a HACK, there is no reason the set directory has
		# to be named HTML and be under the dest dir, but oh well.
		
		#sLkPath = rpw_pds.util.relPath(self.sDestDir, pjoin(sSetDir, 'abcde'), '/')
		
		#if sLkPath.find('/') == -1:
		#	sLkPath = ""
		#else:
		#	sLkPath = sLkPath[:-5] #Keep the /
		#print "sLkPath =", sLkPath
		
		sLkPath = "HTML/"
		# <---- HACK end
		
		nSets = len(lFmtTup)
		
		lRowTxt = []
		for i in range(0, nSets):
			lRowTxt.append('<TR ALIGN="CENTER">\r\n')
		
		nCol = 0
		nFiles = 0
		for date in self.lDates:
			nCol += 1
			
			#Must find all or none, none is warning, some is a throw
			lCurFound = []
			for i in range(0,nSets):
				lCurFound.append( os.path.isfile(pjoin(sSetDir, lFmtTup[i][0]%date)) )
				lCurFound.append( os.path.isfile(pjoin(sSetDir, lFmtTup[i][1]%date)) )
			nFilesForDay = self._checkFound(lCurFound, sSetDir, date)
			nFiles += nFilesForDay
			
			#So, if we have any files, add a link string.
			if nFilesForDay > 0:		
			
				for i in range(0, nSets):
					sLkFile = lFmtTup[i][0]%date
					sImgFile = lFmtTup[i][1]%date
					sName = sLkFile
					iDot = sLkFile.rfind('.')
					if iDot > -1:
						sName = sLkFile[:iDot]
				
					lRowTxt[i] += """<TD><A HREF="%s" NAME="%s">\r
  <IMG SRC="%s" HEIGHT=150 WIDTH=200\r
   ALT="%s"><BR>\r
  %s</A>\r
  """%(sLkPath+sLkFile, sName, sLkPath+sImgFile, lFmtTup[i][2], date.getDOMstr())
			
			
			if nCol == 4:  # Time to flush the sets, start new rows.
				for i in range(0,nSets):
					fOut.write(lRowTxt[i])
					lRowTxt[i] = '<TR ALIGN="CENTER">\r\n'
				fOut.write('<TR><TD COLSPAN=4>&nbsp</TD>\r\n')
				fOut.write('<TR><TD COLSPAN=4><HR></TD>\r\n')
				fOut.write('<TR><TD COLSPAN=4>&nbsp</TD>\r\n')
				nCol = 0
			
		
		#Flush any remaining half rows:
		if nCol != 0:
			for i in range(0,nSets):
				fOut.write(lRowTxt[i])
				for j in range(0, nSets - nCol):#Pad out table with empty fields
					fOut.write('<TD>\r\n')
			
			fOut.write('<TR><TD COLSPAN=4>&nbsp</TD>\r\n')
			fOut.write('<TR><TD COLSPAN=4><HR></TD>\r\n')
			fOut.write('<TR><TD COLSPAN=4>&nbsp</TD>\r\n')
		
		#Footer
		fOut.write("""</TABLE>\r
<p>\r
<A HREF="http://validator.w3.org/check/referer">\r
<IMG BORDER="0" SRC="../../ANCILLARY/VALID_HTML32.PNG"\r
 ALT="Valid HTML 3.2!" HEIGHT="31" WIDTH="88"></A>\r
</p>\r
</BODY>\r
</HTML>\r
""");
		fOut.close()
		
		#If file found are 0, then just nuke it.
		if nFiles != nExpected:
			os.remove(pjoin(self.sDest, sOutfile))
			raise RuntimeError("Expected to reference %i files in %s but only found %i"%(
			                   nExpected, pjoin(self.sDestDir, sOutFile), nFiles))
		
		if nFiles == 0:
			os.remove(pjoin(self.sDestDir, sOutFile))
			return 0L
		else:
			return os.path.getsize(pjoin(self.sDestDir, sOutFile))

##############################################################################
class lowRateBrowseReWriter(KP_LRbaseReWriter):
	"""Outputs:
	
	NOTE: Fill in all data links *before* calling this class's parse function.
	
		BROWSE/RPWS_LOW_RATE_FULL/TYYYYDXX/BROWSE.HTM  <-- parsed
	   BROWSE/RPWS_LOW_RATE_FULL/TYYYYDXX/BROWSE.LBL  <-- symlink
		BROWSE/RPWS_LOW_RATE_FULL/TYYYYDXX/HTML/LRB_HTML.LBL  <-- pts to many
	   BROWSE/RPWS_LOW_RATE_FULL/TYYYYDXX/HTML/LRB_PNG.LBL   <-- pts to many
	"""
	
	########################################################################
	def __init__(self, lDates, sSrcDir, sDestDir):
		KP_LRbaseReWriter.__init__(self, lDates, sSrcDir, sDestDir)
	
	########################################################################
	def _genLRB_HTML_LBL(self):
		sPairDir = pjoin(self.sDestDir, 'HTML')
		sOutLbl = pjoin(sPairDir, 'LRB_HTML.LBL')
		sSuperLbl = pjoin(self.sSrcDir, 'HTML', 'LRB_HTML.LBL')
		
		self.log.info("Writing: '%s'"%sOutLbl)
		
		sHdr = """PDS_VERSION_ID          = PDS3\r
RECORD_TYPE             = STREAM\r
^HTML_DOCUMENT          = """
		
		sPubDate = self._getPubDate(sSuperLbl)
		lFileTups = self._getPDSFilePairs(sPairDir, 'T%s_B_LRB.HTM', 'T%s_E_LRB.HTM')

		sFtr = """OBJECT                  = HTML_DOCUMENT\r
  DOCUMENT_NAME           = "CASSINI RPWS LRFC SPECTROGRAM PLOTS"\r
  DOCUMENT_TOPIC_TYPE     = "BROWSE HTML"\r
  INTERCHANGE_FORMAT      = ASCII\r
  DOCUMENT_FORMAT         = HTML\r
  PUBLICATION_DATE        = %s\r
  FILES                   = %d\r
  DESCRIPTION             = "These files are HyperText Markup Language\r
    Version 3.2 aka Wilber (-//W3C//DTD HTML 3.2//EN) for the Cassini\r
    RPWS Low Rate Full Resolution Spectograms PNG files."\r
END_OBJECT              = HTML_DOCUMENT\r
END\r
"""%(sPubDate, 2 * len(lFileTups) )
		
		nSize = self._genPairedMultiFileLbl(sHdr, lFileTups, sFtr, sOutLbl)
		return (nSize, 2 * len(lFileTups))

	########################################################################
	def _genLRB_PNG(self, nExpected):
		
		sPairDir = pjoin(self.sDestDir, 'HTML')
		sOutLbl = pjoin(sPairDir, 'LRB_PNG.LBL')
		sSuperLbl = pjoin(self.sSrcDir, 'HTML', 'LRB_PNG.LBL')
		
		self.log.info("Writing: '%s'"%sOutLbl)
		
		sHdr = """PDS_VERSION_ID          = PDS3\r
RECORD_TYPE             = UNDEFINED\r
^PNG_DOCUMENT           = """
		
		sPubDate = self._getPubDate(sSuperLbl)
		lFileTups = self._getPDSFilePairs(sPairDir, 'T%s_B_LRB_TN.PNG',
		                                  'T%s_E_LRB_TN.PNG')
		nFiles = 2 * len(lFileTups)
		if nExpected !=  nFiles:
			raise RuntimeError("%s: File count mismatch %i .HTM but %i .PNG"%(
			                   sPairDir, nExpected, nFiles))

		sFtr = """OBJECT                  = PNG_DOCUMENT\r
  DOCUMENT_NAME           = "CASSINI RPWS LRFC SPECTROGRAM PLOTS"\r
  DOCUMENT_TOPIC_TYPE     = "BROWSE IMAGES"\r
  INTERCHANGE_FORMAT      = BINARY\r
  DOCUMENT_FORMAT         = PNG\r
  PUBLICATION_DATE        = %s\r
  FILES                   = %i\r
  ENCODING_TYPE           = PNG\r
  DESCRIPTION             = "\r
    These files are Portable Network Graphics (PNG Specification,\r
    Second Edition, ISO/IEC 15948:2003 E) images of Cassini RPWS\r
    Low Rate Full Resolution Spectrograms."\r
END_OBJECT              = PNG_DOCUMENT\r
END\r
"""%(sPubDate, nFiles )
		
		return self._genPairedMultiFileLbl(sHdr, lFileTups, sFtr, sOutLbl)


	########################################################################
	def parse(self, doyPub, sLnkTo):
		nSize = rpw_pds.util.symLinkCkSize(pjoin(self.sSrcDir, 'BROWSE.LBL'),
		                           pjoin(self.sDestDir, 'BROWSE.LBL'))
		(nSizeTmp, nFiles) = self._genLRB_HTML_LBL()
		nSize += nSizeTmp
		nSize += self._genLRB_PNG(nFiles)
		
		lFmtTup = [('T%s_E_LRB.HTM','T%s_E_LRB_TN.PNG','rpws low rate full'),
		           ('T%s_B_LRB.HTM','T%s_B_LRB_TN.PNG','rpws low rate full')]
		
		nTmp = self._genBOWSE(pjoin(self.sSrcDir, 'HTML'), lFmtTup, 
		                      'Cassini Low Rate Full', 'BROWSE.HTM', 
		                      nFiles*2)
		if nTmp > 0L:
			sLkPath = rpw_pds.util.relPath(sLnkTo, pjoin(self.sDestDir, 'BROWSE.HTM'), '/')
			sMstrLnk = '<A href="%s">%s</A>'%(sLkPath, self.sThroughStr)
			return (nSize+nTmp, [sMstrLnk])
		else:
			return (nSize, [])


##############################################################################
class keyParamBrowseReWriter(KP_LRbaseReWriter):
	""" This class dooes more label generating then actual rewriting.
	
	NOTE: Fill in all data links *before* calling this class's parse function.
	
   Outputs:
	   BROWSE/RPWS_KEY_PARAMETERS/TYYYYDXX/BROWSE.HTM    <-- generated
		BROWSE/RPWS_KEY_PARAMETERS/TYYYYDXX/KPB_HTML.LBL  <-- symlink
		BROWSE/RPWS_KEY_PARAMETERS/TYYYYDXX/KPB_PNG.LBL   <-- generated
		BROWSE/RPWS_KEY_PARAMETERS/TYYYYDXX/HTML/KPB_PNG.LBL  <- generated
		BROWSE/RPWS_KEY_PARAMETERS/TYYYYDXX/HTML/KPB_HTML.LBL <- generated
	"""
	########################################################################
	def __init__(self, lDates, sSrcDir, sDestDir):
		KP_LRbaseReWriter.__init__(self, lDates, sSrcDir, sDestDir)


	########################################################################
	def _genKPB_PNG_LBL(self):
		sOutLbl = pjoin(self.sDestDir, 'KPB_PNG.LBL')
		sSuperLbl = pjoin(self.sSrcDir, 'KPB_PNG.LBL')
		
		self.log.info("Writing: '%s'"%sOutLbl)
		
		sHdr = """PDS_VERSION_ID          = PDS3                                                \r
RECORD_TYPE             = UNDEFINED                                           \r
^PNG_DOCUMENT           = """
		
		sPubDate = self._getPubDate(sSuperLbl)
		lFileTups = self._getPDSFilePairs(self.sDestDir, 'T%s_B_KPB.PNG', 'T%s_E_KPB.PNG')

		sFtr = """OBJECT                  = PNG_DOCUMENT                                        \r
  DOCUMENT_NAME           = "CASSINI RPWS KEY PARAMETER SPECTROGRAM PLOTS"    \r
  DOCUMENT_TOPIC_TYPE     = "BROWSE IMAGES"                                   \r
  INTERCHANGE_FORMAT      = BINARY                                            \r
  DOCUMENT_FORMAT         = PNG                                               \r
  PUBLICATION_DATE        = %s                                        \r
  FILES                   = %i                                               \r
  ENCODING_TYPE           = PNG                                               \r
  DESCRIPTION             = "                                                 \r
    These files are Portable Network Graphics (PNG version 1.0, RFC 2083)     \r
    images of Cassini RPWS Key Parameter Spectrograms."                       \r
END_OBJECT              = PNG_DOCUMENT                                        \r
END                                                                           \r
"""%(sPubDate, 2 * len(lFileTups) )
		
		return self._genPairedMultiFileLbl(sHdr, lFileTups, sFtr, sOutLbl)


	########################################################################
	def _genKPB_HTML_LBL(self):
		sPairDir = pjoin(self.sDestDir, 'HTML')
		sOutLbl = pjoin(sPairDir, 'KPB_HTML.LBL')
		sSuperLbl = pjoin(self.sSrcDir, 'HTML', 'KPB_HTML.LBL')
		
		self.log.info("Writing: '%s'"%sOutLbl)
		
		sHdr = """PDS_VERSION_ID          = PDS3\r
RECORD_TYPE             = STREAM\r
^HTML_DOCUMENT           = """
		
		sPubDate = self._getPubDate(sSuperLbl)
		lFileTups = self._getPDSFilePairs(sPairDir, 'T%s_B_KPB.HTM', 'T%s_E_KPB.HTM')

		sFtr = """OBJECT                  = HTML_DOCUMENT\r
  DOCUMENT_NAME           = "CASSINI RPWS KEY PARAMETER SPECTROGRAM PLOTS"\r
  DOCUMENT_TOPIC_TYPE     = "BROWSE HTML"\r
  INTERCHANGE_FORMAT      = ASCII\r
  DOCUMENT_FORMAT         = HTML\r
  PUBLICATION_DATE        = %s\r
  FILES                   = %i\r
  DESCRIPTION             = "These files are HyperText Markup Language\r
    Version 3.2 aka Wilber (-//W3C//DTD HTML 3.2//EN) for the Cassini\r
    RPWS Key Parameter Spectrograms PNG files."\r
END_OBJECT              = HTML_DOCUMENT\r
END\r
"""%(sPubDate, 2 * len(lFileTups) )
		
		nSize = self._genPairedMultiFileLbl(sHdr, lFileTups, sFtr, sOutLbl)
		return (nSize, 2 * len(lFileTups))

	########################################################################
	def _genKPB_PNG(self, nExpected):
		
		sPairDir = pjoin(self.sDestDir, 'HTML')
		sOutLbl = pjoin(sPairDir, 'KPB_PNG.LBL')
		sSuperLbl = pjoin(self.sSrcDir, 'HTML', 'KPB_PNG.LBL')
		
		self.log.info("Writing: '%s'"%sOutLbl)
		
		sHdr = """PDS_VERSION_ID          = PDS3\r
RECORD_TYPE             = UNDEFINED\r
^PNG_DOCUMENT           = """
		
		sPubDate = self._getPubDate(sSuperLbl)
		lFileTups = self._getPDSFilePairs(sPairDir, 'T%s_B_KPB_TN.PNG',
		                                  'T%s_E_KPB_TN.PNG')
		nFiles = 2 * len(lFileTups)
		if nExpected !=  nFiles:
			raise RuntimeError("%s: File count mismatch %i .HTM but %i .PNG"%(
			                   sPairDir, nExpected, nFiles))

		sFtr = """OBJECT                  = PNG_DOCUMENT\r
  DOCUMENT_NAME           = "CASSINI RPWS KEY PARAMETER SPECTROGRAM PLOTS"\r
  DOCUMENT_TOPIC_TYPE     = "BROWSE IMAGES"\r
  INTERCHANGE_FORMAT      = BINARY\r
  DOCUMENT_FORMAT         = PNG\r
  PUBLICATION_DATE        = %s\r
  FILES                   = %i\r
  ENCODING_TYPE           = PNG\r
  DESCRIPTION             = "\r
    These files are Portable Network Graphics (PNG version 1.0, RFC 2083)\r
    images of Cassini RPWS Key Parameter Spectrograms."\r
END_OBJECT              = PNG_DOCUMENT\r
END\r
"""%(sPubDate, nFiles )
		
		return self._genPairedMultiFileLbl(sHdr, lFileTups, sFtr, sOutLbl)


	########################################################################
	def parse(self, doyPub, sLnkTo):
		nSize = rpw_pds.util.symLinkCkSize(pjoin(self.sSrcDir, 'KPB_HTML.LBL'),
		                           pjoin(self.sDestDir, 'KPB_HTML.LBL'))
		nSize += self._genKPB_PNG_LBL()
		
		(nSizeTmp, nFiles) = self._genKPB_HTML_LBL()
		nSize += nSizeTmp
		nSize += self._genKPB_PNG(nFiles)
		
		lFmtTup = [('T%s_E_KPB.HTM','T%s_E_KPB_TN.PNG','rpws key parameter data'),
		           ('T%s_B_KPB.HTM','T%s_B_KPB_TN.PNG','rpws key parameter data')]

		nTmp = self._genBOWSE(pjoin(self.sSrcDir, 'HTML'), lFmtTup, 
		                      'Cassini Key Parameter Data', 'BROWSE.HTM', 
		                      nFiles*2)
		if nTmp > 0L:
			sLkPath = rpw_pds.util.relPath(sLnkTo, pjoin(self.sDestDir, 'BROWSE.HTM'), '/')
			sMstrLnk = '<A href="%s">\r\n%s</A>'%(sLkPath, self.sThroughStr)
			return (nSize+nTmp, [sMstrLnk])
		else:
			return (nSize, [])


##############################################################################
def mkMasterBrowse(sStageDir, dBrowseLinks, lDates):
	# Duration string, used when no data available
	
	sNoData = ""
	if lDates[0] == lDates[-1]:
		sNoData = '%s <I>No data available</I>'%lDates[0].getDOMpDOYstr()
	else:
		sNoData = '%s through %s <I>No data available</I>'%(
		          lDates[0].getDOMpDOYstr(), lDates[-1].getDOMpDOYstr())

	sOut = pjoin(sStageDir, 'BROWSE', 'BROWSE.HTM')
	
	log = logging.getLogger('mkMasterBrowse')
	log.info('Writing: %s'%sOut)

	fOut = file(sOut, 'wb')

	# Header
	fOut.write("""<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">\r
<HTML>\r
<HEAD>\r
<META http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">\r
<TITLE>Cassini RPWS Browse Image Selection</TITLE>\r
</HEAD>\r
\r
<BODY>\r
\r
<H1>Cassini RPWS Browse Image Selection</H1>\r
\r
<UL>\r
<LI><A href="#key">Key Parameter Spectrograms</A>\r
<LI><A href="#lrfc">Low Rate Spectrograms</A>\r
<LI><A href="#wbr">Wideband Spectrograms</A>\r
<LI><A href="#wfr">Waveform Spectrograms</A>\r
</UL>\r
<HR>\r
\r
""")

	# Key parameter Spectragrams
	fOut.write("""<H2><A name="key">Key Parameter Spectrograms</A></H2>\r
<IMG src="ANCILLARY/KEY.PNG" alt="RPWS_KEY_PARAMETERS"\r
width="100" height="75" border="0" align="left">\r
<P>The key parameter spectrograms are calibrated plots from the\r
LFR, MFR, and HFR.  Each presents a full day in time and a frequency\r
range from 2 Hz to 16 MHz.  The data set is described in\r
<A href="../CATALOG/KEYDS.CAT">CATALOG/KEYDS.CAT</A>.</P>\r
<BR clear="all">\r
<P>Each link in the following list presents a thumbnail index with a\r
maximum 100-day interval based on the first digit of the three-digit day\r
of year.</P>\r
<UL>\r
""")
	if len(dBrowseLinks['RPWS_KEY_PARAMETERS']) < 1:
		fOut.write("<LI>%s\r\n"%sNoData)
	else:
		dBrowseLinks['RPWS_KEY_PARAMETERS'].sort()
		for sLine in dBrowseLinks['RPWS_KEY_PARAMETERS']:
			fOut.write("<LI>%s\r\n"%sLine)
	fOut.write("</UL>\r\n<HR>\r\n\r\n")


	# Low Rate Spectrograms
	fOut.write("""<H2><A name="lrfc">Low Rate Spectrograms</A></H2>\r
<IMG src="ANCILLARY/LRFC.PNG" alt="RPWS_LOW_RATE_FULL"\r
width="100" height="75" border="0" align="left">\r
<P>The low rate spectrograms are calibrated plots from the LFR,\r
MFR, and HFR.  Each presents a full day in time and a frequency\r
range from 2 Hz to 16 MHz.  The data set is described in\r
<A href="../CATALOG/LRFULLDS.CAT">CATALOG/LRFULLDS.CAT</A>.</P>\r
<BR clear="all">\r
<P>Each link in the following list presents a thumbnail index with a\r
maximum 100-day interval based on the first digit of the three-digit day\r
of year.</P>\r
<UL>\r
""")
	if len(dBrowseLinks['RPWS_LOW_RATE_FULL']) < 1:
		fOut.write("<LI>%s\r\n"%sNoData)
	else:
		dBrowseLinks['RPWS_LOW_RATE_FULL'].sort()
		for sLine in dBrowseLinks['RPWS_LOW_RATE_FULL']:
			fOut.write("<LI>%s\r\n"%sLine)
			
	fOut.write("</UL>\r\n<HR>\r\n\r\n")
	
	#Wideband Spectrograms
	fOut.write("""<H2><A name="wbr">Wideband Spectrograms</A></H2>\r
<IMG src="ANCILLARY/WBR.PNG" alt="RPWS_WIDEBAND_FULL"\r
width="100" height="75" border="0" align="left">\r
<P>The wideband spectrograms are a set of calibrated plots of the\r
Fourier transformed waveform captured in either of two frequency ranges:\r
10kHz or 75kHz.  Each presents up to an hour in time.\r
The data set is described in\r
<A href="../CATALOG/WBFULLDS.CAT">CATALOG/WBFULLDS.CAT</A>.</P>\r
<BR clear=all>\r
<P>Each link below presents a list of individual days in a maximum\r
100-day interval which link to thumbnail indexes of hourly\r
spectrograms.</P>\r
<UL>\r
""")
	if len(dBrowseLinks['RPWS_WIDEBAND_FULL']) < 1:
		fOut.write("<LI>%s\r\n"%sNoData)
	else:
		dBrowseLinks['RPWS_WIDEBAND_FULL'].sort()
		for sLine in dBrowseLinks['RPWS_WIDEBAND_FULL']:
			fOut.write("<LI>%s\r\n"%sLine)
			
	fOut.write("</UL>\r\n<HR>\r\n\r\n")
	
	
	#Waveform Spectrograms
	fOut.write("""<H2><A name="wfr">Waveform Spectrograms</A></H2>\r
<IMG src="ANCILLARY/WFR.PNG" alt="RPWS_WAVEFORM_FULL"\r
width="100" height="75" border="0" align="left">\r
<P>The waveform spectrograms are a set of calibrated plots of the\r
Fourier transformed waveform captured in either of two frequency ranges:\r
25Hz or 2.5kHz.  Each presents up to a day in time.\r
The data set is described in\r
<A href="../CATALOG/WFFULLDS.CAT">CATALOG/WFFULLDS.CAT</A>.</P>\r
<BR clear="all">\r
<P>Each link in the following list presents a thumbnail index with a\r
maximum 100-day interval based on the first digit of the three-digit day\r
of year.</P>\r
<UL>\r
""")
	if len(dBrowseLinks['RPWS_WAVEFORM_FULL']) < 1:
		fOut.write("<LI>%s\r\n"%sNoData)
	else:
		dBrowseLinks['RPWS_WAVEFORM_FULL'].sort()
		for sLine in dBrowseLinks['RPWS_WAVEFORM_FULL']:
			fOut.write("<LI>%s\r\n"%sLine)
			
	fOut.write("</UL>\r\n<HR>\r\n")
	
	
	#Footer
	fOut.write("""<P><A href="http://validator.w3.org/check/referer"><IMG border="0" src=\r
"ANCILLARY/VALID_HTML32.PNG" alt="Valid HTML 3.2" height="31" width=\r
"88"></A></P>\r
</BODY>\r
</HTML>\r
""")

	fOut.close()
	return os.path.getsize(sOut)


