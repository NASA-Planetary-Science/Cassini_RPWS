"""Trig.py - objects and functions to parse and print Cassini trigger pages."""

import sys
import os
from os.path import join as pjoin
import string
import re
from copy import deepcopy
import logging

import StringIO

#Local libs
import rpw_pds.util

g_sHFRFmt = '/opt/project/cassini/archive/script/cas_seq/hfrfmt'

##############################################################################
def stripHtml(sIn):
	"""Given a string, sIn return the output with all HTML tags stripped out.
	removes tags only, not thier contents"""
	
	sBuf = StringIO.StringIO()
	
	bInTag = False
	for c in sIn:
		
		if bInTag:
			if c == ">":
				bInTag = False
			continue
	
		if c == "<":
			bInTag = True
			continue
		
		sBuf.write(c)
	
	return sBuf.getvalue()

##############################################################################
def tagSplit(sHTML):
	"""Similar to string.split, but also cuts treats the chars < and > as
	boundaries."""
	
	lOut = []
	
	sWord = ""
	for c in sHTML:
		
		bBreak = (c in string.whitespace or c in "<>")
		
		if not bBreak or c == ">":
			sWord += c
		
		if bBreak and len(sWord) > 0:
			lOut.append(sWord)
			sWord = ""
		
		if c == "<":
			sWord += c
	
	if len(sWord) > 0:
		lOut.append(sWord)
	
	return lOut
		

##############################################################################
class trig(object):
	"""Digs through a cassini trigger info file and creates an in memory
	representation of the trigger description.  Internal memory objects are.
	
	   sName - Trigger Name
		
      sFile - Source HTML file
	
	   lNodes - List of document data nodes, format is:
		     
			  (type, html, text)
		
		bAltersHFR - True if it could be determined that this trigger alters
		     the HFR settings.
		
	   where:
		   type is one of: d - discriptive text, t - <table> section
	                      p - <pre> section (un-recognized as rpwsmode tbl)
	
	      html - original html for section
	      
			text - plain text conversion.
	"""
	
	#Constants for NODE objects:
	TYPE = 0
	HTML = 1
	TEXT = 2
	
	tRecievers = (
		'High Frequency Receiver (HFR)', 'Medium Frequency Receiver (MFR)',
		'Low Frequency Receiver (LFR)', 'Waveform Receiver (WFR)',
		'Wideband Reciever (WBR)', 'Langmiur Probe (LP)', 'Sounder',
		'Dust detector algorithm'
	)

	HFR = 0
	MFR = 1
	LFR = 2
	WFR = 3
	WBR = 4
	LP  = 5
	SOUNDER = 6
	
	lKeyReg = [
		[re.compile(r'HFR'),
		 re.compile(r'[Hh]igh\s*[Ff]requency\s*[Rr]eceiver', re.DOTALL)],
		
		[re.compile(r'MFR'),
		 re.compile(r'[Mm]edium\s*[Ff]requency\s*[Rr]eceiver', re.DOTALL)],
		
		[re.compile(r'LFR'),
		 re.compile(r'[Ll]ow\s*[Ff]requency\s*[Rr]eceiver', re.DOTALL)],
		
		[re.compile(r'WFR'),
		 re.compile(r'[Ww]aveform\s*[Rr]eceiver', re.DOTALL)],
		
		[re.compile(r'WBR'),
		 re.compile(r'[Ww]ideband\s*[Rr]eceiver', re.DOTALL)],
		 
		[re.compile(r'LP'),
		 re.compile(r'[Ll]angmiur\s*[Pp]robe', re.DOTALL)],
		 
		[re.compile(r'[Ss]ounder')],
		
		[re.compile(r'[Dd]ust\s*[Dd]etect', re.DOTALL) ]
	]
		
	
	def __init__(self, sFile, sName):
	
		if not sys.platform.startswith('sunos'):
			raise EnvironmentError("This program runs the Solaris only program "+\
			                       "%s as a subprocess, it won't work on %s"%(
										  g_sHFRFmt, sys.platform))
	
		self.log = logging.getLogger('trigInfo')
	
		self.sName = ""
		self.sFile = sFile
		self.lNodes = []   #See doc note above for layout.
		self.lDescribes = [False, False, False, False, False, False,
		                   False, False]
		
		fIn = file(sFile, 'rb')
		sData = fIn.read()
		fIn.close()
		
		regTitle = re.compile(
			r'(<[Tt][Ii][Tt][Ll][Ee]>)(\s*)(Trigger)(\s*)(\S*)(.*?</[Tt])',
			re.DOTALL)
		
		mTitle = regTitle.search(sData)
		if mTitle == None:
			raise RuntimeError("%s: Couldn't find trigger name in <title>"%sFile)
			
		self.sName = mTitle.group(5)
		
		regBody = re.compile(r'(<[Bb][Oo][Dd][Yy]>)(.*?)(</[Bb][Oo][Dd][Yy]>)',
		                     re.DOTALL)
									
		mBody = regBody.search(sData)
		if mBody == None:
			raise RuntimeError("%s: Couldn't find document body."%sFile)
		
		sBody = mBody.group(2)
		
		# Get document nodes <table> and <pre> define node boundaries.
		regPre = re.compile(r'(<[Pp][Rr][Ee]>)(.*?)(</[Pp][Rr][Ee]>)', re.DOTALL)
		regTbl = re.compile(
		  r'(<[Tt][Aa][Bb][Ll][Ee].*?>)(.*?)(</[Tt][Aa][Bb][Ll][Ee]>)',re.DOTALL)
		
		lNodeTmp = []
		for m in regPre.finditer(sBody):
			lNodeTmp.append( ['p', m.start(), m.end()] )
			
		for m in regTbl.finditer(sBody):
			lNodeTmp.append( ['t', m.start(), m.end()] )
			
		lNodeTmp.sort(lambda tA, tB: cmp( tA[1], tB[1]) )
		
		
		START = 1
		END   = 2
		
		iNode = 0
		iStr = 0
		while iStr < len(sBody):
		
			if iStr < lNodeTmp[iNode][START]:
				self.lNodes.append( ('d', sBody[iStr:lNodeTmp[iNode][START]]) )
				iStr = lNodeTmp[iNode][START]
				
			elif iStr == lNodeTmp[iNode][START]:
				self.lNodes.append( (lNodeTmp[iNode][self.TYPE], 
				                     sBody[iStr:lNodeTmp[iNode][END]+1]) )
				
				iStr  = lNodeTmp[iNode][END]+1
				if iNode < len(lNodeTmp) - 1:
					iNode += 1
			
			else:
				#Get remainder of body
				self.lNodes.append( ('d', sBody[iStr:]) )
				iStr = len(sBody)
		
		# Now format the html for text viewing:
		self._reFormatNodes()
		
		# See what is discribed.
		sText = ""
		for tNode in self.lNodes:
			sText += tNode[self.TEXT]
		
		for i in range(0, len(self.tRecievers)):
			for reg in self.lKeyReg[i]:
				if reg.search(sBody) != None:
					self.lDescribes[i] = True
					break
	
	########################################################################
	def _reFormatNodes(self):
		"""Re fromats the HTML into text, with PDS wrapping at 78 chars.
		"""
		
		for i in range(0,len(self.lNodes)):
			
			cType = self.lNodes[i][self.TYPE]
			
#			print "NODE ", cType.upper(), "Original HTML:"
#			print "----------------------"
#			print self.lNodes[i][self.HTML]
#			print "----------------------"
#			print
			
			if cType == 'd':
				
				self.lNodes[i] = ('d', self.lNodes[i][self.HTML],
				                  self._reFormatText(self.lNodes[i][self.HTML]))
			elif cType == 'p':
				sHTML = self.lNodes[i][self.HTML]
				if sHTML == None or len(sHTML) == 0:
					raise ValueError("Null HTML for node from: %s"%self.sFile)
				try:
					self.lNodes[i] = ('p', sHTML, self._reFormatPre(sHTML))
				except RuntimeError, e:
					if sHTML.find('Correlations') > -1:
						self.log.info("%s: Running long <pre> section through hfrfmt"%self.sFile)
					else:
						raise e
					
					(stdin, stdout) = os.popen2(g_sHFRFmt, 'rb')
					stdin.write(sHTML)
					stdin.close()
					sNewHTML = stdout.read()
					self.lNodes[i] = ('p', sNewHTML, self._reFormatPre(sNewHTML))
					
					
			elif cType == 't':
				self.lNodes[i] = ('t', self.lNodes[i][self.HTML],
				                  self._TblToLists(self.lNodes[i][self.HTML]))
				
			else:
				raise ValueError('Expected type to be d, p, or t: not %s'%cType)

			
#			print "NODE ", cType.upper(), "Reformated as TEXT:"
#			print "---------------------------"
#			print self.lNodes[i][self.TEXT]
#			print "---------------------------"
#			print

	
	########################################################################
	def _reFormatText(self, sHTML):
		sBuf = StringIO.StringIO()
		sNL = '\r\n'
		
		bComment = False
		iCol = 0
		
		for sWord in tagSplit(sHTML):
			sLower = sWord.lower()
					
			if bComment:
				if sLower == '-->':
					bComment = False
				continue
			
			#Ignore these tags
			if sLower in ['<h2>','<b>','<h1>','</p>','</b>']:
				continue
					
			if sLower == "<!--":  # Set comment on:
				bComment = True
				continue
			
			
			#Look for unclosed <pre> and <table> tags.
			if sLower == '<pre>':
				raise RuntimeError("%s: Unclosed <pre> tag detected!"%self.sFile)
			
			if sLower.startswith('<table'):
				raise RuntimeError("%s: Unclosed <table> tag detected!"%self.sFile)
					
			if sLower.startswith('</h') or sLower == '<p>':
				sBuf.write(sNL)
				if iCol > 0:
					sBuf.write(sNL)
				iCol = 0
				continue
			
			if sLower.startswith('<br'):
				sBuf.write(sNL)
				iCol = 0
				continue
					
			#Else write it out:
			if iCol + len(sWord) > 76: 
				sBuf.write(sNL)
				iCol = 0
					
			sBuf.write(sWord)
			iCol += len(sWord)
					
			if sWord.endswith('.') or sWord.endswith(':'):
				sBuf.write("  ")
				iCol += 2
			else:
				sBuf.write(" ")
				iCol += 1
					
			#Look up mentions here
		
		sTmp = sBuf.getvalue()
		
#		print
#		print sTmp
#		print
		
		return sTmp
	
	
	########################################################################
	def _reFormatPre(self, sHTML): 
		
		regPre = re.compile(r'(<[Pp][Rr][Ee]>)(.*?)(</[Pp][Rr][Ee]>)', re.DOTALL)
		mBody = regPre.search(sHTML)
		if mBody == None:
			raise ValueError("Failed to find <pre> section in:\n %s\nfrom: %s"%(
			                 sHTML, self.sFile))
		sBody = mBody.group(2)
		
		
		sBuf = StringIO.StringIO()
				
		#Just make sure lines not too long:
		for sLine in sBody.split('\n'):
			
			sTmp = sLine.rstrip()
			if len(sTmp) > 78:
				raise RuntimeError("%s: Preformatted text line > 78 chars."%
				                   self.sFile)
			sBuf.write('%s\r\n'%sTmp)
				
		return sBuf.getvalue()	
		
		
	########################################################################
	def _TblToLists(self, sHTML):
		lTbl = []
		regTR = re.compile(r'(<[Tt][Rr].*?>)(.*?)(</[Tt][Rr]>)',re.DOTALL)
		regTD = re.compile(r'(<[Tt][DdHh].*?>)(.*?)(</[Tt][DdHh]>)',re.DOTALL)
		
	
		for mRow in regTR.finditer(sHTML):
			
			sRow = mRow.group(2)
			
			lRow = []
			for mField in regTD.finditer(sRow):
				sTxt = stripHtml(mField.group(2)).strip()
				if sTxt == 'Samples per Snapshot':
					sTxt = 'Sample Size'
				elif sTxt == 'Time between Snapshots':
					sTxt = 'Snapshot Spacing'
					
				lRow.append(sTxt)
			
			lTbl.append( deepcopy(lRow) )
		
		
		# Normalize table (yea won't work if last columns are not empty ones)
		nCols = 0
		for lRow in lTbl:
			if len(lRow) > nCols:
				nCols = len(lRow)
		
		for lRow in lTbl:
			nCurCols = len(lRow)
			if nCurCols < nCols:
				for i in range(0, nCols - nCurCols):
					lRow.append("")
		
		if nCols == 0 or len(lTbl) == 0:
			return ""
		
		# Find col widths
		nCols = len(lTbl[0])
		lWidths = []
		lBars = []
		for iCol in range(0, nCols):
			nWidth = 0
			
			for iRow in range(0, len(lTbl)):
				sField = lTbl[iRow][iCol]
			
				if len(sField) > nWidth:
					nWidth = len(sField)
					
			lWidths.append(nWidth+1)
			lBars.append("-"*nWidth)
		
		#See if is too wide:
		nTmp = sum(lWidths) + nCols - 1 
		if nTmp > 78:
			raise RuntimeError("%s: Text version of table is > 78 chars"%self.sFile)
				
		#See if can go up to 2 spaces between cols
		if sum(lWidths) + nCols - 1 < 78:
			for i in range(0,nCols):
				lWidths[i] += 1
		
		# Get the format strings.
		lFmts = []
		for i in range(0, nCols):
			lFmts.append("%%-%ds"%lWidths[i])
			
		
		# Now print
		sBuf = StringIO.StringIO()
		
		for i in range(0, nCols):
			sBuf.write(lFmts[i]%lTbl[0][i])
		sBuf.write('\r\n')
		
		for i in range(0, nCols):	
			sBuf.write(lFmts[i]%lBars[i])
		sBuf.write('\r\n')
		
		
		for lRow in lTbl[1:]:
			
			for i in range(0, len(lRow)):
				sBuf.write(lFmts[i]%lRow[i])
			sBuf.write('\r\n')
			
		return sBuf.getvalue()

	###########################################################################
	
	def writePDSfile(self, sOutFile, bOverWrite=True):
		"""Write the trigger information as a PDS text file, to the given file
		name."""
		
		if os.path.exists(sOutFile) and not bOverWrite:
			self.log.error("%s: File exists, and I was asked not to overwrite it."%
			          sOutFile)
			raise RuntimeError("%s: File exists"%sOutFile)
		
		
		fOut = file(sOutFile, 'wb')
		sFmt = "%-78s\r\n"
		fOut.write(sFmt%"PDS_VERSION_ID         = PDS3")
		fOut.write(sFmt%"RECORD_TYPE            = STREAM")
		fOut.write(sFmt%"OBJECT                 = TEXT")
		fOut.write(sFmt%("  PUBLICATION_DATE     = %s"%
		                 rpw_pds.util.DOYdate().getDOMstr()))
		fOut.write(sFmt%'  NOTE                 = "')
		nLine = 5
		
		
		sOut = os.path.basename(sOutFile)
		sOut += " describes the RPWS instrument configuration for"
		
		nDescribes = 0
		for bDescribes in self.lDescribes:
			if bDescribes:
				nDescribes += 1
		
		nPrinted = 0
		for i in range(0, len(self.tRecievers)):
			if not self.lDescribes[i]:
				continue
			
			if nPrinted == 0:
				sOut += " the %s"%self.tRecievers[i]
				
			elif nPrinted > 0 and nPrinted < nDescribes-1:
				sOut += ", the %s"%self.tRecievers[i]
		
			else:
				if nDescribes > 1:
					sOut += ", and the %s"%self.tRecievers[i]
				else:
					sOut += " and the %s"%self.tRecievers[i]
		
			nPrinted += 1
		
		sOut += ' when using Trigger %s."'%self.sName
		
		
		# make lines out of a pile of words.
		lWords = sOut.split()
		sLine = "      "
		for sWord in lWords:
			
			# check flush
			if (len(sWord) + len(sLine)) > 72:
				fOut.write(sFmt%sLine)
				nLine += 1
				sLine = "      "
			
			sLine += "%s "%sWord
			if sWord[-1] == "." or sWord[-1] == ":":
				sLine += " "
			
		
		if sLine != "      ":  #Flush last line
			fOut.write(sFmt%sLine)
			nLine += 1
		
		fOut.write(sFmt%"END_OBJECT           = TEXT")
		fOut.write(sFmt%"END")
		fOut.write('\r\n')
		nLine += 3
		
		regLine = re.compile(r'(.*?)(.$)', re.MULTILINE)
		bLastWasBlank = True
		for lNode in self.lNodes:
			#Insure at least one blank line between node outputs.
			if not bLastWasBlank:
				fOut.write(sFmt%"")
				
			for mLine in regLine.finditer(lNode[self.TEXT]):
				sLine = mLine.group(1)
				
				nLine += 1
				if sLine == None or len(sLine) == 0:
					fOut.write(sFmt%"")
					bLastWasBlank = True
					
				elif len(sLine) <= 78:
					fOut.write(sFmt%sLine)
					bLastWasBlank = False
					
				else:
					fOut.write(sFmt%sLine)
					self.log.error("%s,%d: Text line is %d chars long."%(
					          sOutFile, nLine, len(sLine)))
					bLastWasBlank = False
			
		fOut.close()
		
		
		
		
		
		
		
		
