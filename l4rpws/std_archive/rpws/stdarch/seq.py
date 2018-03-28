"""seq.py -- generate PDS sequence info output from HTML inputs in
$PDS_SUPERVOL/EXTRAS/SEQUENCE_INFO

The main routine will generate the TXT sections for the trigger
in the DATAVOL if requested.
"""

import sys
import os
from os.path import join as pjoin
import string
import re
import logging
from copy import deepcopy

# General local libs
import rpw_pds.util
import pspice

# Libs in this module
import archutil
import trig
		

##############################################################################
class seq(object):
	"""Digs through a cassini sequence info file and creates an in memory
	representation of the sequence.
	"""
	def __init__(self, sFile, sSeq=None, start=None, end=None):
		"""Initialize a sequence object given an HTML file.  Params:
		
		sFile - Required, the HTML file to parse
		
		sSeq  - Optional, if specified make sure sequence name matches this
		        value, throws runtime error if it doesn't
		
		start - Optional, if specifed make sure sequence start date matches
		        this value, throws runtime error if it doesn't
		
		end   - Optional, if specifed make sure sequence end date matches
		        this value, throws runtime error if doesn't
		"""
	
		self.log = logging.getLogger('seqInfo')
	
		self._hexDigits = "0123456789abcdefABCDEF"
		self.sFile = sFile
		self.sName = None
		self.lTrigs = []  # List of dicts: 'name','brief','file', 'trig_obj'
		self.lCmds  = []  # List of dicts: 'name','brief'
		self.lTL    = []  # Time line, list of tuples 
		                  # (time_str, [t|c], short_txt, orig_text)
		self.bFlown = False  #May switch to true if file says so.
		
		
		self.regRow = re.compile(r'(<[Tt][Rr].*?>)(.*?)(</[Tt][Rr]>)', re.DOTALL)
		
		self.regCell = re.compile(r'(<[Tt][HhDd].*?>)(.*?)(</[Tt][HhDd]>)', re.DOTALL)
		
		fIn = file(sFile, 'rb')
		sData = fIn.read()
		fIn.close()
		
		# Get title
		regTitle = re.compile(
		   r'(<[Tt][Ii][Tt][Ll][Ee]>)(.*?)([SC][0-9]+)(.*?)(</[Tt][Ii][Tt][Ll][Ee]>)',
		   re.DOTALL)
		
		m = regTitle.search(sData)
		if m == None:
			sTmp = "%s: Can't Sequence name in the <title> section.", sFile
			raise RuntimeError(sTmp)
			
		self.sName = m.group(3).upper()
		
		if sSeq != None and self.sName != sSeq:
			sTmp = "%s: Expected %s for sequence name, found %s in <title>."%(
			                   sFile, sSeq, self.sName)
			raise RuntimeError(sTmp)
		
		# Get 1st 2 Tables, assume they are triger and commands respectively
		regTable = re.compile(
			r'(<[Tt][Aa][Bb][Ll][Ee].*?>)(.*?)(</[Tt][Aa][Bb][Ll][Ee]>)',re.DOTALL)
		
		nTbl = 0
		sTrigTbl = None
		sCmdTbl  = None
		for m in regTable.finditer(sData):
			if nTbl == 0:
				sTrigTbl = m.group(2)
			elif nTbl == 1:
				sCmdTbl = m.group(2)
			else:
				self.log.warning("%s: Ignoring unknown third table.",sFile)
			nTbl += 1
		
		if nTbl < 2:
			sTmp = "%s: Couldn't find Trigger and/or Command Tables"%sFile
			raise RuntimeError(sTmp)
		
		self.initTrigs(sTrigTbl)
		self.initCmds(sCmdTbl)
		
		#Get timeline
		regTL = re.compile('(<[Pp][Rr][Ee]>)(.*?)(</[Pp][Rr][Ee]>)',re.DOTALL)
		m = regTL.search(sData)
		if m == None:
			sTmp = "%s: Couldn't find timeline in <pre> tag."%sFile
			raise RuntimeError(sTmp)
		
		self.initTL("%s\n"%m.group(2).strip())
		
		# Now, check to make sure all items in time line are in triggers and
		# commands.
		bAllDescribed = self.chTLhasDescription()
		bAllInTL = self.chDescInTL()
				
		if not bAllDescribed or not bAllInTL:
			sErr = ""
			if not bAllDescribed:
				sErr = "Description tables incomplete"
				
			if not bAllInTL:
				if len(sErr) > 0:
					sErr += " and "
				sErr += "Discription tables contain extra entries"
				
			raise RuntimeError('%s:%s.'%(sFile, sErr))
		
		#Load the trigger descriptions.
		for dTrig in self.lTrigs:
			if dTrig['file'] != None:
				dTrig['obj'] = trig.trig(dTrig['file'], dTrig['name'])
	  
	
	########################################################################
	def initTrigs(self, sTrigTbl):
		# 1st count the <tr> tags and for later comparison.
		_sTrigTblLow = sTrigTbl.lower()
		nCountRows = _sTrigTblLow.count('<tr>')
		
		regLink = re.compile(r'(<[Aa].*?")(.*?)(".*?>)(.*?)(</[Aa]>)', re.DOTALL)
		
		nRows = 0
		for mRow in self.regRow.finditer(sTrigTbl):
			nRows += 1
			lCells = []
			
			for mCell in self.regCell.finditer(mRow.group(2)):
				if mCell.group(3).lower() == '</th>':
					continue 
			
				lCells.append( mCell.group(2) )
			
			if len(lCells) == 0:
				continue
				
			if len(lCells) != 2:
				sTmp = "%s: Row %d of the trigger table doesn't have two columns."%(
				       self.sFile, nRow)
				raise RuntimeError(sTmp)
			
			if lCells[1].strip().lower() == 'Description':
				continue
			
			#Get trigger name and link (if any)
			mLink = regLink.search(lCells[0])
			if mLink != None:
				self.lTrigs.append( {
				  'name' : mLink.group(4).strip(),
				  'brief': lCells[1].strip(),
				  'file' : pjoin( os.path.dirname(self.sFile), 
				                 mLink.group(2).strip()),
				  'obj'  : None
				})
			else:
				self.lTrigs.append( {
				   'name':lCells[0].strip(), 'brief':lCells[1].strip(),
					'file':None, 'obj':None
				})
				self.log.warning('%s: No description file for Trigger %s.'%(
				   self.sFile, lCells[0].strip()))

		if nCountRows != nRows:
			sTmp = "%s: No all trigger table rows were read!"%self.sFile
			raise RuntimeError(sTmp)
	
	########################################################################
	def initCmds(self, sCmdTbl):
		
		for mRow in self.regRow.finditer(sCmdTbl):
		
			lCells = []
			for mCell in self.regCell.finditer(mRow.group(2)):
				if mCell.group(3).lower() == '</th>':
					continue 
				else:
					lCells.append( mCell.group(2) )
	
			if len(lCells)	== 0:
				continue
			
			if lCells[1].strip().lower() == 'Description':
				continue
			
			self.lCmds.append({'name':lCells[0].strip(),'brief':lCells[1].strip()})
	
	
	########################################################################
	def getCmdDesc(self, sExplicitCmd):
		"""Given an explicit command, see if there is a command that is
		the same to within a variable, and return it's description."""
		
		dCmdNames = {}
		for dCmd in self.lCmds:
			dCmdNames[ dCmd['name'] ] = dCmd['brief']
		
		for sGenCmd in dCmdNames.iterkeys():
			nLenGen = len(sGenCmd)
			
			bSame = True
			for i in range(0,len(sExplicitCmd)):
			
				if i < nLenGen:
					if sGenCmd[i] != sExplicitCmd[i]:
						if sGenCmd[i] != 'N' or \
						   sExplicitCmd[i] not in self._hexDigits:
							bSame = False
							break
				else:
					bSame = False
					break
							
					
				
			if bSame:
				#print "Same:", dCmdNames[sGenCmd]
				return dCmdNames[sGenCmd]
		
		return None
	
	########################################################################
	def initTL(self, sTL):
		regTL = re.compile(r'(\S*)(\s*)(.*)')
		
		lLines = sTL.split('\n')
		nLine = 0
		for sLine in lLines:
			sType = None
			sItem = None
			sOrig = None #Original item text, may have comments
			
			nLine += 1
			m = regTL.search(sLine)
			sErr = "%s: Couldn't parse line %s of the timeline."%(self.sFile, nLine)
			if m == None:
				raise RuntimeError(sErr)
			
			sTime = m.group(1).strip()
			if len(sTime) < 1:
				continue 
			
			sOrig = m.group(3).strip()
			if sOrig.lower().startswith('trigger'):
				sType = 't'
				lItem = sOrig.split()
				if len(lItem) < 2:
					raise RuntimeError(sErr)
				sItem = lItem[1]
			else:
				sType = 'c'
				sItem = sOrig
			
			self.lTL.append( (m.group(1), sType, sItem, sOrig) )
	
	
	########################################################################
	def chTLhasDescription(self):
		"""Check that all items in timeline have descriptions."""
		
		lTrigNames = []
		for dTrig in self.lTrigs:
			if dTrig['name'] not in lTrigNames:
				lTrigNames.append(dTrig['name'])
			else:
				sTmp = "%s: Trigger %s is mentioned twice in trigger table."%(sFile,
				       dTrig['name'])
				raise RuntimeError(sTmp)
		
		lCmdNames = []
		for dCmd in self.lCmds:
			if dCmd['name'] not in lCmdNames:
				lCmdNames.append(dCmd['name'])
			else:
				sTmp = "%s: Command %s is mentioned twice in command table."%(
					sFile, dCmd['name'])
		
		lSentError = []
		for tTL in self.lTL:
			if tTL[1] == 't':
				if tTL[2] not in lTrigNames:
					if tTL[2] not in lSentError:
						self.log.error('%s: Trigger %s'%(self.sFile, tTL[3])+\
						          ' is not in the description table.')
						lSentError.append(tTL[2])
			else:
				if self.getCmdDesc(tTL[2]) == None:
					if tTL[2] not in lSentError:
						self.log.error('%s: Command %s'%(self.sFile, tTL[3])+\
						          ' is not in the description table.')
						lSentError.append(tTL[2])
	
		return (len(lSentError) < 1)		
	
	########################################################################
	def sameCmd(self, sExplicit, sGen):
		"""Compare some explicit command to a more general form and see
		if the 1st is a special case of the second, 'N' can be a variable"""
		
		# Zero length commands are considered unknown and unknown never
		# equals unknown
		nLenEx = len(sExplicit)
		nLenGn = len(sGen)
		
		if nLenEx == 0 or nLenGn == 0:
#			print sExplicit, "<>", sGen, "len 0"
			return False
		
		# N is a single digit substitution
		if nLenEx != nLenGn:
#			print sExplicit, "<>", sGen, "len unequal"
			return False
		
		bSame = True
		for i in range(0,nLenGn):
	
			if sGen[i] != sExplicit[i]:
			
				if sGen[i] == 'N':
					if sExplicit[i] not in self._hexDigits:
#						print sExplicit, "<>", sGen, "Gen <> Exp, Gen = 'N' but sExp[%d] ="%i,sExplicit[i]
						bSame = False
						break
				else:
#					print sExplicit, "<>", sGen, "Gen <> Exp, and Gen != 'N'"
					bSame = False
					break
		
		return bSame
	
	########################################################################
	def chDescInTL(self):
		"""Check that all described items appear in the timeline"""
		
		bAllIn = True
		for dTrig in self.lTrigs:
			bIn = False
			for tTL in self.lTL: #(time_str, [t|c], short_txt, orig_text)
				if tTL[1] == 't' and tTL[2].lower() == dTrig['name'].lower():
					bIn = True
					break
			
			if not bIn:
				self.log.error("%s: Trigger %s does not appear in the timeline."%(
				               self.sFile, dTrig['name']))
				bAllIn = False
				
		
		for dCmd in self.lCmds:
			bIn = False
#			print
#			print "-------------------------------------"
			for tTL in self.lTL:
				if tTL[1] == 'c' and self.sameCmd(tTL[2], dCmd['name']):
					bIn = True
					break
			
			if not bIn:
				self.log.error("%s: Command %s does not appear in the timeline."%(
				               self.sFile, dCmd['name']))
				bAllIn = False
		
		return bAllIn
		
	########################################################################
	def prn(self):
		"""Could use help"""
		print self.sName
		print
		print "Triggers:"
		
		for d in self.lTrigs:
			if d.has_key('file'):
				print "%s  %s"%(d['name'], d['file'])
			else:
				print d['name']
			
			print "    %s"%d['brief']
			print
			
		print "Commands:"
		
		for d in self.lCmds:
			print d['name']
			print "   ", d['brief']
			print
		
		print
		for t in self.lTL:
			if t[1] == 't':
				print "trig: ",t[0],t[3]
			else:
				print "cmd:  ",t[0],t[3]


	######################################################################
	# PDS output functions
	
	def _pdsLabel(self, log, sTplt, sSeqOutDir, bOverWrite):
		dRep = {}
		dRep['recs'] = len(self.lTL) + 1
		dRep['seq'] = self.sName
		dRep['start-scet'] = self.lTL[0][0]
		dRep['stop-scet']  = self.lTL[-1][0]
		dRep['create-date'] = rpw_pds.util.today()
		dRep['start-sclk'] = archutil.scetToSclk(dRep['start-scet'])
		dRep['stop-sclk'] = archutil.scetToSclk(dRep['stop-scet'])
		dRep['rows'] = len(self.lTL)
		
		fIn = file(sTplt, 'rb')
		sFmt = fIn.read()
		fIn.close()
		
		sOut = pjoin(sSeqOutDir, '%s_TOL.LBL'%self.sName)
		
		if os.path.exists(sOut) and not bOverWrite:
			log.error("%s: File exists, and I was asked not to overwrite it."%sOut)
			raise RuntimeError("%s: File exists"%sOut)
			
		fOut = file(sOut, 'wb')
		fOut.write(sFmt%dRep)
		fOut.close()
	
	
	def _pdsTimeLine(self, log, sSeqOutDir, bOverWrite):
		sOut = pjoin(sSeqOutDir, '%s_TOL.TAB'%self.sName)
		if os.path.exists(sOut) and not bOverWrite:
			log.error("%s: File exists, and I was asked not to overwrite it."%sOut)
			raise RuntimeError("%s: File exists"%sOut)
		
		fOut = file(sOut, 'wb')
		sFmt = '%-21s    "%-51s"\r\n'
		fOut.write(sFmt%('"Start Time"', "Trigger or command"))
		
		for tTL in self.lTL:
			fOut.write(sFmt%(tTL[0], tTL[3]))
		
		fOut.close()
	
	def _pdsSeqDesc(self, log, sSeqOutDir, bOverWrite):
		sOut = pjoin(sSeqOutDir, '%s_IEB.TXT'%self.sName)
		if os.path.exists(sOut) and not bOverWrite:
			log.error("%s: File exists, and I was asked not to overwrite it."%sOut)
			raise RuntimeError("%s: File exists"%sOut)
		
		fOut = file(sOut, 'wb')
		sFmt = "%-78s\r\n"
		fOut.write(sFmt%"PDS_VERSION_ID         = PDS3")
		fOut.write(sFmt%"RECORD_TYPE            = STREAM")
		fOut.write(sFmt%"OBJECT                 = TEXT")
		fOut.write(sFmt%("  PUBLICATION_DATE     = %s"%rpw_pds.util.today()))
		fOut.write(sFmt%('  NOTE                 = "%s_IEB.TXT describes the set of Instrument'%self.sName))
		fOut.write(sFmt%("      Expanded Blocks (IEBs) and individual commands used in the %s"%self.sName))
		
		sStart = self.startDOY().getDOMstr()
		sEnd   = self.endDOY().getDOMstr()
		fOut.write(sFmt%('      sequence from %s through %s, and a short'%(sStart, sEnd)))
		fOut.write(sFmt%'      description of each."')
		fOut.write(sFmt%"END_OBJECT             = TEXT")
		fOut.write(sFmt%"END")
		fOut.write(sFmt%"")
		
		fOut.write(sFmt%("%s RPWS Triggers"%self.sName))
		fOut.write(sFmt%"-----------------")
		fOut.write(sFmt%"")
		
		for dTrig in self.lTrigs:
			fOut.write(sFmt%("Trigger %s"%dTrig['name']))

			for sLine in rpw_pds.util.lnBreak(dTrig['brief'], "", "  ", 78):
				fOut.write(sFmt%sLine)
			
			if dTrig['obj'] != None:
				fOut.write(sFmt%("  For more information see TRIG_%s.TXT."%dTrig['name']))
			
			fOut.write(sFmt%"")
		
		fOut.write(sFmt%"")
		fOut.write(sFmt%"")
		fOut.write(sFmt%("%s RPWS Commands"%self.sName))
		fOut.write(sFmt%"-----------------")
		fOut.write(sFmt%"")

		for dCmd in self.lCmds:
			fOut.write(sFmt%dCmd['name'])
			
			for sLine in rpw_pds.util.lnBreak(dCmd['brief'], "", "  ", 78):
				fOut.write(sFmt%sLine)
		
			fOut.write(sFmt%"")
			
		fOut.close()
		
			
	######################################################################
	def pdsOutput(self,  sTplt, sSeqOutDir, bOverWrite):
		"""Given a directory and a label template, write the PDS files
		   
			SXX_IEB.TXT, SXX_TOL.LBL, SXX_TOL.TAB, SXX_TRIG_XX.TXT
		"""
		log = logging.getLogger('seq.pdsOutput')
		
		self._pdsLabel(log, sTplt, sSeqOutDir, bOverWrite)
		
		self._pdsTimeLine(log, sSeqOutDir, bOverWrite)
		
		self._pdsSeqDesc(log, sSeqOutDir, bOverWrite)

		for dTrig in self.lTrigs:
			sOut = pjoin(sSeqOutDir, "TRIG_%s.TXT"%dTrig['name'])
			if dTrig['obj'] != None:
				dTrig['obj'].writePDSfile(sOut, bOverWrite)
		
		

	######################################################################
	# Getting start and stop times.
	
	def start(self):
		"""Returns 1st timeline string"""
		return self.lTL[0][0]
	
	def startDOY(self):
		s = self.lTL[0][0]
		return rpw_pds.util.DOYdate(int(s[:4]), int(s[5:8]))
		
	def end(self):
		return self.lTL[-1][0]
	
	def endDOY(self):
		s = self.lTL[-1][0]
		return rpw_pds.util.DOYdate(int(s[:4]), int(s[5:8]))
	
	
##############################################################################
def listSeq(log, sSuperVol, sSeq = None):
	"""Opens $SUPER_VOL/EXTRAS/SEQUENCE_INFO/INDEX.HTM and finds 
	all sequences.
	
	sSuperVol - Location of super volume
	sSeq      - if not None, just return info for sequence sSeq
	
	
	Returns a list of dictionaries with the keys:
	
	   'seq'   - name of sequence
	   'link'  - name of file contianing more info on the sequence.
	   'start' - DOYdate object with start date
	   'end'   - DOYdate object with end date
	
	If sSeq is supplied, then only that sequence will be in the
	output list, if it was found.
	
	May return an empty list.
	"""
	
	sIdxFile = pjoin(sSuperVol, 'EXTRAS','SEQUENCE_INFO', 'INDEX.HTM')
	fIn = file(sIdxFile, 'rb')
	log.info('Reading sequences from %s'%sIdxFile)
	sIdxData = fIn.read()
	fIn.close()
	
	regRow = re.compile(r'(<[Tt][Rr].*?>)(.*?)(</TR>)', re.DOTALL)
	regCell = re.compile(r'(<[Tt][Dd].*?>)(.*?)(</[Tt][Dd]>)', re.DOTALL)
	regSeq  = re.compile(r'([CS][0-9]+)(.*?)(<[Aa] [Hh][Rr][Ee][Ff]=")(.*?)(">.*)',
	                     re.DOTALL)
								
	regDate = re.compile(r'([0-9]{2})(.*?)([0-9]{4})(.*?)([0-9]{3})')
	
	lSeq = []
	
	for mRow in regRow.finditer(sIdxData):
		sRowData = mRow.group(2)
		
		_sSeq = ""
		sLink = ""
		doyStart = None
		doyEnd = None
		
		lCells = []
		for mCell in regCell.finditer(sRowData):
			lCells.append( mCell.group(2) )
		
		mSeq = regSeq.search(lCells[0])
		if mSeq != None:
			_sSeq = mSeq.group(1)
			sLink = pjoin(sSuperVol, 'EXTRAS','SEQUENCE_INFO', mSeq.group(4))
			
			if sSeq != None:
				if _sSeq != sSeq:
					continue
		else:
			continue
		
		mStart = regDate.search(lCells[1])
		if mStart != None:
			doyStart = rpw_pds.util.DOYdate(int(mStart.group(3)), int(mStart.group(5)))
		else:
			continue
		
		mEnd = regDate.search(lCells[2])
		if mEnd != None:
			doyEnd = rpw_pds.util.DOYdate(int(mEnd.group(3)), int(mEnd.group(5)))
		else:
			continue

		lSeq.append({'seq':_sSeq, 'link':sLink, 'start':doyStart, 'end':doyEnd})
	
	return lSeq

##############################################################################
def main():
	"""Testing function used during developement"""
	
	rpw_pds.util.setupLogging('warning')
	
	if len(sys.argv) < 2:
		print "Listing all Cassini RPWS sequences..."
		print
	
		l = listSeq("/opt/project/cassini/pds")
		for d in l:
			print "%4s: %s through %s in %s"%(d['seq'], d['start'].getDOMstr(),
			                                  d['end'].getDOMstr(), d['link'])
		return 0
		
	else:
		nRet = 0
		for sSeq in sys.argv[1:]:
#			print "Loading Sequence", sSeq.upper()
		
			l = listSeq("/opt/project/cassini/pds", sSeq.upper())
			if len(l) == 0:
				print "ERROR: Couldn't gather information on sequence ",\
				      sSeq.upper()
				return 1
			
			d = l[0]
			try:
				seq = seq(d['link'],d['seq'],d['start'],d['end'])
				print "Sequence %s looks okay."%d['seq']
			except RuntimeError, e:
				print str(e)
				nRet = 1
		
		return nRet


##############################################################################
if __name__ == "__main__":
	sys.exit(main())	
	













