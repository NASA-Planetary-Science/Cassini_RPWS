"""Reading product labels and providing summary information"""

import sys
import types
import os.path
import stat
import time

from copy import deepcopy
from os.path import join as pjoin

import util as U
import parse as P

##############################################################################

def getLblPath(sVolRoot, sPath, dElements):
	"""Helper to fill in the volume path column value, may be used in
	the callback argument to the Column constructor.  And paired with
	the column "FILE_SPECIFICATION_NAME".
	"""
	
	if sPath.find(sVolRoot) == -1:
		raise IOError("%s: File is not contained by the volume root %s"%(
		              sPath, sVolRoot))
	
	sRelPath = sPath.replace(sVolRoot, '')
	
	# Handle vol root specs that don't have a trailing '/' character
	if sRelPath[0] == '/':
		sRelPath = sRelPath[1:]

	return sRelPath

##############################################################################

def getLblHash(sVolRoot, sPath, dElements):
	"""Helper to fill in the the label MD5 sum column, may be used as
	the callback argument to the Column constructor.
	"""
	return U.getFileHash(sPath)
	
	
##############################################################################

class Column(object):
	
	def __init__(self, sColName, sType, sDesc, sHdrName = None, 
	             sElement = None, sVal = None, callback = None):
		"""
		  sColName - The NAME of the column name in the .LBL file
		  
		  sType - The DATA_TYPE for the column in the .LBL file
		  
		  sDesc - the DESCRIPTION for the column in the .LBL file
		  
		  sHdrName - The column header in the .TAB file
		  
		  sElement - The ELEMENT to dig out of the product label files
		             defaults to sColName if not specified
						 
		  sVal - A constant value to place into the .TAB file, if 
		         specified the value in the product labels is ignored
					
		  callback - A callback function for getting the value from
		         a product label, the call back should take three args:
					
					  sVolRoot, sPath, dElements
					
		         sVolRoot - The Root of the volume being indexed
		         sPath    - The path to the label being indexed
		         dElements - A dictionary of the elements in the label being
					            indexed
		"""
		self.sColName = sColName	 
		self.sType = sType
		
		if sType not in ["CHARACTER", "INTEGER", "REAL", "IDENTIFIER", "TIME"]:
			raise ValueError("%s: Unknown data type '%s'"%(sColName, sType))
		
		self.sDesc = sDesc
		
		if sHdrName == None:
			self.sHdrName = sColName
		else:
			self.sHdrName = sHdrName
			
		if sElement == None:
			self.sElement = sColName
		else:
			self.sElement = sElement
		
		self.sVal = sVal
		
		self.callback = callback
		
		self.sNoVal = None # Set this to something else to allow for missing
		                   # values 'NULL'
		
		self.sWidth = len(self.sHdrName) + 2

	
	########################################################################
	def getVal(self, sVolRoot, sPath, dElements):
			
		if self.sVal != None:
			return self.sVal
		
		if self.callback != None:
			return self.callback(sVolRoot, sPath, dElements)
		
		if not dElements.has_key(self.sElement):
			if self.sNoVal != None:
				return self.sNoVal
			else:
				sRP = sPath.replace(sVolRoot, '')
				if sRP[0] == '/':
					sRP = sRP[1:]
				raise ValueError("%s,%s: Missing Value. "%(sRB, col.sColName)+\
				                 "(Set Column.sNoVal to allow for missing "+\
									  "element values in label files")
							
		if type(dElements[self.sElement]) != types.StringType:
			sRP = sPath.replace(sVolRoot, '')
			if sRP[0] == '/':
				sRP = sRP[1:]
			raise ValueError("%s,%s: Only single valued "%(sRP, self.sColName)+\
			                 "items allowed for INDEX entries")
		
		return dElements[self.sElement]
	
		
##############################################################################
class Indexer(object):
	"""Used to generate PDS volume indicies, 1st define the columns you want
	indexed, then feed it labels, finally write the results"""
	
	
	def __init__(self, log, sVolRoot, lStatic, lUnique, lExtrema, lCols):
		"""
		lStatic - List of static keyword value tuples ex:
		          [ ('INSTRUMENT_HOST_NAME':'"Juno"'), ... ]
		
		lExtrema - Elements to record as min or max values from all labels.
		
		lUnique - Elements to record as a unique list of all values found
		          in the product labels
		
		lCols - Elements to place into columns in the *.TAB file
		"""
		
		self.log = log
		self.sVolRoot = sVolRoot
		self.lStatic = lStatic
		self.bFileHdr = True
		
		self.lExtrema = []
		for tup in lExtrema:
			if tup[2].lower() not in ['min','max']:
				raise ValueError("%s: Extrema type must be 'min' or 'max' not '%s'"%(
				                 tup[0], tup[2]))
			lTmp = [tup[0], tup[1], tup[2].lower(), None, None]
			
			# Sometime data is extracted from one element but placed into
			# another, such as ORBIT_NUMBER -to-> START_ORBIT_NUMBER
			if len(tup) > 3:
				lTmp[3] = tup[3]
			else:
				lTmp[3] = lTmp[0]  # Record to extract element
			
			self.lExtrema.append( lTmp )
		
		self.lUnique = []
		for tup in lUnique:
			self.lUnique.append( [tup[0], tup[1], [] ] )
		
		self.lCols = lCols
		self.sorter = None
		
		# Get my list of stuff to extract from the labels
		self.lExtract = []
		for tup in self.lExtrema:
			self.lExtract.append(tup[3])
			
		for tup in self.lUnique:
			self.lExtract.append(tup[0])
		
		for col in self.lCols:
			self.lExtract.append(col.sElement)
			
		self.lRows = []
		
		self.bPSAquoteStyle = False
			
	########################################################################
	def _sorter(self, dRow1, dRow2):
		""" The default sorter just goes down the list trying to hit at least
		one column that is different, 0 cannot be returned"""
		
		lKeys = []
		for col in self.lCols:
			lKeys.append(col.sColName)
		
		for sKey in lKeys:
			
			if dRow1[sKey] > dRow2[sKey]:
				return 1
			
			if dRow1[sKey] < dRow2[sKey]:
				return -1
				
		
		raise ValueError("Dump of non-unique rows encontered:\n   %s\n   %s"%(
		                  dRow1, dRow2))
		return None
	
	
	def setSorter(self, sorter):
		"""
		Replace the standard output sorter with a custom version.  The 
		callback is given two row dictionaries and must return -1 if row
		the first row should be earlier in the file, 1 if the row should be
		later in the file and 0 if it makes no difference which row is first.
		"""
		self.sorter = sorter
	
	
	########################################################################
	def readLabel(self, sPath):
		
		if sPath.find(self.sVolRoot) == -1:
			raise ValueError("%s: Volroot not contained within the path"%sPath)
		
		sRP = sPath.replace(self.sVolRoot, '')
		if sRP[0] == '/':
			sRP = sRP[1:]
				
		dElements = P.extractFromRoot(sPath, self.lExtract, 0)
		
		# Strip the quote marks
		for sElement in dElements.keys():
		
			if dElements[sElement] != None:
				if type(dElements[sElement]) == types.StringType:
					dElements[sElement] = dElements[sElement].strip(' "\'')
			
				elif type(dElements[sElement]) == types.ListType:				
					for i in xrange(0, len(dElements[sElement])):
						dElements[sElement][i] = dElements[sElement][i].strip(' "\'')
				else:
					raise ValueError("%s: Don't know result type: %s"%(
					                 sRP, dElements[sElement]))
					
	
		# See if elements were collected for the extream values
		for extream in self.lExtrema:
			
			if not dElements.has_key(extream[3]) or dElements[extream[3]] == None:
				continue
			
			if extream[4] == None:
				extream[4] = dElements[extream[3]]
				continue
			
			if extream[2] == 'min':	
				if dElements[extream[3]] < extream[4]:
					extream[4] = dElements[extream[3]]
			else:
				if dElements[extream[3]] > extream[4]:
					extream[4] = dElements[extream[3]]

		
		# Get our unique list items
		for unique in self.lUnique:
			
			if not dElements.has_key(unique[0]) or dElements[unique[0]] == None:
				continue
			
			if type(dElements[unique[0]]) == types.StringType:
				
				if dElements[unique[0]] not in unique[2]:
					unique[2].append(dElements[unique[0]])
					
			elif type(dElements[unique[0]]) == types.ListType:
			
				for i in xrange(0, len(dElements[unique[0]])):
					
					if dElements[unique[0]][i] not in unique[2]:
						unique[2].append(dElements[unique[0]][i])
			else:
				raise ValueError("%s: Don't how to deal with %s return."%(
				                 sRP, type(dElements[unique[0]])))
				                 
		
		# Now get our row dictionary from the column items
		dRow = {}
		for col in self.lCols:
			dRow[col.sColName] = col.getVal(self.sVolRoot, sPath, dElements)
			
		
		self.lRows.append(dRow)
	
	
	########################################################################
	def _breakText(self, sText, nPreIndent, nSubIndent, nLine=78):
		"""Take a text block (that's aready quoted) and break in on word 
		boundaries.  Assume the 1st line may have less room that the others
		as this is common for KEYWORD = "TEXT BLOCK" pairs"""
		
		lWords = sText.split()
		nWords = len(lWords)
		iWord = 0
		iLine = 0
		
		llLines = []
		while iWord < nWords:
			
			if iLine == 0:
				nRoom = nLine - nPreIndent
			else:
				nRoom = nLine - nSubIndent
			
			llLines.append( [] )
			
			nUsed = 0
			while iWord < nWords:
				
				sWord = lWords[iWord]
			
				if len(sWord) + 1 + nUsed <= nRoom:
					llLines[-1].append(sWord)
					iWord += 1
					nUsed += len(sWord) + 1
				else:
				
					# Check for the pathologic case, single word won't fit on a
					# line
					if len(llLines[-1]) == 0 and len(sWord) + 1 > nRoom:
						raise ValueError ("Line break error: Word %s "%sWord +\
						      "to too big to fit on a single line of "+\
								"%d characters."%nRoom)
					break
			
			iLine += 1
				
			
		lsLines = []
		for lLine in llLines:
			lsLines.append( " ".join(lLine) )
		
		if nSubIndent > 0:
			sTmp = " "*nSubIndent
			sSep = '\r\n%s'%sTmp
		else:
			sSep = "\r\n"
		
		return sSep.join(lsLines)
		
	
	########################################################################
	def _prnKeyVal(self, sKey, sType, value, nIndent, nEquals, fOut):
		
		if nIndent != 0:
			fOut.write(" "*nIndent)
		
		# Use the distance to the equals sign -1 as the field width
		nSz = nEquals - nIndent - 1
		if nSz <= 0:
			raise ValueError("Equals sign position, %d, "%nEquals +\
			                 "is too close to the start of the line"+\
								  ", %d"%nIndent)
		
		sFmt = "%%-%ds = "%nSz
		fOut.write(sFmt%sKey)
		
		# Quote stuff as needed, go with recomendation 12.5.4.2 in the 
		# 3.8 standard of using " when ' should be used.
		if sType == "CHARACTER" or sType == 'IDENTIFIER':
			if sType == "CHARACTER" or self.bPSAquoteStyle:
				sQuote = '"'
			else:
				sQuote = "'"
			if type(value) == types.StringType:
				_value = '%s%s%s'%(sQuote, value, sQuote)
			elif type(value) == types.ListType:
			
				if len(value) == 1:
					_value = '%s%s%s'%(sQuote, value[0], sQuote)
				else:
					_value = []
					for i in xrange(0, len(value)):
						_value.append('%s%s%s'%(sQuote, value[i], sQuote))
			else:
				raise ValueError("value input in neither a string nor a list")
		else:
			_value = deepcopy(value)
		
		# Now handle printing the value
		if type(_value) == types.ListType:
			if len(_value) == 0:
				sVal = "NULL"
			else:
				sVal = U.pdsList(_value, nEquals + 2)
			fOut.write(sVal)
			
		else:  # Must be a string, see above
			
			# Okay if this is shorter than distance from equals to end of line
			# we have it made.
			if _value == None:
				self.log.error("You're about to have a problem with this key: %s"%sKey)
			if len(_value) < 77 - nEquals:
				fOut.write(_value)
			else:
				# Crap have to break lines
				sVal = self._breakText(_value, nEquals + 1, nIndent + 2)
				fOut.write(sVal)		
		
		fOut.write("\r\n")
	
	########################################################################
	def _mkLabel(self, lWidths, lBytes, nRecBytes, sHash, sOutDir, sBaseName,
	             sIdxProdId):
				
		if not os.path.isdir(sOutDir):
			self.log.info("%s: Dir created"%sOutDir)
			os.makedirs(sOutDir, 0775)
			os.chmod(sOutDir, stat.S_ISGID | stat.S_IRWXU | stat.S_IRWXG |
			         stat.S_IXOTH | stat.S_IROTH )
				
		sPath = pjoin(sOutDir, '%s.LBL'%sBaseName)
		
		if sPath.find(self.sVolRoot) != -1:
			sRP = sPath.replace(self.sVolRoot, '')
			if sRP[0] == '/':
				sRP = sRP[1:]
		else:
			sRP = sPath
						
		self.log.info("%s: Generating Index Label"%sRP)
		
		fOut = file(sPath, 'wb')
		
		fOut.write("PDS_VERSION_ID        = PDS3            /* Version 3.8 February 27, 2009 */\r\n\r\n")
		
		nEqCol = 24
		for tup in self.lStatic:
				
			self._prnKeyVal(tup[0], tup[1], tup[2], 0, nEqCol, fOut)
			if tup[0] == "LABEL_REVISION_NOTE":
				fOut.write("\r\n/* Identification Data Elements */\r\n")
		
		if sIdxProdId:
			self._prnKeyVal("PRODUCT_ID", "CHARACTER", sIdxProdId, 0, nEqCol, fOut)
			tTmp = time.gmtime()
			sTmp = "%04d-%02d-%02d"%(tTmp[0], tTmp[1], tTmp[2])
			self._prnKeyVal("PRODUCT_CREATION_TIME", "TIME", sTmp, 0, nEqCol, fOut)
			
		fOut.write("\r\n")
		
		for tup in self.lUnique:
			self._prnKeyVal(tup[0], tup[1], tup[2], 0, nEqCol, fOut)
			
		fOut.write("\r\n")
		
		for tup in self.lExtrema:
			#print tup
			self._prnKeyVal(tup[0], tup[1], tup[4], 0, nEqCol, fOut)
		
		fOut.write("\r\n")
		
		fOut.write('/* File characteristics Data Elements                                       */\r\n')
		fOut.write('\r\n')
		fOut.write('/* PDS 3 Labels are not polymorphic, derived types cannot always be placed  */\r\n')
		fOut.write('/* where the general types are allowed.  For example, even though generic   */\r\n')
		fOut.write('/* TABLE objects may be contained by FILE objects, the specific table type, */\r\n')
		fOut.write('/* INDEX_TABLE, cannot be contained within a FILE object.   Thus the FILE   */\r\n')
		fOut.write('/* object has to be commented out below, even though it is still implicitly */\r\n')
		fOut.write('/* present!                                                                 */\r\n')
		fOut.write('\r\n')
		fOut.write('/* OBJECT                  = FILE                                           */\r\n')
		fOut.write('/* FILE_NAME               = "%s.TAB"                 %s*/\r\n'%(
		          sBaseName, " "*len(sBaseName)))
		fOut.write('  RECORD_TYPE             = FIXED_LENGTH\r\n')
		fOut.write('  RECORD_BYTES            = %d\r\n'%nRecBytes)
		fOut.write('  FILE_RECORDS            = %d\r\n'%(len(self.lRows) + 1))
		fOut.write('  MD5_CHECKSUM            = "%s"\r\n'%sHash)
		fOut.write('  \r\n')
		
		if self.bFileHdr:
			fOut.write('  ^%s_TABLE            = ("%s.TAB",2)\r\n'%(sBaseName,sBaseName))
		else:
			fOut.write('  ^%s_TABLE            = "%s.TAB"\r\n'%(sBaseName,sBaseName))
		
		fOut.write('  \r\n')
		fOut.write('  /* Data Object */\r\n')
		fOut.write('  OBJECT                  = %s_TABLE\r\n'%sBaseName)
		fOut.write('    NAME                    = "%s_TABLE"\r\n'%sBaseName)
		fOut.write('    INDEX_TYPE              = SINGLE\r\n')
		fOut.write('    INTERCHANGE_FORMAT      = ASCII\r\n')
		fOut.write('    ROWS                    = %d\r\n'%len(self.lRows))
		fOut.write('    COLUMNS                 = %d\r\n'%len(self.lCols))
		fOut.write('    ROW_BYTES               = %d\r\n'%nRecBytes)
		fOut.write('    \r\n')
		
		nStartByte = 1
		for i in xrange(0, len(self.lCols)):
			col = self.lCols[i]
			
			if col.sType == "CHARACTER" or col.sType == "IDENTIFIER":
				nStartByte += 1
		
			fOut.write('    OBJECT                  = COLUMN\r\n')
			fOut.write('      NAME                    = "%s"\r\n'%col.sColName)
			
			# Throw in COLUMN_NUMBER to help out ESA's PVV tool, which looks to
			# get confused easily.
			fOut.write('      COLUMN_NUMBER           = %d\r\n'%(i+1))
			
			# Another inconsistancy in PDS.  Some of the values in PDS are of 
			# type IDENTIFIER, but if you try to create a column of these objects
			# the DATA_TYPE keyword can't take the type IDENTIFIER.
			if col.sType == "IDENTIFIER":
				fOut.write('      DATA_TYPE               = CHARACTER\r\n')
			else:
				fOut.write('      DATA_TYPE               = %s\r\n'%col.sType)
			
			# START_BYTE has to be handled differently for right adjusted fields
			# such as integers and reals
			if col.sType in ['INTEGER', 'REAL']:
				nLeftStartByte = nStartByte + (lWidths[i] - lBytes[i])
				fOut.write('      START_BYTE              = %d\r\n'%nLeftStartByte)
			else:
				fOut.write('      START_BYTE              = %d\r\n'%nStartByte)
					
			fOut.write('      BYTES                   = %d\r\n'%lBytes[i])
			self._prnKeyVal('DESCRIPTION', 'CHARACTER', col.sDesc, 6, nEqCol + 6, fOut)
			fOut.write('    END_OBJECT              = COLUMN\r\n')
			fOut.write('    \r\n')
			
			if col.sType == "CHARACTER" or col.sType == "IDENTIFIER":
				nStartByte += lWidths[i]
			else:
				nStartByte += lWidths[i] + 1	
		
		fOut.write('  END_OBJECT              = %s_TABLE\r\n'%sBaseName)
		fOut.write('  \r\n')
		fOut.write('/* END_OBJECT              = FILE                                           */\r\n')
		fOut.write('END\r\n')
		
	
		fOut.close()
		os.chmod(sPath, stat.S_IRUSR | stat.S_IWUSR | stat.S_IRGRP |
			      stat.S_IWGRP | stat.S_IROTH )
		
	
	########################################################################
	def setFileHeader(self, bOn):
		self.bFileHdr = bOn
	
	
	def _mkTable(self, lWidths, nRecBytes, sOutDir, sBaseName):
		
		if not os.path.isdir(sOutDir):
			self.log.info("%s: Dir created"%sOutDir)
			os.makedirs(sOutDir, 0775)
			os.chmod(sOutDir, stat.S_ISGID | stat.S_IRWXU | stat.S_IRWXG |
			         stat.S_IXOTH | stat.S_IROTH )
				
		sPath = pjoin(sOutDir, '%s.TAB'%sBaseName)
		
		if sPath.find(self.sVolRoot) != -1:
			sRP = sPath.replace(self.sVolRoot, '')
			if sRP[0] == '/':
				sRP = sRP[1:]
		else:
			sRP = sPath
			
		self.log.info("%s: Generating Index Table"%sRP)
				
		fOut = file(sPath, 'wb')
		
		
		# First do the header row, all items here need quotes, so this is easy
		
		if self.bFileHdr:
			for i in xrange(0, len(self.lCols)):
			
				if i == 0:
					sFmt = '"%%-%ds"'%(lWidths[i] - 2)
				else:
					sFmt = ',"%%-%ds"'%(lWidths[i] - 2)
				fOut.write(sFmt%self.lCols[i].sHdrName)
			
			fOut.write('\r\n')
		
		
		# Now for the data rows
		for dRow in self.lRows:
		
			for i in xrange(0, len(self.lCols)):
				
				col = self.lCols[i]
				
				sAdj = '-'
				if col.sType in ['INTEGER', 'REAL']:
					sAdj = ''
				
				if col.sType in ["CHARACTER","IDENTIFIER"]:
					
					if i == 0:
						sFmt = '"%%%s%ds"'%(sAdj, (lWidths[i] - 2))
					else:
						sFmt = ',"%%%s%ds"'%(sAdj, (lWidths[i] - 2))
				else:
					if i == 0:
						sFmt = '%%%s%ds'%(sAdj, lWidths[i])
					else:
						sFmt = ',%%%s%ds'%(sAdj, lWidths[i])
					
				fOut.write(sFmt%dRow[ col.sColName ])
			
			fOut.write('\r\n')
			
		fOut.close()
		os.chmod(sPath, stat.S_IRUSR | stat.S_IWUSR | stat.S_IRGRP |
			      stat.S_IWGRP | stat.S_IROTH )
		
		return U.getFileHash(sPath)
		

	########################################################################
	def mkIndex(self, sOutDir=None, sIdxProdId=False, sBaseName="INDEX"):
		"""Using the labels that have been read, generate an INDEX.TAB and 
		INDEX.LBL file.  
		
		sOutDir:  Default sVolRoot/INDEX
		   Normally these go under sVolRoot/INDEX but the sOutDir parameter
			can be used to write these to any absolute filesystem path
		
		sIdxProdId: Default False 
		   If true then treat the INDEX.TAB as a PDS product and provide a 
		   product ID and product creation time to the INDEX object.  This
			option is needed for PSA compliance.
		
		sBaseName: Default 'INDEX'
		   Use this to have the product be something other than INDEX_TABLE
		   defined in INDEX.LBL pointing to INDEX.TAB.  All instances of
		   INDEX in the label and output filenames will be replaced with this
		   value.  This is needed to make PSA 'BROWSE_INDEX.TAB' files
		"""
		
		if sOutDir == None:
			sOutDir = pjoin(self.sVolRoot, 'INDEX')
		
		# sort the output list
		if self.sorter != None:
			self.lRows.sort(self.sorter)
		else:
			self.lRows.sort(self._sorter)
		
		# Get the column widths
		lKeys = []
		for col in self.lCols:
			#sys.stdout.write(',"%s"'%col.sHdrName)
			lKeys.append(col.sColName)
		
		lWidths = [0]*len(self.lCols)
		lBytes  = [0]*len(self.lCols)
		
		for i in xrange(0, len(self.lCols)):
			
			if lWidths[i] < len(self.lCols[i].sHdrName) + 2:
				lWidths[i] = len(self.lCols[i].sHdrName) + 2
			
			nAdd = 0
			if self.lCols[i].sType in ["CHARACTER","IDENTIFIER"]:
				nAdd = 2
			
			for dRow in self.lRows:
				
				if lWidths[i] < len(dRow[self.lCols[i].sColName]) + nAdd:
					lWidths[i] = len(dRow[self.lCols[i].sColName]) + nAdd
				
				if lBytes[i] < len(dRow[self.lCols[i].sColName]):
					lBytes[i] = len(dRow[self.lCols[i].sColName])
		
		# Now get the line length
		nRecBytes = 2
		for nWidth in lWidths:
			nRecBytes += nWidth
		
		nRecBytes += len(lWidths) - 1 # Add in space for the commas
				
		
		# Use these to make the table, nRecBytes is used as a check
		sHash = self._mkTable(lWidths, nRecBytes, sOutDir, sBaseName)
		
		# Use these to make the label
		self._mkLabel(lWidths, lBytes, nRecBytes, sHash, sOutDir, sBaseName,
		              sIdxProdId)
		
		









