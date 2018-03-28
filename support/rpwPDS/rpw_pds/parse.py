"""PDS Parse, limited tools for PDS label parsing.
"""

import logging

############################################################################
def _parseErr(ret, log, sTmp, bRaise):
	log.error(sTmp)
	if bRaise:
		raise ValueError(sTmp)
	
	return ret


############################################################################
def _convertSet(sSet, log, sFile, nLine, bRaise):
	"""Convert PDS set to a python list.
	
		PDS set format is { token, token, token } where the whitespace
		between tokens and be space, \r\n.
		
		The token format is any non space character, unless enclosed in a quote
	"""
	
	#Go through the string making tokens, then interperate the tokens
	
	# Tokens are started by any non white space character
	
	# Tokens are terminated by a white space character, or by ',' and '}',
	# unless started by a quote and then they must be terminated by a quote.
	
	# the characters {}, are stand alone tokens
	
	lRet = []
	lTok = []
	bInTok = False
	bQuoteTok = False
	
	for c in sSet:
		
		if not bInTok:
			if c.isspace():
				continue
				
			if c in '{},':  #Add as complete token
				lTok.append(c) 
				continue
			
			#Starting a token
			lTok.append(c)
			bInTok = True
			if c == '"':
				bQuoteTok = True
			continue
			
		else:  
		
			# Always add token if in quote mode
			if bQuoteTok:
				lTok[-1] += c
				
				if c == '"':
					bQuoteTok = False
				continue	
			
			if c.isspace():
				bInTok = False
				continue
				
			if c in '{},':
				bInTok = False
				lTok.append(c)
				continue
				
			lTok[-1] += c
				
	
	# Okay, make sure tokens make a meaningful "sentence". 
	# ie: { item, item, item, item }
	sPre = "%s, %i:"%(sFile, nLine)
	bR = bRaise
	
	if len(lTok) < 2:
		return _parseErr(lRet, log, "%s Too few items in set\n %s"%(sPre, sSet), bR)
		
	if bQuoteTok:
		return _parseErr(lRet, log, "%s unclosed quote in set\n %s."%(sPre, sSet), bR)
		
	if lTok[0] != '{' or lTok[-1] != '}':
		return _parseErr(lRet, log, "%s bracket missing in set\n %s."%(sPre, sSet), bR)
	
	bExComma = True
	for i in range(1,len(lTok)-1):
		bExComma = not bExComma
		
		if lTok[i] == ',':
			if not bExComma:
				return _parseErr(lRet, log, 
				                 "%s unexpected comma in set\n %s."%(sPre, sSet), bR)
		else:
			if bExComma:
				return _parseErr(lRet, log,
				                 "%s missing comma in set\n %s."%(sPre, sSet), bR)
			lRet.append(lTok[i])	
		
	return lRet
	
############################################################################

def extractFromRoot(sInPath, lKeys=[], nMissingDis=3, bRaiseError=True):
	"""Extract root fields from a PDS label
	
	   All data is returned as strings unless it is multi-values, then it
		is returned as list of strings.
	
		sInPath - Input label file, if doesn't end in .LBL -> ValueError
		
		lKeys   - The list of keys to extract, if the empty list [] then
		   all root keys up to the 1st object definition are extracted.
		
		nMissingDis - Short for 'Missing Disposition', what to do in the
		    case that certian values couldn't be found in the set. 
			 0 - No big deal
			 1 - warning
			 2 - error
			 3 - error and exception
		
		bRaiseParse - By default raise an exception on all parse errors.
		
	   Returns: a dictionary with one key for each given element.  Note
		   if bRaiseMissing is False, some of the values may be None.
			also, if bRaiseErrror is False the dict is returned in what ever
			state it has when the error is encountered.
	"""
	
	log = logging.getLogger('extractRootFields')
	
	#Go line by line until all keys are found.
	
	#if not sInPath.endswith('.LBL'):
	#	raise ValueError("extractRootFields: PDS label '%s' doesn't end in .LBL"%sInPath)
	
	#Init the dict
	dRet = {}
	if lKeys != []:
		for sKey in lKeys:
			dRet[sKey] = None
	
	fIn = file(sInPath, 'rb')
	sRawLine = "not in file"
	
	sSetKey = None     # Name of key if in multi-value mode
	sTextKey = None      # Name of key if in text mode
	
	sAccum = None       # Value accumulator for multiline or multi-value modes
	nLine = 0L
	while len(sRawLine) > 0:
		sRawLine = fIn.readline() #Save rawline away for test condition above
		
		sLine = sRawLine
		nLine += 1
		
		
		# Continue accumulation and possible termination of text mode
		if sTextKey:
			sAccum += sLine
			
			# Check for getting out of text mode
			if sLine.find('"') > -1:
				if lKeys == [] or sTextKey in lKeys:
					dRet[sTextKey] = sAccum
				sTextKey = None
			continue
		
		
		# handle blanks and comments
		i = sLine.find('/*') 
		if i > -1:
			sLine = sLine[:i] 
			
		#sLine = sLine.strip()
		
		if len(sLine.strip()) < 1:
			continue
		
		
		# Continue accumulation and possible termination of multi-value mode
		if sSetKey:
			sAccum += sLine.strip()  # Accumulate stripped line
			
			#Conver to list and go back to single line mode.
			if sLine.find('}') > -1:
				if lKeys == [] or sSetKey in lKeys:
					dRet[sSetKey] = _convertSet(sAccum, log, sInPath, nLine, bRaiseError)
				sSetKey = None	
			continue
		
		# Standard situation
		
		#Find Key
		i = sLine.find('=')
		if i == -1:
			# Not in multi line, or object mode, and this ain't a comment but
			# we can't find the =.
			sTmp = "%s, %i: "%(sInPath, nLine)
			sTmp += "syntax error, no '=' in a non-comment, not-multivalue line.\n"
			sTmp += sLine
			return _parseErr(dRet, log, sTmp, bRaiseError)
			
				
		if i == 0:
			sTmp = "%s, %i: syntax error, '=' sign at beginning of line."%(
			          sInPath, nLine)
			return _parseErr(dRet, log, sTmp, bRaiseError)
			
		sKey = sLine[:i].strip()
		sVal = sLine[i+1:].lstrip() #Can't strip both sides yet, because of text vals.
		sValStrip = sVal.rstrip()
		
		if sKey == 'OBJECT':
			log.debug('%s, %i: Parse ending at first OBJECT encountered.'%(sInPath, nLine))
			break
			
		if len(sValStrip) < 1:
			sTmp = "%s, %i: syntax error, no value after '='."%(sInPath, nLine)
			return _parseErr(dRet, log, sTmp, bRaiseError)
		
		
		# If the first char is { then I might be going to multi-value mode
		if sValStrip[0] == '{':
			
			if sValStrip[-1] == '}':
				
				if lKeys == [] or sKey in lKeys:  # All values on one line.
					dRet[sKey] = _convertSet(sValStrip, log, sInPath, nLine, bRaiseError)
			else:
				sSetKey = sKey
				sAccum = sValStrip  # Accumulate as string for now.

			continue
		
		
		# If the first char is " then I might be going to text mode
		if sValStrip[0] == '"':
		
			#Only all in one line if also have closing " (which can't have unless
			#val is at least two chars long after stripping
			if sValStrip[-1] == '"' and len(sValStrip) > 1:
				
				if lKeys == [] or sKey in lKeys: # All text on one line.
					dRet[sKey] = sValStrip
			else:
				sTextKey = sKey
				sAccum = sVal  # <--Using NO strip version! Accumulate as string for now.
		
			continue
		
		# Just a simple value
		if lKeys == [] or sKey in lKeys:
			dRet[sKey] = sValStrip
	
	fIn.close()
	
	# If I ended in text or multi mode then that's an error
	if sSetKey:
		sTmp = "%s, %i: Unclosed set."%(sInPath, nLine)
		return _parseErr(dRet, log, sTmp, bRaiseError)
	if sTextKey:
		sTmp = "%s, %i: Unclosed text value."%(sInPath, nLine)
		return _parseErr(dRet, log, sTmp, bRaiseError)
			
	
	# Check for empty keys, if the caller cares.
	if nMissingDis > 0 and lKeys != []:
		lEmpty = []
		for sKey,sVal in dRet.iteritems():
			if sVal == None:
				lEmpty.append(sKey)
	
		if len(lEmpty) > 0:
		
			sTmp = "%s: Could not find values for keys: %s "%(sInPath, lEmpty)
			if nMissingDis == 1:
				log.warning(sTmp)
			else:
				return _parseErr(dRet, log, sTmp, (nMissingDis == 3))
				

	# Well, have keys, parsed file
	return dRet


###############################################################################
def _genParseCheckPatterns(lMatch, log):

	lStartPat = []
	lEndPat   = []
	lFunc     = []
	lMax      = []
	nLongestEnd = 1
	nPats     = 0
	
	for i in range(0, len(lMatch)):
		sStart = lMatch[i][0].lower()
		sEnd = lMatch[i][1].lower()
		if len(lMatch[i]) > 3:
			lMax.append( int(lMatch[i][3]) )
		else:
			lMax.append(-1)
		
		if len(sStart) < 1:
			sTmp = "Empty pattern for match list item %i ('%s','%s')"%(i,sStart,sEnd)
			log.error(sTmp)
			raise ValueError(sTmp)
		
		if len(sEnd) > nLongestEnd:
			nLongestEnd = len(sEnd)
		
		if sStart in lStartPat:
			sTmp = "Non-unique start pattern %s in list (note matching is "
			sTmp += "case insensitive)."
			log.error(sTmp)
			raise ValueError(sTmp)
			
		for sPat in lStartPat:
			if sPat.find(sStart) == 0 or sStart.find(sPat) == 0:
				sTmp = "Starting character sequence for patterns '%s' "%sPat
				sTmp += "and '%s' overlap."%sStart
				log.error(sTmp)
				raise ValueError(sTmp)
		
		lStartPat.append(sStart)
		lEndPat.append(lMatch[i][1].lower())
		lFunc.append(lMatch[i][2])
		nPats += 1
	
	return (lStartPat, lEndPat, lFunc, lMax, nPats, nLongestEnd)


class readAhead:
	"""A class to provide read ahead functionality for character by character
	file reading.  
	"""
	def __init__(self, fIn, nAhead=10):
		self.nAhead = nAhead
		self.sBuf = ""
		self.fIn = fIn
		if nAhead < 0:
			raise ValueError('readAhead.__init__, nAhead should be >= 0')
	
	def readChar(self):
		sBk = ""                                           # Figure Keep
		if len(self.sBuf) > 1:
			sBk = self.sBuf[1:]
		
		sFill = self.fIn.read((1+self.nAhead) - len(sBk))  # Do Fill
		self.sBuf = "%s%s"%(sBk, sFill)
		
		if len(self.sBuf) < 1:                             # Figure Return
			return ('','')
		elif len(self.sBuf) == 1:
			return (self.sBuf, '')
		else:
			return (self.sBuf[0], self.sBuf[1:])
		
		 
		
		
	
def genericTextProc(fIn, fOut, lMatch, nAhead=0):
	"""Generic text processor, find text chunks and exec functions on them.
	
		fIn - File input object
		
		fOut - File output object
		
		lMatch - list of tuples that are of the format
		   
			(START_STR, [END_STR|''], PROC_FUNC[, MAX_CHARS])
			
		   PROC_FUNC must have the prototype:
			
			   string = PROC_FUNC(sString, sLookAhead)     (returns string)
		
		nAhead - a number of characters after the match to supply to the look
		   ahead string for the PROC_FUNC's.  This is a max number but the
			parser may supply less.
		
		The function sends input text to the output unless a match is found, in
		that case the text is accumulated until the end pattern is reached and
		then the matching processing function is called.  If the end pattern	is
		the empty string, '', then the processing function is called as soon as
		the start pattern has accumulated.  The output of the processing function
		is sent to the output instead of that particular text chunk.  Note,
		MAX_CHARS is optional.
	
		Regular expressions or other patterns are not yet supported in the
		start and stop string.  All matching is currently case insensitive.
		
		Returns:  (lPtrnsFound, nBytesOutput)  where:
		
		   lPtrnsFound  - a list of matches per pattern with the list index
			is the same as the input index for lMatch
			
			nBytesOutput - the number bytes sent to the output file object. 
	"""
	# This is intentially not an optimized parser.  For example I could
	# create a vector if 1st, 2nd, 3rd match characters and cut down on
	# all the looping.  I'm not going to worry about it. --cwp
	
	log = logging.getLogger('genericTextProc')
	
	
	# Check that pattern starts are unique, while creating lowcase version
	# of patterns.
	(lStartPat, lEndPat, lFunc, lMax, nPats, nLongestEnd) = \
	                                        _genParseCheckPatterns(lMatch, log)
	lPtrnsFound = []
	for i in range(0,nPats):
		lPtrnsFound.append(0)
	
	sTagAcc = ""       # Holds accumulating text while looking for end pattern.
	iTagLtr = -1        # Current letter in start pattern.
	
	sTxtAcc = ""       # Holds accumulating text while search for end pattern.
	iCurPat = -1       # If parsing a pattern, which one
	
	vPossTags = []      # List of still possible start patterns
	
	nBytesWritten = 0L
	cCur = ' '
	reader = readAhead(fIn, nAhead)
	while cCur != "":
		(cCur, sAhead) = reader.readChar()
		cLow = cCur.lower()
		
		#Print status for each character
		#print 'cCur:',cCur,' sTagAcc:',sTagAcc,' iTagLtr:',iTagLtr,' sTxtAcc:',sTxtAcc,' iCurPat:',iCurPat,' vPossTags:',vPossTags
		
		
		if sTagAcc == "":               # Try to start acum'ing start tag.
			for iPtn in range(0, nPats):
				if cLow == lStartPat[iPtn][0]:
					vPossTags.append(iPtn)
			
			if len(vPossTags) > 0:     # Yep, start accumulating tag
				sTagAcc += cCur
				iTagLtr = 0
				
			else:                        # Nope, just output
				fOut.write(cCur)
				nBytesWritten += 1
		
		else:                           
			
			if iCurPat < 0:              # Not set on a pattern yet
				iTagLtr += 1
			
				vTmp = []
				for iPoss in range(0, len(vPossTags)):
					iPtn = vPossTags[iPoss]
				
					if len(lStartPat[ iPtn ]) > iTagLtr:
						if cLow == lStartPat[ iPtn ][ iTagLtr ]:		
							vTmp.append( iPtn )
					#else:
						
						# I must have run out of pattern and all matched, done
						#vTmp.append(iPtn)
							
				vPossTags = vTmp
				
				#print 'cCur:',cCur,' sTagAcc:',sTagAcc,' iTagLtr:',iTagLtr,' sTxtAcc:',sTxtAcc,' iCurPat:',iCurPat,' vPossTags:',vPossTags
				
				if len(vPossTags) == 0: 
					# No more start patterns matched, dump
					sOut = "%s%s"%(sTagAcc, cCur)
					fOut.write(sOut)
					nBytesWritten += len(sOut)
					sTagAcc = ""
					iTagLtr = -1
					
				elif len(vPossTags) == 1:
					# Only one tag matches, but we might not have read the whole tag
					sTagAcc += cCur
					iPtn = vPossTags[0]
					if len(lStartPat[iPtn]) == (iTagLtr+1):
					
						# I've locked on to a tag, clear the pattern search vars
						vPossTags = []
						iTagLtr = -1
						sTxtAcc = ""  #Reset text acc

						# See if pattern is only a start not an end
						if lEndPat[iPtn] == '':
							sOut = lFunc[iPtn](sTagAcc, sAhead)
							fOut.write(sOut)
							nBytesWritten += len(sOut)
							sTagAcc = ''				
						else:
							iCurPat = iPtn
						
				else:
					# Default, keep accumulating tag.
					sTagAcc += cCur
				
			else:                        # I have tag lock, try to end pattern.
				
				sTxtAcc += cCur
				if lMax[iCurPat] > -1 and len(sTxtAcc) > lMax[iCurPat]:
					sTmp = "Max text length exceeded while looking for an end to pattern"
					sTmp += " ('%s','%s')."%(lStartPat[iCurPat],lEndPat[iCurPat])
					log.error(sTmp)
					raise ValueError(sTmp)
				
				sTmp = sTxtAcc[-nLongestEnd:].lower()
				#print "Trying to finish, sTxtAcc[:-nLongestEnd]:", sTmp
				if sTmp.find(lEndPat[iCurPat]) > -1:
					
					sOut = lFunc[iCurPat]("%s%s"%(sTagAcc, sTxtAcc), sAhead)
					fOut.write(sOut)
					#print "Flushing pattern"
					nBytesWritten += len(sOut)
					iCurPat = -1
					sTxtAcc = ""
					sTagAcc = ""
					iTagLtr = -1
					lPtrnsFound[iCurPat] += 1
	
		#print 'cCur:',cCur,' sTagAcc:',sTagAcc,' iTagLtr:',iTagLtr,' sTxtAcc:',sTxtAcc,' iCurPat:',iCurPat,' vPossTags:',vPossTags
		#print
		
	# check to see if ended in the middle of a match
	if len(sTxtAcc) > 0:	
		sTmp = "Input file ended in the middle of pattern ('%s','%s')."%(
		                lStartPat[iCurPat],lEndPat[iCurPat])
		log.error(sTmp)
		raise ValueError(sTmp)
		
	return (lPtrnsFound, nBytesWritten)


###############################################################################
# Test set for the general tag parser

#def test():
	


