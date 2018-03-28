import sys
import optparse
import os
import os.path
import glob
import logging
import struct
import math

from os.path import join as pjoin
from struct import unpack
from os.path import basename as bname

import pspice

import das2 as D

##############################################################################
#  Table found in /opt/project/cassini/pds/DOCUMENT/RPWSUG/RPWSUG.PDF on pages 
#  285-287
#
#  "In general, odd indicies indicate HFR/H1 is selected with frequency in 
#   25 KHz steps, and even indicies indicate HFR/H2 is selected with frequency
#   in 50 Khz steps with an offset of 4.025 Mhz"
#
#	- RPWS_WBR_WFR_ROW_PREFIX.FMT in /opt/project/cassini/pds/LABEL
#
#  Values below in units of Hz
##############################################################################

_g_HFtable = [
	 4025000,    None,  4125000,    None,  4225000,  125000,  4225000,  175000,  
	 4425000,  225000,  4525000,  275000,  4625000,  325000,  4725000,  375000,  
	 4825000,  425000,  4925000,  475000,  5025000,  525000,  5125000,  575000,  
	 5225000,  625000,  5325000,  675000,  5425000,  725000,	 5525000,  775000,  
	 5625000,  825000,  5725000,  875000,  5825000,  925000,  5925000,  975000, 
	 6025000, 1025000,  6125000, 1075000,  6225000, 1125000,  6325000, 1175000,  
	 6425000, 1225000,  6525000, 1275000,  6625000, 1325000,  6725000, 1375000,
	 6825000, 1425000,  6925000, 1475000,  7025000, 1525000,  7125000, 1575000,
	 7225000, 1625000,  7325000, 1675000,  7425000, 1725000,  7525000, 1775000,
	 7625000, 1825000,  7725000, 1875000,  7825000, 1925000,  7925000, 1975000, 
	 8025000, 2025000,  8125000, 2075000,  8225000, 2125000,  8325000, 2175000,
	 8425000, 2225000,  8525000, 2275000,  8625000, 2325000,  8725000, 2375000,
	 8825000, 2425000,  8925000, 2475000,  9025000, 2525000,  9125000, 2575000,
	 9225000, 2625000,  9325000, 2675000,  9425000, 2725000,  9525000, 2775000,
	 9625000, 2825000,  9725000, 2875000,  9825000, 2925000,  9925000, 2975000, 
	10025000, 3025000, 10125000, 3075000, 10225000, 3125000, 10325000, 3175000,
	15225000,    None, 15325000,    None, 15425000,    None, 15525000,    None,
	15625000,    None, 15725000,    None, 15825000,    None, 15925000,    None,
	16025000,    None,     None,    None,     None,    None,     None,    None,
	    None,    None,     None,    None,     None,    None,     None,    None 
]


##############################################################################
class WbrRecord (object):
	"""Little class to parse and wrap WBR records"""
		
	def __init__(self, sRec, sId):
		
		# The original un-parsed record
		self.sRec = sRec
		
		self.dtBeg = None
		(self.nSclkSec,) = unpack(">I", sRec[0:4])
		self.nSclkPart = ord(sRec[4])
		self.nSclkFine = ord(sRec[5])
		
		(self.nScetDay, self.nScetMilli) = unpack(">HI", sRec[6:12])
		
		(self.nLen, self.nSamples, self.nRti) = unpack(">HHH", sRec[12:18])
	 
		uFlag = ord(sRec[18])
		if uFlag & 0x80: 
			self.bMsf = True
		else:
			self.bMsf = False
			
		if uFlag & 0x40:
			self.bIsWbr = True
		else:
			raise IOError("%s: is not a WBR record"%sId)
			self.bIsWbr = False
			
		if uFlag & 0x20:
			raise IOError("%s: is a WFR record"%sId)
			self.bIsWfr = True
		else:
			self.bIsWfr = False
			
		# Skipping Walsh Bit
			 
		
		# Skipping HFR-XLate
		
		# Skipping LP-DAC bit
		
		nStatus = ord(sRec[19])
		
		# Skipping AGC_ENABLE bit		
		
		# Skipping Timeout bit
		self.bSuspect = False
		if nStatus & 0x20:   
			self.bSuspect = True  # Timeout condition occured
			
		# Skipping Suspect Bit
		if nStatus & 0x10:
			self.bSuspect = True  # Ground software things packet is a problem
			
		
		self.bHFR = False
		self.rHfrBaseFreq = None
		# H2 Down-converter usage bit
		if nStatus & 0x08:
			self.bHFR = True
		
		# H1 Down-converter usage bit
		if nStatus & 0x04:
			self.bHFR = True
		
		# Skipping Eu Current Bit
		
		# Skipping Ev Current Bit
		
		# Get sampling period in seconds
		
		self.nBand = ord(sRec[20])
		if self.nBand == 2:
			self.rPeriod = 3.6e-5
		elif self.nBand == 3:
			self.rPeriod = 4.5e-6
		else:
			raise IOError("%s: invalid WBR sampling period"%sId)
		
		nGain = ord(sRec[21])
		
		# Skipping Walsh Compression Factor
		
		# Analog gain
		self.nAnaGain = nGain & 0x7
		
		self.nAnt = ord(sRec[22])   # The current antenna, use function to 
		                      # convert to string
		
		
		self.nAGC = ord(sRec[23])   # Signal level that will be used to set 
		                      # next cycle's gain setting
			
		# HFR_XLATE value, Translation Frequency when HFR is slected as a 
		# signal source
		self.nHFR = None
		self.rHfrBaseFreq = None
		if self.bHFR:
			self.nHFR = ord(sRec[24])
			self.rHfrBaseFreq = _g_HFtable[self.nHFR]
		
		# Handle Sub-RTI timing, only present if MSF = True, and
		# sub-RTI flag bit is turned on
		if self.bMsf and (uFlag &  0x04):
			nMilliSec = ord(sRec[25])
			self.nScetMilli += nMilliSec
			if self.nScetMilli >= 86400000:
				self.nScetDay += 1
				self.nScetMilli -= 86400000
			
			self.nSclkFine += (nMilliSec * 256)/1000
			if self.nSclkFine >= 256:
				self.nSclkSec += 1
				self.nSclkFine -= 256
		
		# Skipping Voltage on Langmuir Probe sphere
		
		# Skipping Voltage on Langmuir Probe Cylinder
		
		# Skipping Flight Software Version
		
		#self.tSamples = tuple([ ord(c) for c in sRec[32:32+self.nSamples] ])
		self.lSamples = [ ord(c) for c in sRec[32:32+self.nSamples] ]
		
		# Calibrated values:
		self.lValues = None
		
		
	_g_getAnt = ['Ex', 'Eu', 'Ev', 'Ew', 'Bx', 'By', 'Bz', None, 
	             'HF', None, None, 'LP', None, None, None, 'UNK' ]
	
	def suspect(self):
		"""Returns true if ground processing detected an issue with this
		waveform or if a timeout condition occured within the instrument
		either way it's best to discard the packet"""
		return self.bSuspect
	
	
	def mixingFreq(self):
		""" Get Translation Frequency index when HFR is selected as a signal source
		Returns None otherwise
		"""
		return self.rHfrBaseFreq
		
	def antenna(self):
		"""Get a string indicating the antenna used to collect this waveform
		sample"""
		
		# Since Willy ignored Bill's directive to make sure we are using the
		# PDS High-Rate data for day to day work it was never discovered that
		# the antenna value for HFR data is *DROPPED* and never makes it to
		# the PDS products.  Great work, as usual.  Just assume Ex and hope.
		
		if self._g_getAnt[self.nAnt] == "HF":
			return 'Ex'
		else:
			return self._g_getAnt[self.nAnt]
		
		
	def begin(self):
		"""Returns the data collection initiation time of the first sample
		in the record as a Das2 Time object.  Code assumes you've called
		pspice.furnsh()
		"""
		
		if not self.dtBeg:
			# Formats time from records (UTC)
			sSclk = "%d/%d:%d"%(self.nSclkPart, self.nSclkSec, self.nSclkFine)
			et = pspice.scs2e(-82, sSclk)
			sScet = pspice.et2utc(et, "ISOC", 6)
			self.dtBeg = D.DasTime(sScet)	
		
		return self.dtBeg
	
	def data(self):
		"""A tuple containing uncalibrated waveform samples.  Field strength
		ranges from from 0 to 255.  Zero amplitude is nominally 127.5 with 127
		being just below and 128 just above zero amplitude.
		"""
		return self.lSamples
	
	def period(self):
		"""The timing between samples"""
		return self.rPeriod
		
	def gain(self):
		"""The gain value in dB. Should range from 40 to 70 dB"""
		
		# Note: WBR data are not gained up digitally so Walsh flag doesn't
		#       matter
		return self.nAnaGain*10
		
	
	###########################################################################
	# Getting Calibrated values
	
	#                10Khz 75Khz 
	_g_rCalFactor = (6.33, 6.43)	# in dB
	
									
	#    10KHz = idx 0,   80Khz  = idx 1 (i.e. self.nBand - 2)
	_g_rCoilFac = { 
		"Bx":( 0.1325, 1.0), "By":( 1.0, 1.0), "Bz":(1.0, 1.0)
	}
	
	# Geometric length, resonances not accounted for
	_g_rAntLen = {"Ex":9.26, "Ew":5.0}  

	
	def calibrate(self):
		"""Returns a tuple of calibrated waveform samples.  Proceedure used is
		documented at:  /opt/project/cassini/pds/DOCUMENT/WBRWFR/WBRWFR.TXT
		"""
		
		if self.lValues:         # return pre-computed list if available
			return self.lValues		
	
		# -- Step a -- Find average value for the sensor's data set
		rRawAvg = float( math.fsum(self.lSamples)) / len(self.lSamples)
		
		
		# -- Step c -- Apply the calibration factor (dB)
		# -- Step d -- Gain amp setting, already found in Gain[] (dB)
		# -- Step e -- convert dB to linear scale   
		db_full_scale = self._g_rCalFactor[self.nBand - 2] + self.gain()
		linear_scale = math.pow(10.0, (db_full_scale/20.0)) / math.sqrt(2.0)
		
		
		# -- Step g -- Convert to electric field strength (V/m)
		if self.antenna() in ("Ex", "Ew"):				
			rCalFactor = 1.0 / self._g_rAntLen[self.antenna()]
				
		# -- Step h -- Convert to magnetic field strength
		elif self.antenna() in ("Bx", "By", "Bz"):
				
			# This is because there are amplifiers with a gain of 1/24 following
			# the search coils
			rCalFactor  = 24.0 / self._g_rCoilFac[self.antenna()][self.nBand-2]
		
		# -- Step f -- Normalize by dividng by the maximum amp WBR sine wave
		# After this step, we have the actual voltage measured by the receiver
		rCalFactor /= (127.5 * linear_scale)

		# -- Step b -- Normalize about zero, subtract DC value from 
		#              all values of the data set
		self.lValues = [ (n - rRawAvg)*rCalFactor for n in self.lSamples]
				
		return self.lValues
		
		
##############################################################################
def readWbr(log, sFile):
	"""Read all the WBR records into a file and return list of record objects.
	The list is not necessarily sorted.
	"""
	
	log.info("Reading %s for WBR records"%sFile)
	fIn = file(sFile, 'rb')
	
	lRecs = []
	
	while True:
	
		sHdr = fIn.read(32)
		if len(sHdr) != 32:
			break
		
		(nLen,) = unpack(">H", sHdr[12:14])
		
		sBody = fIn.read(nLen - 32)
		if len(sBody) != (nLen - 32):
			break
			
		sRec = sHdr + sBody
		
		# Only append Wbr Records
		uFlag = ord(sHdr[18])
		if uFlag  & 0x40:
			sId = "%s, record %d"%(bname(sFile), len(lRecs))
			lRecs.append( WbrRecord(sRec, sId) )
			
	return lRecs
	
##############################################################################
def findWbr(log, sDataDir, dtBeg, dtEnd, b10khz, b80khz, bHFR):
		
	lFiles = []
	
	# For each hour in the coverage period, try to find files
	dt = D.DasTime(dtBeg.year(), dtBeg.month(), dtBeg.dom(), dtBeg.hour())
	
	lPtrns = []
	if b10khz:
		lPtrns.append( "T%4d%1dXX/T%4d%03d/T%4d%03d_%02d_10KHZ*.DAT")
	elif b80khz:
		lPtrns.append( "T%4d%1dXX/T%4d%03d/T%4d%03d_%02d_75KHZ*.DAT")
	elif bHFR:
		lPtrns.append( "T%4d%1dXX/T%4d%03d/T%4d%03d_%02d_???*KHZ*.DAT")
	
		
	while dt < dtEnd:
		for sPtrn in lPtrns:
			sGlob = pjoin(sDataDir, sPtrn % (
			              dt.year(), dt.doy()/100, dt.year(),dt.doy(),dt.year(),
							  dt.doy(),dt.hour())
							 )
			log.debug(" Searching for files matching: %s"%sGlob)
			lFiles += glob.glob(sGlob)
			
		dt.adjust(0,0,0,1)
	
	log.debug("%d files found"%len(lFiles))
	return lFiles
	
##############################################################################	
