#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import optparse
import os
import os.path
import glob
import logging
import struct
import math
import pspice

from os.path import join as pjoin
from struct import unpack
from os.path import basename as bname

import das2 as D

##############################################################################
class WfrRecord (object):
	"""Little class to parse and wrap WFR records"""
		
	def __init__(self, sRec, sId):
		
		# The original un-parsed record
		self.sRec = sRec
		
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

		if uFlag & 0x20:
			#raise IOError("%s: is a WFR record"%sId)
			self.bIsWfr = True
		else:
			raise IOError("%s: is not a WFR record"%sId)
			self.bIsWfr = False

		if uFlag & 0x40:
			raise IOError("%s: is a WBR record"%sId)
			self.bIsWbr = True
		else:
			#raise IOError("%s: is not a WBR record"%sId)
			self.bIsWbr = False
			
		# Skipping Walsh Bit
		
		# Skipping Sub RTI bit
		
		# Skipping HFR-XLate
		
		# Skipping LP-DAC bit
		
		nStatus = ord(sRec[19])
		
		# Skipping AGC_ENABLE bit
		
		# Skipping Fine Time Quality bit
		
		# Skipping Timeout bit
		
		# Skipping Suspect Bit
		
		# Skipping H2 Down-converter usage bit
		
		# Skipping H1 Down-converter usage bit
		
		# Skipping Eu Current Bit
		
		# Skipping Ev Current Bit
		
		# Get sampling period in seconds
		
		self.nBand = ord(sRec[20])
		
		if self.nBand == 0:    # Band 0 is the low-band (100 Hz sample freq)
			self.rPeriod = 10e-3
			
		elif self.nBand == 1:  # Band 1 is the high-band (7.142 kHz sample freq)
			self.rPeriod = 140e-6
			
		else:
			raise IOError("%s: invalid WFR sampling period"%sId)
		
		nGain = ord(sRec[21])
		
		# Skipping Walsh Compression Factor
		
		# Analog gain
		self.nAnaGain = nGain & 0x7
		
		self.nAnt = ord(sRec[22])   # The current antenna, use function to 
		                            # convert to string
		
		self.nAGC = ord(sRec[23])   # Signal level that will be used to set 
		                      # next cycle's gain setting
			
		# Skipping HFR_XLATE value
		
		# Skipping SUB_RTI valuse
		
		# Skipping Voltage on Langmuir Probe sphere
		
		# Skipping Voltage on Langmuir Probe Cylinder
		
		# Skipping Flight Software Version
		
		#self.tSamples = tuple([ ord(c) for c in sRec[32:32+self.nSamples] ])
		#self.lSamples = [ ord(c) for c in sRec[32:32+self.nSamples] ]

		sfmt = ">%dH"%self.nSamples
		self.lSamples = unpack(sfmt,sRec[32:(32+self.nSamples*2)])
		
		self.lValues = None
		
		self.dtBeg = None
		
	_g_getAnt = ['Ex', 'Eu', 'Ev', 'Ew', 'Bx', 'By', 'Bz', None, 
	             'HF', None, None, 'LP', None, None, None, 'UNK' ]

	########################################################################
	# Getting simple values
		
	def antenna(self):
		"""Get a string indicating the antenna used to collect this waveform
		sample"""
		return self._g_getAnt[self.nAnt]
	
	def period(self):
		"""The timing between samples"""
		return self.rPeriod
		
	def gain(self):
		"""The gain value in dB.  WFR should range from 0 to 30 dB"""
		
		# Note: WBR data are not gained up digitally so Walsh flag doesn't
		#       matter
		return self.nAnaGain*10

	
	def data(self):
		"""A tuple containing uncalibrated waveform samples.  Field strength
		ranges from from 0 to 255.  Zero amplitude is nominally 127.5 with 127
		being just below and 128 just above zero amplitude.
		"""
		return self.lSamples
	
	###########################################################################
	# Getting calibrated UTC times
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
	
	###########################################################################
	# Getting calibrated samples 
	
	#    25Hz = idx 0,   2.5Khz  = idx 1 (same as self.nBand)
	_g_rSearchCoilFac = { 
		"Bx": (.0461, .1474), "By": (.0456, .1467), "Bz": (.0458, .1466) 
	}
		
	#				  25Hz  2.5Khz   
	_g_rCalFac = (9.63,   9.45)  # in dB
	
	# Geometric length, resonances not accounted for
	_g_rAntLen = {"Ex":9.26, "Ew":5.0}  
	
	
	def calibrate(self):
		"""Returns a tuple of calibrated waveform samples.  Proceedure used is
		documented at:  /opt/project/cassini/pds/DOCUMENT/WBRWFR/WBRWFR.TXT
		"""
		
		# Return pre-computed list if available
		if self.lValues:
			return self.lValues
					
		# -- Step a -- Find average value for the sensor's data set
		rRawAvg = float(math.fsum(self.lSamples)) / len(self.lSamples)
		
		self.lValues = [None] * len(self.lSamples)
		
		for i in xrange(0, len(self.lSamples)):
			# -- Step b -- Normalize about zero, subtract DC value from 
			#				all values of the data set
			self.lValues[i] = self.lSamples[i] - rRawAvg

			# -- Step c -- Apply the cal factor (dB)
			# -- Step d -- Gain amp already found in AllGains[][] (dB)
			# -- Step e -- Convert dB to linear scale			
			db_full_scale = self._g_rCalFac[self.nBand] + self.gain()
				
			linear_scale = math.pow(10.0, (db_full_scale/20.0)) / math.sqrt(2.0)

			# -- Step f -- Normalize by dividng by the maximum aplitude sine wave
			
			# Dividing by maximum amplitude sine wave for WFR
			self.lValues[i] /= 2047.5

			# After this step, we have the actual voltage measured by the receiver
			self.lValues[i] /= linear_scale	

			# -- Step g -- Convert to electric field strength (V/m)...
			if self.antenna() in ("Ex","Ew"):
				self.lValues[i] /= self._g_rAntLen[self.antenna()]
			
			# -- Step h -- ...or to magnetic field strength (nT)
			elif self.antenna() in ("Bx","By", "Bz"):
			
				# This is because there are amplifiers with a gain of 1/24 following
				# the search coils
				self.lValues[i] *= 24.0 
				self.lValues[i] /= self._g_rSearchCoilFac[self.antenna()][self.nBand]
	
		return self.lValues
				
##############################################################################
def readWfr(log, sFile):
	"""Read all the WFR records into a file and return list of record objects.
	The list is not necessarily sorted.
	"""
	
	log.info("Reading %s for WFR records"%sFile)
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
		
		# Only append Wfr Records
		uFlag = ord(sHdr[18])
		if uFlag & 0x20:
			sId = "%s, record %d"%(bname(sFile), len(lRecs))
			lRecs.append( WfrRecord(sRec, sId) )
			
	return lRecs
	

##############################################################################	
def findWfr(log, sDataDir, dtBeg, dtEnd, bLowBand, bHighBand):

	log.info("Finding files between %s and %s"%(dtBeg, dtEnd))
		
	lFiles = []
	
	# For each 24 hours in the coverage period, try to find files
	dt = D.DasTime(dtBeg.year(), dtBeg.month(), dtBeg.dom())
	
	lPtrns = []
	if bLowBand:
		lPtrns.append("T%4d%1dXX/T%4d%03d/T%4d%03d_25HZ*.DAT")

	if bHighBand:
		lPtrns.append("T%4d%1dXX/T%4d%03d/T%4d%03d_2_5KHZ*.DAT")
		
	while dt < dtEnd:
		for sPtrn in lPtrns:
			sGlob = pjoin(sDataDir, sPtrn % (
			              dt.year(), dt.doy()/100, dt.year(), dt.doy(),
			              dt.year(), dt.doy()
			             ))
			log.info("Checking time %s using pattern: %s"%(dt, sGlob))
			lFiles += glob.glob(sGlob)
					
		dt.adjust(0,0,1)  # Get next day
		
	return lFiles
