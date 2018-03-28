'''Values and methods common to all RPWS instruments

unitEx
unitEu
unitEv
unitEw
   Unit vector directions for each of the four electric antennae.
   The direction of the dipole antenna Ex is along the x axis and
   the directions for the other antennas is computed from physical
   angles specified in the intstrument documentation.

   Physical orientations of the electric monopole antennas:
   Antenna    theta (degrees) phi (degrees)
       Eu        107.5           24.8
       Ev        107.5          155.2
       Ew         37.0           90.0

   The angle theta is the polar angle measured from the spacecraft +z
   axis.  The angle phi is the azimuthal angle, measured from the
   spacecraft +x axis.
'''

import math
import vmath
import mag
from collections import namedtuple


unitEx = (1.0,0.0,0.0)



unitEu = ( math.sin(math.radians(107.5)) * math.cos(math.radians(24.8)),
		   math.sin(math.radians(107.5)) * math.sin(math.radians(24.8)),
		   math.cos(math.radians(107.5)) )



unitEv = ( math.sin(math.radians(107.5)) * math.cos(math.radians(155.2)),
		   math.sin(math.radians(107.5)) * math.sin(math.radians(155.2)),
		   math.cos(math.radians(107.5)) )



unitEw = ( math.sin(math.radians(37.0)) * math.cos(math.radians(90.0)),
		   math.sin(math.radians(37.0)) * math.sin(math.radians(90.0)),
		   math.cos(math.radians(37.0)) )

MetaData = namedtuple('MetaData',
	'bad_data hfr_sounder lp_raw_sweep antenna')

RpwsRecord = namedtuple('RpwsRecord', 'time metaData data')

McRpwsRecord = namedtuple('McRpwsRecord',
						  'time angle parallel perpendicular')

def magParallelPerpendicular(lMagVector, lRecords):
	'''Compute the components of the data values that are paralellel and
       perpendicular to magnetic field vector.
       '''
	
	lResult = []
	for dTime, metadata, lData in lRecords:
		iAntenna = metadata.antenna
		if iAntenna == 0:
			vAnt = unitEx
		elif iAntenna == 1:
			vAnt = unitEu
		elif iAntenna == 2:
			vAnt= unitEv
		elif iAntenna == 3:
			vAnt = unitEw
		else:
			raise ValueError('%d is not a valid electric antenna'%iAntenna)

		vMag = mag.magInterp(dTime, lMagVector)
		vMag = vmath.normalize(vMag)
		dCosine = vmath.dot(vMag, vAnt)
		dAngle = math.acos(dCosine)
		lParallel = []
		lPerpendicular = []
		for d in lData:
			dParallel = d * dCosine * dCosine
			lParallel.append(dParallel)
			lPerpendicular.append(d-dParallel)
		lResult.append(McRpwsRecord(dTime, dAngle, lParallel, lPerpendicular))

	return lResult


