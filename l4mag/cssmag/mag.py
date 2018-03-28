import os
import cssdas
from bisect import bisect_left, bisect_right
from collections import namedtuple

NAME_FORMAT = 'MAG__SC___%04d%03d_%c.TAB'

MagRecord = namedtuple('MagRecord', 'dTime x y z m')

def latestVersion(l,s):
	if l and l[-1][:-6] == s[:-6]:
		l[-1] = s
	else:
		l.append(s)
	return l

def magData(sPath,dStart,dEnd):
	lDataFiles = listMagFiles(sPath,dStart,dEnd)
	lRecords = []

	foundStart = False
	previous = None
	
	for sFile in lDataFiles:
		for sLine in open(os.path.join(sPath,sFile)):
			lItems = sLine.split()
			if len(lItems) == 5:	
				dTime = das.ttime(das.parsetime(lItems[0]))
				bx = float(lItems[1])
				by = float(lItems[2])
				bz = float(lItems[3])
				bm = float(lItems[4])
				if dTime < dStart:
					previous = MagRecord(dTime, bx, by, bz, bm)
					continue
				if not lRecords and previous:
					lRecords.append(previous)
				lRecords.append(MagRecord(dTime, bx, by, bz, bm))
				
				if dTime >= dEnd:
					break
	return lRecords
				
				
def magInterp(dTime, lData):
	if dTime < lData[0][0] or dTime > lData[-1][0]:
		raise ValueError('dTime not in range %f [%f, %f]'%(dTime,lData[0][0],lData[-1][0]))

	lo = 0
	hi = len(lData)

	while lo < hi:
		mid = (lo+hi)//2
		if lData[mid][0] < dTime: lo = mid+1
		else: hi = mid

	if lData[lo][0] == dTime:
		return lData[lo]

	mag0 = lData[lo-1]
	mag1 = lData[lo]

	t = (dTime-mag0.dTime)/(mag1.dTime-mag0.dTime)

	bx = t*(mag1.x-mag0.x) + mag0.x
	by = t*(mag1.y-mag0.y) + mag0.y
	bz = t*(mag1.z-mag0.z) + mag0.z
	bm = t*(mag1.m-mag0.m) + mag0.m

	return bx, by, bz, bm

def listMagFiles(sPath,dStart,dEnd):
	lDataFiles = os.listdir(sPath)
	
	lStart = das.emitt(dStart)
	lEnd = das.emitt(dEnd)

	sStartFile = NAME_FORMAT%(lStart[0],lStart[3],'0')
	sEndFile = NAME_FORMAT%(lEnd[0],lEnd[3],'_')

	lDataFiles = [item for item in lDataFiles if item.endswith('.TAB')]

	lDataFiles.sort()

	lSelectedFiles = lDataFiles[
		bisect_left(lDataFiles,sStartFile):bisect_right(lDataFiles,sEndFile)]

	lSelectedFiles = reduce(latestVersion,lSelectedFiles,[])

	return lSelectedFiles


def writeToFile(lRecords,sPath):

	out = open(sPath,'w')
	for d,x,y,z,m in lRecords:
		out.write('%s %f %f %f %f\n'%
			(das.formattime(d),x,y,z,m))
	out.close()

if __name__ == '__main__':
	print 'creating test files'
	dStart = das.parsetime('2011-02-26T12:00')
	dEnd = das.parsetime('2011-02-26T13:00')

	data = magData('/opt/project/cassini/mag/SC',dStart,dEnd)

	foo(data,'minute.txt')

	secData = [ ( dTime, magInterp(dTime,data) ) for dTime in range(int(dStart)+1,int(dEnd)-1) ]

	print secData[0]

	foo(secData,'second.txt')
