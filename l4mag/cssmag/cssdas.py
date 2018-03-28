import das2
from collections import namedtuple

DasTime = namedtuple('DasTime', 'year month mday yday hour min sec')

def parsetime(sTime):
	'''Returns a DasTime namedtuple representing the parsed time'''
	return DasTime(*das2.parsetime(sTime))

def ttime(dasTime):
	t=dasTime
	return das2.ttime(t.year, t.month, t.mday, t.hour, t.min, t.sec)

def tnorm(dasTime):
	t = dasTime
	return DasTime(*das2.tnorm(t.year,t.month,t.mday,t.hour,t.min,t.sec))

def emitt(dTime):
	return DasTime(*das2.emitt(dTime))

def formattime(dTime):
	dasTime = emitt(dTime)
	sTimeFormat='{0:04d}-{1:02d}-{2:02d}T{4:02d}:{5:02d}:{6:06.3f}'
	return sTimeFormat.format(*dasTime)


