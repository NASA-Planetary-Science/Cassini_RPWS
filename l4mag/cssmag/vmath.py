'''Vector math utilities

All vector values returned are 3 element tuples (x,y,z)
All vector inputs are assumed to be 3 element sequences containing number
types.  Any items after the first three will be ignored.
'''

import math

def dot(va,vb):
	'''Standard vector dot product.  Returns a scalar'''
	return va[0]*vb[0] + va[1]*vb[1] + va[2]*vb[2]


def normalize(v):
	'''Returns a vector that points in the same direction as v but with
a magnitude of 1'''
	l = math.sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2])
	return (v[0]/l, v[1]/l, v[2]/l)


def foo():
	pass
