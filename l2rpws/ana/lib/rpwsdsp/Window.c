#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "rpwsdsp.h"

long WindowFunction_f(long length,float *Tmprl,long type)
{
/* Create a window function of type.
   Nsamp coefficients are returned in the Tmprl array.
   These coefficients are used to multiply the length input values
   before an FFT is performed. Ncode is defined as follows:
   0 => Uniform weightingz
   1 => Cosine weighting
   2 => Bartlett weighting (Triangle weighting)
   3 => Hanning weighting
   4 => Hamming weighting
   5 => Blackman weighting
*/


	int	i, limit;
	double	xn, xn2, Pi, q;
	double	a [] = { 0.0, 0.0, 1.0, 0.5, 0.54, 0.42 };
	double	b [] = { 0.0, 0.0,-1.0, 0.0, 0.00, 0.00 };
	double	c [] = { 0.0, 0.0, 0.0, 0.0, 0.00, 0.08 };
	double	d [] = { 0.0, 0.0, 0.0, 0.5, 0.46, 0.50 };
	double	e [] = { 0.0, 1.0, 0.0, 0.0, 0.00, 0.00 };

  unsigned long nu;

  nu=0;
  while((unsigned long)(length>>=1))  ++nu;
  length=(unsigned long)1<<nu;

	Pi = 4.0 * atan(1.0);
	limit = length/2 + (length&1);
	xn2 = (double)(length-1) / 2.0;
	b[2] = -1. / xn2 ;
	if (type<0 || type>6)
	{	printf ("WARNING! Invalide NCODE in build_filter \n");
		return (0);
	}
	else if( type==6 )
	  {
	  for( i=0;i<length;i++ )
	    {Tmprl[i]=1.0-1.0*cos( 2.0*Pi*i/(length-1) );}
	  return 0;
	  }
	for (i=0; i<limit; i++)
	{	xn = (double) i;
		q = a[type] +
		    b[type] * xn +
		    c[type] * cos(2.0*xn*Pi/xn2) +
		    d[type] * cos(xn*Pi/xn2) +
		    e[type] * cos(0.5*xn*Pi/xn2);
		Tmprl[i] = 1.0-q;
		Tmprl[length-1-i] = 1.0-q;
	}

return length;
}
