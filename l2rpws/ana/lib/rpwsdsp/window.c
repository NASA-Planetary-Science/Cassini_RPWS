#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "rpwsdsp.h"

int rpwsdsp_window_f(float *data, int nsamp, int ncode)
{
/* Create a window function of type ncode.
   Nsamp coefficients are returned in the data array.
   These coefficients are used to multiply the nsamp input values
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


	Pi = 4.0 * atan(1.0);
	limit = nsamp/2 + (nsamp&1);
	xn2 = (double)(nsamp-1) / 2.0;
	b[2] = -1. / xn2 ;
	if (ncode<0 || ncode>6)
	{	printf ("WARNING! Invalide NCODE in build_filter \n");
		return (0);
	}
	else if( ncode==6 )
	  {
	  for( i=0;i<nsamp;i++ )
	    {data[i]=1.0-1.0*cos( 2.0*Pi*i/(nsamp-1) );}
	  return 0;
	  }
	for (i=0; i<limit; i++)
	{	xn = (double) i;
		q = a[ncode] +
		    b[ncode] * xn +
		    c[ncode] * cos(2.0*xn*Pi/xn2) +
		    d[ncode] * cos(xn*Pi/xn2) +
		    e[ncode] * cos(0.5*xn*Pi/xn2);
		data[i] = 1.0-q;
		data[nsamp-1-i] = 1.0-q;
	}

return(0);
}
