#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define RealOnlyFFT_C_

#include "rpwsdsp.h"

#define NSIZE 32768

double	PI     =       (3.14159265358979324);
double	TWO_PI = (2.0 * 3.14159265358979324);

static int bitrev (int j, int nu);

long RealOnlyFFT_f(long length,float *Tmprl,float *Real,float *Imag)
{
/* Performs FFT on complex data array, which is represented by the two
   input arrays Real & Imag. The number of samples in the complex data
   set is given by the input n, which must be a power of 2, and the
   input parameter nu must be such that 2**nu = n.
   The output is returned in the original complex array, with DC being
   in the Real[0] and the AC components being in the complex items
   (Real[1],Imag[1]) through (Real[n/2],Imag[n/2]).
*/




	int	n2, nu1, k;
	int	i, l, p, mult;
	double	c, s, arg, treal, timag;
	double	a1, a2, b1, b2, init;
	double  x1real[NSIZE], x1imag[NSIZE];/**/

int nu,n;



  /* Make sure that length is a power of two */

  nu=0;
  while((unsigned long)(length>>=1))  ++nu;
  length=(unsigned long)1<<nu;
  n=(int)length;



  /* Start of real-only fft code */

	n = n/2;
	nu = nu-1;
	for (i=0; i<n; i++){
	  Real[i] = Tmprl[2*i];
	  Imag[i] = Tmprl[2*i+1];
          }
/* End of real-only fft code */



	TWO_PI = (2.0 * 3.14159265358979324);
	n2 = n / 2;
	nu1 = nu - 1;
	k = 0;
	mult=0;
	for (l=1; l <= nu; l++)
	{   do
	    {	for (i=1; i<= n2; i++)
		{
		    p = bitrev(k/(1<<nu1),nu);
		    arg = TWO_PI * (double)p / (double)n;
		    c = cos (arg);
		    s = sin (arg);
		    treal = Real[k+n2]*c+Imag[k+n2]*s;
		    timag = Imag[k+n2]*c-Real[k+n2]*s;
		    Real[k+n2] = Real[k] - treal;
		    Imag[k+n2] = Imag[k] - timag;
		    Real[k] = Real[k] + treal;
		    Imag[k] = Imag[k] + timag;
		    k = k + 1;
		}
		k = k + n2;
	    }	while (k < n);
	    k = 0;
	    nu1 = nu1 - 1;
	    n2 = n2 / 2;
	}
	for (k=1; k <= n; k++)
	{   i = bitrev((k-1),nu) + 1;
	    if (i > k)
	    {	treal = Real[k-1];
		timag = Imag[k-1];
		Real[k-1] = Real[i-1];
		Imag[k-1] = Imag[i-1];
		Real[i-1] = treal;
		Imag[i-1] = timag;
	    }
	}

/* Start of real-only fft code */

	arg = TWO_PI / 2. / (double) n ;
	init = arg;
	for (i=1; i<n; i++)
	{
		s = sin(arg);
		c = cos(arg);
		a1 = (Real[i]+Real[n-i]) / 2.0;
		a2 = (Real[i]-Real[n-i]) / 2.0;
		b1 = (Imag[i]+Imag[n-i]) / 2.0;
		b2 = (Imag[i]-Imag[n-i]) / 2.0;
		x1real[i] = a1+c*b1-s*a2;
		x1imag[i] = b2-s*b1-c*a2;
		arg = arg + init;
	}
	Real[n] = Real[0] - Imag[0];
	Imag[n] = 0.0;
	Real[0] = Real[0] + Imag[0];
	Imag[0] = 0.0;
	Imag[n] = 0.0;
	k = 2*n;
	for (i=1; i<n; i++)
	{	Real[i] = x1real[i];
		Imag[i] = x1imag[i];
		Real[k-i] = x1real[i];
		Imag[k-i] = -Imag[i];
	}
	n = 2*n;
	nu = nu + 1;

/* End of real-only fft code */

/* Scale data back to original units:
	DC and (N/2)th value get divided by N
	For 1st AC value through the (N/2-1)th value, divide by N/2
*/


	n2 = n / 2;
	Real[0] = Real[0] / n;
	Imag[0] = Imag[0] / n;	
	Real[n/2] = Real[n/2] / 2;
	Imag[n/2] = Imag[n/2] / 2;
	for (i = 1; i < n; i++)
	{	Real[i] = Real[i] / n2;
		Imag[i] = Imag[i] / n2;
	}
    
return n;
}




static int bitrev (int j, int nu)
{

/*	Reverses lowest nu bits of j & returns result	*/

	int	i, j1, j2, k;

	j1 = j;
	k = 0;
	for (i=1; i <= nu; i++)
	{	j2 = j1>>1;
		k = (k<<1) + (j1 - (j2<<1));
		j1 = j2;
	}
	return (k);
}
