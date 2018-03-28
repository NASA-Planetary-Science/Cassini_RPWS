/*	file <fftx.c>
 *
 *	Demonstration program to show off FFT algorithm
 */

#include <stdarg.h>
#include <stdio.h>
#include <ctype.h>
#include <math.h>

#define NU	8
#define	NSIZE	256

int	build_trig (void);
int	fft (double xreal[], double ximag[], int n, int nu);
int	printsamp (double x[], double y[], int 	nn);
int	bitrev (int j, int nu);

/* Globals */

static	int		Samples, Nu;
static	double		Sine[NSIZE];
static	double		TWO_PI;

/* Main program */

int main()
{
	int  	loop, i;
	double	Time, Freq, Phase;
	double	Hanning[NSIZE];
	double	Rsave[NSIZE];
	double	Real[NSIZE], Imag[NSIZE], Sample;

	TWO_PI = (2.0 * 3.14159265358979324);

	Samples = NSIZE;
	Nu = NU;
/*	build_filter (Hanning, Samples, 3);	*/
/* Pre-compute the twiddle factors */
	build_trig ();

	printf("Input the test Frequency (Hz) ? ");
	scanf("%lf",&Freq);

	printf("Input the test Phase (Deg) ? ");
	scanf("%lf",&Phase);
	Phase = Phase * TWO_PI / 360.;

	printf("Input the sampling Frequency (Hz) ?");
	scanf("%lf",&Sample);
	Sample = 1.0 / Sample;			/* Calculate Sample Time */

	Time = 0.0;
	for (loop = 0; loop < Samples; loop++)
	{	Rsave[loop] =  sin(TWO_PI * Freq * Time + Phase) + 1.;
		Time += Sample;			/* t = t + deltat */
	}

	  for (loop = 0; loop < Samples; loop++)
	  {   Real[loop] = Rsave[loop];
	      Imag[loop] = 0.0;
	  }
	  fft (Real, Imag, Samples, Nu);
	  printsamp (Real, Imag, Samples);

	return(0);
}

int	printsamp (double x[], double y[], int 	nn)
{
	int	i;

	for (i = 0; i < nn; i++)
	{	printf ("%d %f %f\n", i, x[i], y[i]);
	}
	return (0);
}
int	fft (double xreal[], double ximag[], int n, int nu)
{
/* Performs FFT on complex data array, which is represented by the two
   input arrays xreal & ximag. The number of samples in the complex data
   set is given by the input n, which must be a power of 2, and the
   input parameter nu must be such that 2**nu = n.
   The output is returned in the original complex array, with DC being
   in the xreal[0] and the AC components being in the complex items
   (xreal[1],ximag[1]) through (xreal[n/2],ximag[n/2]).
*/
	int	n2, nu1, k;
	int	i, l, p, mult;
	double	c, s, arg, treal, timag;
	double	a1, a2, b1, b2, init;
	double  x1real[NSIZE], x1imag[NSIZE];

/* Start of real-only fft code */

	n = n/2;
	nu = nu-1;
	for (i=0; i<n; i++)
	{	x1real[i] = xreal[2*i];
		x1imag[i] = xreal[2*i+1];
	}
	for (i=0; i<n; i++)
	{	xreal[i] = x1real[i];
		ximag[i] = x1imag[i];
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
/*
		    arg = TWO_PI * p / n;
		    c = cos (arg);
		    s = sin (arg);
*/
		    c = Sine[2*((p+(n>>2))%n)];
		    s = Sine[2*p];

		    treal = xreal[k+n2]*c+ximag[k+n2]*s;
		    timag = ximag[k+n2]*c-xreal[k+n2]*s;
		    xreal[k+n2] = xreal[k] - treal;
		    ximag[k+n2] = ximag[k] - timag;
		    xreal[k] = xreal[k] + treal;
		    ximag[k] = ximag[k] + timag;
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
	    {	treal = xreal[k-1];
		timag = ximag[k-1];
		xreal[k-1] = xreal[i-1];
		ximag[k-1] = ximag[i-1];
		xreal[i-1] = treal;
		ximag[i-1] = timag;
	    }
	}

/* Start of real-only fft code */

	arg = TWO_PI / 2. / (double) n ;
	init = arg;
	for (i=1; i<n; i++)
	{
/*
		s = sin(arg);
		c = cos(arg);
*/
		s = Sine[i];
		c = Sine[(i+(n>>1))%(n<<1)];
		a1 = (xreal[i]+xreal[n-i]) / 2.0;
		a2 = (xreal[i]-xreal[n-i]) / 2.0;
		b1 = (ximag[i]+ximag[n-i]) / 2.0;
		b2 = (ximag[i]-ximag[n-i]) / 2.0;
		x1real[i] = a1+c*b1-s*a2;
		x1imag[i] = b2-s*b1-c*a2;
		arg = arg + init;
	}
	xreal[n] = xreal[0] - ximag[0];
	ximag[n] = 0.0;
	xreal[0] = xreal[0] + ximag[0];
	ximag[0] = 0.0;
	ximag[n] = 0.0;
	k = 2*n;
	for (i=1; i<n; i++)
	{	xreal[i] = x1real[i];
		ximag[i] = x1imag[i];
		xreal[k-i] = x1real[i];
		ximag[k-i] = -ximag[i];
	}
	n = 2*n;
	nu = nu + 1;

/* End of real-only fft code */

/* Scale data back to original units:
	DC and (N/2)th value get divided by N
	For 1st AC value through the (N/2-1)th value, divide by N/2
*/
	n2 = n / 2;
	xreal[0] = xreal[0] / n;
	ximag[0] = ximag[0] / n;	/* This oughta be zero */
	xreal[n/2] = xreal[n/2] / 2;
	ximag[n/2] = ximag[n/2] / 2;
	for (i = 1; i < n; i++)
	{	xreal[i] = xreal[i] / n2;
		ximag[i] = ximag[i] / n2;
	}
	return (0);
}
int	bitrev (int j, int nu)
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
int	build_trig ()
{
/* pre-computes the sine/cosine lookup table */
	int		loop;
	double	angle, increment;

	angle = 0.0;
	increment = TWO_PI / (double) Samples;

	for (loop = 0; loop < Samples; loop++)
	{	Sine[loop] = sin(angle);
		angle += increment;
	}
	return (0);
}
int	build_filter (double data[], int nsamp, int ncode)
{
/* Create a window function of type ncode.
   Nsamp coefficients are returned in the data array.
   These coefficients are used to multiply the nsamp input values
   before an FFT is performed. Ncode is defined as follows:
   0 => Uniform weighting
   1 => Cosine weighting
   2 => Triangle weighting
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

	Pi = 4.0 * atan (1.0);
	limit = nsamp/2 + (nsamp&1);
	xn2 = (double)(nsamp-1) / 2.0;
	b[2] = -1. / xn2 ;
	if (ncode<0 || ncode>5)
	{	printf ("WARNING! Invalide NCODE in build_filter \n");
		return (0);
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
