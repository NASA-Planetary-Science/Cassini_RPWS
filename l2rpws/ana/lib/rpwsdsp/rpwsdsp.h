#ifndef _RPWSDSP_H
#define _RPWSDSP_H


#define  HanningFFTmax		16384

#define  UNIFORM_WINDOW		(0)
#define  COSINE_WINDOW		(1)
#define  BARTLETT_WINDOW	(2)
#define  HANNING_WINDOW		(3)
#define  HAMMING_WINDOW		(4)
#define  BLACKMAN_WINDOW	(5)

#ifndef RealOnlyFFT_C_
extern double PI,TWO_PI;		/* Defined in RealOnlyFFT.c */
#endif

int rpwsdsp_fft_f(float *xreal,float *yimag,int n);
int rpwsdsp_window_f(float *data, int nsamp, int ncode);
int hanningfft_f(float *xreal,int n);
int realonlyfft_f(float *xreal,float *mag,float *phs,int n,float avg);

long WindowFunction_f(long length,float *Tmprl,long type);
long RealOnlyFFT_f(long length,float *Tmprl,float *Real,float *Imag);
long MagnitudeFFT_f(long length,float *real,float *imag,float *mag);
long PhaseDegreesFFT_f(long length,float *real,float *imag,float *phs);
long PhaseRadiansFFT_f(long length,float *real,float *imag,float *phs);
int polarization_vectors(long length,float *Xreal,float *Ximag,
				     float *Yreal,float *Yimag,
				     float *Zreal,float *Zimag,
                                     float *Xpynt,float *Ypynt,float *Zpynt);

#endif
