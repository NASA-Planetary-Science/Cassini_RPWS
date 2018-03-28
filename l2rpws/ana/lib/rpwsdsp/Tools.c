#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "rpwsdsp.h"
  
  
  
long MagnitudeFFT_f(long length,float *real,float *imag,float *mag)
{
long loop;

  for(loop=0;loop<length;loop++){
    mag[loop] = sqrt( (real[loop]*real[loop]) + (imag[loop]*imag[loop]) );
    }

return length;
}



long PhaseRadiansFFT_f(long length,float *real,float *imag,float *phs)
{
long loop;

  for(loop=0;loop<length;loop++){
    if(real[loop] || imag[loop]){	/* Trap for zero condition of both */
      phs[loop] = atan2( imag[loop],real[loop] );
      }
    else{
      phs[loop] = 0.0;			/* ArcTan(0/0) */
      }
    }

return length;
}



long PhaseDegreesFFT_f(long length,float *real,float *imag,float *phs)
{
long loop;
double factor=360.0/TWO_PI;

  for(loop=0;loop<length;loop++){
    if(real[loop] || imag[loop]){	/* Trap for zero condition of both */
      phs[loop] = factor*atan2( imag[loop],real[loop] );
      }
    else{
      phs[loop] = 0.0;			/* ArcTan(0/0) */
      }
    }

return length;
}
