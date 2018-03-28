#include <math.h>

int window (double *windo, int lenw, int weight)
{
   int   index;
   float PI = 3.1415927;
   float denom, factor;
/*
      0 => uniform weighting
      1 => cosine weighting
      2 => Bartlett (triangle) weighting
      3 => Hanning weighting
      4 => Hamming weighting
      5 => Blackman weighting
*/
   float A [6] = { 0.0, 0.0, 1.0, 0.5, 0.54, 0.42 };
   float B [6] = { 0.0, 0.0, 0.0, 0.0, 0.00, 0.00 };
   float C [6] = { 0.0, 0.0, 0.0, 0.0, 0.00, 0.08 };
   float D [6] = { 0.0, 0.0, 0.0, 0.5, 0.46, 0.50 };
   float E [6] = { 0.0, 1.0, 0.0, 0.0, 0.00, 0.00 };
   float loss [6] = { 1.20, 1.64, 1.78, 2.00, 1.80, 2.30 };

/*   weight = 3                  Hanning window */

   /* B [2] depends on what lenw is */
   denom = (float) (lenw - 1)/2.0;
   B [2] = (float) (-1.0/denom);

      /* calculate window */
      for (index = 0; index < (int) (lenw/2); index++)
      {
         factor = A [weight] +
                  B [weight]*index +
                  C [weight]*(cos ((double) ((2.0*index*PI)/denom))) +
                  D [weight]*(cos ((double) ((index*PI)/denom))) +
                  E [weight]*(cos ((double) ((0.5*index*PI)/denom)));
         windo [index] = loss[weight] * (1.0 - factor);
         windo [(lenw - 1 - index)] = loss[weight] * (1.0 - factor);
      }
   return (0);
}
