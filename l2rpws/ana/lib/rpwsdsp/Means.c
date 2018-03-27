#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <math.h>




int polarization_vectors(long length,float *Xreal,float *Ximag,
				     float *Yreal,float *Yimag,
				     float *Zreal,float *Zimag,
                                     float *Xpynt,float *Ypynt,float *Zpynt)
{
int status=1;
long l;
float Jreal[3][3],Jimag[3][3],Jnorm;

  for(l=0;l<length;l++){			/* Calculate covariance matrix */
    Jreal[0][0]=(Xreal[l]*Xreal[l])+(Ximag[l]*Ximag[l]);
    Jimag[0][0]=0.0;

    Jreal[0][1]=(Xreal[l]*Yreal[l])+(Ximag[l]*Yimag[l]);
    Jimag[0][1]=(Yreal[l]*Ximag[l])-(Xreal[l]*Yimag[l]);

    Jreal[0][2]=(Xreal[l]*Zreal[l])+(Ximag[l]*Zimag[l]);
    Jimag[0][2]=(Zreal[l]*Ximag[l])-(Xreal[l]*Zimag[l]);

    Jreal[1][1]=(Yreal[l]*Yreal[l])+(Yimag[l]*Yimag[l]);
    Jimag[1][1]=0.0;

    Jreal[1][0]=(Xreal[l]*Yreal[l])+(Ximag[l]*Yimag[l]);
    Jimag[1][0]=(Xreal[l]*Yimag[l])-(Yreal[l]*Ximag[l]);

    Jreal[1][2]=(Yreal[l]*Zreal[l])+(Yimag[l]*Zimag[l]);
    Jimag[1][2]=(Zreal[l]*Yimag[l])-(Yreal[l]*Zimag[l]);

    Jreal[2][2]=(Zreal[l]*Zreal[l])+(Zimag[l]*Zimag[l]);
    Jimag[2][2]=0.0;

    Jreal[2][1]=(Zreal[l]*Yreal[l])+(Zimag[l]*Yimag[l]);
    Jimag[2][1]=(Yreal[l]*Zimag[l])-(Zreal[l]*Yimag[l]);

    Jreal[2][0]=(Xreal[l]*Zreal[l])+(Ximag[l]*Zimag[l]);
    Jimag[2][0]=(Xreal[l]*Zimag[l])-(Zreal[l]*Ximag[l]);

    Jnorm=sqrt( (Jimag[0][1]*Jimag[0][1]) + (Jimag[0][2]*Jimag[0][2]) + (Jimag[1][2]*Jimag[1][2]) );
    Xpynt[l]=Jimag[1][2]/Jnorm;
    Ypynt[l]=(-Jimag[0][2])/Jnorm;
    Zpynt[l]=Jimag[0][1]/Jnorm;
    }


return status;
}
