/*
	Transforms a vector from J2000 to GSE coordinate system
	Modification History:
	17-Mar-00	TFA	Add Planet to argument list to do
				   either Venus (299)	VSE coordinate system
				   or     Earth (399)	GSE coordinate system
				   or   Jupiter (599)	JSE coordinate system
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <SpiceUsr.h>    /* Intended interface */
#include <SpiceZfc.h>    /* direct access to fortran functions */

#include <gllspice.h>

/* 
void spkez_ (int *target, double *et, char *frame, char *aberr, int *obs, double state [], double *lt, int framelen, int aberrlen);
void gsmtrn_g_ (int *target, double *et, char *frame, char *aberr, double tipm[][], int framelen, int aberrlen);
void gsetrn_g_ (int *target, double *et, char *frame, char *aberr, double tipm[][], int framelen, int aberrlen);
void utc2et_ (char *fstring, double *et, int len);
void sce2s_ (int *spacecraft, double *et, char *sclk, int sclklen);
void et2utc_ (double *et, char *form, int *prec, char *utc, int formlen, int utclen);
void vminus_ (double vin [], double vout []);
void mxv_ (double tipm [][], double vin [], double vout []);
void mtxv_ (double tipm [][], double vin [], double vout []);
void bodmat_ (int *target, double *et, double *tipm);
void reclat_ (double vin [], double *rad, double *lon, double *lat);
*/


/*-------------------------------------------------
   Converts a C 2D matrix to a FORTRAN 2D matrix.
--------------------------------------------------*/
void cnvt_f2d (double *in, double *out, int nx, int ny)
{
   int x, y;

   for (y = 0; y < ny; y++) 
      for (x = 0; x < nx; x++) 
         out [(x*ny) + y] = in [x + (nx*y)];
}

double *make_seed_f2d (double *in, int nx, int ny)
{
   double *tmp_ptr;

   tmp_ptr = (double *) malloc ((nx*ny)*sizeof (double));
   if (tmp_ptr != NULL)
      cnvt_f2d (in, tmp_ptr, nx, ny);
   else
      perror ("make_seed_f2d");

   return (tmp_ptr);
}

void convert_f2c (double *array, int x, int y)
{ 
   double *tmp_array;

   tmp_array = make_seed_f2d (array, x, y);
   memcpy (array, tmp_array, sizeof (double)*x*y);
   free (tmp_array);
}

void bodmat (int body, double et, double *tipm)
{
   SpiceInt    lbody = body;
   double let   = et;

   bodmat_ (&lbody, &let, tipm);
   convert_f2c (tipm, 3, 3); 
}

void gsetrn_g (int body, double et, char *ref, char *corr, double *t12gse)
{
   int    lbody  = body;
   double let    = et;
   long   slref  = strlen (ref);
   long   slcorr = strlen (corr);

   gsetrn_g_ (&lbody, &let, ref, corr, t12gse, slref, slcorr);
   convert_f2c (t12gse, 3, 3);
}

void spkez (
   int    targ,
   double et,
   char   *ref,
   char   *abcorr,
   int    obs,
   double *starg, /* 6 element array */
   double *lt)
{
   SpiceInt  ltarg    = targ;
   double    let      = et;
   SpiceInt  lobs     = obs;
   long      slref    = strlen (ref);
   long      slabcorr = strlen (abcorr);

   spkez_ (&ltarg, &let, ref, abcorr, &lobs, starg, lt, slref, slabcorr); 
}

void utc2et (char *utcstr, double *et)
{
   long slutcstr = strlen (utcstr);

   utc2et_ (utcstr, et, slutcstr);
}

void vminus (double *vin, double *vout)
{
   double tmpvec [3];
   int    i;

   memcpy (tmpvec, vin, sizeof (tmpvec));
   for (i = 0; i < 3; i++)
      vout [i] = -1.0*tmpvec [i];
}

void mxv (double matrix [3][3], double *vin, double *vout)
{
   int    i;
   double tmp [3];

   memcpy (tmp, vin, sizeof (tmp));
   for (i = 0; i < 3; i++)
   {
      vout [i] = matrix [i][0]*tmp [0] +
                 matrix [i][1]*tmp [1] +
                 matrix [i][2]*tmp [2];
   }
}

void reclat (double *rectan, double *radius, double *tlong, double *lat) 
{
   reclat_ (rectan, radius, tlong, lat);
}

void gei2gse (int yr, int dy, int hr, int mn, int ss, double vec_in[], double vec_out[], int planet)
{
   char scstr [80], utstr2 [25];
   int Sun = 10, Venus = 299, Earth = 399, Jupiter = 599, Saturn = 699;
   int Cassini = -82, precision = 3;
   int len, index, index2;
   double sc_pos [6], sc_pos2 [3], sun_pos [6], sun_pos2 [3];
   double x_vec [3], y_vec [3], z_vec [3];
   double state [6], pos [3], temp [3], temp2 [3], tipm [3][3];
   double matrix [3][3];
   double radius, longitude, latitude;
   double theta_sc, theta_gm, theta_su, gamma;
   double lt, et, x, y, z, vx, vy, vz;
   char *frame = "J2000";
   char *aberr = "NONE";

   {
      sprintf (scstr, "%04d %03d // %02d:%02d:%02d.0", yr, dy, hr, mn, ss);
      utc2et (scstr, &et);

      spkez (planet, et, frame, aberr, Cassini, sc_pos, &lt);

      for (index = 0; index < 3; index++)
         pos [index] = sc_pos [index];

      vminus (pos, sc_pos2);
      gsetrn_g (planet, et, frame, aberr, (double*)matrix);

      mxv (matrix, vec_in, vec_out);	/* transform to GSE coordinates */

   }
}
