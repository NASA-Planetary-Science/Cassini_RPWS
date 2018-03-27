#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include <das.h>

#include "y1958.h"
#include "casephem.h"

char *makeword (char *line, char stop);
char *fmakeword (FILE *f, char stop, int *len);
void unescape_url (char *url);
void plustospace (char *str);

void furnsh_ (char *filename, int namelen);
void ldpool_ (char *name, int namelen);
void spkez_ (int *target, double *et, char *frame, char *aberr, int *obs, double state [], double *lt, int framelen, int aberrlen);
void gsetrn_g_ (int *target, double *et, char *frame, char *aberr, double tipm [][], int framelen, int aberrlen);
void utc2et_ (char *fstring, double *et, int len);
void vminus_ (double vin [], double vout []);
void vhat_ (double vin [], double vout []);
void ucrss_ (double v1 [], double v2 [], double vout []);
void mxv_ (double tipm [][], double vin [], double vout []);
void mtxv_ (double tipm [][], double vin [], double vout []);
void bodmat_ (int *target, double *et, double tipm [][]);
void reclat_ (double vin [], double *rad, double *lon, double *lat);

void getcorot_ (int *observer, int *target, int *bodyname, double *et, double *px, double *py, double *pz, double *vx, double *vy, double *vz);
void sls2_sls3_ (int *observer, double *et, double *loctime, double *lighttimeseconds);
void getksm_ (int *observer, int *target, double *et, double *px, double *py, double *pz,
                                                      double *vx, double *vy, double *vz);
void get_altitude_ (int *target, int *observer, double *et, double *altitude);
void get_geo_ (int *observer, int *target, int *bodyname, double *et, double *radius, double *mlat, double *lvalue, double *loctime);
void get_jupiter_mag_coords_ (int *observer, int *target, double *et, double *radius, double *longitude, double *mlat, double *mlt, double *lvalue, double *iophase);
void get_altitude_s_ (int *satellite, int *observer, double *et, double *altitude, double *radius);
void get_altitude_j_ (int *satellite, int *observer, double *et, double *altitude, double *radius);
void get_solar_ecliptic_ (int *observer, int *target, double *et, double *px, double *py, double *pz, double *vx, double *vy, double *vz);
void get_solar_eq_ (int *observer, int *target, double *et, double *px, double *py, double *pz, double *vx, double *vy, double *vz);

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
   int    lbody = body;
   double let   = et;

   bodmat_ (&lbody, &let, tipm);
   convert_f2c (tipm, 3, 3); 
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
   int    ltarg    = targ;
   double let      = et;
   int    lobs     = obs;
   long   slref    = strlen (ref);
   long   slabcorr = strlen (abcorr);

   spkez_ (&ltarg, &let, ref, abcorr, &lobs, starg, lt, slref, slabcorr); 
}

void utc2et (char *utcstr, double *et)
{
   long slutcstr = strlen (utcstr);

   utc2et_ (utcstr, et, slutcstr);
}

void ldpool (char *kernel)
{
   long slkernel = strlen (kernel);

   ldpool_ (kernel, slkernel);
}

double max (double c1, double c2, double c3)
{
   double cmax;

   cmax = fabs (c1);
   if (fabs (c2) > cmax)
      cmax = fabs (c2);
   if (fabs (c3) > cmax)
      cmax = fabs (c3);
   return (cmax);
}

double vnorm (double *vin)
{
   double tmpvec [3];
   double vmax;
   double norm;

   memcpy (tmpvec, vin, sizeof (tmpvec));

   vmax = max (tmpvec [0], tmpvec [1], tmpvec [2]);
   if (vmax == 0.0)
      return (0.0);

   norm = vmax*sqrt (pow (tmpvec [0]/vmax, 2.0) +
                     pow (tmpvec [1]/vmax, 2.0) +
                     pow (tmpvec [2]/vmax, 2.0));
   return (norm);
}

void vhat (double *v1, double *vout)
{
   double tmpvec [3];
   double norm;
   
   memcpy (tmpvec, v1, sizeof (tmpvec));

   norm = vnorm (tmpvec);
   if (norm > 0.0)
   {
      vout [0] = tmpvec [0]/norm;
      vout [1] = tmpvec [1]/norm;
      vout [2] = tmpvec [2]/norm;
   }
   else
   {
      memset (vout, 0, sizeof (tmpvec));
   }
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

void gsetrn_g (int body, double et, char *ref, char *corr, double *t12gse)
{
   int    lbody  = body;
   double let    = et;
   long   slref  = strlen (ref);
   long   slcorr = strlen (corr);

   gsetrn_g_ (&lbody, &let, ref, corr, t12gse, slref, slcorr);
   convert_f2c (t12gse, 3, 3);
}

void ucrss (double *v1, double *v2, double *vout)
{
   double tmpvec1 [3];
   double tmpvec2 [3];
   double vcrss [3];
   double v1max;
   double v2max;
   double norm;

   v1max = max (v1 [0], v1 [1], v1 [2]);
   v2max = max (v2 [0], v2 [1], v2 [2]);

   if (v1max != 0.0)
   {
      tmpvec1 [0] = v1 [0]/v1max;
      tmpvec1 [1] = v1 [1]/v1max;
      tmpvec1 [2] = v1 [2]/v1max;
   }
   else
      memset (tmpvec1, 0, sizeof (tmpvec1));

   if (v2max != 0.0)
   {
      tmpvec2 [0] = v2 [0]/v2max;
      tmpvec2 [1] = v2 [1]/v2max;
      tmpvec2 [2] = v2 [2]/v2max;
   }
   else
      memset (tmpvec2, 0, sizeof (tmpvec2));

   vcrss [0] = ((tmpvec1 [1]*tmpvec2 [2]) - (tmpvec1 [2]*tmpvec2 [1]));
   vcrss [1] = ((tmpvec1 [2]*tmpvec2 [0]) - (tmpvec1 [0]*tmpvec2 [2]));
   vcrss [2] = ((tmpvec1 [0]*tmpvec2 [1]) - (tmpvec1 [1]*tmpvec2 [0]));

   norm = vnorm (vcrss);

   if (norm > 0.0)
   {
      vout [0] = vcrss [0]/norm;
      vout [1] = vcrss [1]/norm;
      vout [2] = vcrss [2]/norm;
   }
   else
      memset (vout, 0, sizeof (tmpvec1));
}

void transpose (double matin [3][3], double matout [3][3])
{
   int i, j;

   for (i = 0; i < 3; i++)
      for (j = 0; j < 3; j++)
         matout [i][j] = matin [j][i];
}

void mtxv (double matrix [3][3], double *vin, double *vout)
{
   double mt [3][3];

   transpose (matrix, mt);
   mxv (mt, vin, vout);
}

int past_1958 (int year, int day)
{
   int yr, past;

   yr = year - 1958;
   past = days_past [yr];
   past += (day - 1);

   return (past);
}

void ms2dhms (a, b, c, d, e)
int *a, *b, *c;
double *d;
double e;
{
   *d = (double) fmod ((double) (e / 1000.0), 60.0);
   *c = (int) fmod ((double) (e / 60000.0), 60.0);
   *b = (int) fmod ((double) (e / 3600000.0), 24.0);
   *a = (int) (e / 86400000.0);
}

void yrdy1958 (year, day, dy58)
int *year, *day;
int dy58;
{
   int index;

   index = 0;
   while (dy58 >= days_past [index])
      index++;

   index--;
   *year = 1958 + index;
   *day = dy58 - days_past [index] + 1;
}

double GetTime (yr, day, hr, mn, ss)
int yr, day, hr, mn;
float ss;
{
   double temp_dy, total_ms;

   temp_dy = (double) past_1958 (yr, day);
   total_ms = (double) (temp_dy*86400.0) +
              (double) (hr*3600) +
              (double) (mn*60) +
              (double) ss;

   total_ms = (total_ms*1000.0);

   return (total_ms);
}

int ValidateTime (st)
struct time_struc *st;
{
   int past, year, month, day, doy, hr, mn;
   double sec, ValidSt, ValidSp;
   char temp1 [80], temp2 [80];
   char *v1StTime = "1977-248-13-58-37";
   char *v1SpTime = "2049-364-23-58-52";
   char *v2StTime = "1977-232-15-31-44";
   char *v2SpTime = "2049-364-23-58-52";
   char *glStTime = "1989-292-01-28-38";
   char *glSpTime = "2003-273-11-58-55";
   char *ulStTime = "1990-279-20-26-01";
   char *ulSpTime = "2050-001-11-58-49";
   char *caStTime = "1997-288-09-26-10";
   char *caSpTime = "2017-258-10-32-49";
   char *juStTime = "2011-217-17-18-12";
   char *juSpTime = "2021-212-04-33-26";
   char *ssStTime = "1958-001-00-00-00";
   char *ssSpTime = "2094-001-00-00-00";

   if (st->observer == Voyager1)
   {
      sprintf (temp1, "%s", v1StTime);

      parsetime (temp1, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSt = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;

      sprintf (temp2, "%s", v1SpTime);

      parsetime (temp2, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSp = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;
   }
   else if (st->observer == Voyager2)
   {
      sprintf (temp1, "%s", v2StTime);

      parsetime (temp1, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSt = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;

      sprintf (temp2, "%s", v2SpTime);

      parsetime (temp2, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSp = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;
   }
   else if (st->observer == Galileo)
   {
      sprintf (temp1, "%s", glStTime);

      parsetime (temp1, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSt = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;

      sprintf (temp2, "%s", glSpTime);

      parsetime (temp2, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSp = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;
   }
   else if (st->observer == Ulysses)
   {
      sprintf (temp1, "%s", ulStTime);

      parsetime (temp1, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSt = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;

      sprintf (temp2, "%s", ulSpTime);

      parsetime (temp2, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSp = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;
   }
   else if (st->observer == Cassini)
   {
      sprintf (temp1, "%s", caStTime);

      parsetime (temp1, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSt = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;

      sprintf (temp2, "%s", caSpTime);

      parsetime (temp2, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSp = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;
   }
   else if (st->observer == Juno)
   {
      sprintf (temp1, "%s", juStTime);

      parsetime (temp1, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSt = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;

      sprintf (temp2, "%s", juSpTime);

      parsetime (temp2, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSp = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;
   }
   else
   {
      sprintf (temp1, "%s", ssStTime);

      parsetime (temp1, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSt = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;

      sprintf (temp2, "%s", ssSpTime);

      parsetime (temp2, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSp = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;
   }

   /* return failure if start/stop time outside kernel range */
   if ((st->st_total_ms < ValidSt) || (st->st_total_ms > ValidSp))
   {
      printf ("Content-type: text/plain\n\n");
      printf ("No data for the time range selected.\n");
      printf ("Select times within this range:\n\n");
      printf ("   %s - %s\n", temp1, temp2);
      return (False);
   }

   if ((st->sp_total_ms < ValidSt) || (st->sp_total_ms > ValidSp))
   {
      printf ("Content-type: text/plain\n\n");
      printf ("No data for the time range selected.\n");
      printf ("Select times within this range:\n\n");
      printf ("   %s - %s\n", temp1, temp2);
      return (False);
   }

   return (True);
}

char GetLetter (st)
struct time_struc *st;
{
   char ChTarget;

   switch (st->target)
   {
      case Venus:
         ChTarget = 'v';
         break;

      case Earth:
         ChTarget = 'e';
         break;

      case Jupiter:
         ChTarget = 'j';
         break;

      case Io:
         ChTarget = 'i';
         break;

      case Europa:
         ChTarget = 'e';
         break;

      case Ganymede:
         ChTarget = 'g';
         break;

      case Callisto:
         ChTarget = 'c';
         break;

      case SaturnBC:
         ChTarget = 's';
         break;

      case Saturn:
         ChTarget = 's';
         break;

      case Mimas:
         ChTarget = 'm';
         break;

      case Enceladus:
         ChTarget = 'e';
         break;

      case Tethys:
         ChTarget = 't';
         break;

      case Dione:
         ChTarget = 'd';
         break;

      case Rhea:
         ChTarget = 'r';
         break;

      case Titan:
         ChTarget = 't';
         break;

      case Hyperion:
         ChTarget = 'h';
         break;

      case Iapetus:
         ChTarget = 'i';
         break;

      case Phoebe:
         ChTarget = 'p';
         break;

      case Methone:
         ChTarget = 'm';
         break;

      case Anthe:
         ChTarget = 'a';
         break;

      case Pallene:
         ChTarget = 'p';
         break;

      case Telesto:
         ChTarget = 't';
         break;

      case Helene:
         ChTarget = 'h';
         break;
   }

   return (ChTarget);
}

void PrintLabels (st)
struct time_struc *st;
{
   int index, DashIndex;
   int FirstDash [9] = { 12, 12, 11, 11, 10, 10,  9,  9,  8 };
   int LastDash [9]  = { 12, 11, 11, 10, 10,  9,  9,  8,  8 };
   char ChTarget;

   if (st->radius < 10.0)
      DashIndex = 0;
   else if (st->radius < 100.0)
      DashIndex = 1;
   else if (st->radius < 1000.0)
      DashIndex = 2;
   else if (st->radius < 10000.0)
      DashIndex = 3;
   else if (st->radius < 100000.0)
      DashIndex = 4;
   else if (st->radius < 1000000.0)
      DashIndex = 5;
   else if (st->radius < 10000000.0)
      DashIndex = 6;
   else if (st->radius < 100000000.0)
      DashIndex = 7;
   else
      DashIndex = 8;

   printf ("Content-type: text/plain\n\n");

   if (st->coord == Geographic)
   {
      printf ("                                             ");
      printf ("Geographic Coordinate System\n\n");

      if (st->target == Sun)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (AU)\n");
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Venus)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Rv)   1 Rv = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Earth)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      MLat (deg)       MLT (hrs)             L");
         printf ("            R (Re)   1 Re = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------      ----------      ----------      ----------\n");
      }

      if ((st->target == Jupiter) && (st->observer == Juno))
      {
         printf ("                                                ");
         printf ("                                     ");
         printf ("  1 Rj = %1.1f km\n", st->radius);
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("      MLat (deg)       MLT (hrs)          R (Rj)         alt (km)           L    ");
         printf ("  Io phase (deg)\n");
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------      ----------      -----------      ----------");
         printf ("  --------------\n");
      }

      if ((st->target == Jupiter) && (st->observer != Juno))
      {
         printf ("                                                ");
         printf ("                                     ");
         printf ("  1 Rj = %1.1f km\n", st->radius);
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("      MLat (deg)       MLT (hrs)          R (Rj)           L    ");
         printf ("  Io phase (deg)\n");
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------      ----------      ----------");
         printf ("  --------------\n");
      }

      if (st->target == Io)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Ri)   1 Ri = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Europa)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Re)   1 Re = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Ganymede)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Rg)   1 Rg = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Callisto)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Rc)   1 Rc = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (((st->target == SaturnBC) || (st->target == Saturn)) && (st->observer != Cassini))
      {
         printf ("      SCET (UT)           WL(IAU) (deg)    WL(SLS2) (deg)    WL(SLS3) (deg)");
         printf ("       Lat (deg)           L          LT (hrs)          R (Rs)   1 Rs = %1.1f km\n", st->radius);
         printf ("---------------------     -------------    --------------    --------------");
         printf ("      ----------      --------      ----------      ----------\n");
      }

      if (((st->target == SaturnBC) || (st->target == Saturn)) && (st->observer == Cassini))
      {
         printf ("      SCET (UT)           WL(IAU) (deg)    WL(SLS2) (deg)    WL(SLS3) (deg)");
         printf ("       Lat (deg)           L          LT (hrs)       alt (km)          R (Rs)   1 Rs = %1.1f km\n", st->radius);
         printf ("---------------------     -------------    --------------    --------------");
         printf ("      ----------      --------      ----------     -----------      ----------\n");
      }

      if (st->target == Mimas)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Rm)   1 Rm = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Enceladus)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Re)   1 Re = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Tethys)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Rt)   1 Rt = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Dione)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Rd)   1 Rd = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Rhea)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Rr)   1 Rr = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Titan)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Rt)   1 Rt = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Hyperion)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Rh)   1 Rh = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Iapetus)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Ri)   1 Ri = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Phoebe)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Rp)   1 Rp = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Methone)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Rm)   1 Rm = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Anthe)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Ra)   1 Ra = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Pallene)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Rp)   1 Rp = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Telesto)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Rt)   1 Rt = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }

      if (st->target == Helene)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)      npoint alt (km)          R (Rh)   1 Rh = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ---------------      ----------\n");
      }
   }
   else
   {
      if (st->coord == Ecliptic)
      {
         printf ("                              ");
         printf ("                              ");
         printf ("Ecliptic Coordinate System\n\n");
      }
      else if (st->coord == Equatorial)
      {
         printf ("                              ");
         printf ("                             ");
         printf ("Equatorial Coordinate System\n\n");
      }
      else if (st->coord == CoRotational)
      {
         printf ("                              ");
         printf ("                           ");
         printf ("Co-rotational Coordinate System\n\n");
      }
      else if (st->coord == SaturnSolarMagnetic)
      {
         printf ("                              ");
         printf ("                       ");
         printf ("Saturn Solar Magnetospheric Coordinate System\n\n");
      }

      printf ("                         +");

      for (index = 0; index < FirstDash [DashIndex]; index++)
         printf ("-");

      ChTarget = GetLetter (st);

      if (st->target == Sun)
         printf (" Position (1 AU = %1.1f km) ", (float) st->radius);
      else
         printf (" Position (1 R%c = %1.1f km) ", ChTarget, (float) st->radius);

      for (index = 0; index < LastDash [DashIndex]; index++)
         printf ("-");

      printf ("+    +-------------------- Velocity --------------------+\n\n");

      if (st->target == Sun)
      {
         printf ("      SCET (UT)              X (AU)        Y (AU)");
         printf ("        Z (AU)        R (AU)");
      }
      else
      {
         printf ("      SCET (UT)              X (R%c)        Y (R%c)", ChTarget, ChTarget);
         printf ("        Z (R%c)        R (R%c)", ChTarget, ChTarget);
      }

      printf ("      X (km/s)      Y (km/s)      Z (km/s)   Vmag (km/s)\n");

      printf ("---------------------    ----------    ----------");
      printf ("    ----------    ----------");
      printf ("    ----------    ----------    ----------   -----------\n");
   }
}

int KSMCoords (stime)
struct time_struc *stime;
{
   int no_data;
   int past, year, month, day, doy, hr, mn;
   double Vx, Vy, Vz, Vmag;
   double et, radius, x, y, z;
   double delta, sec, st_ms;
   char scstr [80];

   no_data = True;

   if (!(ValidateTime (stime)))
      return (True);

   st_ms = stime->st_total_ms;
   delta = (stime->deltatee*1000.0);

   while (st_ms <= stime->sp_total_ms)
   {
      if (no_data)
      {
         no_data = False;
         PrintLabels (stime);
      }

      ms2dhms (&past, &hr, &mn, &sec, st_ms);
      yrdy1958 (&year, &doy, past);

      st_ms += delta;

      sprintf (scstr, "%04d %03d // %02d:%02d:%06.3f",
               year, doy, hr, mn, (float) sec);

      utc2et (scstr, &et);

      getksm_ (&stime->target, &stime->observer, &et, &x, &y, &z, &Vx, &Vy, &Vz);

      Vmag = sqrt (((Vx*Vx) + (Vy*Vy) + (Vz*Vz)));

      x /= stime->radius;
      y /= stime->radius;
      z /= stime->radius;

      radius = sqrt (((x*x) + (y*y) + (z*z)));

      printf ("%04d %03d %02d %02d %06.3f %13.3f %13.3f %13.3f %13.3f %13.3f %13.3f %13.3f %13.3f\n",
               year, doy, hr, mn, (float) sec,
               x, y, z, radius, Vx, Vy, Vz, Vmag);
   }

   return (no_data);
}

int CoRotCoords (stime)
struct time_struc *stime;
{
   int index, no_data, CoPlanet;
   int past, year, month, day, doy, hr, mn;
   double st_ms, sec, et, delta;
   double px, py, pz;
   double vx, vy, vz;
   double radius;
   double Vmag;
   char scstr [80];

   no_data = True;

   if (!(ValidateTime (stime)))
      return (True);

   st_ms = stime->st_total_ms;

   delta = (stime->deltatee*1000.0);

   if ((stime->target == Mimas)    || (stime->target == Enceladus) ||
       (stime->target == Tethys)   || (stime->target == Dione)     ||
       (stime->target == Rhea)     || (stime->target == Titan)     ||
       (stime->target == Hyperion) || (stime->target == Iapetus)   ||
       (stime->target == Phoebe)   || (stime->target == Methone)   ||
       (stime->target == Anthe)    || (stime->target == Pallene)   ||
       (stime->target == Telesto)  || (stime->target == Helene))
      CoPlanet = Saturn;
   else if ((stime->target == Io)       || (stime->target == Europa) ||
            (stime->target == Ganymede) || (stime->target == Callisto))
      CoPlanet = Jupiter;

   while (st_ms <= stime->sp_total_ms)
   {
      if (no_data)
      {
         no_data = False;
         PrintLabels (stime);
      }

      ms2dhms (&past, &hr, &mn, &sec, st_ms);
      yrdy1958 (&year, &doy, past);

      st_ms += delta;

      sprintf (scstr, "%04d %03d // %02d:%02d:%06.3f",
               year, doy, hr, mn, (float) sec);

      utc2et (scstr, &et);

      getcorot_ (&stime->observer, &stime->target, &CoPlanet, &et, &px, &py, &pz, &vx, &vy, &vz);

      Vmag = sqrt ((vx*vx) + (vy*vy) + (vz*vz));

      radius = sqrt ((px*px) + (py*py) + (pz*pz));

      radius = (radius/stime->radius);

      px /= stime->radius;
      py /= stime->radius;
      pz /= stime->radius;

      printf ("%04d %03d %02d %02d %06.3f %13.3f %13.3f %13.3f %13.3f %13.3f %13.3f %13.3f %13.3f\n",
               year, doy, hr, mn, (float) sec,
               px, py, pz, radius, vx, vy, vz, Vmag);
   }
}

int EclipticCoords (stime)
struct time_struc *stime;
{
   int index, no_data, observer;
   int past, year, month, day, doy, hr, mn;
   double sc_pos [6], sc_pos2 [3], pos [3], vel [3], velXYZ [3], Vmag;
   double matrix [3][3];
   double lt, et, radius, longitude, latitude, x, y, z;
   double px, py, pz;
   double vx, vy, vz;
   double delta, sec, st_ms;
   char scstr [80];
   char *frame1 = "J2000";
   char *frame2 = "ECLIPJ2000";
   char *aberr  = "LT+S";
   char *aberr2 = "NONE";

   no_data = True;

   if (!(ValidateTime (stime)))
      return (True);

   st_ms = stime->st_total_ms;
   delta = (stime->deltatee*1000.0);

   while (st_ms <= stime->sp_total_ms)
   {
      if (no_data)
      {
         no_data = False;
         PrintLabels (stime);
      }

      ms2dhms (&past, &hr, &mn, &sec, st_ms);
      yrdy1958 (&year, &doy, past);

      st_ms += delta;

      sprintf (scstr, "%04d %03d // %02d:%02d:%06.3f",
               year, doy, hr, mn, (float) sec);

      utc2et (scstr, &et);

      if ((stime->target == Jupiter) && (stime->observer == Juno))
      {
         get_solar_ecliptic_ (&stime->observer, &stime->target, &et, &px, &py, &pz, &vx, &vy, &vz);

         radius = sqrt ((px*px) + (py*py) + (pz*pz));

         velXYZ [0] = vx;
         velXYZ [1] = vy;
         velXYZ [2] = vz;

         x = (px/stime->radius);
         y = (py/stime->radius);
         z = (pz/stime->radius);
      }
      else
      {
         if (stime->target == Sun)
            spkez (stime->observer, et, frame2, aberr2, stime->target, sc_pos, &lt);
         else
            spkez (stime->target, et, frame1, aberr, stime->observer, sc_pos, &lt);

         for (index = 0; index < 3; index++)
         {
            pos [index] = sc_pos [index];
            vel [index] = sc_pos [(index + 3)];
         }

         if (stime->target == Sun)
         {
            reclat (pos, &radius, &longitude, &latitude);

            velXYZ [0] = vel [0];
            velXYZ [1] = vel [1];
            velXYZ [2] = vel [2];
         }
         else
         {
            vminus (pos, sc_pos2);
            gsetrn_g (stime->target, (et - lt), frame1, aberr, matrix);
            mxv (matrix, sc_pos2, pos);
            reclat (pos, &radius, &longitude, &latitude);

            vminus (vel, sc_pos2);
            mxv (matrix, sc_pos2, velXYZ);
         }

         x = (pos [0]/stime->radius);
         y = (pos [1]/stime->radius);
         z = (pos [2]/stime->radius);
      }

      radius = (radius/stime->radius);

      Vmag = sqrt (((velXYZ [0]*velXYZ [0]) +
                    (velXYZ [1]*velXYZ [1]) +
                    (velXYZ [2]*velXYZ [2])));

      printf ("%04d %03d %02d %02d %06.3f %13.3f %13.3f %13.3f %13.3f %13.3f %13.3f %13.3f %13.3f\n",
               year, doy, hr, mn, (float) sec,
               x, y, z, radius, velXYZ [0], velXYZ [1], velXYZ [2], Vmag);
   }

   return (no_data);
}

double GetLongitudeSLS3 (st, CurrentTime)
struct time_struc *st;
double CurrentTime;
{
   int past, year, doy, hr, mn;
   int on_yr, on_dy, on_hr, on_mn;
   float on_ss;
   double lt, et;
   double loctime;
   double sec, OWLT;
   double ElapsedTimeL, ElapsedTimeLSq, ElapsedTimeLCu, OldNewDivideL;
   double ElapsedTimeL4, ElapsedTimeL5;
   double AltLongitudeL, SunLongitudeL;
   double term1, term2, term3, term4, term5, fmodConst;
   double ConstA, ConstB, ConstC, ConstD, ConstE, ConstF;
   char scstr [80];

   ConstA = 86.6681;
   ConstB = -2.7537;
   ConstC =  4.7730e-03;
   ConstD = -4.8755e-06;
   ConstE =  3.5653e-09;
   ConstF = -9.1485e-13;

   fmodConst = 360.0;

   on_yr = 2004;
   on_dy =    1;
   on_hr =    0;
   on_mn =    0;
   on_ss =  0.0;

   OldNewDivideL = GetTime (on_yr, on_dy, on_hr, on_mn, on_ss);

   ms2dhms (&past, &hr, &mn, &sec, CurrentTime);
   yrdy1958 (&year, &doy, past);

   sprintf (scstr, "%04d %03d // %02d:%02d:%06.3f",
            year, doy, hr, mn, (float) sec);

   utc2et (scstr, &et);

   sls2_sls3_ (&st->observer, &et, &loctime, &lt);

   OWLT = lt*1000.0;

   ElapsedTimeL = (CurrentTime - OldNewDivideL - OWLT);
   ElapsedTimeL /=  1000.0;
   ElapsedTimeL /= 86400.0;

   ElapsedTimeLSq = (ElapsedTimeL*ElapsedTimeL);
   ElapsedTimeLCu = (ElapsedTimeL*ElapsedTimeL*ElapsedTimeL);
   ElapsedTimeL4 =  (ElapsedTimeL*ElapsedTimeL*ElapsedTimeL*ElapsedTimeL);
   ElapsedTimeL5 =  (ElapsedTimeL*ElapsedTimeL*ElapsedTimeL*ElapsedTimeL*ElapsedTimeL);

   term1 = (ConstB*ElapsedTimeL);
   term2 = (ConstC*ElapsedTimeLSq);
   term3 = (ConstD*ElapsedTimeLCu);
   term4 = (ConstE*ElapsedTimeL4);
   term5 = (ConstF*ElapsedTimeL5);

   SunLongitudeL = -(ConstA + term1 + term2 + term3 + term4 + term5) + 360.*(1.0/0.4497)*ElapsedTimeL;

   SunLongitudeL += 100.0;   /* add 100 degress to it */

   if (SunLongitudeL < 0.0)
   {
      do
      {
         SunLongitudeL += 360.0;
      }
      while (SunLongitudeL < 0.0);
   }

   SunLongitudeL = fmod (SunLongitudeL, fmodConst);

   term1 = (12.0 - loctime);
   term1 *= 15.0;

   AltLongitudeL = (SunLongitudeL + term1);

   if (AltLongitudeL < 0.0)
   {
      do
      {
         AltLongitudeL += 360.0;
      }
      while (AltLongitudeL < 0.0);
   }

   AltLongitudeL = fmod (AltLongitudeL, fmodConst);

   return (AltLongitudeL);
}

double GetLongitudeSLS2 (st, CurrentTime)
struct time_struc *st;
double CurrentTime;
{
   int past, year, doy, hr, mn;
   int on_yr, on_dy, on_hr, on_mn;
   float on_ss;
   double lt, et;
   double loctime;
   double sec, OWLT;
   double ElapsedTimeL, ElapsedTimeLSq, ElapsedTimeLCu, OldNewDivideL;
   double AltLongitudeL, SunLongitudeL;
   double term1, term2, term3, fmodConst;
   double ConstD1, ConstD2, ConstD3, ConstD4;
   double TBar, TPrime, TPrimeSq, TPrimeCu;
   char scstr [80];

   ConstD1 = -514.11433;
   ConstD2 =    0.089561911;

   ConstD3 =    1.7424768e-03;
   ConstD4 =   -7.913e-07;

   fmodConst = 360.0;

   TBar = 547.0;

   ms2dhms (&past, &hr, &mn, &sec, CurrentTime);
   yrdy1958 (&year, &doy, past);

   sprintf (scstr, "%04d %03d // %02d:%02d:%06.3f",
            year, doy, hr, mn, (float) sec);

   utc2et (scstr, &et);

   sls2_sls3_ (&st->observer, &et, &loctime, &lt);

   OWLT = lt*1000.0;

   on_yr = 2004;
   on_dy =    1;
   on_hr =    0;
   on_mn =    0;
   on_ss =  0.0;

   OldNewDivideL = GetTime (on_yr, on_dy, on_hr, on_mn, on_ss);

   ElapsedTimeL = (CurrentTime - OldNewDivideL - OWLT);
   ElapsedTimeL /=  1000.0;
   ElapsedTimeL /= 86400.0;

   TPrime = ElapsedTimeL;
   TPrime -= TBar;

   TPrimeSq = (TPrime*TPrime);
   TPrimeCu = (TPrime*TPrime*TPrime);

   term1 = (ConstD2*TPrime);

   term2 = (ConstD3*TPrimeSq);

   term3 = (ConstD4*TPrimeCu);

   SunLongitudeL = -(ConstD1 + term1 + term2 + term3) + 360.0*(1.0/0.4497)*ElapsedTimeL;

   SunLongitudeL += 100.0;    /* add 100 degress to it */
	    
   if (SunLongitudeL < 0.0)
   {
      do
      {
         SunLongitudeL += 360.0;
      }
      while (SunLongitudeL < 0.0);
   }

   SunLongitudeL = fmod (SunLongitudeL, fmodConst);

   term1 = (12.0 - loctime);
   term1 *= 15.0;

   AltLongitudeL = (SunLongitudeL + term1);

   if (AltLongitudeL < 0.0)
   {
      do
      {
         AltLongitudeL += 360.0;
      }
      while (AltLongitudeL < 0.0);
   }

   AltLongitudeL = fmod (AltLongitudeL, fmodConst);

   return (AltLongitudeL);
}

int GeographicCoords (stime)
struct time_struc *stime;
{
   int index, no_data;
   int past, year, month, day, doy, hr, mn;
   int on_yr, on_dy, on_hr, on_mn;
   int target, observer;
   float on_ss;
   double mlt;
   double matrix [3][3], matrix2 [3][3], tipm [3][3];
   double sun_pos [6], sun_pos2 [3], x_vec [3], y_vec [3], z_vec [3];
   double sc_pos [6], sc_pos2 [3], pos [3], position [3];
   double obs_lt, lt, et, radius, longitude, latitude, dummy1, dummy2;
   double mlat, sunlong, loctime;
   double io_longitude, theta_sc, theta_gm, theta_su, gamma;
   double delta, sec, st_ms;
   double ConLongitude, SKRLongitude;
   double Model_SLS2_Longitude;
   double Model_SLS3_Longitude;
   double SKR_lt;
   double StModelSLS2, SpModelSLS2;
   double StModelSLS3, SpModelSLS3;
   double temp1, temp2, LValue, RLatitude;
   double NearPointAltitude;
   double EarthRadius, EarthMLat, EarthMLT, EarthLValue;
   double JupiterLValue;
   double GalileoRadius;
   double GalileoLongitude;
   double altitude;
   double JupiterAltitude;
   char scstr [80];
   char *StModelSLS2Name = "2004-001T00:00:00";
   char *SpModelSLS2Name = "2006-240T00:00:00";
   char *StModelSLS3Name = "2004-001T00:00:00";
   char *SpModelSLS3Name = "2007-222T23:59:59";
   char *frame = "J2000";
   char *aberr1 = "NONE";
   char *aberr2 = "LT+S";

   no_data = True;

   if (!(ValidateTime (stime)))
      return (True);

   if ((stime->target == SaturnBC) || (stime->target == Saturn))
   {
      parsetime (StModelSLS2Name, &year, &month, &day, &doy, &hr, &mn, &sec);

      StModelSLS2 = GetTime (year, doy, hr, mn, (float) sec);

      parsetime (SpModelSLS2Name, &year, &month, &day, &doy, &hr, &mn, &sec);

      SpModelSLS2 = GetTime (year, doy, hr, mn, (float) sec);
   }

   if ((stime->target == SaturnBC) || (stime->target == Saturn))
   {
      parsetime (StModelSLS3Name, &year, &month, &day, &doy, &hr, &mn, &sec);

      StModelSLS3 = GetTime (year, doy, hr, mn, (float) sec);

      parsetime (SpModelSLS3Name, &year, &month, &day, &doy, &hr, &mn, &sec);

      SpModelSLS3 = GetTime (year, doy, hr, mn, (float) sec);
   }

   st_ms = stime->st_total_ms;
   delta = (stime->deltatee*1000.0);

   while (st_ms <= stime->sp_total_ms)
   {
      if (no_data)
      {
         no_data = False;
         PrintLabels (stime);
      }

      ms2dhms (&past, &hr, &mn, &sec, st_ms);
      yrdy1958 (&year, &doy, past);

      sprintf (scstr, "%04d %03d // %02d:%02d:%06.3f",
               year, doy, hr, mn, (float) sec);

      utc2et (scstr, &et);

      /* determine latitude and longitude based on Voyager SKR data */
      spkez (stime->target, et, frame, aberr2, stime->observer, sc_pos, &lt);

      for (index = 0; index < 3; index++)
         pos [index] = sc_pos [index];

      vminus (pos, sc_pos2);

      if (stime->target == Venus)
         bodmat (Venus, (et - lt), matrix);
      else if (stime->target == Earth)
         bodmat (Earth, (et - lt), matrix);
      else if (stime->target == Jupiter)
         bodmat (Jupiter, (et - lt), matrix);
      else if (stime->target == SaturnBC)
         bodmat (Saturn, (et - lt), matrix);
      else
         bodmat (stime->target, (et - lt), matrix);

      mxv (matrix, sc_pos2, position);

      reclat (position, &radius, &longitude, &latitude);

      if (longitude < 0.0)
         longitude += 2.0*PI;

      radius = (radius/stime->radius);

      latitude = (latitude*DPR);

      longitude = (longitude*DPR);
      longitude = (360.0 - longitude);

      ConLongitude = longitude;

      /* determine spacecraft longitude to determine local time */
      spkez (stime->target, et, frame, aberr1, stime->observer, sc_pos, &lt);

      spkez (stime->target, (et - lt), frame, aberr1, stime->observer, sc_pos, &dummy1);

      SKR_lt = lt;

      for (index = 0; index < 3; index++)
         pos [index] = sc_pos [index];

      vminus (pos, sc_pos2);

      if (stime->target == Venus)
         bodmat (Venus, (et - SKR_lt), matrix);
      else if (stime->target == Earth)
         bodmat (Earth, (et - SKR_lt), matrix);
      else if (stime->target == Jupiter)
         bodmat (Jupiter, (et - SKR_lt), matrix);
      else if (stime->target == SaturnBC)
         bodmat (Saturn, (et - SKR_lt), matrix);
      else
         bodmat (stime->target, (et - SKR_lt), matrix);

      mxv (matrix, sc_pos2, position);

      reclat (position, &dummy1, &longitude, &dummy2);

      if (longitude < 0.0)
         longitude += 2.0*PI;

      longitude = (longitude*DPR);
      longitude = (360.0 - longitude);

      SKRLongitude = longitude;

      if ((stime->target == SaturnBC) || (stime->target == Saturn))
      {
         if ((st_ms >= StModelSLS2) && (st_ms < SpModelSLS2))
            Model_SLS2_Longitude = GetLongitudeSLS2 (stime, st_ms);
         else
            Model_SLS2_Longitude = -1.0;

         if ((st_ms >= StModelSLS3) && (st_ms <= SpModelSLS3))
            Model_SLS3_Longitude = GetLongitudeSLS3 (stime, st_ms);
         else
            Model_SLS3_Longitude = -1.0;

         RLatitude = (latitude*RPD);
         temp1 = cos (RLatitude);
         temp2 = (temp1*temp1);

         LValue = (radius/temp2);
      }

      if (((stime->target == SaturnBC) || (stime->target == Saturn)) && (stime->observer == Cassini))
      {
         observer = Cassini;
         target = Saturn;

         get_altitude_s_ (&target, &observer, &et, &altitude, &dummy1);
      }

      /* determine system III longitude of the Sun */
      spkez (stime->target, (et - SKR_lt), frame, aberr1, Sun, sc_pos, &dummy1);

      for (index = 0; index < 3; index++)
         pos [index] = sc_pos [index];

      vminus (pos, sc_pos2);

      mxv (matrix, sc_pos2, position);

      reclat (position, &dummy1, &sunlong, &dummy2);

      if (sunlong < 0.0)
         sunlong += 2.0*PI;

      sunlong = (sunlong*DPR);
      sunlong = (360.0 - sunlong);

      /* determine local time */
      loctime = (sunlong - SKRLongitude);
      loctime += 180.0;
      loctime = (loctime/15.0);

      if (loctime < 0.0)
         loctime += 24.0;

      if (loctime >= 24.0)
         loctime -= 24.0;

      if (stime->target == Jupiter)
      {
         GalileoRadius = radius;

         GalileoLongitude = longitude;

         observer = stime->observer;

         target = stime->target;

         get_jupiter_mag_coords_ (&observer, &target, &et, &radius, &longitude, &mlat, &mlt, &JupiterLValue, &gamma);

         radius = (radius/stime->radius);

         if (stime->observer == Juno)
            get_altitude_j_ (&target, &observer, &et, &JupiterAltitude, &dummy1);

         if (stime->observer == Galileo)
         {
            /* determine phase of Io */
            spkez (Sun, (et - SKR_lt), frame, aberr1, stime->target, sun_pos, &dummy1);

            for (index = 0; index < 3; index++)
               pos [index] = sun_pos [index];

            vhat (pos, sun_pos2);
   
            bodmat (Jupiter, (et - SKR_lt), tipm);

            for (index = 0; index < 3; index++)
            {
               x_vec [index] = sun_pos2 [index];
               z_vec [index] = tipm [2][index];
            }
   
            ucrss (z_vec, x_vec, y_vec);
            ucrss (y_vec, z_vec, x_vec);

            /* the rotation matrix for the Jupiter Solar Equatorial system */
            for (index = 0; index < 3; index++)
            {
               matrix2 [index][0]   = x_vec [index];
               matrix2 [index][1]   = y_vec [index];
               matrix2 [index][2]   = z_vec [index];
            }

            /* find the longitude of the observer */
            spkez (stime->target, et, frame, aberr2, stime->observer, sc_pos, &lt);

            obs_lt = lt;

            for (index = 0; index < 3; index++)
               pos [index] = sc_pos [index]; 

            vminus (pos, sc_pos2);
            mtxv (matrix2, sc_pos2, pos);

            reclat (pos, &dummy1, &io_longitude, &dummy2);

            if (io_longitude < 0.0)
               io_longitude += 2.0*PI;

            io_longitude = (io_longitude*DPR);
            io_longitude = (360.0 - io_longitude);

            theta_sc = io_longitude;

            /* find the longitude of Io */
            spkez (stime->target, (et - obs_lt), frame, aberr2, Io, sc_pos, &lt);

            for (index = 0; index < 3; index++)
               pos [index] = sc_pos [index]; 

            vminus (pos, sc_pos2);
            mtxv (matrix2, sc_pos2, pos);

            reclat (pos, &dummy1, &io_longitude, &dummy2);

            if (io_longitude < 0.0)
               io_longitude += 2.0*PI;

            io_longitude = (io_longitude*DPR);
            io_longitude = (360.0 - io_longitude);

            theta_gm = io_longitude;

            theta_su = (theta_sc + 180.0);

            if (theta_su > 360.0)
               theta_su = (theta_su - 360.0);

            gamma = (360.0 - theta_gm);
            gamma = (gamma + theta_su);

            /* gamma is the phase angle of Io */
            if (gamma >= 360.0)
               gamma = (gamma - 360.0);

            radius = GalileoRadius;

            longitude = GalileoLongitude;
         }
      }

      if (stime->target == Jupiter)
      {
         if (stime->observer == Juno)
         {
            printf ("%04d %03d %02d %02d %06.3f ",
                    year, doy, hr, mn, (float) sec);

            if (JupiterAltitude < 100000.0)
               printf ("%15.3f %15.3f %15.3f %15.3f %15.3f %15.3f %15.3f %12.3f\n",
                        longitude, latitude, mlat, mlt, radius, JupiterAltitude, JupiterLValue, gamma);
            else
               printf ("%15.3f %15.3f %15.3f %15.3f %15.3f %15.3e %15.3f %12.3f\n",
                        longitude, latitude, mlat, mlt, radius, JupiterAltitude, JupiterLValue, gamma);
         }
         else
         {
            printf ("%04d %03d %02d %02d %06.3f ",
                    year, doy, hr, mn, (float) sec);

            printf ("%15.3f %15.3f %15.3f %15.3f %15.3f %15.3f %12.3f\n",
                     (float) longitude, (float) latitude,
                     (float) mlat, (float) mlt,
                     (float) radius, (float) JupiterLValue, (float) gamma);
         }
      }
      else if (((stime->target == SaturnBC) || (stime->target == Saturn)) && (stime->observer != Cassini))
      {
         printf ("%04d %03d %02d %02d %06.3f %14.3f %17.3f %17.3f %17.3f %13.3f %15.3f %15.3f\n",
                  year, doy, hr, mn, (float) sec,
                  (float) ConLongitude, (float) Model_SLS2_Longitude,
                  (float) Model_SLS3_Longitude,
                  (float) latitude, (float) LValue,
                  (float) loctime, (float) radius);
      }
      else if (((stime->target == SaturnBC) || (stime->target == Saturn)) && (stime->observer == Cassini))
      {
         if (altitude < 100000.0)
            printf ("%04d %03d %02d %02d %06.3f %14.3f %17.3f %17.3f %17.3f %13.3f %15.3f %15.3f %15.3f\n",
                     year, doy, hr, mn, (float) sec,
                     (float) ConLongitude, (float) Model_SLS2_Longitude,
                     (float) Model_SLS3_Longitude,
                     (float) latitude, (float) LValue,
                     (float) loctime, (float) altitude, (float) radius);
         else
            printf ("%04d %03d %02d %02d %06.3f %14.3f %17.3f %17.3f %17.3f %13.3f %15.3f %15.3e %15.3f\n",
                     year, doy, hr, mn, (float) sec,
                     (float) ConLongitude, (float) Model_SLS2_Longitude,
                     (float) Model_SLS3_Longitude,
                     (float) latitude, (float) LValue,
                     (float) loctime, (float) altitude, (float) radius);
      }
      else if ((stime->observer != Juno) &&
               ((stime->target == Io)        ||
                (stime->target == Europa)    ||
                (stime->target == Ganymede)  ||
                (stime->target == Callisto)  ||
                (stime->target == Mimas)     ||
                (stime->target == Enceladus) ||
                (stime->target == Tethys)    ||
                (stime->target == Dione)     ||
                (stime->target == Rhea)      ||
                (stime->target == Titan)     ||
                (stime->target == Hyperion)  ||
                (stime->target == Iapetus)   ||
                (stime->target == Phoebe)    ||
                (stime->target == Methone)   ||
                (stime->target == Anthe)     ||
                (stime->target == Pallene)   ||
                (stime->target == Telesto)   ||
                (stime->target == Helene)))
      {
         ms2dhms (&past, &hr, &mn, &sec, st_ms);
         yrdy1958 (&year, &doy, past);

         sprintf (scstr, "%04d %03d // %02d:%02d:%06.3f",
                  year, doy, hr, mn, (float) sec);

         utc2et (scstr, &et);

         target = stime->target;
         observer = stime->observer;

         get_altitude_ (&target, &observer, &et, &NearPointAltitude);

         printf ("%04d %03d %02d %02d %06.3f %15.3f %15.3f %15.3f %20.3f %15.3f\n",
                  year, doy, hr, mn, (float) sec,
                  (float) longitude, (float) latitude,
                  (float) loctime, NearPointAltitude,
                  (float) radius);
      }
      else if (stime->target == Earth)
      {
         get_geo_ (&stime->observer, &stime->target, &stime->BodyName, &et, &EarthRadius, &EarthMLat, &EarthLValue, &EarthMLT);

         printf ("%04d %03d %02d %02d %06.3f %15.3f %15.3f %15.3f %15.3f %15.3f %15.3f %15.3f\n",
                  year, doy, hr, mn, (float) sec,
                  (float) longitude, (float) latitude,
                  (float) loctime, (float) EarthMLat,
                  (float) EarthMLT, (float) EarthLValue,
                  (float) EarthRadius);
      }
      else
      {
         printf ("%04d %03d %02d %02d %06.3f %15.3f %15.3f %15.3f %15.3f\n",
                  year, doy, hr, mn, (float) sec,
                  (float) longitude, (float) latitude,
                  (float) loctime, (float) radius);
      }

      st_ms += delta;
   }

   return (no_data);
}

int EquatorialCoords (stime)
struct time_struc *stime;
{
   int index, no_data;
   int past, year, month, day, doy, hr, mn;
   double matrix [3][3], tipm [3][3];
   double sun_pos [6], sun_pos2 [3], x_vec [3], y_vec [3], z_vec [3];
   double sc_pos [6], sc_pos2 [3], pos [3], vel [3], velXYZ [3], Vmag;
   double lt, et, radius, longitude, latitude, x, y, z;
   double delta, sec, st_ms;
   double px, py, pz;
   double vx, vy, vz;
   double dummy;
   char scstr [80];
   char *frame = "J2000";
   char *aberr1 = "LT+S";
   char *aberr2 = "NONE";

   no_data = True;

   if (!(ValidateTime (stime)))
      return (True);

   st_ms = stime->st_total_ms;
   delta = (stime->deltatee*1000.0);

   while (st_ms <= stime->sp_total_ms)
   {
      if (no_data)
      {
         no_data = False;
         PrintLabels (stime);
      }

      ms2dhms (&past, &hr, &mn, &sec, st_ms);
      yrdy1958 (&year, &doy, past);

      st_ms += delta;

      sprintf (scstr, "%04d %03d // %02d:%02d:%06.3f",
               year, doy, hr, mn, (float) sec);

      utc2et (scstr, &et);

      if ((stime->target == Jupiter) && (stime->observer == Juno))
      {
         get_solar_eq_ (&stime->observer, &stime->target, &et, &px, &py, &pz, &vx, &vy, &vz);

         radius = sqrt ((px*px) + (py*py) + (pz*pz));

         x = (px/stime->radius);
         y = (py/stime->radius);
         z = (pz/stime->radius);

         velXYZ [0] = vx;
         velXYZ [1] = vy;
         velXYZ [2] = vz;
      }
      else
      {
         if (stime->target == Sun)
            spkez (Sun, et, frame, aberr2, stime->target, sun_pos, &lt);
         else
         {
            spkez (stime->target, et, frame, aberr1, stime->observer, sc_pos, &lt);

            spkez (Sun, (et - lt), frame, aberr2, stime->target, sun_pos, &dummy);
         }

         for (index = 0; index < 3; index++)
            pos [index] = sun_pos [index];

         vhat (pos, sun_pos2);
   
         if (stime->target == Sun)
            spkez (stime->observer, et, frame, aberr1, stime->target, sc_pos, &lt);
         else
            spkez (stime->target, et, frame, aberr1, stime->observer, sc_pos, &lt);

         for (index = 0; index < 3; index++)
         {
            pos [index] = sc_pos [index]; 
            vel [index] = sc_pos [(index + 3)];
         }

         if (stime->target == Venus)
            bodmat (Venus, (et- lt), tipm);
         else if (stime->target == Earth)
            bodmat (Earth, (et- lt), tipm);
         else if (stime->target == Jupiter)
            bodmat (Jupiter, (et- lt), tipm);
         else if (stime->target == SaturnBC)
            bodmat (Saturn, (et - lt), tipm);
         else
            bodmat (stime->target, (et - lt), tipm);

         for (index = 0; index < 3; index++)
         {
            x_vec [index] = sun_pos2 [index];
            z_vec [index] = tipm [2][index];
         }
   
         ucrss (z_vec, x_vec, y_vec);
         ucrss (y_vec, z_vec, x_vec);

         for (index = 0; index < 3; index++)
         {
            matrix [index][0]   = x_vec [index];
            matrix [index][1]   = y_vec [index];
            matrix [index][2]   = z_vec [index];
         }

         if (stime->target == Sun)
         {
            reclat (pos, &radius, &longitude, &latitude);

            velXYZ [0] = vel [0];
            velXYZ [1] = vel [1];
            velXYZ [2] = vel [2];
         }
         else
         {
            vminus (pos, sc_pos2);
            mtxv (matrix, sc_pos2, pos);
            reclat (pos, &radius, &longitude, &latitude);

            vminus (vel, sc_pos2);
            mtxv (matrix, sc_pos2, velXYZ);
         }

         x = (pos [0]/stime->radius);
         y = (pos [1]/stime->radius);
         z = (pos [2]/stime->radius);
      }

      Vmag = sqrt (((velXYZ [0]*velXYZ [0]) +
                    (velXYZ [1]*velXYZ [1]) +
                    (velXYZ [2]*velXYZ [2])));

      radius = (radius/stime->radius);

      printf ("%04d %03d %02d %02d %06.3f %13.3f %13.3f %13.3f %13.3f %13.3f %13.3f %13.3f %13.3f\n",
               year, doy, hr, mn, (float) sec,
               x, y, z, radius, velXYZ [0], velXYZ [1], velXYZ [2], Vmag);
   }

   return (no_data);
}

int main (int argc, char *argv [])
{
   int index, x, cl;
   int year, month, day, doy, hr, mn, past;
   double st_ms, sp_ms, sec, NewRadius;
   char st_string [80], sp_string [80], temp [80];
   char *dummy;
   char *SolarName = "SolarSystemKernels.list";
   char *VoyagerName = "VoyagerKernels.list";
   char *GalileoName = "GalileoKernels.list";
   char *UlyssesName = "UlyssesKernels.list";
   char *CassiniName = "CassiniKernels.list";
   char *JunoName    = "JunoKernels.list";
   struct time_struc st_time;
   entry entries [MAXENTRIES];

   if (strcmp (getenv ("REQUEST_METHOD"), "POST"))
   {
      printf ("This script should be referenced with a METHOD of POST.\n");
      printf ("If you don't understand this, see this ");
      printf ("<A HREF=\"http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/fill-out-forms/overview.html\">forms overview</A>.%c",10);
      exit (1);
   }

   if (strcmp (getenv ("CONTENT_TYPE"), "application/x-www-form-urlencoded"))
   {
      printf ("This script can only be used to decode form results. \n");
      exit (1);
   }

   cl = atoi (getenv ("CONTENT_LENGTH"));

   for (x = 0; cl && (!feof (stdin)); x++)
   {
      entries [x].val = fmakeword (stdin, '&', &cl);
      plustospace (entries [x].val);
      unescape_url (entries [x].val);
      entries [x].name = makeword (entries [x].val,'=');
   }

   sprintf (temp, "%s", entries [0].val);

   if (parsetime (temp, &year, &month, &day, &doy, &hr, &mn, &sec))
   {
      printf ("Content-type: text/plain\n\n");
      printf ("%s: error parsing %s\n", argv [0], temp);
      exit (1);
   }

   sprintf (st_string, "%04d-%02d-%02dT%02d:%02d:%f",
            year, month, day, hr, mn, (float) sec);

   past = past_1958 (year, doy);
   st_ms = (double) past*86400000.0 +
           (double) hr*3600000.0 +
           (double) mn*60000.0 +
           (double) sec*1000.0;

   st_time.st_past = past;
   st_time.st_year = year;
   st_time.st_day = doy;
   st_time.st_hr = hr;
   st_time.st_mn = mn;
   st_time.st_sec = (float) sec;
   st_time.st_total_ms = st_ms;

   sprintf (temp, "%s", entries [1].val);

   if (parsetime (temp, &year, &month, &day, &doy, &hr, &mn, &sec))
   {
      printf ("Content-type: text/plain\n\n");
      printf ("%s: error parsing %s\n", argv [0], temp);
      exit (1);
   }

   sprintf (sp_string, "%04d-%02d-%02dT%02d:%02d:%f",
            year, month, day, hr, mn, (float) sec);

   past = past_1958 (year, doy);
   sp_ms = (double) past*86400000.0 +
           (double) hr*3600000.0 +
           (double) mn*60000.0 +
           (double) sec*1000.0;

   st_time.sp_past = past;
   st_time.sp_year = year;
   st_time.sp_day = doy;
   st_time.sp_hr = hr;
   st_time.sp_mn = mn;
   st_time.sp_sec = (float) sec;
   st_time.sp_total_ms = sp_ms;

   sprintf (temp, "%s", entries [2].val);

   st_time.deltatee = (double) strtod (temp, &dummy);

   if (st_time.deltatee <= 0.0)
   {
      printf ("Content-type: text/plain\n\n");
      printf ("Invalid time interval entry:  %s\n", temp);
      return (0);
   }

   if (sp_ms <= st_ms)
   {
      printf ("Content-type: text/plain\n\n");
      printf ("Invalid time range selection:  %s to %s\n",
              st_string, sp_string);
      return (0);
   }

   sprintf (temp, "%s", entries [3].val);

   if      (!strcmp (temp, " sun"))
   {
      st_time.target = Sun;
      st_time.radius = RAU;
   }
   else if (!strcmp (temp, "venu"))
   {
      st_time.target = Venus;
      st_time.radius = RVenus;
   }
   else if (!strcmp (temp, "eart"))
   {
      st_time.target = Earth;
      st_time.BodyName = Earth;
      st_time.radius = REarth;
   }
   else if (!strcmp (temp, "jupi"))
   {
      st_time.target = Jupiter;
      st_time.radius = RJupiter;
   }
   else if (!strcmp (temp, "  io"))
   {
      st_time.target = Io;
      st_time.radius = RIo;
   }
   else if (!strcmp (temp, "euro"))
   {
      st_time.target = Europa;
      st_time.radius = REuropa;
   }
   else if (!strcmp (temp, "gany"))
   {
      st_time.target = Ganymede;
      st_time.radius = RGanymede;
   }
   else if (!strcmp (temp, "call"))
   {
      st_time.target = Callisto;
      st_time.radius = RCallisto;
   }
   else if (!strcmp (temp, "sat1"))
   {
      st_time.target = SaturnBC;
      st_time.radius = RSaturn;
   }
   else if (!strcmp (temp, "sat2"))
   {
      st_time.target = Saturn;
      st_time.radius = RSaturn;
   }
   else if (!strcmp (temp, "mima"))
   {
      st_time.target = Mimas;
      st_time.radius = RMimas;
   }
   else if (!strcmp (temp, "ence"))
   {
      st_time.target = Enceladus;
      st_time.radius = REnceladus;
   }
   else if (!strcmp (temp, "teth"))
   {
      st_time.target = Tethys;
      st_time.radius = RTethys;
   }
   else if (!strcmp (temp, "dion"))
   {
      st_time.target = Dione;
      st_time.radius = RDione;
   }
   else if (!strcmp (temp, "rhea"))
   {
      st_time.target = Rhea;
      st_time.radius = RRhea;
   }
   else if (!strcmp (temp, "tita"))
   {
      st_time.target = Titan;
      st_time.radius = RTitan;
   }
   else if (!strcmp (temp, "hype"))
   {
      st_time.target = Hyperion;
      st_time.radius = RHyperion;
   }
   else if (!strcmp (temp, "iape"))
   {
      st_time.target = Iapetus;
      st_time.radius = RIapetus;
   }
   else if (!strcmp (temp, "phoe"))
   {
      st_time.target = Phoebe;
      st_time.radius = RPhoebe;
   }
   else if (!strcmp (temp, "meth"))
   {
      st_time.target = Methone;
      st_time.radius = RMethone;
   }
   else if (!strcmp (temp, "anth"))
   {
      st_time.target = Anthe;
      st_time.radius = RAnthe;
   }
   else if (!strcmp (temp, "pall"))
   {
      st_time.target = Pallene;
      st_time.radius = RPallene;
   }
   else if (!strcmp (temp, "tele"))
   {
      st_time.target = Telesto;
      st_time.radius = RTelesto;
   }
   else if (!strcmp (temp, "hele"))
   {
      st_time.target = Helene;
      st_time.radius = RHelene;
   }
   else
   {
      printf ("%s: unknown target %s\n", argv [0], temp);
      exit (1);
   }

   sprintf (temp, "%s", entries [4].val);

   if      (!strcmp (temp, "eart"))
      st_time.observer = Earth;
   else if (!strcmp (temp, "venu"))
      st_time.observer = Venus;
   else if (!strcmp (temp, "jupi"))
      st_time.observer = Jupiter;
   else if (!strcmp (temp, "  io"))
      st_time.observer = Io;
   else if (!strcmp (temp, "euro"))
      st_time.observer = Europa;
   else if (!strcmp (temp, "gany"))
      st_time.observer = Ganymede;
   else if (!strcmp (temp, "call"))
      st_time.observer = Callisto;
   else if (!strcmp (temp, "sat1"))
      st_time.observer = SaturnBC;
   else if (!strcmp (temp, "sat2"))
      st_time.observer = Saturn;
   else if (!strcmp (temp, "mima"))
      st_time.observer = Mimas;
   else if (!strcmp (temp, "ence"))
      st_time.observer = Enceladus;
   else if (!strcmp (temp, "teth"))
      st_time.observer = Tethys;
   else if (!strcmp (temp, "dion"))
      st_time.observer = Dione;
   else if (!strcmp (temp, "rhea"))
      st_time.observer = Rhea;
   else if (!strcmp (temp, "tita"))
      st_time.observer = Titan;
   else if (!strcmp (temp, "hype"))
      st_time.observer = Hyperion;
   else if (!strcmp (temp, "iape"))
      st_time.observer = Iapetus;
   else if (!strcmp (temp, "phoe"))
      st_time.observer = Phoebe;
   else if (!strcmp (temp, "meth"))
      st_time.observer = Methone;
   else if (!strcmp (temp, "anth"))
      st_time.observer = Anthe;
   else if (!strcmp (temp, "pall"))
      st_time.observer = Pallene;
   else if (!strcmp (temp, "tele"))
      st_time.observer = Telesto;
   else if (!strcmp (temp, "hele"))
      st_time.observer = Helene;
   else if (!strcmp (temp, "voy1"))
      st_time.observer = Voyager1;
   else if (!strcmp (temp, "voy2"))
      st_time.observer = Voyager2;
   else if (!strcmp (temp, "gali"))
      st_time.observer = Galileo;
   else if (!strcmp (temp, "ulys"))
      st_time.observer = Ulysses;
   else if (!strcmp (temp, "cass"))
      st_time.observer = Cassini;
   else if (!strcmp (temp, "juno"))
      st_time.observer = Juno;
   else
   {
      printf ("%s: unknown observer %s\n", argv [0], temp);
      exit (1);
   }

   if (st_time.target == st_time.observer)
   {
      printf ("Content-type: text/plain\n\n");
      printf ("Choosing the same object as the observer \n");
      printf ("and the origin is not a valid choice.\n");
      exit (1);
   }

   sprintf (temp, "%s", entries [5].val);

   if      (!strcmp (temp, "geog"))
      st_time.coord = Geographic;
   else if (!strcmp (temp, "ecli"))
      st_time.coord = Ecliptic;
   else if (!strcmp (temp, "equa"))
      st_time.coord = Equatorial;
   else if (!strcmp (temp, "crot"))
      st_time.coord = CoRotational;
   else if (!strcmp (temp, "khsm"))
      st_time.coord = SaturnSolarMagnetic;
   else
   {
      printf ("Content-type: text/plain\n\n");
      printf ("%s: unknown coordinate system %s\n", argv [0], temp);
      exit (1);
   }

   sprintf (temp, "%s", entries [7].val);

   NewRadius = (double) strtod (temp, &dummy);
   st_time.radius = NewRadius;

   if ((st_time.observer == Voyager1) || (st_time.observer == Voyager2))
      furnsh_ (VoyagerName, strlen (VoyagerName));
   else if (st_time.observer == Galileo)
      furnsh_ (GalileoName, strlen (GalileoName));
   else if (st_time.observer == Ulysses)
      furnsh_ (UlyssesName, strlen (UlyssesName));
   else if (st_time.observer == Cassini)
      furnsh_ (CassiniName, strlen (CassiniName));
   else if (st_time.observer == Juno)
      furnsh_ (JunoName, strlen (JunoName));
   else if ((st_time.observer == Mimas)    || (st_time.observer == Enceladus) ||
            (st_time.observer == Tethys)   || (st_time.observer == Dione)     ||
            (st_time.observer == Rhea)     || (st_time.observer == Titan)     ||
            (st_time.observer == Hyperion) || (st_time.observer == Iapetus)   ||
            (st_time.observer == Phoebe)   || (st_time.observer == Methone)   ||
            (st_time.observer == Anthe)    || (st_time.observer == Pallene)   ||
            (st_time.observer == Telesto)  || (st_time.observer == Helene))
      furnsh_ (CassiniName, strlen (CassiniName));
   else if ((st_time.observer == Io)       || (st_time.observer == Europa) ||
            (st_time.observer == Ganymede) || (st_time.observer == Callisto))
      furnsh_ (GalileoName, strlen (GalileoName));
   else
      furnsh_ (SolarName, strlen (SolarName));

   if (st_time.target == Earth)
      ldpool ("rbsp_general010.tf");

   if (st_time.coord == Geographic)
      GeographicCoords (&st_time);
   else if (st_time.coord == Ecliptic)
      EclipticCoords (&st_time);
   else if (st_time.coord == Equatorial)
      EquatorialCoords (&st_time);
   else if (st_time.coord == CoRotational)
      CoRotCoords (&st_time);
   else if (st_time.coord == SaturnSolarMagnetic)
      KSMCoords (&st_time);
   else
   {
      printf ("Content-type: text/plain\n\n");
      printf ("%s: unknown coordinate system %d\n", argv [0], st_time.coord);
      exit (1);
   }

   return (0);
}
