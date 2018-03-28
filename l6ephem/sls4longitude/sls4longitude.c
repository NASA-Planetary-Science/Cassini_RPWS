#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include <das.h>

#include "y1958.h"
#include "sls4longitude.h"
#include "OrbitNames.h"
#include "OrbitBphiConstants.h"

#include "rotplot14_data.h"
#include "syslong4_data_28Dec2012.h"

static char *progname;

static int NorthOrSouth;
static int SunOrSC;
static int OneHundredOrZero;

static double SouthStartTime;
static double NorthStartTime;

static double NorthStartPhaseTime;
static double SouthStartPhaseTime;

static int SouthIndex;
static int NorthIndex;

static int SLS4SIndex;
static int SLS4NIndex;

static double SLS4_S_XValues [100];
static double SLS4_S_YValues [100];
static double SLS4_S_BValues [100];
static double SLS4_S_CValues [100];
static double SLS4_S_DValues [100];

static double SLS4_N_XValues [100];
static double SLS4_N_YValues [100];
static double SLS4_N_BValues [100];
static double SLS4_N_CValues [100];
static double SLS4_N_DValues [100];

static double XValuesPhase_S [500];
static double YValuesPhase_S [500];
static double BValuesPhase_S [500];
static double CValuesPhase_S [500];
static double DValuesPhase_S [500];

static double XValuesPhase_N [500];
static double YValuesPhase_N [500];
static double BValuesPhase_N [500];
static double CValuesPhase_N [500];
static double DValuesPhase_N [500];

static double OrbitStTimesMS [295];
static double OrbitSpTimesMS [295];

static int OrbitNumberSt;
static int OrbitNumberSp;
static int OrbitNumberIndex;
static int OrbitNumberArray [295];

static double BphiCorrection;

char *makeword (char *line, char stop);
char *fmakeword (FILE *f, char stop, int *len);
void unescape_url (char *url);
void plustospace (char *str);

void furnsh_ (char *filename, int namelen);
void spkez_ (int *target, double *et, char *frame, char *aberr, int *obs, double state [], double *lt, int framelen, int aberrlen);
void utc2et_ (char *fstring, double *et, int len);
void vminus_ (double vin [], double vout []);
void vhat_ (double vin [], double vout []);
void ucrss_ (double v1 [], double v2 [], double vout []);
void mxv_ (double tipm [][], double vin [], double vout []);
void mtxv_ (double tipm [][], double vin [], double vout []);
void bodmat_ (int *target, double *et, double tipm [][]);
void reclat_ (double vin [], double *rad, double *lon, double *lat);

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

double GetDayTimePosition (CurrentTime)
double CurrentTime;
{
   int past, yr, dy, hr, mn;
   double sec;
   double secval, daytime;

   ms2dhms (&past, &hr, &mn, &sec, CurrentTime);
   yrdy1958 (&yr, &dy, past);

   secval = (double) (hr*3600.0) +
            (double) (mn*60.0) +
            (double) (sec);

   secval = (secval/86400.0);

   daytime = ((double) past + secval);

   return (daytime);
}

int ValidateTime (st)
struct time_struc *st;
{
   int past, year, month, day, doy, hr, mn;
   double sec, ValidSt, ValidSp;
   char temp1 [80], temp2 [80];
   char *v1StTime = "1977-248-13-58-37";
   char *v1SpTime = "2013-007-23-58-55";
   char *v2StTime = "1977-232-15-31-44";
   char *v2SpTime = "2013-007-23-58-55";
   char *glStTime = "1989-292-01-28-38";
   char *glSpTime = "2003-273-11-58-55";
   char *caStTime = "1997-288-09-26-09";
   char *caSpTime = "2017-264-23-58-52";
   char *ssStTime = "1958-001-00-00-00";
   char *ssSpTime = "2013-007-23-58-55";

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

void ValidateTimeSLS4 (StMS, SpMS)
double *StMS, *SpMS;
{
   int past, year, month, day, doy, hr, mn;
   double sec, ValidSt, ValidSp;
   double LightTimeCorrection;
   char temp1 [80], temp2 [80];
   char *SLS4NStTime = "2006-095-00-00-00";
   char *SLS4NSpTime = "2009-259-00-00-00";
   char *SLS4SStTime = "2004-256-00-00-00";
   char *SLS4SSpTime = "2009-289-00-00-00";

   if (NorthOrSouth)
   {
      sprintf (temp1, "%s", SLS4NStTime);
      parsetime (temp1, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSt = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;

      sprintf (temp2, "%s", SLS4NSpTime);
      parsetime (temp2, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSp = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;
   }
   else
   {
      sprintf (temp1, "%s", SLS4SStTime);
      parsetime (temp1, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSt = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;

      sprintf (temp2, "%s", SLS4SSpTime);
      parsetime (temp2, &year, &month, &day, &doy, &hr, &mn, &sec);

      past = past_1958 (year, doy);

      ValidSp = (double) past*86400000.0 +
                (double) hr*3600000.0 +
                (double) mn*60000.0 +
                (double) sec*1000.0;
   }

   if (!SunOrSC)
   {
      if (NorthOrSouth)
         LightTimeCorrection =  6.0;
      else
         LightTimeCorrection = 30.0;

      LightTimeCorrection *= 1000.0;

      ValidSt += LightTimeCorrection;
   }

   *StMS = ValidSt;
   *SpMS = ValidSp;
}

char GetLetter (st)
struct time_struc *st;
{
   char ChTarget;

   switch (st->target)
   {
      case VenusBC:
         ChTarget = 'v';
         break;

      case Earth:
         ChTarget = 'e';
         break;

      case JupiterBC:
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

      case SaturnBC:
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

      if (st->SunOrSC)
      {
         printf ("                                                   ");
         printf ("Solar Longitude\n\n");
      }
      else
      {
         printf ("                                                 ");
         printf ("Spacecraft Longitude\n\n");
      }

      if (st->target == Sun)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (AU)\n");
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == VenusBC)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Rv)   1 Rv = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Earth)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Re)   1 Re = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == JupiterBC)
      {
         printf ("                                                ");
         printf ("                                     ");
         printf ("  1 Rj = %1.1f km\n", st->radius);
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("      MLat (deg)        LT (hrs)          R (Rj)");
         printf ("  Io phase (deg)\n");
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------      ----------");
         printf ("  --------------\n");
      }

      if (st->target == Io)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Ri)   1 Ri = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Europa)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Re)   1 Re = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Ganymede)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Rg)   1 Rg = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Callisto)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Rc)   1 Rc = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if ((st->target == SaturnBC) || (st->target == Saturn))
      {
         if (NorthOrSouth)
            printf ("      SCET (UT)           WL(SLS4 N) (deg)");
         else
            printf ("      SCET (UT)           WL(SLS4 S) (deg)");

         printf ("       Lat (deg)          L         LT (hrs)         R (Rs)   1 Rs = %1.1f km\n", st->radius);
         printf ("---------------------     ----------------");
         printf ("      ----------        -----      ----------      ----------\n");
      }

      if (st->target == Mimas)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Rm)   1 Rm = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Enceladus)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Re)   1 Re = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Tethys)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Rt)   1 Rt = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Dione)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Rd)   1 Rd = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Rhea)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Rr)   1 Rr = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Titan)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Rt)   1 Rt = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Hyperion)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Rh)   1 Rh = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Iapetus)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Ri)   1 Ri = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
      }

      if (st->target == Phoebe)
      {
         printf ("      SCET (UT)           W Lon (deg)       Lat (deg)");
         printf ("        LT (hrs)          R (Rp)   1 Rp = %1.1f km\n", st->radius);
         printf ("---------------------     -----------      ----------");
         printf ("      ----------      ----------\n");
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

void InitializeOrbits (void)
{
   int index;
   int year, month, day, doy, hr, mn;
   double sec;

   for (index = 0; index < 295; index++)
   {
      if (parsetime (OrbitStTimes [index], &year, &month, &day, &doy, &hr, &mn, &sec))
      {
         printf ("%s: error parsing %s\n", progname, OrbitStTimes [index]);
         exit (1);
      }

      OrbitStTimesMS [index] = GetTime (year, doy, hr, mn, (float) sec);

      if (parsetime (OrbitSpTimes [index], &year, &month, &day, &doy, &hr, &mn, &sec))
      {
         printf ("%s: error parsing %s\n", progname, OrbitSpTimes [index]);
         exit (1);
      }

      OrbitSpTimesMS [index] = GetTime (year, doy, hr, mn, (float) sec);
   }
}

int FindOrbitNumber (CurrentTime)
double CurrentTime;
{
   int index, FoundOrbit;

   if (CurrentTime < OrbitStTimesMS [0])
      return (-1);

   if (CurrentTime > OrbitSpTimesMS [294])
      return (-1);

   index = 0;
   FoundOrbit = False;

   while ((index < 295) && (!FoundOrbit))
   {
      if ((CurrentTime >= OrbitStTimesMS [index]) &&
          (CurrentTime <  OrbitSpTimesMS [index]))
         FoundOrbit = True;
      else
         index++;
   }

   if (!FoundOrbit)
      index = -1;

   return (index);
}

int spline (n, x, y, b, c, d)
int n;
double x [], y [], b [], c [], d [];
{
   int nm1, ib, i;
   double t;
   double temp;

   nm1 = (n - 1);

   if (n < 2)
      return (False);

   if (n < 3)
   {
      b [0] = (y [1] - y [0])/(x [1] - x [0]);
      c [0] = 0.0;
      d [0] = 0.0;

      b [1] = b [0];
      c [1] = 0.0;
      d [1] = 0.0;

      return (True);
   }

   d [0] = x [1] - x [0];
   c [1] = (y [1] - y [0])/d [0];

   for (i = 1; i < nm1; i++)
   {
      d [i] = x [(i + 1)] - x [i];
      b [i] = 2.0*(d [(i - 1)] + d [i]);
      c [(i + 1)] = (y [(i + 1)] - y [i])/d [i];
      c [i] = c [(i + 1)] - c [i];
   }

   b [0] = -d [0];
   b [(n - 1)] = -d [(n - 2)];
   c [0] = 0.0;
   c [(n - 1)] = 0.0;

   if (n > 3)
   {
      c [0] = c [2]/(x [3] - x [1]) - c [1]/(x [2] - x [0]);
      c [(n - 1)] = c [(n - 2)]/(x [(n - 1)] - x [(n - 3)]) -
                    c [(n - 3)]/(x [(n - 2)] - x [(n - 4)]);

      temp = (d [0]*d [0]);
      c [0] =  c [0]*temp/(x [3] - x [0]);

      temp = (d [(n - 2)]*d [(n - 2)]);
      c [(n - 1)] = -c [(n - 1)]*temp/(x [(n - 1)] - x [(n - 4)]);
   }

   for (i = 1; i < n; i++)
   {
      t = d [(i - 1)]/b [(i - 1)];
      b [i] = b [i] - t*d [(i - 1)];
      c [i] = c [i] - t*c [(i - 1)];
   }

   c [(n - 1)] = c [(n - 1)]/b [(n - 1)];

   for (ib = 0; ib < nm1; ib++)
   {
      i = n - ib;
      i -= 2;
      c [i] = (c [i] - d [i]*c [(i + 1)])/b [i];
   }

   b [(n - 1)] = (y [(n - 1)] - y [(nm1 - 1)])/d [(nm1 - 1)] +
                  d [(nm1 - 1)] + d [(nm1 - 1)]*(c [(nm1 - 1)] + 2.0*c [(n - 1)]);

   for (i = 0; i < nm1; i++)
   {
      b [i] = (y [(i + 1)] - y [i])/d [i] -
               d [i]*(c [(i + 1)] + 2.0*c [i]);

      d [i] = (c [(i + 1)] - c [i])/d [i];
      c [i] = 3.0*c [i];
   }

   c [(n - 1)] = 3.0*c [(n - 1)];
   d [(n - 1)] = d [(n - 2)];

   return (True);
}

double seval (n, u, x, y, b, c, d)
int n;
double u;
double x [], y [], b [], c [], d [];
{
   int i;
   double dx;
   double ReturnValue;

   if (u == x [0])
      i = 0;
   else if ((float) u == (float) x [(n - 1)])
      i = n - 2;
   else
   {
      i = 0;

      while (i < (n - 1))
      {
         if ((u > x [i]) && (u <= x [(i + 1)]))
            break;

         i++;
      }
   }

   dx = u - x [i];

   ReturnValue = y [i] + dx*(b [i] + dx*(c [i] + dx*d [i]));

   return (ReturnValue);
}

double seval_integral (i, n, u, x, y, b, c, d)
int i, n;
double u;
double x [], y [], b [], c [], d [];
{
   double dx;
   double term1, term2, term3, term4, ConstantTerm;
   double OneHalf, OneThird, OneFourth;
   double ReturnValue;

   ConstantTerm = 0.0;
   OneHalf = (1.0/2.0);
   OneThird = (1.0/3.0);
   OneFourth = (1.0/4.0);

   ConstantTerm = 0.0;

   dx = u - x [i];

   term1 = (u*y [i]);

   term2 = (dx*dx);
   term2 *= b [i];
   term2 *= OneHalf;

   term3 = (dx*dx*dx);
   term3 *= c [i];
   term3 *= OneThird;

   term4 = (dx*dx*dx*dx);
   term4 *= d [i];
   term4 *= OneFourth;

   ReturnValue = (term1 + term2 + term3 + term4 + ConstantTerm);

   return (ReturnValue);
}

void ReadAndComputeSplineSLS4_S (void)
{
   int index;
   int year, month, day, doy, hr, mn, past;
   double sec;
   double CurrentTime;
   char *SouthStartString = "2004-136T00-00-00.000";

   if (parsetime (SouthStartString, &year, &month, &day, &doy, &hr, &mn, &sec))
   {
      printf ("%s: error parsing %s\n", progname, SouthStartString);
      exit (1);
   }

   CurrentTime = GetTime (year, doy, hr, mn, (float) sec);
   SouthStartTime = GetDayTimePosition (CurrentTime);

   SLS4SIndex = 0;

   for (index = 0; index < NumPoints; index++)
   {
      if (parsetime (CassiniTimes [index], &year, &month, &day, &doy, &hr, &mn, &sec))
      {
         printf ("%s: error parsing %s\n", progname, CassiniTimes [index]);
         exit (1);
      }

      CurrentTime = GetTime (year, doy, hr, mn, (float) sec);

      if (CassiniValuesS [index] >= 0)
      {
         if (CassiniErrorS [index] > 0.0)
         {
            SLS4_S_XValues [SLS4SIndex]  = GetDayTimePosition (CurrentTime);
            SLS4_S_XValues [SLS4SIndex] -= SouthStartTime;

            SLS4_S_YValues [SLS4SIndex] = (double) CassiniValuesS [index];

            SLS4SIndex++;
         }
      }
   }

   spline (SLS4SIndex, SLS4_S_XValues, SLS4_S_YValues, SLS4_S_BValues,
                      SLS4_S_CValues, SLS4_S_DValues);
}

void ReadAndComputeSplineSLS4_N (void)
{
   int index;
   int year, month, day, doy, hr, mn, past;
   double sec;
   double CurrentTime;
   char *NorthStartString = "2005-340T00-00-00.000";

   if (parsetime (NorthStartString, &year, &month, &day, &doy, &hr, &mn, &sec))
   {
      printf ("%s: error parsing %s\n", progname, NorthStartString);
      exit (1);
   }

   CurrentTime = GetTime (year, doy, hr, mn, (float) sec);
   NorthStartTime = GetDayTimePosition (CurrentTime);

   SLS4NIndex = 0;

   for (index = 0; index < NumPoints; index++)
   {
      if (parsetime (CassiniTimes [index], &year, &month, &day, &doy, &hr, &mn, &sec))
      {
         printf ("%s: error parsing %s\n", progname, CassiniTimes [index]);
         exit (1);
      }

      CurrentTime = GetTime (year, doy, hr, mn, (float) sec);

      if (CassiniValuesN [index] >= 0)
      {
         if (CassiniErrorN [index] > 0.0)
         {
            SLS4_N_XValues [SLS4NIndex]  = GetDayTimePosition (CurrentTime);
            SLS4_N_XValues [SLS4NIndex] -= NorthStartTime;

            SLS4_N_YValues [SLS4NIndex] = (double) CassiniValuesN [index];

            SLS4NIndex++;
         }
      }
   }

   spline (SLS4NIndex, SLS4_N_XValues, SLS4_N_YValues, SLS4_N_BValues,
                      SLS4_N_CValues, SLS4_N_DValues);
}

void ReadAndComputePhaseSplineS (void)
{
   int index;
   int dflag;
   int yr, dy, hr, mn;
   int year, month, day, doy, past;
   float fss;
   float PhaseValue;
   double sec;
   char *FileNameS = "syslong2_S_28Dec2012.output";
   double CurrentTime;
   char *SouthStartString = "2004-256T00-00-00.000";
   FILE *infp;

   if (parsetime (SouthStartString, &year, &month, &day, &doy, &hr, &mn, &sec))
   {
      printf ("%s: error parsing %s\n", progname, SouthStartString);
      exit (1);
   }

   CurrentTime = GetTime (year, doy, hr, mn, (float) sec);
   SouthStartPhaseTime = GetDayTimePosition (CurrentTime);

   SouthIndex = 0;

   /* Open the input file */
   if (!(infp = fopen (FileNameS, "r")))
   {
      (void) fprintf (stderr, "Can't open input file.\n");
      exit (1);
   }

   while (!feof (infp))
   {
      dflag = fscanf (infp, "%d %d %d %d %f %*f %f %*f",
                      &yr, &dy, &hr, &mn, &fss, &PhaseValue);

      if (dflag < 0)
         break;

      CurrentTime = GetTime (yr, dy, hr, mn, fss);

      XValuesPhase_S [SouthIndex] = GetDayTimePosition (CurrentTime);
      XValuesPhase_S [SouthIndex] -= SouthStartPhaseTime;

      YValuesPhase_S [SouthIndex] = (double) PhaseValue;

      SouthIndex++;
   }

   spline (SouthIndex, XValuesPhase_S, YValuesPhase_S, BValuesPhase_S, CValuesPhase_S, DValuesPhase_S);

   fclose (infp);
}

void ReadAndComputePhaseSplineN (void)
{
   int index;
   int dflag;
   int yr, dy, hr, mn;
   int year, month, day, doy, past;
   float fss;
   float PhaseValue;
   double sec;
   char *FileNameN = "syslong2_N_04Jan2011.output";
   double CurrentTime;
   char *NorthStartString = "2006-095T00-00-00.000";
   FILE *infp;

   if (parsetime (NorthStartString, &year, &month, &day, &doy, &hr, &mn, &sec))
   {
      printf ("%s: error parsing %s\n", progname, NorthStartString);
      exit (1);
   }

   CurrentTime = GetTime (year, doy, hr, mn, (float) sec);
   NorthStartPhaseTime = GetDayTimePosition (CurrentTime);

   NorthIndex = 0;

   /* Open the input file */
   if (!(infp = fopen (FileNameN, "r")))
   {
      (void) fprintf (stderr, "Can't open input file.\n");
      exit (1);
   }

   while (!feof (infp))
   {
      dflag = fscanf (infp, "%d %d %d %d %f %*f %f %*f",
                      &yr, &dy, &hr, &mn, &fss, &PhaseValue);

      if (dflag < 0)
         break;

      CurrentTime = GetTime (yr, dy, hr, mn, fss);

      XValuesPhase_N [NorthIndex] = GetDayTimePosition (CurrentTime);
      XValuesPhase_N [NorthIndex] -= NorthStartPhaseTime;

      YValuesPhase_N [NorthIndex] = (double) PhaseValue;

      NorthIndex++;
   }

   spline (NorthIndex, XValuesPhase_N, YValuesPhase_N, BValuesPhase_N, CValuesPhase_N, DValuesPhase_N);

   fclose (infp);
}

double GetLongitudeSLS4_S (CurrentTime)
double CurrentTime;
{
   int index;
   int SplineIndex;
   int past, year, month, day, doy, hr, mn;
   double matrix [3][3];
   double state [6], sc_pos [3], sun_pos [3], pos [3], position [3];
   double lt, et, radius, longitude, latitude, dummy1, dummy2;
   double sunlong, loctime;
   double delta, sec, st_ms, OWLT;
   double AltLongitude, SunLongitude;
   double term1, fmodConst;
   double CurrentDay, PhaseCorrection, ConstValue;
   double PhaseConstant;
   char scstr [80];
   char *frame = "J2000";
   char *aberr = "NONE";

   fmodConst = 360.0;
   PhaseConstant =  -86.426;

   if (SunOrSC)
      OWLT = 0.0;
   else
   {
      ms2dhms (&past, &hr, &mn, &sec, CurrentTime);
      yrdy1958 (&year, &doy, past);

      sprintf (scstr, "%04d %03d // %02d:%02d:%06.3f",
               year, doy, hr, mn, (float) sec);

      utc2et (scstr, &et);

      /* determine spacecraft longitude to determine local time */
      spkez (SaturnBC, et, frame, aberr, Cassini, state, &lt);

      spkez (SaturnBC, (et - lt), frame, aberr, Cassini, state, &dummy1);

      for (index = 0; index < 3; index++)
         pos [index] = state [index];

      vminus (pos, sc_pos);

      bodmat (Saturn, (et - lt), matrix);

      mxv (matrix, sc_pos, position);

      reclat (position, &dummy1, &longitude, &dummy2);

      if (longitude < 0.0)
         longitude += 2.0*PI;

      longitude = (longitude*DPR);
      longitude = (360.0 - longitude);

      /* determine system III longitude of the Sun */
      spkez (SaturnBC, (et - lt), frame, aberr, Sun, state, &dummy1);

      for (index = 0; index < 3; index++)
         pos [index] = state [index];

      vminus (pos, sun_pos);

      mxv (matrix, sun_pos, position);

      reclat (position, &dummy1, &sunlong, &dummy2);

      if (sunlong < 0.0)
         sunlong += 2.0*PI;

      sunlong = (sunlong*DPR);
      sunlong = (360.0 - sunlong);

      /* determine local time */
      loctime = (sunlong - longitude);
      loctime += 180.0;
      loctime = (loctime/15.0);

      if (loctime < 0.0)
         loctime += 24.0;

      if (loctime >= 24.0)
         loctime -= 24.0;

      OWLT = lt*1000.0;
   }

   CurrentTime -= OWLT;

   CurrentDay  = GetDayTimePosition (CurrentTime);
   CurrentDay -= SouthStartTime;

   SplineIndex = (int) (CurrentDay/30.0);

   ConstValue = SLS4Corrections_S [SplineIndex];

   SunLongitude = seval_integral (SplineIndex, SLS4SIndex, CurrentDay, SLS4_S_XValues, SLS4_S_YValues, SLS4_S_BValues, SLS4_S_CValues, SLS4_S_DValues);

   CurrentDay = GetDayTimePosition (CurrentTime);

   CurrentDay -= SouthStartPhaseTime;

   PhaseCorrection = seval (SouthIndex, CurrentDay, XValuesPhase_S, YValuesPhase_S, BValuesPhase_S, CValuesPhase_S, DValuesPhase_S);

   PhaseCorrection *= -1.0;

   SunLongitude += ConstValue;
   SunLongitude += PhaseCorrection;
   SunLongitude += PhaseConstant;

   if (OneHundredOrZero)
      SunLongitude += 100.0;

   SunLongitude += BphiCorrection;

   if (SunLongitude < 0.0)
   {
      do
      {
         SunLongitude += 360.0;
      }
      while (SunLongitude < 0.0);
   }

   SunLongitude = fmod (SunLongitude, fmodConst);

   if (SunOrSC)
      return (SunLongitude);
   else
   {
      term1 = (12.0 - loctime);
      term1 *= 15.0;

      AltLongitude = (SunLongitude + term1);

      if (AltLongitude < 0.0)
      {
         do
         {
            AltLongitude += 360.0;
         }
         while (AltLongitude < 0.0);
      }

      AltLongitude = fmod (AltLongitude, fmodConst);

      return (AltLongitude);
   }
}

double GetLongitudeSLS4_N (CurrentTime)
double CurrentTime;
{
   int index;
   int SplineIndex;
   int past, year, month, day, doy, hr, mn;
   double matrix [3][3];
   double state [6], sc_pos [3], sun_pos [3], pos [3], position [3];
   double lt, et, radius, longitude, latitude, dummy1, dummy2;
   double sunlong, loctime;
   double delta, sec, st_ms, OWLT;
   double AltLongitude, SunLongitude;
   double term1, fmodConst;
   double CurrentDay, PhaseCorrection, ConstValue;
   double PhaseConstant;
   char scstr [80];
   char *frame = "J2000";
   char *aberr = "NONE";

   fmodConst = 360.0;
   PhaseConstant = -134.354;

   if (SunOrSC)
      OWLT = 0.0;
   else
   {
      ms2dhms (&past, &hr, &mn, &sec, CurrentTime);
      yrdy1958 (&year, &doy, past);

      sprintf (scstr, "%04d %03d // %02d:%02d:%06.3f",
               year, doy, hr, mn, (float) sec);

      utc2et (scstr, &et);

      /* determine spacecraft longitude to determine local time */
      spkez (SaturnBC, et, frame, aberr, Cassini, state, &lt);

      spkez (SaturnBC, (et - lt), frame, aberr, Cassini, state, &dummy1);

      for (index = 0; index < 3; index++)
         pos [index] = state [index];

      vminus (pos, sc_pos);

      bodmat (Saturn, (et - lt), matrix);

      mxv (matrix, sc_pos, position);

      reclat (position, &dummy1, &longitude, &dummy2);

      if (longitude < 0.0)
         longitude += 2.0*PI;

      longitude = (longitude*DPR);
      longitude = (360.0 - longitude);

      /* determine system III longitude of the Sun */
      spkez (SaturnBC, (et - lt), frame, aberr, Sun, state, &dummy1);

      for (index = 0; index < 3; index++)
         pos [index] = state [index];

      vminus (pos, sun_pos);

      mxv (matrix, sun_pos, position);

      reclat (position, &dummy1, &sunlong, &dummy2);

      if (sunlong < 0.0)
         sunlong += 2.0*PI;

      sunlong = (sunlong*DPR);
      sunlong = (360.0 - sunlong);

      /* determine local time */
      loctime = (sunlong - longitude);
      loctime += 180.0;
      loctime = (loctime/15.0);

      if (loctime < 0.0)
         loctime += 24.0;

      if (loctime >= 24.0)
         loctime -= 24.0;

      OWLT = lt*1000.0;
   }

   CurrentTime -= OWLT;

   CurrentDay  = GetDayTimePosition (CurrentTime);
   CurrentDay -= NorthStartTime;

   SplineIndex = (int) (CurrentDay/30.0);

   ConstValue = SLS4Corrections_N [SplineIndex];

   SunLongitude = seval_integral (SplineIndex, SLS4NIndex, CurrentDay, SLS4_N_XValues, SLS4_N_YValues, SLS4_N_BValues, SLS4_N_CValues, SLS4_N_DValues);

   CurrentDay = GetDayTimePosition (CurrentTime);

   CurrentDay -= NorthStartPhaseTime;

   PhaseCorrection = seval (NorthIndex, CurrentDay, XValuesPhase_N, YValuesPhase_N, BValuesPhase_N, CValuesPhase_N, DValuesPhase_N);

   PhaseCorrection *= -1.0;

   SunLongitude += ConstValue;
   SunLongitude += PhaseCorrection;
   SunLongitude += PhaseConstant;

   if (OneHundredOrZero)
      SunLongitude += 100.0;

   if (SunLongitude < 0.0)
   {
      do
      {
         SunLongitude += 360.0;
      }
      while (SunLongitude < 0.0);
   }

   SunLongitude = fmod (SunLongitude, fmodConst);

   if (SunOrSC)
      return (SunLongitude);
   else
   {
      term1 = (12.0 - loctime);
      term1 *= 15.0;

      AltLongitude = (SunLongitude + term1);

      if (AltLongitude < 0.0)
      {
         do
         {
            AltLongitude += 360.0;
         }
         while (AltLongitude < 0.0);
      }

      AltLongitude = fmod (AltLongitude, fmodConst);

      return (AltLongitude);
   }
}

int GeographicCoords (stime)
struct time_struc *stime;
{
   int index, no_data;
   int past, year, month, day, doy, hr, mn;
   int on_yr, on_dy, on_hr, on_mn;
   int OrbitIndex;
   float on_ss;
   double matrix [3][3], matrix2 [3][3], tipm [3][3];
   double sun_pos [6], sun_pos2 [3], x_vec [3], y_vec [3], z_vec [3];
   double sc_pos [6], sc_pos2 [3], pos [3], position [3];
   double obs_lt, lt, et, radius, longitude, latitude, dummy1, dummy2;
   double value1, value2, value3, mlat, sunlong, loctime;
   double delta, sec, st_ms;
   double ConLongitude, SKRLongitude;
   double Model_SLS4_Longitude_N;
   double Model_SLS4_Longitude_S;
   double SKR_lt;
   double temp1, temp2, LValue, RLatitude;
   double ValidSt, ValidSp;
   char scstr [80];
   char *frame = "J2000";
   char *aberr1 = "NONE";
   char *aberr2 = "LT+S";

   no_data = True;

   ValidateTimeSLS4 (&ValidSt, &ValidSp);

   if (NorthOrSouth)
   {
      ReadAndComputePhaseSplineN ();

      ReadAndComputeSplineSLS4_N ();
   }
   else
   {
      ReadAndComputePhaseSplineS ();

      ReadAndComputeSplineSLS4_S ();
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

      if (stime->target == SaturnBC)
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

      if (stime->target == SaturnBC)
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
         if (NorthOrSouth)
         {
            if ((st_ms >= ValidSt) && (st_ms <= ValidSp))
               Model_SLS4_Longitude_N = GetLongitudeSLS4_N (st_ms);
            else
               Model_SLS4_Longitude_N = -1.0;
         }
         else
         {
            if ((st_ms >= ValidSt) && (st_ms <= ValidSp))
            {
               if (stime->UseBphiCorrection)
               {
                  OrbitIndex = FindOrbitNumber (st_ms);

                  BphiCorrection  = 0.0;
                  BphiCorrection -= OrbitBphiConstants [OrbitIndex];
                  BphiCorrection -= 90.0;
               }
               else
                  BphiCorrection = 0.0;

               Model_SLS4_Longitude_S = GetLongitudeSLS4_S (st_ms);
            }
            else
               Model_SLS4_Longitude_S = -1.0;
         }

         RLatitude = (latitude*RPD);
         temp1 = cos (RLatitude);
         temp2 = (temp1*temp1);

         LValue = (radius/temp2);
      }

      st_ms += delta;

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

      if ((stime->target == SaturnBC) || (stime->target == Saturn))
      {
         if (NorthOrSouth)
            printf ("%04d %03d %02d %02d %06.3f %16.3f %17.3f %14.1f %13.3f %15.3f\n",
                     year, doy, hr, mn, (float) sec,
                     (float) Model_SLS4_Longitude_N,
                     (float) latitude, (float) LValue,
                     (float) loctime, (float) radius);
         else
            printf ("%04d %03d %02d %02d %06.3f %16.3f %17.3f %14.1f %13.3f %15.3f\n",
                     year, doy, hr, mn, (float) sec,
                     (float) Model_SLS4_Longitude_S,
                     (float) latitude, (float) LValue,
                     (float) loctime, (float) radius);
      }
   }

   return (no_data);
}

int main (int argc, char *argv [])
{
   int index, x, cl;
   int year, month, day, doy, hr, mn, past;
   int VerifyIndex;
   int itemp;
   double st_ms, sp_ms, sec, NewRadius;
   char st_string [80], sp_string [80], temp [80];
   char *dummy;
   char *CassiniName = "CassiniKernels.list";
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

   for (index = 0; index < x; index++)
   {
      sprintf (temp, "%s", entries [index].val);

      if (!strcmp (entries [index].name, "StTime"))
      {
         if (parsetime (temp, &year, &month, &day, &doy, &hr, &mn, &sec))
         {
            printf ("Content-type: text/html%c%c", 10, 10);
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
      }
      else if (!strcmp (entries [index].name, "SpTime"))
      {
         if (parsetime (temp, &year, &month, &day, &doy, &hr, &mn, &sec))
         {
            printf ("Content-type: text/html%c%c", 10, 10);
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
      }
      else if (!strcmp (entries [index].name, "TimeInterval"))
      {
         for (VerifyIndex = 0; VerifyIndex < (int) strlen (temp); VerifyIndex++)
            if ((!(isdigit (temp [VerifyIndex]))) &&
                           (temp [VerifyIndex] != ' ') &&
                           (temp [VerifyIndex] != '+') &&
                           (temp [VerifyIndex] != '-') &&
                           (temp [VerifyIndex] != 'd') &&
                           (temp [VerifyIndex] != 'D') &&
                           (temp [VerifyIndex] != 'e') &&
                           (temp [VerifyIndex] != 'E') &&
                           (temp [VerifyIndex] != '.'))
            {
               printf ("Content-type: text/html%c%c", 10, 10);
               printf ("Invalid entry  %s\n", temp);
               return (0);
            }

         st_time.deltatee = (double) strtod (temp, &dummy);

         if (st_time.deltatee <= 0.0)
         {
            printf ("Content-type: text/plain\n\n");
            printf ("Invalid time interval entry:  %s\n", temp);
            return (0);
         }
      }
      else if (!strcmp (entries [index].name, "origin"))
      {
         if (!strcmp (temp, "sat1"))
         {
            st_time.target = SaturnBC;
            st_time.radius = RSaturn;
         }
         else if (!strcmp (temp, "sat2"))
         {
            st_time.target = Saturn;
            st_time.radius = RSaturn;
         }
      }
      else if (!strcmp (entries [index].name, "northorsouth"))
      {
         if      (!strcmp (temp, "nort"))
            NorthOrSouth = True;
         else if (!strcmp (temp, "sout"))
            NorthOrSouth = False;
      }
      else if (!strcmp (entries [index].name, "solarorsc"))
      {
         if      (!strcmp (temp, "spac"))
         {
            SunOrSC = False;
            st_time.SunOrSC = False;
         }
         else if (!strcmp (temp, "sola"))
         {
            SunOrSC = True;
            st_time.SunOrSC = True;
         }
      }
      else if (!strcmp (entries [index].name, "phasenum"))
      {
         if      (!strcmp (temp, "pzer"))
            OneHundredOrZero = False;
         else if (!strcmp (temp, "pcen"))
            OneHundredOrZero = True;
      }
      else if (!strcmp (entries [index].name, "OriginRadius"))
      {
         for (VerifyIndex = 0; VerifyIndex < (int) strlen (temp); VerifyIndex++)
            if ((!(isdigit (temp [VerifyIndex]))) &&
                           (temp [VerifyIndex] != ' ') &&
                           (temp [VerifyIndex] != '+') &&
                           (temp [VerifyIndex] != '-') &&
                           (temp [VerifyIndex] != 'd') &&
                           (temp [VerifyIndex] != 'D') &&
                           (temp [VerifyIndex] != 'e') &&
                           (temp [VerifyIndex] != 'E') &&
                           (temp [VerifyIndex] != '.'))
            {
               printf ("Content-type: text/html%c%c", 10, 10);
               printf ("Invalid entry  %s\n", temp);
               return (0);
            }

         NewRadius = (double) strtod (temp, &dummy);

         st_time.radius = NewRadius;
      }
      else if (!strcmp (entries [index].name, "entry0"))
      {
         for (VerifyIndex = 0; VerifyIndex < (int) strlen (temp); VerifyIndex++)
            if ((!(isdigit (temp [VerifyIndex]))) &&
                           (temp [VerifyIndex] != ' '))
            {
               printf ("Content-type: text/html%c%c", 10, 10);
               printf ("Invalid plot entry  %s\n", temp);
               return (0);
            }

         itemp = (int) strtod (temp, &dummy);

         if (itemp == 1)
            st_time.UseBphiCorrection = False;
         else if (itemp == 2)
            st_time.UseBphiCorrection = True;
         else
         {
            printf ("Content-type: text/html%c%c", 10, 10);
            printf ("Invalid plot entry  %s\n", temp);
            return (0);
         }
      }
   }

   if (sp_ms <= st_ms)
   {
      printf ("Content-type: text/plain\n\n");
      printf ("Invalid time range selection:  %s to %s\n",
              st_string, sp_string);

      return (0);
   }

   st_time.observer = Cassini;
   st_time.coord = Geographic;

   if (!(ValidateTime (&st_time)))
      return (True);

   furnsh_ (CassiniName, strlen (CassiniName));

   if (st_time.UseBphiCorrection)
      InitializeOrbits ();

   GeographicCoords (&st_time);

   return (0);
}
