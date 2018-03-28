#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <das.h>

#include "y1958.h"
#include "OrbitNames.h"

#define Zeroeth  0
#define First    1
#define Second   2

#define True  1
#define False 0
#define NotTrue 999

#define X     0
#define Y     1
#define Z     2
#define RHO   3
#define RHO_Z 4

#define MAX_POINTS  65000
#define MAX_ENTRIES   100

#define RPD 0.0174532925

#define RSaturn 60268.0

#define Cassini     -82
#define Saturn      699
#define SaturnBC      6
#define Sun          10

typedef struct
{
   char *name;
   char *val;
}
entry;

struct time_struc
{
   int st_past;
   int sp_past;
   int st_year;
   int sp_year;
   int st_day;
   int sp_day;
   int st_hr;
   int sp_hr;
   int st_mn;
   int sp_mn;
   int EqEcOrKSM;
   int observer;
   int target;
   int psorgif;
   int x;
   int y;
   int FirstSecondOrThirdPlot;
   int StOrbit;
   int SpOrbit;
   float st_sec;
   float sp_sec;
   float xmin;
   float xmax;
   float ymin;
   float ymax;
   double lshell1;
   double lshell2;
   double ticks;
   double st_total_ms;
   double sp_total_ms;
   char title [80];
};

void getksm_ (int *observer, double *et, double *px, double *py, double *pz,
                                         double *vx, double *vy, double *vz);

void get_solar_eq_ (int *observer, int *target, double *et,
                    double *px, double *py, double *pz);

char *makeword (char *line, char stop);
char *fmakeword (FILE *f, char stop, int *len);
char x2c (char *what);
void unescape_url (char *url);
void plustospace (char *str);

double* make_seed_f2d ();
void furnsh_ (char *filename, int namelen);
void spkez_ (int *target, double *et, char *frame, char *aberr, int *obs, double state [], double *lt, int framelen, int aberrlen);
void gsmtrn_g_ (int *target, double *et, char *frame, char *aberr, double tipm [][], int framelen, int aberrlen);
void gsetrn_g_ (int *target, double *et, char *frame, char *aberr, double tipm [][], int framelen, int aberrlen);
void utc2et_ (char *fstring, double *et, int len);
void sce2s_ (int *spacecraft, double *et, char *sclk, int sclklen);
void et2utc_ (double *et, char *form, int *prec, char *utc, int formlen, int utclen);
void vminus_ (double vin [], double vout []);
void vhat_ (double vin [], double vout []);
void mtxv_ (double tipm [][], double vin [], double vout []);
void ucrss_ (double v1 [], double v2 [], double vout []);
void mxv_ (double tipm [][], double vin [], double vout []);
void bodmat_ (int *target, double *et, double tipm [][]);
void reclat_ (double vin [], double *rad, double *lon, double *lat);

int pgbeg_ (int *unit, char *file, int *nxsub, int *nysub, int filel);
void pgwnad_ (float *x1, float *x2, float *y1, float *y2);
void pgline_ (int *n, float xpts [], float ypts []);
void pgmtxt_ (char *side, float *disp, float *coord, float *fjust, char *text, int sl, int tl);
void pgpt_ (int *n, float xpts [], float ypts [], int *symbol);
void pgpt1_ (float *x, float *y, int *symbol);
void pgsci_ (int *ci);
void pgpap_ (float *width, float *aspect);
void pgsvp_ (float *xleft, float *xright, float *ybot, float *ytop);
void pgslw_ (int *lwidth);
void pgscr_ (int *ci, float *cr, float *cg, float *cb);
void pgswin_ (float *x1, float *x2, float *y1, float *y2);
void pgbox_ (char *xopt, float *xtick, int *nxsub, char *yopt, float *ytick, int *nysub, int xl, int yl);
void pgmove_ (float *x, float *y);
void pgdraw_ (float *x, float *y);
void pgend_ (void);

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

void vminus (double *vin, double *vout)
{
   double tmpvec [3];
   int    i;

   memcpy (tmpvec, vin, sizeof (tmpvec));
   for (i = 0; i < 3; i++)
      vout [i] = -1.0*tmpvec [i];
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

void mtxv (double matrix [3][3], double *vin, double *vout)
{
   double mt [3][3];

   transpose (matrix, mt);
   mxv (mt, vin, vout);
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

int past_1958 (int year, int day)
{
   int yr, past;

   yr = year - 1958;
   past = days_past [yr];
   past += (day - 1);

   return (past);
}

void get_oaEQ (st, x, y, z)
struct time_struc *st;
double *x, *y, *z;
{
   int index;
   int target, observer;
   double px, py, pz;
   double lt, et;
   char scstr [80];

   sprintf (scstr, "%4d %03d // %02d:%02d:%06.3f",
            st->st_year, st->st_day, st->st_hr, st->st_mn, st->st_sec);

   utc2et (scstr, &et);

   target = st->target;
   observer = st->observer;

   get_solar_eq_ (&observer, &target, &et, &px, &py, &pz);

   *x = (px/RSaturn);
   *y = (py/RSaturn);
   *z = (pz/RSaturn);
}

void get_oaEC (st, x, y, z)
struct time_struc *st;
double *x, *y, *z;
{
   int index, yr, dy, hr, mn, ss;
   double sc_pos [6], sc_pos2 [3];
   double pos [3];
   double matrix [3][3];
   double lt, et;
   char scstr [80];
   char *frame = "J2000";
   char *aberr = "LT";

   yr = st->st_year;
   dy = st->st_day;
   hr = st->st_hr;
   mn = st->st_mn;
   ss = (int) st->st_sec;

   sprintf (scstr, "%04d %03d // %02d:%02d:%02d.0", yr, dy, hr, mn, ss);
   utc2et (scstr, &et);

   spkez (SaturnBC, et, frame, aberr, st->observer, sc_pos, &lt);

   for (index = 0; index < 3; index++)
      pos [index] = sc_pos [index];

   vminus (pos, sc_pos2);

   gsetrn_g (SaturnBC, (et - lt), frame, aberr, matrix);

   mxv (matrix, sc_pos2, pos);

   *x = (double) (pos [0]/RSaturn);
   *y = (double) (pos [1]/RSaturn);
   *z = (double) (pos [2]/RSaturn);
}

int ValidateTime (st)
struct time_struc *st;
{
   int past, year, month, day, doy, hr, mn;
   double sec, ValidSt, ValidSp;
   char temp1 [80], temp2 [80];
   char *caStTime = "1997-288-09-26-10";
   char *caSpTime = "2017-258-10-32-49";

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

void DrawCircle (rcircle)
double rcircle;
{
   int index;
   float xcircle [400], ycircle [400];
   double xorigin = 0.0, yorigin = 0.0;
   double delta, Theta, angle1, xpt, ypt;

   /* draw planet or moon at origin */
   delta = 1.0;
   Theta = 0.0;
   index = 0;

   while (Theta <= 360.0)
   {
      angle1 = (double) (RPD*Theta);

      xpt = cos (angle1);
      xpt *= rcircle;
      xpt += xorigin;

      ypt = sin (angle1);
      ypt *= rcircle;
      ypt += yorigin;

      xcircle [index] = (float) xpt;
      ycircle [index] = (float) ypt;

      Theta += delta;
      index++;
   }

   pgline_ (&index, xcircle, ycircle);
}

int HowManyTimes (char *s, int c)
{
   int n = 0;

   while (s)
   {
      s = strchr (s, c);

      if (s) n++, s++;
   }

   return n;
}

int ProcessString (char *TheString)
{
   int StringIsValid;
   int counter, NumberOfBlanks, StringLength;
   int chZero = '0';
   int chCapA = 'A';
   int chCapB = 'B';
   int chCapC = 'C';
   int chSmallA = 'a';
   int chSmallB = 'b';
   int chSmallC = 'c';
   int chBlank = ' ';

   StringIsValid = NotTrue;

   StringLength = strlen (TheString);

   NumberOfBlanks = HowManyTimes (TheString, chBlank);

   counter = HowManyTimes (TheString, chZero);

   if (((counter + NumberOfBlanks) == StringLength) && (counter == 1))
      StringIsValid = -3;

   counter = HowManyTimes (TheString, chCapA);

   if (((counter + NumberOfBlanks) == StringLength) && (counter == 1))
      StringIsValid = -2;

   counter = HowManyTimes (TheString, chCapB);

   if (((counter + NumberOfBlanks) == StringLength) && (counter == 1))
      StringIsValid = -1;

   counter = HowManyTimes (TheString, chCapC);

   if (((counter + NumberOfBlanks) == StringLength) && (counter == 1))
      StringIsValid = 0;

   counter = HowManyTimes (TheString, chSmallA);

   if (((counter + NumberOfBlanks) == StringLength) && (counter == 1))
      StringIsValid = -2;

   counter = HowManyTimes (TheString, chSmallB);

   if (((counter + NumberOfBlanks) == StringLength) && (counter == 1))
      StringIsValid = -1;

   counter = HowManyTimes (TheString, chSmallC);

   if (((counter + NumberOfBlanks) == StringLength) && (counter == 1))
      StringIsValid = 0;

   return (StringIsValid);
}

void InitPlot (st)
struct time_struc *st;
{
   int nsymbol = 1, ncount = 1;
   int unit, nx, ny, fg_color, bg_color, lwidth;
   int index1, index2, dy_mn1, dy_mn2, leap1, leap2, xtick2, ytick2;
   int m_days [2][13] = {{1,32,60,91,121,152,182,213,244,274,305,335,366},
                         {1,32,61,92,122,153,183,214,245,275,306,336,367}};
   float x1, x2, y1, y2, xtick1, ytick1, fdiff;
   float rgb, width;
   float displ, coord = 0.5, fjust = 0.5;
   float xcircle [1], ycircle [1];
   float zvals [1], rhovals [1];
   double tempval, tempval1, tempval2, tempval3, mlatitude;
   double xval, yval, rcircle = 1.0;
   double xorigin = 0.0, yorigin = 0.0, delta = 0.001;
   char months [12][12] = {  "January", "February",    "March",    "April",
                                 "May",     "June",     "July",   "August",
                           "September",  "October", "November", "December"};
   char name [80];

   unit = 0;
   nx = 1;
   ny = 1;

   x1 = st->xmin;
   x2 = st->xmax;

   y1 = st->ymin;
   y2 = st->ymax;

   fg_color = 1;
   bg_color = 0;
   rgb = 0.0;
   lwidth = 3;

   fdiff = (float) fabs ((double) (st->xmax - st->xmin));

   if (fdiff < 5.0)
   {
      xtick1 = 0.0;
      xtick2 = 0;

      ytick1 = 0.0;
      ytick2 = 0;
   }
   else if ((fdiff >= 5.0) && (fdiff < 10.0))
   {
      xtick1 = 1.0;
      xtick2 = 4;

      ytick1 = 1.0;
      ytick2 = 4;
   }
   else if ((fdiff >= 10.0) && (fdiff < 15.0))
   {
      xtick1 = 2.0;
      xtick2 = 4;

      ytick1 = 2.0;
      ytick2 = 4;
   }
   else if ((fdiff >= 15.0) && (fdiff < 50.0))
   {
      xtick1 = 5.0;
      xtick2 = 5;

      ytick1 = 5.0;
      ytick2 = 5;
   }
   else if ((fdiff >= 50.0) && (fdiff < 100.0))
   {
      xtick1 = 10.0;
      xtick2 = 5;

      ytick1 = 10.0;
      ytick2 = 5;
   }
   else if ((fdiff >= 100.0) && (fdiff < 200.0))
   {
      xtick1 = 25.0;
      xtick2 = 5;

      ytick1 = 25.0;
      ytick2 = 5;
   }
   else if ((fdiff >= 200.0) && (fdiff < 400.0))
   {
      xtick1 = 50.0;
      xtick2 = 5;

      ytick1 = 50.0;
      ytick2 = 5;
   }
   else
   {
      xtick1 = 100.0;
      xtick2 = 5;

      ytick1 = 100.0;
      ytick2 = 5;
   }

   if (st->psorgif)
      fprintf (stdout, "Content-type: application/x-cgi-postscript\n\n");
   else
      fprintf (stdout, "Content-type: image/x-cgi-gif\n\n");

   fflush (stdout);

   if (st->psorgif)
   {
      width = 0.0;
      pgbeg_ (&unit, "-/cps", &nx, &ny, 5L);
   }
   else
   {
      width = 12.04;
      pgbeg_ (&unit, "-/gif", &nx, &ny, 5L);
   }

   pgwnad_ (&x1, &x2, &y1, &y2);

   pgslw_ (&lwidth);

   pgscr_ (&fg_color, &rgb, &rgb, &rgb);

   rgb = 1.0;

   pgscr_ (&bg_color, &rgb, &rgb, &rgb);

   pgswin_ (&x1, &x2, &y1, &y2);

   pgbox_ ("ABCINST", &xtick1, &xtick2, "ABCINSTV", &ytick1, &ytick2, 7L, 8L);

   DrawCircle (1.0);

   if (st->lshell1 != 0.0)
   {
      mlatitude = -90.0;

      while (mlatitude <= 90.0)
      {
         tempval2 = (double) (mlatitude*RPD);
         tempval1 = sin (tempval2);
         tempval3 = cos (tempval2);
         tempval3 = (tempval3*tempval3);
         tempval3 = (tempval3*st->lshell1);
         yval = (double) (tempval3*tempval1);
         zvals [0] = (float) (tempval3*tempval1);

         tempval1 = cos (tempval2);
         tempval3 = (tempval1*tempval1*tempval1);
         tempval3 = (tempval3*st->lshell1);
         xval = (double) (tempval3);
         rhovals [0] = (float) (tempval3);
   
         tempval = ((xval*xval) + (yval*yval));
         tempval = sqrt (tempval);

         if (tempval >= 1.0)
            pgpt_ (&ncount, rhovals, zvals, &nsymbol);

         mlatitude += 0.1;
      }
   }

   if (st->lshell2 != 0)
   {
      mlatitude = -90.0;

      while (mlatitude <= 90.0)
      {
         tempval2 = (double) (mlatitude*RPD);
         tempval1 = sin (tempval2);
         tempval3 = cos (tempval2);
         tempval3 = (tempval3*tempval3);
         tempval3 = (tempval3*st->lshell2);
         yval = (double) (tempval3*tempval1);
         zvals [0] = (float) (tempval3*tempval1);

         tempval1 = cos (tempval2);
         tempval3 = (tempval1*tempval1*tempval1);
         tempval3 = (tempval3*st->lshell2);
         xval = (double) (tempval3);
         rhovals [0] = (float) (tempval3);
   
         tempval = ((xval*xval) + (yval*yval));
         tempval = sqrt (tempval);

         if (tempval >= 1.0)
            pgpt_ (&ncount, rhovals, zvals, &nsymbol);

         mlatitude += 0.1;
      }
   }

   if (st->EqEcOrKSM == Zeroeth)
   {
      displ = 3.2;

      if      (st->x == X)
         pgmtxt_ ("B", &displ, &coord, &fjust, "equatorial X (R\\dS\\u)", 1L, 21L);
      else if (st->x == Y)
         pgmtxt_ ("B", &displ, &coord, &fjust, "equatorial Y (R\\dS\\u)", 1L, 21L);
      else if (st->x == Z)
         pgmtxt_ ("B", &displ, &coord, &fjust, "equatorial Z (R\\dS\\u)", 1L, 21L);
      else if (st->x == RHO)
         pgmtxt_ ("B", &displ, &coord, &fjust, "\\gr, distance from Saturn projected on the equatorial plane (R\\dS\\u)", 1L, 68L);
      else if (st->x == RHO_Z)
         pgmtxt_ ("B", &displ, &coord, &fjust, "Z, distance above the equatorial plane (R\\dS\\u)", 1L, 47L);

      displ = 4.0;

      if      (st->y == X)
         pgmtxt_ ("L", &displ, &coord, &fjust, "equatorial X (R\\dS\\u)", 1L, 21L);
      else if (st->y == Y)
         pgmtxt_ ("L", &displ, &coord, &fjust, "equatorial Y (R\\dS\\u)", 1L, 21L);
      else if (st->y == Z)
         pgmtxt_ ("L", &displ, &coord, &fjust, "equatorial Z (R\\dS\\u)", 1L, 21L);
      else if (st->y == RHO)
         pgmtxt_ ("L", &displ, &coord, &fjust, "\\gr, distance from Saturn projected on the equatorial plane (R\\dS\\u)", 1L, 68L);
      else if (st->y == RHO_Z)
         pgmtxt_ ("L", &displ, &coord, &fjust, "Z, distance above the equatorial plane (R\\dS\\u)", 1L, 47L);
   }
   else if (st->EqEcOrKSM == First)
   {
      displ = 3.2;

      if      (st->x == X)
         pgmtxt_ ("B", &displ, &coord, &fjust, "ecliptic X (R\\dS\\u)", 1L, 19L);
      else if (st->x == Y)
         pgmtxt_ ("B", &displ, &coord, &fjust, "ecliptic Y (R\\dS\\u)", 1L, 19L);
      else if (st->x == Z)
         pgmtxt_ ("B", &displ, &coord, &fjust, "ecliptic Z (R\\dS\\u)", 1L, 19L);
      else if (st->x == RHO)
         pgmtxt_ ("B", &displ, &coord, &fjust, "\\gr, distance from Saturn projected on the ecliptic plane (R\\dS\\u)", 1L, 66L);
      else if (st->x == RHO_Z)
         pgmtxt_ ("B", &displ, &coord, &fjust, "Z, distance above the ecliptic plane (R\\dS\\u)", 1L, 45L);

      displ = 4.0;

      if      (st->y == X)
         pgmtxt_ ("L", &displ, &coord, &fjust, "ecliptic X (R\\dS\\u)", 1L, 19L);
      else if (st->y == Y)
         pgmtxt_ ("L", &displ, &coord, &fjust, "ecliptic Y (R\\dS\\u)", 1L, 19L);
      else if (st->y == Z)
         pgmtxt_ ("L", &displ, &coord, &fjust, "ecliptic Z (R\\dS\\u)", 1L, 19L);
      else if (st->y == RHO)
         pgmtxt_ ("L", &displ, &coord, &fjust, "\\gr, distance from Saturn projected on the ecliptic plane (R\\dS\\u)", 1L, 66L);
      else if (st->y == RHO_Z)
         pgmtxt_ ("L", &displ, &coord, &fjust, "Z, distance above the ecliptic plane (R\\dS\\u)", 1L, 45L);
   }
   else if (st->EqEcOrKSM == Second)
   {
      displ = 3.2;

      if      (st->x == X)
         pgmtxt_ ("B", &displ, &coord, &fjust, "KSM X (R\\dS\\u)", 1L, 14L);
      else if (st->x == Y)
         pgmtxt_ ("B", &displ, &coord, &fjust, "KSM Y (R\\dS\\u)", 1L, 14L);
      else if (st->x == Z)
         pgmtxt_ ("B", &displ, &coord, &fjust, "KSM Z (R\\dS\\u)", 1L, 14L);
      else if (st->x == RHO)
         pgmtxt_ ("B", &displ, &coord, &fjust, "\\gr, distance from Saturn projected on the X-Y plane (R\\dS\\u)", 1L, 61L);
      else if (st->x == RHO_Z)
         pgmtxt_ ("B", &displ, &coord, &fjust, "Z, distance above the X-Y plane (R\\dS\\u)", 1L, 40L);

      displ = 4.0;

      if      (st->y == X)
         pgmtxt_ ("L", &displ, &coord, &fjust, "KSM X (R\\dS\\u)", 1L, 14L);
      else if (st->y == Y)
         pgmtxt_ ("L", &displ, &coord, &fjust, "KSM Y (R\\dS\\u)", 1L, 14L);
      else if (st->y == Z)
         pgmtxt_ ("L", &displ, &coord, &fjust, "KSM Z (R\\dS\\u)", 1L, 14L);
      else if (st->y == RHO)
         pgmtxt_ ("L", &displ, &coord, &fjust, "\\gr, distance from Saturn projected on the ecliptic plane (R\\dS\\u)", 1L, 61L);
      else if (st->y == RHO_Z)
         pgmtxt_ ("L", &displ, &coord, &fjust, "Z, distance above the X-Y plane (R\\dS\\u)", 1L, 40L);
   }

   /* draw the plot title */
   if (st->FirstSecondOrThirdPlot == Zeroeth)
   {
      if ((st->st_year % 4) == 0)
         leap1 = 1;
      else
         leap1 = 0;

      for (index1 = 0; index1 < 12; index1++)
         if (st->st_day < m_days [leap1][index1 + 1])
            break;

      dy_mn1 = st->st_day - m_days [leap1][index1] + 1;

      if ((st->sp_year % 4) == 0)
         leap2 = 1;
      else
         leap2 = 0;

      for (index2 = 0; index2 < 12; index2++)
         if (st->sp_day < m_days [leap2][index2 + 1])
            break;

      dy_mn2 = st->sp_day - m_days [leap2][index2] + 1;

      if (st->st_past == st->sp_past)
         sprintf (name, "%.4d %.3d (%s %d) %02d:%02d:%02.0f - %02d:%02d:%02.0f",
                  st->st_year, st->st_day,
                  months [index1], dy_mn1,
                  st->st_hr, st->st_mn, st->st_sec,
                  st->sp_hr, st->sp_mn, st->sp_sec);
      else
         sprintf (name, "%.4d %.3d (%s %d) %02d:%02d:%02.0f - %.4d %.3d (%s %d) %02d:%02d:%02.0f",
                  st->st_year, st->st_day,
                  months [index1], dy_mn1,
                  st->st_hr, st->st_mn, st->st_sec,
                  st->sp_year, st->sp_day,
                  months [index2], dy_mn2,
                  st->sp_hr, st->sp_mn, st->sp_sec);
   }
   else if (st->FirstSecondOrThirdPlot == First)
   {
      if (st->StOrbit == st->SpOrbit)
      {
         if (st->StOrbit == -3)
            sprintf (name, "Orbit 0");
         else if (st->StOrbit == -2)
            sprintf (name, "Orbit A");
         else if (st->StOrbit == -1)
            sprintf (name, "Orbit B");
         else if (st->StOrbit ==  0)
            sprintf (name, "Orbit C");
         else
            sprintf (name, "Orbit %1d", (st->StOrbit + 2));
      }
      else
      {
         if ((st->StOrbit >= 1) && (st->SpOrbit >= 1))
            sprintf (name, "Orbits %1d through %1d", (st->StOrbit + 2), (st->SpOrbit + 2));
         else if ((st->StOrbit <= 0) && (st->SpOrbit <= 0))
         {
            if (st->StOrbit == -3)
            {
               if (st->SpOrbit == -2)
                  sprintf (name, "Orbits 0 through A");
               else if (st->SpOrbit == -1)
                  sprintf (name, "Orbits 0 through B");
               else if (st->SpOrbit ==  0)
                  sprintf (name, "Orbits 0 through C");
            }
            else if (st->StOrbit == -2)
            {
               if (st->SpOrbit == -3)
                  sprintf (name, "shouldn't be here!"); /* not possible */
               else if (st->SpOrbit == -1)
                  sprintf (name, "Orbits A through B");
               else if (st->SpOrbit ==  0)
                  sprintf (name, "Orbits A through C");
            }
            else if (st->StOrbit == -1)
            {
               if (st->SpOrbit == -3)
                  sprintf (name, "shouldn't be here!"); /* not possible */
               else if (st->SpOrbit == -2)
                  sprintf (name, "shouldn't be here!"); /* not possible */
               else if (st->SpOrbit ==  0)
                  sprintf (name, "Orbits B through C");
            }
            else if (st->StOrbit ==  0)
            {
               if (st->SpOrbit == -3)
                  sprintf (name, "shouldn't be here!"); /* not possible */
               else if (st->SpOrbit == -2)
                  sprintf (name, "shouldn't be here!"); /* not possible */
               else if (st->SpOrbit == -1)
                  sprintf (name, "shouldn't be here!"); /* not possible */
            }
         }
         else if ((st->StOrbit <= 0) && (st->SpOrbit >= 1))
         {
            if (st->StOrbit == -3)
               sprintf (name, "Orbits 0 through %1d", (st->SpOrbit + 2));
            else if (st->StOrbit == -2)
               sprintf (name, "Orbits A through %1d", (st->SpOrbit + 2));
            else if (st->StOrbit == -1)
               sprintf (name, "Orbits B through %1d", (st->SpOrbit + 2));
            else if (st->StOrbit ==  0)
               sprintf (name, "Orbits C through %1d", (st->SpOrbit + 2));
         }
         else if ((st->StOrbit >= 1) && (st->SpOrbit <= 0))
         {
            if (st->SpOrbit == -3)
               sprintf (name, "shouldn't be here!"); /* not possible */
            else if (st->SpOrbit == -2)
               sprintf (name, "shouldn't be here!"); /* not possible */
            else if (st->SpOrbit == -1)
               sprintf (name, "shouldn't be here!"); /* not possible */
            else if (st->SpOrbit ==  0)
               sprintf (name, "shouldn't be here!"); /* not possible */
         }
      }
   }

   displ = 1.5;
   pgmtxt_ ("T", &displ, &coord, &fjust, name, 1L, (long) strlen (name));

   displ = 3.0;
   pgmtxt_ ("T", &displ, &coord, &fjust, st->title, 1L, (long) strlen (st->title));

   if (st->FirstSecondOrThirdPlot == Zeroeth)
   {
      if (st->ticks <= 3600.0)
         sprintf (name, "Tick marks every %1.1f minutes", (st->ticks/60.0));
      else
         sprintf (name, "Tick marks every %1.1f hours", (st->ticks/3600.0));

      displ = 8.0;
      coord = 0.25;

      pgmtxt_ ("R", &displ, &coord, &fjust, name, 1L, (long) strlen (name));
   }
}

double GetTickTime (st)
struct time_struc *st;
{
   double OneMinute = 60000.0, OneHour = 3600000.0, OneDay = 8.64e+07;
   double TickTime, TicksMS;

   TicksMS = (st->ticks*1000.0);

   if (st->ticks == 0.0)
      TickTime = 0.0;
   else if (fmod (TicksMS, OneDay) == 0.0)
   {
      TickTime = (double) (st->st_past*OneDay);

      if (fmod (st->st_total_ms, OneDay) != 0.0)
         TickTime += OneDay;
   }
   else if (fmod (TicksMS, OneHour) == 0.0)
   {
      TickTime  = (double) (st->st_past*OneDay);
      TickTime += (double) (st->st_hr*OneHour);

      if (fmod (st->st_total_ms, OneHour) != 0.0)
         TickTime += OneHour;
   }
   else if (fmod (TicksMS, OneMinute) == 0.0)
   {
      TickTime  = (double) (st->st_past*OneDay);
      TickTime += (double) (st->st_hr*OneHour);
      TickTime += (double) (st->st_mn*OneMinute);

      if (fmod (st->st_total_ms, OneMinute) != 0.0)
         TickTime += OneMinute;
   }
   else
      TickTime = 0.0;

   return (TickTime);
}

double GetDeltaTime (stime)
struct time_struc *stime;
{
   int NoDelta;
   int itemp, OldItemp;
   double TimeSpan, TickTime, DeltaTime;

   NoDelta = True;
   DeltaTime = 1.0;
   TimeSpan = (stime->sp_total_ms - stime->st_total_ms);

   TickTime = stime->ticks;
   TickTime *= 1000.0;

   while (DeltaTime < TimeSpan)
   {
      if ((fmod (TimeSpan, DeltaTime) == 0.0) &&
          (fmod (TickTime, DeltaTime) == 0.0))
      {
         NoDelta = False;

         OldItemp = itemp;

         itemp = (int) (TimeSpan/DeltaTime);

         if (itemp < 500)
            break;
      }

      if (DeltaTime < 1000.0)
         DeltaTime +=    1.0;
      else
         DeltaTime += 1000.0;
   }

   if (NoDelta)
      DeltaTime = 1000.0;
   else
      DeltaTime = (TimeSpan/(double) OldItemp);

   return (DeltaTime);
}

int ProcessData (stime)
struct time_struc *stime;
{
   int nsymbol = 850, tick_count = 0, data_count;
   int no_data, past, year, doy, hr, mn;
   int red = 2, green = 3, black = 1;
   float xpt, ypt;
   float StXpt, StYpt;
   float SpXpt, SpYpt;
   float xvals [MAX_POINTS], yvals [MAX_POINTS];
   double et, dum1, dum2, dum3;
   double DeltaPoint, DeltaTicks, TickTime;
   double delta, sec, st_ms;
   double rho, x, y, z;
   char scstr [80];
   struct time_struc sttime;

   sttime.observer = Cassini;
   sttime.target = Saturn;

   DeltaTicks = (double) (stime->ticks*1000.0);

   DeltaPoint = GetDeltaTime (stime);

   TickTime = GetTickTime (stime);

   delta = 1000.0;

   data_count = 0;
   no_data = True;
   st_ms = stime->st_total_ms;

   while (st_ms <= stime->sp_total_ms)
   {
      if (data_count >= MAX_POINTS)
      {
         st_ms += delta;
         continue;
      }

      ms2dhms (&past, &hr, &mn, &sec, st_ms);
      yrdy1958 (&year, &doy, past);

      sprintf (scstr, "%04d %03d // %02d:%02d:%06.3f",
               year, doy, hr, mn, (float) sec);

      if (fmod (st_ms, DeltaPoint) == 0.0)
      {
         no_data = False;

         utc2et (scstr, &et);

         sttime.st_year = year;
         sttime.st_day = doy;
         sttime.st_hr = hr;
         sttime.st_mn = mn;
         sttime.st_sec = (float) sec;

         if (stime->EqEcOrKSM == Zeroeth)
            (void) get_oaEQ (&sttime, &x, &y, &z);
         else if (stime->EqEcOrKSM == First)
            (void) get_oaEC (&sttime, &x, &y, &z);
         else if (stime->EqEcOrKSM == Second)
         {
            getksm_ (&sttime.target, &et, &x, &y, &z, &dum1, &dum2, &dum3);

            x /= RSaturn;
            y /= RSaturn;
            z /= RSaturn;
         }

         rho = ((x*x) + (y*y));
         rho = sqrt (rho);

         if      (stime->x == X)
            xvals [data_count] = (float) x;
         else if (stime->x == Y)
            xvals [data_count] = (float) y;
         else if (stime->x == Z)
            xvals [data_count] = (float) z;
         else if (stime->x == RHO)
            xvals [data_count] = (float) rho;
         else if (stime->x == RHO_Z)
            xvals [data_count] = (float) z;

         if      (stime->y == X)
            yvals [data_count] = (float) x;
         else if (stime->y == Y)
            yvals [data_count] = (float) y;
         else if (stime->y == Z)
            yvals [data_count] = (float) z;
         else if (stime->y == RHO)
            yvals [data_count] = (float) rho;
         else if (stime->y == RHO_Z)
            yvals [data_count] = (float) z;

         data_count++;
      }

      if ((fmod ((st_ms - TickTime), DeltaTicks) == 0.0) && (DeltaTicks > 0.0))
      {
         utc2et (scstr, &et);

         sttime.st_year = year;
         sttime.st_day = doy;
         sttime.st_hr = hr;
         sttime.st_mn = mn;
         sttime.st_sec = (float) sec;

         if (stime->EqEcOrKSM == Zeroeth)
            (void) get_oaEQ (&sttime, &x, &y, &z);
         else if (stime->EqEcOrKSM == First)
            (void) get_oaEC (&sttime, &x, &y, &z);
         else if (stime->EqEcOrKSM == Second)
         {
            getksm_ (&sttime.target, &et, &x, &y, &z, &dum1, &dum2, &dum3);

            x /= RSaturn;
            y /= RSaturn;
            z /= RSaturn;
         }

         rho = ((x*x) + (y*y));
         rho = sqrt (rho);

         if      (stime->x == X)
            xpt = (float) x;
         else if (stime->x == Y)
            xpt = (float) y;
         else if (stime->x == Z)
            xpt = (float) z;
         else if (stime->x == RHO)
            xpt = (float) rho;
         else if (stime->x == RHO_Z)
            xpt = (float) z;

         if      (stime->y == X)
            ypt = (float) x;
         else if (stime->y == Y)
            ypt = (float) y;
         else if (stime->y == Z)
            ypt = (float) z;
         else if (stime->y == RHO)
            ypt = (float) rho;
         else if (stime->y == RHO_Z)
            ypt = (float) z;

         pgpt1_ (&xpt, &ypt, &nsymbol);

         if (tick_count == 0)
         {
            StXpt = xpt;
            StYpt = ypt;
         }

         SpXpt = xpt;
         SpYpt = ypt;

         tick_count++;
      }

      st_ms += delta;
   }

   pgline_ (&data_count, xvals, yvals);

   if (DeltaTicks > 0.0)
   {
      pgsci_ (&green);

      pgpt1_ (&StXpt, &StYpt, &nsymbol);

      pgsci_ (&red);

      pgpt1_ (&SpXpt, &SpYpt, &nsymbol);

      pgsci_ (&black);
   }

   if (stime->FirstSecondOrThirdPlot == Zeroeth)
      pgend_ ();

   return (no_data);
}

int main (int argc, char *argv [])
{
   register int m = 0, x;
   int counter;
   int year, month, day, doy, hr, mn, past, cl, index;
   int StIndex, SpIndex;
   double st_ms, sp_ms, sec, dtemp;
   char st_string [80], sp_string [80];
   char *dummy;
   char *name = "CassiniKernels.list";
   char temp [80];
   entry entries [MAX_ENTRIES];
   struct time_struc stime;

   if (strcmp (getenv ("REQUEST_METHOD"), "POST"))
   {
      printf ("Content-type: text/html%c%c", 10, 10);
      printf ("This script should be referenced with a METHOD of POST.\n");
      printf ("If you don't understand this, see this ");
      printf ("<A HREF=\"http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/fill-out-forms/overview.html\">forms overview</A>.%c",10);
      exit (1);
   }

   if (strcmp (getenv ("CONTENT_TYPE"), "application/x-www-form-urlencoded"))
   {
      printf ("Content-type: text/html%c%c", 10, 10);
      printf ("This script can only be used to decode form results. \n");
      exit (1);
   }

   cl = atoi (getenv ("CONTENT_LENGTH"));

   for (x = 0; cl && (!feof (stdin)); x++)
   {
      m = x;
      entries [x].val = fmakeword (stdin, '&', &cl);
      plustospace (entries [x].val);
      unescape_url (entries [x].val);
      entries [x].name = makeword (entries [x].val,'=');
   }

   for (counter = 0; counter < x; counter++)
   {
      sprintf (temp, "%s", entries [counter].val);

      if (!strcmp (entries [counter].name, "entry0"))
      {
         for (index = 0; index < (int) strlen (temp); index++)
            if ((!(isdigit (temp [index]))) && (temp [index] != ' '))
            {
               printf ("Content-type: text/html%c%c", 10, 10);
               printf ("Invalid plot entry  %s\n", temp);
               return (0);
            }

         dtemp = (double) strtod (temp, &dummy);

         if ((int) dtemp == 1)
            stime.FirstSecondOrThirdPlot = Zeroeth;
         else if ((int) dtemp == 2)
            stime.FirstSecondOrThirdPlot = First;
         else if ((int) dtemp == 3)
            stime.FirstSecondOrThirdPlot = Second;
         else
         {
            printf ("Content-type: text/html%c%c", 10, 10);
            printf ("Invalid plot entry  %s\n", temp);
            return (0);
         }
      }
      else if (!strcmp (entries [counter].name, "entry1"))
      {
         if (stime.FirstSecondOrThirdPlot == Zeroeth)
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

            stime.st_past = past;
            stime.st_year = year;
            stime.st_day = doy;
            stime.st_hr = hr;
            stime.st_mn = mn;
            stime.st_sec = (float) sec;
            stime.st_total_ms = st_ms;
         }
         else if (stime.FirstSecondOrThirdPlot == First)
         {
            if (ProcessString (temp) == NotTrue)
            {
               for (index = 0; index < (int) strlen (temp); index++)
                  if ((!(isdigit (temp [index]))) && (temp [index] != ' '))
                  {
                     printf ("Content-type: text/html%c%c", 10, 10);
                     printf ("%s is not a valid orbit name.  \n", temp);
                     printf ("Choose from the following orbits: 0, A, B, C, 3, 4, ... , 292, 293\n");
                     return (0);
                  }

               stime.StOrbit = (int) strtod (temp, &dummy);

               if ((stime.StOrbit < -3) || (stime.StOrbit == 1) ||
                   (stime.StOrbit == 2) || (stime.StOrbit > 293))
               {
                  printf ("Content-type: text/html%c%c", 10, 10);
                  printf ("%s is not a valid orbit name.  \n", temp);
                  printf ("Choose from the following orbits: 0, A, B, C, 3, 4, ... , 292, 293\n");
                  return (0);
               }

               stime.StOrbit -= 2;
            }
            else
               stime.StOrbit = ProcessString (temp);
         }
      }
      else if (!strcmp (entries [counter].name, "entry2"))
      {
         if (stime.FirstSecondOrThirdPlot == Zeroeth)
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

            stime.sp_past = past;
            stime.sp_year = year;
            stime.sp_day = doy;
            stime.sp_hr = hr;
            stime.sp_mn = mn;
            stime.sp_sec = (float) sec;
            stime.sp_total_ms = sp_ms;
         }
         else if (stime.FirstSecondOrThirdPlot == First)
         {
            if (ProcessString (temp) == NotTrue)
            {
               for (index = 0; index < (int) strlen (temp); index++)
                  if ((!(isdigit (temp [index]))) && (temp [index] != ' '))
                  {
                     printf ("Content-type: text/html%c%c", 10, 10);
                     printf ("%s is not a valid orbit name.  \n", temp);
                     printf ("Choose from the following orbits: 0, A, B, C, 3, 4, ... , 292, 293\n");
                     return (0);
                  }

               stime.SpOrbit = (int) strtod (temp, &dummy);

               if ((stime.SpOrbit < -3) || (stime.SpOrbit == 1) ||
                   (stime.SpOrbit == 2) || (stime.SpOrbit > 293))
               {
                  printf ("Content-type: text/html%c%c", 10, 10);
                  printf ("%s is not a valid orbit name.  \n", temp);
                  printf ("Choose from the following orbits: 0, A, B, C, 3, 4, ... , 292, 293\n");
                  return (0);
               }

               stime.SpOrbit -= 2;
            }
            else
               stime.SpOrbit = ProcessString (temp);
         }
      }
      else if (!strcmp (entries [counter].name, "entry3"))
      {
         for (index = 0; index < (int) strlen (temp); index++)
            if ((!(isdigit (temp [index]))) && (temp [index] != ' '))
            {
               printf ("Content-type: text/html%c%c", 10, 10);
               printf ("Invalid hour entry  %s\n", temp);
               return (0);
            }

         dtemp = (double) strtod (temp, &dummy);

         stime.ticks = (float) (3600.0*dtemp);
      }
      else if (!strcmp (entries [counter].name, "entry4"))
      {
         for (index = 0; index < (int) strlen (temp); index++)
            if ((!(isdigit (temp [index]))) && (temp [index] != ' '))
            {
               printf ("Content-type: text/html%c%c", 10, 10);
               printf ("Invalid minute entry  %s\n", temp);
               return (0);
            }

         dtemp = (double) strtod (temp, &dummy);

         stime.ticks += (float) (60.0*dtemp);
      }
      else if (!strcmp (entries [counter].name, "entry5"))
      {
         if      (!strcmp (temp, "sequ"))
            stime.EqEcOrKSM = Zeroeth;
         else if (!strcmp (temp, "secl"))
            stime.EqEcOrKSM = First;
         else if (!strcmp (temp, "sksm"))
            stime.EqEcOrKSM = Second;
         else
         {
            printf ("Content-type: text/html%c%c", 10, 10);
            printf ("%s: unknown coordinate system type %s\n", argv [0], temp);
            exit (1);
         }
      }
      else if (!strcmp (entries [counter].name, "entry6"))
      {
         if      (!strcmp (temp, "plt1"))
         {
            stime.x = X;
            stime.y = Y;
         }
         else if (!strcmp (temp, "plt2"))
         {
            stime.x = X;
            stime.y = Z;
         }
         else if (!strcmp (temp, "plt3"))
         {
            stime.x = Y;
            stime.y = Z;
         }
         else if (!strcmp (temp, "plt4"))
         {
            stime.x = RHO;
            stime.y = RHO_Z;
         }
         else
         {
            printf ("Content-type: text/html%c%c", 10, 10);
            printf ("%s: unknown plot type %s\n", argv [0], temp);
            exit (1);
         }
      }
      else if (!strcmp (entries [counter].name, "entry7"))
      {
         for (index = 0; index < (int) strlen (temp); index++)
            if ((!(isdigit (temp [index]))) &&
                           (temp [index] != ' ') &&
                           (temp [index] != '.') &&
                           (temp [index] != '+') &&
                           (temp [index] != '-'))
            {
               printf ("Content-type: text/html%c%c", 10, 10);
               printf ("Invalid entry  %s\n", temp);
               return (0);
            }

         stime.xmin = (float) strtod (temp, &dummy);
      }
      else if (!strcmp (entries [counter].name, "entry8"))
      {
         for (index = 0; index < (int) strlen (temp); index++)
            if ((!(isdigit (temp [index]))) &&
                           (temp [index] != ' ') &&
                           (temp [index] != '.') &&
                           (temp [index] != '+') &&
                           (temp [index] != '-'))
            {
               printf ("Content-type: text/html%c%c", 10, 10);
               printf ("Invalid entry  %s\n", temp);
               return (0);
            }

         stime.xmax = (float) strtod (temp, &dummy);
      }
      else if (!strcmp (entries [counter].name, "entry9"))
      {
         for (index = 0; index < (int) strlen (temp); index++)
            if ((!(isdigit (temp [index]))) &&
                           (temp [index] != ' ') &&
                           (temp [index] != '.') &&
                           (temp [index] != '+') &&
                           (temp [index] != '-'))
            {
               printf ("Content-type: text/html%c%c", 10, 10);
               printf ("Invalid entry  %s\n", temp);
               return (0);
            }

         stime.ymin = (float) strtod (temp, &dummy);
      }
      else if (!strcmp (entries [counter].name, "entry10"))
      {
         for (index = 0; index < (int) strlen (temp); index++)
            if ((!(isdigit (temp [index]))) &&
                           (temp [index] != ' ') &&
                           (temp [index] != '.') &&
                           (temp [index] != '+') &&
                           (temp [index] != '-'))
            {
               printf ("Content-type: text/html%c%c", 10, 10);
               printf ("Invalid entry  %s\n", temp);
               return (0);
            }

         stime.ymax = (float) strtod (temp, &dummy);
      }
      else if (!strcmp (entries [counter].name, "entry11"))
      {
         for (index = 0; index < (int) strlen (temp); index++)
            if ((!(isdigit (temp [index]))) &&
                           (temp [index] != ' ') &&
                           (temp [index] != '.') &&
                           (temp [index] != '+') &&
                           (temp [index] != '-'))
            {
               printf ("Content-type: text/html%c%c", 10, 10);
               printf ("Invalid entry  %s\n", temp);
               return (0);
            }

         stime.lshell1 = (double) strtod (temp, &dummy);
      }
      else if (!strcmp (entries [counter].name, "entry12"))
      {
         for (index = 0; index < (int) strlen (temp); index++)
            if ((!(isdigit (temp [index]))) &&
                           (temp [index] != ' ') &&
                           (temp [index] != '.') &&
                           (temp [index] != '+') &&
                           (temp [index] != '-'))
            {
               printf ("Content-type: text/html%c%c", 10, 10);
               printf ("Invalid entry  %s\n", temp);
               return (0);
            }

         stime.lshell2 = (double) strtod (temp, &dummy);
      }
      else if (!strcmp (entries [counter].name, "entry13"))
      {
         sprintf (stime.title, "%s", temp);
      }
      else if (!strcmp (entries [counter].name, "entry14"))
      {
         if      (!strcmp (temp, "gif"))
            stime.psorgif = False;
         else if (!strcmp (temp, "pps"))
            stime.psorgif = True;
         else
         {
            printf ("Content-type: text/html%c%c", 10, 10);
            printf ("%s: unknown output type %s\n", argv [0], temp);
            exit (1);
         }
      }
   }

   if (stime.x != RHO)
   {
      stime.lshell1 = 0.0;
      stime.lshell2 = 0.0;
   }

   if (stime.FirstSecondOrThirdPlot == Zeroeth)
   {
      if (!(ValidateTime (&stime)))
         exit (1);
   }

   furnsh_ (name, strlen (name));

   if (stime.StOrbit > stime.SpOrbit)
   {
      index = stime.StOrbit;
      stime.StOrbit = stime.SpOrbit;
      stime.SpOrbit = index;
   }

   if (stime.FirstSecondOrThirdPlot == Zeroeth)
   {
      (void) InitPlot (&stime);

      ProcessData (&stime);
   }
   else if (stime.FirstSecondOrThirdPlot == First)
   {
      stime.ticks = 0.0;

      (void) InitPlot (&stime);

      StIndex = stime.StOrbit;
      SpIndex = stime.SpOrbit;

      StIndex += 3;
      SpIndex += 3;

      for (index = StIndex; index <= SpIndex; index++)
      {
         sprintf (temp, "%s", OrbitStTimes [index]);

         if (parsetime (temp, &year, &month, &day, &doy, &hr, &mn, &sec))
         {
            printf ("Content-type: text/html%c%c", 10, 10);
            printf ("%s: error parsing %s\n", argv [0], temp);
            exit (1);
         }

         past = past_1958 (year, doy);

         st_ms = (double) past*86400000.0 +
                 (double) hr*3600000.0 +
                 (double) mn*60000.0 +
                 (double) sec*1000.0;

         stime.st_past = past;
         stime.st_year = year;
         stime.st_day = doy;
         stime.st_hr = hr;
         stime.st_mn = mn;
         stime.st_sec = (float) sec;
         stime.st_total_ms = st_ms;

         sprintf (temp, "%s", OrbitSpTimes [index]);

         if (parsetime (temp, &year, &month, &day, &doy, &hr, &mn, &sec))
         {
            printf ("Content-type: text/html%c%c", 10, 10);
            printf ("%s: error parsing %s\n", argv [0], temp);
            exit (1);
         }

         past = past_1958 (year, doy);

         sp_ms = (double) past*86400000.0 +
                 (double) hr*3600000.0 +
                 (double) mn*60000.0 +
                 (double) sec*1000.0;

         stime.sp_past = past;
         stime.sp_year = year;
         stime.sp_day = doy;
         stime.sp_hr = hr;
         stime.sp_mn = mn;
         stime.sp_sec = (float) sec;
         stime.sp_total_ms = sp_ms;

         ProcessData (&stime);
      }

      pgend_ ();
   }

   return (0);
}
