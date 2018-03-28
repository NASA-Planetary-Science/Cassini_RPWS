
/*********************************************/

/* Copyright 1993-2003, William T Robison    */

/*                      952 N. St. SW        */

/*                      Cedar Rapids, Iowa   */

/*                          52404-2710       */

/*                      U.S.A.               */

/*********************************************/

/*       See COPYING for terms of use        */

/* This software is covered under terms of   */

/* the GNU General Public License as         */

/* published by the Free Software Foundation */

/*********************************************/

/* You should have received this software at */

/* no charge.  If you have been charged for  */

/* the privelege of obtaining a copy please  */

/* contact the author at                     */

/*                william-robison@uiowa.edu  */

/*********************************************/

/*       PCL_PLOT.C               */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __MSDOS__
#include "source\plotinit.h"
#include "source\symbol.h"
#include "source\util.h"
#include "source\la.h"
#else
#include "plotinit.h"
#include "symbol.h"
#include "util.h"
#include "la.h"
#endif

#define DEFAULT_DPI 150.0
#define POINTS 72.0
#define FUDGE 1.50
#define CHAR_WIDTH 0.75
#define TYPE 4101
#define MIN_PIX 0
#define ESC 0x1B
#define SPACING 1                       /* 0=fixed 1=proportional */

                            /*
                             * 1         2         3   3  
                             */
                            /*
                             * 1234567890123456789012345678901234  
                             */
static char raster[] = { "RAS" };
static char driver_name[] = { "PCL_PLOT, RASTER/PCL plot driver,       " };

#define DRIVER_NAME_COMMA 34
static long offset_x = 0;
static long offset_y = 0;
static int typeface = TYPE;
static float current_s = 1.0;           /* scale factor */

int pcl_width = 3;                      /* line width, pixels */
static long edge;
static float dpi = DEFAULT_DPI;
float pcl_size_[2] = { 10.5, 8.0 };


static int plotting = 0;
static char *pcl_fname;

extern FILE *plotout;
extern int page_count;
extern float set_date_size[3];
extern int orient[3];
static float pcl_set_date_size[3] = { 10.00, 15.00, 0.25 };
int pcl_factor (float val)
{
  current_s = val;
  return plotting;
}
long pcll_scale (float val, int loc)
{
  float q;

  q = val * current_s;
  switch (loc) {
   case 1:
   case 10:
     return offset_x + (long) (q * dpi);
   case 2:
   case 20:                            /* IV quadrant */
     return edge - (offset_y + (long) (q * dpi));
   case 30:
     return (long) (q * dpi);
   default:
     return (long) q;
  }
}
float pclf_scale (float val)
{
  return val * current_s * POINTS * FUDGE;
}

char *pcl_setplt (char *ver, char *date, char *fname, int flag)
{
  char *resolution = NULL;
  char *type_c = NULL;
  int i;

  /*
   *  flag kind-of indicates PCL level, so
   *  make stick characters for lower PCL levels
   *  (Raster images use stick chars)
   */
  if (flag < 5)
    symbol_read ();

  for (i = 0; i < 3; i++)
    set_date_size[i] = pcl_set_date_size[i];

  if (plotting == 2)
    fclose (plotout);
  if (fname == NULL) {
    plotout = stdout;
    plotting = 1;
  } else {
    plotout = plot_fopen (fname, BINARY);
    if (plotout != NULL)
      plotting = 2;
  }

  type_c = getenv ("PLP_FONT");
  if (type_c)
    typeface = atoi (type_c);
  resolution = getenv ("RESOLUTION");
  if (resolution)
    dpi = atof (resolution);
  pcl_width = (int) (dpi / 100.0);
  edge = (int) dpi *8;

  pcl_fname = fname;
  plotting = la_allocate (pcl_size_[0], pcl_size_[1], dpi);
  plot_init (plotout, 0);
  if (flag) {
    fprintf (plotout, "%cE", ESC);      /* reset */
    fprintf (plotout, "%c(8U%c)8U", ESC, ESC);  /* PCL symbol set ID */
    fprintf (plotout, "%c(s%iP%c)s%iP", ESC, SPACING, ESC, SPACING);    /* spacing */
    fprintf (plotout, "%c(s0S%c)s0S", ESC, ESC);        /* style */
    fprintf (plotout, "%c(s0B%c)s3B", ESC, ESC);        /* stroke weight */
    fprintf (plotout, "%c(s%iT%c)s%iT", ESC, typeface, ESC, typeface);  /* typeface */
  }
  plot_init (plotout, 10);
  if (flag) {
    fprintf (plotout, "%c&u%iD", ESC, (int) dpi);       /* unit of measure =pixels */
    fprintf (plotout, "%c&l%iO", ESC, orient[0]);       /* orient landscape */
  }
  plot_init (plotout, 20);
  if (flag)
    if (page_count > 1)
      fprintf (plotout, "%c&l%iX", ESC, page_count);

  if (page_count)
    sprintf (&driver_name[DRIVER_NAME_COMMA + 2], "%i", page_count);
  else
    driver_name[DRIVER_NAME_COMMA] = 0;

  if (!flag) {
    driver_name[0] = raster[0];
    driver_name[1] = raster[1];
    driver_name[2] = raster[2];
  }

  return driver_name;
}
int pcl_wrapup (void)
{
  plotting = la_frame (plotout);
  plotting = la_deallocate ();
  fprintf (plotout, "%cE", ESC);
  plot_clear (plotout, 0);
  fclose (plotout);
  plotting = 0;
  return plotting;
}
int pcl_wrapup6 (void)
{
  return pcl_wrapup ();
}
int pcl_wrapup7 (void)
{
  plotting = la_frame7 (plotout);
  plotting = la_deallocate ();
  plot_clear (plotout, 0);
  fclose (plotout);
  plotting = 0;
  return plotting;
}
int pcl_frame (void)
{
  plotting = la_frame (plotout);
  plot_page (plotout, 0);
  fprintf (plotout, "\014");
  plot_page (plotout, 99);
  return plotting;
}
int pcl_frame6 (void)
{
  return pcl_frame ();
}
int pcl_frame7 (void)
{
  plotting = la_frame7 (plotout);
  plot_page (plotout, 0);
  fprintf (plotout, "\014");
  plot_page (plotout, 99);
  return plotting;
}
int pcl_rect (long x, long y, long xw, long yw)
{
  long ix, iy, ixw, iyw;

  ix = x;
  iy = y;
  ixw = xw;
  iyw = yw;
  if (xw < 0) {
    ix = x + xw;
    ixw = labs (xw);
  }
  if (yw < 0) {
    iy = y + yw;
    iyw = labs (yw);
  }

  if (pcl_width > 1) {
    if (ixw > pcl_width)
      ixw = ixw + pcl_width - 1;
    if (iyw > pcl_width)
      iyw = iyw + pcl_width - 1;
  }

  fprintf (plotout, "%c*p%liX%c*p%liY", ESC, ix, ESC, iy);
  fprintf (plotout, "%c*c%liA%c*c%liB", ESC, ixw, ESC, iyw);
  fprintf (plotout, "%c*c0P", ESC);
  return 1;
}
int pcl_line (long from_x, long from_y, long to_x, long to_y)
{
  int i = 0;

  if (from_x - to_x)
    i = i | 1;
  if (from_y - to_y)
    i = i | 2;

#if MIN_PIX == 0
  i = 3;
#else
  switch (i) {
   case 0:                             /* DOT */
     break;
   case 1:                             /* horiz line */
     if (labs (from_x - to_x) < MIN_PIX)
       i = 3;
     break;
   case 2:                             /* ver line */
     if (labs (from_y - to_y) < MIN_PIX)
       i = 3;
     break;
   default:                            /*  */
     break;
  }
#endif

  switch (i) {
   case 0:
     pcl_rect (from_x, from_y, pcl_width, pcl_width);
     break;
   case 1:
     pcl_rect (from_x, from_y, to_x - from_x, pcl_width);
     break;
   case 2:
     pcl_rect (from_x, from_y, pcl_width, to_y - from_y);
     break;
   case 3:
     la_line (from_x, from_y, to_x, to_y);
     break;
  }
  return 1;
}
int pcl_dot (long to_x, long to_y)
{

#if MIN_PIX == 0
  la_dot (to_x, to_y);
#else
  pcl_rect (to_x, to_y, pcl_width, pcl_width);
#endif

  return 1;
}
int pcl_plot (float x, float y, int ipen)
{
  static long from_x = 0;
  static long from_y = 0;
  static long to_x, to_y;

  if (!plotting)
    return 0;

  to_x = pcll_scale (x, 1);
  to_y = pcll_scale (y, 2);

  switch (labs (ipen)) {
   case PEN_DOWN:                      /* draw line */
     pcl_line (from_x, from_y, to_x, to_y);
     break;
   case PEN_DOT:
     pcl_dot (to_x, to_y);
     break;
   default:                            /* unknown (i.e. ERROR)  */
   case 3:                             /* move only */
     break;
  }
  from_x = to_x;
  from_y = to_y;
  switch (labs (ipen)) {
   case -2:
   case -3:
   case -4:
     offset_x = pcll_scale (x, 1);
     offset_y = pcll_scale (y, 1);
   default:
     break;
  }
  return plotting;
}
int pcl_symbol (float x, float y, float h, char *c, float theta, int just)
{
  long ix;
  long hgt;

  if (!plotting)
    return 0;

  if (h > 0.25) {
    fprintf (plotout, "\016");
    fprintf (plotout, "%c)s%.2fV", ESC, pclf_scale (h));
  } else {
    fprintf (plotout, "\017");
    fprintf (plotout, "%c(s%.2fV", ESC, pclf_scale (h));
  }

  hgt = pcll_scale ((h * (float) strlen (c) * CHAR_WIDTH), 30);
  ix = pcll_scale (x, 10);
  switch (just) {
   case 0:
     ix = ix - hgt / 2;
     break;
   case 1:
     ix = ix - hgt;
     break;
   default:
     break;
  }
  fprintf (plotout, "%c*p%liX%c*p%liY", ESC, ix, ESC, pcll_scale (y, 20));
  fputs (c, plotout);
  return plotting;
}
int pcl_symbol6 (float x, float y, float h, char *c, float theta, int just)
{
  int il, ic;
  int ip;
  int ichar;
  float xc, yc;
  float xl, yl;
  float xo;
  char ipen;

  if (!plotting)
    return 0;
  il = strlen (c);
  xo = 0;
  for (ic = 0; ic < il; ic++) {
    ichar = c[ic];
    if (symbol_entry (ichar)) {
      while (symbol_value (&xc, &yc, &ipen, h));
      xo = xo + xc;
    }
  }
  xl = x;
  yl = y;
  switch (just) {
   case 0:
     xl = x - (xo / 2.0);
     break;
   case 1:
     xl = x - xo;
     break;
  }
  for (ic = 0; ic < il; ic++) {
    ichar = c[ic];
    if (symbol_entry (ichar)) {
      while (symbol_value (&xc, &yc, &ipen, h)) {
        ip = PEN_UP;
        if (ipen == 'D')
          ip = PEN_DOWN;
        pcl_plot (xc + xl, yc + yl, ip);
      }
      xl = xl + xc;
    }
  }
  return plotting;
}
int pcl_macro (float x, float y, float h, char *name)
{
  return 0;
}
