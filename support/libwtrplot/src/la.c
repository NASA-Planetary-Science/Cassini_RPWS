
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __MSDOS__
#include <alloc.h>
#include "source\plotinit.h"
#else
#include "plotinit.h"
#endif

 /***********************************************
  *
  *	Line Approximation routine
  *
  ***********************************************/

#define ESC 0x1B
#define RASTER

long la_xoffset = 0;
long la_yoffset = 0;
extern int pcl_width;
static long memory;
static int dpi_;
static long x_byte;
static long x_bit;
static long y_byte;
static int istat = 1;

#ifdef __MSDOS__
static char huge *array = NULL;
#else
static char *array = NULL;
#endif

  /***************************
   * bits are arranged in X
   ***************************/
void la_set (long x, long y)
{
  long addr;
  long lbit;

  lbit = x + (y * x_bit);
  addr = lbit >> 3;
  if (array)
    array[addr] = array[addr] | (0x80 >> (lbit & 0x07));
  return;
}
void la_block (long x, long y)
{
  long i, j;

  for (i = 0; i < pcl_width; i++)
    for (j = 0; j < pcl_width; j++)
      la_set (x + i, y + j);
  return;
}

void la_clear (void)
{
  long i;

  if (array) {
    for (i = 0; i < memory; i++) {
      array[i] = 0;
    }
  }
  return;
}
int la_allocate (float x_size, float y_size, float dpi)
{
  dpi_ = dpi;

  y_byte = ((long) (y_size * dpi) + 7) & 0xFFFFFFF8;


  x_bit = ((long) (x_size * dpi) + 31) & 0xFFFFFFE0;
  x_byte = x_bit >> 3;

  memory = x_byte * y_byte;             /* size of memory array */

#ifdef __MSDOS__
  array = farmalloc (memory);
#else
  array = malloc (memory);
#endif

  la_clear ();
  return istat;
}

int la_deallocate (void)
{
  return istat;
}

#ifdef __MSDOS__
void la_row (FILE * plotout, char huge * line)
#else
void la_row (FILE * plotout, char *line)
#endif
{
  int ix;
  char temp[32];

  for (ix = x_byte; ix >= 0; ix--) {
    if (line[ix - 1]) {

#ifdef RASTER
      /*
       * fprintf(plotout,"%c*b%iW",ESC, ix);
       */
      sprintf (temp, "%c*b%iW", ESC, ix);
      fwrite (temp, strlen (temp), 1, plotout);

#ifdef SHITTY
      for (i = 0; i < ix; i++)
        fputc (line[i], plotout);
#else
      fwrite (line, ix, 1, plotout);
#endif

      plot_delay (1);                   /* overun printer ??? */
#endif

      return;
    }
  }

#ifdef RASTER
  fprintf (plotout, "%c*b0W", ESC);
#endif
}

#ifdef __MSDOS__
void la_row7 (FILE * plotout, int iy, char huge * line)
#else
void la_row7 (FILE * plotout, int iy, char *line)
#endif
{
  int left;
  int right;
  int ix;

  for (left = 0; left < x_byte; left++)
    if (line[left])
      break;
  if (left == x_byte)
    return;
  for (right = x_byte - 1; right >= 0; right--)
    if (line[right])
      break;

#ifdef RASTER
  fprintf (plotout, "%i %i %i:", iy, left, right);
  for (ix = left; ix <= right; ix++)
    fprintf (plotout, "%2.2X ", (int) (line[ix] & 0x00FF));
  fprintf (plotout, "\n");
#endif

  return;
}

int la_frame (FILE * plotout)
{
  long iy;

#ifdef RASTER
  fprintf (plotout, "%c*r0F", ESC);     /* orientation mode */
  fprintf (plotout, "%c*p%ix%iY", ESC, 0, 0);   /* orientation mode */
  plot_pcl_init (plotout, 0);
  fprintf (plotout, "%c*t%iR", ESC, dpi_);      /* select raster resolution */
  fprintf (plotout, "%c*r1A", ESC);     /* start graphics at left margin */
  plot_pcl_init (plotout, 10);
#endif

  for (iy = 0; iy < y_byte; iy++)
    la_row (plotout, &array[iy * x_byte]);

#ifdef RASTER
  plot_pcl_init (plotout, 80);
  fprintf (plotout, "%c*rC", ESC);      /*end of graphics */
  plot_pcl_init (plotout, 90);
#endif

  la_clear ();
  return istat;
}

int la_frame7 (FILE * plotout)
{
  long iy;

  plot_pcl_init (plotout, 0);
  fprintf (plotout, "RASTER, DPI=%i, X=%li, Y=%li\n", dpi_, x_byte, y_byte);
  for (iy = 0; iy < y_byte; iy++)
    la_row7 (plotout, iy, &array[iy * x_byte]);
  plot_pcl_init (plotout, 99);
  la_clear ();
  return istat;
}

int la_line (long from_x, long from_y, long to_x, long to_y)
{
  long Dx, Dy;
  long curX, curY;
  long nC;
  long nT;
  long nA;
  long nR;
  long Xdir = 1;
  long Ydir = 1;
  int majorY = 0;

      /*******************************
       *  always run line in + dir   *
       *******************************/
  Dx = to_x - from_x;
  curX = from_x;
  if (Dx < 0) {
    Xdir = -1;
    Dx = labs (Dx);
  }

  Dy = to_y - from_y;
  curY = from_y;
  if (Dy < 0) {
    Ydir = -1;
    Dy = labs (Dy);
  }

      /***************************
       *  starting point of line *
       ***************************/
  la_block (curX, curY);

      /**************************
       *  determine major axis  *
       **************************/
  if (Dx < Dy) {
    nT = Dx;
    Dx = Dy;
    Dy = nT;
    majorY = 1;
  }

  nC = Dx;
  nT = nC;
  nA = nC / 2;
  nR = Dy;

  do {                                  /*  STEP 1  */
    nA = nA + nR;                       /*  STEP 2  */
    if (nA < nT) {                      /*  STEP 3  */
      if (majorY)                       /*  STEP 4  */
        curY = curY + Ydir;
      else
        curX = curX + Xdir;
    } else {
      nA = nA - nT;                     /*  STEP 5  */
      curY = curY + Ydir;               /*  STEP 6  */
      curX = curX + Xdir;
    }
    nC = nC - 1;                        /*  STEP 7  */
    la_block (curX, curY);
  } while (nC > 0);
  return istat;
}

int la_dot (long to_x, long to_y)
{
  la_block (to_x, to_y);
  return istat;
}
