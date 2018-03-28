
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

/***************** hp_plot.c **********/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __MSDOS__
#include "source\util.h"
#include "source\plotinit.h"
#else
#include "util.h"
#include "plotinit.h"
#endif

#define conversion 1016
#define HPGL_DT 3

extern float set_date_size[3];
static float hp_set_date_size[3] = { 5.00, 8.00, 0.24 };
static float hpgl2_set_date_size[3] = { 5.00, 7.75, 0.12 };
static char driver_name[] = { "HP_PLOT, HPGL driver" };
static char driver2_name[] = { "HP_PLOT, HPGL2 driver" };
static float offset_x = 0.0;
static float offset_y = 0.0;
static float current_x = 0.0;
static float current_y = 0.0;
static float current_s = 1.0;
static int old_x = -9999;
static int old_y = +9999;
static int old_pen = 99;
static char *typeface_family = { "4101" };
static int plotting = 0;
static int newline = 0;
static int B_flag = 0;

extern FILE *plotout;
extern int orient[3];

int hp_factor (float val)
{
  current_s = val;
  return plotting;
}
float hpf_scale (float val, int loc)
{
  float q;

  q = val * current_s;

  switch (loc) {
   case 1:
     current_x = q;
     return q + offset_x;
   case 2:
     current_y = q;
     return q + offset_y;
   default:
     return q;
  }
}
int hpi_scale (float val, int loc)
{
  return (int) (hpf_scale (val, loc) * conversion);
}
char *hp_setplt (char *ver, char *date, char *fname)
{
  int i;

  for (i = 0; i < 3; i++)
    set_date_size[i] = hp_set_date_size[i];

  if (plotting == 2)
    fclose (plotout);
  if (fname == NULL) {
    plotout = stdout;
    plotting = 1;
  } else {
    plotout = plot_fopen (fname, TEXT);
    if (plotout != NULL)
      plotting = 2;
  }
  if (plotting) {
    if (B_flag)
      fprintf (plotout, "%%1A"
               "E"
               "&l6A"
               "&l1O"
               "*c11520x7680Y"
               "*p0x-140Y" "*c0T" "%%1B" "WU0;" "PW0.25;" "SP1;\n");
    newline = plot_init (plotout, 0);
  }
  old_pen = PEN_DOWN;
  return driver_name;
}
char *hpgl2_setplt (char *ver, char *date, char *fname)
{
  int i;
  char *name;

  name = util_env ("PLT_FONT");
  if (name)
    typeface_family = name;
  name = hp_setplt (ver, date, fname);
  for (i = 0; i < 3; i++)
    set_date_size[i] = hpgl2_set_date_size[i];
  if (name)
    return driver2_name;
  return name;
}
char *hpgl2b_setplt (char *ver, char *date, char *fname)
{
  B_flag = 1;
  return hpgl2_setplt (ver, date, fname);
}
int hp_wrapup (void)
{
  if (plotting) {
    if (!newline)
      fputs ("\n", plotout);
    fputs ("PU;PG;\n", plotout);
    if (B_flag)
      fprintf (plotout, "%%1A" "E" "&l2A" "&l0O" "\n");
    plot_clear (plotout, 0);
    newline = 1;
  }
  if (plotting == 2)
    fclose (plotout);
  plotting = 0;
  plotout = NULL;
  old_pen = PEN_UP;
  return plotting;
}
int hp_frame (void)
{
  if (plotting) {
    if (!newline)
      fputc ('\n', plotout);
    plot_page (plotout, 0);
    fputs ("PG;\n", plotout);
    plot_page (plotout, 99);
    newline = 1;
  }
  return plotting;
}
int hpgl2_frame (void)
{
  if (plotting) {
    if (!newline)
      fputc ('\n', plotout);
    plot_page (plotout, 0);
    fputc ('\014', plotout);
    plot_page (plotout, 99);
    newline = 1;
  }
  return plotting;
}

int hpi_newline (int jcnt, int increment)
{
  int itmp;

  itmp = jcnt + increment;
  if (itmp < 15)
    return itmp;
  fputs ("\n", plotout);
  newline = 1;
  return 0;
}

int hp_zinc (int inten, int color)
{
  static int icnt = 0;
  char temp[32];

  if (color)
    sprintf (temp, "SP%d;", color);
  sprintf (temp, "PW%4.2f;", inten / 300.);
  fputs (temp, plotout);
  icnt = hpi_newline (icnt, 1);
  return 1;
}

int hp_plot (float x, float y, int ipen)
{
  int new_x, new_y;
  static int icnt = 0;

  if (newline)
    icnt = 0;
  newline = 0;

  new_x = hpi_scale (x, 1);
  new_y = hpi_scale (y, 2);

  if (plotting) {
    if ((new_x == old_x) & (new_y == old_y)) {
      if (labs (ipen) == PEN_DOT) {
        fputs ("PD;PU;", plotout);
        old_pen = PEN_UP;
      }
    } else {
      old_x = new_x;
      old_y = new_y;
      if (ipen != old_pen) {
        if (labs (ipen) == PEN_DOWN) {
          fputs ("PD;", plotout);
          old_pen = PEN_DOWN;
          icnt = hpi_newline (icnt, 1);
        }
        if ((labs (ipen) == PEN_UP) | (labs (ipen) == PEN_DOT)) {
          fputs ("PU;", plotout);
          old_pen = PEN_UP;
          icnt = hpi_newline (icnt, 1);
        }
      }
      fprintf (plotout, "PA%d,%d;", new_x, new_y);
      icnt = hpi_newline (icnt, 3);

      if (labs (ipen) == 4) {
        fputs ("PD;PU;", plotout);
        old_pen = PEN_UP;
        icnt = hpi_newline (icnt, 2);
      }
    }
  }

  if (ipen < 0) {
    offset_x = hpf_scale (x, 1);
    offset_y = hpf_scale (y, 1);
  }
  return plotting;
}

#define HW_ratio .7
#define CM_inch 2.54
int hp_symbol (float x, float y, float h, char *c, float theta, int just)
{
  static float old_h = -999.0;
  static int old_just = 999;
  int justification;

  if (plotting) {
    if (!newline) {
      fputc ('\n', plotout);
      newline = 1;
    }
    if (old_h != h)
      fprintf (plotout, "SI%f,%f; ",
               hpf_scale (h, 0) * CM_inch * HW_ratio,
               hpf_scale (h, 0) * CM_inch);
    old_h = h;
    if (just != -99) {
      if (old_just != just) {
        justification = 4;
        if (just == 1)
          justification = 7;
        if (just == -1)
          justification = 1;
        fprintf (plotout, "LO%d; ", justification);
      }
      old_just = just;

      if (old_pen == PEN_DOWN)
        fputs ("PU;", plotout);
      fprintf (plotout, "PA%d,%d;", hpi_scale (x, 1), hpi_scale (y, 2));
    }
    fprintf (plotout, "LB%s%c\n", c, HPGL_DT);
    old_y = -9999;
    old_x = -9999;
    old_pen = PEN_UP;
    newline = 1;
  }
  return plotting;
}
int hp_macro (float x, float y, float h, char *name)
{
  return 0;
}

int hpgl2_symbol (float x, float y, float h, char *c, float theta, int just)
{
  static float old_h = -999.0;
  static int old_just = 999;
  int justification;

  if (plotting) {
    if (!newline) {
      fputc ('\n', plotout);
      newline = 1;
    }
    if (old_h != h) {
      int stroke = 0;

      if (h > 0.25)
        stroke = 3;
      if (abs (just) & 0x02)
        stroke = 3;
      fprintf (plotout, "SD2,1,4,%f,6,%d,7,%s; ", hpf_scale (h, 0) * 108.0,     /* height */
               stroke, typeface_family);
    }
    old_h = h;
    if (just != -99) {
      if (old_just != just) {
        justification = 4;
        if (just == 1)
          justification = 7;
        if (just == -1)
          justification = 1;
        fprintf (plotout, "LO%d; ", justification);
      }
      old_just = just;

      if (old_pen == PEN_DOWN)
        fputs ("PU;", plotout);
      fprintf (plotout, "PA%d,%d;", hpi_scale (x, 1), hpi_scale (y, 2));
    }
    fprintf (plotout, "LB%s%c\n", c, HPGL_DT);
    old_y = -9999;
    old_x = -9999;
    old_pen = PEN_UP;
    newline = 1;
  }
  return plotting;
}
