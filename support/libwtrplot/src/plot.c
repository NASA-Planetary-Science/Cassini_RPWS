
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

/************** PLOT.C ************/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "plotinit.h"
#include "util.h"
#include "p_plot.h"
#include "set.h"
#include "ps_plot.h"

FILE *plotout;
float set_date_size[3];
int page_count = 0;
static int P_plotter_selection = 1;

 /*
  *     plotter selection/dispatch
  */
static struct pltab
{
  int index;
  char *mnemonic;
} plt_tab[] = {
	{2, "HPGL"},                            /* HPGL pen         */
	{3, "PS"},                            /* Post Script      */
	{4, "PCL5"},                          /* PCL raster       */
	{5, "HPGL2"},                         /* HPGL laser       */
	{6, "PCL"},                           /* old PCL printers */
	{7, "RASTER"},                        /* old PCL printers */
	{8, "HPGLB"}, {99, "NULL"}, {0, ""}       /* list terminator  */
};

int p_factor (float val)
{
  switch (P_plotter_selection) {
   default:
     break;
   case 2:                             /* "HPGL" */
   case 5:
   case 8:
     return hp_factor (val);
   case 3:                             /* "PS"   */
     return ps_factor (val);
   case 4:                             /* "PCL"   */
   case 6:
   case 7:
     return pcl_factor (val);
  }
  return 0;
}
char *p_setplt (char *ver, char *date, char *fname, char *font, int orient)
{
  char *dev = NULL;
  int i = 0;

  page_count = strtol (util_envd ("PAGE_COUNT", "1"), NULL, 0);
  if (page_count < 0)
    page_count = 0;
  plot_delay (0);
  dev = getenv ("PLOTTER");
  if (!dev)
    dev = ver;
  while (plt_tab[i].index) {
    if (!util_strcmpi (dev, plt_tab[i].mnemonic)) {
      P_plotter_selection = plt_tab[i].index;
      break;
    }
    i = i + 1;
  }
  switch (P_plotter_selection) {
   case 2:                             /* "HPGL" */
     return hp_setplt (ver, date, fname);
   case 3:                             /* "PS"   */
     return ps_setplt (ver, date, fname, font, orient);
   case 4:                             /* "PCL"   */
     return pcl_setplt (ver, date, fname, 5);
   case 6:
     return pcl_setplt (ver, date, fname, 2);
   case 7:
     return pcl_setplt (ver, date, fname, 0);
   case 5:
     return hpgl2_setplt (ver, date, fname);
   case 8:
     return hpgl2b_setplt (ver, date, fname);
   case 99:
     return dev;
   default:
     set_warn ("PLOT", "p_setplt", " PLOTTER environment variable missing");
     exit (0);
  }
  return NULL;
}
int p_wrapup (void)
{
  switch (P_plotter_selection) {
   default:
     break;
   case 2:                             /* "HPGL" */
   case 5:
   case 8:
     return hp_wrapup ();
   case 3:                             /* "PS"   */
     return ps_wrapup ();
   case 4:                             /* "PCL"   */
     return pcl_wrapup ();
   case 6:
     return pcl_wrapup6 ();
   case 7:
     return pcl_wrapup7 ();
  }
  return 0;
}
int p_frame (void)
{
  switch (P_plotter_selection) {
   default:
     break;
   case 2:                             /* "HPGL" */
     return hp_frame ();
   case 3:                             /* "PS"   */
     return ps_frame ();
   case 4:                             /* "PCL"   */
     return pcl_frame ();
   case 6:
     return pcl_frame6 ();
   case 7:
     return pcl_frame7 ();
   case 5:
   case 8:
     return hpgl2_frame ();
  }
  return 0;
}
int p_plot (float x, float y, int ipen)
{
  switch (P_plotter_selection) {
   default:
     break;
   case 2:                             /* "HPGL" */
   case 5:
   case 8:
     return hp_plot (x, y, ipen);
   case 3:                             /* "PS"   */
     return ps_plot (x, y, ipen);
   case 4:                             /* "PCL"   */
   case 6:
   case 7:
     return pcl_plot (x, y, ipen);
  }
  return 0;
}
int p_zinc (int inten, int color)
{
  switch (P_plotter_selection) {
   default:
     break;
   case 2:                             /* "HPGL" */
   case 5:
   case 8:
     return hp_zinc (inten, color);
   case 3:                             /* "PS"   */
     return ps_zinc (inten, color);
   case 4:                             /* "PCL"   */
   case 6:
   case 7:
     break;
  }
  return 0;
}
int p_zrgb (int inten, float *color)
{
  switch (P_plotter_selection) {
   default:
     break;
   case 2:                             /* "HPGL" */
   case 5:
   case 8:
     break;
   case 3:                             /* "PS"   */
     return ps_zcolor (inten, color, 0);
   case 4:                             /* "PCL"   */
   case 6:
   case 7:
     break;
  }
  return 0;
}
int p_zcmyk (int inten, float *color)
{
  switch (P_plotter_selection) {
   default:
     break;
   case 2:                             /* "HPGL" */
   case 5:
   case 8:
     break;
   case 3:                             /* "PS"   */
     return ps_zcolor (inten, color, 1);
   case 4:                             /* "PCL"   */
   case 6:
   case 7:
     break;
  }
  return 0;
}
int p_zmark (char *text)
{
  switch (P_plotter_selection) {
   default:
     break;
   case 2:                             /* "HPGL" */
   case 5:
   case 8:
     break;
   case 3:                             /* "PS"   */
     return ps_zmark (text);
   case 4:                             /* "PCL"   */
   case 6:
   case 7:
     break;
  }
  return 0;
}
int p_symbol (float x,
              float y, float h, char *c, float theta, int just, char *font)
{
  if (c)
    switch (P_plotter_selection) {
     default:
       break;
     case 2:                           /* "HPGL" */
       return hp_symbol (x, y, h, c, theta, just);
     case 3:                           /* "PS"   */
       return ps_symbol (x, y, h, c, theta, just, font);
     case 4:                           /* "PCL"   */
       return pcl_symbol (x, y, h, c, theta, just);
     case 6:
     case 7:
       return pcl_symbol6 (x, y, h, c, theta, just);
     case 5:
     case 8:
       return hpgl2_symbol (x, y, h, c, theta, just);
    }
  return 0;
}
int p_macro (float x, float y, float h, char *c)
{
  switch (P_plotter_selection) {
   default:
     break;
   case 2:                             /* "HPGL" */
   case 5:
   case 8:
     return hp_macro (x, y, h, c);
   case 3:                             /* "PS"   */
     return ps_macro (x, y, h, c);
   case 4:                             /* "PCL"   */
   case 6:
   case 7:
     return pcl_macro (x, y, h, c);
  }
  return 0;
}
int p_comment (char *c)
{
  switch (P_plotter_selection) {
   default:
     break;
   case 2:                             /* "HPGL" */
   case 5:
   case 8:
     break;
   case 3:                             /* "PS"   */
     return ps_comment (c);
   case 4:                             /* "PCL"   */
   case 6:
   case 7:
     break;
  }
  return 0;
}
