
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

#ifdef __MSDOS__
#include <dos.h>
#include "source\util.h"
#include "source\set.h"
#else
#include "util.h"
#include "set.h"
#endif
static int newline = 0;
static int i;
static char env[12];
static char *init;

FILE *plot_fopen (const char *filename, int mode_flag)
{
  static FILE *plotout;
  static char mode[2][8] = { "w+",      /*  plain TEXT */
    "wb+"
  };                                    /* BINARY files */
  plotout = fopen (filename, mode[mode_flag]);
  rewind (plotout);
  return plotout;
}
int plot_setup (FILE * plotout, char *key, int dir)
{
  int start = 0;
  int end = 100;
  int step = 1;

  switch (dir) {
   default:
     break;
   case 10:
   case 20:
   case 30:
   case 40:
   case 50:
   case 60:
   case 70:
   case 80:
   case 90:
     start = dir;
     end = dir + 10;
     break;
   case 99:
     start = 99;
     end = 50;
     step = -1;
     break;
  }
  for (i = start; i != end; i += step) {
    sprintf (env, "%s%2.2d\n", key, i);
    init = util_env (env);
    if (init) {

#ifdef DBG
      fprintf (stderr, "%s <=> %s\n", env, util_stg (init));
#endif

      fputs (util_stg (init), plotout);
      newline = 1;
    } else {

#ifdef DBG
      fprintf (stderr, "%s\n", env);
#endif

      break;
    }
  }
  return newline;
}
int plot_delay (int flag)
{
  static int plt_dly = 0;
  char *pdc;

  switch (flag) {
   case 0:
     pdc = util_envd ("PLT_DLY", "0");
     plt_dly = strtol (pdc, NULL, 0);
     if (plt_dly) {
       char ermsg[32];

       sprintf (ermsg, "Delay set to %i", plt_dly);
       set_information ("PLOTINIT", "plot_delay", ermsg);
     }
     break;
   case 1:

#ifdef __MSDOS__
     if (plt_dly)
       delay (plt_dly);
#endif

     break;
  }
  return plt_dly;
}
int plot_init (FILE * plotout, int start)
{
  return plot_setup (plotout, "PLT_INI", start);
}
int plot_pcl_init (FILE * plotout, int start)
{
  return plot_setup (plotout, "PLT_PCL", start);
}
int plot_clear (FILE * plotout, int start)
{
  return plot_setup (plotout, "PLT_CLR", start);
}
int plot_page (FILE * plotout, int start)
{
  return plot_setup (plotout, "PLT_PGE", start);
}
