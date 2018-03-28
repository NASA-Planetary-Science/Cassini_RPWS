
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

#include "wtrplot.h"

char *color[] = {
  "p_NONE",
  "p_RED",
  "p_GREEN",
  "p_BLUE",
  "p_CYAN",
  "p_MAGENTA",
  "p_YELLOW",
  "p_BLACK",
  "pm_BLACK",
  "pm_RED",
  "pm_GREEN",
  "pm_BLUE",
  "pm_CYAN",
  "pm_MAGENTA",
  "pm_YELLOW",
  "pf_BLACK",
  "pff_BLACK",
  "pf_RED",
  "pf_GREEN",
  "pf_BLUE",
  "pf_CYAN",
  "pf_MAGENTA",
  "pf_YELLOW",
  "pf_BLACK",
  "p_ORANGE",
  "p_PURPLE",
  "pf_",
  "n27",
  "n28",
  "n29",
  "n30",
  "n31",
  "n32"
};

int main (int argc, char *argv[])
{
  int i;
  float y;
  char temp[128];

  p_setplt ("PS", "", "output.ps", "", 0);
  p_plot (0.0, 0.0, PEN_UP);
  p_plot (1.0, 0.0, PEN_DOWN);
  p_plot (1.0, 1.0, PEN_DOWN);
  p_plot (0.0, 1.0, PEN_DOWN);
  p_plot (0.0, 0.0, PEN_DOWN);
  for (i = 0; i < 32; i++) {
    sprintf (temp, "Color Index %2d is %s", i, color[i]);
    printf ("%s\n", temp);
    y = i * 0.15;
    p_zinc (50, i);
    p_symbol (5.0, y, 0.100, temp, 0.0, JUST_CENTER, "");
  }

  p_wrapup ();
}
