
/****************************************
 *	Deposit Printing ROutine	*
 ****************************************/
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "plot.h"

int main (int argc, char *argv[])
{
  float x, y;
  float x1, y1;
  time_t t;
  char ts[128];

  t = time (NULL);
  strcpy (ts, ctime (&t));
  p_setplt ("PS", ts, "ruler.ps", "Helvetica", 1);

  p_symbol (0.5, 0.5, .25, "ruler.c V1.0 MVG/3001B", 0.0, JUST_LEFT, "");
  for (x = 0; x < 11.1; x += 1.0)
    for (y = 0; y < 11.1; y += 1.0) {
      p_plot (x, y - .1, PEN_UP);
      p_plot (x, y + .1, PEN_DOWN);
      p_plot (x - .1, y, PEN_UP);
      p_plot (x + .1, y, PEN_DOWN);
      sprintf (ts, "x=%.1f y=%.1f", x, y);
      p_symbol (x + .12, y - .03, .0625, ts, 0.0, JUST_LEFT, "");
      for (x1 = .1; x1 < 1.0; x1 += .1)
        for (y1 = .1; y1 < 1.0; y1 += .1) {
          p_plot (x + x1, y + y1 - .02, PEN_UP);
          p_plot (x + x1, y + y1 + .02, PEN_DOWN);
          p_plot (x + x1 - .02, y + y1, PEN_UP);
          p_plot (x + x1 + .02, y + y1, PEN_DOWN);
        }

    }
  p_wrapup ();
  exit (0);
}
