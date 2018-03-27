#include <stdio.h>
#include <stdlib.h>
#include "mcmd.h"

void main (void)
{
  char in[1024];
  int count;
  double delay;
  int i = 0;
  char *buf;

  buf = mcmd_gpib_help (i++);
  while (buf[0]) {
    /*
     * puts(buf); /*
     */
    buf = mcmd_gpib_help (i++);
  }
  while (*gets (in)) {
    delay = mcmd_dly (in);
    fprintf (stderr, " %6f = mcmd_dly(%s)\n", delay, in);
    count = mcmd_main (in);
    fprintf (stderr, " %6d = mcmd_main(%s)\n", count, in);
  }
}
