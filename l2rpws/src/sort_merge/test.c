#include <math.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <values.h>

void main (int argc, char **argv)
{
  double a, b, c;

  a = 1e10;
  c = MINFLOAT * 10.;
  b = a + c;

  if (a == b)
    fprintf (stdout, "SHIT %e %e\n", c, MINFLOAT);
}
