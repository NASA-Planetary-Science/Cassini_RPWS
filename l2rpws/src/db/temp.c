#include <stdio.h>
float vout (float r1, float r2)
{
  float Vout;
  float Vref = 1.25;
  float Iadj = 50e-6;

  Vout = (Vref * (1.0 + (r2 / r1))) + (Iadj * r2);
  return Vout;
}
int main (int argc, char *argv[])
{
  float r1 = 121.0;
  float r2;
  float r2a;
  float Vout;

  fprintf (stdout, "         ");
  for (r2a = 0.0; r2a < 10.0; r2a += 1.0)
    fprintf (stdout, "+%5.2f ", r2a);
  fprintf (stdout, "\n");
  for (r2 = 10; r2 < 400.; r2 += 10.0) {
    fprintf (stdout, "R2 %5.1f  ", r2);
    for (r2a = 0.0; r2a < 10.0; r2a += 1.0) {
      Vout = vout (r1, r2 + r2a);
      fprintf (stdout, "%5.2f  ", Vout);
    }
    fprintf (stdout, "\n");
  }
}
