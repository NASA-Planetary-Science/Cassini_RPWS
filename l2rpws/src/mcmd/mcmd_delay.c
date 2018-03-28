#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/time.h>

int delay_period = 250;

void mcmd_delay (int msec)
{
  struct timeval timeout[3];

  timeout[1].tv_usec = (1000 * msec % 1000000);
  timeout[1].tv_sec = msec / 1000;
  if (msec)
    select (0, NULL, NULL, NULL, &timeout[1]);
  return;
}

int mcmd_set_delay (int d)
{
  int i;

  i = delay_period;
  delay_period = d;
  return i;
}
