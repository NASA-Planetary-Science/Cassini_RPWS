#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <errno.h>

#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "utilo.h"
#include "utilt.h"

main (int argc, char *argv[])
{
  struct tm *pkt_tm;
  struct event_time scet;
  float days;
  int old;
  time_t pkt_time;
  int i;

  fprintf (stdout, "first ");
  fflush (stdout);
  pkt_time = time (NULL);
  for (i = 0; i < 10; i++) {
    pkt_tm = gmtime (&pkt_time);
    fprintf (stdout, "%d ", pkt_tm->tm_year + 1900);
    fflush (stdout);
    fprintf (stdout, "%d ", pkt_tm->tm_yday + 1);
    fflush (stdout);
    fprintf (stdout, "\n");
    pkt_time -= 86400;
  }

  days = 365;
  old = days;
  scet.milliseconds = 0;
  for (i = 0; i < 60; i++) {
    scet.days = days + 0.5;
    pkt_tm = UTIL_event_scet_tm (scet, 0);
    fprintf (stdout, "%10.2f  ", days);
    fprintf (stdout, "%4d  ", (int) days - old);
    fprintf (stdout, "%04d-", pkt_tm->tm_year + 1900);
    fprintf (stdout, "%03dT", pkt_tm->tm_yday + 1);
    fprintf (stdout, "%02d:", pkt_tm->tm_hour);
    fprintf (stdout, "%02d:", pkt_tm->tm_min);
    fprintf (stdout, "%02d ", pkt_tm->tm_sec);
    fprintf (stdout, "\n");
    old = days;
    days += 365.25;
    scet.milliseconds += 2000;
  }
}
