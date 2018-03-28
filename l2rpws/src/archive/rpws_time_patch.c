#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <SpiceUsr.h>

#include <das2/das1.h>

#include "rpws_label.h"

static char *Version = { "V1.1" };

 /*
  *     Build start/stop times on nice boundary
  */
static char *rpws_time_patch_1 (char *scet, int mod, int offset)
{
  static char result[128];
  int year, month, mday, yday, hour, minute;
  double second;
  double rtime;

  parsetime (scet, &year, &month, &mday, &yday, &hour, &minute, &second);
  hour = hour - (hour % mod);
  minute = 0;
  second = 0.0;
  rtime = ttime (&year, &month, &mday, &yday, &hour, &minute, &second);
  rtime += offset * 3600;
  emitt (rtime, &year, &month, &mday, &yday, &hour, &minute, &second);
  sprintf (result, "%04d-%03dT%02d:%02d:%06.3f",
           year, yday, hour, minute, second);
  return result;
}

 /*
  *     Rebuild sclk
  */
static char *rpws_time_patch_2 (char *rtime)
{
  static char result[32];
  double et;

  utc2et_c(rtime, &et);
  sprintf(result, "%.3f", et);
  return result;
}
static char *rpws_time_patch_3 (char *rtime)
{
  static char result[32];               /* 1:1234567890:000 */
  static int SpaceCraft_ID = -82;
  double et;

  utc2et_c(rtime, &et);
  sce2s_c(SpaceCraft_ID, et, 20, result);
  result[16] = 0;
  *strchr (result, '.') = ':';          /* change to to colon */
  return result;
}
int rpws_time_patch (struct RPWS_LABEL *label)
{
  double st;
  double sp;

  switch (label->instrument) {
   default:
     return 0;
   case RPWS_LABEL_RAW:
   case RPWS_LABEL_WBR:
     strcpy (label->scet_start, rpws_time_patch_1 (label->scet_start, 1, 0));
     strcpy (label->scet_stop, rpws_time_patch_1 (label->scet_start, 1, 1));
     break;
   case RPWS_LABEL_HSK:
   case RPWS_LABEL_DUST:
   case RPWS_LABEL_BFDL:
   case RPWS_LABEL_IPC:
   case RPWS_LABEL_WFR:
     strcpy (label->scet_start, rpws_time_patch_1 (label->scet_start, 24, 0));
     strcpy (label->scet_stop, rpws_time_patch_1 (label->scet_start, 24, 24));
     break;
  }
  strcpy (label->ephem_start, rpws_time_patch_2 (label->scet_start));
  strcpy (label->ephem_stop, rpws_time_patch_2 (label->scet_stop));
  strcpy (label->sclk_start, rpws_time_patch_3 (label->scet_start));
  strcpy (label->sclk_stop, rpws_time_patch_3 (label->scet_stop));
  return 1;
}
