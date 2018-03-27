 /*
  * rpws_archive.c   
  */
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include <math.h>
#include <ulimit.h>
#include <sys/types.h>
#include <sys/stat.h>

/* RPWS Stuff */
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>
#include <UTIL_status.h>
#include <hsk_micro.h>
#include <archive.h>

/* #include "mp_status.h"
#include "wbr_status.h"
#include "wfr_status.h"
#include "hfr_status.h"
#include "lp_status.h" 
#include "stim_status.h"
#include "dust_status.h" 
*/

#include "rpws_timing.h"
#include "rpws_label.h"
#include "rpws_browse.h"
#include "rpws_fsw_ver.h"

static char result[1024];

float eng3a_conv (unsigned char *count, int index)
{
  float result;
  float val;

  val = (float) count[index];
  switch (index) {
   default:
     result = -999;
     break;
   case 3:                             /* HFR Temp */
     /*
      * val = val * .01960784;/*
      */
     result = (119.3227) +
       (-1.5300595 * val) +
       (.009225 * val * val) + (-.00002561 * val * val * val);
     /**/ break;
   case 7:                             /* Antenna Deploy Motors */
   case 8:
   case 9:
     result = (128.556358) +
       (-1.76813034 * val) +
       (.0107905618 * val * val) + (-.0000261868956 * val * val * val);
     /**/ break;
   case 11:                            /* Search Coil */
     result = .780 * val - 98.5;
     break;
  }
  return result;
}

float eng3_conv (unsigned char *count, int index)
{
  float vlrp;
  float k = 5.00 / 255;
  float result;

  vlrp = (float) count[index] * k;
  switch (index) {
   default:
     result = vlrp;
     break;
   case 0:                             /*  HFR Current  */
     result = vlrp / 18.6 * 1000.;
     break;
   case 1:                             /*  ME02 Current  */
     result = vlrp / 30.4 * 1000.;
     break;
   case 2:                             /*  LP Current  */
     result = vlrp / 67. * 1000.;
     break;
   case 3:                             /*  ME01 Current  */
     result = vlrp / 9.75 * 1000.;
     break;
   case 4:                             /*  HFR +5  */
     result = vlrp * 1.22;
     break;
   case 5:                             /*  HFR +6  */
     result = vlrp * 1.48;
     break;
   case 6:                             /*  ME02 +12  */
     result = vlrp * 2.85;
     break;
   case 7:                             /*  ME02 +6  */
     result = vlrp * 1.48;
     break;
   case 8:                             /*  ME02 +5  */
     result = vlrp * 1.23;
     break;
   case 9:                             /*  LP +45  */
     result = vlrp * 11.1;
     break;
   case 10:                            /*  ME01 +12  */
     result = vlrp * 3.21;
     break;
   case 11:                            /*  ME01 +5  */
     result = vlrp * 1.22;
     break;
   case 12:                            /*  HFR -6  */
     result = (float) count[5] * k;
     result = vlrp * 3.2 - result * 3.245;
     break;
   case 13:                            /*  ME02 -12  */
     result = (float) count[6] * k;
     result = vlrp * 2.46 - result * 4.14;
     break;
   case 14:                            /*  ME02 -6  */
     result = (float) count[7] * k;
     result = vlrp * 3.2 - result * 3.25;
     break;
   case 15:                            /*  LP -45  */
     result = (float) count[9] * k;
     result = result * -13.14 + vlrp * 2.1;
     break;
  }
  return result;
}
char *power (struct CDS_buffer *buf)
{
  int count[16];
  int i;
  static char result[256];

  for (i = 0; i < 16; i++)
    sprintf (&result[i * 8], "%7.3f,",
             eng3_conv (buf->packet.rom_housekeeping.HFR_analog, i));
  result[16 * 8] = 0;
  return result;
}

char *analog (struct CDS_buffer *buf)
{
  int count[16];
  int i;
  static char result[256];

  result[0] = 0;
  sprintf (&result[0 * 8], "%7.3f,",
           eng3a_conv (buf->packet.rom_housekeeping.LRP_analog, 3));
  sprintf (&result[1 * 8], "%7.3f,",
           eng3a_conv (buf->packet.rom_housekeeping.LRP_analog, 7));
  sprintf (&result[2 * 8], "%7.3f,",
           eng3a_conv (buf->packet.rom_housekeeping.LRP_analog, 8));
  sprintf (&result[3 * 8], "%7.3f,",
           eng3a_conv (buf->packet.rom_housekeeping.LRP_analog, 9));
  sprintf (&result[4 * 8], "%7.3f,",
           eng3a_conv (buf->packet.rom_housekeeping.LRP_analog, 11));
  result[5 * 8] = 0;
  return result;
}

char *halt (struct CDS_buffer *buf)
{
  static char result[256];
  float hlt;

  hlt = buf->packet.rom_housekeeping.LRP_analog[15];
  sprintf (result, "%7.3f,", 100. - (hlt * 100.) / 255.);
  return result;
}
char *rpws_engineering (struct CDS_buffer *buf)
{
  static char result[1024];

  strcpy (result, power (buf));
  strcat (result, analog (buf));
  strcat (result, halt (buf));
  result[strlen (result) - 1] = 0;
  return result;
}
