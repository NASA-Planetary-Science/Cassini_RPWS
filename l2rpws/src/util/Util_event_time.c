
/* Util_event_time - RPWS time extraction routines */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

/* Cassini Stuff */
#include <rtiu.h>
#include <util.h>
#include <Util.h>                       /* Wow, varies just by captilization, dude really? */


static int Util_mon (int Doy, int Feb, int *Month, int *Day)
{                                       /*  Jan Feb Mar Apr May Jun Jly Aug Sep Oct Nov Dec */
  /*
   * 31  28  31  30  31  30  31  31  30  31  30  31 
   */
  static int _month[2][13] =
    { {0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365} ,
      {0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366}
  };
  int index = 0;

  while (Doy >= _month[Feb][index])
    index++;
  index--;
  *Month = index;
  *Day = Doy - _month[Feb][index] + 1;
  return 0;
}
static int Util_yrdy1958 (int *Year, int *Day, int Dy58)
{
  static int leap_[] = { 0, 0, 1, 0 };
  static int days_past[] = { 0, 365, 730, 1096, /* 1958 */
    1461, 1826, 2191, 2557,             /* 1962 */
    2922, 3287, 3652, 4018,             /* 1966 */
    4383, 4748, 5113, 5479,             /* 1970 */
    5844, 6209, 6574, 6940,             /* 1974 */
    7305, 7670, 8035, 8401,             /* 1978 */
    8766, 9131, 9496, 9862,             /* 1982 */
    10227, 10592, 10957, 11323,         /* 1986 */
    11688, 12053, 12418, 12784,         /* 1990 */
    13149, 13514, 13879, 14245,         /* 1994 */
    14610, 14975, 15340, 15706,         /* 1998 */
    16071, 16436, 16801, 17167,         /* 2002 */
    17532, 17897, 18262, 18628,         /* 2004 */
    18993, 19358, 19732, 20089,         /* 2008 */
    20454, 20819, 21184, 21550,         /* 2012 */
    21915, 22280, 22645, 23011,         /* 2016 */
    23376, 23741, 24106, 24472,         /* 2020 */
    24837, 25202, 25567, 25933          /* 2024 */
  };
  int index = 0;

  while (Dy58 >= days_past[index])
    index++;
  index--;
  *Year = index + 58;
  *Day = Dy58 - days_past[index];
  return leap_[index % 4];
}

struct tm *Util_event_scet_tm (struct event_time scet,  /* instrument event time */
                               int start_point)
{                                       /* day start from 0 or 1 */
  static struct tm temp[8];
  static int index = 0;
  int seconds;
  int feb;

  index += 1;                           /* Keep a copy in case user is keeping  */
  index &= 0x07;                        /* pointers (and not copying data)      */

  seconds = Util_32 (scet.milliseconds) / 1000; /* seconds of day */

  temp[index].tm_sec = seconds % 60;
  temp[index].tm_min = (seconds / 60) % 60;
  temp[index].tm_hour = seconds / 3600;
  temp[index].tm_wday = (Util_16 (scet.days) + 3) % 7;
  temp[index].tm_isdst = 0;

  feb = Util_yrdy1958 (&temp[index].tm_year,
                       &temp[index].tm_yday, Util_16 (scet.days));

  Util_mon (temp[index].tm_yday,
            feb, &temp[index].tm_mon, &temp[index].tm_mday);
  /*
   *      Adjust from 0..n-1 to 1..n
   */
  if (start_point) {
    temp[index].tm_yday += 1;
    temp[index].tm_wday += 1;
  }
  return &temp[index];
}

 /*
  * returns event time structure with instrument event time
  * Event time structure contains a days field and a milliseconds field.
  * This routine returns a reasonable accurate instrument
  */
struct event_time *Util_event_scet (struct MP_buffer *buf /* data buffer */ ,
                                    struct event_clock sclk)
{                                       /* spacecraft clock */
  static struct event_time new_scet[16];
  static int index = 0;
  struct event_clock delta_sclk;
  long msec;
  int flag = 1;

  index += 1;
  index &= 0x0F;
  if (!flag)
    fprintf (stdout, "\nUTIL_event_scet =================== %d \n", index);
  if (!flag)
    fprintf (stdout, "  chdo_sclk %8X.%X chdo_scet %d,%d \n",
             buf->packet.chdo_tag.sclk.seconds,
             buf->packet.chdo_tag.sclk.fine >> 5,
             buf->packet.chdo_tag.scet.days,
             buf->packet.chdo_tag.scet.milliseconds);

    /*--------------------------------------------------------------
     *  Build difference between CHDO time and time-of-interest (MP)
     *
     *	We'll use all of "fine" because CHDO time is referenced to
     *	 this precision.  RPWS puts 'false' information in the lower
     *	 5 bits of "fine" so CHDO SCET is referenced to a non-RTI
     *	 boundary.
     *	Note that we must strip the lower bits of the RPWS
     *	 time as it is meaningless
     */
  delta_sclk.seconds = sclk.seconds - buf->packet.chdo_tag.sclk.seconds;
  delta_sclk.fine = (sclk.fine & 0x00E0) - (buf->packet.chdo_tag.sclk.fine);

  /*
   * was ** delta_sclk.fine =  (sclk.fine>>5) - (buf->packet.chdo_tag.sclk.fine>>5);  
   */


    /*---------------------------------------------------------
     *  Adjust for direction (subtract a second when delta-fine
     *		is negative)
     */
  /*
   * was ** if(delta_sclk.fine&0xFFF8) 
   */
  if (delta_sclk.fine & 0xFF00) {       /* using 8 bit fine now */
    delta_sclk.seconds -= 1;            /* fine delta is negative */
    /*
     * was ** delta_sclk.fine    += 8;     make fine delta positive 
     */
    delta_sclk.fine += 256;             /* 8 bit number now */
  }

  if (!flag)
    fprintf (stdout, "  delta_sclk %08X.%02X \n",
             delta_sclk.seconds, delta_sclk.fine);
  /*
   *  Adjust SCET in CHDO
   */
  /*
   * was ** msec = (delta_sclk.fine * 125) + 
   * (delta_sclk.seconds * 1000);   // delta mS CHDO:MP 
   */
  msec = (int) ((float) delta_sclk.fine * 3.90625) +
    (delta_sclk.seconds * 1000);

  if (!flag)
    fprintf (stdout, "  msec %6d \n", msec);

  new_scet[index].days = buf->packet.chdo_tag.scet.days;
  new_scet[index].milliseconds =
    buf->packet.chdo_tag.scet.milliseconds + msec;

  if (!flag)
    fprintf (stdout, "  new_scet %d,%d\n",
             new_scet[index].days, new_scet[index].milliseconds);

  /*
   * Since we're looking at direction (sign) we have to
   *         change the cast of the millisecond field of
   *         it always looks positive...
   ********
   ******** 12/22/98 updates
   ********    When performing one of the (almost continuous) OVT's
   ********    we noticed that SCLK:SCET was several days different
   ********    from SCLK in the RPWS/CDS record.  Makes times a pain
   ********    as this is a multi-day roll.  Changed single day test
   ********    to multiday test by changing "if" to "while"
   */
  while ((signed) new_scet[index].milliseconds >= 86400 * 1000) {
    new_scet[index].milliseconds -= (86400 * 1000);
    new_scet[index].days += 1;
    if (!flag)
      fprintf (stdout, "    new_scet days+1\n");
  }
  while ((signed) new_scet[index].milliseconds < 0) {
    new_scet[index].milliseconds += (86400 * 1000);
    new_scet[index].days -= 1;
    if (!flag)
      fprintf (stdout, "    new_scet days-1\n");
  }
  return &new_scet[index];

     /********      ********      ********      ********      ********      ********/
}
