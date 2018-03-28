
/* UTIL_event_time - RPWS time extraction routines */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include "rtiu.h"
#include "util.h"
 /*
  *     The MAX_DELTA value determines how long we can expect
  *     the HRP to stall with HRS data.  HRP applies time-tag
  *     to a CDS packet at the begining of formatting, so the
  *     data product can be produced after the time tag (as well 
  *     as before).
  *       Remember that LRS data time-tag is ALWAYS applied after
  *     the CDS record is formatted (i.e. after the data has been
  *     collected, formatted, and delivered to BIU handler.
  */
#define MAX_DELTA 64                    /*The MAX_DELTA value determines how long we can expect the HRP to stall with HRS data */

 /*
  * Extract instrument event time
  * * Data from the spacecraft is delivered with enough information to
  * * recover an event time for the attached dataset.  This time indicates
  * * the time at which data acquisition began.  Accuracy is limited by timing
  * * resulution within the instrument (usually good to better than 10mSec)
  * * and by the accuracy achieved at JPL when generating a SCLK/SCET relationship
  * * that is delivered with the data record.
  * *
  * * returns UNIX time_t structure containing the decoded instrument event time
  */
time_t UTIL_event_time (struct MP_buffer *buf,  /* data buffer */
                        int epoch)
{                                       /* offset to UNIX time */
  int pkt_time;
  int mp_time;
  int delta;
  int l_epoch;

  pkt_time = UTIL_extract_PKT_TIME (buf);
  mp_time = (buf->packet.mpp.mini_packet[2]) |
    (buf->packet.mpp.mini_packet[3] << 8);

  switch (UTIL_extract_MP_type (buf)) {
   default:
     mp_time = (mp_time >> 3) & 0x1FFF; /* 13 bit second in mP */
     delta = mp_time - (pkt_time & 0x00001FFF); /* 32 bit SCLK */

     /*
      * OK, nominally 'mp - SCLK' is negative
      * if it's positive, then there is a roll
      * involved and we need to adjust the delta
      * we using by 8192 (13 bit roll value)
      * UNLESS it's tiny.. Then it is probably
      * from HRS stream which is a little imprecice
      * so compare against a few seconds...
      */
     if (delta > MAX_DELTA)
       delta = delta - 0x2000;
     break;
  }
  switch (epoch) {
   case 0:
     l_epoch = 0;
     break;
   case -1:
     l_epoch = (buf->packet.cds_tag.epoch[0] << 24) |
       (buf->packet.cds_tag.epoch[1] << 16) |
       (buf->packet.cds_tag.epoch[2] << 8) |
       (buf->packet.cds_tag.epoch[3] << 0);

     break;
   default:
     l_epoch = epoch;
     break;
  }
  return pkt_time + delta + l_epoch;
}

 /*
  * Extract micro packet event time
  * * Data from the spacecraft is delivered with enough information to
  * * recover an event time for the attached dataset.  This time indicates
  * * the time at which data acquisition began.  Accuracy is limited by timing
  * * resulution within the instrument (usually good to better than 10mSec)
  * * and by the accuracy achieved at JPL when generating a SCLK/SCET relationship
  * * that is delivered with the data record.
  * *
  * * returns UNIX time_t structure containing the decoded instrument event time
  */
time_t UTIL_micro_time (struct CDS_buffer * buf,        /* CDS record data buffer */
                        int index,      /* packet offset to micro packet */
                        int epoch)
{                                       /* offset to UNIX time */
  int cds_time;
  int up_time;
  int delta;
  int l_epoch;

  cds_time = UTIL_extract_TIME (buf);   /* data buffer */
  up_time = (buf->packet.housekeeping.micro_packet[index][2]) |
    (buf->packet.housekeeping.micro_packet[index][3] << 8);

  up_time = (up_time >> 3) & 0x1FFF;    /* 13 bit second in mP */
  delta = up_time - (cds_time & 0x00001FFF);    /* 32 bit SCLK */
  /*
   * OK, nominally 'mp - SCLK' is negative
   * if it's positive, then there is a roll
   * involved and we need to adjust the delta
   * we using by 8192 (13 bit roll value)
   * UNLESS it's tiny.. Then it is probably
   * from HRS stream which is a little imprecice
   * so compare against a few seconds...
   */
  if (delta > MAX_DELTA)
    delta = delta - 0x2000;

  switch (epoch) {
   case 0:
     l_epoch = 0;
     break;
   case -1:
     l_epoch = (buf->packet.cds_tag.epoch[0] << 24) |
       (buf->packet.cds_tag.epoch[1] << 16) |
       (buf->packet.cds_tag.epoch[2] << 8) |
       (buf->packet.cds_tag.epoch[3] << 0);

     break;
   default:
     l_epoch = epoch;
     break;
  }
  return cds_time + delta + l_epoch;
}

 /*
  * Extract instrument event time
  * * Data from the spacecraft is delivered with enough information to
  * * recover an event time for the attached dataset.  This time indicates
  * * the time at which data acquisition began.  Accuracy is limited by timing
  * * resulution within the instrument (usually good to better than 10mSec)
  * * and by the accuracy achieved at JPL when generating a SCLK/SCET relationship
  * * that is delivered with the data record.
  * *
  * * returns UNIX time_t structure containing the decoded instrument event time
  */
time_t UTIL_event_time_2 (struct MP_buffer * buf /* data buffer */ ,
                          int epoch)
{                                       /* offset to UNIX time */
  int pkt_time;
  int mp_time;
  int delta;
  int l_epoch;

  pkt_time = UTIL_extract_PKT_TIME (buf);
  mp_time = (buf->packet.mpp.mini_packet[2]) |
    (buf->packet.mpp.mini_packet[3] << 8);

  switch (UTIL_extract_MP_type (buf)) {
   default:
     mp_time = (mp_time >> 3) & 0x1FFF; /* 13 bit second in mP */
     delta = mp_time - (pkt_time & 0x00001FFF); /* 32 bit SCLK */

     /*
      * OK, nominally 'mp - SCLK' is negative
      * if it's positive, then there is a roll
      * involved and we need to adjust the delta
      * we using by 8192 (13 bit roll value)
      * UNLESS it's tiny.. Then it is probably
      * from HRS stream which is a little imprecice
      * so compare against a few seconds...
      */
     if (delta > MAX_DELTA)
       delta = delta - 0x2000;
     break;
  }
  switch (epoch) {
   case 0:
     l_epoch = 0;
     break;
   case -1:
     l_epoch = (buf->packet.cds_tag.epoch[0] << 24) |
       (buf->packet.cds_tag.epoch[1] << 16) |
       (buf->packet.cds_tag.epoch[2] << 8) |
       (buf->packet.cds_tag.epoch[3] << 0);

     break;
   default:
     l_epoch = epoch;
     break;
  }
  return pkt_time + delta + l_epoch;
}

        /*
         *      Do the time thing, but adjust the 
         *        YEAR to show all digits (Y2K etc.)
         *        and the MONTH, DAY-OF-WEEK, and DAY-OF-YEAR so 
         *        they start from one !!!
         *
         *      We're also mantaining 8 copies, in case someone
         *        does something stupid (this will keep everyone
         *        wondering how the hell it works when they do
         *        something kind-of dumb...)
         */

 /*
  * Get instrument event as a tm structure
  * * This call builds an instrument event time in a tm structure
  * * that is accurate to 1 second (i.e. has no sub-second field).
  * * Usefukl for GSE dispalys where precice timeing is not too
  * * much of an issue.
  * *returns string containing time
  */
struct tm *UTIL_event_time_tm (struct MP_buffer *buf /* data buffer */ ,
                               int epoch)
{                                       /* offset to UNIX time */
  static struct tm temp[8];
  static int index = 0;
  struct tm *result;
  time_t timer;

  index += 1;
  index &= 0x07;
  timer = UTIL_event_time (buf, epoch);
  result = gmtime (&timer);

  temp[index].tm_sec = result->tm_sec;
  temp[index].tm_min = result->tm_min;
  temp[index].tm_hour = result->tm_hour;
  temp[index].tm_mday = result->tm_mday;
  temp[index].tm_isdst = result->tm_isdst;

  temp[index].tm_wday = result->tm_wday + 1;
  temp[index].tm_mon = result->tm_mon + 1;
  temp[index].tm_yday = result->tm_yday + 1;

  temp[index].tm_year = result->tm_year + 1900;
  if (temp[index].tm_year < 1970)
    temp[index].tm_year += 100;
  return &temp[index];
}


  /*************************************************************************
   ***** NEW TIME SHIT *****************************************************
   *************************************************************************/

 /*
  * returns event time structure with instrument event time
  * Event time structure contains a days field and a milliseconds field.
  * This routine returns a reasonable accurate instrument
  */
struct event_time *UTIL_event_scet (struct MP_buffer *buf /* data buffer */ ,
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
   * was ** delta_sclk.fine =  (sclk.fine>>5) - (buf->packet.chdo_tag.sclk.fine>>5); /* 
   */


    /*---------------------------------------------------------
     *  Adjust for direction (subtract a second when delta-fine
     *		is negative)
     */
  /*
   * was ** if(delta_sclk.fine&0xFFF8) /* 
   */
  if (delta_sclk.fine & 0xFF00) {       /* using 8 bit fine now */
    delta_sclk.seconds -= 1;            /* fine delta is negative */
    /*
     * was ** delta_sclk.fine    += 8;     /* make fine delta positive 
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
   * (delta_sclk.seconds * 1000); /* delta mS CHDO:MP 
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

static int UTIL_mon (int Doy, int Feb, int *Month, int *Day)
{                                       /*  Jan Feb Mar Apr May Jun Jly Aug Sep Oct Nov Dec */
  /*
   * 31  28  31  30  31  30  31  31  30  31  30  31 
   */
  static int _month[2][13] =
    { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365,
    0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366
  };
  int index = 0;

  while (Doy >= _month[Feb][index])
    index++;
  index--;
  *Month = index;
  *Day = Doy - _month[Feb][index] + 1;
  return 0;
}
static int UTIL_yrdy1958 (int *Year, int *Day, int Dy58)
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
    18993, 19358, 19723, 20089,         /* 2008 */
    20454, 20819, 21184, 21550,         /* 2012 */
    21915, 22280, 22645, 23011,         /* 2016 */
    23376, 23741, 24106, 24472,         /* 2020 */
    24837, 25202, 25567, 25933,         /* 2024 */
    26298, 26663, 27028, 27394          /* 2028 */
  };
  int index = 0;

  while (Dy58 >= days_past[index])
    index++;
  index--;
  *Year = index + 58;
  *Day = Dy58 - days_past[index];
  return leap_[index % 4];
}

        /********************************
	 *	     January 1958	*
	 *	 S  M Tu  W Th  F  S	*
	 *	          1  2  3  4	*
	 *	 5  6  7  8  9 10 11	*
	 *	12 13 14 15 16 17 18	*
	 *	19 20 21 22 23 24 25	*
	 *	26 27 28 29 30 31	*
	 ********************************/
 /*
  *returns tm struct containing instrument event time
  */
struct tm *UTIL_event_scet_tm (struct event_time scet,  /* instrument event time */
                               int start_point)
{                                       /* day start from 0 or 1 */
  static struct tm temp[8];
  static int index = 0;
  int seconds;
  int feb;

  index += 1;                           /* Keep a copy in case user is keeping  */
  index &= 0x07;                        /* pointers (and not copying data)      */

  seconds = scet.milliseconds / 1000;   /* seconds of day */

  temp[index].tm_sec = seconds % 60;
  temp[index].tm_min = (seconds / 60) % 60;
  temp[index].tm_hour = seconds / 3600;
  temp[index].tm_wday = ((int) scet.days + 3) % 7;
  temp[index].tm_isdst = 0;

  feb = UTIL_yrdy1958 (&temp[index].tm_year, &temp[index].tm_yday, scet.days);

  UTIL_mon (temp[index].tm_yday,
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
