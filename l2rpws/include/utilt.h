
/*
 *      UTILT.H  Time routine definitions
 */

#ifndef _TIME_INCLUDED
#include <time.h>
#endif

#ifndef _rtiu_h
#include <rtiu.h>
#endif

#ifndef _utilt_h
#define _utilt_h
extern void UTIL_align_on_second (double, double);

    /*
     *  2nd argument:
     *          value  0  indicates use raw time
     *          value -1  indicates use adjusted time
     *          all others are added to the raw time.
     */
extern time_t UTIL_event_time (struct MP_buffer *, int);

    /*
     *  Similar to event_time, bu get the time of a
     *  micro packet event.  This can get really ugly
     *  if micropackets aren't flowing.  Usually we'll
     *  have DUST sending data about 1/minute, so it's
     *  not a problem (and we're really looking for
     *  good times with dust data).
     *
     *  1st. argumnent is CDS buffer 'cause that's
     *          where we'll find dust housekeeping.
     *  2nd. argument is the dust packet number, assumes
     *          that the're all 16 bytes long.  If not,
     *          simply move it to position 0...
     *  3rd. argument is epoch arg, like above...
     */
extern time_t UTIL_micro_time (struct CDS_buffer *buf,  /* CDS record data buffer */
                               int index,       /* packet offset to micro packet */
                               int epoch);      /* offset to UNIX time */

    /*
     *  this uses the unix "tm" structure  BUT BUT BUT BUT BUT BUT
     *          year is expressed fully (i.e. 1998, 1999, 2000)
     *            and should be Y2K compliant.
     *          day-of-year, day-of-week, and month count from 1 (not Zero)
     *
     *  2nd argument:
     *          value  0  indicates use raw time
     *          value -1  indicates use adjusted time
     *          all others are added to the raw time.
     *
     *      user_tm -> int tm_sec;      / * seconds after the minute - [0,61] 
     *      user_tm -> int tm_min;      / * minutes after the hour - [0,59] 
     *      user_tm -> int tm_hour;     / * hours - [0,23] 
     *      user_tm -> int tm_mday;     / * day of month - [1,31] 
     *      user_tm -> int tm_mon;      / * month of year - [1,12]
     *      user_tm -> int tm_year;     / * years [1998 is 1998, etc.]
     *      user_tm -> int tm_wday;     / * days since sunday - [1,7]
     *      user_tm -> int tm_yday;     / * days since January 1 - [1,366]
     *      user_tm -> int tm_isdst;    / * daylight savings time flag 
     *
     */
extern struct tm *UTIL_event_time_tm (struct MP_buffer *, int);

   /*
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
    * * New time stuff  * * * * * * * * * * * * * * * * * * * * * * * * *
    * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

    /*
     *          returns the "scet" of an event-of-interest.  Pass in the
     *            mini-packet buffer and the sclk in JPL format.
     */
extern struct event_time *UTIL_event_scet (struct MP_buffer *,
                                           struct event_clock);

    /*
     *          Use the result of UTIL_event_scet to build a tm structure
     *          NOTE that the "event_time" structure has milliseconds-of-day
     *            so mod with 86400 to obtain mSec
     *          2nd. argument is added to tm.wday, tm,mon and tm.yday to allow these
     *            to start from 1 is used for display purposes (rather than as an
     *            index...)
     */
extern struct tm *UTIL_event_scet_tm (struct event_time, int);
#endif
