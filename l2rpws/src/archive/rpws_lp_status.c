
/*
 * hfr_status.c
 */
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>

#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>
#include <UTIL_status.h>
#include <archive.h>

#define _rpws_lp_status_
#include "rpws_lp_status.h"

static char *Version = { "V1.0" };

static struct LP_STATUS recent_lp_status = { 0 };


  /**********************************************************************
   *									*
   *	LP Sweep timetag timing and size:				*
   *									*
   **********************************************************************/

struct LP_STATUS *rpws_lp_status (struct ARCHIVE_TIME_SERIES *archive,
                                  struct RPWS_buffer *buffer)
{
  float period;
  float len;
  int div, fdiv;

  len = (get_status (buffer->packet.mpp.mini_packet,
                     LP_minipacket_length,
                     LP_minipacket_length_MSB) - 7) / 2.0;

  div = get_status (buffer->packet.mpp.mini_packet, LP_Clock, LP_Clock_MSB);
  fdiv = div & 0x3FFF;

  if (get_status (buffer->packet.mpp.mini_packet, LP_Clock_Mode, 0))
    period = fdiv * 125.;
  else
    period = fdiv * 0.008;

  switch (get_status (buffer->packet.mpp.mini_packet, LP_packet_type, 0)) {
   case LP_packet_RAW_DENSITY:
     recent_lp_status.lp_mode = LP_MODE_DENSITY;
     recent_lp_status.lp_event_clock = 0;
     recent_lp_status.lp_event_fine = 0;
     recent_lp_status.lp_event_duration = 0;
     break;
   case LP_packet_RAW_SWEEP:
     recent_lp_status.lp_mode = LP_MODE_SWEEP;
     recent_lp_status.Sweep_event_clock =
       (int) UTIL_extract_PKT_TIME ((struct MP_buffer *) buffer);
     recent_lp_status.Sweep_event_fine =
       (get_status (buffer->packet.mpp.mini_packet, LP_minipacket_RTI, 0) &
        0x03) << 5;
     recent_lp_status.Sweep_event_duration = period * (len - 1.0);

     recent_lp_status.lp_event_clock = recent_lp_status.Sweep_event_clock;
     recent_lp_status.lp_event_fine = recent_lp_status.Sweep_event_fine;
     recent_lp_status.lp_event_duration =
       recent_lp_status.Sweep_event_duration;

     break;
   default:
     recent_lp_status.lp_mode = LP_MODE_UNKNOWN;
     recent_lp_status.lp_event_clock = 0;
     recent_lp_status.lp_event_fine = 0;
     recent_lp_status.lp_event_duration = 0;
     break;
  }
  return &recent_lp_status;
}
