#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <rtiu.h>
#include <UTIL_status.h>

#define DS_WFR_MAX 10.240
#define DS_WBR_MAX  0.200

char *rpws_timing_version = { "V1.0" };

double sample_wbr_sub (struct RPWS_buffer *buffer)
{
  int type;
  double offset = 0.0;

  type = get_status (buffer->packet.mpp.mini_packet, MP_packet_ID, 0);
  if (type != MP_packet_ID_WBR)
    return 0.0;
  if (!get_status
      (buffer->packet.mpp.mini_packet, WFR_More_Status_Follows, 0))
    return 0.0;

  return fabs (offset);
}
double sample_wbr_wfr (struct RPWS_buffer *buffer)
{
  int type;
  char *text = "UNK";
  double sample_rate_wfr[] = { 10.E-3, 140.E-6 };
  double sample_rate_wbr[] = { 36.E-6, 4.5E-6 };
  double len;
  double ds_time;
  double sr;

  type = get_status (buffer->packet.mpp.mini_packet, MP_packet_ID, 0);

  len = buffer->packet.index.data_length;

  if (get_status (buffer->packet.mpp.mini_packet, WFR_More_Status_Follows, 0))
    len -= 7.0;                         /* 10 status bytes */
  else                                  /* -- OR -- */
    len -= 5.0;                         /*  8 status bytes */

  switch (type) {
   case MP_packet_ID_WFR:
     len /= 2.0;                        /* samples */
     break;
   case MP_packet_ID_WBR:
     break;
   default:
     len = 0;
     break;
  }

  switch (type) {
   case MP_packet_ID_WFR:
     sr =
       sample_rate_wfr[get_status
                       (buffer->packet.mpp.mini_packet, WFR_frequency_band,
                        0)];
     break;
   case MP_packet_ID_WBR:
     sr =
       sample_rate_wbr[get_status
                       (buffer->packet.mpp.mini_packet, WBR_frequency_band,
                        0)];
     break;
   default:
     sr = 0.0;
     break;
  }

  ds_time = (len - 1.0) * sr;

  switch (type) {
   case MP_packet_ID_WFR:
     if (ds_time > DS_WFR_MAX)
       ds_time = DS_WFR_MAX;
     break;
   case MP_packet_ID_WBR:
     if (ds_time > DS_WBR_MAX)
       ds_time = DS_WBR_MAX;
     break;
  }

  switch (type) {
   case MP_packet_ID_WFR:
     text = "WFR";
     break;
   case MP_packet_ID_WBR:
     text = "WBR";
     break;
   case MP_packet_ID_HFR:
     text = "HFR";
     break;
   case MP_packet_ID_MFR:
     text = "MFR";
     break;
   case MP_packet_ID_LFDR:
     text = "LFD";
     break;
   case MP_packet_ID_LP:
     text = "L/P";
     break;
   case MP_packet_ID_STIM:
     text = "STM";
     break;
  }
  if (0) {
    fprintf (stdout, "%s ", __FILE__);
    fprintf (stdout, "%s ", text);
    fprintf (stdout,
             "len %5.0f/%d  ",
             len, get_status (buffer->packet.mpp.mini_packet,
                              MP_packet_size_LSB, MP_packet_size_MSB)
      );
    fprintf (stdout, "sample rate %7.3f ", sr * 1000);
    fprintf (stdout, "ds_time %.3f ", ds_time);
    if (type == MP_packet_ID_WFR) {
      fprintf (stdout,
               "%2d/",
               get_status (buffer->packet.mpp.mini_packet,
                           WFR_minipacket_segment, 0));
      fprintf (stdout,
               "%2d",
               get_status (buffer->packet.mpp.mini_packet,
                           WFR_minipacket_size, 0));
    /**/}
    fprintf (stdout, "\n");
    fflush (stdout);
  }

  return fabs (ds_time);
}
