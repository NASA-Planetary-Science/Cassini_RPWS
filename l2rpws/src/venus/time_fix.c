
/*	mpb.c	*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include "fg.h"
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "utilmp.h"

 /*
  * buffer->packet.mpp.mini_packet /*
  */

int time_fix (struct MP_buffer *buffer, int base_time)
{
  int rti;
  int rti16_second;
  int cds16_second;
  int cds32_second;

  /*
   *      Reset the RTI number to be
   *       RTI's, not seconds...
   */
  rti16_second = (unsigned char) buffer->packet.mpp.mini_packet[2];
  rti16_second |= (unsigned char) buffer->packet.mpp.mini_packet[3] << 8;
  rti = rti16_second << 3;
  buffer->packet.mpp.mini_packet[2] = rti & 0xFF;
  buffer->packet.mpp.mini_packet[3] = (rti >> 8) & 0xFF;

  /*
   * fprintf(stderr," %8X rti16    %8X rti    %2.2X%2.2X\n",
   * rti16_second, rti,
   * buffer->packet.mpp.mini_packet[3],
   * buffer->packet.mpp.mini_packet[2]); /*
   */
  /*
   *      Patch up the CDS time field...  RTI field
   *      only covers a span of 2 hours (8100 seconds)
   *      but we have about 18 hour span embodied within
   *      the 16 bit RTI field that holds seconds, not RTI
   *
   *      Roll CDS time field back until lower bits match
   *       then replace it into the CDS time field...
   */
  if (base_time) {
    buffer->packet.cds_tag.begin[0] =
      (unsigned char) (base_time >> 24) & 0xFF;
    buffer->packet.cds_tag.begin[1] =
      (unsigned char) (base_time >> 16) & 0xFF;
    buffer->packet.cds_tag.begin[2] = (unsigned char) (base_time >> 8) & 0xFF;
    buffer->packet.cds_tag.begin[3] = (unsigned char) (base_time >> 0) & 0xFF;
  } else {
    cds16_second = (unsigned char) buffer->packet.cds_tag.begin[3] << 0;
    cds16_second |= (unsigned char) buffer->packet.cds_tag.begin[2] << 8;
    cds32_second = (unsigned char) buffer->packet.cds_tag.begin[1] << 0;
    cds32_second |= (unsigned char) buffer->packet.cds_tag.begin[0] << 8;

    if (rti16_second > cds16_second) {
      cds32_second -= 1;
    }

    buffer->packet.cds_tag.begin[0] = (cds32_second >> 8) & 0xFF;
    buffer->packet.cds_tag.begin[1] = (cds32_second >> 0) & 0xFF;
    buffer->packet.cds_tag.begin[2] = (rti16_second >> 8) & 0xFF;
    buffer->packet.cds_tag.begin[3] = (rti16_second >> 0) & 0xFF;
  }

  return rti & 0xFFFF;
}
