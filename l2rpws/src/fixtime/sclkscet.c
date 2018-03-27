#include <stdlib.h>
#include <stdio.h>
#include "rtiu.h"
#include "utilo.h"
#pragma HP_ALIGN NOPADDING

long chdo_sclkscet (struct CDS_buffer *buffer)
{
  static long scet_epoch = 378691200;
  static long scet_delta = -1;
  static int first = 1;
  long scet_seconds;

  if (first) {
    first = 0;
    scet_delta = utilo_int ("SCET_DELTA");
  }

  scet_seconds = buffer->packet.chdo_tag.scet.days * 86400;
  scet_seconds = scet_seconds +
    (buffer->packet.chdo_tag.scet.milliseconds / 1000);
  scet_seconds += 86400;

  return scet_seconds
    - buffer->packet.chdo_tag.sclk.seconds - scet_epoch + scet_delta;
}
