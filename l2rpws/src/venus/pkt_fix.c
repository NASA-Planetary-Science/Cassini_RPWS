
/*	mpb.c	*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include "venus.h"
#include "fg.h"
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "utilmp.h"

extern char *sort;

 /*
  * buffer->packet.mpp.mini_packet /*
  */

FILE *debug;
int pkt_delta (struct DATA_RECORD *data_buffer)
{
  return data_buffer->rti_delta;
}
static int index = 0;
int pkt_disp (struct DATA_RECORD *data_buffer, char *text)
{
  int temp;

  if (!debug) {
    return;
    debug = fopen ("venus.debug", "w");
  }
  temp = pkt_delta (data_buffer);
  fprintf (debug, "%s %5d  %4d  %P->%P %6X %2.2X%2.2X%2.2X%2.2X\n",
           text,
           index++,
           temp,
           data_buffer,
           data_buffer->link,
           data_buffer->rti_itime,
           data_buffer->buffer->packet.cds_tag.begin[0],
           data_buffer->buffer->packet.cds_tag.begin[1],
           data_buffer->buffer->packet.cds_tag.begin[2],
           data_buffer->buffer->packet.cds_tag.begin[3]);
  fflush (debug);
  return temp;
}
int pkt_fix (void)
{
  struct DATA_RECORD *data_buffer;
  struct DATA_RECORD *temp;
  int delta;

  if (strcmp (sort, "xsort"))
    return 0;

  return;
}
