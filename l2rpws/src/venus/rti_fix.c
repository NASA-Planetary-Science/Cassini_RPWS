
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

 /*
  *     This variable determines how the offset is applied
  */
#define fudge 1
#define delta_base 63
int threshold = 64;

void rti_update (void)
{
  struct DATA_RECORD *data_buffer;

  data_buffer = venus_dequeue (1, 1);
  while (data_buffer) {
    data_buffer->buffer->packet.mpp.mini_packet[2] =
      data_buffer->rti_temp & 0xFF;
    data_buffer->buffer->packet.mpp.mini_packet[3] =
      (data_buffer->rti_temp >> 8) & 0xFF;
    data_buffer = venus_dequeue (0, 1);
  }
  return;
}
void rti_offset (int off)
{
  struct DATA_RECORD *old_data_buffer;
  struct DATA_RECORD *data_buffer;
  int offset;

  offset = off;
  data_buffer = venus_dequeue (1, 1);
  while (data_buffer) {
    offset &= 0x07;
    data_buffer->rti_temp = data_buffer->rti_itime + offset;
    offset -= fudge;
    old_data_buffer = data_buffer;
    data_buffer = venus_dequeue (0, 1);
    if (data_buffer) {
      old_data_buffer->rti_delta =
        data_buffer->rti_itime - old_data_buffer->rti_itime;
      if (old_data_buffer->rti_delta < 0)
        old_data_buffer->rti_delta += 0x10000;
    }
  }
  return;
}
int rti_delta (void)
{
  struct DATA_RECORD *data_buffer;
  int count = 0;
  int rti_prior;
  int rti_now;
  int delta;

  data_buffer = venus_dequeue (1, 1);
  rti_prior = data_buffer->rti_temp;
  data_buffer = venus_dequeue (0, 1);
  while (data_buffer) {
    rti_now = data_buffer->rti_temp;
    delta = rti_now - rti_prior;
    if (delta != delta_base)
      count += 1;
    data_buffer = venus_dequeue (0, 1);
    rti_prior = rti_now;
  }
  return count;
}
int rti_fix (int flag)
{
  int i;
  int k;

  if (flag)
    rti_offset (0);
  else
    for (i = 0; i < 8; i += fudge) {
      rti_offset (i);                   /* load temp field with trial offset */
      k = rti_delta ();
      if (k < threshold)
        rti_update ();                  /* patch in the rti_temp field into the minipacket */
    }
}
