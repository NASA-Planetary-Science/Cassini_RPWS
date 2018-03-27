#include <stdlib.h>
#include <stdio.h>
#include "venus.h"
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "fg.h"

char *sort = { "vsort" };
char *comment = { "sorted by RTI tag" };
static struct DATA_RECORD *head = NULL;

struct DATA_RECORD *venus_alloc (struct MP_buffer *in_buffer,
                                 struct MP_buffer *mro_buffer, int valid)
{
  struct DATA_RECORD *temp = NULL;
  temp = (struct DATA_RECORD *) malloc (sizeof (struct DATA_RECORD));
  if (temp) {
    temp->link = NULL;
    temp->buffer = (struct MP_buffer *) malloc (in_buffer->f_length + 4);
    if (temp->buffer) {
      memcpy (temp->buffer, in_buffer, in_buffer->f_length + 4);
      temp->rti_itime = 0;
      temp->rti_temp = 0;
      temp->valid_flag = valid;

      temp->memory.pattern =
        (unsigned char) mro_buffer->packet.mpp.mini_packet[4] << 8;
      temp->memory.pattern |=
        (unsigned char) mro_buffer->packet.mpp.mini_packet[5];
      temp->memory.address =
        (unsigned char) mro_buffer->packet.mpp.mini_packet[4] << 8;
      temp->memory.address |=
        (unsigned char) mro_buffer->packet.mpp.mini_packet[5] & 0x80;
      temp->memory.bank =
        ((unsigned char) mro_buffer->packet.mpp.mini_packet[5] & 0x10) >> 4;
      temp->memory.processor =
        ((unsigned char) mro_buffer->packet.mpp.mini_packet[5] & 0x0E) >> 1;

    }
  }
  /*
   * fprintf(stderr,"%P = venus_alloc(%P)\n",
   * temp, in_buffer); /*
   */
  return temp;
}
void venus_array_dump (void)
{
  return;
}
int venus_mem_dump (struct DATA_RECORD *temp, FILE * file)
{
  char *proc[] = { "",                  /* 0 */
    "LRP",                              /* 1 */
    "DCP",                              /* 2 */
    "LRP/DCP",                          /* 3 */
    "HRP",                              /* 4 */
    "HRP/LRP",                          /* 5 */
    "HRP/DCP",                          /* 6 */
    "HRP/DCP/LRP"
  };                                    /* 7 */
  static int index = 0;

  if (temp->memory.processor == 2) {
    temp->memory.bank = 0;
    temp->memory.pattern &= 0xFFEF;
  }
  fprintf (file, "           NULL, NULL, 0x%4X, 0x%4X, %d, %d, 0, 0, 0, 0,\n",
           temp->memory.pattern,
           temp->memory.address, temp->memory.bank, temp->memory.processor);
}
static int ven_time (struct MP_buffer *in_buffer)
{
  int cds_time;

  cds_time = (unsigned char) in_buffer->packet.cds_tag.begin[3] << 0;
  cds_time |= (unsigned char) in_buffer->packet.cds_tag.begin[2] << 8;
  cds_time |= (unsigned char) in_buffer->packet.cds_tag.begin[1] << 16;
  cds_time |= (unsigned char) in_buffer->packet.cds_tag.begin[0] << 24;
  return cds_time;
}

 /*
  * fprintf(stderr,"\n",
  * ); /*
  */
int venus_enqueue (struct MP_buffer *buf, struct MP_buffer *mro, int itime)
{
  struct DATA_RECORD *next = NULL;
  struct DATA_RECORD *temp = NULL;
  int cds_time, cds_time_temp;
  int status;
  static int i = 0;

  cds_time = ven_time (buf);
  temp = venus_alloc (buf, mro, 1);
  temp->rti_itime = itime;

  if (!head) {                          /* first elkement in list */
    head = temp;
    temp->link = NULL;
    return 1;
  }

  cds_time_temp = ven_time (head->buffer);
  if (cds_time < cds_time_temp) {       /* element belongs at begining */
    temp->link = head;
    head = temp;
    return 2;
  }

  if (!head->link) {                    /* element belongs at end of shot list */
    temp->link = NULL;
    head->link = temp;
    return 3;
  }

  next = head;
  while (next->link) {
    cds_time_temp = ven_time (next->link->buffer);
    if (cds_time < cds_time_temp) {
      temp->link = next->link;
      next->link = temp;
      return 4;
    }
    next = next->link;
  }
  next->link = temp;
  temp->link = NULL;
  return 5;
}

#define SPECIAL_LENGTH 256+32+256
struct DATA_RECORD *venus_insert (struct DATA_RECORD *buf, int delta_time)
{
  struct DATA_RECORD *temp;
  int itime;
  static int i = 0;

  /*
   *         Allocate another buffer and copy the
   *           previous record into the new...
   *         We're only interested in the time,
   *           so it's not important that we duplicate
   *           data as we won't pass this on to the user...
   */
  temp = (struct DATA_RECORD *) malloc (sizeof (struct DATA_RECORD));
  if (!temp)
    return NULL;
  temp->buffer = (struct MP_buffer *) malloc (SPECIAL_LENGTH);
  if (!temp->buffer)
    return NULL;
  memcpy (temp->buffer, buf->buffer, SPECIAL_LENGTH);

  /*
   *         Insert into the list
   */
  temp->link = buf->link;
  buf->link = temp;

  /*
   *         Patch up the time (seconds)
   */
  itime = (unsigned char) buf->buffer->packet.cds_tag.begin[0] << 24;
  itime |= (unsigned char) buf->buffer->packet.cds_tag.begin[1] << 16;
  itime |= (unsigned char) buf->buffer->packet.cds_tag.begin[2] << 8;
  itime |= (unsigned char) buf->buffer->packet.cds_tag.begin[3] << 0;
  itime += delta_time / 8 + 1;
  temp->buffer->packet.cds_tag.begin[0] = (itime >> 24) & 0xFF;
  temp->buffer->packet.cds_tag.begin[1] = (itime >> 16) & 0xFF;
  temp->buffer->packet.cds_tag.begin[2] = (itime >> 8) & 0xFF;
  temp->buffer->packet.cds_tag.begin[3] = (itime >> 0) & 0xFF;

  /*
   *         Patch up the time (RTI)
   */
  itime = (buf->buffer->packet.mpp.mini_packet[0] & 0x0F) << 8;
  itime |= buf->buffer->packet.mpp.mini_packet[1];
  itime += delta_time + 1;
  temp->buffer->packet.mpp.mini_packet[0] =
    buf->buffer->packet.mpp.mini_packet[0] & 0x00F0;
  temp->buffer->packet.mpp.mini_packet[0] |= (itime & 0x0F00) >> 8;
  temp->buffer->packet.mpp.mini_packet[1] = (itime & 0x00FF) >> 0;

  temp->rti_itime = buf->rti_itime + delta_time;
  temp->rti_delta = buf->rti_delta - delta_time;
  buf->rti_delta = delta_time;

  return temp;
}
static struct DATA_RECORD *venus_deq (const int first)
{
  static struct DATA_RECORD *temp = NULL;
  static struct DATA_RECORD *next = NULL;

  if (first) {
    next = head;
  }
  temp = next;
  next = next->link;
  return temp;
}
struct DATA_RECORD *venus_dequeue (const int first, int flag)
{
  struct DATA_RECORD *temp;
  static int i = 0;
  int valid = 0;

  while (!valid) {
    temp = venus_deq (first);
    if (!temp)
      break;
    if (flag)
      break;
    valid = temp->valid_flag;
  }
  return temp;
}
int venus_discard (const int delta)
{
  struct DATA_RECORD *temp;
  int i;
  static int ix;

  i = delta;
  temp = venus_deq (1);
  while (temp) {
    i -= 1;
    if (!i) {
      if (temp->link)
        temp->link = temp->link->link;
      i = delta;
    }
    temp = venus_deq (0);
  }
  return 0;
}
