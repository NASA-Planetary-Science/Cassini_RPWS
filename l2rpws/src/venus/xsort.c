#include <stdlib.h>
#include <stdio.h>
#include "venus.h"
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "fg.h"
#include "xvenus.h"

char *sort = { "xsort" };
char *comment = { "sorted using MRO address information" };
struct DATA_RECORD *venus_alloc (struct MP_buffer *in_buffer,
                                 struct MP_buffer *mro_buffer, int valid)
{
  int i = 0;
  int pattern;

  pattern = (unsigned char) mro_buffer->packet.mpp.mini_packet[4] << 8;
  pattern |= (unsigned char) mro_buffer->packet.mpp.mini_packet[5];
  if (pattern & 0x0004)                 /* DCP ?? */
    pattern &= 0xFFEF;                  /* Yes, strip bank select bit */
  while (1) {
    if (!array[i].memory.pattern)
      return NULL;
    if (pattern == array[i].memory.pattern)
      break;
    i++;
  }
  if (array[i].buffer) {
    fprintf (stderr, "alloc duplicate record\n");
    return NULL;
  }
  array[i].buffer = (struct MP_buffer *) malloc (in_buffer->f_length + 4);
  if (array[i].buffer) {
    memcpy (array[i].buffer, in_buffer, in_buffer->f_length + 4);
    array[i].rti_itime = 0;
    array[i].rti_temp = 0;
    array[i].valid_flag = valid;
  } else
    fprintf (stderr, "alloc failured\n");
  return &array[i];
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

  fprintf (file, " %5d %4X    %s Bank %1d Addr %4X \n",
           index++,
           temp->memory.pattern,
           proc[temp->memory.processor],
           temp->memory.bank, temp->memory.address);
}
void venus_array_dump (void)
{
  int i;
  int icnt = 0;

  for (i = 0; i < 1024; i++)
    if (!array[i].valid_flag) {
      fprintf (stderr, "invalid record %5d %5d ", i, icnt++);
      venus_mem_dump (&array[i], stderr);
    }
  return;
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

  temp = venus_alloc (buf, mro, 1);
  if (!temp)
    return 0;
  temp->rti_itime = itime;
  return 5;
}

struct DATA_RECORD *venus_insert (struct DATA_RECORD *buf, int delta_time)
{
  return NULL;
}
static struct DATA_RECORD *venus_deq (const int first)
{
  static int index = 0;
  int invalid = 1;

  if (first)
    index = 0;
  if (index < 1024)
    return &array[index++];
  else
    return NULL;
}
struct DATA_RECORD *venus_dequeue (const int first, int flag)
{
  struct DATA_RECORD *temp;
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
  return 0;
}
