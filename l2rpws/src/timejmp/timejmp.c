
/*	timejmp.c	*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include "fg.h"
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "utilmp.h"

static char *Version = { "V0.0" };
struct CDS_HEADER
{
  int id_CCSDS;
  int sequence;
  int length;
  int time[3];
} *cds_header;
FILE *input = stdin;
FILE *output = stdout;
FILE *debug_file = NULL;

#define MASK 0xFFFFFF00
#define STEP 0x00000100
int fixbuffer_CDS (struct CDS_buffer *c_buffer)
{
  int time;
  int new_time;
  int rti;
  int delta;
  int flag = 0;

  static int regression;
  static int previous_time;
  static int previous_sequence;

  UTIL_extract_CDS (c_buffer, (long *) cds_header);
  cds_header->sequence &= 0x3FFF;
  time = (cds_header->time[0] & 0x00FF) << 24;
  time |= (cds_header->time[1] & 0xFFFF) << 8;
  time |= (cds_header->time[2] & 0xFF00) >> 8;
  rti = (cds_header->time[2] & 0x00FF) >> 5;
  delta = cds_header->sequence - previous_sequence;
  if (delta < 0)
    delta += 16384;

  if (regression) {
    if ((time & MASK) == regression) {
      new_time = time + STEP;
      flag = 1;
    } else {
      regression = 0;
    }
  } else {
    if ((previous_time > time) && (delta < 32)) {
      regression = time & MASK;
      new_time = time + STEP;
      flag = 1;
      if (debug_file)
        fprintf (debug_file,
                 "Time regression %6d(%d) %08X %08X.%X\n",
                 cds_header->sequence, delta, previous_time, time, rti);
    }
  }
  if (flag) {
    c_buffer->packet.cds_header.time_3 = (new_time >> 24) & 0x00FF;
    c_buffer->packet.cds_header.time_2 = (new_time >> 16) & 0x00FF;
    c_buffer->packet.cds_header.time_1 = (new_time >> 8) & 0x00FF;
    c_buffer->packet.cds_header.time_0 = (new_time >> 0) & 0x00FF;
    if (debug_file)
      fprintf (debug_file,
               "Time fixup      %6d(%d) %08X %08X.%X\n",
               cds_header->sequence, delta, time, new_time, rti);
  }

  if (debug_file)
    if (time < 0x40000000)
      fprintf (debug_file,
               "Time error      %6d(%d) %08X %08X.%X\n",
               cds_header->sequence, delta, time, new_time, rti);


  previous_time = time;
  previous_sequence = cds_header->sequence;
  return 0;
}

int getbuffer_CDS (struct CDS_buffer *c_buffer, FILE * file, int flag,
                   int debug)
{
  int ilen;

  ilen = UTIL_getbuffer_CDS (c_buffer, file, flag);
  return ilen;
}

int putbuffer_CDS (struct CDS_buffer *c_buffer, FILE * file, int debug)
{
  int ilen;

  if (file)
    ilen = UTIL_putbuffer_CDS (c_buffer, file);
  /**/ return ilen;
}

int main (int argc, char *argv[])
{
  struct CDS_buffer *c_buffer;
  int eof_flag = 0;
  int dbg_flag = 0;
  int ilen;

  c_buffer = malloc (65536);
  cds_header = malloc (256);
  fg_flags (argc, argv);

  if (fg_flag ("help") || fg_flag ("h")) {
    printf (" \n");
    printf (" \n");
    printf (" timejump %s %s - mini packet formatter\n", Version);
    printf (" \n");
    printf ("      -help\n");
    printf ("      -debug    Prints status for modified records\n");
    printf ("      -test     Suppress output\n");
    printf (" \n");
    exit (0);
  }

  if (fg_flag ("test"))
    output = NULL;

  if (fg_flag ("debug"))
    debug_file = stderr;

  ilen = getbuffer_CDS (c_buffer, input, eof_flag, dbg_flag);
  while (ilen > 0) {
    if (UTIL_extract_HRS (c_buffer))
      fixbuffer_CDS (c_buffer);
    ilen = putbuffer_CDS (c_buffer, output, dbg_flag);
    ilen = getbuffer_CDS (c_buffer, input, eof_flag, dbg_flag);
  }
  fprintf (stderr, "\n");
  return 1;
}
