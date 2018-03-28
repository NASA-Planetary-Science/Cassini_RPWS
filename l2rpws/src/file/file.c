
/*
 * file.c
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include "fg.h"
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "utilo.h"
#include "utilt.h"

static int K60 = 60;
static int Kstyle = 0;

static char *title = { " CASSINI rpws_file (1 hour split) 0.1" };
struct MP_buffer *buffer;


static void time_dump (struct tm *pkt_tm, char *text)
{
  fprintf (stderr,
           " %s Time  %4d %3d %2.2d:%2.2d:%2.2d\n",
           text,
           pkt_tm->tm_year + 1900,
           pkt_tm->tm_yday, pkt_tm->tm_hour, pkt_tm->tm_min, pkt_tm->tm_sec);
}

static char *new_file_name (FILE * output, struct MP_buffer *buffer)
{
  time_t hdr_time, cds_time, mp_time;
  time_t evt_time;
  time_t *_time = &hdr_time;

  struct tm *pkt_tm;
  int epoch;
  int record_type;
  int minute_of_day;
  char *text = { " CDS Header" };
  char *cds_text = { " CDS Record" };
  char *mp_text = { "  MP Record" };
  char *mpu_text = { "MPUS Record" };
  char type = 'a';
  static char filename[128];

  epoch = (buffer->packet.cds_tag.epoch[0] << 24) |
    (buffer->packet.cds_tag.epoch[1] << 16) |
    (buffer->packet.cds_tag.epoch[2] << 8) |
    (buffer->packet.cds_tag.epoch[3] << 0);
  hdr_time = (buffer->packet.cds_tag.begin[0] << 24) |
    (buffer->packet.cds_tag.begin[1] << 16) |
    (buffer->packet.cds_tag.begin[2] << 8) |
    (buffer->packet.cds_tag.begin[3] << 0);


  cds_time = UTIL_extract_TIME (buffer);
  mp_time = UTIL_extract_PKT_TIME (buffer);
  evt_time = UTIL_event_time (buffer, epoch);

  hdr_time += epoch;
  cds_time += epoch;
  mp_time += epoch;

  record_type = buffer->record_type;
  switch (record_type & 0xF00) {
   case DATA_telemetry:
     _time = &cds_time;
     text = cds_text;
     type = 'r';
     break;
   case DATA_MP_packet:
   case DATA_MP_segment:
   case DATA_MP_large_segment:
     _time = &evt_time;
     text = mp_text;
     type = 'm';
     break;
   case DATA_MP_complete_segment:
     _time = &evt_time;
     text = mpu_text;
     type = 'u';
     break;
  }

  pkt_tm = gmtime (_time);
  minute_of_day = pkt_tm->tm_hour * 60 + pkt_tm->tm_min;
  minute_of_day = minute_of_day / K60;
  minute_of_day = minute_of_day * K60;
  switch (Kstyle) {
   case 0:
     sprintf (filename, "t%04d%03d%02d%02d.%c00",
              pkt_tm->tm_year + 1900,
              pkt_tm->tm_yday + 1,
              minute_of_day / 60, minute_of_day % 60, type);
     break;
   case 1:
     sprintf (filename, "t%04d%03d%04d.%c00",
              pkt_tm->tm_year + 1900,
              pkt_tm->tm_yday + 1, minute_of_day, type);
     break;
  }

/*     time_dump(pkt_tm, filename); /**/
  return filename;
}

main (int argc, char *argv[])
{
  FILE *input = stdin;
  FILE *output = NULL;
  /**/ int write_flag = 1;
  int ilen;
  int icnt, jcnt;
  char fname[128];
  char old_name[128] = { "" };
  char *new_name = { "" };

  int eof_flag = UTIL_GET_NON_BLOCKING;

  buffer = malloc (32768 + 1024);
  fg_flags (argc, argv);
  if (fg_flag ("help") || fg_flag ("h")) {
    fprintf (stdout, "%s   HELP SCREEN\n", title);
    fprintf (stdout,
             "  This utility accepts RAW data, UNsegmented mini-packets\n");
    fprintf (stdout,
             "    or segmented mini-packets and produces output files\n");
    fprintf (stdout,
             "    based on the time of the data.  This is intended to\n");
    fprintf (stdout,
             "    allow the data query to be performed on a long time\n");
    fprintf (stdout,
             "    period while allowing data to be stored in manageable\n");
    fprintf (stdout, "    files.\n");
    fprintf (stdout, "  In the case of mini-packet data, there will be no\n");
    fprintf (stdout, "    packet loss at the begining of each file.  In\n");
    fprintf (stdout,
             "    addition, each minipacket file will be time-aligned\n");
    fprintf (stdout,
             "    (i.e. the data in the file is placed in the file\n");
    fprintf (stdout,
             "    based on the time in the minipacket, not the CDS \n");
    fprintf (stdout,
             "    timestamp which is normally after the data timestamp\n");
    fprintf (stdout,
             "  Note that the first housekeeping record followinga reset\n");
    fprintf (stdout,
             "    or software load usually has zero in the SCLK field.\n");
    fprintf (stdout, "    this record will be placed into a filed called \n");
    fprintf (stdout,
             "    't19580000.r00'.  The record contains all zeros.\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "    -size mm  file size in minutes\n");
    fprintf (stdout, "                  default is %d\n", K60);
    fprintf (stdout, "                selection is %d\n",
             fg_int ("size", K60));
    fprintf (stdout, "    -style n  file name style\n");
    fprintf (stdout, "                  0 tYYYYDDDHHMM\n");
    fprintf (stdout, "                  1 tYYYYDDDMMMM\n");
    fprintf (stdout, "    -test     test run, generate filenames only\n");
    exit (0);
  }

  K60 = fg_int ("size", K60);
  Kstyle = fg_int ("style", Kstyle);
  if (fg_flag ("test"))
    write_flag = 0;

  if (fg_flag ("getmp") == '+') {
    strcpy (fname, UTIL_filename (FILE_MP, FILE_LOCATE_DEFAULT));
    fprintf (stderr, "file source: %s\n", fname);
    if (!fname)
      exit (0);
    input = fopen (fname, "rb");
  }
  if (fg_flag ("getcds") == '+') {
    strcpy (fname, UTIL_filename (FILE_RAW, FILE_LOCATE_DEFAULT));
    fprintf (stderr, "file source: %s\n", fname);
    if (!fname)
      exit (0);
    input = fopen (fname, "rb");
  }
  ilen = UTIL_getbuffer_MP (buffer, input, eof_flag);
  while (ilen > 0) {
    icnt += 1;
    jcnt += 1;
    new_name = new_file_name (output, buffer);
    if (new_name[0])
      if (strcmp (old_name, new_name)) {
        strcpy (old_name, new_name);
        if (output) {
          fclose (output);
        }
        fprintf (stderr, "  %d records \n", jcnt);
        /**/ if (write_flag)
          output = fopen (new_name, "ab");
        fprintf (stderr, "file:%s", new_name);
        /**/ jcnt = 0;
      }
    if (output)
      UTIL_putbuffer_MP (buffer, output);
    /**/ ilen = UTIL_getbuffer_MP (buffer, input, eof_flag);
  }
  fprintf (stderr, "  %d records , %d total\n", jcnt, icnt);
  /**/ fclose (output);
  return (0);
}
