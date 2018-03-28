
/*
 * dsp7.c
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <term.h>
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "fg.h"

static char *title = { " CASSINI CDS header dump  (DSP7) 0.0" };
struct CDS_buffer *buffer;
char *UTIL_extract_packet_type ();
int format_flag = 1;

void u_init (void)
{
  if (format_flag) {
    initscr ();
    /*
     * nonl();
     */
    clear ();
    refresh ();
  }
}
void u_refresh (void)
{
  if (format_flag) {
    refresh ();
  } else {
    fflush (stdout);
  }
}
static int iy = 1;
char *u_cr (char *s, int y)
{
  if (format_flag)
    clrtoeol ();
  else if (iy != y) {
    iy = y;
    printf ("\n");
  }
  return s;
}

u_print (int y, int x, char *s)
{
  if (format_flag) {
    mvprintw (y, x, s);
    clrtoeol ();
  } else
    printf (u_cr (s, y));
}
u_print1 (int y, int x, char *s, int a1)
{
  if (format_flag) {
    mvprintw (y, x, s, a1);
    clrtoeol ();
  } else
    printf (u_cr (s, y), a1);
}
int raw_disp (struct CDS_buffer *buffer, int width, int vert)
{
  int index, i, j;

  if (UTIL_extract_CDS_type (buffer) > 0)
    index = UTIL_extract_CDS_length (buffer) - 5;
  else {
    u_print (vert, 0, "BIU default table in CDS packet ???");
    index = UTIL_extract_RTIU_length (buffer) - 44;
    vert = vert + 1;
  }
  for (i = 0; i < (index / width); i++) {
    u_print1 (vert, 0, "%4.4X: ", i * width);
    for (j = 0; j < width; j++) {
      u_print1 (vert, j * 3 + 6, "%2.2X ",
                buffer->packet.rpws.data[i * width + j]);
    }
    vert = vert + 1;
  }

  if (index % width) {
    u_print1 (vert, 0, "%4.4X: ", i * width);
    for (j = 0; j < (index % width); j++)
      u_print1 (vert, j * 3 + 6, "%2.2X ",
                buffer->packet.rpws.data[i * width + j]);
    vert = vert + 1;
  }
  u_print (vert, 1, "      ");
  for (i = 1; i < width * 3; i = i + 3)
    u_print (vert, i + 5, "-- ");
  vert = vert + 1;
  u_print (vert, 1, " ");
  return vert;
}
int ancillary_structure (struct CDS_buffer *buffer, int width, int vert)
{
  char temp[256];

  sprintf (temp,
           "ancillary %2.2X%2.2X %2.2X%2.2X %2.2X%2.2X %2.2X%2.2X %2.2X%2.2X %2.2X%2.2X %2.2X%2.2X %2.2X%2.2X",
           buffer->packet.ancil.data[1], buffer->packet.ancil.data[0],
           buffer->packet.ancil.data[3], buffer->packet.ancil.data[2],
           buffer->packet.ancil.data[5], buffer->packet.ancil.data[4],
           buffer->packet.ancil.data[7], buffer->packet.ancil.data[6],
           buffer->packet.ancil.data[9], buffer->packet.ancil.data[8],
           buffer->packet.ancil.data[11], buffer->packet.ancil.data[10],
           buffer->packet.ancil.data[13], buffer->packet.ancil.data[12],
           buffer->packet.ancil.data[15], buffer->packet.ancil.data[14]);
  u_print (vert++, 1, temp);
  sprintf (temp, "biust %2.2X%2.2X %2.2X%2.2X %2.2X%2.2X %2.2X%2.2",
           buffer->packet.biust.data[1],
           buffer->packet.biust.data[0],
           buffer->packet.biust.data[3],
           buffer->packet.biust.data[2],
           buffer->packet.biust.data[5],
           buffer->packet.biust.data[4],
           buffer->packet.biust.data[7], buffer->packet.biust.data[6]);
  u_print (vert++, 1, temp);
  return vert;
}
int length_structure (struct CDS_buffer *buffer, int width, int vert)
{
  char temp[256];

  sprintf (temp, "length  data start: %5d  data_length: %5d",
           buffer->packet.index.data_start, buffer->packet.index.data_length);
  u_print (vert++, 1, temp);
  return vert;
}
int compress_structure (struct CDS_buffer *buffer, int width, int vert)
{
  char temp[256];

  sprintf (temp, "Compress:  Method:%2.2X  bit count:%2.2X  resut:%2.2X",
           buffer->packet.compress.method,
           buffer->packet.compress.bit_count, buffer->packet.compress.result);
  u_print (vert++, 1, temp);
  return vert;
}
int ws_time_structure (struct CDS_buffer *buffer, int width, int vert)
{
  char temp[256];

  sprintf (temp,
           "WS_time_tag A: %2.2X%2.2X/%1.1X%2.2X %2.2X:%2.2X:%2.2X B: %2.2X%2.2X/%1.1X%2.2X %2.2X:%2.2X:%2.2X",
           buffer->packet.ws_tag.A.year[0], buffer->packet.ws_tag.A.year[1],
           buffer->packet.ws_tag.A.doy[0], buffer->packet.ws_tag.A.doy[1],
           buffer->packet.ws_tag.A.hour, buffer->packet.ws_tag.A.minute,
           buffer->packet.ws_tag.A.second, buffer->packet.ws_tag.B.year[0],
           buffer->packet.ws_tag.B.year[1], buffer->packet.ws_tag.B.hour,
           buffer->packet.ws_tag.B.minute, buffer->packet.ws_tag.B.second);
  u_print (vert++, 1, temp);
  return vert;
}
int cds_time_structure (struct CDS_buffer *buffer, int width, int vert)
{
  char temp[256];

  sprintf (temp,
           "CDS begin: %2.2X%2.2X%2.2X%2.2X       end: %2.2X%2.2X%2.2X%2.2X ",
           buffer->packet.cds_tag.begin[0], buffer->packet.cds_tag.begin[1],
           buffer->packet.cds_tag.begin[2], buffer->packet.cds_tag.begin[3],
           buffer->packet.cds_tag.end[0], buffer->packet.cds_tag.end[1],
           buffer->packet.cds_tag.end[2], buffer->packet.cds_tag.end[3]);
  u_print (vert++, 1, temp);
  sprintf (temp,
           "CDS epoch: %2.2X%2.2X%2.2X%2.2X  sequence: %2.2X%2.2X%2.2X%2.2X ",
           buffer->packet.cds_tag.epoch[0], buffer->packet.cds_tag.epoch[1],
           buffer->packet.cds_tag.epoch[2], buffer->packet.cds_tag.epoch[3],
           buffer->packet.cds_tag.sequence[0],
           buffer->packet.cds_tag.sequence[1],
           buffer->packet.cds_tag.sequence[2],
           buffer->packet.cds_tag.sequence[3]);
  u_print (vert++, 1, temp);
  return vert;
}
int time_structure (struct CDS_buffer *buffer, int width, int vert)
{
  char temp[256];

  sprintf (temp, "SCLK: %d.%d  SCET:%d.%3.3d  ERT:%d.%3.3d  RDT:%d.%3.3d",
           buffer->packet.chdo_tag.sclk.seconds,
           buffer->packet.chdo_tag.sclk.fine,
           buffer->packet.chdo_tag.scet.days,
           buffer->packet.chdo_tag.scet.milliseconds,
           buffer->packet.chdo_tag.ert.days,
           buffer->packet.chdo_tag.ert.milliseconds,
           buffer->packet.chdo_tag.rct.days,
           buffer->packet.chdo_tag.rct.milliseconds);
  u_print (vert++, 1, temp);
  return vert;
}

int _getbuffer_CDS (struct CDS_buffer *buffer,
                    FILE * input, int iflg, int *rti_start, int *rti_count)
{
  int ilen;
  int rti1, rti2;

  if (!*rti_count)
    return 0;
  if (*rti_count > 0)
    *rti_count = *rti_count - 1;
  while (1) {
    ilen = UTIL_getbuffer_CDS (buffer, input, iflg);
    rti1 = (UTIL_extract_TIME (buffer) & 0x1FF8) << 3;
    rti2 = *rti_start & 0xFFC0;
    if (!*rti_start)
      return ilen;
    if (rti1 == rti2) {
      *rti_start = 0;
      return ilen;
    }
  }
}

main (int argc, char *argv[])
{
  static int width = 16;
  static int delay = 0;
  int flag, second, icnt;
  int ilen;
  long cds[6];
  int i;
  int eof_flag = 1;
  int raw_flag = 1;
  int hdr_flag = 0;
  int h_flag = 0;
  int shdr_flag = 0;
  int rhdr_flag = 0;
  unsigned char string[128];
  int rti_start = 0;
  int rti_count = -1;
  int skip_count = 0;
  FILE *input = stdin;
  char fname[128];
  char temp[256];
  char temp2[128];
  int max = 0;
  int last_seq[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  char splat = ' ';
  int vert;

  fg_flags (argc, argv);
  if (fg_flag ("get")) {
    fg_flagx ("+getraw", "");
    fg_flagx ("+getcds", "");
  }
  if (fg_flag ("help") || fg_flag ("h")) {
    fprintf (stderr, "%s   HELP SCREEN\n", title);
    fprintf (stderr, "\n");
    fprintf (stderr, "     +find fn    find archive data\n");
    fprintf (stderr, "     +getcds     get data from current file\n");
    fprintf (stderr, "     +get          synonym\n");
    fprintf (stderr, "     -width nn   hexdump width (bytes)\n");
    fprintf (stderr, "     -delay nn   nn second delay between update\n");
    fprintf (stderr, "     -rti 0xXX   RTI to start display\n");
    fprintf (stderr, "     -count nn   display nn records\n");
    fprintf (stderr, "     -skip nn    skip nn records\n");
    fprintf (stderr, "     -format     supress screen addressing\n");
    fprintf (stderr, "     -raw        supress hex dump\n");
    fprintf (stderr, "     +eof        quit at eof\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "\n");
    exit (0);
  }

  buffer = malloc (32768 + 1024);
  width = fg_int ("width", 16);
  delay = fg_int ("delay", 0);
  if (fg_flag ("eof") == '+')
    eof_flag = 0;
  if (fg_flag ("raw") == '-')
    raw_flag = 0;
  if (fg_flag ("get"))
    fg_flagx ("getcds", "");
  if (fg_flag ("format") == '-')
    format_flag = 0;
  rti_start = fg_int ("rti", rti_start);
  rti_count = fg_int ("count", rti_count);
  skip_count = fg_int ("skip", skip_count);
  if (fg_flag ("find")) {
    input = UTIL_find_open (fg_flagc ("find"), "rb");
    if (input)
      fprintf (stderr, "dsp7 find: %s\n", UTIL_find_name ());
    else {
      fprintf (stderr, "dsp7 find: file not found: %s\n", fg_flagc ("find"));
      exit (0);
    }
  }
  if (fg_flag ("getcds") == '+') {
    strcpy (fname, UTIL_filename (FILE_RAW, FILE_LOCATE_DEFAULT));
    fprintf (stderr, "dsp7 source: %s\n", fname);
    input = fopen (fname, "rb");
    if (!fname)
      exit (0);
  }
  fprintf (stderr, "dsp7 rti_start: %d (%d)\n", rti_start, rti_count);

  sleep (3);
  u_init ();
  vert = 0;
  u_print (vert++, 10, " negative length for clear-text");
  u_print (vert, 10, title);
  for (icnt = -1; icnt < skip_count; icnt++)
    ilen = _getbuffer_CDS (buffer, input, eof_flag, &rti_start, &rti_count);
  while (ilen > 0) {
    vert = 2;
    UTIL_extract_CDS (buffer, cds);
    if (hdr_flag) {
      splat = ' ';
      if (UTIL_extract_CDS_sequence (buffer) !=
          last_seq[UTIL_extract_packet_sindex (buffer)])
        splat = '*';
      if (cds[3] & 0xFF00)
        splat = 'E';
      sprintf (temp,
               " Cycle %5d Seq %c%5d CDS %4.4X %4.4X %4.4X %4.4X %4.4X %4.4X  Time 0x%8.4X.%X:%X  %c  %s",
               icnt, splat,
               UTIL_extract_CDS_sequence (buffer),
               cds[0], cds[1], cds[2], cds[3], cds[4], cds[5],
               UTIL_extract_TIME (buffer),
               UTIL_extract_RTI (buffer),
               UTIL_extract_subRTI (buffer),
               splat, UTIL_extract_packet_type (buffer));
      last_seq[UTIL_extract_packet_sindex (buffer)] =
        UTIL_extract_CDS_sequence (buffer) + 1;
    } else {
      if (raw_flag)
        u_print1 (vert++, 50, " Cycle %d", icnt++);
      else
        u_print1 (vert++, 5,
                  " Cycle %3d   ------------------------------------------",
                  icnt++);
      vert = ancillary_structure (buffer, width, vert);
      vert = length_structure (buffer, width, vert);
      vert = compress_structure (buffer, width, vert);
      vert = ws_time_structure (buffer, width, vert);
      vert = cds_time_structure (buffer, width, vert);
      vert = time_structure (buffer, width, vert);

      sprintf (temp,
               " %2.2X %s  (.record_type)%4d    ",
               UTIL_extract_CDS_type (buffer),
               UTIL_extract_packet_type (buffer), buffer->record_type);
      u_print (vert++, 1, temp);
      u_print1 (vert, 1, " Seq%6.2d", UTIL_extract_CDS_sequence (buffer));
      if (UTIL_extract_CDS_length (buffer) > max)
        max = UTIL_extract_CDS_length (buffer);
      sprintf (temp,
               " Len%5.2d/%5.2d", UTIL_extract_CDS_length (buffer), max);
      u_print (vert, 15, temp);
      sprintf (temp,
               " Time 0x%8.4X.%X:%X",
               UTIL_extract_TIME (buffer),
               UTIL_extract_RTI (buffer), UTIL_extract_subRTI (buffer)
        );
      u_print (vert, 32, temp);
      sprintf (temp, " %d.%d:%d",
               UTIL_extract_TIME (buffer),
               UTIL_extract_RTI (buffer), UTIL_extract_subRTI (buffer)
        );
      u_print (vert++, 50, temp);
      sprintf (temp, "CDS HDR (0x0000) %4.4X %4.4X %4.4X %4.4X %4.4X %4.4X",
               cds[0], cds[1], cds[2], cds[3], cds[4], cds[5]);
      u_print (vert++, 0, temp);

      if (UTIL_extract_CDS_length (buffer))
        if (raw_flag)
          vert = raw_disp (buffer, width, vert);
    }
    u_refresh ();
    if (delay)
      sleep (delay);
    icnt += 1;
    ilen = _getbuffer_CDS (buffer, input, eof_flag, &rti_start, &rti_count);
  }
  return (0);
}
