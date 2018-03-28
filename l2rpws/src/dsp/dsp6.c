
/*
 * dsp6.c
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

static char *title = { " CASSINI RTIU data dump  (DSP6) 1.0" };
struct CDS_buffer buffer;
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
int mv_printw (int vert, int hor, char *stg)
{
  static int vertical = 99;
  static int horizontal = 0;
  int spaces;
  int i;

  if (format_flag) {
    mvprintw (vert, hor, "%s", stg);
    if (vert != vertical)
      clrtoeol ();
  } else {
    if (vert != vertical) {
      fputs ("\n", stdout);
      horizontal = 0;
    }
    spaces = hor - horizontal;
    horizontal = hor + strlen (stg);
    if (spaces > 0) {
      for (i = 0; i < spaces; i++)
        fputc (' ', stdout);
    }
    fputs (stg, stdout);
  }
  vertical = vert;
  return vert + 1;
}

rtiu_disp (struct CDS_buffer * buffer, int width, int _vert)
{
  int i, vert;
  char temp[128];
  char tmp[32];

  vert = _vert;
  sprintf (temp, "f_length      %8d", buffer->f_length);
  mv_printw (vert++, 5, temp);
  sprintf (temp, "record_type 0x%8.4X", buffer->record_type);
  mv_printw (vert++, 5, temp);
  sprintf (temp, "status      0x%8.4X", buffer->status);
  mv_printw (vert++, 5, temp);
  temp[0] = 0;
  for (i = 0; i < 36; i += 2) {
    sprintf (tmp, "%2.2X%2.2X  ",
             buffer->packet.rtiu.status[i + 0],
             buffer->packet.rtiu.status[i + 1]);
    strcat (temp, tmp);
    if ((i & 15) == 14) {
      mv_printw (vert++, 3, temp);
      temp[0] = 0;
    }
  }
  if (temp[0]) {
    mv_printw (vert++, 3, temp);
    temp[0] = 0;
  }

  for (i = 0; i < 32; i += 2) {
    sprintf (tmp, "%2.2X%2.2X  ",
             buffer->packet.rtiu.header[i + 0],
             buffer->packet.rtiu.header[i + 1]);
    strcat (temp, tmp);
    if ((i & 15) == 14) {
      mv_printw (vert++, 3, temp);
      temp[0] = 0;
    }
  }
  if (temp[0]) {
    mv_printw (vert++, 3, temp);
    temp[0] = 0;
  }

  return vert;
}

pcrtiu_disp (struct CDS_buffer * buffer, int width, int _vert)
{
  int i, vert;
  char temp[128];
  char tmp[32];

  vert = _vert;
  sprintf (temp, "f_length      %8d", buffer->f_length);
  mv_printw (vert++, 5, temp);
  sprintf (temp, "record_type 0x%8.4X", buffer->record_type);
  mv_printw (vert++, 5, temp);
  sprintf (temp, "status      0x%8.4X", buffer->status);
  mv_printw (vert++, 5, temp);
  temp[0] = 0;
  for (i = 0; i < 32; i += 2) {
    sprintf (tmp, "%2.2X %2.2X  ",
             buffer->packet.rtiu.header[i],
             buffer->packet.rtiu.header[i + 1]);
    strcat (temp, tmp);
    if ((i & 15) == 14) {
      mv_printw (vert++, 3, temp);
      temp[0] = 0;
    }
  }
  if (temp[0]) {
    mv_printw (vert++, 3, temp);
    temp[0] = 0;
  }

  return vert;
}

int raw_dump (struct CDS_buffer *buffer, int width, int _vert)
{
  int index, i, j, vert;
  char temp[256];

  vert = _vert;
  index = 1024;
  for (i = 0; i < (index / width); i++) {
    sprintf (temp, "%4.4X: ", i * width);
    mv_printw (vert, 0, temp);
    for (j = 0; j < width; j++) {
      sprintf (temp, "%2.2X ", buffer->packet.cds.header[i * width + j]);
      mv_printw (vert, j * 3 + 6, temp);
    }
    vert = vert + 1;
  }

  if (index % width) {
    sprintf (temp, "%4.4X: ", i * width);
    mv_printw (vert, 0, temp);
    for (j = 0; j < (index % width); j++) {
      sprintf (temp, "%2.2X ", buffer->packet.cds.header[i * width + j]);
      mv_printw (vert, j * 3 + 6, temp);
    }
    vert = vert + 1;
  }
  mv_printw (vert, 1, "      ");
  for (i = 1; i < width * 3; i = i + 3)
    mv_printw (vert, i + 5, "-- ");
  vert = vert + 1;
  mv_printw (vert, 1, " ");
  return vert;
}
int raw_dumpl (struct CDS_buffer *buffer, int width, int _vert, int length)
{
  int i, j, vert;
  char temp[256];

  vert = _vert;
  for (i = 0; i < (length / width); i++) {
    sprintf (temp, "%4.4X: ", i * width);
    mv_printw (vert, 0, temp);
    for (j = 0; j < width; j++) {
      sprintf (temp, "%2.2X ", buffer->packet.cds.header[i * width + j]);
      mv_printw (vert, j * 3 + 6, temp);
    }
    vert = vert + 1;
  }

  if (length % width) {
    sprintf (temp, "%4.4X: ", i * width);
    mv_printw (vert, 0, temp);
    for (j = 0; j < (length % width); j++) {
      sprintf (temp, "%2.2X ", buffer->packet.cds.header[i * width + j]);
      mv_printw (vert, j * 3 + 6, temp);
    }
    vert = vert + 1;
  }
  mv_printw (vert, 1, "      ");
  for (i = 1; i < width * 3; i = i + 3)
    mv_printw (vert, i + 5, "-- ");
  vert = vert + 1;
  mv_printw (vert, 1, " ");
  return vert;
}

int raw_disp (struct CDS_buffer *buffer, int width, int _vert)
{
  int index, i, j, vert;
  char temp[256];

  vert = _vert;
  if (UTIL_extract_CDS_type (buffer) > 0)
    index = UTIL_extract_CDS_length (buffer) - 5;
  else {
    mv_printw (vert, 0, "BIU default table in CDS packet ???");
    index = UTIL_extract_RTIU_length (buffer) - 44;
    vert = vert + 1;
  }
  for (i = 0; i < (index / width); i++) {
    sprintf (temp, "%4.4X: ", i * width);
    mv_printw (vert, 0, temp);
    for (j = 0; j < width; j++) {
      sprintf (temp, "%2.2X ", buffer->packet.rpws.data[i * width + j]);
      mv_printw (vert, j * 3 + 6, temp);
    }
    vert = vert + 1;
  }

  if (index % width) {
    sprintf (temp, "%4.4X: ", i * width);
    mv_printw (vert, 0, temp);
    for (j = 0; j < (index % width); j++) {
      sprintf (temp, "%2.2X ", buffer->packet.rpws.data[i * width + j]);
      mv_printw (vert, j * 3 + 6, temp);
    }
    vert = vert + 1;
  }
  mv_printw (vert, 1, "      ");
  for (i = 1; i < width * 3; i = i + 3)
    mv_printw (vert, i + 5, "-- ");
  vert = vert + 1;
  mv_printw (vert, 1, " ");
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
int main (int argc, char *argv[])
{
  static int width = 16;
  int cdsflag = 0;
  int rawflag = 0;
  int rtiuflag = 0;
  int pcrtiuflag = 0;
  int dumpflag = 0;
  int ldumpflag = 0;
  static int delay = 0;
  int flag, second, icnt;
  int ilen;
  int jlen;
  long cds[6];
  int i;
  unsigned char string[128];
  int rti_start = 0;
  int rti_count = -1;
  FILE *input = stdin;
  char fname[128];
  char temp[128];
  int max = 0;
  int vert;

  fg_flags (argc, argv);
  if (fg_flag ("get"))
    fg_flagx ("getcds", "");
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
    fprintf (stderr, "     -format     supress screen addressing\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "     +raw        display raw data buffer\n");
    fprintf (stderr, "     +cds        display CDS header line\n");
    fprintf (stderr, "     +rtiu       display rtiu lines\n");
    fprintf (stderr, "     +pcrtiu     display pc-rtiu lines\n");
    fprintf (stderr, "     +dump       display data area, 1024\n");
    fprintf (stderr, "     +ldump      display data area, calculated\n");
    fprintf (stderr, "\n");
    return 0;
  }

  width = fg_int ("width", 16);
  delay = fg_int ("delay", 0);
  if (fg_flag ("format") == '-')
    format_flag = 0;
  rti_start = fg_int ("rti", rti_start);
  rti_count = fg_int ("count", rti_count);
  if (fg_flag ("find")) {
    input = UTIL_find_open (fg_flagc ("find"), "rb");
    if (input)
      fprintf (stderr, "dsp6 find: %s\n", UTIL_find_name ());
    else {
      fprintf (stderr, "dsp6 find: file not found: %s\n", fg_flagc ("find"));
      return 0;
    }
  }
  if (fg_flag ("cds"))
    cdsflag = 1;
  if (fg_flag ("raw"))
    rawflag = 1;
  if (fg_flag ("rtiu"))
    rtiuflag = 1;
  if (fg_flag ("pcrtiu"))
    pcrtiuflag = 1;
  if (fg_flag ("dump")) {
    rawflag = 0;
    dumpflag = 1;
  }
  if (fg_flag ("ldump")) {
    rawflag = 0;
    dumpflag = 0;
    ldumpflag = 1;
  }
  if (fg_flag ("getcds") == '+') {
    strcpy (fname, UTIL_filename (FILE_RAW, FILE_LOCATE_DEFAULT));
    fprintf (stderr, "dsp6 source: %s\n", fname);
    input = fopen (fname, "rb");
    if (!fname)
      return 0;
  }
  fprintf (stderr, "dsp6 rti_start: %d (%d)\n", rti_start, rti_count);

  sleep (3);
  u_init ();
  mv_printw (0, 10, " negative length for clear-text");
  mv_printw (1, 10, title);
  ilen = _getbuffer_CDS (&buffer, input, 1, &rti_start, &rti_count);
  while (ilen) {
    vert = 1;
    sprintf (temp, " Cycle %d", icnt++);
    mv_printw (vert++, 50, temp);
    UTIL_extract_CDS (&buffer, cds);
    sprintf (temp,
             " %2.2X %s  (.record_type)%4d    ",
             UTIL_extract_CDS_type (&buffer),
             UTIL_extract_packet_type (&buffer), buffer.record_type);
    mv_printw (vert++, 1, temp);
    sprintf (temp, " Seq%6.2d", UTIL_extract_CDS_sequence (&buffer));
    mv_printw (vert, 1, temp);
    if (UTIL_extract_CDS_length (&buffer) > max)
      max = UTIL_extract_CDS_length (&buffer);
    sprintf (temp, " Len%5.2d/%5.2d", UTIL_extract_CDS_length (&buffer), max);
    mv_printw (vert, 15, temp);
    sprintf (temp,
             " Time 0x%8.4X %2.2X",
             UTIL_extract_TIME (&buffer), UTIL_extract_RTI (&buffer));
    mv_printw (vert, 32, temp);
    sprintf (temp, " %d.%d",
             UTIL_extract_TIME (&buffer), UTIL_extract_RTI (&buffer)
      );
    mv_printw (vert++, 50, temp);
    if (cdsflag) {
      sprintf (temp, "CDS HDR (0x0000) %4.4X %4.4X %4.4X %4.4X %4.4X %4.4X",
               cds[0], cds[1], cds[2], cds[3], cds[4], cds[5]);
      mv_printw (vert++, 0, temp);
    }
    if (rtiuflag) {
      vert = rtiu_disp (&buffer, width, vert);
    }
    if (pcrtiuflag) {
      vert = pcrtiu_disp (&buffer, width, vert);
    }
    if (rawflag) {
      if (UTIL_extract_CDS_length (&buffer))
        vert = raw_disp (&buffer, width, vert);
    }
    if (dumpflag) {
      vert = raw_dump (&buffer, width, vert);
    }
    if (ldumpflag) {
      jlen = buffer.packet.rtiu.header[0] << 8 | buffer.packet.rtiu.header[1];
      sprintf (temp, "data buffer length(%d) %2.2X%2.2X",
               jlen,
               buffer.packet.rtiu.header[0], buffer.packet.rtiu.header[1]);
      mv_printw (vert++, 0, temp);
      jlen = jlen - 52;
      if (jlen > 0)
        vert = raw_dumpl (&buffer, width, vert, jlen);
    }
    u_refresh ();
    if (delay)
      sleep (delay);
    ilen = _getbuffer_CDS (&buffer, input, 1, &rti_start, &rti_count);
  }
  return 0;
}
