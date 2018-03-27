
/*
 * dsp5.c
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

static char *title = { " CASSINI MP data dump  (DSP5) 2.0" };
struct MP_buffer *buffer;
int format_flag = 1;
static int epoch;

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
void u_clear (int y, int x, int flag)
{
  if (format_flag) {
    move (y, x);
    switch (flag) {
     case 0:
       clrtobot ();
       break;
     case 1:
       clrtoeol ();
       break;
    }
  }
}
u_print0 (int y, int x, char *s)
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
u_print2 (int y, int x, char *s, int a1, int a2)
{
  if (format_flag) {
    mvprintw (y, x, s, a1, a2);
    clrtoeol ();
  } else
    printf (u_cr (s, y), a1, a2);
}
u_print3 (int y, int x, char *s, int a1, char *s2, int a3)
{
  if (format_flag) {
    mvprintw (y, x, s, a1, s2, a3);
    clrtoeol ();
  } else
    printf (u_cr (s, y), a1, s2, a3);
}
u_print6 (int y, int x, char *s,
          int a1, int a2, int a3, int a4, int a5, int a6)
{
  if (format_flag) {
    mvprintw (y, x, s, a1, a2, a3, a4, a5, a6);
    clrtoeol ();
  } else
    printf (u_cr (s, y), a1, a2, a3, a4, a5, a6);
}
char *st_wfr (int st, int flg)
{
  char *msg[] = { "LOband", "HIband",   /* 0 -   WFR 0 */
    "(234)", "(012)",                   /* 2 - 1 */
    "raw", "CM-1", "DCC",               /* 4 - 2 */
    "DCC/ext", "raw+1", "CM-5",
    "CM-6", "CM-7", "CM-8",
    "CM-9", "CM-10", "CM-11",
    "CM-12", "CM-13", "CM-14",
    "CM-15",
    "Ant-0", "Ant-1", "Ant-2",          /* 20 - 3 */
    "Ant-3", "Ant-4", "Ant-5",
    "Ant-6", "Ant-7",
    "CH0", "CH1", "CH2",                /* 28 - 4 */
    "CH3", "CH4", "bad-5",
    "bad-6", "COM",
    "CH-0", "CH-1", "CH-2",             /* 36 - 5 */
    "CH-3", "CH-4", "CHx2",
    "CHx3", "CHx5",
    "Nominal", "More STS",              /* 44 - 6 */
    "Ant-0", "Ant-1",                   /* 46 - LFDR 10 */
    "Ant-2", "Ant-3",
    "LOG", "LINEAR",                    /* 50 - 11 */
    "CM-0", "CM-1", "DCC",              /* 52 - WBR 22 */
    "DCC/ext", "CM-4", "CM-5",
    "CM-6", "CM-7", "CM-8",
    "CM-9", "CM-10", "CM-11",
    "CM-12", "CM-13", "CM-14",
    "CM-15",
    "Ant-0", "Ant-1", "Ant-2",          /* 68 - 23 */
    "Ant-3", "Ant-4", "Ant-5",
    "Ant-6", "Ant-7",
    "Ex", "Ez", "Bx", "Bz",             /* 76 - MFR 33 */
    "Uncompressed", "COMPRESSED",       /* 80 - 32 */
    "0dB", "10dB", "20dB", "30dB"
  };
  int offset[] = { 0, 2, 4, 20, 28, 36, 44, 82, 0, 0,
    46, 50, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 52, 68, 0, 0, 0, 0, 0, 0,
    0, 0, 80, 76, 0, 0, 0, 0, 0, 0,
    86
  };
  return msg[offset[st] + flg];
}
int status_wfr (unsigned char *mp, int ipos)
{
  char string[128];
  int vert;

  vert = ipos;
  sprintf (string, "size:%d, segment:%d, FB:%s  LP:%s, gains:%s %s %s       ",
           mp[4] >> 4,
           mp[4] & 0x0F,
           st_wfr (0, (mp[5] & 0x80) >> 7),
           st_wfr (1, (mp[5] & 0x40) >> 6),
           st_wfr (7, (mp[5] & 0x03) >> 0),
           st_wfr (7, (mp[5] & 0x0C) >> 2), st_wfr (7, (mp[5] & 0x30) >> 4));

  u_print0 (vert++, 0, string);
  sprintf (string, "cmprs:%s, MSF:%s, Ant:%s, Chan:%s, Mode:%s        ",
           st_wfr (2, (mp[6] & 0xF0) >> 4),
           st_wfr (6, (mp[6] & 0x08) >> 3),
           st_wfr (3, mp[6] & 0x07),
           st_wfr (4, (mp[7] & 0x38) >> 3), st_wfr (5, mp[7] & 0x07));
  u_print0 (vert++, 0, string);
  if (mp[6] & 0x08) {
    sprintf (string, "LP DAC 0:%2.2X   LP DAC 1:%2.2X        ", mp[8], mp[9]);
    u_print0 (vert++, 0, string);
  }
  return vert;
}
int status_lfdr (unsigned char *mp, int ipos)
{
  char string[128];
  int vert;

  vert = ipos;
  sprintf (string, "ant:%s, Size:%d, L/L:%s, Chan:%s, Gain:%2.2X        ",
           st_wfr (10, (mp[4] & 0xC0) >> 6),
           (mp[4] & 0x30) >> 4,
           st_wfr (11, (mp[4] & 0x08) >> 3), (mp[4] & 0x07) >> 0, mp[5]);
  u_print0 (vert++, 0, string);
  return vert;
}
int status_wbr (unsigned char *mp, int ipos)
{
  char string[128];
  int vert;

  vert = ipos;
  sprintf (string, "size:%d, segment:%d, FB:%s  gains:%X        ",
           mp[4] >> 4,
           mp[4] & 0x0F, st_wfr (0, (mp[5] & 0x80) >> 7), mp[5] & 0x07);
  u_print0 (vert++, 0, string);
  sprintf (string, "cmprs:%s, MSF:%s, Ant:%s, AGC:0x%2.2X       ",
           st_wfr (22, (mp[6] & 0xF0) >> 4),
           st_wfr (6, (mp[6] & 0x08) >> 3), st_wfr (23, mp[6] & 0x07), mp[7]);
  u_print0 (vert++, 0, string);
  if (mp[6] & 0x08) {
    sprintf (string, " SUB-RTI %d  ", mp[9]);
    if ((mp[6] & 0x07) == 4) {
      sprintf (string, " LP DAC-0: %2.2X  SUB-RTI %d  ", mp[8], mp[9]);
    }
    if ((mp[6] & 0x07) == 3) {
      sprintf (string, "HFR XLATE: %2.2X  SUB-RTI %d  ", mp[8], mp[9]);
    }
    u_print0 (vert++, 0, string);
  }
  return vert;
}
char *st_mro (int flag)
{
  static char *text[] = { "low rate   ",
    "high rate  ",
    "compression"
  };
  if (flag & 0x02)
    return text[0];
  if (flag & 0x04)
    return text[2];
  if (flag & 0x08)
    return text[1];
}
char *st_bank (int flag)
{
  static char *text[] = { "",
    "hi bank"
  };
  if (flag & 0x10)
    return text[1];
  return text[0];
}
int addr_mro (unsigned char *mp, int len)
{
  int addr;

  addr = mp[4] << 8;
  addr = addr + mp[5];
  if (len > 64) {
    addr = addr & 0xFFE0;
  }
  return addr;
}
int status_mro (unsigned char *mp, int ipos, int len)
{
  char string[128];
  int vert;
  int addr;

  vert = ipos;
  addr = addr_mro (mp, len);
  if (len > 64) {
    sprintf (string, " address: %4.4X    source: %s    %s     ",
             addr, st_mro (mp[5] & 0x0F), st_bank (mp[5]));
  } else
    sprintf (string, " address: %4.4X                              ", addr);

  u_print0 (vert++, 0, string);
  return vert;
}
int status_mfr (unsigned char *mp, int ipos)
{
  char string[128];
  int vert;

  vert = ipos;
  sprintf (string, "Antenna:%s, Compression:%s                 ",
           st_wfr (33, (mp[4] & 0x06) >> 1),
           st_wfr (32, (mp[4] & 0x01) >> 0));
  u_print0 (vert++, 0, string);
  return vert;
}
int mro_disp (struct MP_buffer *buffer, int width, int ipos, int status,
              int addr)
{
  int index, i, j, vert;
  int hor;
  char text[1024];

  index = UTIL_MP_length (buffer) + 3;
  vert = ipos;
  if (UTIL_extract_MP_type (buffer) <= 0) {
    u_print0 (vert++, 0, "BIU default table in mini packet ???");

/*	    return; /**/
  }
  i = 0;
  if (status >= 0) {
    u_print1 (vert, 0, "      ", i * width);
    for (j = 0; j < 6; j++) {
      u_print1 (vert, j * 3 + 6, "%2.2X ",
                buffer->packet.mpp.mini_packet[i * width + j]);
    }
    vert = vert + 1;
  }
  index = index - abs (status);
  for (i = 0; i < (index / width); i++) {
    u_print1 (vert, 0, "%4.4X: ", i * width + addr);
    for (j = 0; j < width; j++) {
      u_print1 (vert, j * 3 + 6, "%2.2X ",
                buffer->packet.mpp.mini_packet[i * width + j + 6]);
    }
    for (j = 0; j < width; j++) {
      char ch;

      ch = buffer->packet.mpp.mini_packet[i * width + j + 6];
      if (ch < 0x20)
        ch = '.';
      if (ch > 0x7E)
        ch = '.';
      u_print1 (vert, j + (width * 3) + 8, "%c", ch);
    }
    vert = vert + 1;
  }

  if (index % width) {
    u_print1 (vert, 0, "%4.4X: ", i * width + addr);
    for (j = 0; j < (index % width); j++) {
      hor = j * 3 + 6;
      u_print1 (vert, hor, "%2.2X ",
                buffer->packet.mpp.mini_packet[i * width + j + 6]);
    }
    for (j = 0; j < (index % width); j++) {
      char ch;

      hor = j + (width * 3) + 8;
      ch = buffer->packet.mpp.mini_packet[i * width + j + 6];
      if (ch < 0x20)
        ch = '.';
      if (ch > 0x7E)
        ch = '.';
      u_print1 (vert, hor, "%c", ch);
    }

    u_clear (vert, hor + 3, 1);
    vert = vert + 1;

  }
  if (status >= 0) {
    u_print0 (vert, 1, "      ");
    for (i = 1; i < width * 3; i = i + 3)
      u_print0 (vert, i + 5, "-- ");
    vert = vert + 1;
    u_print0 (vert, 1, " ");
    u_clear (vert, 1, 0);
  }
  return vert;
}
int raw_disp (struct MP_buffer *buffer, int width, int ipos)
{
  int index, i, j, vert;
  int hor;

  index = UTIL_MP_length (buffer) + 3;
  vert = ipos;
  if (UTIL_extract_MP_type (buffer) <= 0) {
    u_print0 (vert++, 0, "BIU default table in mini packet ???");

/*	    return; /**/
  }
  for (i = 0; i < (index / width); i++) {
    u_print1 (vert, 0, "%4.4X: ", i * width);
    for (j = 0; j < width; j++) {
      u_print1 (vert, j * 3 + 6, "%2.2X ",
                buffer->packet.mpp.mini_packet[i * width + j]);
    }
    vert = vert + 1;
  }

  if (index % width) {
    u_print1 (vert, 0, "%4.4X: ", i * width);
    for (j = 0; j < (index % width); j++) {
      hor = j * 3 + 6;
      u_print1 (vert, hor, "%2.2X ",
                buffer->packet.mpp.mini_packet[i * width + j]);
    }
    u_clear (vert, hor + 3, 1);
    vert = vert + 1;
  }
  u_print0 (vert, 1, "      ");
  for (i = 1; i < width * 3; i = i + 3)
    u_print0 (vert, i + 5, "-- ");
  vert = vert + 1;
  u_print0 (vert, 1, " ");
  u_clear (vert, 1, 0);
  return vert;
}
int _getbuffer_MP (struct MP_buffer *buffer,
                   FILE * input, int iflg, int *rti_start, int *rti_count)
{
  int ilen;
  int rti;

  if (!*rti_count)
    return 0;
  if (*rti_count > 0)
    *rti_count -= 1;
  while (1) {
    ilen = UTIL_getbuffer_MP (buffer, input, iflg);
    rti = UTIL_extract_MP_TIME (buffer);
    if (!*rti_start)
      return ilen;
    if (rti == (long) *rti_start) {
      *rti_start = 0;
      return ilen;
    }
  }
}

main (int argc, char *argv[])
{
  FILE *input = stdin;
  char str0[1024];
  char fname[128];
  static int type_count[16] = { 16 * 0 };
  static int width = 16;
  static int delay = 0;
  int flag, second, icnt;
  int ilen, jlen;
  int ipos;
  int rti_start = 0;
  int rti_count = -1;
  int skip_count = 0;
  int mp_len;
  long cds[6];
  int i;
  time_t pkt_time, pkt_etime, pkt_epoc;
  struct tm *pkt_tm;
  struct tm *pkt_ev;
  char string[128];
  char wfr, lfdr, wbr, dust, mfr, lp, hfr, mro, mrod;
  char headers = 1;
  int t_mask = 0xFFFFFFFF;
  int eof_flag = 1;

  buffer = malloc (32768 + 1024);
  fg_flags (argc, argv);
  if (fg_flag ("help") || fg_flag ("h")) {
    fprintf (stderr, "%s   HELP SCREEN\n", title);
    fprintf (stderr, "\n");
    fprintf (stderr, "     +find fn    find archive file\n");
    fprintf (stderr, "     +getmp      get data from current file\n");
    fprintf (stderr, "     +getmpus    un-segmented data set\n");
    fprintf (stderr, "     -width nn   hexdump width (bytes)\n");
    fprintf (stderr, "     -delay nn   nn second delay between update\n");
    fprintf (stderr, "     -rti 0xXX   RTI to start display\n");
    fprintf (stderr, "     -count nn   display nn records\n");
    fprintf (stderr, "     -skip nn    skip nn records\n");
    fprintf (stderr, "     -format     supress screen addressing\n");
    fprintf (stderr, "     +eof        quit at eof\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "                 status display enable for:\n");
    fprintf (stderr, "     +wfr\n");
    fprintf (stderr, "     +lfdr\n");
    fprintf (stderr, "     +dust\n");
    fprintf (stderr, "     +wbr\n");
    fprintf (stderr, "     +mfr\n");
    fprintf (stderr, "     +mro +dmro\n");
    fprintf (stderr, "     lp\n");
    fprintf (stderr, "     hfr\n");
    fprintf (stderr, "\n");
    exit (0);
  }
  if (fg_flag ("find")) {
    input = UTIL_find_open (fg_flagc ("find"), "rb");
    if (input)
      fprintf (stderr, "dsp5 find: %s\n", UTIL_find_name ());
    else {
      fprintf (stderr, "dsp5 find: file not found: %s\n", fg_flagc ("find"));
      exit (0);
    }
  }
  if (fg_flag ("eof") == '+')
    eof_flag = 0;
  if (fg_flag ("getmp") == '+') {
    strcpy (fname, UTIL_filename (FILE_MP, FILE_LOCATE_DEFAULT));
    fprintf (stderr, "dsp5 source: %s\n", fname);
    input = fopen (fname, "rb");
    if (!fname)
      exit (0);
  } else if (fg_flag ("getmpus") == '+') {
    strcpy (fname, UTIL_filename (FILE_MPUS, FILE_LOCATE_DEFAULT));
    fprintf (stderr, "dsp5 source: %s\n", fname);
    input = fopen (fname, "rb");
    if (!fname)
      exit (0);
  }
  width = fg_int ("width", 16);
  delay = fg_int ("delay", 0);
  if (fg_flag ("format") == '-')
    format_flag = 0;
  wfr = fg_flag ("wfr");
  lfdr = fg_flag ("lfdr");
  wbr = fg_flag ("wbr");
  dust = fg_flag ("dust");
  mfr = fg_flag ("mfr");
  lp = fg_flag ("lp");
  hfr = fg_flag ("hfr");
  mrod = fg_flag ("dmro");
  if (mrod) {
    headers = 0;
    format_flag = 0;
  }
  mro = fg_flag ("mro");
  rti_start = fg_int ("rti", rti_start);
  rti_count = fg_int ("count", rti_count);
  skip_count = fg_int ("skip", skip_count);
  fprintf (stderr, "dsp5 rti_start: %d (%d)\n", rti_start, rti_count);
  if (fg_flag ("time_mask")) {
    t_mask = fg_int ("time_mask", -1);
    fprintf (stderr, "time mask %8.8X\n", t_mask);
  }
  sleep (2);
  u_init ();
  u_print0 (0, 10, " negative length for clear-text");
  u_print0 (1, 10, title);
  for (icnt = -1; icnt < skip_count; icnt++);
  ilen = _getbuffer_MP (buffer, input, eof_flag, &rti_start, &rti_count);
  while (ilen > 0) {
    ipos = 2;
    type_count[UTIL_extract_MP_type (buffer)] += 1;
    epoch = (buffer->packet.cds_tag.epoch[0] << 24) |
      (buffer->packet.cds_tag.epoch[1] << 16) |
      (buffer->packet.cds_tag.epoch[2] << 8) |
      (buffer->packet.cds_tag.epoch[3] << 0);

/*	if(format_flag)
	  u_print1(ipos++,50," Cycle %d", icnt); /**/
    if (headers)
      u_print3 (ipos++, 1,
                " %2.2X %s  (.record_type) %4.4X    ",
                UTIL_extract_MP_type (buffer),
                UTIL_extract_MP_packet_type (buffer),
                UTIL_MSB_to_long (buffer->record_type)
        );

    mp_len = UTIL_MP_length (buffer);
    sprintf (str0, " Len%6.2d   [%d:%d]     Cycle %4d",
             mp_len,
             buffer->packet.index.data_start,
             buffer->packet.index.data_length, icnt);
    if (headers)
      u_print0 (ipos++, 10, str0);

    pkt_time = UTIL_extract_PKT_TIME (buffer) & t_mask;
    pkt_epoc = pkt_time + epoch;
    pkt_tm = localtime (&pkt_epoc);
    sprintf (str0, " Time %8X %4X (%2.2d:%2.2d:%2.2d %4d %3d )",
             pkt_time,
             (pkt_time & 0x1FFF) << 3,
             pkt_tm->tm_hour,
             pkt_tm->tm_min,
             pkt_tm->tm_sec, pkt_tm->tm_year + 1900, pkt_tm->tm_yday);
    if (headers)
      u_print0 (ipos++, 8, str0);

    pkt_etime = UTIL_event_time (buffer);
    pkt_epoc = pkt_etime + epoch;
    pkt_ev = localtime (&pkt_epoc);
    sprintf (str0, "Event %8X %4X (%2.2d:%2.2d:%2.2d.%3.3d) %X ",
             pkt_etime,
             (pkt_etime & 0x1FFF) << 3,
             pkt_ev->tm_hour,
             pkt_ev->tm_min,
             pkt_ev->tm_sec, UTIL_extract_MP_RTI (buffer) * 125, epoch);
    if (headers)
      u_print0 (ipos++, 8, str0);

    string[0] = 0;
    for (i = 0; i < 16; i++) {
      jlen = strlen (string);
      if (type_count[i]) {
        strcat (string, UTIL_get_MP_packet_type (i));
        sprintf (&string[jlen + 5], "%d  ", type_count[i]);
      }
    }
    if (headers)
      u_print0 (ipos++, 1, string);
    if (UTIL_MP_length (buffer)) {
      int do_raw = 0;
      unsigned int addr = 0;

      if (wfr == '+')
        if (PACKET_TYPE_wfr == UTIL_extract_MP_type (buffer))
          ipos = status_wfr (buffer->packet.mpp.mini_packet, ipos);
      if (lfdr == '+')
        if (PACKET_TYPE_lfdr == UTIL_extract_MP_type (buffer))
          ipos = status_lfdr (buffer->packet.mpp.mini_packet, ipos);
      if (wbr == '+')
        if (PACKET_TYPE_wbr == UTIL_extract_MP_type (buffer))
          ipos = status_wbr (buffer->packet.mpp.mini_packet, ipos);
      if (mro == '+')
        if (PACKET_TYPE_mro == UTIL_extract_MP_type (buffer)) {
          do_raw = 6;
          ipos = status_mro (buffer->packet.mpp.mini_packet, ipos, mp_len);
        }
      if (mrod == '+')
        if (PACKET_TYPE_mro == UTIL_extract_MP_type (buffer)) {
          do_raw = -6;
          ipos = addr_mro (buffer->packet.mpp.mini_packet, mp_len);
        }
      if (mfr == '+')
        if (PACKET_TYPE_mfr == UTIL_extract_MP_type (buffer))
          ipos = status_mfr (buffer->packet.mpp.mini_packet, ipos);
      switch (do_raw) {
       default:
         ipos = mro_disp (buffer, width,
                          ipos, do_raw,
                          addr_mro (buffer->packet.mpp.mini_packet, mp_len));
         break;
       case 0:
         ipos = raw_disp (buffer, width, ipos);
         break;
      }
    }
    u_refresh ();
    if (delay)
      sleep (delay);
    icnt += 1;
    ilen = _getbuffer_MP (buffer, input, eof_flag, &rti_start, &rti_count);
  }
  return (0);
}
