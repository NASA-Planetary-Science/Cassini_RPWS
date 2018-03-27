
/*
 * dsp8.c
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <term.h>
#include <strings.h>

/* Cassini Stuff */
#include <rtiu.h>
#include <util.h>
#include <utilt.h>
#include <utilf.h>
#include <fg.h>

                        /* #include <Util.h> *//*
                         * Wow one char different, really guys? 
                         */

static char version[] = { " CASSINI time dump  (DSP8) 8.0" };
struct MP_buffer *buffer;
char *UTIL_extract_packet_type ();
static int format_flag = 1;
static int vert;

int _getbuffer_MP (struct MP_buffer *buffer,
                   FILE * input, int iflg, int *rti_start, int *rti_count)
{
  int ilen;
  int rti1, rti2;

  if (!*rti_count)
    return 0;
  if (*rti_count > 0)
    *rti_count = *rti_count - 1;
  while (1) {
    ilen = UTIL_getbuffer_MP (buffer, input, iflg);
    rti1 = (UTIL_extract_TIME ((struct CDS_buffer *) buffer) & 0x1FF8) << 3;
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
  long page_length = 0;
  long cds[6];
  int eof_flag = 1;
  int icnt = 0;
  int skip_count = 0;
  int rti_start = 0;
  int rti_count = -1;
  int ilen = 1;
  int delay = 0;
  int first_time = 1;

  char temp[16][64];
  char title[16][64] = {
    " workstation tag A |",             /*  0 */
    " workstation tag B |",             /*  1 */
    "   minipacket SCET     |",         /*  2 */
    "   using gmtime        |",         /*  3 */
    "   epoch  |",                      /*  4 */
    "    chdo type 94 SCLK/SCET   |",   /*  5 */
    "CDS seq|",                         /*  6 */
    "  minipacket SCET   ",             /*  7 */
    "  day millisec",                   /*  8 */
    "   mPkt SCLK  ",                   /*  9 */
    "CDS SCLK b|",                      /* 10 */
    "CDS SCLK e|",                      /* 11 */
    " event ck |",                      /* 12 */
    " CDS clk  |",                      /* 13 */
    " ",                                /* 14 */
    " "                                 /* 15 */
  };
  char temp0[1024];

  FILE *input = stdin;
  char fname[128];

  time_t pkt_time, pkt_etime, pkt_epoc;
  int epoch;
  struct tm *pkt_tm;
  struct tm *pkt_ev;
  struct event_time *evt_tim;
  struct event_clock evt_clk;

  int seq = 0;
  int ws_a = 0;
  int ws_b = 0;
  int tnew = 0;
  int told = 0;
  int tepoch = 0;
  int sclkscet = 0;
  int cdsclk = 0;

  fg_flags (argc, argv);
  if (fg_flag ("get")) {
    fg_flagx ("+getraw", "");
    fg_flagx ("+getcds", "");
  }
  if (fg_flag ("help") || fg_flag ("h")) {
    fprintf (stdout, "%s   HELP SCREEN\n", version);
    fprintf (stdout, "\n");
    fprintf (stdout, "     +getcds     get data from current file\n");
    fprintf (stdout, "     +get          synonym\n");
    fprintf (stdout, "     +eof        quit at eof\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "     +ws_a       Workstation tag A\n");
    fprintf (stdout, "     +ws_b       Workstation tag B\n");
    fprintf (stdout, "     +tnew       sclk/scet derived\n");
    fprintf (stdout, "     +told       epoch derived\n");
    fprintf (stdout, "     +epoch      epoch value\n");
    fprintf (stdout, "     +sclkscet   sclk/scet hex\n");
    fprintf (stdout, "     +seq        CDS record sequence\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "     +all        everything\n");
    fprintf (stdout, "                 use '-' to supress above items\n");
    fprintf (stdout, "\n");
    return 0;
  }

  buffer = malloc (65536 + 1024);

  if (fg_flag ("all") == '+') {
    seq = 1;
    ws_a = 1;
    ws_b = 1;
    tnew = 1;
    told = 1;
    tepoch = 1;
    sclkscet = 1;
    cdsclk = 1;
  }
  if (fg_flag ("seq") == '+')
    seq = 1;
  if (fg_flag ("ws_a") == '+')
    ws_a = 1;
  if (fg_flag ("ws_b") == '+')
    ws_b = 1;
  if (fg_flag ("tnew") == '+')
    tnew = 1;
  if (fg_flag ("told") == '+')
    told = 1;
  if (fg_flag ("epoch") == '+')
    tepoch = 1;
  if (fg_flag ("sclkscet") == '+')
    sclkscet = 1;
  if (fg_flag ("cdsclk") == '+')
    cdsclk = 1;

  if (fg_flag ("seq") == '-')
    seq = 0;
  if (fg_flag ("ws_a") == '-')
    ws_a = 0;
  if (fg_flag ("ws_b") == '-')
    ws_b = 0;
  if (fg_flag ("tnew") == '-')
    tnew = 0;
  if (fg_flag ("told") == '-')
    told = 0;
  if (fg_flag ("epoch") == '-')
    tepoch = 0;
  if (fg_flag ("sclkscet") == '-')
    sclkscet = 0;
  if (fg_flag ("cdsclk") == '-')
    cdsclk = 0;

  if (fg_flag ("eof") == '+')
    eof_flag = 0;
  if (fg_flag ("find")) {
    input = UTIL_find_open (fg_flagc ("find"), "rb");
    if (input)
      fprintf (stderr, "dsp8 find: %s\n", UTIL_find_name ());
    else {
      fprintf (stderr, "dsp8 find: file not found: %s\n", fg_flagc ("find"));
      return 0;
    }
  }

  if (fg_flag ("getcds") == '+') {
    strcpy (fname, UTIL_filename (FILE_RAW, FILE_LOCATE_DEFAULT));
    fprintf (stderr, "dsp8 source: %s\n", fname);
    input = fopen (fname, "rb");
    if (!fname)
      exit (0);
  }

  sleep (3);
  fprintf (stdout, "%s \n", version);

  ilen = _getbuffer_MP (buffer, input, eof_flag, &rti_start, &rti_count);
  while (ilen > 0) {
    vert = 0;
    UTIL_extract_CDS ((struct CDS_buffer *) buffer, cds);
    sprintf (temp[0],
             " %2X%02X%2X%02X %02X %02X %02X |",
             buffer->packet.ws_tag.A.year[0],
             buffer->packet.ws_tag.A.year[1],
             buffer->packet.ws_tag.A.doy[0],
             buffer->packet.ws_tag.A.doy[1],
             buffer->packet.ws_tag.A.hour,
             buffer->packet.ws_tag.A.minute, buffer->packet.ws_tag.A.second);
    sprintf (temp[1],
             " %2X%02X%2X%02X %02X %02X %02X |",
             buffer->packet.ws_tag.B.year[0],
             buffer->packet.ws_tag.B.year[1],
             buffer->packet.ws_tag.B.doy[0],
             buffer->packet.ws_tag.B.doy[1],
             buffer->packet.ws_tag.B.hour,
             buffer->packet.ws_tag.B.minute, buffer->packet.ws_tag.B.second);

    epoch = (buffer->packet.cds_tag.epoch[0] << 24) |
      (buffer->packet.cds_tag.epoch[1] << 16) |
      (buffer->packet.cds_tag.epoch[2] << 8) |
      (buffer->packet.cds_tag.epoch[3] << 0);

    pkt_etime = UTIL_event_time (buffer, 0);
    sprintf (temp[12], " %08X |", pkt_etime);
    pkt_epoc = (int) pkt_etime + (int) epoch;
    evt_clk.seconds = pkt_etime;
    evt_clk.fine = UTIL_extract_MP_RTI (buffer) << 5;
    if (epoch) {
      evt_tim = UTIL_event_scet ((struct MP_buffer *) buffer, evt_clk);
      pkt_ev = UTIL_event_scet_tm (*evt_tim, 0);
      sprintf (temp[2], " %04d-%03dT%2.2d:%2.2d:%2.2d.%3.3d |",
               pkt_ev->tm_year + 1900,
               pkt_ev->tm_yday + 1,
               pkt_ev->tm_hour,
               pkt_ev->tm_min, pkt_ev->tm_sec, evt_tim->milliseconds % 1000);
      sprintf (temp[7], " %04X,%03d_%05d.%3d",
               (int) evt_tim->days,
               ((int) evt_tim->milliseconds / 1000) / 86400,
               ((int) evt_tim->milliseconds / 1000) % 86400,
               evt_tim->milliseconds % 1000);
      sprintf (temp[8], " %04X,%08X", evt_tim->days, evt_tim->milliseconds);
    } else {
      temp[2][0] = NULL;
      temp[7][0] = NULL;
      temp[8][0] = NULL;
    }
    sprintf (temp[9], " %08X,%04X", evt_clk.seconds, evt_clk.fine);
    pkt_ev = gmtime (&pkt_epoc);

    sprintf (temp[3], " %04d-%03d-%2.2d:%2.2d:%2.2d.%3.3d |",
             pkt_ev->tm_year + 1900,
             pkt_ev->tm_yday + 1,
             pkt_ev->tm_hour,
             pkt_ev->tm_min,
             pkt_ev->tm_sec, UTIL_extract_MP_RTI (buffer) * 125);
    sprintf (temp[4], " %8X |", epoch);

    sprintf (temp[5], " %04X%04X.%04X-%04X.%04X%04X |",
             buffer->packet.chdo_ancillary.type_94.cas_sclk[0],
             buffer->packet.chdo_ancillary.type_94.cas_sclk[1],
             buffer->packet.chdo_ancillary.type_94.cas_sclk[2],
             buffer->packet.chdo_ancillary.type_94.scet[0],
             buffer->packet.chdo_ancillary.type_94.scet[1],
             buffer->packet.chdo_ancillary.type_94.scet[2]);

    sprintf (temp[6], " %5X |",
             UTIL_extract_CDS_sequence ((struct CDS_buffer *) buffer)
      );
    sprintf (temp[10], " %02X%02X%02X%02X |",
             buffer->packet.cds_tag.begin[0],
             buffer->packet.cds_tag.begin[1],
             buffer->packet.cds_tag.begin[2],
             buffer->packet.cds_tag.begin[3]);
    sprintf (temp[11], " %02X%02X%02X%02X |",
             buffer->packet.cds_tag.end[0],
             buffer->packet.cds_tag.end[1],
             buffer->packet.cds_tag.end[2], buffer->packet.cds_tag.end[3]);
    sprintf (temp[13], " %02X%02X%02X%02X |",
             buffer->packet.cds.header[6],
             buffer->packet.cds.header[9],
             buffer->packet.cds.header[8], buffer->packet.cds.header[11]);

    /*
     *         * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *
     *         Print the selected data titles
     */
    page_length -= 1;
    if (page_length <= 0) {
      temp0[0] = NULL;
      if (seq)
        strcat (temp0, title[6]);
      if (ws_a)
        strcat (temp0, title[0]);
      if (ws_b)
        strcat (temp0, title[1]);
      if (sclkscet)
        if (epoch)
          strcat (temp0, title[8]);
      if (sclkscet)
        strcat (temp0, title[9]);
      if (tnew)
        if (epoch)
          strcat (temp0, title[2]);
      if (told)
        strcat (temp0, title[3]);
      if (tepoch)
        strcat (temp0, title[4]);
      if (sclkscet)
        strcat (temp0, title[5]);
      if (cdsclk)
        strcat (temp0, title[10]);
      if (cdsclk)
        strcat (temp0, title[11]);
      if (cdsclk)
        strcat (temp0, title[12]);
      if (cdsclk)
        strcat (temp0, title[13]);
      page_length = 60;
      fprintf (stdout, "\n%s\n", temp0);
    }

    /*
     *         * * * * * * * * * * * * * * * * * * * * * * * * * * *
     *
     *         Print the selected data...
     */
    temp0[0] = NULL;
    if (seq)
      strcat (temp0, temp[6]);
    if (ws_a)
      strcat (temp0, temp[0]);
    if (ws_b)
      strcat (temp0, temp[1]);
    if (sclkscet) {
      strcat (temp0, temp[8]);
      strcat (temp0, temp[9]);
    }
    if (tnew)
      strcat (temp0, temp[2]);
    if (told)
      strcat (temp0, temp[3]);
    if (tepoch)
      strcat (temp0, temp[4]);
    if (sclkscet)
      strcat (temp0, temp[5]);
    if (cdsclk)
      strcat (temp0, temp[10]);
    if (cdsclk)
      strcat (temp0, temp[11]);
    if (cdsclk)
      strcat (temp0, temp[12]);
    if (cdsclk)
      strcat (temp0, temp[13]);

    fprintf (stdout, "%s\n", temp0);
    if (delay)
      sleep (delay);
    icnt += 1;
    ilen = _getbuffer_MP (buffer, input, eof_flag, &rti_start, &rti_count);
  }
  return 0;
}
