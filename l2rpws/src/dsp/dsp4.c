#include <SpiceUsr.h>

/*
 * dsp4.c
 */
#include <curses.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <term.h>
#include <strings.h>


/* Cassini Stuff */
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <fg.h>


/*****************************************************************************/

static char *title = { " CASSINI CDS data dump  (DSP4) 4.17" };

/* See, this is why the code needs a maintainer... 
char *leapfile = { "/home/tfa/cassini/naif/naif0007.tls" };
char *sclkfile = { "/home/tfa/cassini/naif/cas00075.tsc" };
*/

struct CDS_buffer *buffer;
char *UTIL_extract_packet_type ();
static int format_flag = 1;
static int lost_flag = 0;
static int vert;
static int hexflag = 1;

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
char *u_cr (char *s, int y)
{
  static int iy = 1;

  if (format_flag)
    clrtoeol ();
  else if (iy != y) {
    iy = y;
    printf ("\n");

#ifdef FLUSH
    fflush (stdout);
    /**/
#endif
  }
  return s;
}
void u_print0 (int y, int x, char *s)
{
  if (format_flag) {
    mvprintw (y, x, s);
    clrtoeol ();
  } else {
    printf (u_cr (s, y));

#ifdef FLUSH
    fflush (stdout);
    /**/
#endif
  }
}
void u_print1 (int y, int x, char *s, int a1)
{
  if (format_flag) {
    mvprintw (y, x, s, a1);
    clrtoeol ();
  } else
    printf (u_cr (s, y), a1);
}
void raw_disp (struct CDS_buffer *buffer, int width, int flag)
{
  int index, i, j;

  if (UTIL_extract_CDS_type (buffer) > 0)
    if (flag)
      index = UTIL_extract_CDS_length (buffer) + 7;
    else
      index = UTIL_extract_CDS_length (buffer) - 5;
  else {
    u_print0 (vert, 0, "BIU default table in CDS packet ???");
    index = UTIL_extract_RTIU_length (buffer) - 44;
    vert = vert + 1;
  }
  for (i = 0; i < (index / width); i++) {
    u_print1 (vert, 0, "%4.4X: ", i * width);
    for (j = 0; j < width; j++) {
      if (flag)
        u_print1 (vert, j * 3 + 6, "%2.2X ",
                  buffer->packet.cds.header[i * width + j]);
      else
        u_print1 (vert, j * 3 + 6, "%2.2X ",
                  buffer->packet.rpws.data[i * width + j]);
    }
    vert = vert + 1;
  }

  if (index % width) {
    u_print1 (vert, 0, "%4.4X: ", i * width);
    for (j = 0; j < (index % width); j++)
      if (flag)
        u_print1 (vert, j * 3 + 6, "%2.2X ",
                  buffer->packet.cds.header[i * width + j]);
      else
        u_print1 (vert, j * 3 + 6, "%2.2X ",
                  buffer->packet.rpws.data[i * width + j]);
    vert = vert + 1;
  }
  u_print0 (vert, 1, "      ");
  for (i = 1; i < width * 3; i = i + 3)
    u_print0 (vert, i + 5, "-- ");
  vert = vert + 1;
  u_print0 (vert, 1, " ");
  return;
}
void chdo_dump (unsigned char *buffer, int width, int index, char *text)
{
  int i, j;
  char temp[256];

  sprintf (temp, "CHDO Header, %s, length:%d", text, index);
  u_print0 (vert++, 1, temp);
  for (i = 0; i < (index / width); i++) {
    u_print1 (vert, 0, "%4.4X: ", i * width);
    for (j = 0; j < width; j++) {
      u_print1 (vert, j * 3 + 6, "%2.2X ", buffer[i * width + j]);
    }
    vert = vert + 1;
  }

  if (index % width) {
    u_print1 (vert, 0, "%4.4X: ", i * width);
    for (j = 0; j < (index % width); j++)
      u_print1 (vert, j * 3 + 6, "%2.2X ", buffer[i * width + j]);
    vert = vert + 1;
  }
  return;
}
float chdo_float (unsigned char *si)
{
  union
  {
    unsigned char s[4];
    float f;
  } t;

  t.s[0] = si[0];
  t.s[1] = si[1];
  t.s[2] = si[2];
  t.s[3] = si[3];
  return t.f;
}
char *chdo_dump_antenna (unsigned char ant)
{
  int i;
  unsigned char antt;
  static char *antenna[] = { "26M    ",
    "34M STD",
    "34M hef",
    "Unused ",
    "70M    ",
    "34M BWG",
    "34M HSB",
    "Unused "
  };
  static char temp[65];

  antt = ant;
  temp[0] = 0;
  for (i = 0; i < 8; i++) {
    if (antt & 0x80) {
      strcat (temp, antenna[i]);
    }
    antt = antt << 1;
  }
  return temp;                          /* removed % */
}
char *chdo_dump_flags (unsigned short array, int max)
{
  static char *flag = { "A " };
  static char *result = { "                " };
  unsigned short temp;
  int i;

  temp = array;
  result[0] = 0;
  for (i = 0; i < max - 1; i++) {
    if (temp & 0x8000) {
      flag[0] = i + 'A';
      strcat (result, flag);
    }
    temp = temp << 1;
  }
  return result;
}
void chdo_dump_type_92 (struct CHDO_type_92 *buffer)
{
  int i, j;
  char temp[256];
  char *pub_text = { "      " };
  static char *source[] = { "N/A   ",
    "Rtr A ",
    "Rtr B ",
    "WB Sw ",
    "IDR T ",
    "DSN-GIF",
    "CDA Sp ",
    "SFDU T ",
    "DTS VC ",
    "CDA By ",
    "UNIX By",
    "error  ",
    "error  ",
    "error  ",
    "error  ",
    "error  "
  };
  static char *decode_status[] = { "undecoded",
    "N/A      ",
    "invalid  ",
    "valid    ",
    "corrected",
    "error    ",
    "error    ",
    "error    "
  };
  static char *decode_method[] = { "off",
    "R/S"
  };

  u_print0 (vert++, 1,
            "CHDO Secondary Header type 92 dump ---------**********");
  sprintf (temp, "%4.4X          chdo_type=%d", buffer->chdo_type,
           buffer->chdo_type);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%4.4X          chdo_length=%d",
           buffer->chdo_length, buffer->chdo_length);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%2.2X  %2.2X        originator  last_modifier  ",
           buffer->originator, buffer->last_modifier);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%2.2X  %2.2X        scft_id=%d  data_source=%d",
           buffer->scft_id,
           buffer->data_source, buffer->scft_id, buffer->data_source);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%4.4X          status_flags=(%s)",
           buffer->status_flags, chdo_dump_flags (buffer->status_flags, 16));
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%4.4X.%4.4X%4.4X ert",
           buffer->ert[0], buffer->ert[1], buffer->ert[2]);
  u_print0 (vert++, 1, temp);
  sprintf (temp,
           "%2.2X  %2.2X        rs_codeword_status  frame_extract_count",
           buffer->rs_codeword_status, buffer->frame_extract_count);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%4.4X%4.4X      dsn_record_seq",
           buffer->dsn_record_seq[0], buffer->dsn_record_seq[1]);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%2.2X  %2.2X        bet fly", buffer->bet, buffer->fly);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%2.2X  %2.2X        decode_status=(%s) decode_method=(%s)",
           buffer->decode_status,
           buffer->decode_method,
           decode_status[buffer->decode_status & 0x07],
           decode_method[buffer->decode_method & 0x01]);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%2.2X  %2.2X        sync_flags=(%s)    pn_errors",
           buffer->sync_flags, buffer->pn_errors,
           chdo_dump_flags (buffer->sync_flags, 8));
  u_print0 (vert++, 1, temp);
  sprintf (temp,
           "%2.2X  %2.2X        virtual_channel_id  virtual_frame_count",
           buffer->virtual_channel_id, buffer->virtual_frame_count);
  u_print0 (vert++, 1, temp);
  sprintf (temp,
           "%2.2X  %2.2X        frame_hdr_error_flag=(%s)  rs_decode_errors",
           buffer->frame_hdr_error_flag, buffer->rs_decode_errors,
           chdo_dump_flags (buffer->frame_hdr_error_flag, 8));
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%2.2X   %c        frequency band",
           buffer->frequency_band, buffer->frequency_band);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%8.2f      bit_rate", chdo_float (buffer->bit_rate));
  u_print0 (vert++, 1, temp);

  sprintf (temp, "%8.2f      snt system noise temp Kelvins",
           chdo_float (buffer->snt));
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%8.2f      ssnr symbol SNR", chdo_float (buffer->ssnr));
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%8.2f      signal_level dBm",
           chdo_float (buffer->signal_level));
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%2.2X  %2.2X        antennas receivers",
           buffer->antennas, buffer->receivers);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%2.2X  %2.2X        master_antenna=(%s)  master_receiver",
           buffer->master_antenna,
           buffer->master_receiver,
           chdo_dump_antenna (buffer->master_antenna));
  u_print0 (vert++, 1, temp);

  sprintf (temp, "%2.2X  %2.2X        dtm_group tlm_channel",
           buffer->dtm_group, buffer->tlm_channel);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%4.4X          lock_status", buffer->lock_status);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%2.2X  %2.2X        version  build",
           buffer->version, buffer->build);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%2.2X  %2.2X        orig_source=(%s)  curr_source=(%s)",
           buffer->orig_source, buffer->curr_source,
           source[buffer->orig_source & 0x0F],
           source[buffer->curr_source & 0x0F]);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%4.4X.%4.4X%4.4X rct ",
           buffer->rct[0], buffer->rct[1], buffer->rct[2]);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%4.4X          anomaly_flags=(%s)",
           buffer->anomaly_flags,
           chdo_dump_flags (buffer->anomaly_flags, 16));
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%4.4X          lock_count", buffer->lock_count);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%4.4X          lrn  logical record number", buffer->lrn);
  u_print0 (vert++, 1, temp);

  if ((buffer->pub[0] >> 8) & 0xFF)
    pub_text[0] = (buffer->pub[0] >> 8) & 0xFF;
  if ((buffer->pub[0] >> 0) & 0xFF)
    pub_text[1] = (buffer->pub[0] >> 0) & 0xFF;
  if ((buffer->pub[1] >> 8) & 0xFF)
    pub_text[2] = (buffer->pub[1] >> 8) & 0xFF;
  if ((buffer->pub[1] >> 0) & 0xFF)
    pub_text[3] = (buffer->pub[1] >> 0) & 0xFF;
  if ((buffer->pub[2] >> 8) & 0xFF)
    pub_text[4] = (buffer->pub[2] >> 8) & 0xFF;
  if ((buffer->pub[2] >> 0) & 0xFF)
    pub_text[5] = (buffer->pub[2] >> 0) & 0xFF;

  sprintf (temp, " %s       pub   %2.2X %2.2X %2.2X %2.2X %2.2X %2.2X ",
           pub_text,
           buffer->pub[0] >> 8, buffer->pub[0] & 0xFF,
           buffer->pub[1] >> 8, buffer->pub[1] & 0xFF,
           buffer->pub[2] >> 8, buffer->pub[2] & 0xFF);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%4.4X          block_v_rcvrs", buffer->block_v_rcvrs);
  u_print0 (vert++, 1, temp);

  return;
}

void chdo_dump_type_94 (struct CHDO_type_94 *buffer)
{
  int i, j;
  char temp[256];

  u_print0 (vert++, 1,
            "CHDO Tertiary Header type 94 dump ----------**********");
  sprintf (temp, "%4.4X          chdo_type=%d", buffer->chdo_type,
           buffer->chdo_type);
  u_print0 (vert++, 1, temp);
  sprintf (temp, "%4.4X          chdo_length=%d",
           buffer->chdo_length, buffer->chdo_length);
  u_print0 (vert++, 1, temp);

  sprintf (temp, "%4.4X          cds_error_flags=(%s)",
           buffer->cds_error_flags,
           chdo_dump_flags (buffer->cds_error_flags, 16));
  u_print0 (vert++, 1, temp);

  sprintf (temp, "%4.4X          source_pkt_seq_count=%d",
           buffer->source_pkt_seq_count, buffer->source_pkt_seq_count);
  u_print0 (vert++, 1, temp);

  sprintf (temp, "%4.4X          non_fill_length=%d",
           buffer->non_fill_length, buffer->non_fill_length);
  u_print0 (vert++, 1, temp);

  sprintf (temp, "%4.4X          packet_apid", buffer->packet_apid);
  u_print0 (vert++, 1, temp);

  sprintf (temp, "%4.4X          superpkt_seq_cnt=%d",
           buffer->superpkt_seq_cnt, buffer->superpkt_seq_cnt);
  u_print0 (vert++, 1, temp);

  sprintf (temp, "%4.4X%4.4X.%4.4X cas_sclk",
           buffer->cas_sclk[0], buffer->cas_sclk[1], buffer->cas_sclk[2]);
  u_print0 (vert++, 1, temp);

  sprintf (temp, "%4.4X.%4.4X%4.4X scet",
           buffer->scet[0], buffer->scet[1], buffer->scet[2]);
  /**/ u_print0 (vert++, 1, temp);
  /**/
    sprintf (temp, "%2.2X  %2.2X        probe_rs_errors  scet_flags=(%s)",
             buffer->probe_rs_errors,
             buffer->scet_flags, chdo_dump_flags (buffer->scet_flags, 8));
  u_print0 (vert++, 1, temp);

  return;
}

#define RATE 17
static int dsp4_delta;
static int dsp4_last = 0;
static float dsp4_rate[RATE] = { RATE * 0 };
int dsp4_pkt_delta (struct CDS_buffer *buffer)
{
  static int previous = 0;
  static int dsp4_offset;
  int now;

  now = UTIL_extract_TIME (buffer);
  dsp4_delta = now - previous;
  previous = now;
  if (dsp4_delta) {
    dsp4_offset = dsp4_offset + 1;
    dsp4_offset = dsp4_offset % RATE;
    dsp4_rate[dsp4_offset] = 1. / (float) dsp4_delta;
    dsp4_last = dsp4_rate[dsp4_offset];
  } else
    dsp4_rate[dsp4_offset] += 1.;
  return dsp4_delta;
}
float dsp4_avg (void)
{
  int i;
  float avg = 0.;

  for (i = 0; i < RATE; i++) {
    avg = avg + dsp4_rate[i];
  }
  return (avg - dsp4_last) / ((float) RATE - 1.0);
}
char *dsp4_pkt_sec (struct CDS_buffer *buffer)
{
  static int count;
  static int index = 0;
  static char temp[256] = { "        " };
  float bit_rate;

  sprintf (temp, "");
  count += 1;
  index += 1;
  bit_rate = dsp4_avg () * 7616.0;
  if (dsp4_pkt_delta (buffer)) {
    if ((dsp4_delta) == 1) {
      sprintf (temp, " %d Pkt/sec %6.2fbs", count, bit_rate);
      if (bit_rate > 10.0) {
        sprintf (temp, " %d Pkt/sec %5.1fb", count, bit_rate);
      }
      if (bit_rate > 100.0) {
        sprintf (temp, " %d Pkt/sec %4.0fb", count, bit_rate);
      }
      if (bit_rate > 1000.0) {
        sprintf (temp, " %d Pkt/sec %6.2fKb", count,
                 (float) count * 952. * 8. / 1000.);
      }
      if (bit_rate > 10000.0) {
        sprintf (temp, " %d Pkt/sec %5.1fKb", count,
                 (float) count * 952. * 8. / 1000.);
      }
      if (bit_rate > 100000.0) {
        sprintf (temp, " %d Pkt/sec %4.0fKb", count,
                 (float) count * 952. * 8. / 1000.);
      }
    } else
      sprintf (temp, " %d Pkt/%d sec", count, dsp4_delta);
    count = 0;
  }
  return temp;                          /* removed & */
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
char *chdo_text (struct CDS_buffer *buffer)
{
  short temp;
  static int count[4] = { 4 * 0 };
  static char *ctemp = { "                                " };
  static char *fill[] = { "         ",
    " RS ERROR",
    "CDS ERROR",
    "FILL	 DATA"
  };
  int index = 0;

  temp = buffer->packet.chdo_ancillary.type_94.cds_error_flags;
  if (temp & 0x0400)
    index = 3;
  if (temp & 0xC1E0)
    index = 2;
  if (temp & 0x3A00)
    index = 1;
  count[index] += 1;
  sprintf (ctemp, "%s %3d %3d %3d", fill[index], count[1], count[2],
           count[3]);
  return ctemp;
}
int error_flag (struct CDS_buffer *buffer)
{
  short temp;
  int index = 0;

  temp = buffer->packet.chdo_ancillary.type_94.cds_error_flags;
  if (temp & 0x0400)
    index = 3;
  if (temp & 0xC1E0)
    index = 2;
  if (temp & 0x3A00)
    index = 1;
  return index;
}
int main (int argc, char *argv[])
{
  static int width = 16;
  static int delay = 0;
  int flag, second, icnt;
  int ilen;
  long cds[6];
  int i;
  int eof_flag = 1;
  int hdr_flag = 0;
  int chdo_flag = 0;
  int schdo_flag = 0;
  int tchdo_flag = 0;
  int xchdo_flag = 0;
  int h_flag = 0;
  int thdr_flag = 0;
  int shdr_flag = 0;
  int rhdr_flag = 0;
  int dhdr_flag = 0;
  int bcd_flag = 0;
  unsigned char string[128];
  int rti_start = 0;
  int rti_count = -1;
  int skip_count = 0;
  FILE *input = stdin;
  char fname[128];
  char temp[256];
  char temp2[128];
  char *ctemp;
  char *corrupt = { "Corrupted Packet" };
  char *corrupt_brief = { "Crpt Pkt" };
  char stemp[64];
  int max = 0;
  int last_seq[8] = { -1, -1, -1, -1, -1, -1, -1, -1 };
  int last_tim[8] = { -1, -1, -1, -1, -1, -1, -1, -1 };
  char splat = ' ';
  char spuke[3] = { "  " };
  char Spuke = ' ';
  char tpuke = ' ';
  int sum_index;
  int sum_temp;
  int sum_lost[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  int sum_dupe1[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  int sum_dupe[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  int sum_rs[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  int sum_fill[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  int sum_packets[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  int expected_length[8] = { 0, 952, 952, 192, 0, 0, 0, 0 };
  char* metafile = NULL;

  fg_flags (argc, argv);
  if (fg_flag ("get")) {
    fg_flagx ("+getraw", "");
    fg_flagx ("+getcds", "");
  }
  if (fg_flag ("help") || fg_flag ("h")) {
    fprintf (stdout, "%s   HELP SCREEN\n", title);
    fprintf (stdout, "\n");
    fprintf (stdout, "     +find fn    find archive data\n");
    fprintf (stdout, "     +getcds     get data from current file\n");
    fprintf (stdout, "     +get          synonym\n");
    fprintf (stdout, "     -width nn   hexdump width (bytes)\n");
    fprintf (stdout, "     -delay nn   nn second delay between update\n");
    fprintf (stdout, "     -rti 0xXX   RTI to start display\n");
    fprintf (stdout, "     -count nn   display nn records\n");
    fprintf (stdout, "     -skip nn    skip nn records\n");
    fprintf (stdout, "     -hex        suppress hex dump\n");
    fprintf (stdout, "     -hdr        headers only\n");
    fprintf (stdout, "     -dhdr       header debugging\n");
    fprintf (stdout, "     -shdr       headers only w/bad sequence\n");
    fprintf (stdout, "     -thdr       headers only data loss analysis\n");
    fprintf (stdout, "     -rhdr       headers only \n");
    fprintf (stdout, "     +chdo       display the chdo time tags\n");
    fprintf (stdout, "     +schdo      Secondary CHDO header (92) dump)\n");
    fprintf (stdout, "     +tchdo      Tertiary CHDO header (94) dump)\n");
    fprintf (stdout,
             "     +hchdo      display the chdo time tags (hexadecimal)\n");
    fprintf (stdout, "     +mchdo      display the chdo time tags (mixed)\n");
    fprintf (stdout, "     +xchdo      display CHDO hex dump\n");
    fprintf (stdout, "     +bcd        display bcd time tags\n");
    fprintf (stdout, "     -format     supress screen addressing\n");
    fprintf (stdout,
             "     -lost       Change \"-hdr\" format to scan for LOST RTI\n");
    fprintf (stdout,
             "     -time           Add day=hh:mm:ss.mmm time field\n");
	 
	 if( getenv("CAS_TIME_KERNELS") != NULL)
		fprintf (stdout, "     -kernel xx     Specify spice metakernel file (%s)\n",
                getenv("CAS_TIME_KERNELS"));
	 else
	    fprintf (stdout, "     -kernel xx     Specify spice metakernel file (no default found)\n");
	 
    fprintf (stdout, "     +eof        quit at eof\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "\n");
    return 0;
  }

  buffer = malloc (32768 + 1024);
  width = fg_int ("width", 16);
  delay = fg_int ("delay", 0);
  if (fg_flag ("mchdo") == '+')
    chdo_flag = 3;
  if (fg_flag ("schdo") == '+')
    schdo_flag = 1;
  if (fg_flag ("tchdo") == '+')
    tchdo_flag = 1;
  if (fg_flag ("xchdo") == '+')
    xchdo_flag = 1;
  if (fg_flag ("hchdo") == '+')
    chdo_flag = 2;
  if (fg_flag ("chdo") == '+')
    chdo_flag = 1;
  if (fg_flag ("chdo") == '-')
    chdo_flag = 0;
  if (fg_flag ("eof") == '+')
    eof_flag = 0;
  if (fg_flag ("bcd") == '+')
    bcd_flag = 1;


  if (fg_flag ("get"))
    fg_flagx ("getcds", "");
  if (fg_flag ("kernel")) {
    metafile = malloc (256);
    strcpy (metafile, fg_flagc ("kernel"));
    fprintf (stdout, "leap second filename String: %s\n", metafile);
    fflush (stdout);
  }
  if (fg_flag ("lost") == '-') {
    lost_flag = 1;
	 
	 if( getenv("CAS_TIME_KERNELS") == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		return 13;
  	}
  
	furnsh_c(getenv("CAS_TIME_KERNELS"));
  }
  if (fg_flag ("time") == '-') {
    lost_flag |= 2;
  }
  if (fg_flag ("hex") == '-')
    hexflag = 0;
  if (fg_flag ("format") == '-')
    format_flag = 0;
  if (fg_flag ("hdr") == '-') {
    format_flag = 0;
    hdr_flag = 1;
  }
  if (fg_flag ("shdr") == '-') {
    format_flag = 0;
    hdr_flag = 1;
    shdr_flag = 1;
  }
  if (fg_flag ("thdr") == '-') {
    format_flag = 0;
    hdr_flag = 1;
    shdr_flag = 1;
    thdr_flag = 1;
  }
  if (fg_flag ("rhdr") == '-') {
    format_flag = 0;
    hdr_flag = 1;
    rhdr_flag = 1;
  }
  if (fg_flag ("dhdr") == '-') {
    dhdr_flag = 1;
  }
  rti_start = fg_int ("rti", rti_start);
  rti_count = fg_int ("count", rti_count);
  skip_count = fg_int ("skip", skip_count);
  if (fg_flag ("find")) {
    input = UTIL_find_open (fg_flagc ("find"), "rb");
    if (input)
      fprintf (stderr, "dsp4 find: %s\n", UTIL_find_name ());
    else {
      fprintf (stderr, "dsp4 find: file not found: %s\n", fg_flagc ("find"));
      return 0;
    }
  }
  if (fg_flag ("getcds") == '+') {
    strcpy (fname, UTIL_filename (FILE_RAW, FILE_LOCATE_DEFAULT));
    fprintf (stderr, "dsp4 source: %s\n", fname);
    input = fopen (fname, "rb");
    if (!fname)
      return 0;
  }
  fprintf (stderr, "dsp4 rti_start: %d (%d)\n", rti_start, rti_count);

  sleep (3);
  u_init ();
  u_print0 (0, 10, " negative length for clear-text");
  u_print0 (1, 10, title);
  for (icnt = -1; icnt < skip_count; icnt++)
    ilen = _getbuffer_CDS (buffer, input, eof_flag, &rti_start, &rti_count);
  while (ilen > 0) {
    vert = 0;
    UTIL_extract_CDS (buffer, cds);
    sprintf (temp,
             "BCD Time Tag %2X%02X %2X%02X %2X %2X %2X -"
             " %2X%02X %2X%02X %2X %2X %2X\n",
             buffer->packet.ws_tag.A.year[0],
             buffer->packet.ws_tag.A.year[1],
             buffer->packet.ws_tag.A.doy[0],
             buffer->packet.ws_tag.A.doy[1],
             buffer->packet.ws_tag.A.hour,
             buffer->packet.ws_tag.A.minute,
             buffer->packet.ws_tag.A.second,
             buffer->packet.ws_tag.B.year[0],
             buffer->packet.ws_tag.B.year[1],
             buffer->packet.ws_tag.B.doy[0],
             buffer->packet.ws_tag.B.doy[1],
             buffer->packet.ws_tag.B.hour,
             buffer->packet.ws_tag.B.minute, buffer->packet.ws_tag.B.second);

    if (bcd_flag)
      u_print0 (vert++, 0, temp);

    if (hdr_flag) {
      splat = ' ';
      strcpy (spuke, "  ");
      Spuke = ' ';
      tpuke = ' ';
      if (UTIL_extract_TIME (buffer) <
          last_tim[UTIL_extract_packet_sindex (buffer)])
        tpuke = 'E';
      if (UTIL_extract_CDS_sequence (buffer) !=
          last_seq[UTIL_extract_packet_sindex (buffer)]) {
        splat = '*';
        strcpy (spuke, "Er");
        if (abs (UTIL_extract_CDS_sequence (buffer) -
                 last_seq[UTIL_extract_packet_sindex (buffer)]) > 100) {
          Spuke = 'I';
        }
      }
      if (cds[3] & 0xFF00) {
        splat = 'E';
        strcpy (spuke, "Er");
      }

      ctemp = UTIL_extract_packet_type (buffer);
      if (!ctemp)
        ctemp = corrupt;
      if (lost_flag) {
        static int id = -82;
        static char sclk_temp[256];
        static double et;
        static char *format = { "D" };
        static int prec = 3;
        static char utcout[256];

        sprintf (sclk_temp, "1/%d:%d", UTIL_extract_TIME (buffer),
                 UTIL_extract_RTI (buffer) << 5);
        scs2e_c(id, sclk_temp, &et);
        et2utc_c(et, format, prec, 32, utcout);
        sprintf (temp, " C %5d %c%sSeq %5d ",
                 icnt, Spuke, spuke, UTIL_extract_CDS_sequence (buffer));
        sprintf (stemp, "CDS %4.4X %4.4X %4.4X %4.4X %4.4X %4.4X  ",
                 cds[0], cds[1], cds[2], cds[3], cds[4], cds[5]);
        strcat (temp, stemp);

        sprintf (stemp, "%c", tpuke);
        strcat (temp, stemp);

        sprintf (stemp, "Time 0x%8.4X.%X:%X(%X) ",
                 UTIL_extract_TIME (buffer),
                 UTIL_extract_RTI (buffer),
                 UTIL_extract_subRTI (buffer), UTIL_extract_Qflag (buffer));
        strcat (temp, stemp);

        utcout[8] = 'T';
        strcpy (&utcout[9], &utcout[12]);
        utcout[22] = 0;
        if (lost_flag & 0x02) {
          strcat (temp, utcout);
        }

        sprintf (stemp, "%c  ", splat);
        strcat (temp, stemp);

        sprintf (stemp, "%s ", UTIL_extract_packet_brief (buffer));
        strcat (temp, stemp);

        if (!(lost_flag & 0x02)) {
          utcout[4] = 0;
          utcout[8] = 0;
          utcout[11] = 0;
          sprintf (stemp, "t%s%s%s00.r00", &utcout[0], &utcout[5], &utcout[9]); /* LOST RTI */
          strcat (temp, stemp);
        }
      } else {
        sprintf (temp,
                 " Cycle %5d %c%sSeq %5d ",
                 icnt, Spuke, spuke, UTIL_extract_CDS_sequence (buffer));

        sprintf (stemp, "CDS %4.4X %4.4X %4.4X %4.4X %4.4X %4.4X  ",
                 cds[0], cds[1], cds[2], cds[3], cds[4], cds[5]);
        strcat (temp, stemp);

        sprintf (stemp, "%c", tpuke);
        strcat (temp, stemp);

        sprintf (stemp, "Time 0x%8.4X.%X:%X(%X) ",
                 UTIL_extract_TIME (buffer),
                 UTIL_extract_RTI (buffer),
                 UTIL_extract_subRTI (buffer), UTIL_extract_Qflag (buffer));
        strcat (temp, stemp);

        sprintf (stemp, "%c  %s %s", splat, ctemp, dsp4_pkt_sec (buffer));
        /**/ strcat (temp, stemp);
      }
      sum_index = UTIL_extract_packet_sindex (buffer);
      sum_temp = UTIL_extract_CDS_sequence (buffer) - last_seq[sum_index];

      ctemp = UTIL_extract_packet_brief (buffer);
      if (!ctemp)
        ctemp = corrupt_brief;
      if (thdr_flag)
        sprintf (temp,
                 " Cycle %5d %c%sSeq %5d(%5d)  "
                 "%c"
                 "Time 0x%8.4X.%X:%X  "
                 "%c  %s %s",
                 icnt, Spuke, spuke,
                 UTIL_extract_CDS_sequence (buffer),
                 last_seq[UTIL_extract_packet_sindex (buffer)],
                 tpuke,
                 UTIL_extract_TIME (buffer),
                 UTIL_extract_RTI (buffer),
                 UTIL_extract_subRTI (buffer),
                 splat, ctemp, dsp4_pkt_sec (buffer));

      if (sum_temp > 0)
        if (last_seq[UTIL_extract_packet_sindex (buffer)] > 0)
          sum_lost[sum_index] += sum_temp;
      if (sum_temp < 0)
        sum_dupe[sum_index] += 1;
      if (sum_temp == -1)
        sum_dupe1[sum_index] += 1;
      sum_packets[sum_index] += 1;
      if (buffer->packet.chdo_ancillary.type_94.non_fill_length !=
          expected_length[sum_index])
        sum_fill[sum_index] += 1;
      if (buffer->packet.chdo_ancillary.type_92.rs_decode_errors)
        sum_rs[sum_index] += 1;
      if ((shdr_flag == 1) && splat != ' ') {
        sprintf (temp2, " %5d, {%5d}",
                 UTIL_extract_CDS_sequence (buffer) -
                 last_seq[UTIL_extract_packet_sindex (buffer)], sum_temp);
        strcat (temp, temp2);
      }
      strcat (temp, "\n");
      h_flag = 0;
      if ((shdr_flag == 0) && (rhdr_flag == 0))
        h_flag = 1;
      if ((shdr_flag == 1) && splat != ' ')
        h_flag = 1;
      if ((rhdr_flag == 1) && dsp4_delta)
        h_flag = 1;
      if (dhdr_flag == 1)
        h_flag = 1;

      if (h_flag)
        u_print0 (vert, 0, temp);

      last_seq[UTIL_extract_packet_sindex (buffer)] =
        UTIL_extract_CDS_sequence (buffer) + 1;
      last_tim[UTIL_extract_packet_sindex (buffer)] =
        UTIL_extract_TIME (buffer);
    } else {
      u_print1 (vert++, 50, " Cycle %d", icnt);
      ctemp = UTIL_extract_packet_type (buffer);
      if (!ctemp)
        ctemp = corrupt;
      sprintf (temp,
               " %2.2X %s  (.record_type)%4d    ",
               UTIL_extract_CDS_type (buffer), ctemp, buffer->record_type);
      u_print0 (vert++, 1, temp);
      switch (UTIL_extract_CDS_type (buffer)) {
       default:
         ctemp = "   ";
         break;
       case 0x01:
       case 0x20:
         ctemp = "LRS";
         break;
       case 0x02:
       case 0x23:
         ctemp = "HRS";
         break;
       case 0x10:
       case 0x13:
       case 0x15:
         ctemp = "HSK";
         break;
      }
      sprintf (temp, " %s Seq%6.2d",
               ctemp, UTIL_extract_CDS_sequence (buffer));
      u_print0 (vert, 1, temp);
      if (UTIL_extract_CDS_length (buffer) > max)
        max = UTIL_extract_CDS_length (buffer);
      sprintf (temp,
               " Len%5.2d/%5.2d", UTIL_extract_CDS_length (buffer), max);
      u_print0 (vert, 15, temp);
      sprintf (temp,
               " Time 0x%8.4X.%X:%X",
               UTIL_extract_TIME (buffer),
               UTIL_extract_RTI (buffer), UTIL_extract_subRTI (buffer)
        );
      u_print0 (vert, 32, temp);
      sprintf (temp, " %d.%d:%d",
               UTIL_extract_TIME (buffer),
               UTIL_extract_RTI (buffer), UTIL_extract_subRTI (buffer)
        );
      u_print0 (vert++, 50, temp);

      if (chdo_flag) {
        static char *format[5][2] = {
          " ", " ",
          " CHDO_Time   seconds fine       days millisec       days millisec       days millisec",
          "     SCLK-%10d.%3.3d  SCET-%5.5d:%8.8d  ERT-%5.5d:%8.8d  RCT-%5.5d:%8.8d",
          " CHDO_Time seconds fine       days millisec        days millisec        days millisec",
          "   SCLK-0x%8.8X.%2.2X  SCET-0x%4.4X:%8.8X  ERT-0x%4.4X:%8.8X  RCT-0x%4.4X:%8.8X",
          " CHDO_Time seconds fine      days millisec       days millisec       days millisec",
          "   SCLK-0x%8.8X.%2.2X  SCET-%5.5d:%8.8d  ERT-%5.5d:%8.8d  RCT-%5.5d:%8.8d",
          " ", " "
        };
        sprintf (temp, format[chdo_flag][1],
                 buffer->packet.chdo_tag.sclk.seconds,
                 (int) buffer->packet.chdo_tag.sclk.fine,
                 (int) buffer->packet.chdo_tag.scet.days,
                 buffer->packet.chdo_tag.scet.milliseconds,
                 (int) buffer->packet.chdo_tag.ert.days,
                 buffer->packet.chdo_tag.ert.milliseconds,
                 (int) buffer->packet.chdo_tag.rct.days,
                 buffer->packet.chdo_tag.rct.milliseconds);
        u_print0 (vert++, 0, format[chdo_flag][0]);
        u_print0 (vert++, 0, temp);
      /**/}
      sprintf (temp, " CDS HDR   %4.4X %4.4X %4.4X %4.4X %4.4X %4.4X  %s",
               cds[0], cds[1], cds[2], cds[3], cds[4], cds[5],
               chdo_text (buffer));
      u_print0 (vert++, 0, temp);
      if (xchdo_flag) {
        chdo_dump ((unsigned char *) &buffer->packet.chdo_ancillary.type_92,
                   width, sizeof (struct CHDO_type_92), "Secondary, type 92");
        chdo_dump ((unsigned char *) &buffer->packet.chdo_ancillary.type_94,
                   width, sizeof (struct CHDO_type_94), "Tertiary, type 94");
        u_print0 (vert++, 1, "Instrument Data following header");
      }
      if (schdo_flag) {
        chdo_dump_type_92 (&buffer->packet.chdo_ancillary.type_92);
      /**/}
      if (tchdo_flag) {
        chdo_dump_type_94 (&buffer->packet.chdo_ancillary.type_94);
      /**/}
      if (UTIL_extract_CDS_length (buffer))
        if (hexflag)
          raw_disp (buffer, width, schdo_flag + tchdo_flag);
    }
    u_refresh ();
    if (delay)
      sleep (delay);
    icnt += 1;
    ilen = _getbuffer_CDS (buffer, input, eof_flag, &rti_start, &rti_count);
  }
  if (thdr_flag) {
    fprintf (stdout, "\nSummary\n");
    fprintf (stdout,
             "                                               LRS   HRS   HSK\n");
    fprintf (stdout,
             "      Processed packets (packets processed): %5.1d %5.1d %5.1d\n",
             sum_packets[1], sum_packets[2], sum_packets[3]);
    fprintf (stdout,
             "    Duplicate packets (sequence regression): %5d %5d %5d\n",
             sum_dupe[1], sum_dupe[2], sum_dupe[3]);
    fprintf (stdout,
             "      Dupe packets (sequence regression -1): %5d %5d %5d\n",
             sum_dupe1[1], sum_dupe1[2], sum_dupe1[3]);
    fprintf (stdout,
             "    Calculated lost packets (sequence jump): %5d %5d %5d\n",
             sum_lost[1], sum_lost[2], sum_lost[3]);
    fprintf (stdout,
             "            Short packets (fill length > 0): %5d %5d %5d\n",
             sum_fill[1], sum_fill[2], sum_fill[3]);
    fprintf (stdout,
             "     Reed Solomon error packets (R/S count): %5d %5d %5d\n",
             sum_rs[1], sum_rs[2], sum_rs[3]);
  }
  return 0;
}
