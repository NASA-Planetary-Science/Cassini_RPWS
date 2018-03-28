
/*	mpb.c	*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include "fg.h"
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "utilmp.h"

#define MAX_NUMBER_OF_CDS_SOURCES 16

extern char UTIL_get_mp_version[];
static char *version = { "V3.1" };
static char hrs_flag = 0;
static char lrs_flag = 0;
static char report_flag = 0;

struct source
{
  char mnemonic[8];
  int mask;
  int fenum;
} src[] = {
"STIM", 0x0, FILE_STIM,
    "MFR", 0x1, FILE_MFR,
    "HFR", 0x2, FILE_HFR,
    "KRONOS", 0x2, FILE_KRONOS,
    "LP", 0x4, FILE_LP,
    "LFDR", 0x7, FILE_LFDR,
    "WFR", 0x8, FILE_WFR,
    "DUST", 0xB, FILE_DUST,
    "MRO", 0xD, FILE_MRO,
    "WBR", 0xE, FILE_WBR,
    "FILL", 0xF, 0,
    "stim", 0x0, FILE_STIM,
    "mfr", 0x1, FILE_MFR,
    "hfr", 0x2, FILE_HFR,
    "kronos", 0x2, FILE_KRONOS,
    "lp", 0x4, FILE_LP,
    "lfdr", 0x7, FILE_LFDR,
    "wfr", 0x8, FILE_WFR,
    "dust", 0xB, FILE_DUST,
    "mro", 0xD, FILE_MRO, "wbr", 0xE, FILE_WBR, "fill", 0xF, 0, "", -1, -1};
static F_enum = FILE_MP;

void mp_report (char dir, char flag)
{
  int count[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  count[dir & 0x03] += 1;
  if (flag != '+')
    return;
  if (dir == 0)
    fprintf (stderr, ">");
  if (dir == 1)
    fprintf (stderr, "<");
  if (dir == 2)
    fprintf (stderr, ".");
  if (dir == 3)
    fprintf (stderr, "r");
  return;
}
int arg_source (char *source)
{
  static int i, j;

  j = 0;
  while (1) {
    if (!strcmp (source, src[j].mnemonic)) {
      F_enum = src[j].fenum;
      return src[j].mask;
    }
    j = j + 1;
    if (src[j].mask == -1)
      return -1;
  }
  return -1;
}
int getbuffer_CDS (struct CDS_buffer *c_buffer, FILE * file, int flag,
                   int debug)
{
  int i;
  int ilen;
  int active = 1;
  long cds_header[6];
  long type;
  unsigned long cds_time;

  while (active) {
    ilen = UTIL_getbuffer_CDS (c_buffer, file, flag);
    mp_report (1, report_flag);
    if (ilen > 0)
      active = 0;
    UTIL_extract_CDS (c_buffer, cds_header);
    cds_time = UTIL_extract_TIME (c_buffer);
    if (debug)
      fprintf (stderr, "cds_time2 %X\n", cds_time);
    type = UTIL_extract_CDS_type (c_buffer);
    if (lrs_flag) {
      if (cds_header[0] == 0x0A81)
        active = 1;
      if (cds_header[0] == 0x0AA0)
        active = 1;
    }
    if (hrs_flag) {
      if (cds_header[0] == 0x0AA3)
        active = 1;
      if (cds_header[0] == 0x0A82)
        active = 1;
    }
    if (0)
      fprintf (stderr, "active(%d), type(%8X), cds_header[0](%8X)\n",
               active, type, cds_header[0]);
  }
  return ilen;
}
int main (int argc, char *argv[])
{
  int file_mp = FILE_MP;
  int filter_source;
  int i, j, k, ilen;
  int local_flag = FILE_LOCATE_DEFAULT;
  int length;
  char eof_flag = 1;
  int dbg_flg = 0;
  int comp = 0;
  char string[128];
  char *fname;
  char src[256] = { "" };
  char fsrc[256] = { "" };
  FILE *input = stdin;
  FILE *otput = stdout;
  struct CDS_buffer c_buffer;
  struct MP_buffer m_buffer[MAX_NUMBER_OF_CDS_SOURCES];

  fg_flags (argc, argv);
  strcpy (src, fg_flagc ("filter"));
  filter_source = arg_source (src);

  src[0] = 0;
  strcpy (src, fg_flagc ("file"));
  if (src[0]) {
    char txt[256];
    int i;

    filter_source = arg_source (src);
    file_mp = F_enum;
    strcpy (txt, "+put");
    strcat (txt, UTIL_fmnemonic (file_mp));
    for (i = 0; i < strlen (txt); i++)
      txt[i] = tolower (txt[i]);
    fg_flagx (txt, "");
  }

  report_flag = fg_flag ("report");
  if (fg_flag ("comp") == '+')
    comp = 1;
  if (fg_flag ("eof") == '+')
    eof_flag = 0;
  if (fg_flag ("getcds"))
    fg_flagx ("+getraw", "");
  if (fg_flag ("get"))
    fg_flagx ("+getraw", "");
  if (fg_flag ("put"))
    fg_flagx ("+putmp", "");
  dbg_flg = fg_flag ("debug");

  if (fg_flag ("nohrs"))
    hrs_flag = 1;
  if (fg_flag ("nolrs"))
    lrs_flag = 1;

  if (fg_flag ("dump") == '+')
    UTIL_dumprc (local_flag);;
  if (fg_flag ("pipe")) {
    input = fopen (UTIL_filepipe (FILE_RAW), "rb");
    if (!input) {
      fprintf (stderr, "mp(b) ERROR, pipe not found");
      exit (0);
    }
  } else if (fg_flag ("find")) {
    input = UTIL_find_open (fg_flagc ("find"), "rb");
    if (input)
      fprintf (stderr, "mp(b) find: %s\n", UTIL_find_name ());
    else {
      fprintf (stderr, "mp(b) find: file not found: %s\n", fg_flagc ("find"));
      exit (0);
    }
  } else
    input = UTIL_FILEname (FILE_RAW, local_flag);
  if (input)
    fprintf (stderr, "mp(b) %s %s, source %s: %s\n",
             version,
             UTIL_get_mp_version,
             UTIL_fmnemonic (FILE_RAW), UTIL_fname (FILE_OLD));
  if (!input) {
    input = stdin;
    fprintf (stderr, "mp(b) %s %s, source stdin\n", version,
             UTIL_get_mp_version);
  }
  otput = UTIL_FILEnew (file_mp, local_flag);
  if (!otput)
    otput = stdout;
  fprintf (stderr, "mp(b) %s %s, destination %s: %s\n",
           version,
           UTIL_get_mp_version,
           UTIL_fmnemonic (file_mp), UTIL_fname (FILE_NEW)
    );
  if (fg_flag ("dump") == '+')
    UTIL_dumprc (local_flag);;

  if (fg_flag ("help") || fg_flag ("h")) {
    printf (" \n");
    printf (" \n");
    printf (" mpb %s %s - mini packet formatter\n", version,
            UTIL_get_mp_version);
    printf (" \n");
    printf ("      -filter MFR, LP, HFR, WFR, WBR, LFDR, DUST, STIM\n");
    printf ("      -file MFR, LP, HFR, WFR, WBR, LFDR, DUST, STIM, KRONOS\n");
    printf ("               (puts minipacket to dedicated file)\n");
    printf ("      -nohrs   supress HRS data\n");
    printf ("      -nolrs   supress LRS data\n");
    printf ("      -find fn find archive file (on CDROM)\n");
    printf ("      +report  indicate i/o activity\n");
    printf ("      +eof     stop at normal eof (non-realtime\n");
    printf ("      +pipe    pipe from %s\n", UTIL_filepipe (FILE_RAW));
    printf ("      +getcds  get current file\n");
    printf ("      +get       (synonym)\n");
    printf ("      +putmp   put mini packet file\n");
    printf ("      +put       (synonym)\n");
    printf ("      +comp    remove padding at end of record\n");
    printf ("      -local   use local .filerc\n");
    printf ("      help\n");
    exit (0);
    printf (" \n");
  }
  fprintf (stderr, "mp(b) %s %s, filter source(%4X)  report(%c) eof(%X)\n",
           version,
           UTIL_get_mp_version, filter_source, report_flag, fg_flag ("eof"));
  for (i = 0; i < MAX_NUMBER_OF_CDS_SOURCES; i++) {
    /*
     * zero out the buffers: 
     */
    memset (&m_buffer[i], 0, sizeof (m_buffer[i]));
  }
  ilen = getbuffer_CDS (&c_buffer, input, eof_flag, dbg_flg);
  mp_report (1, report_flag);
  j = REQUEST_DATA_FLAG;
  while (ilen > 0) {

    while (!UTIL_extract_CDS_length (&c_buffer)) {
      ilen = getbuffer_CDS (&c_buffer, input, 1, dbg_flg);
      mp_report (1, report_flag);
      if ((ilen == -1) || (ilen == 0))
        return 1;
    }

    i = UTIL_get_CDS_source_number (&c_buffer);
    if (i != -1) {
      j = UTIL_get_mp (&c_buffer, &m_buffer[i], i);
      mp_report (2, report_flag);

      if (j & MP_PACKET_COMPLETED_FLAG) {
        if ((UTIL_extract_MP_type (&m_buffer[i]) == filter_source) ||
            (filter_source == -1)) {
          m_buffer[i].packet.index.data_start =
            UTIL_extract_MP_length (&m_buffer[i]);
          m_buffer[i].packet.index.data_length =
            m_buffer[i].packet.index.data_start;
          if (comp) {
            length = 256 + 12 + 4;
            length += UTIL_extract_MP_length (&m_buffer[i]);
            k = UTIL_putbuffr2_MP (&m_buffer[i], otput, length);
            mp_report (0, report_flag);
          } else {
            k = UTIL_putbuffer_MP (&m_buffer[i], otput);
            mp_report (0, report_flag);
          }
          memset (&m_buffer[i], 0, sizeof (m_buffer[i]));
          if (!k) {
            sprintf (string, "lrp error");
            perror (string);
          }
        }
      }

    }

    if (j & REQUEST_DATA_FLAG) {
      ilen = getbuffer_CDS (&c_buffer, input, eof_flag, dbg_flg);
      mp_report (1, report_flag);
      if ((ilen == -1) || (ilen == 0))
        break;
    }

  }
  fprintf (stderr, "\n");
  return 1;
}
