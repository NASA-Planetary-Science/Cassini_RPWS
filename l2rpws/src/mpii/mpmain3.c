
/*	mpb.c	*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>

#include <fg.h>

#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilmp.h>

#include "mpii3.h"

#define MAX_NUMBER_OF_CDS_SOURCES 16

int Minimum_Buffer_Size = 0;
int Minimum_Buffer_Pad = 0;
int pool_size = 128;

unsigned int sclk_stop = 0xFFFFFFFF;
unsigned int sclk_span = 0;

static char *version = { "mpii 3.9b" };
struct source
{
  char mnemonic[8];
  int mask;
  int fenum;
} src[] = {
	"STIM", 1 << 0x0, FILE_STIM,
	"MFR", 1 << 0x1, FILE_MFR,
	"HFR", 1 << 0x2, FILE_HFR,
	"KRONOS", 1 << 0x2, FILE_KRONOS,
	"LP", 1 << 0x4, FILE_LP,
	"LFDR", 1 << 0x7, FILE_LFDR,
	"WFR", 1 << 0x8, FILE_WFR,
	"DUST", 1 << 0xB, FILE_DUST,
    "MRO", 1 << 0xD, FILE_MRO,
    "WBR", 1 << 0xE, FILE_WBR,
    "FILL", 1 << 0xF, 0,
    "stim", 1 << 0x0, FILE_STIM,
    "mfr", 1 << 0x1, FILE_MFR,
    "hfr", 1 << 0x2, FILE_HFR,
    "kronos", 1 << 0x2, FILE_KRONOS,
    "lp", 1 << 0x4, FILE_LP,
    "lfdr", 1 << 0x7, FILE_LFDR,
    "wfr", 1 << 0x8, FILE_WFR,
    "dust", 1 << 0xB, FILE_DUST,
    "mro", 1 << 0xD, FILE_MRO,
    "wbr", 1 << 0xE, FILE_WBR, 
	 "fill", 1 << 0xF, 0, "", -1, -1
};
static int F_enum = FILE_MP;

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
  /* return -1; */
}
void mp_report (int dir, char flag)
{
  int count[4] = { 0, 0, 0, 0 };
  count[dir & 0x03] += 1;
  if (flag != '+')
    return;
  if (dir == 0)
    fprintf (stderr, ">");
  if (dir == 1)
    fprintf (stderr, "<");
  return;
}
int main (int argc, char *argv[])
{
  int file_mp = FILE_MP;
  int filter_source;
  int i, j, k, ilen;
  int local_flag = FILE_LOCATE_DEFAULT;
  int length;
  char report_flag = 0;
  int eof_flag = UTIL_GET_BLOCKING;
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
    eof_flag = UTIL_GET_NON_BLOCKING;
  if (fg_flag ("eof") == '-')
    eof_flag = UTIL_GET_NON_BLOCKING;
  if (fg_flag ("getcds"))
    fg_flagx ("+getraw", "");
  if (fg_flag ("get"))
    fg_flagx ("+getraw", "");
  if (fg_flag ("put"))
    fg_flagx ("+putmp", "");

  if (fg_flag ("ddebug"))
    dbg_flg |= 1;
  if (fg_flag ("mdebug"))
    dbg_flg |= 2;
  if (fg_flag ("cdebug"))
    dbg_flg |= 4;
  if (fg_flag ("qdebug"))
    dbg_flg |= 8;

  if (fg_flag ("pool")) {
    pool_size = fg_int ("pool", 16);
    if (pool_size < 16)
      pool_size = 16;
  }

  if (fg_flag ("dump") == '+')
    UTIL_dumprc (local_flag);;
  if (fg_flag ("pipe")) {
    input = fopen (UTIL_filepipe (FILE_RAW), "rb");
    if (!input) {
      fprintf (stderr, "mpii ERROR, pipe not found\n");
      exit (0);
    }
  } else if (fg_flag ("find")) {
    input = UTIL_find_open (fg_flagc ("find"), "rb");
    if (input)
      fprintf (stderr, "mpii find: %s\n", UTIL_find_name ());
    else {
      fprintf (stderr, "mpii find: file not found: %s\n", fg_flagc ("find"));
      exit (0);
    }
  } else
    input = UTIL_FILEname (FILE_RAW, local_flag);
  if (input)
    fprintf (stderr, "mpii source %s: %s\n",
             UTIL_fmnemonic (FILE_RAW), UTIL_fname (FILE_OLD));
  if (!input) {
    input = stdin;
    fprintf (stderr, "mpii source stdin\n");
  }
  otput = UTIL_FILEnew (file_mp, local_flag);
  if (!otput)
    otput = stdout;
  fprintf (stderr, "mpii destination %s: %s\n",
           UTIL_fmnemonic (file_mp), UTIL_fname (FILE_NEW)
    );
  if (fg_flag ("dump") == '+')
    UTIL_dumprc (local_flag);;

  if (fg_flag ("mppad") == '-')
    Minimum_Buffer_Pad = fg_int ("mppad", Minimum_Buffer_Pad);
  if (fg_flag ("mpsize") == '-')
    Minimum_Buffer_Size = fg_int ("mpsize", Minimum_Buffer_Size);

  if (fg_flag ("sclk_span")) {
    char *temp;

    temp = fg_flagc ("sclk_span");
    sclk_span = strtol (temp, NULL, 0);
    fprintf (stderr, "SCLK_SPAN %8X\n", sclk_span);
  }
  if (fg_flag ("sclk_stop")) {
    char *temp;

    temp = fg_flagc ("sclk_stop");
    sclk_stop = strtol (temp, NULL, 0);
    fprintf (stderr, "SCLK_STOP %8X\n", sclk_stop);
  }

  if (fg_flag ("help") || fg_flag ("h")) {
    printf (" \n");
    printf (" \n");
    printf (" %s - mini packet formatter\n", version);
    printf (" \n");
    printf ("      -file        "
            "MFR, LP, HFR, WFR, WBR, LFDR, DUST, STIM, KRONOS\n");
    printf ("                     (puts minipacket to dedicated file)\n");
    printf ("      -find fn     find archive file (on CDROM)\n");
    printf ("      -filter      MFR, LP, HFR, WFR, WBR, LFDR, DUST, STIM\n");
    printf ("      +report      indicate i/o activity\n");
    printf ("      +eof         stop at normal eof (non-realtime\n");
    printf ("      +pipe        pipe from %s\n", UTIL_filepipe (FILE_RAW));
    printf ("      +getcds      get current file\n");
    printf ("      +get           (synonym)\n");
    printf ("      +putmp       put mini packet file\n");
    printf ("      +put           (synonym)\n");
    printf ("      -local       use local .filerc\n");
    printf ("      -pool   %3d  uSort pool size\n", pool_size);
    printf ("                     0 to supress microsort\n");
    printf ("                     32 or greates to eliminate duplicates\n");
    printf ("      -mpsize %3d  minumum size of MP buffer emitted\n",
            Minimum_Buffer_Size);
    printf ("      -mppad  %3d  MP padding (at end of MP)\n",
            Minimum_Buffer_Pad);
    printf ("      \n");
    printf ("      sclk_stop x  stop data after sclk_stop\n");
    printf ("      sclk_span x  stop data after start-time + sclk_span\n");
    printf ("      \n");
    printf ("      -ddebug      debug discards    (ddebug.mp2)\n");
    printf ("      -mdebug      debug mp records  (mdebug.mp2)\n");
    printf ("      -cdebug      debug cds records (cdebug.mp2)\n");
    printf ("      -qdebug      debug queueing    (qdebug.mp2)\n");
    printf ("      -help\n");
    exit (0);
    printf (" \n");
  }
  fprintf (stderr, "filter source(%4X)  report(%c) eof(%X) pool(%d)\n",
           filter_source, report_flag, eof_flag, pool_size);


  mpii (input, otput, filter_source, pool_size, eof_flag, dbg_flg);
  fclose (input);
  fclose (otput);
}
