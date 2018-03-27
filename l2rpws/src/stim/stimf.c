
/*	stim.c	*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include "fg.h"
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "utilmp.h"

int stim_filter (struct MP_buffer *buffer, int id0, int id1, int debug)
{
  int status = 0;
  int check = 0;
  long type, length;
  unsigned tmpid, trtic, trtii, tseq;
  unsigned tid0, tid1;
  unsigned tday;

  type = UTIL_extract_MP_type (buffer);
  length = UTIL_extract_MP_length (buffer);
  if (type == PACKET_TYPE_stim)
    check += 1;
  if (length == 13)
    check += 1;

  if (check == 2)
    status = -1;

  tmpid = (buffer->packet.mpp.mini_packet[0] << 8) |
    (buffer->packet.mpp.mini_packet[1] << 0);
  trtic = (buffer->packet.mpp.mini_packet[3] << 8) |
    (buffer->packet.mpp.mini_packet[2] << 0);
  trtii = (buffer->packet.mpp.mini_packet[5] << 8) |
    (buffer->packet.mpp.mini_packet[4] << 0);
  tseq = (buffer->packet.mpp.mini_packet[7] << 8) |
    (buffer->packet.mpp.mini_packet[6] << 0);
  tid0 = (buffer->packet.mpp.mini_packet[9] << 8) |
    (buffer->packet.mpp.mini_packet[8] << 0);
  tid1 = (buffer->packet.mpp.mini_packet[11] << 8) |
    (buffer->packet.mpp.mini_packet[10] << 0);
  tday = (buffer->packet.mpp.mini_packet[15] << 8) |
    (buffer->packet.mpp.mini_packet[14] << 0);

  if (tid0 == id0)
    check += 1;
  else if (id0 == -1)
    check += 1;

  if (tid1 == id1)
    check += 1;
  else if (id1 == -1)
    check += 1;

  if (check == 4)
    status = 1;
  if (debug)
    if (status)
      fprintf (stderr,
               "stim: %4.4X %4.4X %4.4X s:%4.4d 0:%4.4d 1:%4.4d d:%4.4d\n",
               tmpid, trtii, trtic, tseq, tid0, tid1, tday);
  return status;
}

int main (int argc, char *argv[])
{
  int eof_flag = 1;
  int local_flag = 0;
  int file_mp = 0;
  int active = 0;
  int dbg_flag = 0;
  int id0, id1;
  int ilen;
  FILE *input = NULL;
  FILE *otput = NULL;
  struct MP_buffer *buffer;

  buffer = malloc (65536 + 1024);
  fg_flags (argc, argv);

  if (fg_flag ("eof") == '+')
    eof_flag = 0;
  if (fg_flag ("local") == '-')
    local_flag = 1;
  if (fg_flag ("debug") == '-')
    dbg_flag = 1;

  id0 = fg_int ("id0", -1);
  id1 = fg_int ("id1", -1);

  if (!input)
    if (fg_flag ("getmp"))
      input = UTIL_FILEname (FILE_MP, local_flag);
  if (!input)
    if (fg_flag ("getmpus"))
      input = UTIL_FILEname (FILE_MPUS, local_flag);

  if (!input)
    if (fg_flag ("find")) {
      input = UTIL_find_open (fg_flagc ("find"), "rb");
      if (input)
        fprintf (stderr, "stimf find: %s\n", UTIL_find_name ());
      else {
        fprintf (stderr, "stimf find: file not found: %s\n",
                 fg_flagc ("find"));
        exit (0);
      }
    }
  if (!input) {
    input = stdin;
    fprintf (stderr, "stimf source stdin\n");
  } else {
    fprintf (stderr, "stimf source %s: %s\n",
             UTIL_fmnemonic (FILE_RAW), UTIL_fname (FILE_OLD));
  }

  if (!otput)
    if (fg_flag ("putmp"))
      otput = UTIL_FILEnew (FILE_MP, local_flag);
  if (!otput)
    if (fg_flag ("putmpus"))
      input = UTIL_FILEnew (FILE_MPUS, local_flag);
  if (!otput)
    otput = stdout;
  else
    fprintf (stderr, "stimf destination: %s\n", UTIL_fname (FILE_NEW));

  fprintf (stderr, "stimf search: ID0:%i ID1:%i\n", id0, id1);
  if (fg_flag ("help") || fg_flag ("h")) {
    printf (" \n");
    printf (" \n");
    printf (" stimf 0.0 - stim packet filter\n");
    printf ("     (works best if you don't filter\n");
    printf ("       the stim packets out too soon)\n");
    printf (" \n");
    printf ("      -id0 nn  stim ID-0 field\n");
    printf ("      -id1 nn  stim ID-1 field\n");
    printf ("                  omit for wildcard\n");
    printf ("      -find fn find archive file (on CDROM)\n");
    printf ("      +eof     stop at normal eof (non-realtime\n");
    printf ("      +getmp   get minipacket file\n");
    printf ("      +putmp   put mini packet file\n");
    printf ("      +getmpus get unsegmented minipacket file\n");
    printf ("      +putmpus put unsegmented mini packet file\n");
    printf ("      -local   use local .filerc\n");
    printf ("      -debug   debug information\n");
    printf ("      -help/-h\n");
    exit (0);
    printf (" \n");
  }
  ilen = UTIL_getbuffer_MP (buffer, input, eof_flag);
  while (ilen > 0) {
    switch (stim_filter (buffer, id0, id1, dbg_flag)) {
     case 1:
       active = 1;
       break;
     case -1:
       active = 0;
       break;
     default:
       break;
    }

    if (active)
      UTIL_putbuffer_MP (buffer, otput);
    ilen = UTIL_getbuffer_MP (buffer, input, eof_flag);
  }
  fprintf (stderr, "\n");
  return 1;
}
