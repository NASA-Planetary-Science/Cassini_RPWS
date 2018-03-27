
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
#include "venus.h"

extern char *sort;
extern char *comment;

static char Version[] = { "2.2" };
struct PATCH
{
  int index;
  int offset;
  int data_length;
  int pattern_length;
  int patch_length;
  int eof_pattern;
  char pattern[32];
  char patch[32];
};
struct PATCH patch[] = { 0, 5, 128, 0, 20, 0x5A, "", "",
  1, 5, 128, 0, 20, 0x5A, "", "",
  2, 5, 128, 0, 20, 0x5A, "", "",

  3, 5, 139, 2, 20, 0x5A,
  0xD0, 0x83, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,

  0x00, 0x87, 0x0F, 0x05, 0x1C, 0x01, 0x01, 0x01,
  0x04, 0xCC, 0x8C, 0xCC, 0x05, 0x0C, 0x0E, 0x6D,
  0x08, 0x1E, 0x04, 0xAA, 0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

char *status_text (int valid, struct MP_buffer *buffer)
{
  char *text[] = { "Wrong MP - ",
    "Checksum Valid",
    "Checksum Invalid",
    "Unknown Status"
  };
  char temp[64];

  switch (valid) {
   case -1:
     strcpy (temp, text[0]);
     strcat (temp, UTIL_extract_MP_packet_type (buffer));
     return temp;
   case 0:
     return text[1];
   case 1:
     return text[2];
   default:
     return text[3];
  }
}
int main (int argc, char *argv[])
{
  FILE *input = stdin;
  FILE *otput = stdout;
  int eof_flag = UTIL_GET_NON_BLOCKING;
  int dbg_flag = 0;
  int mro_flag = 0;
  int cks_flag = 0;
  int rti_flag = 0;
  int ntime_flag = 1;
  int rep_flag = 0;
  int sort_flag = 0;
  int lost_flag = 0;
  int pattern_flag = 0;
  int cds_time = 0;
  int data_only_flag = 1;
  int strip_index = 3;
  int valid;
  int itime;
  int ilen, jlen;
  int jcnt[10] = { 10 * 0 };
  int count = 0;
  int i;
  struct MP_buffer *in_buffer, *out_buffer;
  struct DATA_RECORD *data_buffer;

  fg_flags (argc, argv);
  if (fg_flag ("help") || fg_flag ("h")) {
    printf (" \n");
    printf (" \n");
    printf (" venus %s w/%s, %s\n", Version, sort, comment);
  }
  if (fg_flag ("help")) {
    printf ("\t\t  This filter reformats MRO packets used to\n");
    printf ("\t\tdump the Venus observation data from memory.\n");
    printf ("\t\tThe MRO data is reformed into HFR packets with\n");
    printf ("\t\tredundant status information supplied by the\n");
    printf ("\t\tfilter.\n");
    printf ("\t\t\n");
    printf ("\t\t  Time information is also reformatted under\n");
    printf ("\t\tthe assumption that the MRO was performed\n");
    printf ("\t\twithin 18 hours of the observation.\n");
    printf ("\t\t  The first CDS timetag is altered to reflect\n");
    printf ("\t\tthe time at which the observation occured while\n");
    printf ("\t\tthe second CDS tag is left unaltered and will indicate\n");
    printf ("\t\tthe time the data was MRO'd from memory.\n");
    printf ("\t\t\n");
    printf ("\t\t  The -rti flag attemps to adjust the RTI that is\n");
    printf ("\t\tplaced into the minipacket.  This adjustment will,\n");
    printf ("\t\tif successful, refine the timetag to an accuracy\n");
    printf ("\t\tof 250mS (i.e. 2 RTI periods)\n");
    printf ("\t\t\n");
    printf ("\t\t  The -lost flag will attempt to recover lost packets.\n");
    printf ("\t\tCurrently, this is a virtually non-working function so\n");
    printf ("\t\tone should not attempt to use the -rti flag when the\n");
    printf ("\t\tdata contains gaps\n");
    printf ("\t\t\n");
    printf ("\t\t  Use the -data flag to eliminate non-HFR data from\n");
    printf
      ("\t\tthe output file/stream.  Otherwise extra MRO packets will\n");
    printf ("\t\tprobably appear in the output data.\n");
    printf ("\t\t\n");
    printf ("\t\t  A sort operation, is activated with the -sort flag.\n");
    printf ("\t\tThe MRO address information is used to place the records\n");
    printf ("\t\tinto the correct time-order (w/xsort is the MRO method,\n");
    printf ("\t\tw/vsort is the older method)\n");
    printf ("\t\t\n");
    printf
      ("\t\t  Note that it is preferred to make use of mpii to extract\n");
    printf ("\t\tminipackets (not the best way to handle real-time data)\n");
    printf ("\t\t\n");
    printf ("\tSuggested use\n");
    printf
      ("\t\tmpii +eof < FILE | venus -sort -data -rti -rep > kronos.???\n");
  }
  if (fg_flag ("help") || fg_flag ("h")) {
    printf ("\t\t\n");
    printf ("     -strip n\n");
    printf ("            0  zero fill\n");
    printf ("            3  Version 2.2 (default)\n");
    printf (" \n");
    printf ("     -ntime do NOT perform time correction\n");
    printf ("     -sort  perform time sort\n");
    printf ("     -data  data only flag (strips MRO/trash)\n");
    printf ("     -rti   fix the RTI in the minipacket\n");
    printf ("     -lost  fix lost minipackets to make -rti work\n");
    printf ("     -cds n place 'n' into the minipacket header\n");
    printf ("             CDS time field\n");
    printf ("     -patt  memory storage pattern\n");
    printf ("     -rep   report errors\n");
  }
  if (fg_flag ("h")) {
    printf ("     -help  detailed explanation of this routine\n");
  }
  if (fg_flag ("help")) {
    printf ("     -h     help synopsis\n");
  }
  if (fg_flag ("help") || fg_flag ("h")) {
    printf (" \n");
    exit (0);
  }
  if (fg_flag ("rep"))
    rep_flag = 1;
  if (fg_flag ("ntime"))
    ntime_flag = 0;
  if (fg_flag ("sort"))
    sort_flag = 1;
  if (fg_flag ("data"))
    data_only_flag = 0;
  if (fg_flag ("rti"))
    rti_flag = 1;
  if (fg_flag ("lost"))
    lost_flag = 1;
  if (fg_flag ("patt")) {
    pattern_flag = 1;
    sort_flag = 1;
  }
  cds_time = fg_int ("cds", 0);

  in_buffer = malloc (32768);
  out_buffer = malloc (32768);

  ilen = UTIL_getbuffer_MP (in_buffer, input, eof_flag);
  while (ilen > 0) {
    mro_flag = 0;
    if (in_buffer->record_type == DATA_MPP_MRO)
      mro_flag = 1;
    if (in_buffer->record_type == DATA_MPS_MRO)
      mro_flag = 1;
    if (in_buffer->record_type == DATA_MPC_MRO)
      mro_flag = 1;
    if (in_buffer->record_type == DATA_MPL_MRO)
      mro_flag = 1;
    if ((in_buffer->packet.mpp.mini_packet[0]) != (PACKET_TYPE_mro << 4))
      mro_flag = 0;
    if ((in_buffer->packet.mpp.mini_packet[1]) != 131)
      mro_flag = 0;
    cks_flag = chk3 (&in_buffer->packet.mpp.mini_packet[6]);
    /**/ valid = 1;
    if (!mro_flag)
      valid = 0;
    if (cks_flag)
      valid = 0;

    if (rep_flag)
      if (!valid)
        fprintf (stderr,
                 "count %4d valid %d mro_flag %d cks_flag %d %2.2X %2.2X %2.2X %2.2X %s\n",
                 count++, valid, mro_flag, cks_flag,
                 in_buffer->packet.mpp.mini_packet[0],
                 in_buffer->packet.mpp.mini_packet[1],
                 in_buffer->packet.mpp.mini_packet[2],
                 in_buffer->packet.mpp.mini_packet[3], status_text (cks_flag,
                                                                    in_buffer)
          );
    /**/ if (valid) {
      jlen = in_buffer->f_length;
      memcpy (out_buffer, in_buffer, 256 + 12);
      out_buffer->f_length = jlen + patch[strip_index].patch_length + 15 + 4;
      out_buffer->f_length &= 0x0000FFF0;
      out_buffer->f_length -= 4;

      /*
       *      1st. copy in the 5 header bytes:
       *              2 MP ID
       *              2 seconds (the RTI fiueld)
       *              1 segmedntation control
       */
      memcpy (&out_buffer->packet.mpp.mini_packet[0],
              &in_buffer->packet.mpp.mini_packet[6],
              patch[strip_index].offset);
      out_buffer->packet.mpp.mini_packet[1] = patch[strip_index].data_length;

      /*
       *      Next, copy in the fixed portion of the minipacket.
       *        this contains state information about the HFR
       *        that is identical in all packets.  
       */
      memcpy (&out_buffer->packet.mpp.mini_packet[patch[strip_index].offset],
              patch[strip_index].patch, patch[strip_index].patch_length);

      /*
       *      Now move the data from the venus packet to the
       *        correct position in the MP
       */
      memcpy (&out_buffer->packet.mpp.mini_packet[patch[strip_index].offset +
                                                  patch[strip_index].
                                                  patch_length],
              &in_buffer->packet.mpp.mini_packet[patch[strip_index].offset +
                                                 6],
              patch[strip_index].data_length);

      /*
       *      Now lets do some minipacket record type horsing around
       */
      out_buffer->record_type = DATA_MPC_HFR;

      /*
       *      Now lets fixup nay other minipacket stuff
       */
      out_buffer->packet.index.data_start =
        UTIL_extract_MP_length (out_buffer);
      out_buffer->packet.index.data_length =
        UTIL_extract_MP_length (out_buffer);

      /*
       *      Finally, place the EOF indicator, the 0x5A
       *        into the MP
       */
      out_buffer->packet.mpp.mini_packet[patch[strip_index].data_length + 2] =
        patch[strip_index].eof_pattern;
      for (i = 0; i < 32; i++)
        out_buffer->packet.mpp.mini_packet[patch[strip_index].data_length +
                                           3 + i] = 0x00;

      /*
       *      Now fix the time field...
       */
      if (ntime_flag)
        itime = time_fix (out_buffer, cds_time);

    } else {
      memcpy (out_buffer, in_buffer, in_buffer->f_length + 4);
    }
    if (data_only_flag)
      valid = 1;
    if (valid) {
      if (sort_flag)
        jlen = venus_enqueue (out_buffer, in_buffer, itime);
      else
        jlen = UTIL_putbuffr2_MP (out_buffer, otput, out_buffer->f_length);
    }
    memset (out_buffer, 0, 1024);
    itime = 0;
    ilen = UTIL_getbuffer_MP (in_buffer, input, eof_flag);
  }
  venus_array_dump ();
  if (sort_flag) {
    if (lost_flag) {
      /*
       * venus_discard(13); /*
       */
      pkt_fix (0);
    }
    if (rti_flag)
      rti_fix (0);
    data_buffer = venus_dequeue (1, 0);
    while (data_buffer) {
      if (pattern_flag)
        venus_mem_dump (data_buffer, stdout);
      else
        jlen =
          UTIL_putbuffr2_MP (data_buffer->buffer, otput,
                             data_buffer->buffer->f_length);
      data_buffer = venus_dequeue (0, 0);
    }
  }
  return 1;
}
