#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>

 /**/
#include "rtiu.h"
#include "pcrtiu.h"
#include "fg.h"
#include "utilo.h"
#define CDS_minimum_length 32
struct CDS_buffer buffer;
int eof = -1;
main (int argc, char *argv[])
{
  char *host;
  char string[128];
  int len;
  int status;
  int active = 1;
  int length;
  FILE *fileout;
  FILE *msgs = NULL;
  int error_count = 0;
  int error_MAX = 30000;
  int counter = 0;
  int write_flag;
  int record_length;
  int i;
  int logno = 0;
  long epoch = 0;

  fg_flags (argc, argv);
  epoch = utilo_epoch (NULL);
  buffer.f_length = (&buffer.r_length - &buffer.f_length) * 4;
  buffer.r_length = buffer.f_length;
  buffer.record_type = DATA_RTIU;
  buffer.record_type = DATA_BIU_SIM;
  fileout = stdout;
  if (fg_flag ("debug"))
    msgs = fopen ("pcpipe.log", "wa");
  if (!fileout) {
    perror ("bad open");
    exit (0);
  }

  host = pcrtiu_hostname (NULL, argc, argv);
  fprintf (stderr, "Host: %s\n", host);
  status = pcrtiu_init_socket (host, 1025);
  if (status < 0) {
    sprintf (string, ": pcrtiu_init_socket error %d\n", status);
    perror (string);
  }

  while (active) {
    buffer.status = pcrtiu_getCDS (buffer.packet.rtiu.header);
    buffer.packet.cds_tag.epoch[0] = ((unsigned long) epoch >> 24) & 0xFF;
    buffer.packet.cds_tag.epoch[1] = ((unsigned long) epoch >> 16) & 0xFF;
    buffer.packet.cds_tag.epoch[2] = ((unsigned long) epoch >> 8) & 0xFF;
    buffer.packet.cds_tag.epoch[3] = ((unsigned long) epoch >> 0) & 0xFF;
    if (buffer.status > 0) {
      error_count = 0;
      memset (buffer.packet.trailer.data, 0x00, 256);
      buffer.record_type = UTIL_long_to_MSB (DATA_RTIU);
      UTIL_get_time (&(buffer.packet.ws_tag.A));
      write_flag = 0;
      if (!strcmp (argv[1], "-passall"))
        write_flag = 1;
      if (buffer.status >= CDS_minimum_length)
        if (UTIL_extract_CDS_length (buffer) >= CDS_minimum_length)
          write_flag = 1;
      record_length =
        buffer.packet.rtiu.header[2] | (buffer.packet.rtiu.header[3] << 8);
      record_length += 256;
      record_length += 12;
      if (msgs) {
        fprintf (msgs, "%4d: ", logno++);
        fprintf (msgs, "  SA%3d", buffer.packet.rtiu.header[13]);
        fprintf (msgs, "  rl%5d", record_length);
        fprintf (msgs, "  \n        ");
        for (i = 0; i < 16; i++)
          fprintf (msgs, " %2.2X", buffer.packet.rtiu.header[i]);
        fprintf (msgs, "  \n        ");
        for (i = 0; i < 16; i++)
          fprintf (msgs, " %2.2X", buffer.packet.cds.header[i]);
        fprintf (msgs, "  \n");
        fflush (msgs);
      }
      switch (buffer.packet.rtiu.header[13]) {
       default:
         buffer.record_type = DATA_RTIU;
         break;
       case BIU_sub_address_nomcmd:
         buffer.record_type = DATA_RTIU_nomcmd;
         break;
       case BIU_sub_address_fpcmd:
         buffer.record_type = DATA_RTIU_fpcmd;
         break;
       case BIU_sub_address_crtcmd:
         buffer.record_type = DATA_RTIU_crtcmd;
         break;
       case BIU_sub_address_telem:
         buffer.record_type = DATA_RTIU_telem;
         break;
       case BIU_sub_address_hsk:
         buffer.record_type = DATA_RTIU_hsk;
         break;
       case BIU_sub_address_biust:
         buffer.record_type = DATA_RTIU_biust;
         break;
       case BIU_sub_address_ancil:
         buffer.record_type = DATA_RTIU_ancil;
         break;
      }
      if (write_flag)
        if (1)
          len = UTIL_putbuffr2_CDS (&buffer, fileout, record_length);
        else
          len =
            UTIL_putbuffr2_CDS (&buffer, fileout,
                                sizeof (struct CDS_buffer) - 4);
    } else {
      switch (buffer.status) {
       case -2:
       case -3:
         active = 0;
         break;
       case -4:
         sleep (5);
         break;
       case 0:
         pcrtiu_close_socket ();
         break;
       case -1:
       default:
         if (error_count++ > error_MAX) {
           active = 0;
           break;
         }
      }
    }
  }
  len = fwrite (&eof, 4, 1, fileout);
  fclose (fileout);
  fclose (msgs);
}
