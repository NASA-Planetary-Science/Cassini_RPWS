#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>

 /*
  * chdopipe.c 
  */
#include <rtiu.h>
#include <fg.h>
#include <utilo.h>
#include <util.h>

#include "chdo.h"

#define CDS_minimum_length 32

FILE *dbg = NULL;

static char Version[] = { "V7.0" };
static int epoch_count = 0;
static int epoch_max = 0;
static int epoch_off = 0;
static long epoch = -1;

int chdo_exit_flag = 0;

long chdo_epoch (struct CDS_buffer *buffer)
{
  static long lcl_epoch;

  lcl_epoch = chdo_sclkscet (buffer);
  buffer->packet.cds_tag.epoch[0] = ((unsigned long) lcl_epoch >> 24) & 0xFF;
  buffer->packet.cds_tag.epoch[1] = ((unsigned long) lcl_epoch >> 16) & 0xFF;
  buffer->packet.cds_tag.epoch[2] = ((unsigned long) lcl_epoch >> 8) & 0xFF;
  buffer->packet.cds_tag.epoch[3] = ((unsigned long) lcl_epoch >> 0) & 0xFF;
  return lcl_epoch;
}
int main (int argc, char *argv[])
{
  struct CDS_buffer *buffer;
  char *host;
  char string[128];
  char value[32];
  int version = 0;
  int eof = -1;
  int write_len;
  int len;
  int i;
  int status;
  int active = 1;
  int length;
  FILE *fileout;
  int error_count = 0;
  int error_MAX = 30000;
  int counter = 0;
  int write_flag;
  int label_flag = 0;
  int update_flag = 0;
  int passall = 0;
  int dc = 0;
  int data_length;
  long offset;
  int misc_count = 0;

  fg_flags (argc, argv);
  if (fg_flag ("help") | fg_flag ("h")) {
    printf ("chdopipe %s\n", Version);
    printf ("  re-format CHDO/SFDU records into RPWS\n");
    printf ("  internal data records.  No arguments\n");
    printf ("  stdin -> stdout\n");
    printf ("  \n");
    printf ("  EOF sensitive.  When EOF is encountered, it\n");
    printf ("  is passed along through the pipe\n");
    printf ("  \n");
    printf ("  +update      update the offset file\n");
    printf ("  -debug       enable debugging\n");
    /*
     * printf("  +label       mark 'CHDO' in record\n"); /*
     */
    printf ("  -epup n      update epoch every 'n' records\n");
    printf ("  -epon n      update epoch once after 'n' records\n");
    printf ("  -epof n      update offset 'n' seconds\n");
    printf ("  -epoch n     set an epoch time\n");
    printf ("  \n");
    return 1;
  }

  buffer = malloc (65536);
  if (!buffer)
    exit (0);
  epoch_max = fg_int ("epup", fg_int ("epon", 0));
  epoch_off = fg_int ("epof", 0);
  if (epoch_max) {
    epoch_count = fg_int ("epon", -1);
    fprintf (stderr, "CDHO3: eup = %d %d\n", epoch_max, epoch_count);
  }
  if (fg_flag ("epoch") == '-')
    epoch = fg_int ("epoch", 0);
  if (fg_flag ("label") == '+')
    label_flag = 1;
  if (fg_flag ("update") == '+')
    update_flag = 1;
  if (fg_flag ("debug")) {
    dbg = fopen ("chdopipe.log", "wa");
  }
  passall = fg_flag ("passall");
  buffer->f_length = (&buffer->r_length - &buffer->f_length) * 4;
  buffer->r_length = buffer->f_length;
  buffer->record_type = DATA_RTIU;
  buffer->record_type = DATA_BIU_SIM;
  fileout = stdout;

  if (!fileout) {
    perror ("bad open");
    exit (0);
  }
  while (active) {
    data_length = chdo_getCDS (buffer);
    if (chdo_exit_flag)
      break;
    buffer->status = data_length;
    offset = chdo_epoch (buffer);
    if (dbg) {
      fprintf (dbg, "%4d:", dc++);
      for (i = 0; i < 12; i += 2) {
        fprintf (dbg, " %2.2X%2.2X",
                 buffer->packet.cds.header[i + 1],
                 buffer->packet.cds.header[i + 0]);
      }
      for (i = 12; i < 20; i++) {
        fprintf (dbg, "%3.2X", buffer->packet.cds.header[i]);
      }
      fprintf (dbg, "\n");
      fflush (dbg);
    }
    if (buffer->status > 0) {
      error_count = 0;
      memset (buffer->packet.trailer.data, 0x00, 256);
      buffer->record_type = UTIL_long_to_MSB (DATA_RTIU);
      UTIL_get_time (&(buffer->packet.ws_tag.A));
      write_flag = 0;
      if (buffer->status >= CDS_minimum_length)
        write_flag = 1;
      if (write_flag) {
        /**/
          write_flag = valid_rpws_data ((char *) buffer->packet.cds.header);
      }
      if (passall)
        /**/ write_flag = 1;
      /**/ buffer->record_type = DATA_telemetry;
      if (write_flag) {
        write_len = sizeof (struct CDS_buffer);
        write_len = data_length + 256 + 16 + 28 + 16;
        write_len += 3;                 /* make sure multiple of 32 bits */
        write_len &= 0x7FC;             /* strip lower 2 bits */
        len = UTIL_putbuffr2_CDS (buffer, fileout, write_len);
      } else
        dc += 1;
    }
    switch (epoch_count) {
     default:
       epoch_count -= 1;
       break;
     case 0:
       break;
     case 1:
       epoch_count = -1;
       break;
     case -1:
       if (offset) {
         sprintf (value, "%d", offset - epoch_off);
         if (update_flag)
           utilo_update ("OFFSET", value, version++);
         epoch_count = epoch_max;
         if (fg_flag ("epon"))
           epoch_count = 0;
       }
       break;
    }

  }
  len = fwrite (&eof, 4, 1, fileout);

  return 0;

}
