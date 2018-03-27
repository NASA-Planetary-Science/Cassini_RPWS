#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <errno.h>
 /*
  * chdopipe.c 
  */
#include "rtiu.h"
#include "fg.h"
#include "util.h"
#include "utilo.h"
#define CDS_minimum_length 32

FILE *file_no = stdin;
FILE *file_no2 = stdout;
FILE *report_file = stderr;
long fix_pos;
long fix_size;
static struct CDS_buffer *buffer;

void display_time (struct tm *pkt_tm, long epoch, unsigned long l_epoch,
                   unsigned long sclk)
{
  fprintf (report_file,
           "  %8.8X  %8.8X/%8.8X-%8.8X %4d %3d %2.2d:%2.2d:%2.2d \n", sclk,
           l_epoch, epoch, epoch - l_epoch, pkt_tm->tm_year + 1900,
           pkt_tm->tm_yday, pkt_tm->tm_hour, pkt_tm->tm_min, pkt_tm->tm_sec);
  return;
}
void time_1 (long epoch, int count, int year)
{
  time_t cds_time;
  time_t pkt_epoc;
  struct tm *pkt_tm;
  unsigned long l_epoch;
  unsigned long sclk;
  static int cnt = 0;

  sclk =
    (((unsigned long) buffer->packet.cds_tag.
      begin[0] << 24) & 0xFF000000) | (((unsigned long) buffer->packet.
                                        cds_tag.
                                        begin[1] << 16) & 0x00FF0000) |
    (((unsigned long) buffer->packet.cds_tag.
      begin[2] << 8) & 0x0000FF00) | (((unsigned long) buffer->packet.cds_tag.
                                       begin[3] << 0) & 0x000000FF);
  if (!sclk) {
    sclk =
      (((unsigned long) buffer->packet.cds.
        header[6] << 24) & 0xFF000000) | (((unsigned long) buffer->packet.cds.
                                           header[9] << 16) & 0x00FF0000) |
      (((unsigned long) buffer->packet.cds.
        header[8] << 8) & 0x0000FF00) | (((unsigned long) buffer->packet.cds.
                                          header[11] << 0) & 0x000000FF);
  }
  l_epoch =
    (((unsigned long) buffer->packet.cds_tag.
      epoch[0] << 24) & 0xFF000000) | (((unsigned long) buffer->packet.
                                        cds_tag.
                                        epoch[1] << 16) & 0x00FF0000) |
    (((unsigned long) buffer->packet.cds_tag.
      epoch[2] << 8) & 0x0000FF00) | (((unsigned long) buffer->packet.cds_tag.
                                       epoch[3] << 0) & 0x000000FF);

  cds_time = (time_t) UTIL_extract_TIME (buffer);
  pkt_epoc = epoch + cds_time;
  pkt_tm = gmtime (&pkt_epoc);
  if ((!cnt) || (pkt_tm->tm_year != year))
  {
    display_time (pkt_tm, epoch, l_epoch, sclk);
  }
  if (cnt < count)
    cnt += 1;
  else
    cnt = 0;
  return;
}

main (int argc, char *argv[])
{
  char filename[256];
  char filename2[256];
  static long lcl_epoch;
  static unsigned long fixed_epoch = 0xFFFFFFFF;
  int len;
  int eof = -1;
  int istat;
  int year = 96;

  fg_flags (argc, argv);
  if (fg_flag ("help") | fg_flag ("h")) {
    printf ("fixtim V3.0\n");
    printf ("  re-format time relationship\n");
    printf ("  \n");
    printf ("  -in name     input filename(stdin)\n");
    printf ("  -out name    output filename(stdout)\n");
    printf ("  -epoch nn    use this epoch number D31=1\n");
    printf ("  +epoch nn    use this epoch number\n");
    printf ("  -year nn     sanity check (2 digit year)\n");
    printf ("  -log         report to 'stdout'\n");
    printf ("  \n");
    return;
  }

  buffer = malloc (65536);
  if (fg_flag ("log") == '-')
    report_file = stdout;

  fprintf (report_file, "fixtime");

  if (fg_flag ("epoch") == '-') {
    char temp[128];

    strcpy (temp, fg_flagc ("epoch"));
    if (fg_flag ("epoch") == '-')
      fixed_epoch = strtoul (temp, NULL, 0) | 0x80000000;
    if (fg_flag ("epoch") == '+')
      fixed_epoch = strtoul (temp, NULL, 0);
    fprintf (report_file, " epoch: %X", fixed_epoch);
  }

  if (fg_flag ("year") == '-') {
    year = fg_int ("year", year);
    fprintf (report_file, " year: %d", fg_flagc ("year"));
  }

  if (fg_flag ("in") == '-') {
    file_no = fopen (fg_flagc ("in"), "r");
    if (!file_no) {
      perror ("bad open (read)");
      exit (0);
    }
    fprintf (report_file, " in: %s", fg_flagc ("in"));
  }
  if (fg_flag ("out") == '-') {
    file_no2 = fopen (fg_flagc ("out"), "w");
    if (!file_no2) {
      perror ("bad open (write)");
      exit (0);
    }
    fprintf (report_file, " out: %s", fg_flagc ("out"));
  }
  fprintf (report_file, "\n");

  while (istat = UTIL_getbuffer_CDS (buffer, file_no, UTIL_GET_NON_BLOCKING)) {
    if (istat < 0)
      break;

    lcl_epoch = chdo_sclkscet (buffer);
    if (fixed_epoch != -1)
      lcl_epoch = fixed_epoch;
    buffer->packet.cds_tag.epoch[0] =
      ((unsigned long) lcl_epoch >> 24) & 0xFF;
    buffer->packet.cds_tag.epoch[1] =
      ((unsigned long) lcl_epoch >> 16) & 0xFF;
    buffer->packet.cds_tag.epoch[2] = ((unsigned long) lcl_epoch >> 8) & 0xFF;
    buffer->packet.cds_tag.epoch[3] = ((unsigned long) lcl_epoch >> 0) & 0xFF;
    time_1 (lcl_epoch, 512, year);
    /**/ UTIL_putbuffer_CDS (buffer, file_no2);
  }
  UTIL_putbuffer_eof (file_no2);
}
