
/*	mpb.c	*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include "fg.h"
#include "rtiu.h"
#include "util.h"
#include "utilf.h"

static unsigned int rti_ = 0;
static unsigned int time1_ = 0;

int _getbuffer_CDS (char *c_buffer, FILE * input, int flg)
{
  int flag = 1;
  int ilen;
  int rti, time1;

  while (flag) {
    ilen = UTIL_getbuffer_CDS (&c_buffer, input, flg);
    if (!(rti_ + time1_))
      flag = 0;
    rti = UTIL_extract_MP_TIME (&c_buffer);
    if (rti == rti_)
      flag = 0;
    time1 = UTIL_extract_PKT_TIME (&c_buffer);
    if (time1 == time1_)
      flag = 0;
  }
  return ilen;
}
int main (int argc, char *argv[])
{
  char filename[256];
  int ilen;
  FILE *input = NULL;
  FILE *otput = stdout;
  char c_buffer[65536];

  fg_flags (argc, argv);

  if (fg_flag ("help") || fg_flag ("h")) {
    printf (" \n");
    printf (" \n");
    printf (" gett  dataproduct from anywhere\n");
    printf (" \n");
    printf ("      -find name    find & read file\n");
    printf ("                       (looks on CDROM)\n");
    printf ("      -locate name  search for file\n");
    printf ("                       (looks on CDROM)\n");
    printf ("      -rti 0x00     search for rti number\n");
    printf ("                       (use on mini-packet only)\n");
    printf ("      -time 0x00    search for time field 1\n");
    printf ("                       (use on mini-packet only)\n");
    printf (" \n");
    printf ("      -help\n");
    exit (0);
    printf (" \n");
  }

  strcpy (filename, fg_flagc ("locate"));
  input = UTIL_find_open (filename, "rb");
  if (input) {
    fprintf (stderr, "LOCATE %s \n", UTIL_find_name ());
    exit (0);
  }
  if (fg_flag ("locate"))
    exit (0);

  strcpy (filename, fg_flagc ("find"));
  input = UTIL_find_open (filename, "rb");
  if (!input) {
    exit (0);
  }
  fprintf (stderr, "OPEN %s \n", UTIL_find_name ());

  ilen = _getbuffer_CDS (c_buffer, input, UTIL_GET_NON_BLOCKING);
  while (ilen > 0) {
    ilen = UTIL_putbuffer_CDS (&c_buffer, otput);
    ilen = _getbuffer_CDS (c_buffer, input, UTIL_GET_NON_BLOCKING);
  }
  return 1;
}
