
/*
 * dsp1.c
 */
#include <stdio.h>
#include <stdlib.h>
#include<fcntl.h>
#include "rtiu.h"
#include "util.h"

struct CDS_buffer buffer;

main ()
{
  int flag, second;
  int ilen;
  long cds[6];
  int i;
  unsigned char string[128];

  ilen = UTIL_getbuffer_CDS (&buffer, stdin, 1);
  printf ("first buffer\n");
  while (ilen) {
    UTIL_extract_CDS (&buffer, cds);
    printf ("%X %s Length %d bytes   %d\n",
            UTIL_extract_CDS_type (&buffer),
            UTIL_extract_packet_type (&buffer),
            UTIL_extract_CDS_length (&buffer),
            UTIL_extract_CDS_sequence (&buffer)
      );
    printf (" %4.4X %4.4X %4.4X %4.4X %4.4X %4.4X\n",
            cds[0], cds[1], cds[2], cds[3], cds[4], cds[5]);
    ilen = UTIL_getbuffer_CDS (&buffer, stdin, 1);
  }
  return (0);
}
