
/*
 * dsp3.c
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <term.h>
#include "rtiu.h"
#include "util.h"

struct CDS_buffer buffer;

 /*
  * char *UTIL_extract_packet_type();
  */

raw_disp (struct CDS_buffer *buffer, int width)
{
  int ilen, i, j, vert;

  ilen = UTIL_extract_RTIU_length (buffer);
  ilen = ilen + width - 1;
  ilen = ilen / width;
  vert = 6;
  for (i = 0; i < ilen; i++) {
    mvprintw (vert, 0, "%4.4X: ", i * width);
    for (j = 0; j < width; j++) {
      if (i * width + j >= UTIL_extract_RTIU_length (buffer))
        break;
      mvprintw (vert, j * 3 + 6, "%2.2X ",
                buffer->packet.rtiu.header[i * width + j]);
    }
    vert = vert + 1;
  }
  return;
}

main (int argc, char *argv[])
{
  static int width = 16;
  static int delay = 0;
  int flag, second, icnt;
  int ilen;
  long cds[6];
  int i;
  unsigned char string[128];

  initscr ();
  nonl ();
  clear ();
  refresh ();
  mvprintw (1, 10, "CASSIN PC-RTIU raw data dump (DSP3)");
  if (argc > 1) {
    width = strtol (argv[1], (char **) NULL, 0);
    if (!width)
      width = 16;
  }
  if (argc > 2) {
    delay = strtol (argv[2], (char **) NULL, 0);
  }
  ilen = UTIL_getbuffer_CDS (&buffer, stdin, 1);
  while (ilen) {
    mvprintw (1, 50, "Cycle %d ", icnt++);
    UTIL_extract_CDS (&buffer, cds);
    mvprintw (2, 1,
              "%2.2X %s  (.record_type)%4d    ",
              UTIL_extract_CDS_type (&buffer),
              UTIL_extract_packet_type (&buffer), buffer.record_type);
    mvprintw (2, 50, "RTIU length %d   ", UTIL_extract_RTIU_length (&buffer));
    mvprintw (3, 15, "Len%6.2d ", UTIL_extract_CDS_length (&buffer));
    mvprintw (3, 1, "Seq%6.2d ", UTIL_extract_CDS_sequence (&buffer));
    mvprintw (3, 30, " Time %d.%d ",
              UTIL_extract_TIME (&buffer), UTIL_extract_RTI (&buffer)
      );
    mvprintw (4, 1, " %4.4X %4.4X %4.4X %4.4X %4.4X %4.4X\n",
              cds[0], cds[1], cds[2], cds[3], cds[4], cds[5]);
    raw_disp (&buffer, width);
    refresh ();
    if (delay)
      sleep (delay);
    ilen = UTIL_getbuffer_CDS (&buffer, stdin, 1);
  }
  return (0);
}
