
/*
 * rpws_archive.c
 */
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <sys/types.h>
#include <time.h>
#include "fg.h"
#include "rpws_label_graphics.h"

#define K 0

int main (int argc, char *argv[])
{
  int i;
  struct GRAPHICS_TEXT text[2] = {
    "Red Hat Linux",                    /* upper */
    "Iowa City, Iowa",
    "Department of Physics & Astronomy",
    "University of Iowa",
    NULL,
    NULL,
    "Shrike",                           /* lower */
    "Version 9.0",
    "Gathered on",
    "March 14, 2003",
    "                             ",    /* date stamp */
    "",
    " ",                                /* left */
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",                                /* right */
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",

    "MARSIS / Mars Express",            /* upper */
    "Iowa City, Iowa",
    "Department of Physics & Astronomy",
    "University of Iowa",
    NULL,
    NULL,
    "Linux GSE System Image",           /* lower */
    "Post Testing",
    " ",
    "Archived on",
    "                             ",    /* date stamp */
    "",
    " ",                                /* left */
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",                                /* right */
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " ",
    " "
  };
  char *text_1[] = { "i386-disc-1  ",
    "i386-disc-2  ",
    "i386-disc-3  ",
    "SRPMS-disc-1  ",
    "SRPMS-disc-2  ",
    "SRPMS-disc-3  ",
    "docs-US  ",
    "Spare  ",
    "  "
  };
  char *text_2[] = { "  BOOT & Install 1",
    "  Install 2",
    "  Install 3",
    "  Source 1",
    "  Source 2",
    "  Source 3",
    "  documentation",
    "  Spare  ",
    "  "
  };
  char *text_1a[] = { "CD ROM  ",
    "DVD  ",
    "i386-disc-3  ",
    "SRPMS-disc-1  ",
    "SRPMS-disc-2  ",
    "SRPMS-disc-3  ",
    "docs-US",
    "  ",
    "  "
  };
  char *text_2a[] = { "  ISO 9660",
    "  ISO 9660",
    "  Install 3",
    "  Source 1",
    "  Source 2",
    "  Source 3",
    "  documentation",
    "  ",
    "  "
  };
  time_t time_t_uttime;
  struct tm *tm_uttime;

  time_t_uttime = time (NULL);
  tm_uttime = gmtime (&time_t_uttime);

  sprintf (text[K].lower_text[4], "%04d-%02d-%02dT%02d:%02d:%02d",
           tm_uttime->tm_year + 1900,
           tm_uttime->tm_mon + 1,
           tm_uttime->tm_mday,
           tm_uttime->tm_hour, tm_uttime->tm_min, tm_uttime->tm_sec);

  gr_main (0, NULL, "./");
  for (i = 0; i < 2; i++) {
    text[K].left_text[4] = text_1[i];
    text[K].right_text[4] = text_2[i];
    gr_main (1, &text[K], NULL);
  }
  gr_main (2, NULL, NULL);
}
