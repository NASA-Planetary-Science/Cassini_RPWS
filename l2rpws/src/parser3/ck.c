#include <stdio.h>
#include <string.h>

unsigned short JPL_checksum (unsigned short *data,
                             int num_words, int skip_factor)
{
  int i;
  unsigned long accum, acc;
  unsigned long lsbs, msbs;
  unsigned short us;

  accum = 0x55AA;                       /* checksum seed */

  for (i = 0; i < num_words; i++) {
    acc = (unsigned long) data[i * skip_factor];
    accum = accum + acc;
  }
  lsbs = (accum & 0xFFFF);
  msbs = ((accum >> 16) & 0xFFFF);      /* mask overflow */
  acc = lsbs + msbs;                    /* add overflow (!?!) */

  lsbs = (acc & 0xFFFF);
  msbs = ((acc >> 16) & 0xFFFF);        /* check overflow again */

  us = (unsigned short) lsbs + (unsigned short) msbs;

  return us;
}
