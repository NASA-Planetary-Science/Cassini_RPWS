#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include "rtiu.h"
#include "util.h"


#include <math.h>

/* CRC generation tables */

static long crcta[16] = { 0L, 010201L, 020402L, 030603L, 041004L,
  051205L, 061406L, 071607L, 0102010L, 0112211L, 0122412L, 0132613L, 0143014L,
  0153215L, 0163416L, 0173617L
};

static long crctb[16] = { 0L, 010611L, 021422L, 031233L, 043044L,
  053655L, 062466L, 072277L, 0106110L, 0116701L, 0127532L, 0137323L, 0145154L,
  0155745L, 0164576L, 0174367L
};


/*  C H K 3  --  Compute a type-3 Kermit block check.  */

/*
 * Calculate the 16-bit CRC-CCITT of an HFR mini-packet using a lookup 
 * table.
 *
 *	Return values:
 *		-1 wrong type
 *		 1 bad crc
 *		 0 OK
 */
unsigned int chk3 (char *pkt)
{
  register long i, c, crc, value;
  register unsigned int m, nchar;
  char *save;

  crc = 0L;
  m = 0377;
  save = pkt;
  if (((*pkt >> 4) & 0x0F) != PACKET_TYPE_hfr)
    return (-1);
  nchar = ((int) (*pkt) & 0x0F);
  pkt++;
  nchar = (nchar << 8) + (int) (*pkt);
  pkt--;
  for (i = 0; i <= nchar; i++) {
    c = crc ^ (long) (*pkt & m);
    crc = (crc >> 8) ^ (crcta[(c & 0xF0) >> 4] ^ crctb[c & 0x0F]);
    pkt++;
  }
  value = (long) (*pkt & m);
  pkt++;
  value = value + ((long) (*pkt & m) << 8);
  save++;
  (*save)--;
  (*save)--;
  if (crc == value)
    return (0);
  else
    return (1);
}
