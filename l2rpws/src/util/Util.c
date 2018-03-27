
/* RPWS_util - Cassini/RPWS file utilities
 *************
 *	Adding MS-DOS compatible hooks to allow
 *      this to be compatible...
 *
 *      ALL 32 bit ITEMS are  BIG-ENDIAN !!!
 *************
 */
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#ifdef __MSDOS__

#ifndef _Windows
#include <dos.h>
#endif
#endif

#include "rtiu.h"

#define _FLUSH

static char ident[] = { "IDENT util.c 16-June-2000" };
static int eof = -1;

static union bt
{
  unsigned char bit8[4];
  unsigned short bit16[2];
  unsigned long bit32[1];
} bits;

int Util_16 (short buf)
{
  bits.bit16[0] = buf;
  return bits.bit8[0] << 8 | bits.bit8[1];
}
int Util_32 (long buf)
{
  bits.bit32[0] = buf;
  return bits.bit8[0] << 24 | bits.bit8[1] << 16 | bits.bit8[2] << 8 | bits.
    bit8[3];
}

/********************************************************
 * Extract CDS header from RPWS record
 * Knowing the order of bytes from the 8085, we
 * will attempt to build the 6 CDS header words
 * in native order
 ********************************************************/
void Util_extract_CDS (struct CDS_buffer *buffer /* data buffer */ ,
                       long *cds /* 6 word (16 bit) CDS header */ )
{
  long i;

  for (i = 0; i < 6; i++) {
    cds[i] = buffer->packet.cds.header[i * 2 + 1];
    cds[i] = buffer->packet.cds.header[i * 2] + (cds[i] << 8);
  }
  return;
}

/********************************************************
 *							*
 *	Extract the 32 bit time field from the CDS	*
 *	  secondary header.  Shuffle it around to 	*
 *	  make it into a native integer.		*
 *							*
 ********************************************************/
 /*
  * Extract timetag from CDS record
  * * Recovers the 32 bit CDS SCLK.  RTI is NOT returned with this call
  * *returns packet.cds.header
  */
unsigned long Util_extract_TIME (struct CDS_buffer *buffer)
{                                       /* data buffer */
  unsigned long i[4];

  i[0] = buffer->packet.cds.header[11] << 0;
  i[1] = buffer->packet.cds.header[8] << 8;
  i[2] = buffer->packet.cds.header[9] << 16;
  i[3] = buffer->packet.cds.header[6] << 24;
  return i[0] | i[1] | i[2] | i[3];
}

/********************************************************
 * Extract RTI field from  CDS buffer
 *	Extract the RTI field from the CDS secondary	
 *	  header.  Nominally this field is in .004	
 *	  second units, although we are only capable	
 *	  of keeping track of RTI (i.e. .125 Sec)	
 *	  intervals.					
 *	Should return anumber in the range of 0-7	
 * returns 3 bit RTI (1/8 second RTI interval)
 ********************************************************/
long Util_extract_RTI (struct CDS_buffer *buffer)
{                                       /* data buffer */
  return buffer->packet.cds.header[10] >> 5;
}
