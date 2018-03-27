
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
#else
#include <unistd.h>
#endif

#include "rtiu.h"

#define _FLUSH

static char ident[] = { "IDENT util.c 16-June-2000" };
static int eof = -1;
static char string[132];

struct segment_type
{
  int index;
  char *type;
  char *brief;
};
static struct segment_type UTIL_seg[65] = { {0, NULL, NULL},      /* 00 */
{1, "Unsegmented LRS Type I ", "UnsLRS 1"},     /* 01 */
{2, "Unsegmented HRS Type I ", "UnsHRS 1"},     /* 02 */
{0, NULL, NULL},                        /* 03 */
{  0, NULL, NULL},                        /* 04 */
{  0, NULL, NULL},                        /* 05 */
{  0, NULL, NULL},                        /* 06 */
{  0, NULL, NULL},                        /* 07 */
{  0, NULL, NULL},                        /* 08 */
{  0, NULL, NULL},                        /* 09 */
{  0, NULL, NULL},                        /* 00 */
{  0, NULL, NULL},                        /* 11 */
{  0, NULL, NULL},                        /* 12 */
{  0, NULL, NULL},                        /* 13 */
{  0, NULL, NULL},                        /* 14 */
{  0, NULL, NULL},                        /* 15 */
{  3, "Housekeeping, ROM MODE ", "Hsk ROM "},     /* 16 */
{  0, NULL, NULL},                        /* 17 */
{  0, NULL, NULL},                        /* 18 */
{  3, "Housekeeping, Deploy   ", "Hsk Dpl "},     /* 19 */
{  0, NULL, NULL},                        /* 20 */
{  3, "Housekeeping, Science  ", "Hsk SCI "},     /* 21 */
{  0, NULL, NULL},                        /* 22 */
{  0, NULL, NULL},                        /* 23 */
{  0, NULL, NULL},                        /* 24 */
{  0, NULL, NULL},                        /* 25 */
{  0, NULL, NULL},                        /* 26 */
{  0, NULL, NULL},                        /* 27 */
{  0, NULL, NULL},                        /* 28 */
{  0, NULL, NULL},                        /* 29 */
{  0, NULL, NULL},                        /* 30 */
{  0, NULL, NULL},                        /* 31 */
{  1, "Segmented LRS Type I   ", "SegLRS 1"},     /* 32 */
{  0, NULL, NULL},                        /* 33 */
{  0, NULL, NULL},                        /* 34 */
{  2, "Segmented HRS Type I   ", "SegHRS 1"},     /* 35 */
{  0, NULL, NULL},                        /* 36 */
{  0, NULL, NULL},                        /* 37 */
{  0, NULL, NULL},                        /* 38 */
{  0, NULL, NULL},                        /* 39 */
{  0, NULL, NULL},                        /* 40 */
{  0, NULL, NULL},                        /* 41 */
{  0, NULL, NULL},                        /* 42 */
{  0, NULL, NULL},                        /* 43 */
{  0, NULL, NULL},                        /* 44 */
{  0, NULL, NULL},                        /* 45 */
{  0, NULL, NULL},                        /* 46 */
{  0, NULL, NULL},                        /* 47 */
{  0, NULL, NULL},                        /* 48 */
{  0, NULL, NULL},                        /* 49 */
{  0, NULL, NULL},                        /* 50 */
{  0, NULL, NULL},                        /* 51 */
{  0, NULL, NULL},                        /* 52 */
{  0, NULL, NULL},                        /* 53 */
{  0, NULL, NULL},                        /* 54 */
{  0, NULL, NULL},                        /* 55 */
{  0, NULL, NULL},                        /* 56 */
{  0, NULL, NULL},                        /* 57 */
{  0, NULL, NULL},                        /* 58 */
{  0, NULL, NULL},                        /* 59 */
{  0, NULL, NULL},                        /* 60 */
{  0, NULL, NULL},                        /* 61 */
{  0, NULL, NULL},                        /* 62 */
{  0, NULL, NULL},                        /* 63 */
{  3, "Special Maintenance    ", "Spc Mnt "}      /* 64 */
};
static char UTIL_mp_type[16][32] = { "STIM      ",      /*   0  */
  "MFR       ",                         /*   1  */
  "HFR       ",                         /*   2  */
  "parity    ",                         /*   3  */
  "LP        ",                         /*   4  */
  "parity    ",                         /*   5  */
  "parity    ",                         /*   6  */
  "LFDR      ",                         /*   7  */
  "WFR       ",                         /*   8  */
  "parity    ",                         /*   9  */
  "parity    ",                         /*  10  */
  "DUST      ",                         /*  11  */
  "parity    ",                         /*  12  */
  "MRO       ",                         /*  13  */
  "WBR       ",                         /*  14  */
  "FILL      "
};                                      /*  15  */

#ifdef _Windows

 /*******************************************************
  *  WHAT a pain in the *SS this MS-Windows is          *
  *   provide a crude SLEEP function for windows        *
  *******************************************************/
void sleep (unsigned seconds)
{
  time_t start, now;
  double dcnt;

  dcnt = seconds + 1;
  start = time (NULL);
  now = time (NULL);
  while (difftime (now, start) < dcnt)
    now = time (NULL);
  return;
}
#endif

/********************************************************
 	Build an MSB integer (32 bit)                    
 ********************************************************/
long UTIL_short_to_MSB (unsigned char *val)
{
  return val[0] + (256 * val[1]);
}

  /*
   * Workstation 32 bit integer to PC(DOS) 16 bit integer 
   */
unsigned short UTIL_long_to_PC16 (unsigned long val)
{
  union bt
  {
    unsigned char bit8[4];
    unsigned short bit16[2];
    unsigned long bit32[1];
  } bits;

  bits.bit8[3] = 0;
  bits.bit8[2] = 0;
  bits.bit8[1] = (val >> 8) & 0xFF;
  bits.bit8[0] = (val >> 0) & 0xFF;
  return bits.bit16[0];
}
unsigned long UTIL_long_to_MSB (unsigned long val)
{
  union bt
  {
    unsigned char bit8[4];
    unsigned short bit16[2];
    unsigned long bit32[1];
  } bits;

  bits.bit8[0] = (val >> 24) & 0xFF;
  bits.bit8[1] = (val >> 16) & 0xFF;
  bits.bit8[2] = (val >> 8) & 0xFF;
  bits.bit8[3] = (val >> 0) & 0xFF;
  return bits.bit32[0];
}

  /*
   * Calculate length of RPWS record
   * *The length of an RPWS record should be a multiple of
   * *32 bits
   */
unsigned long UTIL_long_to_MSB3 (unsigned long val)
{                                       /* length field from RPWS record */
  unsigned long len;

  len = val + 3;
  len = len & 0x1FFFC;
  return UTIL_long_to_MSB (len);
}
unsigned long UTIL_MSB_to_long (unsigned long val)
{
  unsigned long i;
  union bt
  {
    unsigned char bit8[4];
    unsigned short bit16[2];
    unsigned long bit32[1];
  } bits;

  bits.bit32[0] = val;
  i = bits.bit8[0] << 24;
  i = i + bits.bit8[1] << 16;
  i = i + bits.bit8[2] << 8;
  i = i + bits.bit8[3] << 0;
  return i;
}
long U_big (long i)
{
  struct swap
  {
    union
    {
      long in;
      unsigned char out[4];
    } s;
  } s;

  s.s.in = i;
  return (s.s.out[0] << 24) +
    (s.s.out[1] << 16) + (s.s.out[2] << 8) + (s.s.out[3] << 0);
}

  /*
   * Place reverse length field into RPWS record
   * * Put the RPWS reverse link into the appropriate
   * * place in the RPWS record
   * *returns void
   */
int UTIL_r_length (long *buf /* RPWS record */ ,
                   long length /* RPWS record length */ )
{
  buf[length] = UTIL_long_to_MSB (length);
  return 0;
}

 /*******************************************************
     output a buffer                            
  						
  	OPEN a stream file and pass the FILE		
  	    pointer into theis routine.  It will	
   	    attempt to accept data from the stream	
  	    and return it to the caller			
  *******************************************************/
long UTIL_putbuffer_ (long *buffer /* RPWS record */ ,
                      FILE * fileno /* output file */ )
{
  long i;

  if (!buffer)                          /* NO NULL pointers */
    return 0;
  if (!buffer[0])                       /* NO ZERO length records */
    return 0;
  i = fwrite (buffer, U_big (buffer[0]) + 4, 1, fileno);

#ifdef _FLUSH
  fflush (fileno);
#endif

  return i;
}

  /*
   * output a CDS record 
   */
long UTIL_putbuffer_CDS (long *buffer, FILE * fileno)
{
  return UTIL_putbuffer_ (buffer, fileno);
}

  /*
   * output a  record 
   */
long UTIL_putbuffer_MP (long *buffer, FILE * fileno)
{
  return UTIL_putbuffer_ (buffer, fileno);
}

  /*
   * output a  record 
   */
long UTIL_putbuffer_RPWS (long *buffer, FILE * fileno)
{
  return UTIL_putbuffer_ (buffer, fileno);
}

  /*
   * output a CDS record 
   * * set both record length values 
   */
long UTIL_putbuffr2_CDS (long *buffer, FILE * fileno, long length)
{
  buffer[0] = UTIL_long_to_MSB3 (length);
  buffer[U_big (buffer[0]) / 4] = buffer[0];
  return UTIL_putbuffer_ (buffer, fileno);
}

  /*
   * output an MP record 
   * * set both record length values 
   */
long UTIL_putbuffr2_MP (long *buffer, FILE * fileno, long length)
{
  buffer[0] = UTIL_long_to_MSB3 (length);
  buffer[U_big (buffer[0]) / 4] = buffer[0];
  return UTIL_putbuffer_ (buffer, fileno);
}

  /*
   * output an RPWS record
   * * set both record length values 
   */
long UTIL_putbuffr2_RPWS (long *buffer, FILE * fileno, long length)
{
  buffer[0] = UTIL_long_to_MSB3 (length);
  buffer[U_big (buffer[0]) / 4] = buffer[0];
  return UTIL_putbuffer_ (buffer, fileno);
}

  /*
   * set length in a CDS record
   * * set both record length values 
   */
long UTIL_lenbuffr2_CDS (long *buffer, long length)
{
  buffer[0] = UTIL_long_to_MSB3 (length);
  buffer[U_big (buffer[0]) / 4] = buffer[0];
  return buffer[0];
}

  /*
   * set length in an MP record
   * * set both record length values 
   */
long UTIL_lenbuffr2_MP (long *buffer, long length)
{
  buffer[0] = UTIL_long_to_MSB3 (length);
  buffer[U_big (buffer[0]) / 4] = buffer[0];
  return buffer[0];
}

  /*
   * set length in an RPPS record
   * * set both record length values 
   */
long UTIL_lenbuffr2_RPWS (long *buffer, long length)
{
  buffer[0] = UTIL_long_to_MSB3 (length);
  buffer[U_big (buffer[0]) / 4] = buffer[0];
  return buffer[0];
}

  /*
   * output an EOF record 
   */
long UTIL_putbuffer_eof (FILE * fileno)
{
  int i;

  i = fwrite (&eof, 4, 1, fileno);

#ifdef _FLUSH
  fflush (fileno);
#endif

  return i;
}

/********************************************************
 *							*
 *	OPEN a stream file and pass the FILE		*
 *	    pointer into theis routine.  It will	*
 *	    attempt to accept data from the stream	*
 *	    and return it to the caller			*
 *	  NOTE the addition of the 3rd. parameter	*
 *	    to allow a non-blocking function to		*
 *	    occur on data files (pipes always block)	*
 *							*
 ********************************************************/
 /*
  * long UTIL_getbuf_(long *buffer, FILE *fileno)
  * {
  * long reclen;
  * long ilen;
  * long istat;
  * istat = 0;
  * 
  * ilen = fread(&buffer[0], 4, 1, fileno);
  * if ((ilen == 1) && (U_big(buffer[0])  > 0))
  * {
  * reclen = U_big(buffer[0]);
  * if(reclen & 0xFFFF0000)
  * {
  * fprintf(stderr, "Record Length greater than 65K\n");
  * istat = 0;;
  * }
  * else
  * {
  * ilen = fread(&buffer[1], reclen, 1, fileno);
  * if (ilen == 1)
  * istat = 1;
  * }
  * }
  * return istat;
  * }
  */
  /*
   *    Arguments:
   *            buffer (obvious, yes?)
   *            fileno (filen handle, of course)
   *            blocking:
   *                    0   NON_BLOCKING (returns -1)
   *                    1   GET_BLOCKING (doesn't return unless data ready or error)
   *                    2   GET_RELEASE (returns -1)
   *    Return status values:
   *
   *            -1      no data available
   *             0      parameter error
   *             1      data read successfully
   */
long UTIL_getbuffer_ (long *buffer, FILE * fileno, long block)
{
  long reclen;
  long ilen;
  long fpos;

  fpos = ftell (fileno);
  while (1) {
    ilen = fread (&buffer[0], 4, 1, fileno);
    if ((ilen == 1) && (U_big (buffer[0]) > 0)) {
      reclen = U_big (buffer[0]);
      if (reclen & 0xFFFF0000) {
        fprintf (stderr, "Record Length greater than 65K\n");
        return 0;
      } else {
        ilen = fread (&buffer[1], reclen, 1, fileno);
        if (ilen == 1)
          return 1;
      }
    }
    if (ferror (fileno))
      return 0;
    fseek (fileno, fpos, SEEK_SET);
    switch (block) {
     case 0:                           /*  */
       return -1;
     case 1:                           /*  */
     default:
       sleep (1);
       break;
     case 2:                           /*  */
       sleep (1);
       return -1;
    }
  }

#ifndef __MSDOS__
  return 0;
#endif
}

long UTIL_getbuffer_CDS (long *buffer, FILE * fileno, long block)
{
  return UTIL_getbuffer_ (buffer, fileno, block);
}
long UTIL_getbuffer_MP (long *buffer, FILE * fileno, long block)
{
  return UTIL_getbuffer_ (buffer, fileno, block);
}
long UTIL_getbuffer_RPWS (long *buffer, FILE * fileno, long block)
{
  return UTIL_getbuffer_ (buffer, fileno, block);
}

/********************************************************
 * Extract CDS header from RPWS record
 * Knowing the order of bytes from the 8085, we
 * will attempt to build the 6 CDS header words
 * in native order
 ********************************************************/
void UTIL_extract_CDS (struct CDS_buffer *buffer /* data buffer */ ,
                       long *cds /* 6 word (16 bit) CDS header */ )
{
  long i;

  for (i = 0; i < 6; i++) {
    cds[i] = buffer->packet.cds.header[i * 2 + 1];
    cds[i] = buffer->packet.cds.header[i * 2] + cds[i] * 256;
  }
  return;
}

/********************************************************
 *	Extract the length field in the PC-RTIU header	
 *							
 ********************************************************/
long UTIL_extract_RTIU_length (struct CDS_buffer *buffer /* data buffer */ )
{
  return (buffer->packet.rtiu.header[3] << 8) | buffer->packet.rtiu.header[2];
}

/********************************************************
 *	Return the length field in the CDS header	
 *
 *	Return the length field in the CDS header	
 *	  ! BUT !  minimize it other fields		
 *	    (like the PC-RTIU length field)		
 *	Will mess with 'switch' statement to implement	
 *	  unique checking for each data source		
 *							
 ********************************************************/
long UTIL_extract_CDS_length (struct CDS_buffer *buffer /* data buffer */ )
{
  long i, j;

  i = buffer->packet.cds.header[5];
  i = buffer->packet.cds.header[4] + i * 256;
  i = i & 0xFFFF;
  switch ((int) U_big (buffer->record_type)) {
   default:
     j = i;
     break;
   case DATA_RTIU:
   case DATA_BIU_SIM:
     j = UTIL_extract_RTIU_length (buffer) - RTIU_header_length + 6;
     break;
  }
  if (i > j)
    i = j;
  i = i & 0x3FFF;                       /* only 14 bits of length */
  return i;
}

/********************************************************
 *	Return the 14 bit sequence field from the CDS header	
 ********************************************************/
long UTIL_extract_CDS_sequence (struct CDS_buffer *buffer /* data buffer */ )
{
  long i;

  i = buffer->packet.cds.header[3];
  i = buffer->packet.cds.header[2] + i * 256;
  return i & 0x3FFF;
}

/********************************************************
 * Examine record and return CDS type field
 *	Return the CDS typ field from the CDS header	
 *			6 bits				
 *	VALIDATE CDS header bytes!!!	(-1 meand BAD!)	
 *		       & 3F				
 *	LRS	0A81	01	Unsegmented		
 *		0AA0	20	Segmented		
 *	HRS	0A82	02	Unsegmented		
 *		0AA3	23	Segmented		
 ********************************************************/
long UTIL_extract_CDS_type (struct CDS_buffer *buffer /* data buffer */ )
{
  long cds[6];

  /*
   * i = buffer->packet.cds.header[0] & 0x3F; /*
   */
  UTIL_extract_CDS (buffer, cds);
  if (cds[0] == 0)
    return -1;
  return (cds[0] & 0x3F);
}

/********************************************************
 * Return length for CDS-HRS records
 *	Check to see if the CDS 'type' field indicates	
 *	  that this is HIGH RATE SCIENCE, return the	
 *	  length field out of the CDS header, otherwise	
 *	  return a ZERO.  This will cause ZERO-LENGTH	
 *	  packets to be stripped from the data stream	
 *							
 * return length or ZERO if not this type
 *							
 ********************************************************/
long UTIL_extract_HRS (struct CDS_buffer *buffer /* data buffer */ )
{
  long i;

  i = UTIL_extract_CDS_type (buffer);
  if (i == 0x02)
    return UTIL_extract_CDS_length (buffer);
  if (i == 0x23)
    return UTIL_extract_CDS_length (buffer);
  return 0;
}

/********************************************************
 * Return length for CDS-LRS records
 *	Check to see if the CDS 'type' field indicates	
 *	  that this is LOW RATE SCIENCE, return the	
 *	  length field out of the CDS header, otherwise	
 *	  return a ZERO.  This will cause ZERO-LENGTH	
 *	  packets to be stripped from the data stream	
 *							
 * return length or ZERO if not this type
 ********************************************************/
long UTIL_extract_LRS (struct CDS_buffer *buffer /* data buffer */ )
{
  long i;

  i = UTIL_extract_CDS_type (buffer);
  if (i == 0x20)
    return UTIL_extract_CDS_length (buffer);
  if (i == 0x01)
    return UTIL_extract_CDS_length (buffer);
  return 0;
}

/********************************************************
 * Return length for CDS-HSK records
 *	Check to see if the CDS 'type' field indicates	
 *	  that this is HOUSEKEEPING, return the		
 *	  length field out of the CDS header, otherwise	
 *	  return a ZERO.  This will cause ZERO-LENGTH	
 *	  packets to be stripped from the data stream	
 *							
 * return length or ZERO if not this type
 ********************************************************/
long UTIL_extract_HSK (struct CDS_buffer *buffer /* data buffer */ )
{
  long i;

  i = UTIL_extract_CDS_type (buffer);
  if (i == 0x10)
    return UTIL_extract_CDS_length (buffer);
  if (i == 0x13)
    return UTIL_extract_CDS_length (buffer);
  if (i == 0x15)
    return UTIL_extract_CDS_length (buffer);
  return 0;
}

/********************************************************
 * Return length for CDS-MRO records
 *	Check to see if the CDS 'type' field indicates	
 *	  that this is Memory Read Out, return the	
 *	  length field out of the CDS header, otherwise	
 *	  return a ZERO.  This will cause ZERO-LENGTH	
 *	  packets to be stripped from the data stream	
 * return length or ZERO if not this type
 ********************************************************/
long UTIL_extract_MRO (struct CDS_buffer *buffer /* data buffer */ )
{
  long i;

  i = UTIL_extract_CDS_type (buffer);
  if (i == 0xFF)
    return UTIL_extract_CDS_length (buffer);
  if (i == 0xFF)
    return UTIL_extract_CDS_length (buffer);
  return 0;
}

/********************************************************
 * Extract packet type from CDS record
 *	Retuen a pointer to a text string that	 	
 *	  describes the current data buffer		
 *	IF we are on the ball, this string will match	
 *	  something in the documentation (somewhere)	
 *	We're going to hash up the DEPLOY string to	
 *	  indicate special venus mode if we can...      
 * returns text string indicating packet type
 ********************************************************/
char *UTIL_extract_packet_type (struct CDS_buffer *buffer /* data buffer */ )
{
  long i;

  i = UTIL_extract_CDS_type (buffer) & 0x3F;
  if (i == 19)
    if ((buffer->packet.housekeeping.Memory_Address[0] != 0) ||
        (buffer->packet.housekeeping.Memory_Address[1] != 0))
      i = 64;
  return UTIL_seg[i].type;
}

/*
 * Extract packet type from CDS record
 *	Retuen a pointer to a text string that	 	
 *	  describes the current data buffer		
 * returns short text string indicating packet type
 */
char *UTIL_extract_packet_brief (struct CDS_buffer *buffer /* data buffer */ )
{
  long i;

  i = UTIL_extract_CDS_type (buffer) & 0x3F;
  if (i == 19)
    if ((buffer->packet.housekeeping.Memory_Address[0] != 0) ||
        (buffer->packet.housekeeping.Memory_Address[1] != 0))
      i = 64;
  return UTIL_seg[i].brief;
}
int UTIL_extract_packet_sindex (struct CDS_buffer *buffer)
{                                       /* data buffer */
  long i;

  i = UTIL_extract_CDS_type (buffer) & 0x3F;
  return UTIL_seg[i].index;
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
long UTIL_extract_RTI (struct CDS_buffer *buffer)
{                                       /* data buffer */
  return buffer->packet.cds.header[10] >> 5;
}

/*
 * Extract sub RTI field from  CDS buffer
 *	Extract the subRTI field from the CDS secondary	
 *	  header.  The subRTI is a sequence number that
 *	  is incremented as each CDS record is formatted
 *	  on the HRP.
 *	For HRP should return a number in the range of 1-15
 *	For LRP should return 0.
 * returns 4 bit subRTI
 */
long UTIL_extract_subRTI (struct CDS_buffer *buffer)
{                                       /* data buffer */
  return (buffer->packet.cds.header[10] >> 1) & 0x0F;
}

/*
 * Extract Time Quality flag from  CDS buffer
 *	Extract the TQF bit from the CDS secondary	
 *	  header.  The TQF bit inicates when the 
 *	  spacecraft is updating the time field.
 *	  0 indicates the time field was received from
 *	  the spacecraft within the last second. A 1
 *	  indicates that the instrument has rolled
 *	  the seconds counter internally.  HRP generated
 *	  CDS records will almost always have the TQF
 *	  bit set.
 * returns 1 to indicate internal time or a 0 to indicate S.C time.
 */
long UTIL_extract_Qflag (struct CDS_buffer *buffer)
{                                       /* data buffer */
  return (buffer->packet.cds.header[10] >> 0) & 0x01;
}

/********************************************************
 *							*
 *	Return the MP typ field from the MP header	*
 *			4 bits				*
 *	VALIDATE MP header type!!!	(-1 meand BAD!)	*
 *							*
 ********************************************************/
 /*
  * Extract RTI field from RPWS minipacket
  */
long UTIL_extract_MP_RTI (struct MP_buffer *buffer)
{                                       /* data buffer */
  return buffer->packet.mpp.mini_packet[2] & 0x07;
}

 /*
  * Extract minipacket type field from RPWS minipacket
  */
long UTIL_extract_MP_bits (struct MP_buffer *buffer)
{                                       /* data buffer */
  long i;

  i = buffer->packet.mpp.mini_packet[0];
  i = i >> 4;
  return i;
}                                       /* top 4 bits */

 /*
  * Extract validated minipacket type field from RPWS minipacket
  *  Extract minipacket type if valid or return a -1.
  * returns minipacket type for valid packets, -1 for invalid packet
  */
long UTIL_extract_MP_type (struct MP_buffer *buffer)
{                                       /* data buffer */
  static long valid[16] = { 0,          /*  0   */
    1,                                  /*  1   */
    2,                                  /*  2   */
    -1,                                 /*  3   */
    4,                                  /*  4   */
    -1,                                 /*  5   */
    -1,                                 /*  6   */
    7,                                  /*  7   */
    8,                                  /*  8   */
    -1,                                 /*  9   */
    -1,                                 /*  10  */
    11,                                 /*  11  */
    -1,                                 /*  12  */
    13,                                 /*  13  */
    14,                                 /*  14  */
    15
  };                                    /*  15  */
  return valid[UTIL_extract_MP_bits (buffer)];
}

/********************************************************
 *							*
 *	Return the MP typ field from the MP header	*
 *			4 bits				*
 *	VALIDATE MP header type!!!	(-1 meand BAD!)	*
 *							*
 ********************************************************/
 /*
  * Extract unadjusted minipacket length from minipacket
  *  Extract the length expressed in the minipacket.  This is 3
  *  less than the actual length.
  * returns packet length
  */
long UTIL_extract_MP_length (struct MP_buffer *buffer)
{                                       /* data buffer */
  long i;

  i = buffer->packet.mpp.mini_packet[0] & 0x0F;
  i = i * 256 + buffer->packet.mpp.mini_packet[1];
  return i;
}

 /*
  * Extract unadjusted minipacket length from RPWS record
  *  Extract the length expressed in the minipacket.  This is 3
  *  less than the actual length.  This is not limited to a
  *  12 bit length field.  It looks in the RPWS record for
  *  an extended length if this is a unsegmented minipacket.
  * returns packet length
  */
long UTIL_MP_length (struct MP_buffer *buffer)
{                                       /* data buffer */
  long i;

  i = UTIL_extract_MP_length (buffer);
  if (!i)
    i = buffer->packet.index.data_length;
  return i;
}

/*
 *	Returns a pointer to a text string that describes the current mini packet		
 *	IF we are on the ball, this string will match	
 *	  something in the documentation (somewhere)	
 *returns pointer to text string
 */
char *UTIL_extract_MP_packet_type (struct MP_buffer *buffer)
{                                       /* data buffer */
  long i;

  i = UTIL_extract_MP_type (buffer) & 0x0F;
  return &UTIL_mp_type[i][0];
}

 /*
  * extract minipacket type
  * * returns minipacket type
  */
char *UTIL_get_MP_packet_type (long i)
{                                       /* */
  /*
   * data buffer 
   */
  return &UTIL_mp_type[i][0];
}

/********************************************************
 *	Extract the 16 bit time field from the MP header.
 * All minipackets express time as 13 bits of seconds and 3 bits
 * of RTI.  This call extracts this time information.
 * returns 16 bit RTI field
 ********************************************************/
unsigned long UTIL_extract_MP_TIME (struct MP_buffer *buffer)
{                                       /* data buffer */
  long i;

  i = buffer->packet.mpp.mini_packet[3];
  i = i * 256 + buffer->packet.mpp.mini_packet[2];
  return i;
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
unsigned long UTIL_extract_TIME (struct CDS_buffer *buffer)
{                                       /* data buffer */
  unsigned long i[4];

  i[0] = buffer->packet.cds.header[11] << 0;
  i[1] = buffer->packet.cds.header[8] << 8;
  i[2] = buffer->packet.cds.header[9] << 16;
  i[3] = buffer->packet.cds.header[6] << 24;
  return i[0] | i[1] | i[2] | i[3];
}

 /*
  * Extract timetag from CDS record
  * * Recovers the 3 bit CDS SCLK/RTI.  RTI is returned with this call
  * *returns packet.cds.header[0], the RTI field
  */
unsigned long UTIL_extract_CDS_RTI (struct CDS_buffer *buffer)
{                                       /* data buffer */
  unsigned long i;

  i = buffer->packet.cds.header[10] << 0;
  return i;
}

 /*
  * Extract timetag from CDS record
  * * This result, when non-zero, may be added to the seconds portion of SCLK to 
  * * obtain UNIX time.  Accuracy is compromised as there is no accounting
  * * for RTI with this call.
  * *returns packet.cds_tag.header.epoch
  */
unsigned long UTIL_extract_EPOCH (struct CDS_buffer *buffer)
{                                       /* data buffer */
  unsigned long i[4];

  i[0] = buffer->packet.cds_tag.epoch[0] << 24;
  i[1] = buffer->packet.cds_tag.epoch[1] << 16;
  i[2] = buffer->packet.cds_tag.epoch[2] << 8;
  i[3] = buffer->packet.cds_tag.epoch[3] << 0;
  return i[0] | i[1] | i[2] | i[3];
}

/********************************************************
 *							*
 *	Extract the 32 bit time field from the internal	*
 *	  packet header.  Shuffle it around to 		*
 *	  make it into a native integer.		*
 *							*
 ********************************************************/

 /*
  * Extract begining CDS time tag from MP record
  * * This is the timetag associated with the CDS record
  * * that contains the begining of the minipacket.
  * *returns packet.cds_tag.begin
  */
unsigned long UTIL_extract_PKT_TIME (struct CDS_buffer *buffer)
{                                       /* data buffer */
  unsigned long i[4];

  i[0] = buffer->packet.cds_tag.begin[3] << 0;
  i[1] = buffer->packet.cds_tag.begin[2] << 8;
  i[2] = buffer->packet.cds_tag.begin[1] << 16;
  i[3] = buffer->packet.cds_tag.begin[0] << 24;
  return i[0] | i[1] | i[2] | i[3];
}

 /*
  * Extract begining CDS time tag from MP record  
  * * This is the timetag associated with the CDS record
  * * that contains the end of the minipacket.
  * *returns packet.cds_tag.end
  */
unsigned long UTIL_extract_PKT_TIME2 (struct CDS_buffer *buffer)
{                                       /* data buffer */
  unsigned long i[4];

  i[0] = buffer->packet.cds_tag.end[3] << 0;
  i[1] = buffer->packet.cds_tag.end[2] << 8;
  i[2] = buffer->packet.cds_tag.end[1] << 16;
  i[3] = buffer->packet.cds_tag.end[0] << 24;
  return i[0] | i[1] | i[2] | i[3];
}

/********************************************************
 *	Gets Workstation time in BCD format as 		*
 *	yyyy ddd hhmmss  ddd is the day of year (1-366) *
 *	The year we get is biased from 1900		*
 *`	  (so year 2000 shows up as 100)		*
 ********************************************************/
long UTIL_get_time (struct BCD_Time_Tag *BCD_time)
{                                       /* workstation time tag */
  time_t tp;
  struct tm tpp;

  time (&tp);
  tpp = *localtime (&tp);
  if (tpp.tm_year < 100)
    BCD_time->year[0] = 0x19;
  else
    BCD_time->year[0] = 0x20;

  BCD_time->year[1] = ((((tpp.tm_year % 100) / 10) & 0xf) << 4) |
    ((tpp.tm_year % 10) & 0xf);

  BCD_time->doy[0] = (((tpp.tm_yday + 1) / 100) & 0xf);
  BCD_time->doy[1] =
    (((((tpp.tm_yday + 1) % 100) / 10) & 0xf) << 4) | (((tpp.tm_yday +
                                                         1) % 10) & 0xf);

  BCD_time->hour =
    (((tpp.tm_hour / 10) & 0xf) << 4) | ((tpp.tm_hour % 10) & 0xf);
  BCD_time->minute =
    (((tpp.tm_min / 10) & 0xf) << 4) | ((tpp.tm_min % 10) & 0xf);
  BCD_time->second =
    (((tpp.tm_sec / 10) & 0xf) << 4) | ((tpp.tm_sec % 10) & 0xf);

  return 1;
}
