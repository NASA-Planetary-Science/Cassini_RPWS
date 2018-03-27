
/* utild - decompress utilities
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
#include <dos.h>
#endif

#define _utild_h
#include <rtiu.h>
#include <util.h>
long UTIL_DUST_decompress (struct MP_buffer *in_buffer,
                           struct RPWS_buffer *r_buffer);
#include <decomp.h>


 /***********************************************
  *	Decompress control			*
  ***********************************************/

static char ident[] = { "IDENT utild.c 16-June-2000" };
static struct RPWS_buffer *outbuf = { NULL };
static int more_data;

  /*
   * Decompress segemented minipackets
   * This routine will attempt to decompress data that is
   * compressed on an individual bases.  This includes
   * WFR, MFR, L/P, and WBR that is compressed using the DCP.
   * returns buffer, decompressed if possible
   */
unsigned char *UTIL_decompress (struct MP_buffer *inbuf)
{                                       /* input buffer (MP) */
  static unsigned first = 1;
  int more_data = 0;

  if (first) {
    first = 0;
    outbuf = (struct RPWS_buffer *) malloc (sizeof (struct RPWS_buffer) * 2);
  }

  if (!outbuf)
    return NULL;

        /********************************************************
         *      Copy the header stuff                           *
         *      PRE-SET length to ZERO, so application          *
         *       decompress can ignore if compression fails     *
         *      IF decompress is successful, it should load     *
         *       the f_length field (using UTIL_long_to_MSB)    *
         *       with a record length.  If compression does     *
         *       NOT overflow the space that would be occupied  *
         *       by the input, decompression may elect to       *
         *       simply copy f_length from the input buffer     *
         *       to the output buffer                           *
         ********************************************************/
  outbuf->f_length = 0;
  outbuf->record_type = inbuf->record_type;
  outbuf->status = inbuf->status;
  memcpy (outbuf->packet.header.data,
          inbuf->packet.header.data, sizeof (inbuf->packet.header.data));

        /********************************************************
         *      Call application-specific decompress activity   *
         ********************************************************/
  switch ((int) UTIL_extract_MP_bits (inbuf)) {
   case PACKET_TYPE_wfr:
     switch (outbuf->record_type & 0x0FF00) {
      case DATA_MP_complete_segment:
        /*
         * outbuf->packet.compress.result =
         * UTIL_DCC_decompress(inbuf, outbuf,RPWS_PKTMAX ); /*
         */
        outbuf->packet.compress.result == STATUS_COMPRESS_not_my_data;
        break;
      default:
        outbuf->packet.compress.result = UTIL_DCP_decompress (inbuf, outbuf);
        break;
     }
     if (outbuf->packet.compress.result == STATUS_COMPRESS_not_my_data)
       outbuf->packet.compress.result = STATUS_COMPRESS_uncompressed;
     if (outbuf->packet.compress.result == STATUS_COMPRESS_more_data)
       more_data = 1;
     break;

   case PACKET_TYPE_wbr:
     switch (outbuf->record_type & 0x0FF00) {
      case DATA_MP_complete_segment:
        /*
         * outbuf->packet.compress.result =
         * UTIL_DCC_decompress(inbuf, outbuf,RPWS_PKTMAX ); /*
         */
        outbuf->packet.compress.result == STATUS_COMPRESS_not_my_data;
        break;
      default:
        outbuf->packet.compress.result = STATUS_COMPRESS_not_my_data;
        break;
     }
     if (outbuf->packet.compress.result == STATUS_COMPRESS_not_my_data) {
       outbuf->packet.compress.result = UTIL_DUST_decompress (inbuf, outbuf);
       outbuf->packet.compress.result = STATUS_COMPRESS_uncompressed;
     }
     if (outbuf->packet.compress.result == STATUS_COMPRESS_more_data)
       more_data = 1;
     break;
   case PACKET_TYPE_mfr:
     outbuf->packet.compress.result = UTIL_MFR_decompress (inbuf, outbuf);
     break;

 /********************************************* May 1995, add LP decompress ******************/
   case PACKET_TYPE_lp:
     outbuf->packet.compress.result = UTIL_DCP_decompress (inbuf, outbuf);
     break;

 /********************************************************************************************/
   default:
     break;
  }

  if (more_data)
    return NULL;
  if (outbuf->f_length) {
    outbuf->f_length = outbuf->f_length + 15;;  /* 16 byte align */
    outbuf->f_length = outbuf->f_length + 4;;   /* add length */
    outbuf->f_length = outbuf->f_length & 0xFFFFFFF0;;  /* truncate */
    outbuf->f_length = outbuf->f_length - 4;;   /* remove length */
    UTIL_r_length ((long *) outbuf, outbuf->f_length);
    return (unsigned char *) outbuf;
  } else {

    inbuf->packet.compress.result = STATUS_COMPRESS_unknown;
    return (unsigned char *) inbuf;
  }
}
