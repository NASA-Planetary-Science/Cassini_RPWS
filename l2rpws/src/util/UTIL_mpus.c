
/****************************************************************************/

/*				   MPUS.C                                   */

/****************************************************************************/

/*	This routine will attempt to turn segmented mini-packets into       */

/* unsegmented data sets.  MP_buffer -> via mpus -> RPWS_buffer             */

/* No attempt for error checking will be made.                              */

/* 									    */

/*	AUTHOR: 	Robert Johnson					    */

/*	DATE:		May 25, 1995					    */

/*	Updates: 							    */

/*	     November 23, 1998.				    		    */

/*	        Nothing Yet. */

/*	     */

/*	     */

/*	     July 30, 1996.  				    		    */

/*		Stamps packet length for MFR,LFDR,Unseg. Pkts. due to MP    */

/*		not consistently stamping the right length.                 */

/*	    August 16, 1996.  				    		    */

/*		MSF for WBR data means two bytes now, not one.		    */

/*	    June 18, 1997.						    */

/*		Langmuir Probe packet header changed to accomodate          */

/*		    segmentation.	    				    */

/****************************************************************************/

#include <stdio.h>
#include "rtiu.h"
#include "util.h"
#include "mpus.h"                       /* uses definitions in util.h */

#include "Cext.h"

static char ident[] = { "IDENT UTIL_mpus.c 16-June-2000" };
char UTIL_MPUS_VERSION[] = "MPUS F7.5";

long mp2rpws (struct MP_buffer *mpbin, struct RPWS_buffer *rpwsbout);   /* copies the MPMAX data structure from the MP_buffer to the RPWS_buffer */
long get_mp_len (struct MP_buffer *mpbin);
long get_mp_rti (struct MP_buffer *mpbin);
long get_mp_size (struct MP_buffer *mpbin);
long get_mp_seg (struct MP_buffer *mpbin);
long get_mp_msf (struct MP_buffer *mpbin);

long put_mp_len (struct RPWS_buffer *rpwsbout, long mp_length);

long fix_hfr_seg (struct MP_buffer *mpbin, struct RPWS_buffer *rpwsbout);
long fix_lp_seg (struct MP_buffer *mpbin, struct RPWS_buffer *rpwsbout);
long fix_wbr_seg (struct MP_buffer *mpbin, struct RPWS_buffer *rpwsbout);
long fix_wfr_seg (struct MP_buffer *mpbin, struct RPWS_buffer *rpwsbout);

long UTIL_mpus (struct MP_buffer *mpbin, struct RPWS_buffer *rpwsbout)
{
  int rtn_sts;
  long pkt_type;

  pkt_type = UTIL_extract_MP_type (mpbin);

  switch (pkt_type) {
   case PACKET_TYPE_dust:              /* The header infor is copied to a   */
   case PACKET_TYPE_fill:              /* RPWS_buffer and passed to stdout. */
   case PACKET_TYPE_lfdr:              /* These packets are not segmented.  */
   case PACKET_TYPE_mro:               /* mp_buffer is now rpws_buffer.     */
   case PACKET_TYPE_mfr:               /* MPUS stamps 0x4xx in status field */
   case PACKET_TYPE_stim:
     rtn_sts = mp2rpws (mpbin, rpwsbout);
     break;
   case PACKET_TYPE_hfr:
     rtn_sts = fix_hfr_seg (mpbin, rpwsbout);
     break;
   case PACKET_TYPE_lp:
     rtn_sts = fix_lp_seg (mpbin, rpwsbout);
     break;
   case PACKET_TYPE_wfr:
     rtn_sts = fix_wfr_seg (mpbin, rpwsbout);
     break;
   case PACKET_TYPE_wbr:
     rtn_sts = fix_wbr_seg (mpbin, rpwsbout);
     break;
   default:
     rtn_sts = STATUS_MPUS_not_my_data;
     break;
  }

  return rtn_sts;
}


/******************************************************************************/

/******************************************************************************/

/******************************************************************************/


long mp2rpws (struct MP_buffer *mpbin, struct RPWS_buffer *rpwsbout)
{                                       /* used to transfer mp buffers to rpws buffers untouched */
  long mp_len, loop, f_len, pkt_type;

  pkt_type = UTIL_extract_MP_type (mpbin);
  pkt_type |= 0x0400;

  rpwsbout->record_type = pkt_type;
  rpwsbout->status = mpbin->status;

  rpwsbout->packet.mpx.segment_count = mpbin->packet.mpx.segment_count;
  rpwsbout->packet.mpx.segment_number = mpbin->packet.mpx.segment_number;
  rpwsbout->packet.mpx.record_count = mpbin->packet.mpx.record_count;
  rpwsbout->packet.mpx.record_number = mpbin->packet.mpx.record_number;

  for (loop = 0; loop < (256 - 16); loop++) {
    rpwsbout->packet.mpx.filler1[loop] = mpbin->packet.mpx.filler1[loop];
  }

  mp_len = (mpbin->packet.mpx.mini_packet[0] & 0x00F) << 8;
  mp_len |= (mpbin->packet.mpx.mini_packet[1] & 0x0FF);
  for (loop = 0; loop <= (mp_len + 2); loop++)  /* 2 = pkt_id/length and length (2 bytes) */
    rpwsbout->packet.mpx.mini_packet[loop] =
      mpbin->packet.mpx.mini_packet[loop];

  put_mp_len (rpwsbout, mp_len + 3);

  f_len = 3 * sizeof (long) + 256 + loop;
  UTIL_lenbuffr2_RPWS (rpwsbout, f_len);

  return STATUS_MPUS_success;
}

/******************************************************************************/

long fix_hfr_seg (struct MP_buffer *mpbin, struct RPWS_buffer *rpwsbout)
{
  long loop;

  static unsigned char buf[RPWS_PKTMAX];        /* whole rpws_buffer, as of May 22, 1995 */
  static int first_time;
  static long index;                    /* keep track of place in buff */
  static long mp_len, mp_rti, mp_eof, mp_seg_num;

  mp_len = ((mpbin->packet.mpx.mini_packet[0] & 0x00F) << 8);   /* upper nibble of length-1 */
  mp_len |= (mpbin->packet.mpx.mini_packet[1] & 0x0FF); /* lower byte of length-1 */
  mp_rti = mpbin->packet.mpx.mini_packet[2] & 0x0FF;    /* low byte of RTI word */
  mp_rti |= ((mpbin->packet.mpx.mini_packet[3] & 0x0FF) << 8);  /* high byte of RTI word */
  mp_eof = (mpbin->packet.mpx.mini_packet[4] & 0x080);  /* EOF bit set to 1 on last segment */
  mp_seg_num = (mpbin->packet.mpx.mini_packet[4] & 0x07F);      /* segment number */

  if (!first_time) {
    ++first_time;
    index = 0;                          /* reset buffer index */
    buf[index++] = mpbin->packet.mpx.mini_packet[0] & 0x0FF;    /* mp header : packet type/length - 1 */
    buf[index++] = mpbin->packet.mpx.mini_packet[1] & 0x0FF;    /* mp header : length - 1 */
    buf[index++] = mpbin->packet.mpx.mini_packet[2] & 0x0FF;    /* mp header : RTI */
    buf[index++] = mpbin->packet.mpx.mini_packet[3] & 0x0FF;    /* mp header : RTI */
    buf[index++] = mpbin->packet.mpx.mini_packet[4] & 0x0FF;    /* mp header : EOF/Seg. Num. */
  }

  for (loop = 5; loop <= (mp_len + 2); loop++) {
    buf[index++] = mpbin->packet.mpx.mini_packet[loop]; /* data allways starts here */
  }

  if (mp_eof) {
    loop = 3 * (sizeof (long)) + 256 + index;
    UTIL_lenbuffr2_RPWS (rpwsbout, loop);
    rpwsbout->record_type = DATA_MPC_HFR;
    rpwsbout->status = STATUS_MPUS_success;
    rpwsbout->packet.mpx.segment_count = 0x00;
    rpwsbout->packet.mpx.segment_number = 0x00;
    rpwsbout->packet.mpx.record_count = 0x00;
    rpwsbout->packet.mpx.record_number = 0x00;

    for (loop = 0; loop < (256 - 16); loop++) {
      rpwsbout->packet.mpx.filler1[loop] = mpbin->packet.mpx.filler1[loop];
    }

    buf[4] |= 0x080;                    /* mp header : EOF, set / segment number, whatever the first packet was  */

    for (loop = 0; loop < index; loop++) {
      rpwsbout->packet.mpx.mini_packet[loop] = buf[loop];
    }

    put_mp_len (rpwsbout, index);

    first_time = 0;
    return STATUS_MPUS_success;
  }
  /*
   * NOTE: rpwsbout->packet.mpx.filler2 is unable to exist any more, ie. data overwrites it 
   */
  return STATUS_MPUS_more_data;         /* need more data to complete the data set */
}

/******************************************************************************/

long fix_lp_seg (struct MP_buffer *mpbin, struct RPWS_buffer *rpwsbout)
{
  long loop, mp_data_offset;

  static unsigned char buf[RPWS_PKTMAX];        /* whole rpws_buffer, as of May 22, 1995 */
  static int first_time;
  static long index;                    /* keep track of place in buff */
  static long mp_len, mp_rti, mp_size, mp_seg, mp_msf;

/* static long mp_len_old,mp_rti_old,mp_size_old,mp_seg_old; may be usefull for error checking */

  mp_len = get_mp_len (mpbin);
  mp_rti = get_mp_rti (mpbin);
  mp_size = mp_seg = mpbin->packet.mpx.mini_packet[4] & 0x0FF;
  mp_size = (mp_size >> 6) & 0x03;
  mp_seg = (mp_seg >> 4) & 0x03;

  if (!first_time) {
    ++first_time;
    index = 0;                          /* reset buffer index */
    buf[index++] = mpbin->packet.mpx.mini_packet[0] & 0x0FF;    /* mp header : packet type/length - 1 */
    buf[index++] = mpbin->packet.mpx.mini_packet[1] & 0x0FF;    /* mp header : length - 1 */
    buf[index++] = mpbin->packet.mpx.mini_packet[2] & 0x0FF;    /* mp header : RTI */
    buf[index++] = mpbin->packet.mpx.mini_packet[3] & 0x0FF;    /* mp header : RTI */
    buf[index++] = mpbin->packet.mpx.mini_packet[4] & 0x0FF;    /* mp header : Size/Seg. Num. */
    buf[index++] = mpbin->packet.mpx.mini_packet[5] & 0x0FF;    /* mp header :  */
    buf[index++] = mpbin->packet.mpx.mini_packet[6] & 0x0FF;    /* mp header :  */
    buf[index++] = mpbin->packet.mpx.mini_packet[7] & 0x0FF;    /* mp header :  */
    buf[index++] = mpbin->packet.mpx.mini_packet[8] & 0x0FF;    /* mp header :  */
    buf[index++] = mpbin->packet.mps.mini_packet[9] & 0x0FF;
  }

  mp_data_offset = 10;                  /* copy mp data area to temporary buffer */
  for (loop = mp_data_offset; loop <= (mp_len + 2); loop++) {
    buf[index++] = mpbin->packet.mpx.mini_packet[loop]; /* data allways starts here */
  }

  if (mp_seg == mp_size) {
    loop = 3 * (sizeof (long)) + 256 + index;
    UTIL_lenbuffr2_RPWS (rpwsbout, loop);
    rpwsbout->record_type = DATA_MPC_WBR;
    rpwsbout->packet.mpx.segment_count = 0x00;
    rpwsbout->packet.mpx.segment_number = 0x00;
    rpwsbout->packet.mpx.record_count = 0x00;
    rpwsbout->packet.mpx.record_number = 0x00;

    for (loop = 0; loop < (256 - 16); loop++) {
      rpwsbout->packet.mpx.filler1[loop] = mpbin->packet.mpx.filler1[loop];
    }

    buf[4] &= 0x0F;                     /* size and segment number, zero */

    for (loop = 0; loop < index; loop++) {
      rpwsbout->packet.mpx.mini_packet[loop] = buf[loop];
    }

    put_mp_len (rpwsbout, index);

    first_time = 0;
    return STATUS_MPUS_success;
  }
  /*
   * NOTE: rpwsbout->packet.mpx.filler2 is unable to exist any more, ie. data overwrites it 
   */
  return STATUS_MPUS_more_data;         /* need more data to complete the data set */
}

/******************************************************************************/

long fix_wbr_seg (struct MP_buffer *mpbin, struct RPWS_buffer *rpwsbout)
{
  long loop, mp_data_offset;

  static unsigned char buf[RPWS_PKTMAX];        /* whole rpws_buffer, as of May 22, 1995 */
  static int first_time;
  static long index;                    /* keep track of place in buff */
  static long mp_len, mp_rti, mp_size, mp_seg, mp_msf;

/* static long mp_len_old,mp_rti_old,mp_size_old,mp_seg_old; may be usefull for error checking */

  mp_len = get_mp_len (mpbin);
  mp_rti = get_mp_rti (mpbin);
  mp_size = get_mp_size (mpbin);
  mp_seg = get_mp_seg (mpbin);
  mp_msf = get_mp_msf (mpbin);

  if (!first_time) {
    ++first_time;
    index = 0;                          /* reset buffer index */
    buf[index++] = mpbin->packet.mpx.mini_packet[0] & 0x0FF;    /* mp header : packet type/length - 1 */
    buf[index++] = mpbin->packet.mpx.mini_packet[1] & 0x0FF;    /* mp header : length - 1 */
    buf[index++] = mpbin->packet.mpx.mini_packet[2] & 0x0FF;    /* mp header : RTI */
    buf[index++] = mpbin->packet.mpx.mini_packet[3] & 0x0FF;    /* mp header : RTI */
    buf[index++] = mpbin->packet.mpx.mini_packet[4] & 0x0FF;    /* mp header : Size/Seg. Num. */
    buf[index++] = mpbin->packet.mpx.mini_packet[5] & 0x0FF;    /* mp header : Gains,FB */
    buf[index++] = mpbin->packet.mpx.mini_packet[6] & 0x0FF;    /* mp header : Comp,MSF,Antenna */
    buf[index++] = mpbin->packet.mpx.mini_packet[7] & 0x0FF;    /* mp header : AGC value */
    if (mp_msf) {
      buf[index++] = mpbin->packet.mpx.mini_packet[8] & 0x0FF;  /* mp header : HFR XLATE or LP DAC 0 */
      buf[index++] = mpbin->packet.mps.mini_packet[9] & 0x0FF;
    }
  }

  if (mp_msf)                           /* copy mp data area to temporary buffer */
    mp_data_offset = 10;
  else
    mp_data_offset = 8;
  for (loop = mp_data_offset; loop <= (mp_len + 2); loop++) {
    buf[index++] = mpbin->packet.mpx.mini_packet[loop]; /* data allways starts here */
  }

  if (mp_seg == mp_size) {
    loop = 3 * (sizeof (long)) + 256 + index;
    UTIL_lenbuffr2_RPWS (rpwsbout, loop);
    rpwsbout->record_type = DATA_MPC_WBR;
    rpwsbout->packet.mpx.segment_count = 0x00;
    rpwsbout->packet.mpx.segment_number = 0x00;
    rpwsbout->packet.mpx.record_count = 0x00;
    rpwsbout->packet.mpx.record_number = 0x00;

    for (loop = 0; loop < (256 - 16); loop++) {
      rpwsbout->packet.mpx.filler1[loop] = mpbin->packet.mpx.filler1[loop];
    }

    buf[4] = 0x000;                     /* size and segment number, zero */

    for (loop = 0; loop < index; loop++) {
      rpwsbout->packet.mpx.mini_packet[loop] = buf[loop];
    }

    put_mp_len (rpwsbout, index);

    first_time = 0;
    return STATUS_MPUS_success;
  }
  /*
   * NOTE: rpwsbout->packet.mpx.filler2 is unable to exist any more, ie. data overwrites it 
   */
  return STATUS_MPUS_more_data;         /* need more data to complete the data set */
}

/******************************************************************************/

long fix_wfr_seg (struct MP_buffer *mpbin, struct RPWS_buffer *rpwsbout)
{
  long loop, mp_data_offset;

  static unsigned char cBuf0[RPWS_PKTMAX];      /* whole rpws_buffer, as of May 22, 1995 */
  static unsigned char cBuf1[RPWS_PKTMAX];      /* whole rpws_buffer, as of May 22, 1995 */
  static long index;                    /* keep track of place in buff */

  static Bool bFirstTime = True, bSegDone = False;
  static char *pOutBuf, *pBuf;
  static unsigned char cOutBuf[RPWS_PKTMAX];
  static unsigned char *pMpData;

  static long nLen, nRti, nSize, nSeq, nMsf;
  static long nOldLen, nOldRti, nOldSize, nOldSeq, nOldMsf;


  if (bFirstTime == True) {
    bFirstTime = False;
    bSegDone = False;
    nLen = nRti = nSize = nSeq = 0;
    nOldLen = nOldRti = nOldSize = nOldSeq = 0;
    pOutbuf = cBuf0;
    pBuf = cOutBuf;
  }

  /*
   * Decode the Current Packet Status 
   */
  nLen = get_mp_len (mpbin);
  nRti = get_mp_rti (mpbin);
  nSize = get_mp_size (mpbin);
  nSeg = get_mp_seg (mpbin);
  nMsf = get_mp_msf (mpbin);

/* 
Is the Data Set is complete
   1. Does the old data match the new data
   2. All segments are pieced together
*/
  if ((nRti != nOldRti) || (nSize == nSeg)) {
    bSegDone = True;
    pBuf ^= pOutBuf;
    pOutBuf ^= pBuf;
    pBuf ^= pOutBuf;
    nOldRti = nRti;
  }

  if (get_mp_msf (mpbin))
    nHeader = 10;
  else
    nHeader = 8;


  /*
   * If first Packet, copy header 
   */
  if (nSeg == 0) {
    pMpData = mpbin->packet.mpx.mini_packet;
    for (nLoop = 0; nLoop < nHeader; nLoop++)
      *pBuf++ = *pMpData++;             /* Copy Header Information */
    nOldSeg = nSeg - 1;
    nOldRti = nRti;
  }

  if ((nSeg > nOldSeg) && (nRti == nOldRti)) {  /* Add segment to current data buffer */
    for (nLoop = nHeader; nLoop <= (nLen + 2); nLoop++)
      *pBuf++ = *pMpData++;             /* Copy Mini-Packet Data */
    nOldSeg = nSeg;
  }




  if (!first_time) {
    ++first_time;
    index = 0;                          /* reset buffer index */
    buf[index++] = mpbin->packet.mpx.mini_packet[0] & 0x0FF;    /* packet type/length - 1 */
    buf[index++] = mpbin->packet.mpx.mini_packet[1] & 0x0FF;    /* length - 1 */
    buf[index++] = mpbin->packet.mpx.mini_packet[2] & 0x0FF;    /* RTI */
    buf[index++] = mpbin->packet.mpx.mini_packet[3] & 0x0FF;    /* RTI */
    buf[index++] = mpbin->packet.mpx.mini_packet[4] & 0x0FF;    /* Size/Seg. Num. */
    buf[index++] = mpbin->packet.mpx.mini_packet[5] & 0x0FF;    /* Gains,FB,LP */
    buf[index++] = mpbin->packet.mpx.mini_packet[6] & 0x0FF;    /* Comp,MSF,Antenna */
    buf[index++] = mpbin->packet.mpx.mini_packet[7] & 0x0FF;    /* Chan,Mode */
    if (mp_msf) {
      buf[index++] = mpbin->packet.mpx.mini_packet[8] & 0x0FF;  /* mp header : LP DAC 0 */
      buf[index++] = mpbin->packet.mpx.mini_packet[9] & 0x0FF;  /* mp header : LP DAC 1 */
    }
  }

  if (mp_msf)                           /* copy data to buffer */
    mp_data_offset = 10;
  else
    mp_data_offset = 8;
  for (loop = mp_data_offset; loop <= (mp_len + 2); loop++) {
    buf[index++] = mpbin->packet.mpx.mini_packet[loop]; /* data allways starts here */
  }

  if (mp_seg == mp_size) {
    loop = 3 * (sizeof (long)) + 256 + index;   /* stuff f_length field */
    UTIL_lenbuffr2_RPWS (rpwsbout, loop);
    rpwsbout->record_type = DATA_MPC_WFR;
    rpwsbout->packet.mpx.segment_count = 0x00;
    rpwsbout->packet.mpx.segment_number = 0x00;
    rpwsbout->packet.mpx.record_count = 0x00;
    rpwsbout->packet.mpx.record_number = 0x00;

    for (loop = 0; loop < (256 - 16); loop++) {
      rpwsbout->packet.mpx.filler1[loop] = mpbin->packet.mpx.filler1[loop];
    }

    buf[4] = 0x000;                     /* size and segment number, zero */

    for (loop = 0; loop < index; loop++) {
      rpwsbout->packet.mpx.mini_packet[loop] = buf[loop];
    }

    put_mp_len (rpwsbout, index);

    first_time = 0;
    return STATUS_MPUS_success;
  }
  /*
   * NOTE: rpwsbout->packet.mpx.filler2 is unable to exist any more, ie. data overwrites it 
   */
  return STATUS_MPUS_more_data;         /* need more data to complete the data set */
}



/******************************************************************************/

/*          see Willy Robison's "Where Mini-Packets Live" document            */

/******************************************************************************/

long get_mp_len (struct MP_buffer *mpbin)
{
  long x;

  x = (mpbin->packet.mpx.mini_packet[0] & 0x00F) << 8;
  x |= (mpbin->packet.mpx.mini_packet[1] & 0x0FF);
  return x;
}

long get_mp_rti (struct MP_buffer *mpbin)
{
  long x;

  x = (mpbin->packet.mpx.mini_packet[2] & 0x0FF);
  x |= (mpbin->packet.mpx.mini_packet[3] & 0x0FF) << 8;
  return x;
}

long get_mp_size (struct MP_buffer *mpbin)
{
  long x;

  x = (mpbin->packet.mpx.mini_packet[4] >> 4) & 0x0F;
  return x;
}

long get_mp_seg (struct MP_buffer *mpbin)
{
  long x;

  x = (mpbin->packet.mpx.mini_packet[4] & 0x00F);
  return x;
}

long get_mp_msf (struct MP_buffer *mpbin)
{
  long x;

  x = (mpbin->packet.mpx.mini_packet[6] & 0x008);
  return x;
}

long put_mp_len (struct RPWS_buffer *rpwsbout, long mp_length)
{
  if (mp_length < 4096) {
    rpwsbout->packet.mpx.mini_packet[0] &= 0x0F0;       /* zero old mp length */
    rpwsbout->packet.mpx.mini_packet[0] |= (((mp_length - 3) & 0x0F00) >> 8);   /* high nibble is packet type */
    rpwsbout->packet.mpx.mini_packet[1] = ((mp_length - 3) & 0x00FF);
  } else {
    rpwsbout->packet.mpx.mini_packet[0] &= 0x0F0;       /* zero out lenght - 1 */
    rpwsbout->packet.mpx.mini_packet[1] = 0x000;
  }

  rpwsbout->packet.index.data_start = (mp_length - 3);
  rpwsbout->packet.index.data_length = (mp_length - 3);

  return (mp_length - 3);
}
