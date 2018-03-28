#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasMiniPacket.h>
#include <rpwstlm/CasWfr.h>
#include <rpwstlm/CasWbr.h>
#include <rpwstlm/CasType.h>

#include "usdc.h"
#include "rice.h"

/*
#include "telemetry.h"
*/

struct
{
  long inbits, outbits;
  long inbytes, outbytes;
  long bits_per_sample;
} Stats;






/*
long dcc_decompress(TELEMETRY *tlm);
long dcc_unleave(TELEMETRY *tlmin,TELEMETRY ***tlmout);
*/

static unsigned inbuffer[65536], inlength;
static unsigned outbuffer[65536], outlength;




Ulong CasUsdc_DccDecompress (CasRecord * pRec)
{
  int l, m;
  unsigned bits_per_sample;

  Uchar *pMp = pRec->data;
  int nHdrLen, nMpLen, nZeroLen;
  unsigned long nStatus, nRiceErrors;

/*
int i;
unsigned bits_per_sample;
long l,m,mp_data_offset,mp_data_length;
static unsigned inbuffer[DCC_DECOMPRESSION_BUFFER_SIZE],inlength;
static unsigned outbuffer[DCC_DECOMPRESSION_BUFFER_SIZE],outlength;
UNSEGMENTED_MINI_PACKET *usmp=&(tlm->packet.usmp);
*/

  pRec->status.gnd_status &= ~CasRice_Mask;
  pRec->status.gnd_status |= CasRice_Processed;

  switch (pMp[0] & 0xF0) {
   case 0x80:                          /* wfr packet */
     if (CasWfr_bDccCompressed (pMp) == false)
       return 0;
     bits_per_sample = 12;
     if (CasWfr_bMsf (pMp) == true)
       nHdrLen = 10;
     else
       nHdrLen = 8;
     break;
   case 0xE0:                          /* wbr packet */
     if (CasWbr_bDccCompressed (pMp) == false)
       return 0;
     bits_per_sample = 8;
     if (CasWbr_bMsf (pMp) == true)
       nHdrLen = 10;
     else
       nHdrLen = 8;
     if (CasWbr_bHighBand (pMp) == true) {      /* In 80Khz, the flight software */
       if (CasWbr_bDccJunkSample (pMp) == true) /* may/may not add a junk sample */
         nHdrLen += 2;                  /* to the beginning of the data. */
     }
     break;
   default:                            /* not dcp compressed */
     return 0;
  }


  nMpLen = pRec->status.packet_length + 3;      /* real mini-packet length */
  for (l = nHdrLen, inlength = 0; l < nMpLen; inlength++, l++) {
    inbuffer[inlength] = pMp[l++];      /* msb */
    inbuffer[inlength] <<= 8;
    inbuffer[inlength] |= pMp[l];       /* lsb */
  }
  Stats.inbytes = nMpLen;               /* the real packet length (mp_len+3) */
  Stats.inbits = inlength * 16;
  Stats.bits_per_sample = bits_per_sample;

  if ((nZeroLen = CasUsdc_ZeroCheck (pRec)) > 0) {      /* found zeros */
    if (nHdrLen >= nZeroLen)
      inlength = 0;
    else
      inlength = (nZeroLen - nHdrLen) / 2;
  }

  nRiceErrors = rice_decompress (inbuffer, inlength, outbuffer, &outlength,
                                 bits_per_sample);
  if (nRiceErrors) {                    /* check to see if error occured on last block */
    if (nWCout == outlength)
      nRiceErrors = 0;                  /* error occured on last block */
  }


  switch (pMp[0] & 0xF0) {
   case 0x80:                          /* wfr packet */
     pMp[6] &= 0xCF;                    /* zero out dcc compression bits */
     if (CasWfr_bMsf (pMp) == true)
       nHdrLen = 10;
     else
       nHdrLen = 8;
     for (l = nHdrLen, m = 0; m < outlength; l++, m++) {
       pMp[l++] = outbuffer[m] & 0x00FF;        /* Low Byte */
       pMp[l] = (outbuffer[m] >> 8) & 0x00FF;   /* High Byte */
     }
     break;
   case 0xE0:                          /* wbr packet */
     pMp[6] &= 0xCF;                    /* zero out dcc compression bits */
     if (CasWbr_bMsf (pMp) == true)
       nHdrLen = 10;
     else
       nHdrLen = 8;
     for (l = nHdrLen, m = 0; m < outlength; l++, m++) {
       pMp[l] = outbuffer[m] & 0x00FF;  /* 8 bit sample */
     }
     break;
   default:
     return 0;
  }
  Stats.outbytes = l;
  Stats.outbits = outlength * Stats.bits_per_sample;


  pRec->forward_length = 12 + 256 + nHdrLen + outlength;
  /*
   * 16 byte align 
   */
  pRec->forward_length += (0x0C - (pRec->forward_length & 0x0F)) & 0x0F;

  /*
   * real mini-packet length minus 3 
   */
  pRec->status.packet_length = (nHdrLen + outlength) - 3;
  pRec->data[0] &= 0xF0;
  pRec->data[1] = 0x00;
  if (pRec->status.packet_length < 0x0FFF) {
    pRec->data[0] |= (pRec->status.packet_length >> 8) & 0x0F;
    pRec->data[1] |= pRec->status.packet_length & 0x0FF;
  }


  /*
   * insert usdc compression status here 
   */
  nStatus = CasRice_Processed | CasRice_Decompressed;

  for (l = 10; l < 15; l++) {
    if (arHist_Psi[l] > 0)
      nStatus |= CasRice_InvalidOperator;
  }
  if (arHist_Psi[15] > 0)
    nStatus |= CasRice_BackupOperator;

  if (bits_per_sample == 8) {
    if (arHist_Psi[6] > 0)
      nStatus |= CasRice_CodeId_6;
    if (arHist_Psi[7] > 0)
      nStatus |= CasRice_CodeId_7;
    if ((arHist_Psi[8] > 0) || (arHist_Psi[9] > 0))
      nStatus |= CasRice_InvalidOperator;
  }

  /*
   * errors durning decompression or zeros in packet before  
   */

  if (nRiceErrors != 0)
    nStatus |= CasRice_Errors;
  if (nZeroLen > 0)
    nStatus |= CasRice_FoundZeros;
  if ((nRiceErrors != 0) || (nZeroLen > 0)) {
    pRec->status.cmprs_status = 0x06;
    pRec->status.cmprs_status <<= 24;
    pRec->status.cmprs_status |= nHdrLen + nWCout;      /* last valid byte in packet */
  }
  pRec->status.gnd_status &= ~CasRice_Mask;
  pRec->status.gnd_status |= nStatus;


  return 1;
}



Ulong CasWfr_Unleave (CasRecord * pIn, CasRecord ** pOut)
{
  Uchar *pMp = pIn->data;
  int index, inpktlen, outpktlen, nChIdx, nChannels;

/*
long index,rtn_sts;
long pktidx,inpktlen,mplen[5];
UNSEGMENTED_MINI_PACKET *mpin=&in->packet.usmp;
static TELEMETRY data[5],*pkt[6];
*/

/*
static CasRecord WfrCh1,WfrCh2,WfrCh3,WfrCh4,WfrCh5;
*/

  if ((pMp[0] & 0xF0) != 0x80)          /* not a wfr packet */
    return 0;

  if (CasWfr_Channel (pMp) != 0x07)     /* not interleaved channels */
    return 0;

  /* Was CasWfr_Mode --cwp 2012-10-09 */
  switch (CasWfr_CaptureMode (pMp)) {
   case 0x05:
     nChannels = 2;
     break;                             /* channels 0 and 1 */
   case 0x06:
     nChannels = 3;
     break;                             /* channels 0,1,2 or 2,3,4 */
   case 0x07:
     nChannels = 5;
     break;                             /* channels 0,1,2,3,4 */
   default:
     return 0;
     break;
  }

  /*
   * copy flen,ancillary status,mp header 
   */
  for (nChIdx = 0; nChIdx < nChannels; nChIdx++)
    memcpy (pOut[nChIdx], pIn, 12 + 256 + 16);

  if (CasWfr_bMsf (pMp) == true)
    index = 10;
  else
    index = 8;
  inpktlen = pIn->status.packet_length + 3;
  outpktlen = index;
  while (index < inpktlen) {
    for (nChIdx = 0; nChIdx < nChannels; nChIdx++) {
      pOut[nChIdx]->data[outpktlen] = pMp[index++];
      pOut[nChIdx]->data[outpktlen + 1] = pMp[index++];
    }
    outpktlen += 2;
  }

/*
  if(index != inpktlen){
    fprintf(stderr,"dcc_unleave buffer over run ()\n");
    fprintf(stderr,"  index=%d inpktlen=%d\n",index,inpktlen);
  }
*/

  /*
   * stamp channel numbers for seperated data 
   */
  /* Was CasWfr_Mode -cwp 2012-10-09 */
  switch (CasWfr_CaptureMode (pMp)) {
   case 0x05:
     pOut[0]->data[7] &= 0xC7;
     pOut[0]->data[7] |= 0x00;          /* channel 0 */
     pOut[1]->data[7] &= 0xC7;
     pOut[1]->data[7] |= 0x08;          /* channel 1 */
     break;
   case 0x06:
     if (CasWfr_bLangmuirProbeMode (pMp) == true) {
       pOut[0]->data[7] &= 0xC7;
       pOut[0]->data[7] |= 0x00;        /* channel 0 */
       pOut[1]->data[7] &= 0xC7;
       pOut[1]->data[7] |= 0x08;        /* channel 1 */
       pOut[2]->data[7] &= 0xC7;
       pOut[2]->data[7] |= 0x10;        /* channel 2 */
     } else {
       pOut[0]->data[7] &= 0xC7;
       pOut[0]->data[7] |= 0x10;        /* channel 2 */
       pOut[1]->data[7] &= 0xC7;
       pOut[1]->data[7] |= 0x18;        /* channel 3 */
       pOut[2]->data[7] &= 0xC7;
       pOut[2]->data[7] |= 0x20;        /* channel 4 */
     }
     break;
   case 0x07:
     pOut[0]->data[7] &= 0xC7;
     pOut[0]->data[7] |= 0x00;          /* channel 0 */
     pOut[1]->data[7] &= 0xC7;
     pOut[1]->data[7] |= 0x08;          /* channel 1 */
     pOut[2]->data[7] &= 0xC7;
     pOut[2]->data[7] |= 0x10;          /* channel 2 */
     pOut[3]->data[7] &= 0xC7;
     pOut[3]->data[7] |= 0x18;          /* channel 3 */
     pOut[4]->data[7] &= 0xC7;
     pOut[4]->data[7] |= 0x20;          /* channel 4 */
     break;
   default:
     break;
  }

  /*
   * stamp new packet length 
   */
  outpktlen -= 3;
  for (nChIdx = 0; nChIdx < nChannels; nChIdx++) {
    pOut[nChIdx]->data[0] &= 0xF0;
    pOut[nChIdx]->data[1] = 0x00;
    pOut[nChIdx]->status.packet_length = outpktlen;
    if (outpktlen < 0x0FFF) {
      pOut[nChIdx]->data[0] |= (outpktlen >> 8) & 0x0F;
      pOut[nChIdx]->data[1] = (outpktlen & 0xFF);
    }
    pOut[nChIdx]->status.packet_start_length = (inpktlen / nChannels) - 3;
  }



  return nChannels;
}



/* assume these are dcc compressed packets */
Ulong CasUsdc_ZeroCheck (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  int i, nZeroThold, nZeroCnt;
  Ulong nHdrLen, nPktLen, nValidLen, nStatus;

  nPktLen = pRec->status.packet_length + 3;
  nStatus = pRec->status.gnd_status;
  switch (pMp[0] & 0xF0) {
   case 0x80:                          /* wfr packet */
     if (CasWfr_bMsf (pMp) == true)
       nHdrLen = 10;
     else
       nHdrLen = 8;
     break;
   case 0xE0:                          /* wbr packet */
     if (CasWbr_bMsf (pMp) == true)
       nHdrLen = 10;
     else
       nHdrLen = 8;
     if ((CasWbr_bHighBand (pMp) == true) &&    /* In 80Khz, the flight software */
         (CasWbr_bDccJunkSample (pMp) == true)) /* may/may not add a junk sample */
       nHdrLen += 2;                    /* to the beginning of the data. */
     break;
   default:
     return 0;                          /* not wbr wfr packet */
  }

  nZeroThold = 0;
  if (nStatus & CasMpus_ZeroFilled)
    nZeroThold = 2;                     /* fill */
  else if (nStatus & CasMpii_MpiiFill)
    nZeroThold = 2;                     /* fill, at EOP */
  else if (nStatus & CasMpii_CdsFill)
    nZeroThold = 2;                     /* fill, at EOP */
  else if (nStatus & CasMpii_CdsFillMaybe)
    nZeroThold = 7;                     /* possible fill */
  else
    nZeroThold = 7;                     /* possible fill */

  nZeroCnt = 0;
  for (i = nHdrLen; i < nPktLen; i++) {
    if (pMp[i] == 0x00)
      ++nZeroCnt;
    else
      nZeroCnt = 0;
    if (nZeroCnt > nZeroThold)
      break;
  }
  if (i == nPktLen)                     /* no zeros found */
    nValidLen = 0;
  else
    nValidLen = i - nZeroCnt + 1;


  return nValidLen;
}



char *CasUsdc_DecodeGndStatus (Ulong nStatus)
{
  char *pMsg;
  static char sMsg[128];

  sMsg[0] = '\0';
  pMsg = sMsg;

  if (nStatus & CasRice_Errors)
    pMsg += sprintf (pMsg, "errors ");
  if (nStatus & CasRice_FoundZeros)
    pMsg += sprintf (pMsg, "zeros ");

  if (nStatus & CasRice_InvalidOperator)
    pMsg += sprintf (pMsg, "Invalid Psi ");
  if (nStatus & CasRice_CodeId_6)
    pMsg += sprintf (pMsg, "Psi1,6 ");
  if (nStatus & CasRice_CodeId_7)
    pMsg += sprintf (pMsg, "Psi1,7 ");
  if (nStatus & CasRice_BackupOperator)
    pMsg += sprintf (pMsg, "Psi3,1 ");

  return sMsg;
}
