
/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


  File Name: mpus.c   
  Program: mpus 
  Author: Robert Johnson
  Creation: May 25, 1995
  Revisions:
    Version 8.00 :: Janurary 20, 1999 :: raj
	 Revamp the whole program to ..... 
    Version 8.10 :: Janurary xx, 1999 :: raj
	 xx
    Version 8.20 :: Janurary xx, 1999 :: raj
	 xx
    Version 8.30 :: Janurary xx, 1999 :: raj
	 xx
    Version 8.40 :: July 31, 2000:: raj
	 xx
    Version 8.5 :: Sep 11, 2000:: wtr
	 Fix WFR tests...  Must check channel bits to allow
	 5 channel when single segment...
    Version 8.6 :: May 24, 2001:: raj 
	 Fix Langmuir Probe Segmentation - Whole byte in header gets clobbered.
    Version 8.7 :: July 10, 2002 :: raj 
	 Fix WFR Compressed Ancillary Byte Count 
             

  ---------------------------------------------------------------------------
  ---------------------------------------------------------------------------
  ---------------------------------------------------------------------------


  Inputs: 
	  CasRecord* pInPkt  - Cassini record structure containing segmented 
					   mini-packets.
  Outputs:
	  CasRecord* pOutPkt - Cassini record structure containing unsegmented
					   mini-packets.
  Returns:
	   True  - pOutPkt contains valid data.
	   False - pOutPkt contains invalid data.


  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/


#include <stdio.h>
#include <stdlib.h>

#include "Cext.h"
#include "RecordFile.h"
#include "CasRecord.h"


const char *sMpusVersion = "mpus(), Version 8.6";

Bool cmdln_ShowHeader = False;

void CopyPacket (CasRecord * pSrc, CasRecord * pDst);
void DumpPacketHeader (CasRecord * pInPkt);

Bool MiniPacketUnsegmentation (CasRecord * pInPkt, CasRecord * pOutPkt)
{
  Bool bInDataNew = False;
  Bool bOutDataValid = False;
  int nHdrOfs, nLoop, nLoopMax;
  int nType, nLen, nRti, nSize, nSeg;
  int nRtiOld, nSizeOld, nSegOld;
  CasRecord *pMemPkt;
  static UCHAR arHeader[8];
  static CasRecord HFR, LP, WFR, WBR;

/*
  if(cmdln_ShowHeader==True)
    DumpPacketHeader(pInPkt);
*/

  /*
   * Element Common To All Mini Packets 
   */
  nType = (pInPkt->data[0] >> 4) & 0x0F;
  nLen = pInPkt->data[0];
  nLen <<= 8;
  nLen |= pInPkt->data[1];
  nLen &= 0x0FFF;
  nRti = pInPkt->data[3];
  nRti <<= 8;
  nRti |= pInPkt->data[2];
  nRti &= 0x0FFFF;

  switch (nType) {
   case 0x02:                          /* High Frequency Receiver Packets */
     nSeg = pInPkt->data[4] & 0x7F;
     if (pInPkt->data[4] & 0x80)
       nSize = nSeg;                    /* EOF bit is set on last segment */
     else
       nSize = 0x7F;                    /* The Maximum, 127 segments */
     nHdrOfs = 5;
     nSegOld = HFR.data[4] & 0x7F;
     if (HFR.data[4] & 0x80)
       nSizeOld = nSegOld;
     else
       nSizeOld = 0x7F;
     pMemPkt = &HFR;
     break;
   case 0x04:                          /* Langmuir Probe Packets */
     nSeg = (pInPkt->data[4] >> 4) & 0x03;      /* 0 ... 3 */
     nSize = (pInPkt->data[4] >> 6) & 0x03;     /* 0 ... 3 */
     nHdrOfs = 10;
     nSegOld = (LP.data[4] >> 4) & 0x03;        /* 0 ... 3 */
     nSizeOld = (LP.data[4] >> 6) & 0x03;       /* 0 ... 3 */
     pMemPkt = &LP;
     break;
   case 0x08:                          /* Waveform Receiver Packets */
     nSeg = (pInPkt->data[4]) & 0x0F;   /* 0 ... 15 */
     nSize = (pInPkt->data[4] >> 4) & 0x0F;     /* 0 ... 15 */
     if (pInPkt->data[6] & 0x08)        /* msf bit, more stuff follows */
       nHdrOfs = 10;
     else
       nHdrOfs = 8;
     nSegOld = (WFR.data[4]) & 0x0F;    /* 0 ... 15 */
     nSizeOld = (WFR.data[4] >> 4) & 0x0F;      /* 0 ... 15 */
     pMemPkt = &WFR;
     break;
   case 0x0E:                          /* Wideband Receiver Packets */
     nSeg = (pInPkt->data[4]) & 0x0F;   /* 0 ... 15 */
     nSize = (pInPkt->data[4] >> 4) & 0x0F;     /* 0 ... 15 */
     if (pInPkt->data[6] & 0x08)        /* msf bit, more stuff follows */
       nHdrOfs = 10;
     else
       nHdrOfs = 8;
     nSegOld = (WBR.data[4]) & 0x0F;    /* 0 ... 15 */
     nSizeOld = (WBR.data[4] >> 4) & 0x0F;      /* 0 ... 15 */
     pMemPkt = &WBR;
     break;
   default:                            /* Unsegmented Variety: stim,mfr,lfdr,dust,mro,fill */
     nSeg = nHdrOfs = 0;                /* supress invalid compiler warnings */
     pMemPkt = NULL;                    /* supress invalid compiler warnings */
     nSize = 0;
     break;
  }

  /*
   * Check to see if the incomming data is new 
   */
  /*
   * 10SEP2000:wtr  Test WFR channel bits 
   */

/*
  if(nType == 0x08)	
      for(nLoop=0; nLoop<8; nLoop++)
        {
          if(arHeader[nLoop] != pInPkt->data[nLoop])
              bInDataNew = True;
          arHeader[nLoop] = pInPkt->data[nLoop];
         }
  else
    for(nLoop=0;nLoop<5;nLoop++){
      if(arHeader[nLoop]!=pInPkt->data[nLoop])
        bInDataNew=True;
      arHeader[nLoop]=pInPkt->data[nLoop];
    }
*/

  for (nLoop = 0; nLoop < 5; nLoop++) {
    if (arHeader[nLoop] != pInPkt->data[nLoop])
      bInDataNew = True;
    arHeader[nLoop] = pInPkt->data[nLoop];
  }


  /*
   * Packet is not segmented and is new data, output it 
   */
  if (nSize == 0 && bInDataNew == True) {
    pInPkt->status.packet_length = nLen;        /* New data, stamp length field */
    CopyPacket (pInPkt, pOutPkt);
    bOutDataValid = True;
    return bOutDataValid == True ? True : False;
  }

  /*
   * If there is new data and it exists in memory, output the memory data 
   */
  if (bInDataNew == True) {
    if (pMemPkt->status.packet_length == 0) {   /* Memory Buffer is empty */
      pInPkt->status.packet_length = nLen;      /* New data, stamp length field */
      CopyPacket (pInPkt, pMemPkt);
    } else {
      nRtiOld = pMemPkt->data[3];
      nRtiOld <<= 8;
      nRtiOld |= pMemPkt->data[2];
      nRtiOld &= 0x0FFFF;
      if (nRti == nRtiOld) {            /* Data is part of a segment */
        nLoopMax = nLen + 3;
        pMemPkt->status.packet_length += 3;
        for (nLoop = nHdrOfs; nLoop < nLoopMax;
             nLoop++, (pMemPkt->status.packet_length)++)
          pMemPkt->data[pMemPkt->status.packet_length] = pInPkt->data[nLoop];
        pMemPkt->status.packet_length -= 3;
        pMemPkt->status.packet_start_length +=
          (pInPkt->status.packet_start_length + 3);
      } else {                          /* Data that is not part of the current segment */
        CopyPacket (pMemPkt, pOutPkt);
        if ((pOutPkt->data[0] & 0xF0) == 0x20)  /* HFR Packet */
          pOutPkt->data[4] = 0x80;      /* Zero out size/seg field */
        else if ((pOutPkt->data[0] & 0xF0) == 0x40)     /* LP Packet */
          pOutPkt->data[4] &= 0x0F;     /* Zero out size/seg field */
        else                            /* WFR and WBR */
          pOutPkt->data[4] = 0x00;      /* Zero out size/seg field */
        pOutPkt->status.packet_start_length -= 3;       /* correct the start length */
        pMemPkt->status.packet_length = 0;
        bOutDataValid = True;
        pInPkt->status.packet_length = nLen;    /* New data, stamp length field */
        CopyPacket (pInPkt, pMemPkt);
      }
    }                                   /* else */
  }

  /*
   * if new mini packet data 
   */
  /*
   * If the output buffer is empty and the packet in memory
   * is 'complete', output the packet 
   */
  if (bOutDataValid == False && nSeg >= nSize && pMemPkt != NULL) {
    if (pMemPkt->status.packet_length != 0) {
      CopyPacket (pMemPkt, pOutPkt);
      if ((pOutPkt->data[0] & 0xF0) == 0x20)    /* HFR Packet */
        pOutPkt->data[4] = 0x80;        /* Zero out size/seg field */
      else if ((pOutPkt->data[0] & 0xF0) == 0x40)       /* LP Packet, raj 01-05-24 change compare from 0x20 to 0x40 */
        pOutPkt->data[4] &= 0x0F;       /* Zero out size/seg field */
      else                              /* WFR and WBR */
        pOutPkt->data[4] = 0x00;        /* Zero out size/seg field */
      pMemPkt->status.packet_length = 0;        /* Memory buffer is empty */
      bOutDataValid = True;
    }
  }

  return bOutDataValid;
}



void CopyPacket (CasRecord * pSrc, CasRecord * pDst)
{
  ULONG nLoop, *pInBuff, *pOutBuff;

  pDst->forward_length = 12 + 256 + (pSrc->status.packet_length + 3);
  pDst->fill0 = 0x0400 | ((pSrc->data[0] >> 4) & 0x0F);
  pDst->fill1 = pSrc->fill1;

  pInBuff = (ULONG *) (pSrc);
  pInBuff += 3;
  pOutBuff = (ULONG *) (pDst);
  pOutBuff += 3;
  for (nLoop = 0; nLoop < pDst->forward_length; nLoop += 4)
    *pOutBuff++ = *pInBuff++;           /* copy native alignments */

  pDst->data[0] &= 0xF0;
  pDst->data[1] &= 0x00;

  /*
   * Zero out mini packet length if > 0x0FFF 
   */
  if (pDst->status.packet_length < 0x0FFF) {
    pDst->data[0] |= (pDst->status.packet_length >> 8) & 0x0F;
    pDst->data[1] = pDst->status.packet_length & 0x0FF;
  }

  /*
   * if forward_length is not on a 16 byte boundary, align it.
   * reverse_length is written by the write function() 
   */
  pDst->forward_length += (0x0C - (pDst->forward_length & 0x0F)) & 0x0F;

  return;
}



void DumpPacketHeader (CasRecord * pInPkt)
{
  ULONG nTmp;

  fprintf (stderr, "Type=%X ", (pInPkt->data[0] >> 4) & 0x0F);
  nTmp = pInPkt->data[3];
  nTmp <<= 8;
  nTmp |= pInPkt->data[2];
  nTmp &= 0x0FFFF;
  fprintf (stderr, "Rti=%04lX :: ", nTmp);
  fprintf (stderr, "Flen=%lX, ", pInPkt->forward_length);
  fprintf (stderr, "Alen=%lX", pInPkt->status.packet_length);
  fprintf (stderr, "(%lX), ", pInPkt->status.packet_start_length);
  nTmp = pInPkt->data[0];
  nTmp <<= 8;
  nTmp |= pInPkt->data[1];
  nTmp &= 0x0FFF;
  fprintf (stderr, "Plen=%03lX ", nTmp);
  fprintf (stderr, "\n\t");
  for (nTmp = 0; nTmp < 16; nTmp++)
    fprintf (stderr, "%02X ", pInPkt->data[nTmp]);
  fprintf (stderr, "\n");
  return;
}
