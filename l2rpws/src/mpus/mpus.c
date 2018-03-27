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
    Version 8.9 :: March 28, 2004 :: raj
	 Somewhere along the way, some of the assumptions have changed...
         Unsegmented packets are ejected as soon as they come in.
         Only segmented packets can force an ejection of current packets
         in memory.  A call with an null in packet pointer will cause
         all memory buffers to flush.  

         Well, some of the assumptions have changed again,...only single 
         threaded mini-packet streams can forces an ejection of packets 
         held in memory, reguardless of segmentation.  Null pointer rule 
         still applies.
    Version 9.0 :: June 17, 2004 :: raj 
         Propagate the new CDS lost record status.
    Version 9.1 :: Feburary 8, 2005 :: raj
         Change max mini-packet length to 24K from CAS_RECORD_MAX_SIZE 
	 Version 9.2 :: October 9, 2012 :: cwp
	      Switched to C99 and stdbool.h
             

  ---------------------------------------------------------------------------
  ---------------------------------------------------------------------------
  ---------------------------------------------------------------------------


  Inputs: 
     CasRecord* pInPkt  - Cassini record structure containing segmented 
                          mini-packets.  If pInPkt==NULL, flush all memory
                          buffers for the remaining packets.
  Outputs:
     CasRecord* pOutPkt - Cassini record structure containing unsegmented
                         mini-packets.
  Returns:
     true  - pOutPkt contains valid data.
     false - pOutPkt contains invalid data.


  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*/
  

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include <Cext.h>
#include <rpwstlm/RecordFile.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasType.h>

#include "mpus.h"

#define mp_type(x)      ( x[0]&0xF0 )
#define mp_len(x)     ( ((x[0]<<8)|(x[1]))&0x0FFF )
#define mp_rti(x)      ( (x[3]<<8)|(x[2]) )
#define hfr_segment(x)  ( x[4]&0x7F )
#define hfr_eof(x)      ( x[4]&0x80 )
#define hfr_len(x)      ((x[5]<<8)|(x[6]) )
#define lp_segment(x)   ((x[4]>>4)&0x03 )
#define lp_size(x)      ((x[4]>>6)&0x03 )
#define wfr_segment(x)  ( x[4]&0x0F )
#define wfr_size(x)     ((x[4]>>4)&0x0F )
#define wfr_msf(x)      ( x[6]&0x08 )
#define wbr_segment(x)  ( x[4]&0x0F )
#define wbr_size(x)     ((x[4]>>4)&0x0F )
#define wbr_msf(x)      ( x[6]&0x08 )

#define MPUS_RECORD_MAX_SIZE (24*1024)
             
const char *sMpusVersion="rwps_mpus() ver 9.2";

int nMpusDebug=0;
void CopyPacket(CasRecord *pSrc,CasRecord *pDst);
void DumpPacketHeader(CasRecord *pInPkt);

bool CasMpus_Copy(CasRecord *pSrc,CasRecord *pDst);
bool CasMpus_Append(CasRecord *pSrc,CasRecord *pDst);
bool CasMpus_ZeroPad(CasRecord *pIn,CasRecord *pMem,int nPadLen);
bool CasMpus_ZeroFill(CasRecord *pIn,int nPadLen);


bool CasMpus_UnsegmentMiniPacket(CasRecord *pInPkt,CasRecord *pOutPkt)
{
bool bSameSegment;
int nHdrOfs,nLoop;
int nType,nLen,nRti,nSize,nSeg;
int nRtiOld,nSizeOld,nSegOld,*pSegLen;
CasRecord *pMemPkt;
static int nHfrSegLen,nLpSegLen,nWfrSegLen,nWbrSegLen;
static CasRecord HFR,LP,WFR,WBR;



  if(pInPkt==NULL){  /* flush all memory buffers */
    if(HFR.status.packet_length>0){      pMemPkt=&HFR; pSegLen=&nHfrSegLen;}
    else if(LP.status.packet_length>0){  pMemPkt=&LP;  pSegLen=&nLpSegLen; }
    else if(WFR.status.packet_length>0){ pMemPkt=&WFR; pSegLen=&nWfrSegLen;}
    else if(WBR.status.packet_length>0){ pMemPkt=&WBR; pSegLen=&nWbrSegLen;}
    else                                 return false; 

    CasMpus_ZeroFill(pMemPkt,*pSegLen);  /* seg bits are zeroed, start len-3 */
    CasMpus_Copy(pMemPkt,pOutPkt);
    pMemPkt->status.packet_length=0;     /* tag memory as empty */
    return true;
  }

  /* Element Common To All Mini Packets */
  nType=mp_type(pInPkt->data);
  nLen=mp_len(pInPkt->data);
  nRti=mp_rti(pInPkt->data);
  pInPkt->status.packet_length=nLen;            /* New data, stamp length */
  pInPkt->status.gnd_status=CasMpus_Processed;  /* zero-out status */
  if(pInPkt->status.cds_fill){  /* attempt to decode the "cds_fill" byte */
    if(pInPkt->status.cds_fill&0x04)              
      pInPkt->status.gnd_status|=CasMpii_MpiiFill;
    else 
      pInPkt->status.gnd_status|=CasMpii_CdsFillMaybe;
  }

  switch(nType){
    case 0x20 : /* High Frequency Receiver Packets */
      nSeg=   hfr_segment(pInPkt->data);  
      nSegOld=hfr_segment(HFR.data);
      if(hfr_eof(pInPkt->data)){  /* EOF bit is set on last segment*/
        nSize=nSeg;               /* hammer hfr into general */
        nSizeOld=nSegOld+1;
      }
      else{
        nSize=0x7F;                /* don't know stream length */
        nSizeOld=0x7F;
      }
      nHdrOfs=5;
      pMemPkt=&HFR;
      pSegLen=&nHfrSegLen;
      break;
    case 0x40 : /* Langmuir Probe Packets */
      nSeg=   lp_segment(pInPkt->data);  /* 0 ... 3 */
      nSegOld=lp_segment(LP.data);       /* 0 ... 3 */
      nSize=   lp_size(pInPkt->data);  /* 0 ... 3 */
      nSizeOld=lp_size(LP.data);       /* 0 ... 3 */
      nHdrOfs= 10;
      pMemPkt=&LP;
      pSegLen=&nLpSegLen;
      break;
    case 0x80 : /* Waveform Receiver Packets */
      nSeg=   wfr_segment(pInPkt->data);  /* 0 ... 15 */
      nSegOld=wfr_segment(WFR.data);      /* 0 ... 15 */
      nSize=   wfr_size(pInPkt->data);  /* 0 ... 15 */
      nSizeOld=wfr_size(WFR.data);      /* 0 ... 15 */
      if(wfr_msf(pInPkt->data))  nHdrOfs=10;
      else                       nHdrOfs=8;
      pMemPkt=&WFR;
      pSegLen=&nWfrSegLen;
      break;
    case 0xE0 : /* Wideband Receiver Packets */
      nSeg=   wbr_segment(pInPkt->data); /* 0 ... 15 */
      nSegOld=wbr_segment(WBR.data);     /* 0 ... 15 */
      nSize=   wbr_size(pInPkt->data);  /* 0 ... 15 */
      nSizeOld=wbr_size(WBR.data);      /* 0 ... 15 */
      if(wbr_msf(pInPkt->data))  nHdrOfs=10;
      else                       nHdrOfs=8;
      pMemPkt=&WBR;
      pSegLen=&nWbrSegLen;
      break;
    default :   /* Unsegmented Variety: stim,mfr,lfdr,dust,mro,fill */
      nSeg=nHdrOfs=0;  /* supress invalid compiler warnings */
      pMemPkt=NULL;    
      nSize=0;
      nType=0x80;      /* allow for wfr dual routing... */
      break;            
  }
/*
  Single threaded streams run through mpus with the exception of packets which
have no chance of segmentation and the wfr because of dual routing.  Since 
the wfr may be multi-threaded, unsegmented wfr packets don't run through mpus,
while segmented packets do.  This causes a jumbling of the time order within
the wfr stream, but it was routed to the ground in that order.  
*/

  if((nSize==0) && (nType==0x80)){ /* emit the unsegmented variety as well */
    CasMpus_Copy(pInPkt,pOutPkt);  /* as multi-threaded segmented packets  */
    return true;
  }
  if(nLen==0){  /* pass zero length packets */ 
    pInPkt->status.gnd_status|=CasMpus_Error;
    CasMpus_Copy(pInPkt,pOutPkt);
    return true;
  }
  if(nSize!=0)
    pInPkt->status.gnd_status|=CasMpus_Segmented;
 
  if(nSeg<nSize)  /* try to guess at the segment size */
    *pSegLen=nLen;


 
  if(pMemPkt->status.packet_length == 0){  /* Memory Buffer is empty */

    if(nSeg!=0){
      CasMpus_ZeroPad(pInPkt,pMemPkt,*pSegLen); /* zero pad */
      if(nMpusDebug)  
        fprintf(stderr,"error - missing first segment (A) => zero pad\n");
    }

    if(CasMpus_Append(pInPkt,pMemPkt)==false){
      CasMpus_Copy(pMemPkt,pOutPkt);
      pOutPkt->status.gnd_status|=CasMpus_Error;
      pMemPkt->status.packet_length=0;          /* tag memory as empty */
      return true;
    }

    if(nSize==nSeg){                           /* check for size/segment */ 
      CasMpus_Copy(pMemPkt,pOutPkt);
      if(mp_type(pOutPkt->data) == 0x20)       /* HFR Packet */
        pOutPkt->data[4]=0x80;                 /* Zero out size/seg field */
      else if(mp_type(pOutPkt->data) == 0x40)  /* LP Packet */
        pOutPkt->data[4]&=0x0F;                 /* Zero out size/seg field */
      else                                      /* WFR and WBR */
        pOutPkt->data[4]=0x00;                  /* Zero out size/seg field */
      pMemPkt->status.packet_length=0;          /* free up memory */
      return true;
    }

    return false; /* no data to output */
  }/* first packet */



  /* if memory is not empty, decide if it's part of the same segment */
  bSameSegment=true;
  nRtiOld=mp_rti(pMemPkt->data);
  if(nRtiOld!=nRti){
    if(nMpusDebug)  fprintf(stderr,"error - rti changed\n");
    bSameSegment=false;
  }
  else{                                  /* hfr outputs header only once */
    for(nLoop=5;nLoop<nHdrOfs;nLoop++){  /* hfr offset = 5 */
      if(pInPkt->data[nLoop]!=pMemPkt->data[nLoop])
        bSameSegment=false;
    }
    if((nMpusDebug)&&(bSameSegment==false))
      fprintf(stderr,"error - headers are different\n");
  }

  
  if(bSameSegment==false){  /* Data is not part of the segment */
    if(nMpusDebug)
      fprintf(stderr,"error - missing segments => zero fill (B)\n");

    CasMpus_ZeroFill(pMemPkt,*pSegLen);  /* seg bits are zeroed, start len-3 */
    CasMpus_Copy(pMemPkt,pOutPkt);
    pMemPkt->status.packet_length=0;          /* free up memory */

    if(nSeg!=0){
      CasMpus_ZeroPad(pInPkt,pMemPkt,*pSegLen);
      if(nMpusDebug)
        fprintf(stderr,"error - missing first segment (C) => zero pad\n");
    }
    CasMpus_Append(pInPkt,pMemPkt);

    return true;
  }





  /* Data is part of the segment */
  if(nSegOld+1!=nSeg){  /* missing a segment, zero pad */ 
    CasMpus_ZeroPad(pInPkt,pMemPkt,*pSegLen);
    if(nMpusDebug)
      fprintf(stderr,"error - missing middle/last segment(D) => zero fill\n");
  }
  if(CasMpus_Append(pInPkt,pMemPkt)==false){
    CasMpus_Copy(pMemPkt,pOutPkt);
    pMemPkt->status.packet_length=0;            /* tag memory as empty */
    return true;
  }
  


  if(nSeg>=nSize){ 

    if(nSeg>nSize){ 
      if(nMpusDebug)
        fprintf(stderr,"error - segment > size\n");
      pMemPkt->status.gnd_status|=CasMpus_Error; /* set mpus */
    }

    CasMpus_Copy(pMemPkt,pOutPkt);
    if(mp_type(pOutPkt->data)==0x20)          /* HFR Packet */
      pOutPkt->data[4]=0x80;                  /* Zero out size/seg field */
    else if(mp_type(pOutPkt->data)==0x40)     /* LP Packet */
      pOutPkt->data[4]&=0x0F;                 /* Zero out size/seg field */
    else                                      /* WFR and WBR */
      pOutPkt->data[4]=0x00;                  /* Zero out size/seg field */
    pMemPkt->status.packet_length=0;          /* tag memory as empty */

    return true;
  }

return false;
}



void CopyPacket(CasRecord *pSrc,CasRecord *pDst)
{
ULONG nLoop,*pInBuff,*pOutBuff;

  pDst->forward_length = 12 + 256 + (pSrc->status.packet_length+3);
  pDst->fill0 = 0x0400 | ((pSrc->data[0]>>4)&0x0F);
  pDst->fill1 = pSrc->fill1;

  /* change to byte pointer */
  pInBuff =(ULONG*)(pSrc);  pInBuff+=3;
  pOutBuff=(ULONG*)(pDst);  pOutBuff+=3;
  for(nLoop=0;nLoop<pDst->forward_length;nLoop+=4) 
    *pOutBuff++=*pInBuff++;  /* copy native alignments */ 

  pDst->data[0]&=0xF0;   
  pDst->data[1]&=0x00;

  /* Zero out mini packet length if > 0x0FFF */
  if(pDst->status.packet_length < 0x0FFF){
    pDst->data[0]|= (pDst->status.packet_length>>8)&0x0F;
    pDst->data[1] =  pDst->status.packet_length&0x0FF;
  }

  /* if forward_length is not on a 16 byte boundary, align it.
     reverse_length is written by the write function() 
  */
  pDst->forward_length+=(0x0C-(pDst->forward_length&0x0F))&0x0F;

return;
}



void DumpPacketHeader(CasRecord *pInPkt)
{
ULONG nTmp;

  fprintf(stderr,"Type=%X ",(pInPkt->data[0]>>4)&0x0F); 
  nTmp =pInPkt->data[3];  nTmp<<=8;
  nTmp|=pInPkt->data[2];  nTmp&=0x0FFFF; 
  fprintf(stderr,"Rti=%04X :: ",nTmp);
  fprintf(stderr,"Flen=%X, ",pInPkt->forward_length);
  fprintf(stderr,"Alen=%X",pInPkt->status.packet_length);
  fprintf(stderr,"(%X), ",pInPkt->status.packet_start_length);
    nTmp =pInPkt->data[0];  nTmp<<=8; 
    nTmp|=pInPkt->data[1];  nTmp&=0x0FFF;
  fprintf(stderr,"Plen=%03X ",nTmp);
  fprintf(stderr,"\n\t");
  for(nTmp=0;nTmp<16;nTmp++)
    fprintf(stderr,"%02X ",pInPkt->data[nTmp]);
  fprintf(stderr,"\n");
return;
}



char *CasMpus_DecodeGndStatus(Ulong nStatus)
{
char *pMsg;
static char sMsg[128];

  sMsg[0]='\0';  /* initial condition */
  pMsg=sMsg;

  if(nStatus&CasMpus_Error)
    pMsg+=sprintf(pMsg,"error ");                 /*  6 bytes */
  if(nStatus&CasMpus_ZeroFilled)
    pMsg+=sprintf(pMsg,"zero filled, missing ");  /* 21 bytes */
  if(nStatus&CasMpus_FirstSegment)
    pMsg+=sprintf(pMsg,"first..");                /*  7 bytes */
  if(nStatus&CasMpus_MiddleSegment)
    pMsg+=sprintf(pMsg,"middle..");               /*  8 bytes */
  if(nStatus&CasMpus_LastSegment)
    pMsg+=sprintf(pMsg,"last..");                 /*  6 bytes */
  if(nStatus&CasMpus_ZeroFilled)
    pMsg+=sprintf(pMsg,"segments");               /*  8 bytes */
  if(nStatus&CasMpus_LengthGuess)
    pMsg+=sprintf(pMsg,"guessed at length");      /* 18 bytes */

return sMsg;
}



bool CasMpus_Copy(CasRecord *pSrc,CasRecord *pDst)
{
unsigned char *pIn,*pOut;
size_t nLength;


  pIn= (unsigned char*)pSrc;
  pOut=(unsigned char*)pDst;
  nLength=12+256+pSrc->status.packet_length+3;
  memcpy(pOut,pIn,nLength);

  pDst->forward_length = 12 + 256 + (pSrc->status.packet_length+3);
  pDst->fill0 = 0x0400 | ((pSrc->data[0]>>4)&0x0F);
  pDst->fill1 = pSrc->fill1;

  pDst->data[0]&=0xF0;  pDst->data[1]&=0x00;
  if(pDst->status.packet_length < 0x0FFF){
    pDst->data[0]|= (pDst->status.packet_length>>8)&0x0F;
    pDst->data[1] =  pDst->status.packet_length&0x0FF;
  }

  /* if forward_length is not on a 16 byte boundary, align it.
     reverse_length is written by the write function() 
  */
  pDst->forward_length+=(0x0C-(pDst->forward_length&0x0F))&0x0F;


return true;
}



bool CasMpus_Append(CasRecord *pSrc,CasRecord *pDst)
{
int nIdx,nHdrOfs,nMax;
unsigned long nType;

  if(pDst->status.packet_length==0){
    CasMpus_Copy(pSrc,pDst);
    return true;
  }

  nType=mp_type(pSrc->data);
  switch(nType){
    case 0x20 :  nHdrOfs=5;                         break;  /* hfr */
    case 0x40 :  nHdrOfs=10;                        break;  /* Lp */
    case 0x80 :  nHdrOfs=wfr_msf(pSrc->data)?10:8;  break;  /* wfr */
    case 0xE0 :  nHdrOfs=wbr_msf(pSrc->data)?10:8;  break;  /* wbr */
    default :    return false;                      break;
  }

  nMax=pSrc->status.packet_length+3;
  if((nMax+pDst->status.packet_length+3)>MPUS_RECORD_MAX_SIZE)
    return false;

  pDst->status.packet_length+=3;
  for(nIdx=nHdrOfs;nIdx<nMax;nIdx++)
    pDst->data[pDst->status.packet_length++]=pSrc->data[nIdx];  
  pDst->status.packet_length-=3;
  pDst->status.packet_start_length+=(pSrc->status.packet_start_length+3);

  /* Put the current size and seg in the header area */
  if(nType==0x20)       /* HFR */
    pDst->data[4]=pSrc->data[4];             
  else if(nType==0x40)  /* Langmuir Probe */
    pDst->data[4]=(pDst->data[4]&0x0F)|(pSrc->data[4]&0xF0);
  else                                   /* WFR and WBR */
    pDst->data[4]=pSrc->data[4];  

  /* decode and Propogate cds fill status */
  pDst->status.cds_fill|=pSrc->status.cds_fill;
  pDst->status.gnd_status|=pSrc->status.gnd_status;  /* pSrc set at begining */

return true;
}


/* zero pad to the next segment */
bool CasMpus_ZeroPad(CasRecord *pIn,CasRecord *pMem,int nPadLen)
{
bool bStatus=true;
int nIdx,nHdrOfs;
unsigned long nType,nMemSeg,nInSeg,nInSize,nMax;

  nType=mp_type(pIn->data);
  switch(nType){
    case 0x20 :  /* hfr */
      nInSeg= hfr_segment(pIn->data);
      nMemSeg=hfr_segment(pMem->data);
      if(hfr_eof(pIn->data))  nInSize=nInSeg;
      else                    nInSize=0x7F;
      nHdrOfs=5;   
      break;
    case 0x40 :  /* Lp */
      nInSeg= lp_segment(pIn->data);
      nInSize=lp_size(pIn->data);
      nMemSeg=lp_segment(pMem->data);
      nHdrOfs=10;
      break;
    case 0x80 :  /* wfr */
      nInSeg= wfr_segment(pIn->data);
      nInSize=wfr_size(pIn->data);
      nMemSeg=wfr_segment(pMem->data);
      nHdrOfs=wfr_msf(pIn->data)?10:8;
      break;
    case 0xE0 :  /* wbr */
      nInSeg= wbr_segment(pIn->data);
      nInSize=wbr_size(pIn->data);
      nMemSeg=wbr_segment(pMem->data);
      nHdrOfs=wbr_msf(pIn->data)?10:8;
      break;
    default :
      return false;
      break;
  }
  if(nInSeg==0)  return false;

  if(nInSeg<nInSize)
    nPadLen=pIn->status.packet_length;
  else if(nPadLen==0){
    if(nType==0x20)  nPadLen=0x3EA;
    else             nPadLen=1024+nHdrOfs-3;
    pMem->status.gnd_status|=CasMpus_LengthGuess;
    if(nMpusDebug)
      fprintf(stderr,"guessing the pad length, %d(0x%X)\n",nPadLen,nPadLen);
  }
  else
    nPadLen=nPadLen;

  nPadLen+=3;


  
  if(pMem->status.packet_length==0){  /* missed first packet, copy header */
    CasMpus_Copy(pIn,pMem);           /* copy ancillary data */

    for(nIdx=nHdrOfs;nIdx<nPadLen;nIdx++)
      pMem->data[nIdx]=0x00;
    pMem->status.packet_length=(nPadLen-3);
    pMem->status.packet_start_length=(nPadLen-3);
    pMem->status.gnd_status|=(CasMpus_ZeroFilled|CasMpus_FirstSegment);
    nMemSeg=0;
    /* fixup segment bits at the end, size done via copy */
  }

  nMax=pIn->status.packet_length+3;
  --nInSeg;               /* decrement for compare */
  while(nMemSeg<nInSeg){  /* more zero padding */
    if((pMem->status.packet_length+nPadLen+3+nMax)>MPUS_RECORD_MAX_SIZE){
      if(nMpusDebug)
        fprintf(stderr,"memory overun error - ZeroPad()\n");
      pMem->status.gnd_status|=CasMpus_Error;  /* fail */
      bStatus=false;
      break;
    }
    pMem->status.packet_length+=3;
    for(nIdx=nHdrOfs;nIdx<nPadLen;nIdx++)
      pMem->data[pMem->status.packet_length++]=0x00;
    pMem->status.packet_length-=3;
    pMem->status.packet_start_length+=nPadLen;
    pMem->status.gnd_status|=(CasMpus_ZeroFilled|CasMpus_MiddleSegment);
    ++nMemSeg;
  }

  /* fixup segmentation bits, size should already be there */
  if(nType==0x20){        /* hfr */
    pMem->data[4]&=0x80;  /* may/may not be the last segment */
    pMem->data[4]|=(nMemSeg&0x7F);
  }
  else if(nType==0x40){  /* Lp */
    pMem->data[4]&=0xCF;  
    pMem->data[4]|=((nMemSeg<<4)&0x30);
  }
  else{                 /* wfr 0x80, wbr 0xE0 */
    pMem->data[4]&=0xF0;  
    pMem->data[4]|=(nMemSeg&0x0F);
  }

return bStatus;
}



/* fill to end of packet with zeros, fixup segmentation bits */
bool CasMpus_ZeroFill(CasRecord *pIn,int nPadLen)
{
int nIdx,nHdrOfs;
unsigned long nType,nInSeg,nInSize,nHfrLen;


  if(pIn->status.packet_length==0)  return false;

  nType=mp_type(pIn->data);
  switch(nType){
    case 0x20 :/* hfr's expected length, guess segments by last hfr length */
      nInSeg= hfr_segment(pIn->data);
      if(hfr_eof(pIn->data))  nInSize=nInSeg;
      else                    nInSize=0x7F;
      nHdrOfs=5;   
      break;
    case 0x40 :  /* Lp */
      nInSeg= lp_segment(pIn->data);
      nInSize=lp_size(pIn->data);
      nHdrOfs=10;
      break;
    case 0x80 :  /* wfr */
      nInSeg= wfr_segment(pIn->data);
      nInSize=wfr_size(pIn->data);
      nHdrOfs=wfr_msf(pIn->data)?10:8;
      break;
    case 0xE0 :  /* wbr */
      nInSeg= wbr_segment(pIn->data);
      nInSize=wbr_size(pIn->data);
      nHdrOfs=wbr_msf(pIn->data)?10:8;
      break;
    default :
      return false;
      break;
  }
  if(nInSeg==nInSize)  return false;

  if(nPadLen==0){
    if(nType==0x20)  nPadLen=0x3EA;
    else             nPadLen=1024+nHdrOfs-3;
    pIn->status.gnd_status|=CasMpus_LengthGuess;
  }
  nPadLen+=3;


  if(nType==0x20){  /* hfr special case */
    nHfrLen=hfr_len(pIn->data)+7;
    if(nHfrLen>(24*1024))
      nHfrLen=24*1024;  /* data memory in hfr instrument */

    pIn->status.packet_length+=3;
    nPadLen=nHfrLen-pIn->status.packet_length+nHdrOfs;  /* assume 1 lost pkt */
    for(nIdx=pIn->status.packet_length;nIdx<nHfrLen;nIdx++)
      pIn->data[pIn->status.packet_length++]=0x00;

    pIn->status.packet_length-=3;
    pIn->status.packet_start_length+=nPadLen;
    pIn->status.gnd_status|=CasMpus_ZeroFilled;
    pIn->status.gnd_status|=(CasMpus_MiddleSegment|CasMpus_LastSegment);

    pIn->data[4]=0x80;  /* zero out segments */
    /* pIn->status.packet_start_length-=3; */

    return true;
  }


  if((nInSeg+1)<nInSize)  
    pIn->status.gnd_status|=CasMpus_MiddleSegment;

  /* all other cases */
  while(nInSeg<nInSize){  /* more zero padding */
    if((pIn->status.packet_length+nPadLen)>MPUS_RECORD_MAX_SIZE){
      if(nMpusDebug)
        fprintf(stderr,"memory overun error - CasMpus_ZeroFill()\n");
      pIn->status.gnd_status|=CasMpus_Error;  /* fail */
      return false;
    }
    pIn->status.packet_length+=3;
    for(nIdx=nHdrOfs;nIdx<nPadLen;nIdx++)
      pIn->data[pIn->status.packet_length++]=0x00;
    pIn->status.packet_length-=3;
    pIn->status.packet_start_length+=nPadLen;
    pIn->status.gnd_status|=(CasMpus_ZeroFilled|CasMpus_LastSegment);
    ++nInSeg;
  }

  /* fixup segmentation bits, size should already be there */
  if(nType==0x40)        /* Lp */
    pIn->data[4]&=0x0F;   
  else                   /* wfr 0x80, wbr 0xE0 */
    pIn->data[4]=0x00;


return true;
}
