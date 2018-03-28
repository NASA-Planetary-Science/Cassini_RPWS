#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdbool.h>

#include <Cext.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasHfr.h>



/* Ported from IDL conv_h_k.pro */
Ulong CasHfr_RecordToKronos (CasRecord * pIn, Uchar * pOut, bool bSwap)
{
  Ulong nSclk, nEpoch, nRti, nCds;
  Ulong nDay, nHour, nMinute, nSec, nMillisecond;
  Ulong nDsec, nInLen, nOutCnt;
  struct tm *pTm;

  nOutCnt = 0;
  pOut[nOutCnt++] = 'S';
  pOut[nOutCnt++] = 't';
  pOut[nOutCnt++] = 'P';
  pOut[nOutCnt++] = 'k';
  pOut[nOutCnt++] = '1';

  nCds = pIn->status.cds_time;
  nRti = (pIn->data[3] << 8) + pIn->data[2];
  if (nRti > 0x7FFF)
    nRti = (65536L + nRti) & 0x0FFFF;
  nSclk = (1L << 13) * (nCds / (1L << 13)) + (nRti / 8);

  nEpoch = pIn->status.fill_204[16] << 24;      /* 56 + 12 = 68 bytes into */
  nEpoch += pIn->status.fill_204[17] << 16;     /* the record, want the */
  nEpoch += pIn->status.fill_204[18] << 8;      /* 84th byte */
  nEpoch += pIn->status.fill_204[19];
  nSec = nEpoch + nSclk;

  nDay = nSec / 86400;
  nDsec = nSec % 86400;
  nHour = nDsec / 3600;
  nMinute = (nDsec - (3600 * nHour)) / 60;
  nDsec = nDsec % 60;
  nMillisecond = (nRti % 8) * 12.5;

  pTm = gmtime ((time_t *) (&nSec));

  pOut[nOutCnt++] = (Uchar) nMinute;
  pOut[nOutCnt++] = (Uchar) nHour;
  pOut[nOutCnt++] = (Uchar) nMillisecond;
  pOut[nOutCnt++] = (Uchar) nDsec;

  pOut[nOutCnt++] = (Uchar) (1900 + pTm->tm_year);      /* Years since 1900 */
  pOut[nOutCnt++] = (Uchar) ((1900 + pTm->tm_year) >> 8);
  pOut[nOutCnt++] = (Uchar) (pTm->tm_mday);     /* Day of the Month, 1-31 */
  pOut[nOutCnt++] = (Uchar) (1 + pTm->tm_mon);  /* Months since Jan, 0-11 */

  /*
   * Total length including hfr lenght bytes (2) 
   */
  nInLen = (pIn->data[5] << 8) + (pIn->data[6]);
  nInLen += 2;
  memcpy (&(pOut[nOutCnt]), &(pIn->data[5]), nInLen);

  /*
   * Flip the length byte order for programs which read this quanity 
   */
  /*
   * as 16 bits rather than as 2 bytes 
   */
  /*
   * Swap byte pair in reverse from the MiniPacket 
   */
  if (bSwap == false) {
    pOut[13] ^= pOut[14];
    pOut[14] ^= pOut[13];
    pOut[13] ^= pOut[14];
  }

  /*
   * check our calculation against gmtime(), 13 bytes of Kronos Header 
   */

/*
  if((pTm->tm_hour!=nHour)||(pTm->tm_min!=nMinute)||(pTm->tm_sec!=nDsec)){
  int i;
    fprintf(stderr,"CasRecordToKronos()\n");
    fprintf(stderr,"   gmtime(): %02d:%02d:%02d !=  %02ld %02ld %02ld\n",
            pTm->tm_hour,pTm->tm_min,pTm->tm_sec,nHour,nMinute,nDsec);
    for(i=0;i<32;i++){
      if(!(i%16))
        fprintf(stderr,"   ");
      fprintf(stderr,"%02X ",pOut[i]);
      if(!((i+1)%16))
        fprintf(stderr,"\n");
    }
  } 
*/

  return nInLen + 13;
}
