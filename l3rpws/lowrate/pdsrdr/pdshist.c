/*
                      pdshist.c
                     Robert Johnson
                     April 23, 2002

  pdshist.c is a routine for generating and writing histograms. 
          background subtractions
          background division
          ect



April 23, 2003
  write histogram binning alogthym and implement background division

May 12, 2003
  modify histogram algorythm to accomodate an interval, rather than day
  long.

December 30, 2003
  Use galactic background noise levels for background.
  
2012-10-23 C. Piker
  Convert to C99 and use stdbool
  
*/


#include <stdbool.h>
#include <stdio.h> 
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <string.h>
#include <time.h>
#include <limits.h>
#include <math.h>
#include <assert.h>

#include "pdsrdr.h"
#include "pdshist.h"

#include <rpwstlm/CasType.h>
#include <rpwstlm/CasPds.h>

#include "HfrGalNsJup.h"

extern bool bSilent;
extern Ulong cmdln_nNixBadPkt;

Ulong nBgdScetDaysBeg=0,nBgdScetMsecBeg=0;
Ulong nBgdScetDaysEnd=UINT_MAX,nBgdScetMsecEnd=UINT_MAX;
CasHistogram *pHstEu=NULL,*pHstEv=NULL,*pHstEx=NULL,*pHstEz=NULL;
CasHistogram *pHstBx=NULL,*pHstBy=NULL,*pHstBz=NULL;
CasHistogram *pHstLs=NULL,*pHstHf=NULL;

/*
  Make a histogram with log spaced bins from 1 to 10^-20, currently
2001 elements.  Outliers are stuffed into bins 0 and 2000.

           typedef struct caspds_histogram_tag{
             Ulong nItems,nNumRec,nSensor;
             Ulong (*arHist)[CAS_HIST_MAX_BINS];
             float *fBgnd;
           }CasHistogram;
*/

/*
typedef struct caspdsfilelist_tag{
  char *sList[1024];
  Uchar nMpHdr[1024][32];
  Ulong nType[1024],nRecLen[1024],nId[1024],nMax;
}CasPdsFileList;
*/
CasHistogram* CasPds_LRfull_LogBinHist(CasPdsFileList *pList,Ulong nIdxBeg,int nPercent)
{
char sReport[2048],*pReport;
int i,j,nIdx;
Ulong nId,*pDword,nDisCnt=0,nBgdCnt=0,nTotRec=0;

Uchar *arPds=arPdsFileBuf,*pByte;
Ulong nRecLen,nNumRec,nFreq,nType,nDataQuality,nSensorId,nAnt,nRec,nTmp;
Ulong nSclkSec,nSclkFine,nScetDays,nScetMsec;
float *pTime,*pFreq,*pAmpl;

bool bFirstTime=true;
Ulong (*arHist)[CAS_HIST_MAX_BINS];
float *arBgnd;
CasHistogram *pHgram;

  
  pReport=sReport;  sReport[0]='\0';

  /* find all the antennas in all the files */
  nId=pList->nId[nIdxBeg];  nType=0;
  for(nIdx=nIdxBeg;nIdx<pList->nMax;nIdx++){
    if(nId!=pList->nId[nIdx])  
      break;
    else
      nType|=pList->nType[nIdx];
  } 


nId=pList->nId[nIdxBeg];
for(nIdx=nIdxBeg;nIdx<pList->nMax;nIdx++){
  if(nId!=pList->nId[nIdx])  break;
  
  if(CasPds_LRfull_ReadArchive(pList->sList[nIdx],arPds)==NULL){
    fprintf(stderr,"CasPds_LRfull_ReadArchive(%s) failed.\n",
            pList->sList[nIdx]);
    exit(0); 
  }

  /* assign pointers to stuff */
  pByte=arPds+8;
  nRecLen=*((Ulong*)pByte);  pByte+=4;  
  nNumRec=*((Ulong*)pByte);  pByte+=4;
  nFreq=(nRecLen-16)/4;             /* number of data points in the record */
  pTime=(float*)(pByte+1*nRecLen);  /* start of time skipping sclk/scet */
  pFreq=(float*)(pByte+2*nRecLen);  /* start of freq skipping sclk/scet */


  /* scarf off some memory for the histograms */
  if(bFirstTime==true){
    bFirstTime=false; 

    /* clean-up the mess from before */
    if(pHstEu!=NULL){free(pHstEu->arBgnd);free(pHstEu->arHist);free(pHstEu);} 
    if(pHstEv!=NULL){free(pHstEv->arBgnd);free(pHstEv->arHist);free(pHstEv);}
    if(pHstEx!=NULL){free(pHstEx->arBgnd);free(pHstEx->arHist);free(pHstEx);}
    if(pHstEz!=NULL){free(pHstEz->arBgnd);free(pHstEz->arHist);free(pHstEz);}

    if(pHstBx!=NULL){free(pHstBx->arBgnd);free(pHstBx->arHist);free(pHstBx);}
    if(pHstBy!=NULL){free(pHstBy->arBgnd);free(pHstBy->arHist);free(pHstBy);}
    if(pHstBz!=NULL){free(pHstBz->arBgnd);free(pHstBz->arHist);free(pHstBz);}

    if(pHstLs!=NULL){free(pHstLs->arBgnd);free(pHstLs->arHist);free(pHstLs);}
    if(pHstHf!=NULL){free(pHstHf->arBgnd);free(pHstHf->arHist);free(pHstHf);}

    pHstEu=pHstEv=pHstEx=pHstEz=NULL;
    pHstBx=pHstBy=pHstBz=NULL;
    pHstLs=pHstHf=NULL;

    /* allocate space for the histograms */
    nAnt=nType&CasAntMask;
    while(nAnt){
      assert( (pHgram=calloc(1,sizeof(CasHistogram))) != NULL );
      assert( (arHist=calloc(nFreq*CAS_HIST_MAX_BINS,sizeof(Ulong))) !=NULL ); 
      assert( (arBgnd=calloc(nFreq,sizeof(float))) !=NULL );
      pHgram->arHist=arHist;  /* ulong */
      pHgram->arBgnd=arBgnd;  /* float */
      pHgram->nItems=nFreq;
      pHgram->nNumRec=0;      /* should be zero */

      if(nAnt&CasAntEu){        nAnt&=~CasAntEu;   pHstEu=pHgram;}
      else if(nAnt&CasAntEv){   nAnt&=~CasAntEv;   pHstEv=pHgram;}
      else if(nAnt&CasAntEx){   nAnt&=~CasAntEx;   pHstEx=pHgram;}
      else if(nAnt&CasAntEz){   nAnt&=~CasAntEz;   pHstEz=pHgram;}
      else if(nAnt&CasAntBx){   nAnt&=~CasAntBx;   pHstBx=pHgram;}
      else if(nAnt&CasAntBy){   nAnt&=~CasAntBy;   pHstBy=pHgram;}
      else if(nAnt&CasAntBz){   nAnt&=~CasAntBz;   pHstBz=pHgram;}
      else if(nAnt&CasAntLPs){  nAnt&=~CasAntLPs;  pHstLs=pHgram;}
      else if(nAnt&CasAntHF){   nAnt&=~CasAntHF;   pHstHf=pHgram;}
      else                      assert(0);
    }/* while */

  }/* fi first time */


  /* make the histogram */
  nTotRec=nNumRec-3;
  pByte=arPds+2*nRecLen;  /* next record is the first data record */
  for(nRec=3;nRec<nNumRec;++nRec){

    pByte+=nRecLen;                  /* start of the next data record */
    pDword=(void*)pByte;
    nSclkSec= *pDword++;             /* 0-3 */
    nSclkFine=*pDword++;             /* 4-7 */
      nScetDays=nSclkFine;  
      nSclkFine>>=16;
      nScetDays&=0x0FFFF;
    nScetMsec=   *pDword++;         /*  8-11 */
    nDataQuality=*pDword++;         /* 11-15 */
    nSensorId=nDataQuality;
      nDataQuality&=0xFFFFFFF0;         
      nSensorId&=   0x0000000F;         
    pAmpl=(void*)pDword;            /* (float*)(pByte+16); */    


    /* transform the bit field */
    switch(nSensorId){
      case  0:  nSensorId=CasAntEx;   pHgram=pHstEx;  break;
      case  1:  nSensorId=CasAntEu;   pHgram=pHstEu;  break;
      case  2:  nSensorId=CasAntEv;   pHgram=pHstEv;  break;
      case  3:  nSensorId=CasAntEz;   pHgram=pHstEz;  break;
      case  4:  nSensorId=CasAntBx;   pHgram=pHstBx;  break;
      case  5:  nSensorId=CasAntBy;   pHgram=pHstBy;  break;
      case  6:  nSensorId=CasAntBz;   pHgram=pHstBz;  break;
      case  8:  nSensorId=CasAntHF;   pHgram=pHstHf;  break;
      case 11:  nSensorId=CasAntLPs;  pHgram=pHstLs;  break;
      default:  assert(0);  break;
    }      


    /* filter by scet */ 
    if( nScetDays < nBgdScetDaysBeg )       continue;
    else if( nScetDays > nBgdScetDaysEnd )  continue;
    else if( (nScetDays==nBgdScetDaysBeg) && 
             (nScetMsec<nBgdScetMsecBeg) )  continue;
    else if( (nScetDays==nBgdScetDaysEnd) &&
             (nScetMsec>nBgdScetMsecEnd) )  continue;
    else{
      ;/* null statement */
    }

    /* raj filter change Janurary 6, 2005 : cmdln_nNixBadPkt, allows the 
      command line arguments to determine the data set for bgd.  Currently,
      bad_data,hfr_snd,lp_rswp,fake_data,dpf_zero.  Lfdr/Mfdr fake data and
      digital prescale factor = 0 is noted in the data quality, but not marked
      in the data.  The same is true for bad packets; only sources of 
      interferenced are marked.  Discard data sets and continue.  */
    if(nDataQuality&cmdln_nNixBadPkt){
      ++nDisCnt; 
      continue;
    }
    ++nBgdCnt;
/*
       insert bad packet check here, discard and continue 
    if(nDataQuality&0xEFFFFFF0){ allow fake data sets 
      ++nDisCnt; 
      continue;
    }
    ++nBgdCnt;
*/
    for(i=0;i<nFreq;i++){
      if(pAmpl[i]<0.0)         nTmp=0;     /* discard interference points */
      else if(pAmpl[i]<1E-20)  nTmp=2000;  /* trap for zero               */
      else if(pAmpl[i]>1.0)    nTmp=0;
      else                     nTmp=(-100)*log10(pAmpl[i]);
/*
      if(pAmpl[i]<1E-20)    nTmp=2000;   trap for zero 
      else if(pAmpl[i]>1.0) nTmp=0;
      else                  nTmp=(-100)*log10(pAmpl[i]);
*/
      pHgram->arHist[i][nTmp]+=1;
    }
    pHgram->nNumRec++;
  }/* for nRec, processing each record from the archive file */

}/* for nIdx */ 

  /* ntype should be unmodified from the top */
  nAnt=nType&CasAntMask;
  while(nAnt){
  Ulong nNumOcr;
    if(nAnt&CasAntEu){        nTmp=CasAntEu;   pHgram=pHstEu;}
    else if(nAnt&CasAntEv){   nTmp=CasAntEv;   pHgram=pHstEv;}
    else if(nAnt&CasAntEx){   nTmp=CasAntEx;   pHgram=pHstEx;}
    else if(nAnt&CasAntEz){   nTmp=CasAntEz;   pHgram=pHstEz;}
    else if(nAnt&CasAntBx){   nTmp=CasAntBx;   pHgram=pHstBx;}
    else if(nAnt&CasAntBy){   nTmp=CasAntBy;   pHgram=pHstBy;}
    else if(nAnt&CasAntBz){   nTmp=CasAntBz;   pHgram=pHstBz;}
    else if(nAnt&CasAntLPs){  nTmp=CasAntLPs;  pHgram=pHstLs;}
    else if(nAnt&CasAntHF){   nTmp=CasAntHF;   pHgram=pHstHf;}
    else                      assert(0);
    nAnt&=~nTmp;

    arHist=pHgram->arHist;  /* ulong */
    arBgnd=pHgram->arBgnd;  /* float */
    nFreq=pHgram->nItems;   /* number of frequencies */
    nNumRec=pHgram->nNumRec;

    /* find the Nth percent for the background */
    nNumOcr=nNumRec*(nPercent/100.0) + 0.5; 

    pReport+=sprintf(pReport,"  %08X: nNumRec=%d, nNumOcr=%d, nPercent=%d\n",
                     (nType&~CasAntMask)|nTmp,nNumRec,nNumOcr,nPercent);
/*
    if(bSilent==false)
      fprintf(stderr,"%08lX: nNumRec=%ld, nNumOcr=%ld, nPercent=%d\n",
              (nType&~CasAntMask)|nTmp,nNumRec,nNumOcr,nPercent);
*/

    for(i=0;i<nFreq;i++){
      nTmp=0;
      for(j=2000;j>0;j--){
        nTmp+=arHist[i][j];
        if(nTmp>nNumOcr)  
          break;
      }
      arBgnd[i]=1.0/pow(10.0,(j/-100.0));  /* invert for multiply */
    }

  }/* while */

  if(bSilent==false){
    fprintf(stderr,"Bgd %08X :: discarded %d, kept %d, total %d records\n",
            nType,nDisCnt,nBgdCnt,nTotRec);
    fprintf(stderr,"%s",sReport);
  }

return pHgram;
}



void CasPds_Dump_LogBinHist(CasHistogram *pHgram)
{
char sName[128];
int f,i;
double dTmp;
FILE *hOut;

/*
Ulong (*arHist)[CAS_HIST_MAX_BINS];
float *arBgnd;
    arHist=pHgram->arHist;  
    arBgnd=pHgram->arBgnd; 
    nFreq=pHgram->nItems; 
    nNumRec=pHgram->nNumRec;
*/
    for(f=0;f<pHgram->nItems;f++){
      sprintf(sName,"mfdr%02d.hst",f);
      hOut=fopen(sName,"wb"); 
      for(i=0;i<2001;i++){
        dTmp=i;
        dTmp/=-100;
        dTmp=pow(10.0,dTmp);
        fprintf(hOut,"%4d %8d %10.5E\n",i,pHgram->arHist[f][i],dTmp);
      }
      fclose(hOut);
    }

fprintf(stderr,"nFreq=%d, nNumRec=%d\n",pHgram->nItems,pHgram->nNumRec);

return;
}



CasHistogram* CasPds_HfrJup_Hist(CasPdsFileList *pList,Ulong nIdxBeg,int nNoiseProfile)
{
int i,j,nIdx;
Uchar *arPds=arPdsFileBuf,*pByte;
Ulong nRecLen,nNumRec,nFreq,nAnt;
float *pTime,*pFreq;

bool bFirstTime=true;
Ulong (*arHist)[CAS_HIST_MAX_BINS];
float *arBgnd;
double dTmp;
CasHistogram *pHgram;



  nIdx=nIdxBeg; 
  if(CasPds_LRfull_ReadArchive(pList->sList[nIdx],arPds)==NULL){
    fprintf(stderr,"CasPds_LRfull_ReadArchive(%s) failed.\n",
            pList->sList[nIdx]);
    exit(0); 
  }

  /* assign pointers to stuff */
  pByte=arPds+8;
  nRecLen=*((Ulong*)pByte);  pByte+=4;  
  nNumRec=*((Ulong*)pByte);  pByte+=4;
  nFreq=(nRecLen-16)/4;             /* number of data points in the record */
  pTime=(float*)(pByte+1*nRecLen);  /* start of time skipping sclk/scet */
  pFreq=(float*)(pByte+2*nRecLen);  /* start of freq skipping sclk/scet */


  /* scarf off some memory for the histograms */
  if(bFirstTime==true){
    bFirstTime=false; 

    /* clean-up the mess from before */
    if(pHstEu!=NULL){free(pHstEu->arBgnd);free(pHstEu->arHist);free(pHstEu);} 
    if(pHstEv!=NULL){free(pHstEv->arBgnd);free(pHstEv->arHist);free(pHstEv);}
    if(pHstEx!=NULL){free(pHstEx->arBgnd);free(pHstEx->arHist);free(pHstEx);}
    if(pHstEz!=NULL){free(pHstEz->arBgnd);free(pHstEz->arHist);free(pHstEz);}

    if(pHstBx!=NULL){free(pHstBx->arBgnd);free(pHstBx->arHist);free(pHstBx);}
    if(pHstBy!=NULL){free(pHstBy->arBgnd);free(pHstBy->arHist);free(pHstBy);}
    if(pHstBz!=NULL){free(pHstBz->arBgnd);free(pHstBz->arHist);free(pHstBz);}

    if(pHstLs!=NULL){free(pHstLs->arBgnd);free(pHstLs->arHist);free(pHstLs);}
    if(pHstHf!=NULL){free(pHstHf->arBgnd);free(pHstHf->arHist);free(pHstHf);}

    pHstEu=pHstEv=pHstEx=pHstEz=NULL;
    pHstBx=pHstBy=pHstBz=NULL;
    pHstLs=pHstHf=NULL;

    /* allocate space for the histograms */
    nAnt=CasAntEu|CasAntEv|CasAntEx|CasAntEz;
    while(nAnt){
      assert( (pHgram=calloc(1,sizeof(CasHistogram))) != NULL );
      assert( (arHist=calloc(nFreq*CAS_HIST_MAX_BINS,sizeof(Ulong))) !=NULL ); 
      assert( (arBgnd=calloc(nFreq,sizeof(float))) !=NULL );
      pHgram->arHist=arHist;  /* ulong */
      pHgram->arBgnd=arBgnd;  /* float */
      pHgram->nItems=nFreq;
      pHgram->nNumRec=0;      /* should be zero */

      if(nAnt&CasAntEu){        nAnt&=~CasAntEu;   pHstEu=pHgram;}
      else if(nAnt&CasAntEv){   nAnt&=~CasAntEv;   pHstEv=pHgram;}
      else if(nAnt&CasAntEx){   nAnt&=~CasAntEx;   pHstEx=pHgram;}
      else if(nAnt&CasAntEz){   nAnt&=~CasAntEz;   pHstEz=pHgram;}
      else if(nAnt&CasAntBx){   nAnt&=~CasAntBx;   pHstBx=pHgram;}
      else if(nAnt&CasAntBy){   nAnt&=~CasAntBy;   pHstBy=pHgram;}
      else if(nAnt&CasAntBz){   nAnt&=~CasAntBz;   pHstBz=pHgram;}
      else if(nAnt&CasAntLPs){  nAnt&=~CasAntLPs;  pHstLs=pHgram;}
      else if(nAnt&CasAntHF){   nAnt&=~CasAntHF;   pHstHf=pHgram;}
      else                      assert(0);
    }/* while */

  }/* fi first time */

  /* make the histogram for hfr monopole */
  for(i=0;i<nFreq;i++){
    pHstEz->arBgnd[i]=CasHfr_GalNsJupM[0];
    for(j=0;j<206;j++){
      if((CasHfr_GalNsJupM_Freq[j]*1.0E3)<=pFreq[i])
        pHstEz->arBgnd[i]=CasHfr_GalNsJupM[j];
      else
        break;
    }
/*
fprintf(stderr,"i=%d j=%d  %9.3f <= %9.3f => %9.3f  %.4G\n",i,j,
  CasHfr_GalNsJupM_Freq[j-1],pFreq[i],CasHfr_GalNsJupM_Freq[j],
  pHstEz->arBgnd[i]);
*/
  }
  for(i=0;i<nFreq;i++){
    dTmp=pHstEz->arBgnd[i];
    dTmp=pow(10.0,dTmp/20.0);       /* dBv/rtHz -> V/rtHz */
    dTmp=dTmp*2.49459;              /* base capactiance 7.94dB */
    dTmp=(dTmp*dTmp)/(5.0*5.0);     /* V^2/m^2/Hz */
    pHstEz->arBgnd[i]=1.0/dTmp;     /* invert for multiply */
    pHstEu->arBgnd[i]=1.0/dTmp;     /* invert for multiply */
    pHstEv->arBgnd[i]=1.0/dTmp;     /* invert for multiply */
  }

  /* make the histogram for hfr dipole */
  for(i=0;i<nFreq;i++){
    pHstEx->arBgnd[i]=CasHfr_GalNsJupD[0];
    for(j=0;j<206;j++){
      if((CasHfr_GalNsJupD_Freq[j]*1.0E3)<=pFreq[i])
        pHstEx->arBgnd[i]=CasHfr_GalNsJupD[j];
      else
        break;
    }
/*
fprintf(stderr,"i=%d j=%d  %9.3f <= %9.3f => %9.3f  %.4G\n",i,j,
  CasHfr_GalNsJupM_Freq[j-1],pFreq[i],CasHfr_GalNsJupM_Freq[j],
  pHstEx->arBgnd[i]);
*/
  }
  for(i=0;i<nFreq;i++){
    dTmp=pHstEx->arBgnd[i];
    dTmp=pow(10.0,dTmp/20.0);       /* dBv/rtHz -> V/rtHz */
    dTmp=dTmp*2.51478;              /* base capactiance 8.01dB */
    dTmp=(dTmp*dTmp)/(9.26*9.26);   /* V^2/m^2/Hz */
    pHstEx->arBgnd[i]=1.0/dTmp;     /* invert for multiply */
  }



return pHgram;
}
