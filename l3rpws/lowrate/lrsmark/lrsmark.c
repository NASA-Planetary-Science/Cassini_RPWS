#include <stdio.h> 
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <assert.h>
#include <stdbool.h>

#include <rpwstlm/CasType.h>
#include <rpwstlm/CasLp.h>
#include <rpwstlm/CasSpice.h>
#include <rpwstlm/CasMiniPacket.h>
#include <rpwstlm/CasCmdParse.h>
#include <rpwstlm/CasPds.h> 


#define LRSDQ_BAD_PKT    (Ulong)0x80000000
#define LRSDQ_HFR_SND    (Ulong)0x40000000
#define LRSDQ_LP_RSWP    (Ulong)0x20000000
#define LRSDQ_GND_PROC   (Ulong)0x10000000
#define LRSDQ_DPF_ZERO   (Ulong)0x08000000

const char *sVersion="lrsmark() ver 1.2";

#define INST_INT_LIST_MAX  8192
typedef struct interferencelist_tag{
  char  sReceiver[16];  /* col 1-8   */
  Ulong  nCdsSclk;      /* col 10-17 , Mini Packet Time */
  Ulong    nMpRti;      /* col 19-23 , Mini Packet Time */
  char   sEvtTime[32];  /* col 24-44 , Event Capture Time */
  float fDuration;      /* col 46-51 */
  Ulong nMode;          /* col 53-60 */
  char  sComments[32];  /* col 62-80 */
  Ulong nBegSclk,nBegFine,nEndSclk,nEndFine;  /* Interference Interval */
  Ulong     nDataQuality;  /* PDS header bits */
  Ulong     nType;
  char      bUsed;
}InterferenceList;

bool sort_pds_archive(Uchar *pPdsBuf);  /* returns true if archive was sorted */
int read_inst_int_file(char *sFileName, InterferenceList*** pX);
void dump_inst_int_file(InterferenceList** x, FILE *h);
int make_lfdr_list(InterferenceList** x, InterferenceList** lfdr);
int make_mfdr_list(InterferenceList** x, InterferenceList** mfdr);
int make_mfr_list(InterferenceList** x, InterferenceList** mfr);
int make_hfr_list(InterferenceList** x, InterferenceList** hfr);
int make_msc_list(InterferenceList** x, InterferenceList** msc);
float lfdr_duration(Ulong nMode);
void show_help(FILE *h);

Uchar arPdsFileBuf[CAS_PDS_MAX_FILE_SIZE];  /* 64MB buffer */

/*
  Version 1.2 March 8, 2005
    implement sub rti interference timing

*/

int main(int argc,char *argv[])
{
bool bMatch;
char sReceiver[16];
Uchar *pByte;
int i,j;
Ulong nBegSclk,nBegFine,nEndSclk,nEndFine,nEpoch;
Ulong *pDword;
FILE *hOut;

/* Command Line Interface */
bool bVerbose=false,bSilent=false,bClean=true,bMark=true;

/* Pds Files */
char *sFiles[1024];
Ulong nFiles;

/* Pds Data Structure */
int nItems;
Ulong nRecLen,nNumRec,nMode,nRec;
Ulong nSclkSec,nSclkFine,nScetDays,nScetMsec,nDataQuality,nSensorId;
Ulong *pDataQuality;
float *pTime,*pFreq,*pAmpl;
float fBegInt,fEndInt,fBegRec,fBegSam,fEndSam;
float fDuration,arSampleWidth[224];

/* Interference List */
int nIntLen = 0;
char *sInterferenceFilename=NULL;
int nList,nListBase;
InterferenceList **pList;
InterferenceList **iif = NULL;
InterferenceList **lfdr = NULL;
InterferenceList **mfdr = NULL;
InterferenceList **mfr = NULL;
InterferenceList **hfr = NULL;
InterferenceList **msc = NULL;

/* housekeeping */
clock_t tProcTime;
time_t tElaspedTime;
Ulong nBytesRead,nBytesWrote,nWrote;



  tProcTime=clock();
  tElaspedTime=time(NULL);



  nFiles=0;
  while(--argc){
    ++argv;
    if(!strcmp("-help",*argv) || !strcmp("-h",*argv)){
      show_help(stdout);
      exit(0);
    }
    else if(!strcmp("-c",*argv) || !strcmp("-clean",*argv)){
      bClean=false;
    }
    else if(!strcmp("-m",*argv) || !strcmp("-mark",*argv)){
      bMark=false;
    }
    else if(!strcmp("-i",*argv)){
      --argc;  ++argv;
      sInterferenceFilename=*argv;
    }
    else if(!strcmp("-s",*argv))
      bSilent=true;
    else if(!strcmp("-v",*argv)){
      bVerbose=true;
    }
    else if((!strcmp("-ver",*argv)) || (!strcmp("-version",*argv))){
      fprintf(stderr,"%s\n",sVersion);
      fprintf(stderr,"  %s\n",CasTlm_Version());
    }
    else{
      sFiles[nFiles++]=*argv;
      sFiles[nFiles]=NULL;
    }/* esle */
  }/* while parsing command line argurments */


  if(bVerbose==true)  
    bSilent=false;

  /* init spice kernels */
  CasSpice_Init(NULL);

  nIntLen = 0;
  if(sInterferenceFilename!=NULL){
    nIntLen = read_inst_int_file(sInterferenceFilename,&iif);
  }
  
  /* If no files here, allocate one spot for each list, also assume that
     NULL == 0 (AFAIK, not part of the C standard */
  if(nIntLen == 0) nIntLen = 1;
  
  lfdr = (InterferenceList**)calloc(nIntLen, sizeof(void*));
  mfdr = (InterferenceList**)calloc(nIntLen, sizeof(void*));
  mfr  = (InterferenceList**)calloc(nIntLen, sizeof(void*));
  hfr  = (InterferenceList**)calloc(nIntLen, sizeof(void*));
  msc  = (InterferenceList**)calloc(nIntLen, sizeof(void*));
  
  make_lfdr_list(iif,lfdr);
  make_mfdr_list(iif,mfdr);
  make_mfr_list(iif,mfr);
  make_hfr_list(iif,hfr);
  make_msc_list(iif,msc);

  if(bVerbose==true && 0){
    for(j=0;j<0;j++){
      switch(j){
        case 0:  pList=lfdr;  fprintf(stderr,"LFDR List\n"); break;
        case 1:  pList=mfdr;  fprintf(stderr,"MFDR List\n"); break;
        case 2:  pList=mfr;   fprintf(stderr,"MFR List\n"); break;
        case 3:  pList=hfr;   fprintf(stderr,"HFR List\n"); break;
        case 4:  pList=msc;   fprintf(stderr,"MSC List\n"); break;
      }
      i=0;
      while(pList[i]!=NULL){
        fprintf(stderr,"%-8s %08X %04X %s %6.3f %08X %s\n",
                pList[i]->sReceiver,pList[i]->nCdsSclk,pList[i]->nMpRti,
                pList[i]->sEvtTime,pList[i]->fDuration,pList[i]->nMode,
                pList[i]->sComments);
        ++i;
      }
    }
    
  }







  nFiles=0;  nBytesRead=nBytesWrote=0;
  while(sFiles[nFiles]!=NULL){

    if(CasPds_LRfull_ReadArchive(sFiles[nFiles],arPdsFileBuf)==NULL){
      fprintf(stderr,"CasPds_LRfull_ReadArchive(%s) failed.\n",
              sFiles[nFiles]);
      exit(1); 
    }

    /* sort the archive data */
    if(sort_pds_archive(arPdsFileBuf) == true)
      fprintf(stderr," archive out of time order, sorting %s\n",sFiles[nFiles]);
    else
      fprintf(stderr," archive in time order, %s\n",sFiles[nFiles]);

 
    /* assign pointers to stuff */
    pDword=(void*)(arPdsFileBuf+8);  /* CORPWS01  bytes 0-7 */
    nRecLen=*pDword++;               /* record length bytes 8-11 */
    nNumRec=*pDword++;               /* number of records bytes 12-15 */
    nMode=*pDword;                   /* mode 16-19, no increment */
    nBytesRead+=(nNumRec*nRecLen);
    if(bVerbose==true){  
      fprintf(stderr,"%2d: reading %s\n",nFiles,sFiles[nFiles]);
      fprintf(stderr,"    %d bytes, %d records of %d bytes\n",
              nBytesRead,nNumRec,nRecLen);
    }
 
    pTime=(void*)(arPdsFileBuf+1*nRecLen+16);  /* Time Offset Record */
    pFreq=(void*)(arPdsFileBuf+2*nRecLen+16);  /* Freqeuency Record */
    nItems=(nRecLen-16)/4;                     /* items in the record */

    switch(nMode&CasModeMask){
      case CasLfdr_Normal:      
        pList=lfdr;
/*
        pTime=arTimeLfdr;
        arTimeLfdr[31]=lfdr_duration(nMode);
        for(i=0;i<32;i++)  
          arTimeLfdr[i]=arTimeLfdr[31];
*/
        fDuration=lfdr_duration(nMode);  /* total record capture time */
        strcpy(sReceiver,"LFDR");
        assert(nItems==32);
        break; 
      case CasMfdr_Normal:      
        pList=mfdr;
/*
        pTime=arTimeLfdr;
        arTimeLfdr[31]=lfdr_duration(nMode);
        for(i=0;i<32;i++)  
          arTimeLfdr[i]=arTimeLfdr[31];
*/
        fDuration=lfdr_duration(nMode);  /* total record capture time */
        strcpy(sReceiver,"MFDR");
        assert(nItems==32);
        break; 
      case CasMfr_Normal:     
        pList=mfr;
        fDuration=32.0;
        for(i=0;i<224;i++){  
          if(pFreq[i]<180.0)        arSampleWidth[i]=1.00;/*  23.89   169.00Hz*/
          else if(pFreq[i]<1500.0)  arSampleWidth[i]=0.50;/* 192.11  1470.09Hz*/
          else                      arSampleWidth[i]=0.25;/*1536.89 11799.33Hz*/
        }
        strcpy(sReceiver,"MFR_nrm");   
        assert(nItems==224);
        break;
      case CasMfr_FastToggle:   
        pList=mfr;
        fDuration=16.0;   /* mfr capture is split into two pds records */
        for(i=0;i<112;i++){  
          if(pFreq[i]<180.0)        arSampleWidth[i]=1.00;/*  23.89   169.00Hz*/
          else if(pFreq[i]<1500.0)  arSampleWidth[i]=0.50;/* 192.11  1470.09Hz*/
          else                      arSampleWidth[i]=0.25;/*1536.89 11799.33Hz*/
        }
        strcpy(sReceiver,"MFR_ftg");   
        assert(nItems==112);
        break;
      case CasHfr_Analysis:
        pList=hfr;
        fDuration=0.0;
        for(i=0;i<nItems;i++){
          if(pTime[i]>fDuration)  
            fDuration=pTime[i];
        }
        fDuration+=1.0; /* grace period for last sample */ 
        strcpy(sReceiver,"HFR_ana");   
        break;
      case CasHfr_Millisecond:
        pList=msc;
        fDuration=0.0;
        for(i=0;i<nItems;i++){
          if(pTime[i]>fDuration)  
            fDuration=pTime[i];
        }
        fDuration+=1.0; /* grace period for last sample */ 
        strcpy(sReceiver,"HFR_msc");   
        break;
      default:
        strcpy(sReceiver,"unknown");   
        fprintf(stderr,"%s - error processing %s\n",sVersion,sFiles[nFiles]);
        exit(1);  break;
    }
    if(pList[0]==NULL){
      fprintf(stderr," no interference list\n");
      ++nFiles;
      continue;
    } 

    if(bVerbose==true){
      fprintf(stderr,"%s LIST\n",sReceiver);
fprintf(stderr,"Duration = %f\n",fDuration);
/*
      i=0;
      while(pList[i]!=NULL){
        fprintf(stderr,"%-8s %08X %04X %s %6.3f %08X %s\n",
                pList[i]->sReceiver,pList[i]->nCdsSclk,pList[i]->nMpRti,
                pList[i]->sEvtTime,pList[i]->fDuration,pList[i]->nMode,
                pList[i]->sComments);
        ++i;
      }
*/
    }
    


    nList=nListBase=0;
    pByte=arPdsFileBuf+3*nRecLen;  /* start of the first data record */
    /* decide on a epoch for the processing interval */
    pDword=(void*)pByte;
    nSclkSec=*pDword;  /* nSclkSec bytes 0-3 */
    if(nSclkSec < pList[0]->nBegSclk)
      nEpoch=nSclkSec;
    else
      nEpoch=pList[0]->nBegSclk-1;

    for(nRec=3;nRec<nNumRec;++nRec,pByte+=nRecLen){

      pDword=(void*)pByte;
      nSclkSec  = *pDword++;    /* nSclkSec bytes 0-3 */
      nSclkFine = *pDword++;    /* sclk fine bytes 4-5, scet days bytes 6-7 */
      nScetMsec = *pDword++;    /* scet msec bytes 8-11 */
      pDataQuality = pDword++;  /* data quality bytes 12-14 */
      pAmpl=(void*)pDword;      /* data begins here */

      nScetDays = nSclkFine;
      nSclkFine >>= 16;               /* sclk fine bytes 4-5  */
      nScetDays &= 0x0FFFF;           /* scet days bytes 6-7  */ 
      nDataQuality = *pDataQuality;      
      nSensorId = nDataQuality;
      nDataQuality &= 0xFFFFFFF0;     /* Data Quality bytes 12 - 14 */
      nSensorId    &= 0x0000000F;     /* Sensor Id lower nibble of byte 15 */


      if(bClean==true){
        *pDataQuality &= 0x0000000F;    /* Data Quality bytes 12 - 14 */
        for(i=0;i<nItems;i++){
          if(pAmpl[i]<0.0)
            pAmpl[i]*=-1.0;
        }
      }
      if(bMark==false)
        continue;


      nBegSclk = nSclkSec;
      nBegFine = nSclkFine;
      nEndSclk = nBegSclk+((int)fDuration);
      nEndFine = nBegFine;
      nEndFine+=(Ulong)ceil((fDuration-(Ulong)fDuration)/0.125)*32;
      if(nEndFine>0x0FF){ nEndSclk+=nEndFine/256; nEndFine&=0x0FF; }


      /* assume the pds record is time ordered and the list is as well */
      /* the lfdr has flight and ground data; together there is no time order */
      /* within the stream, assume the list is time ordered. */
/*
      if(pList[nListBase]==NULL && nListBase>0){
        if(nEndSclk < pList[nListBase-1]->nBegSclk)
          nListBase=0;     reset list pointer for the unordered lfdr
      }      
*/

      /* update list base pointer for the current pds data record */
      while(pList[nListBase]!=NULL){
        if(pList[nListBase]->nEndSclk < nBegSclk)
          ++nListBase;
        else if((pList[nListBase]->nEndSclk == nBegSclk) &&
                (pList[nListBase]->nEndFine <  nBegFine)) 
          ++nListBase;
        else
          break;
      }
      nList=nListBase;

      /* cycle through interference list and mark packets */
      while(pList[nList]!=NULL){

        if(pList[nList]->nEndSclk < nBegSclk){
          ++nList;
          continue;  /* interference before current data set */
        }
        else if(nEndSclk < pList[nList]->nBegSclk){
          break;     /* interference after current data set */
        }
        else if((nBegSclk == pList[nList]->nEndSclk) &&
                (nBegFine >  pList[nList]->nEndFine)){
          ++nList;
          continue;  /* interference before current data set */
        }
        else if((nEndSclk == pList[nList]->nBegSclk) &&
                (nEndFine <  pList[nList]->nBegFine)){
          break;  /* interference after current data set */
        }
        else if( (nBegSclk <= pList[nList]->nEndSclk) && 
                 (nEndSclk >= pList[nList]->nBegSclk) ){
          ;/* do nothing, log hits */
        }
        else{
          /* sanity check */
          fprintf(stderr,"interference check failed\n");
          fprintf(stderr,"  %08X %s\n",nBegSclk,sFiles[nFiles]);
          exit(1); 
        }

        bMatch=false;
        /* Case I - interference is a bad pkt, mark the whole packet,  */
        /*          ignore duration, and match exact event times */
        /*          hfr snd packets may be bad packet  */
        if( (pList[nList]->nMode&CasBadPacket) && 
            ((pList[nList]->nMode&CasModeMask) == (nMode&CasModeMask)) ){
          if(nBegSclk == pList[nList]->nBegSclk){
            /* status bit, don't mark the packet, part or whole is trashed */
/*
            for(i=0;i<nItems;i++){
              if(pAmpl[i]>0.0)
                pAmpl[i]*=-1.0;
            }
*/
            *pDataQuality |= pList[nList]->nDataQuality;
            bMatch=true;
          }/* exact sclk match */
        }/* case i */

        /* Case II - interference trashes whole pkt, lfdr and mfdr */
        if((nMode&CasMp_Mask)==CasMp_Wfdr){  /* Lfdr Mfdr */
          /* don't mark status bits for ground produced/digital prescale = 0 */
          if( pList[nList]->nDataQuality & ~(LRSDQ_GND_PROC|LRSDQ_DPF_ZERO) ){
            for(i=0;i<nItems;i++){
              if(pAmpl[i]>0.0)
                pAmpl[i]*=-1.0;
            }
          }
           
          bMatch=true;
          *pDataQuality |= pList[nList]->nDataQuality&~LRSDQ_BAD_PKT;
          /* bad data packets are marked above, hfr snd pkts may be bad */
        }/* case ii */
        /* Case III - interference trashes part of mfr pkt */
        else if((nMode&CasMp_Mask)==CasMp_Mfr){  /* Mfr */
          fBegInt = pList[nList]->nBegSclk - nEpoch;
          fBegInt += pList[nList]->nBegFine / 256.0;
          fEndInt = pList[nList]->nEndSclk - nEpoch;
          fEndInt += pList[nList]->nEndFine / 256.0;
          fBegRec = nBegSclk - nEpoch;
          fBegRec += nBegFine / 256.0;

          for(i=0;i<nItems;i++){
            fBegSam = fBegRec + pTime[i];
            fEndSam = fBegSam + arSampleWidth[i];
            if( (fBegSam <= fEndInt) && (fEndSam >= fBegInt) ){/* a match */
              bMatch=true;
              if(pAmpl[i]>0.0)
                pAmpl[i]*=-1.0;
            }
          }
          if(bMatch==true)
            *pDataQuality |= pList[nList]->nDataQuality&~LRSDQ_BAD_PKT;;
            /* bad data packets are marked above, hfr snd pkts may be bad */
        }/* case iii */
        /* Case IV - interference trashes part of pkt, hfr analysis */
        else if((nMode&CasModeMask)==CasHfr_Analysis){
          fBegInt = pList[nList]->nBegSclk - nEpoch;
          fBegInt += pList[nList]->nBegFine / 256.0;
          fEndInt = pList[nList]->nEndSclk - nEpoch;
          fEndInt += pList[nList]->nEndFine / 256.0;
          fBegRec = nBegSclk - nEpoch;
          fBegRec += nBegFine / 256.0;

          for(i=0;i<nItems;i++){
            fBegSam = fBegRec + pTime[i];
            fEndSam = fBegSam + 0.5;  /* hfr sample width ~ < 1/8 sec Band ABC*/
            if( (fBegSam <= fEndInt) && (fEndSam >= fBegInt) ){/* a match */
              bMatch=true;
              if(pAmpl[i]>0.0)
                pAmpl[i]*=-1.0;
            }
          }
          if(bMatch==true)
            *pDataQuality |= pList[nList]->nDataQuality&~LRSDQ_BAD_PKT;;
            /* bad data packets are marked above, hfr snd pkts may be bad */
        }/* case iv */
        /* Case V hfr millisecond and hfr sounder */
        else{ 
          ; /* execute noop (do nothing) */
        }/* case v */


        if((bMatch==true) && (bVerbose==true)){
            fprintf(stderr,"%-8s %08X %02X %s r%d l%d %s\n",
                    sReceiver,nBegSclk,nBegFine,
                    CasSpice_nSclk_to_sScet(nBegSclk,nBegFine,NULL),
                    nRec,nList,pList[nList]->sComments);
            fprintf(stderr,"%-8s %08X %02X %s\n"," ",nEndSclk,nEndFine,
                    CasSpice_nSclk_to_sScet(nEndSclk,nEndFine,NULL));
            fprintf(stderr,"%-8s %08X %02X %s\n","inter",
                    pList[nList]->nBegSclk,pList[nList]->nBegFine,
                    CasSpice_nSclk_to_sScet(pList[nList]->nBegSclk,
                                            pList[nList]->nBegFine,NULL));
            fprintf(stderr,"%-8s %08X %02X %s\n","inter",
                    pList[nList]->nEndSclk,pList[nList]->nEndFine,
                    CasSpice_nSclk_to_sScet(pList[nList]->nEndSclk,
                                            pList[nList]->nEndFine,NULL));
        }


        ++nList;
      }/* while interference list */



    }/* for processing each record from the archive file */

    if((hOut=fopen(sFiles[nFiles],"wb"))==NULL){
      fprintf(stderr,"error writing %s\n",sFiles[nFiles]);
      exit(1);
    }
    nWrote=fwrite(arPdsFileBuf,sizeof(Uchar),nNumRec*nRecLen,hOut);
    nBytesWrote+=nWrote;
    assert(nWrote==(nNumRec*nRecLen));
    fclose(hOut);

    ++nFiles;
  }/* while reading file list */
  /* fflush(stdout); */

  fprintf(stderr,"Bytes Read %d and Wrote %d to pds data archive\n",
          nBytesRead,nBytesWrote);

  fprintf(stderr,"Pds Reader Time (clock ticks) %d to %d, ",
          (int)tProcTime,(int)clock());
  fprintf(stderr,"%.3f seconds\n",(clock()-tProcTime)/1.0E6);
  fprintf(stderr,"Elasped Time=%.6f seconds (rounded to the nearest second)\n",
          difftime(time(NULL),tElaspedTime) );

return 0;
}




/*
-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
*/






/*
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
*/




void show_help(FILE *h)
{

fprintf(h,"%s\n",sVersion);
fprintf(h,"  usage: lrsmark [OPTIONS] -i IFILE FILES\n");
fprintf(h,"-h | -help shows this page\n");

fprintf(h,
"-c | -clean     sets clean to false, default is true.  Clean, when true,\n"
"    scrubs data quality bits and takes the absolute value of the data.\n"
"    Clean can be used by itself to OR in new interference values.\n");

fprintf(h,
"-m | -mark      sets mark to false, default is true.  Mark, when true, marks\n"
"    the data values by multiplying by -1 and sets the corresponding data \n"
"    quality bits.  Mark can be used by itself to scrub the data set clean.\n");

fprintf(h,"-i IFILE     interference file to read, produced by lrsint\n");
fprintf(h,"-s     silent operation\n");
fprintf(h,"-v     verbose operation\n");
fprintf(h,"-ver | -version     show version and lib information\n");

return;
}


float lfdr_duration(Ulong nMode)
{
float fDuration;

  if((nMode&CasModeMask) == CasLfdr_Normal)
    fDuration=10.0E-3;
  else if((nMode&CasModeMask) == CasMfdr_Normal)
    fDuration=140.0E-6;
  else{
    fprintf(stderr,"%s - error with lfdr_duration(%08X)\n",sVersion,nMode);
    exit(1);
  }
  switch(nMode&CasWfdr_SizeMask){
    case CasWfdr_256:   fDuration*=256;   break;
    case CasWfdr_512:   fDuration*=512;   break;  
    case CasWfdr_1024:  fDuration*=1024;  break;
    case CasWfdr_2048:  fDuration*=2048;  break;
    default:
      fprintf(stderr,"%s - error with lfdr_duration(%08X)\n",sVersion,nMode);
      exit(1);  break;
  }


return fDuration;
}


int make_lfdr_list(InterferenceList** x,InterferenceList** lfdr)
{
int nItems=0,nList=0;

  while(x[nList]!=NULL){
    switch(x[nList]->nMode&CasModeMask){
      case CasLfdr_Normal:
      case CasHfr_Sounder:
      case CasLp_RawSweep:
        lfdr[nItems++]=x[nList];
        break;
      default:  
        break;
    }
    ++nList;
  }
  lfdr[nItems]=NULL;

return nItems;
}



int make_mfdr_list(InterferenceList** x, InterferenceList** mfdr)
{
int nItems=0,nList=0;

  while(x[nList]!=NULL){
    switch(x[nList]->nMode&CasModeMask){
      case CasMfdr_Normal:
      case CasHfr_Sounder:
      case CasLp_RawSweep:
        mfdr[nItems++]=x[nList];
        break;
      default:  
        break;
    }
    ++nList;
  }
  mfdr[nItems]=NULL;

return nItems;
}



int make_mfr_list(InterferenceList** x, InterferenceList** mfr)
{
int nItems=0,nList=0;

  while(x[nList]!=NULL){
    switch(x[nList]->nMode&CasModeMask){
      case CasMfr_Normal:
      case CasMfr_FastToggle:
      case CasHfr_Sounder:
      case CasLp_RawSweep:
        mfr[nItems++]=x[nList];
        break;
      default:  
        break;
    }
    ++nList;
  }
  mfr[nItems]=NULL;

return nItems;
}



int make_hfr_list(InterferenceList** x, InterferenceList** hfr)
{
int nItems=0,nList=0;

  while(x[nList]!=NULL){
    switch(x[nList]->nMode&CasModeMask){
      case CasHfr_Analysis:
        if(x[nList]->nMode&(Ulong)CasBadPacket)  /* list only bad packets */
          hfr[nItems++]=x[nList];
        break;
      case CasLp_RawSweep:
        hfr[nItems++]=x[nList];
        break;
      default:  
        break;
    }
    ++nList;
  }
  hfr[nItems]=NULL;

return nItems;
}



int make_msc_list(InterferenceList** x, InterferenceList** msc)
{
int nItems=0,nList=0;

  while(x[nList]!=NULL){
    switch(x[nList]->nMode&CasModeMask){
      case CasHfr_Millisecond:
      case CasLp_RawSweep:
        msc[nItems++]=x[nList];
        break;
      default:  
        break;
    }
    ++nList;
  }
  msc[nItems]=NULL;

return nItems;
}



int read_inst_int_file(char *sFileName,InterferenceList*** pX)
{
  char s[128],*p;
  int nEntries = 0;
  int n=0;
  Ulong nBegSclk,nBegFine,nEndSclk,nEndFine;
  FILE *h;
  InterferenceList** x;

  if((h=fopen(sFileName,"rt"))==NULL){
    fprintf(stderr,"unable to open instrument interference file %s.\n",
            sFileName);
    exit(1);
  }
  
  /* read through once to get the size of the array */
  while( (p = fgets(s, 128, h)) != NULL) nEntries++;
  fseek(h, 0L, SEEK_SET);
  
  if(nEntries == 0){
	 fprintf(stderr, "Warning: No entries in interference file %s\n", sFileName);
	 nEntries = 1;
  }
  
  *pX = (InterferenceList**)calloc(nEntries,sizeof(void*));
  x = *pX;
  
		  
  while( (p = fgets(s, 128, h)) != NULL){
	  
    x[n]=calloc(1,sizeof(InterferenceList));
    strncpy(x[n]->sReceiver,s,9);         /* col 1-8 */
      p=x[n]->sReceiver+8;
      while(*p==' ')  *p--='\0';      /* del spaces and null terminate string */
    x[n]->nCdsSclk=strtoul(s+9,NULL,16);  /* col 10-17 */ 
    x[n]->nMpRti=strtoul(s+18,NULL,16);   /* col 19-23 */
    strncpy(x[n]->sEvtTime,s+23,21);      /* col 24-44 */
      x[n]->sEvtTime[21]='\0';
    x[n]->fDuration=strtod(s+45,NULL);    /* col 46-51 */
    x[n]->nMode=strtoul(s+52,NULL,16);    /* col 53-60 */
    strncpy(x[n]->sComments,s+61,32);     /* col 62-80 */
      x[n]->sComments[31]='\0';
      p=x[n]->sComments;
      while(*p!='\0' && *p!='\n')  ++p;  *p='\0';


    /* compute event time and adjust/normalize it below */
    nBegSclk=GetEventTime(x[n]->nCdsSclk,x[n]->nMpRti);
    nBegFine=(x[n]->nMpRti<<5)&0x0FF;
    nEndSclk=nBegSclk+(Ulong)x[n]->fDuration;
    nEndFine=nBegFine;
    nEndFine+=(Ulong)ceil((x[n]->fDuration-(Ulong)x[n]->fDuration)/0.125)*32;

    if(x[n]->nMode&CasBadPacket)  
      x[n]->nDataQuality=LRSDQ_BAD_PKT;
    else
      x[n]->nDataQuality=0;
    switch(x[n]->nMode&CasModeMask){
      case CasHfr_Sounder:                
        x[n]->nDataQuality|=LRSDQ_HFR_SND;/* The antenna relay is switched    */
        nBegSclk-=0;                      /* before the capture starts and on */
        nBegFine-=0;                      /* the next non-sounder packet ?    */
        nEndSclk+=2;                      /* Add a grace period for the       */
        nEndFine+=0;                      /* switch transients to settle out. */
        break;                           
      case CasLp_RawSweep:                /* The sphere is negatively biased  */
        x[n]->nDataQuality|=LRSDQ_LP_RSWP;/* (-45V) 7-8 RTIs before the sweep */
        nBegSclk-=1;                      /* and returned to ground 1-2 RTIs  */
        nBegFine-=0;                      /* after the sweep is completed.    */
        nEndSclk+=0;                      
        nEndFine+=96;/* raj */
        break;                            
      case CasLfdr_Normal:                   
      case CasMfdr_Normal:                   
        if(x[n]->nMode&CasWfdr_Fake)         
          x[n]->nDataQuality|=LRSDQ_GND_PROC;
        if(x[n]->nMode&CasWfdr_DPFzero)
          x[n]->nDataQuality|=LRSDQ_DPF_ZERO;
        break;  
      default:
        break; 
    }
    /* normalize the time fields and compute the interference time */
    if(nBegFine>0x0FF){ nBegSclk-=nBegFine/256; nBegFine&=0x0FF; }
    if(nEndFine>0x0FF){ nEndSclk+=nEndFine/256; nEndFine&=0x0FF; }
    x[n]->nBegSclk=nBegSclk;  x[n]->nBegFine=nBegFine;
    x[n]->nEndSclk=nEndSclk;  x[n]->nEndFine=nEndFine;
/*
    CasSpice_nSclk_to_nScet(nBegSclk,nBegFine,&x[n]->nBegDays,&x[n]->nBegMsec);
    CasSpice_nSclk_to_nScet(nEndSclk,nEndFine,&x[n]->nEndDays,&x[n]->nEndMsec);
*/

    x[n]->nType=0;
    x[n]->bUsed=0;
    ++n;
  }
  x[n]=NULL;
  fclose(h);

return n;
}



void dump_inst_int_file(InterferenceList *x[],FILE *h)
{
int n=0;

  while(x[n]!=NULL){
    fprintf(h,"%s %s %08X %02X %08X %02X %.3f %s\n",
      x[n]->sReceiver,x[n]->sEvtTime,x[n]->nBegSclk,x[n]->nBegFine,
      x[n]->nEndSclk,x[n]->nEndFine,x[n]->fDuration,x[n]->sComments);

/*
    fprintf(h,"%s %08X %04X %s %.3f %08X %s\n",
      x[n]->sReceiver,x[n]->nCdsSclk,x[n]->nMpRti,x[n]->sEvtTime,
      x[n]->fDuration,x[n]->nMode,x[n]->sComments);
*/
    ++n;
  }

return;
}



bool sort_pds_archive(Uchar *pPdsBuf)
{
bool bOrder;
Uchar *pByte,*pBuf,*pSrc,*pDst,*pRec;
Ulong nRecLen,nNumRec,nMode,nRec,nItems,*pDword,nSclkBeg,nCnt;
Ulong nSclkSec;
Ulong UL_MAX=0xFFFFFFFF;
  

  /* assign pointers to stuff */
  pDword=(void*)(pPdsBuf+8);       /*  0 - 7  CORPWS01  bytes */
  nRecLen=*pDword++;               /*  8 - 11 record length bytes */
  nNumRec=*pDword++;               /* 12 - 15 number of records bytes */
  nMode=*pDword;                   /* 16 - 19 mode, no increment */

  nItems=(nRecLen-16)/4;           /* items in the record */

  switch(nMode&CasModeMask){
    case CasLfdr_Normal:      
      break; 
    case CasMfdr_Normal:      
      break; 
    case CasMfr_Normal:     
      break;
    case CasMfr_FastToggle:   
      break;
    case CasHfr_Analysis:
      break;
    case CasHfr_Millisecond:
      break;
    default:
      break;
  }

  /* check to see if the archive is already in time order */
  bOrder = true;
  nSclkBeg = 0x00;
  pByte = pPdsBuf + 3*nRecLen;  /* start of the first data record */
  for(nRec=3;nRec<nNumRec;++nRec,pByte+=nRecLen){
    pDword = (void*)pByte;
    nSclkSec = *pDword++;      /* nSclkSec bytes 0-3 */
    if(nSclkSec < nSclkBeg){
      bOrder = false;
      break;
    }
    else{
      nSclkBeg = nSclkSec;
    }
  }
  if(bOrder == true)
    return false;


  assert((pBuf=calloc(nNumRec*nRecLen,sizeof(Uchar))) != NULL);
  memcpy(pBuf,pPdsBuf,nNumRec*nRecLen);

  nCnt = 0;
  bOrder = false;
  pDst=pPdsBuf + 3*nRecLen;;
  while(bOrder == false){
    nSclkBeg = UL_MAX;
    pSrc=pBuf + 3*nRecLen;;
    for(nRec=3;nRec<nNumRec;++nRec,pSrc+=nRecLen){
      pDword=(void*)pSrc;
      nSclkSec = *pDword;     /* nSclkSec bytes 0-3 */
      if(nSclkSec < nSclkBeg){
        nSclkBeg = nSclkSec;
        pRec = pSrc;
      }
    }

    if(nSclkBeg == UL_MAX)  /* finished sorting */
      bOrder = true;
    else{
      memcpy(pDst,pRec,nRecLen);
      pDword=(void*)pRec;
      *pDword = UL_MAX;     /* nSclkSec bytes 0-3 */
      pDst+=nRecLen; 
      ++nCnt;
    }
    
  }/* elihw out of order */

  /* quick sanity check */
  pSrc=pBuf + 3*nRecLen;;
  for(nRec=3;nRec<nNumRec;++nRec,pSrc+=nRecLen){
    pDword=(void*)pSrc;
    nSclkSec = *pDword;     /* nSclkSec bytes 0-3 */
    if(nSclkSec != UL_MAX){
      fprintf(stderr,"sort failed\n");
      exit(1);
    } 
  }

fprintf(stderr,"nRec = %d, nCnt = %d\n",nNumRec,nCnt);

  free(pBuf);

return true;
}
