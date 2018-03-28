#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include <strings.h>
#include <string.h>

#include <rpwstlm/CasType.h>
#include <rpwstlm/RecordFile.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasSpice.h>
#include <rpwstlm/CasHfr.h>
#include <rpwstlm/CasMfr.h>
#include <rpwstlm/CasLp.h>
#include <rpwstlm/CasWfdr.h>

#include "lrsint.h"

/* 

Version 0.0
  Wednesday, October 21, 2004
  Monday, November 16, 2004
Version 1.0
  Wednesday, Janurary 12, 2005
    add cmdln switch for producing lfdr/mfdr fake status reports

Version 1.1
  Thursday, Janurary 20, 2005
    add check for mfr packet length

Version 1.2:  2012-10-25 cwp
  Switch to stdbool, use installed cassini libraries
   
*/



extern const char *sCasHfrMeanderVersion;


void Help(void);

static const char *sVersion="lrsint(), ver 1.2";


int main(int argc,char *argv[])
{
bool bVerbose=false,bSilent=false,bVersion=false,bLrsOnly=false,bLfdrFake=false;
char *sFiles[65536];
int nFiles=0;

char sReceiver[16];
Ulong nMode,nErrorMask;

char sTmp[128];


float fDuration;
int nInCnt,nErrCnt;
Ulong nRecLen,nStatus;

RecordFile *hInRec,*hOutRec;   /* Handles to the in & out files */
CasRecord *pInRec,*pOutRec;
Uchar *pMp;

FILE *hIn,*hOut,*hErr;


Ulong nCdsSclk,nMpRti,nEvtSclk,nEvtFine;

/* Hfr Sounder Special Case */
char sSndReceiver[32];
Ulong nSndCdsSclk=0,nSndMpRti=0,nSndEvtSclk=0,nSndEvtFine=0,nSndMode=0;
float fSndDuration=0;



/* Initialize Command Line Variables */
  hIn=stdin;  hOut=stdout;  hErr=stderr; 


  while(--argc){
    ++argv;
    if(!strcmp("-help",*argv)){
      Help();
      exit(1);
    }
    else if(!strcmp("-lfdr_fake",*argv)){
      bLfdrFake=true;
    }
    else if(!strcmp("-lrs",*argv)){
      bLrsOnly=true;
    }
    else if(!strcmp("-v",*argv)){
      bVerbose=true;
    }
    else if((!strcmp("-version",*argv)) || (!strcmp("-ver",*argv))){
      bVersion=true;
    }
    else{
      sFiles[nFiles++]=*argv;
      sFiles[nFiles]=NULL;
    }/* else */
  }

  if(bVerbose==true)
    bSilent=false;

  if(bVersion==true){
    fprintf(stderr,"%s\n",sVersion);
    fprintf(stderr,"  %s\n",CasTlm_Version());
  }

  CasSpice_Init(NULL);

  pInRec=CasRecord_Constructor(NULL);
  pOutRec=CasRecord_Constructor(NULL);

  hInRec=RecordFile_Constructor(NULL);
  if(nFiles==0){
    sFiles[0]=malloc(32);
    strcpy(sFiles[0],"stdin");
    sFiles[1]=NULL;
  }
  hOutRec=RecordFile_Constructor(NULL);
  if(RecordFile_Open(hOutRec,"stdout","wb")==false){
    fprintf(stderr,"Unable to write stdout\n");
    exit(1);
  }


  nInCnt=nErrCnt=0;
  hOut=stdout;
  nSndCdsSclk=nSndMpRti=nSndEvtSclk=nSndEvtFine=nSndMode=0;

  nFiles=0;
  while(sFiles[nFiles]!=NULL){

    if(RecordFile_Open(hInRec,sFiles[nFiles],"rb")==false){
      fprintf(stderr,"Unable to read %s\n",sFiles[nFiles]);
      exit(1);
    }

    while((nRecLen=RecordFile_ReadNextRecord(hInRec,pInRec))>0){
      ++nInCnt;
      pMp=pInRec->data;
      nCdsSclk=pInRec->status.cds_time;
      nMpRti=pInRec->data[3];  nMpRti<<=8;  nMpRti|=pInRec->data[2];
      nEvtFine=(nMpRti<<5)&0x0FF;
      nEvtSclk=GetEventTime(nCdsSclk,nMpRti);
      nStatus=pInRec->status.gnd_status;

      nMode=CasMp_nMode(pInRec);
      strcpy(sReceiver,CasMp_sReceiver(pInRec));
  

     

      /* output bad packets from cds/gnd processing */
      nErrorMask  = CasMpii_MpiiFill | CasMpii_CdsFill; 
      nErrorMask |= CasMpus_ErrorMask | CasMeander_ErrorMask;
      nErrorMask |= CasRice_Errors|CasRice_FoundZeros|CasRice_InvalidOperator;
      if(nStatus&nErrorMask){
        nMode|=CasBadPacket;
        if(nStatus&CasMpii_CdsFill)          strcpy(sTmp,"cds fill");
        else if(nStatus&CasMpii_MpiiFill)    strcpy(sTmp,"mpii fill");
        else if(nStatus&CasMpus_Error)       strcpy(sTmp,"mpus error");
        else if(nStatus&CasMpus_ZeroFilled)  strcpy(sTmp,"mpus fill");
        else if(nStatus&CasMeander_HeaderErrors) 
          strcpy(sTmp,"meander hdr err");
        else if(nStatus&CasMeander_DataTermination)
          strcpy(sTmp,"meander dat term");
        else if(nStatus&CasMeander_ZeroFilled)
          strcpy(sTmp,"meander fill");
        else if(nStatus&(CasMeander_MakeClassError|CasMeander_GetBitsError))
          strcpy(sTmp,"meander error");
        else if(nStatus&CasMeander_BufferOverRun)
          strcpy(sTmp,"meander buf overrun");
        else if(nStatus&CasRice_InvalidOperator)
          strcpy(sTmp,"rice invalid codeid");
        else if(nStatus&CasRice_FoundZeros)
          strcpy(sTmp,"rice found zeros");
        else if(nStatus&CasRice_Errors)
          strcpy(sTmp,"rice decomp errors");
        else
          strcpy(sTmp,"unhandled error");
          /*sTmp[0]='\0'*/

        if(sTmp[0]!='\0')
          fprintf(stdout,"%-8s %08X %04X %s %6.3f %08X %s\n",
                  sReceiver,nCdsSclk,nMpRti,
                  CasSpice_nSclk_to_sScet(nEvtSclk,nEvtFine,NULL),
                  /*CasMp_fDuration(pInRec,0x00),*/
                  0.00,  /* set duration to be arbitrary long */
                  nMode,sTmp);
      }/* bad packets */






      if(CasMfr_bMfrPacket(pMp)==true){/* mfr packet 0x10 */
        if(pInRec->status.packet_length!=226){
          fprintf(stdout,"%-8s %08X %04X %s %6.3f %08X %s %d\n",
                  sReceiver,nCdsSclk,nMpRti,
                  CasSpice_nSclk_to_sScet(nEvtSclk,nEvtFine,NULL),
                  0.0,
                  nMode|CasMfr_BadPacket,"mfr length",
                  pInRec->status.packet_length);
        }
      }/* mfr packet 0x10 */


      else if(CasHfr_bHfrPacket(pMp)==true){/* hfr packet 0x20 */

        /* the next analysis packet switches the antenna relays back, so use
           it's begin time to calculate the sounder black-out period. */
        if(CasHfr_bAnalysis(pMp)==true){

          /* 
             The next analysis packet switches the antenna relays back, so use
             it's begin time to calculate the sounder black-out period. The
             hfr is scheduled on 16 second boundaries, so accept only analysis
             packets which have occured < 16 seconds after the sounder.
          */
          if(nSndEvtSclk != 0x00 ){
          Ulong sec,fine;
            sec  = nEvtSclk - nSndEvtSclk;
            fine = nEvtFine - nSndEvtFine;
            if(fine>0x0FF){ sec -= 1;  fine &= 0x0FF;}
            fDuration = (float)sec + (float)fine/256.0;
            
            if((fDuration - fSndDuration) > 16.0){/* lost an analysis packet */
              fDuration = fSndDuration + 16.0;
              fprintf(stderr,"%-8s %08X %04X %s %6.3f %08X %s\n",
                      sReceiver,nCdsSclk,nMpRti,
                      CasSpice_nSclk_to_sScet(nEvtSclk,nEvtFine,NULL),
                      0.00,nMode,"lost analysis pkt before");
            }

            fprintf(stdout,"%-8s %08X %04X %s %6.3f %08X %s (%.3f)\n",
                    sSndReceiver,nSndCdsSclk,nSndMpRti,
                    CasSpice_nSclk_to_sScet(nSndEvtSclk,nSndEvtFine,NULL),
                    fDuration,nSndMode,"sounder ",fSndDuration);
                 
            fprintf(stdout,"%-8s %08X %04X %s %6.3f %08X %s\n",
                    sReceiver,nCdsSclk,nMpRti,
                    CasSpice_nSclk_to_sScet(nEvtSclk,nEvtFine,NULL),
                    0.0,nMode,"ana relay switch");

            nSndCdsSclk=nSndMpRti=nSndEvtSclk=nSndEvtFine=nSndMode=0;

          }/* if analysis relay switch after sounder packet */
          
        }/* hfr analysis packet */

        if(CasHfr_bSounder(pMp)==true){
          if(nStatus&(CasMpus_FirstSegment|CasMeander_HeaderErrors)){
            fSndDuration=0.0;  /* corrupted header */
          }
          else{
            fSndDuration=CasHfrSounder_fDuration(pInRec,0x00);
            nSndEvtSclk=nEvtSclk;
            nSndEvtFine=nEvtFine;

            strcpy(sSndReceiver,sReceiver);
            nSndCdsSclk=nCdsSclk;
            nSndMpRti=nMpRti;
            nSndMode=nMode;
          }
        }/* hfr sounder packet */
      }/* hfr packets */

      else if(CasLp_bLpPacket(pMp)==true){/* lp packets 0x40 */
        if(CasLp_bRawSweep(pMp)==true){/* raw sweep */
          fprintf(stdout,"%-8s %08X %04X %s %6.3f %08X %s\n",
                  sReceiver,nCdsSclk,nMpRti,
                  CasSpice_nSclk_to_sScet(nEvtSclk,nEvtFine,NULL),
                  CasLp_fDuration(pInRec,CasLp_RawSweep),
                  nMode,"lp raw sweep active");
        }/* raw sweep */
      }/* lp packets */

      else if((pMp[0]&0xF0)==0x70){/* lfdr packets 0x70 */
        if(CasWfdr_PreScale(pMp)==0x00){
          fprintf(stdout,"%-8s %08X %04X %s %6.3f %08X %s\n",
                  sReceiver,nCdsSclk,nMpRti,
                  CasSpice_nSclk_to_sScet(nEvtSclk,nEvtFine,NULL),
                  CasWfdr_fDuration(pInRec,0x00),
                  nMode,"dpf = 0");
        } 
        if((bLfdrFake==true) && (CasWfdr_bFake(pMp)==true)){
          fprintf(stdout,"%-8s %08X %04X %s %6.3f %08X %s\n",
                  sReceiver,nCdsSclk,nMpRti,
                  CasSpice_nSclk_to_sScet(nEvtSclk,nEvtFine,NULL),
                  CasWfdr_fDuration(pInRec,0x00),
                  nMode,"gnd produced");
        } 
      }/* lfdr packets */
    
    }/* elihw records */

    RecordFile_Close(hInRec);
    ++nFiles;
  }/* elihw files */
  fflush(stdout);

  if(bVerbose==true)
    fprintf(hErr,"%s read %d, errors %d\n",sVersion,nInCnt,nErrCnt);
  
 
return 0;
}


void Help(void)
{
fprintf(stderr,"%s\n",sVersion);

fprintf(stderr,
"   Generates a listing of interference and bad packets\n");

fprintf(stderr,"lrsint [OPTIONS] files\n");

fprintf(stderr,
  "-lfdr_fake        output lfdr/mfdr fake status\n"
  "-lrs              output only lrs interference\n"
  "-v                be verbose in the output\n"
  "-ver | -version   send to stderr program version and library version\n"
);

return;
}




/*
      nStatus=pInRec->status.gnd_status;
      if((nStatus&CasMpii_ErrorMask) ||
         (nStatus&CasMpus_ErrorMask) ||
         (nStatus&CasMeander_ErrorMask) || 
         (nStatus&CasRice_ErrorMask)){
        fprintf(hOut,"%s %08X ",CasMp_sStdHdr(pInRec),nStatus);
        if(1){
          fprintf(hOut,"%02X %08X ",
                 pInRec->status.cds_fill,pInRec->status.cmprs_status);
          nSclk=pInRec->status.cds_time;
          nRti=pInRec->data[3];  nRti<<=8;  nRti|=pInRec->data[2];
          nEvtSclk=GetEventTime(nSclk,nRti);
          nRti<<=5;  nRti&=0x0FF;
          fprintf(hOut,"%s",CasSpice_nSclk_to_sScet(nEvtSclk,nRti,NULL));
        }
        fprintf(hOut,"\n");
        ++nErrCnt;
      }
      else if( (pInRec->status.cds_fill || pInRec->status.cmprs_status) &&
               (bAncStatus==true) ){
        fprintf(hOut,"%s %08X ",CasMp_sStdHdr(pInRec),nStatus);
        fprintf(hOut,"%02X %08X ",
                pInRec->status.cds_fill,pInRec->status.cmprs_status);
          nSclk=pInRec->status.cds_time;
          nRti=pInRec->data[3];  nRti<<=8;  nRti|=pInRec->data[2];
          nEvtSclk=GetEventTime(nSclk,nRti);
          nRti<<=5;  nRti&=0x0FF;
          fprintf(hOut,"%s\n",CasSpice_nSclk_to_sScet(nEvtSclk,nRti,NULL));
      }
*/
    
