/*
                      lrscal.c
                     Robert Johnson

September 3, 2003

  lrscal() is a program which reads cassini record file, sort the records
according to instrument and mode, calibrates the data and write a pds archive
file.

  dependancies - 
	CbCasTlm.a 

Tuesday, September 3, 2003  Version 1.1 
  - implement basic skeleton framework
    -> Read Minipackets, use modified CasRecord, CasMpRecord.
    -> Create a linked list of mini-packets
    -> Create a flag list of mini-packets, ie sounder, lp sweep
    -> Sort by logical receiver then by receiver mode 
    *> need to put a mode filter in somewhere

Version 1.3
  December 3, 2003 - add in data quality flags

Version 1.3c
  Janurary 29, 2004 
    seg fault for 2000-333T03:38  hfr data in InstrumentTiming(), during pds
      archiving functions.  Writing das files show packet > 0xFFFF.
    CasHfr.c *_InstrumentTiming()  pEz was null
Version 1.3d
  Feburary 4, 2004
    Blast Hfr 2000-313T06:26 exceeds das packet max length for ABC12 combined.
    -> split packets up into smaller chunks.     

Version 1.3e
  Feburary xxx, 2004
   ??? 

Version 1.4
  Feburary 11, 2004
    ???
  Feburary 12, 2004
    sort_mfdr() splits mfdr real and fake into separate lists.
    no fake data quality stamp for lfdr,mfdr 
    hfr packet checking in lib files 
     
Version 1.5
  Feburary 19, 2004
    implement data flagging for das b0 streams, zero out time,freq,ampl 
 
Version 1.6
  May 27, 2004
    -> propagate zero fill information
 
Version 1.7
  December 3, 2004
    ToDo :: eliminate data marking, check hfr Band C calibration 0.2% diff.
    eroro in A3 of Band 3, was 0.01 should be -0.01.
    leave data marking for real time operation

Version 1.8
  March 8, 2005
    swap 2&3 records sclk fine bytes, was FINE PARTITION, now PARTITION FINE.
    
Version 1.9
  June 20, 2006
    mfr packets has bad record lengths

    Error Message:
      nTmp=224, nItems=112
      nMode=120FF040, pkt=120FF040
      5B25FFEE 2006-168T05:59:12.862
      lrscal.c:1876: failed assertion `0'
      lrs2dat[79]: 20515 Abort
    End Error Message:

Version 2.0 
  December 11, 2006
    lfdr packets have bad record lengths

  Error Message:
    CasWfdr.c:242: failed assertion `pRec->status.packet_length==35'
  End Error Message:

Version 2.1 
  December 13, 2006
    fixed CasWfdr_nMode line 63 assert '0' to break;

Version 2.2
  Builds against libs from SVN: production/stable/GSE/src/castlm
  Is C99 compliant
		
*/


#include <stdio.h> 
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <string.h>
#include <strings.h>

#include <das2/das1.h>

#include <rpwstlm/CasSpice.h>
#include <rpwstlm/RecordFile.h>
#include <rpwstlm/CasMiniPacket.h>
#include <rpwstlm/CasHfr.h>
#include <rpwstlm/CasLp.h>
#include <rpwstlm/CasMfr.h>
#include <rpwstlm/CasWfdr.h>
#include <rpwstlm/CasType.h>

#include "lrscal.h"


float fTimeMargin=0.001;  /* maximum error in time tags */

const char *sVersion="lrscal() Version 2.2"; 


typedef struct mp_lnk_lst_tag{
  struct mp_lnk_lst_tag *next,*prev;
  Ulong nSclkSec,nSclkFine;
  Ulong nSclkSecEnd,nSclkFineEnd;
  Ulong nScetDays,nScetMsec;
  Ulong nMode;          
  CasRecord *pCasRec;
}MpLnkLst;

MpLnkLst *pMpList,*pFlagList,*pMpErr,*pMpDiscard;
MpLnkLst *pLfdr[8],*pMfdr[8],*pMfr[8],*pHfr[128],*pHfrMsc[128];

MpLnkLst* CreateMpLnkLstElement(CasRecord *pRec,bool bData);
#define NextMpLnkLst(p) (p!=NULL?p->next:NULL)
#define PrevMpLnkLst(p) (p!=NULL?p->prev:NULL)

MpLnkLst* HeadMpLnkLst(MpLnkLst *p);
MpLnkLst* TailMpLnkLst(MpLnkLst *p);
MpLnkLst* AppendMpLnkLst(MpLnkLst *list,MpLnkLst *newguy);/*after newguy*/
MpLnkLst* InsertMpLnkLst(MpLnkLst *list,MpLnkLst *newguy);/*before newguy*/
MpLnkLst* DeleteMpLnkLst(MpLnkLst *p);/* next list element */

void DumpMpLnkLst(MpLnkLst *p);

Ulong FlagData(MpLnkLst *pPkt,Ulong nMode,Ulong nLength,
                      float *pTime,float *pFreq,float *pAmpl);
Ulong FlagDataDas(MpLnkLst *pPkt,Ulong nMode,Ulong nLength,
                      float *pTime,float *pFreq,float *pAmpl);

typedef struct mp_event_time_tag{
Ulong nSclkSec,nSclkFine;
Ulong nScetDays,nScetMsec;
int nYear,nDayOfYear,nMonth,nDayOfMonth,nHour,nMinute;
double dSecond;
char sDate[128];
}MpEventTime;

MpEventTime tStart,tStop;

void sort_lfdr(Ulong nMode);
void sort_mfdr(Ulong nMode);
void sort_mfr(Ulong nMode);
void sort_hfr(Ulong nMode);
void sort_hfrmsc(Ulong nMode);
void show_help(void);

Ulong write_pdsrecord(FILE *h,MpLnkLst *pHead,Ulong nWrtMode);
Ulong write_daspacket(FILE *h,MpLnkLst *pHead);
Ulong write_minipacket(FILE *h,MpLnkLst *pHead);

#define OUT_BUF_MAX 64*1024*1024  /* 64M, a wild guess */ 
#define MAX_SAMPLES 64*1024       /* mini-packet limited to 64K bytes */
Uchar OutBuf[OUT_BUF_MAX];        /* output buffer for records */
float arTime[MAX_SAMPLES];        /* hold the calibrated data */
float arFreq[MAX_SAMPLES];
float arAmpl[MAX_SAMPLES];

typedef struct pds_hsk_tag{
char *sFileName;
Ulong nSecBeg,nSecEnd,nFineBeg,nFineEnd;
Ulong nDaysBeg,nDaysEnd,nMsecBeg,nMsecEnd;
Ulong nNumRec,nRecLen,nRecMode;
}PdsHsk;
PdsHsk *hskLfdr[8],*hskMfdr[8],*hskMfr[8],*hskHfr[128],*hskHfrMsc[128];
void print_packet_report(FILE *h);


/* command line options */
bool bVerbose=false,bSilent=false,bReport=false,bVerify=false;
Ulong nLfdrMode,nMfdrMode,nMfrMode,nHfrMode,nHfrMscMode;

unsigned long AntStrToInt(char **pOpt);
Ulong RcvStrToMode(const char *sRcv,char *sArg);


bool bNixHfrSnd=false,bNixLpRswp=false;

int main(int argc,char *argv[])
{
Ulong nIdx;

/* command line options */
bool bWritePds,bWriteDas,bWriteMpr;
bool bDataBase=true;
char *sFiles[1024];
Ulong nFiles;

/* file and record handling */
Ulong nRecLen,nMode,nRecCnt;
CasRecord *pRec=CasRecord_Constructor(NULL);
CasRecord *pRecTmp=CasRecord_Constructor(NULL);
RecordFile *hIn=RecordFile_Constructor(NULL);
MpLnkLst *pll;

Ulong nSec,nFine;
float fDuration;
/* pds file and record handling */






  /* init some stuff */
  CasSpice_Init(NULL);
  CasWfdr_Init(); 
  CasMfr_Init();
  CasHfr_Init();
  
  sFiles[0]=sEquals("stdin");  sFiles[1]=NULL;  nFiles=0;
  pMpList=pFlagList=pMpErr=pMpDiscard=NULL;
  nLfdrMode=nMfdrMode=nMfrMode=nHfrMode=nHfrMscMode=0x00000000;
  bWritePds=bWriteDas=bWriteMpr=false;
  
  tStart.nYear=1980;  tStart.nDayOfYear=1; 
  tStart.nMonth=1;    tStart.nDayOfMonth=1;
  tStart.nHour=0;     tStart.nMinute=0;       tStart.dSecond=0.0;
  sprintf(tStart.sDate,"%04d-%03dT%02d:%02d:%06.3f",tStart.nYear,
          tStart.nDayOfYear,tStart.nHour,tStart.nMinute,tStart.dSecond);
  CasSpice_sScet_to_nSclk(tStart.sDate,&tStart.nSclkSec,&tStart.nSclkFine);
  CasSpice_sScet_to_nScet(tStart.sDate,&tStart.nScetDays,&tStart.nScetMsec);

  tStop.nYear=2020;  tStop.nDayOfYear=1; 
  tStop.nMonth=1;    tStop.nDayOfMonth=1;
  tStop.nHour=0;     tStop.nMinute=0;         tStop.dSecond=0.0;
  sprintf(tStop.sDate,"%04d-%03dT%02d:%02d:%06.3f",tStop.nYear,
          tStop.nDayOfYear,tStop.nHour,tStop.nMinute,tStop.dSecond);
  CasSpice_sScet_to_nSclk(tStop.sDate,&tStop.nSclkSec,&tStop.nSclkFine);
  CasSpice_sScet_to_nScet(tStop.sDate,&tStop.nScetDays,&tStop.nScetMsec);
  
  /* pds record keeping information */
  hskLfdr[0]=hskMfdr[0]=hskMfr[0]=hskHfr[0]=hskHfrMsc[0]=NULL;


  while(--argc){
    ++argv;
    if(!strcmp("-h",*argv)){
      show_help();
      return 0;
    }
    else if( !strcmp("-d",*argv) ){ /* don't use database */
      bDataBase=false;
    }
    else if( !strcmp("-n",*argv) ){ /* don't use database */
      --argc;  ++argv;
      if(!strcmp("hfr_snd",*argv))       bNixHfrSnd=true;
      else if(!strcmp("lp_rswp",*argv))  bNixLpRswp=true;
    }
    else if( !strcmp("-lfr",*argv) || !strcmp("-lfdr",*argv) ){
      --argc;  ++argv;  
      nLfdrMode=RcvStrToMode("lfdr",*argv);
    }
    else if( !strcmp("-mfdr",*argv) ){
      --argc;  ++argv;  
      nMfdrMode=RcvStrToMode("mfdr",*argv);
    }
    else if(!strcmp("-mfr",*argv)){
      --argc;  ++argv;
      nMfrMode=RcvStrToMode("mfr",*argv);
    }
    else if(!strcmp("-hfr",*argv)){
      --argc;  ++argv;  
      nHfrMode=RcvStrToMode("hfr",*argv);
    }
    else if(!strcmp("-msc",*argv)){
      --argc;  ++argv;  
      nHfrMscMode=RcvStrToMode("msc",*argv);
    }
    else if(!strcmp("-o",*argv)){
      --argc;  ++argv;
      if(!strcmp("pds",*argv))       bWritePds=true;
      else if(!strcmp("das",*argv))  bWriteDas=true;
      else if(!strcmp("mpr",*argv))  bWriteMpr=true;
      else{fprintf(stderr,"unknown option: -o %s\n",*argv);}
    }
    else if(!strcmp("-r",*argv)){
      bReport=true;
    }
    else if(!strcmp("-s",*argv)){
      bSilent=true;
    }
    else if(!strcmp("-tStart",*argv)){
      --argc;  ++argv;
      strcpy(tStart.sDate,*argv);
      parsetime(tStart.sDate,&tStart.nYear,&tStart.nMonth,&tStart.nDayOfMonth,
            &tStart.nDayOfYear,&tStart.nHour,&tStart.nMinute,&tStart.dSecond);
      sprintf(tStart.sDate,"%04d-%03dT%02d:%02d:%06.3f",tStart.nYear,
              tStart.nDayOfYear,tStart.nHour,tStart.nMinute,tStart.dSecond);
      CasSpice_sScet_to_nSclk(tStart.sDate,&tStart.nSclkSec,&tStart.nSclkFine);
      CasSpice_sScet_to_nScet(tStart.sDate,&tStart.nScetDays,&tStart.nScetMsec);
    }
    else if(!strcmp("-tStop",*argv)){
      --argc;  ++argv;
      strcpy(tStop.sDate,*argv);
      parsetime(tStop.sDate,&tStop.nYear,&tStop.nMonth,&tStop.nDayOfMonth,
            &tStop.nDayOfYear,&tStop.nHour,&tStop.nMinute,&tStop.dSecond);
      sprintf(tStop.sDate,"%04d-%03dT%02d:%02d:%06.3f",tStop.nYear,
              tStop.nDayOfYear,tStop.nHour,tStop.nMinute,tStop.dSecond);
      CasSpice_sScet_to_nSclk(tStop.sDate,&tStop.nSclkSec,&tStop.nSclkFine);
      CasSpice_sScet_to_nScet(tStop.sDate,&tStop.nScetDays,&tStop.nScetMsec);
    }
    else if(!strcmp("-v",*argv)){
      bVerbose=true;
    }
    else if(!strcmp("-version",*argv)){
      fprintf(stdout,"%s\n",sVersion);
      fprintf(stdout,"  %s\n",CasTlm_Version());
      exit(0);
    }
    else{  
      sFiles[nFiles++]=*argv;  /* assume filenames */
      sFiles[nFiles]=NULL;
    }
  }

  /* set some defaults for absent command line parameters */
  if( (bWritePds==false) && (bWriteDas==false) && (bWriteMpr==false) )
    bWritePds=true;

  if(bVerbose==true){
    fprintf(stderr,"%s\n",sVersion);
    bSilent=false;
    bReport=true;
  }




  if(bVerbose==true){
    fprintf(stderr,"Time Filter\n");
    fprintf(stderr,"  tStart = %s %08X.%02X %04X.%08X\n",tStart.sDate,
        tStart.nSclkSec,tStart.nSclkFine,tStart.nScetDays,tStart.nScetMsec);
    fprintf(stderr,"  tStop  = %s %08X.%02X %04X.%08X\n",tStop.sDate,
        tStop.nSclkSec,tStop.nSclkFine,tStop.nScetDays,tStop.nScetMsec);
    fprintf(stderr,"Mode Filter\n");
    fprintf(stderr,"  Lfdr=%08X Mfdr=%08X Mfr=%08X Hfr=%08X Msc=%08X\n",
            nLfdrMode,nMfdrMode,nMfrMode,nHfrMode,nHfrMscMode);
    fprintf(stderr,"  NixHfrSnd=%s NixLpRswp=%s\n",
            bNixHfrSnd==true?"True":"False",bNixLpRswp==true?"True":"False");
  }


  if(bDataBase==true){
    nFiles=CasMp_ParseDataBase(NULL,tStart.sDate,tStop.sDate,sFiles);
    if(nFiles==0){
      fprintf(stderr,"no files found in data base for %s to %s\n",
             tStart.sDate,tStop.sDate);
      exit(1);
    } 
  }



  nRecCnt=0;
  nFiles=0;
  while(sFiles[nFiles]!=NULL){

    if(bVerbose==true)  fprintf(stderr,"reading %s...",sFiles[nFiles]);
    if(RecordFile_Open(hIn,sFiles[nFiles],"rb")==false){
      fprintf(stderr,"\nunable to read %s.\n",sFiles[nFiles]);
      exit(1);
    }
    if(bVerbose==true)  fprintf(stderr,"done\n");

    while(true){
		 /* Just providing a useful line number for the debugger */
		 nRecLen = RecordFile_ReadNextRecord(hIn,pRec);
		 
		 if(nRecLen < 1) break;
				 
      /* decode and list the packets */
      nMode=CasMp_nMode(pRec);

      /* check for bad packets and put on a list */
      if((nMode&CasMp_Mask)==CasMp_Hfr){
        if((nMode&CasRcv_Mask)==CasHfr_Analysis){
          if(CasHfrAnalysis_sValidPacket(pRec)!=NULL){
            if(bSilent==false)
              fprintf(stderr,"%s\n",CasHfrAnalysis_sValidPacket(pRec));
            pll=CreateMpLnkLstElement(pRec,true);
            pMpErr=AppendMpLnkLst(pMpErr,pll);
            continue;
          }
        }
        else if((nMode&CasRcv_Mask)==CasHfr_Millisecond){
          if(CasHfrMillisecond_sValidPacket(pRec)!=NULL){
            if(bSilent==false)
              fprintf(stderr,"%s\n",CasHfrMillisecond_sValidPacket(pRec));
            pll=CreateMpLnkLstElement(pRec,true);
            pMpErr=AppendMpLnkLst(pMpErr,pll);
            continue;
          }
        }
        else if((nMode&CasRcv_Mask)==CasHfr_Sounder){
          if(CasHfrSounder_sValidPacket(pRec)!=NULL){
            if(bSilent==false)
              fprintf(stderr,"%s\n",CasHfrSounder_sValidPacket(pRec));
            pll=CreateMpLnkLstElement(pRec,true);
            pMpErr=AppendMpLnkLst(pMpErr,pll);
            continue;
          }
        }
        else if((nMode&CasRcv_Mask)==CasHfr_Calibration){
          if((pRec->data[11]&0xFE)!=0x00){
            pll=CreateMpLnkLstElement(pRec,true);
            pMpErr=AppendMpLnkLst(pMpErr,pll);    
            if(bSilent==false){
              fprintf(stderr,"hfr calibration skipping packet\n");
              fprintf(stderr,"  %s\n",CasMp_DumpHeader(pRec,NULL));
            }
            continue;
          }
        }
        else{
          fprintf(stderr,"unknown hfr packet\n");
          fprintf(stderr,"  %s\n",CasMp_DumpHeader(pRec,NULL));
          assert(0);
          continue;
        }
      }

      if((nMode&CasMp_Mask)==CasMp_Wfdr){
        if( (CasWfdr_Channel(pRec->data)>0x04) ||
            (CasWfdr_nSize(pRec->data)!=512) ){
          if(bSilent==false)
            fprintf(stderr,"lfdr packet suspicious header, skipping\n  %s\n",
                    CasMp_DumpHeader(pRec,NULL));
          pll=CreateMpLnkLstElement(pRec,true);
          pMpErr=AppendMpLnkLst(pMpErr,pll);    /* after newguy */
          continue;
        }
        if(pRec->status.packet_length!=0x23){ /*hdr=6,Data=32=>38. len-1=35 */
          if(bSilent==false){
            fprintf(stderr,"Bad LFDR Mini-Packet Length\n");
            fprintf(stderr,"  %s\n",CasMp_DumpHeader(pRec,NULL));
          }/* fi silent */
          pll=CreateMpLnkLstElement(pRec,true);
          pMpErr=AppendMpLnkLst(pMpErr,pll);    /* after newguy */
          continue;
        }
      }

      if((nMode&CasMp_Mask)==CasMp_Lp){
        if(CasLp_bCompressed(pRec->data)==true){
          if(bSilent==false){
            fprintf(stderr,"don't know how to decompress the lp:"
              "cdstime=%08X, len=%08d\n",pRec->status.cds_time,
              pRec->status.packet_length);
            fprintf(stderr,"  %s\n",CasMp_DumpHeader(pRec,NULL));
          }/* fi silent */
          pll=CreateMpLnkLstElement(pRec,true);
          pMpErr=AppendMpLnkLst(pMpErr,pll);    /* after newguy */
          continue;
        } 
      }

      if((nMode&CasMp_Mask)==CasMp_Mfr){
        if(CasMfr_bCompressed(pRec->data)==true){
          if(bSilent==false){
            fprintf(stderr,"don't know how to decompress the mfr\n");
            fprintf(stderr,"  %s\n",CasMp_DumpHeader(pRec,NULL));
          }/* fi silent */
          pll=CreateMpLnkLstElement(pRec,true);
          pMpErr=AppendMpLnkLst(pMpErr,pll);    /* after newguy */
          continue;
        }
        if(CasMfr_nLength(pRec->data)!=0x0E2){ /* mp length = 0x0EC = 226 */
          if(bSilent==false){
            fprintf(stderr,"Bad MFR Mini-Packet Length\n");
            fprintf(stderr,"  %s\n",CasMp_DumpHeader(pRec,NULL));
          }/* fi silent */
          pll=CreateMpLnkLstElement(pRec,true);
          pMpErr=AppendMpLnkLst(pMpErr,pll);    /* after newguy */
          continue;
        }
      }

      if((nMode&CasMp_Mask)==CasMp_Hfr){
        if(CasHfr_bMeanderCompressed(pRec->data)==true){
          CasHfr_Meander(pRec,pRecTmp); 
          memcpy(pRec,pRecTmp,sizeof(CasRecord));
        }
      }


      /* only interested in lfdr, mfdr, mfr, and hfr packets */
      /* might filter by time and mode here ? */
      switch(nMode&CasRcv_Mask){ 
        case CasLfdr_Normal:
        case CasMfdr_Normal:
        case CasMfr_Normal:
        case CasMfr_FastToggle:
        case CasHfr_Analysis:
        case CasHfr_Millisecond:
          pll=CreateMpLnkLstElement(pRec,true);
          pMpList=AppendMpLnkLst(pMpList,pll);    /* after newguy */
          ++nRecCnt;
          break; 
        case CasLp_RawSweep: 
        case CasHfr_Sounder:  /* flag list packets */
          pll=CreateMpLnkLstElement(pRec,false);
          pFlagList=AppendMpLnkLst(pFlagList,pll);  /* after newguy */
          break;
        default:
          continue;
          break; 
      }


      switch(pll->nMode&CasRcv_Mask){
        case CasLfdr_Normal:
        case CasMfdr_Normal:
          fDuration=CasWfdr_fDuration(pll->pCasRec,pll->nMode);  break;
        case CasMfr_Normal:
        case CasMfr_FastToggle:
          fDuration=32.0;  break;
        case CasHfr_Analysis:
          fDuration=CasHfrAnalysis_fDuration(pll->pCasRec,pll->nMode);  break;
        case CasHfr_Sounder:
          fDuration=CasHfrSounder_fDuration(pll->pCasRec,pll->nMode);  break;
        case CasHfr_Millisecond:
          fDuration=CasHfrMillisecond_fDuration(pll->pCasRec,pll->nMode);break;
        case CasLp_RawSweep:
          fDuration=CasLp_fDuration(pll->pCasRec,pll->nMode);  break;
        default:
          fDuration=0.0;  break;
      }
      nSec=(Ulong)fDuration;
      nFine=(fDuration-(Ulong)fDuration)*256.0;
      pll->nSclkSecEnd=pll->nSclkSec+nSec;
      pll->nSclkFineEnd=pll->nSclkFine+nFine;
      if(pll->nSclkFineEnd>255){  /* normalize time */
        pll->nSclkSecEnd+=(pll->nSclkFineEnd/256);
        pll->nSclkFineEnd=(pll->nSclkFineEnd%256);
      }

    }/* mini-packets */
    
    RecordFile_Close(hIn);  /* status return ? */
    ++nFiles;
  }/* files */
  pMpList=HeadMpLnkLst(pMpList);
  pFlagList=HeadMpLnkLst(pFlagList);
  if(bVerbose==true || bReport==true)
    fprintf(stderr,"nRecCnt=%d, nFiles=%d\n",nRecCnt,nFiles);


  if(bVerbose==true)  fprintf(stderr,"Flag List\n");
  pll=pFlagList;  nIdx=0;
  while(pll!=NULL){
    if(bVerbose==true)  DumpMpLnkLst(pll);
    pll=NextMpLnkLst(pll);
    ++nIdx;
  }
  if(bVerbose==true || bReport==true)  
    fprintf(stderr,"Total mini-packets flagged %d\n",nIdx);



  if(bWriteDas==true){
    if(bSilent==false)  fprintf(stderr,"writing das packets\n");
    write_daspacket(stdout,pMpList);
    exit(0);
  }
  

/*
  pll=pMpList;
  while(pll!=NULL){
    DumpMpLnkLst(pll);
    pll=NextMpLnkLst(pll);
  }
*/


  /* two layered sort, first by logical receiver type, then by mode */
  /* watch the list limits: lfdr=mfdr=mfr=8; hfr=hfrmsc=128;  */ 
  /* MpLnkLst *pLfdrList,*pMfdrList,*pMfrList,*pHfrList,*pHfrMsc; */

  pMpDiscard=NULL;
  pll=pMpList=HeadMpLnkLst(pMpList);
  while(pll!=NULL){  
    pMpList=DeleteMpLnkLst(pll);

  
    /* time filter */
    if(pll->nSclkSec<tStart.nSclkSec)
      pMpDiscard=AppendMpLnkLst(pMpDiscard,pll);
    else if(pll->nSclkSec>tStop.nSclkSec)
      pMpDiscard=AppendMpLnkLst(pMpDiscard,pll);
    else if( (pll->nSclkSec==tStart.nSclkSec) &&
             (pll->nSclkFine<tStart.nSclkFine) )
      pMpDiscard=AppendMpLnkLst(pMpDiscard,pll);
    else if( (pll->nSclkSec==tStop.nSclkSec) &&
             (pll->nSclkFine>tStop.nSclkFine) )
      pMpDiscard=AppendMpLnkLst(pMpDiscard,pll);
    else{
      switch((pll->nMode)&CasRcv_Mask){ 
        case CasLfdr_Normal:
          pLfdr[0]=AppendMpLnkLst(pLfdr[0],pll);
          break;
        case CasMfdr_Normal:
          pMfdr[0]=AppendMpLnkLst(pMfdr[0],pll);
          break;
        case CasMfr_Normal:
        case CasMfr_FastToggle:
          pMfr[0]=AppendMpLnkLst(pMfr[0],pll);
          break;
        case CasHfr_Analysis:
          if(pll->nMode&CasHfr_BadPacket)
            pMpErr=AppendMpLnkLst(pMpErr,pll);
          else
            pHfr[0]=AppendMpLnkLst(pHfr[0],pll);
          break;
        case CasHfr_Millisecond:
          if(pll->nMode&CasHfr_BadPacket)
            pMpErr=AppendMpLnkLst(pMpErr,pll);
          else
            pHfrMsc[0]=AppendMpLnkLst(pHfrMsc[0],pll);
          break;
        default:
          pMpErr=AppendMpLnkLst(pMpErr,pll);
          break; 
      }
    }/* else */
      pll=pMpList;
  }/* primary logical receiver sort */

  if(bVerbose==true)  fprintf(stderr,"Discard List\n");
  pll=pMpDiscard=HeadMpLnkLst(pMpDiscard);  nIdx=0;
  while(pll!=NULL){
    if(bVerbose==true)  DumpMpLnkLst(pll);
    pll=NextMpLnkLst(pll);
    ++nIdx;
  }
  if(bVerbose==true || bReport==true)  
    fprintf(stderr,"Total mini-packets discarded %d (time filter)\n",nIdx);
 
  if(bVerbose==true)  fprintf(stderr,"Error List\n");
  pll=pMpErr=HeadMpLnkLst(pMpErr);  nIdx=0;
  while(pll!=NULL){
    if(bVerbose==true)  DumpMpLnkLst(pll);
    pll=NextMpLnkLst(pll);
    ++nIdx;
  }
  if(bVerbose==true || bReport==true)  
    fprintf(stderr,"Total mini-packets errors %d (cmprsed/hfr_valid/mfr_length)\n",nIdx);


  if(bWritePds==true){
    nLfdrMode=CasLfdr_Normal|CasWfdr_Anything|CasAntAll;
    nMfdrMode=CasMfdr_Normal|CasWfdr_Anything|CasAntAll;
    nMfrMode=CasMfr_AnyMode|CasMfr_Band123|CasAntAll;
    nHfrMode=CasHfr_Analysis|CasHfr_BandABC12|CasAntAll;
    nHfrMscMode=CasHfr_Millisecond|CasHfr_BandHF12|CasAntAll;
  }
 
  /* sort by receiver mode */
  if(nLfdrMode)  /* lfdr modes will only differ by log/lin and size */
    sort_lfdr(nLfdrMode);
  if(nMfdrMode)  /* mfdr modes will only differ by log/lin and size */
    sort_mfdr(nMfdrMode);
  if(nMfrMode)   /* mfr modes will only differ by normal/fast toggle mode */
    sort_mfr(nMfrMode);
  if(nHfrMode)   /* hfr modes differ by comparison of mp header */
    sort_hfr(nHfrMode);
  if(nHfrMscMode)/* hfr msc modes differ by comparison of mp header */
    sort_hfrmsc(nHfrMscMode);


  if(bWritePds==true){

    nIdx=0;
    nMode=CasLfdr_Normal|CasWfdr_Anything|CasAntAll;
/*    nMode=nLfdrMode; */
    while(pLfdr[nIdx]!=NULL){
      write_pdsrecord(NULL,pLfdr[nIdx],nMode);
      ++nIdx;
    }
fprintf(stderr,"\n");

    nIdx=0;
    nMode=CasMfdr_Normal|CasWfdr_Anything|CasAntAll;
/*    nMode=nMfdrMode; */
    while(pMfdr[nIdx]!=NULL){
      write_pdsrecord(NULL,pMfdr[nIdx],nMode);
      ++nIdx;
    }

    nIdx=0;
    nMode=CasMfr_Normal|CasMfr_Band123|CasAntAll;
    nMode=CasMfr_FastToggle|CasMfr_Band123|CasAntAll;
    nMode=CasMfr_AnyMode|CasMfr_Band123|CasAntAll;
/*    nMode=nMfrMode; */
    while(pMfr[nIdx]!=NULL){
      write_pdsrecord(NULL,pMfr[nIdx],nMode);
      ++nIdx;
    }

    nIdx=0;
    while(pHfr[nIdx]!=NULL){

      /* Bands ABC */
      nMode=CasHfr_Analysis|CasHfr_BandABC|CasAntAll;
/*      nMode&=nHfrMode; */
      write_pdsrecord(NULL,pHfr[nIdx],nMode);

      /* Bands HF1 */
      nMode=CasHfr_Analysis|CasHfr_BandHF1|CasAntAll;
/*      nMode&=nHfrMode; */
      write_pdsrecord(NULL,pHfr[nIdx],nMode);

      /* Bands HF2 */
      nMode=CasHfr_Analysis|CasHfr_BandHF2|CasAntAll;
/*      nMode&=nHfrMode; */
      write_pdsrecord(NULL,pHfr[nIdx],nMode);

      ++nIdx;
    }/* writing hfr pds packets */


    nIdx=0;
    nMode=CasHfr_Millisecond|CasHfr_BandHF12|CasAntAll;
/*    nMode&=nHfrMscMode; */
    while(pHfrMsc[nIdx]!=NULL)
      write_pdsrecord(NULL,pHfrMsc[nIdx++],nMode);



    if(bReport==true)
      print_packet_report(stderr);

  }
  else if(bWriteDas==true){
    write_daspacket(stdout,pMpList);
  }
  else if(bWriteMpr==true){
    write_minipacket(stdout,pMpList);
  }

/* 
Output formats
  pds record
  das bo
  mini-packet
*/


return 0;  /* same as exit(0); */
}





/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=- */

MpLnkLst* CreateMpLnkLstElement(CasRecord *pRec,bool bData)
{
Ulong nLen;
MpLnkLst *p;

  if(pRec==NULL)  assert(0);

  assert( (p=calloc(1,sizeof(MpLnkLst)))!=NULL );             /* container */

  if(bData==true)  /* copy all the data */
    nLen=pRec->status.packet_length+3;  
  else             /* just the mp header */
    nLen=32;       /* keep natural alignment nLen=CasMp_nHeaderLength(pRec); */
 
  /* Create a variable length CasRecord */
  nLen+=256+12;  /* 256+12 , mabey should keep things naturally aligned */
  assert( (p->pCasRec=calloc(nLen,sizeof(Uchar)))!=NULL );
  memcpy(p->pCasRec,pRec,nLen);

  p->next=p->prev=NULL;

  p->nMode=CasMp_nMode(pRec);
  p->nSclkSec=p->pCasRec->status.cds_time;
  p->nSclkFine= p->pCasRec->data[3];  p->nSclkFine<<=8; 
  p->nSclkFine|=p->pCasRec->data[2]; /* fine is really rti */
  p->nSclkSec=GetEventTime(p->nSclkSec,p->nSclkFine);
  p->nSclkFine&=0x07;  p->nSclkFine<<=5; /* convert rti to fine */
  CasSpice_nSclk_to_nScet(p->nSclkSec,p->nSclkFine,
                        &(p->nScetDays),&(p->nScetMsec));

return p;
}



MpLnkLst* HeadMpLnkLst(MpLnkLst *p)
{
  if(p==NULL)
    return NULL;  /* no list */

  while(p->prev!=NULL)
    p=p->prev;

return p;
}

MpLnkLst* TailMpLnkLst(MpLnkLst *p)
{
  assert(p!=NULL);

  while(p->next!=NULL)
    p=p->next;

return p;
}

MpLnkLst* AppendMpLnkLst(MpLnkLst *list,MpLnkLst *newguy)
{
  if(list==NULL)  return newguy;  /* no list to append to */
  assert(newguy!=NULL);

  /* check to see if this is the end of the list */
  if(list->next!=NULL){
    list->next->prev=newguy;
    newguy->next=list->next; 
  }
  list->next=newguy;
  newguy->prev=list;
   
return newguy;
}

MpLnkLst* InsertMpLnkLst(MpLnkLst *list,MpLnkLst *newguy)
{
  if(list==NULL)  return newguy;  /* no list to insert to */
  assert(newguy!=NULL);

  /* check to see if this is the head of the list */
  if(list->prev!=NULL){
    list->prev->next=newguy;
    newguy->prev=list->prev; 
  }
  list->prev=newguy;
  newguy->next=list;

return newguy;
}

MpLnkLst* DeleteMpLnkLst(MpLnkLst *p)  /* next list element */
{
MpLnkLst *pList;

  assert(p!=NULL);
  if( (p->prev==NULL) && (p->next==NULL) ){       /* single element */
    pList=NULL;
  }
  else if(p->prev==NULL){  /* at head of list */
    p->next->prev=NULL;    /* new head */
    pList=p->next; 
    p->prev=p->next=NULL;  /* orphaned from list */
  }
  else if(p->next==NULL){  /* at tail of list */
    p->prev->next=NULL;    /* new tail */
    pList=NULL;
    p->prev=p->next=NULL;  /* orphaned from list */
  }
  else{                    /* somewhere in the middle */
    p->prev->next=p->next->prev;
    pList=p->next; 
    p->prev=p->next=NULL;  /* orphaned from list */
  }
   
return pList;
}

void DumpMpLnkLst(MpLnkLst *p)
{
float fDuration;


  switch(p->nMode&CasRcv_Mask){
    case CasLfdr_Normal:
    case CasMfdr_Normal:
      fDuration=CasWfdr_fDuration(p->pCasRec,p->nMode);  break;
    case CasMfr_Normal:
    case CasMfr_FastToggle:
      fDuration=32.0;  break;
    case CasHfr_Analysis:
      fDuration=CasHfrAnalysis_fDuration(p->pCasRec,p->nMode);  break;
    case CasHfr_Sounder:
      fDuration=CasHfrSounder_fDuration(p->pCasRec,p->nMode);  break;
    case CasLp_RawSweep:
      fDuration=CasLp_fDuration(p->pCasRec,p->nMode);  break;
    default:
      fDuration=0.0;  break;
  }


  fprintf(stderr,"%08X %08X.%02X to %08X.%02X %s (%.3f)\n",p->nMode,
          p->nSclkSec,p->nSclkFine,p->nSclkSecEnd,p->nSclkFineEnd,
          CasSpice_nSclk_to_sScet(p->nSclkSec,p->nSclkFine,NULL),fDuration );


return;
}



/* assume at this point, there is a flag list of the form
  SecBeg,FineBeg,SecEnd,FineEnd, nMode */
Ulong FlagData(MpLnkLst *pPkt,Ulong nMode,Ulong nLength,
                      float *pTime,float *pFreq,float *pAmpl)
{
bool bHit=false;
int i;
Ulong nFlag=0;
Ulong nSclkBeg,nFineBeg,nSclkEnd,nFineEnd;
float fBeg,fEnd,fSampleWidth;
MpLnkLst *p;

  p=pFlagList;
  while(p!=NULL){

    nSclkBeg=p->nSclkSec;     nFineBeg=p->nSclkFine;
    nSclkEnd=p->nSclkSecEnd;  nFineEnd=p->nSclkFineEnd;
    switch(p->nMode&CasRcv_Mask){
      case CasLp_RawSweep:
        /* glitch 7-8 RTIs before sweep and 1-2 RTIs after */
        nSclkBeg-=1;  nFineEnd+=64;
        if(nFineEnd>255){nFineEnd%=256;  nSclkEnd+=1;}
        break;
      case CasHfr_Sounder:  
        /* sounder seems too short, try 2 RTIs delay */
        nFineEnd+=96;
        if(nFineEnd>255){nFineEnd%=256;  nSclkEnd+=1;}
      default:
        break;
    }
    if((pPkt->nSclkSec<nSclkEnd) && 
       (pPkt->nSclkSecEnd>nSclkBeg))            bHit=true;
    else if((pPkt->nSclkSec==nSclkEnd) && 
            (pPkt->nSclkFine<=nFineEnd))        bHit=true;
    else if((pPkt->nSclkSecEnd==nSclkBeg) && 
            (pPkt->nSclkFineEnd>=nFineBeg))     bHit=true;
    else                                        bHit=false; 


    if(bHit==true){
      switch(p->nMode&CasRcv_Mask){/* 76543210 */
        case CasHfr_Sounder:  nFlag|=0x40000000;   break;
        case CasLp_RawSweep:  nFlag|=0x20000000;   break;
        default:              assert(0);  break;
      }

      /* normalize the interference interval to the pTime[] */
      fBeg=(int)(nSclkBeg-pPkt->nSclkSec);
      fBeg+=((int)(nFineBeg-pPkt->nSclkFine))/256.0;
      fEnd=(int)(nSclkEnd-pPkt->nSclkSec);
      fEnd+=((int)(nFineEnd-pPkt->nSclkFine))/256.0;
/*
fprintf(stderr,"%08X:  %08X.%02X to %08X.%02X (%.4f to %.4f)\n",p->nMode,
        p->nSclkSec,p->nSclkFine,p->nSclkSecEnd,p->nSclkFineEnd,fBeg,fEnd); 
*/
      if( ((pPkt->nMode&CasRcv_Mask)==CasLfdr_Normal) ||
          ((pPkt->nMode&CasRcv_Mask)==CasMfdr_Normal) ){
        for(i=0;i<nLength;i++){
          if(pAmpl[i]>0)  pAmpl[i]*=(-1);  /* mark the whole packet */
        }
      }/* fi lfdr and mfdr */
      else{

        if((nMode&CasRcv_Mask)==CasHfr_Analysis)
          fSampleWidth=CasHfrAnalysis_fSampleWidth(pPkt->pCasRec,nMode);
        else
          fSampleWidth=1.0;
        
        for(i=0;i<nLength;i++){
          if((pTime[i]<=fEnd) && ((pTime[i]+fSampleWidth)>=fBeg)){
            if(pAmpl[i]>0)  pAmpl[i]*=(-1);
          }/* fi mark with negative */
        }
      }/* esle all others */
      
    }/* fi tag numbers */    

    p=NextMpLnkLst(p);
  }

  /* if the packet is lfdr or mfdr it could be fake data */
  if( ((pPkt->nMode&CasRcv_Mask)==CasLfdr_Normal) ||
      ((pPkt->nMode&CasRcv_Mask)==CasMfdr_Normal) ){
    if(pPkt->nMode&CasWfdr_Fake)
      nFlag|=0x10000000;        /* ground produced data */
    
  }/* fi lfdr & mfdr */ 


  /* check the zero fill status */
  /* pPkt->pCasRec->status.gnd_status;  */
  /* pPkt->pCasRec->status.cmpr_status; */
  if(pPkt->pCasRec->status.gnd_status&CasMpii_Processed){
    nFlag|=0x00000000;  /* look for possible fill data later */
  }
  if(pPkt->pCasRec->status.gnd_status&CasMpus_Processed){
    if(pPkt->pCasRec->status.gnd_status&CasMpus_Error)
      nFlag|=0x80000000;  /* Set Error Bit */
    if(pPkt->pCasRec->status.gnd_status&CasMpus_ZeroFilled)
      nFlag|=0x08000000;  /* Set Zero Fill Bit */
  }
  if(pPkt->pCasRec->status.gnd_status&CasMeander_Processed){
    if(pPkt->pCasRec->status.gnd_status &
       (CasMeander_GetBitsError|CasMeander_MakeClassError|
        CasMeander_BufferOverRun|CasMeander_HeaderErrors))
      nFlag|=0x80000000;  /* Set Error Bit */
    if(pPkt->pCasRec->status.gnd_status&CasMeander_ZeroFilled)
      nFlag|=0x08000000;  /* Set Zero Fill Bit */
  } 
  if(nFlag&0x88000000){  /* flag the whole packet */
    for(i=0;i<nLength;i++){
      if(pAmpl[i]>0)  pAmpl[i]*=(-1);  /* mark the whole packet */
    }
  }
   

return nFlag;
}



/* assume at this point, there is a flag list of the form
  SecBeg,FineBeg,SecEnd,FineEnd, nMode */
Ulong FlagDataDas(MpLnkLst *pPkt,Ulong nMode,Ulong nLength,
                      float *pTime,float *pFreq,float *pAmpl)
{
bool bHit=false;
int i;
Ulong nFlag=0;
Ulong nSclkBeg,nFineBeg,nSclkEnd,nFineEnd;
float fBeg,fEnd,fSampleWidth;
MpLnkLst *p;

  p=pFlagList;
  while(p!=NULL){

    nSclkBeg=p->nSclkSec;     nFineBeg=p->nSclkFine;
    nSclkEnd=p->nSclkSecEnd;  nFineEnd=p->nSclkFineEnd;
    switch(p->nMode&CasRcv_Mask){
      case CasLp_RawSweep:
        /* glitch 7-8 RTIs before sweep and 1-2 RTIs after */
        nSclkBeg-=1;  nFineEnd+=64;
        if(nFineEnd>255){nFineEnd%=256;  nSclkEnd+=1;}
        break;
      case CasHfr_Sounder:  
        /* sounder seems too short, try 2 RTIs delay */
        nFineEnd+=96;
        if(nFineEnd>255){nFineEnd%=256;  nSclkEnd+=1;}
      default:
        break;
    }
    if((pPkt->nSclkSec<nSclkEnd) && 
       (pPkt->nSclkSecEnd>nSclkBeg))            bHit=true;
    else if((pPkt->nSclkSec==nSclkEnd) && 
            (pPkt->nSclkFine<=nFineEnd))        bHit=true;
    else if((pPkt->nSclkSecEnd==nSclkBeg) && 
            (pPkt->nSclkFineEnd>=nFineBeg))     bHit=true;
    else                                        bHit=false; 

    if((bNixHfrSnd==false) && (p->nMode&CasRcv_Mask)==CasHfr_Sounder)
      bHit=false;
    if((bNixLpRswp==false) && (p->nMode&CasRcv_Mask)==CasLp_RawSweep)
      bHit=false;

    if(bHit==true){
      switch(p->nMode&CasRcv_Mask){/* 76543210 */
        case CasHfr_Sounder:  nFlag|=0x40000000;   break;
        case CasLp_RawSweep:  nFlag|=0x20000000;   break;
        default:              assert(0);  break;
      }

      /* normalize the interference interval to the pTime[] */
      fBeg=(int)(nSclkBeg-pPkt->nSclkSec);
      fBeg+=((int)(nFineBeg-pPkt->nSclkFine))/256.0;
      fEnd=(int)(nSclkEnd-pPkt->nSclkSec);
      fEnd+=((int)(nFineEnd-pPkt->nSclkFine))/256.0;

      if( ((pPkt->nMode&CasRcv_Mask)==CasLfdr_Normal) ||
          ((pPkt->nMode&CasRcv_Mask)==CasMfdr_Normal) ){
        for(i=0;i<nLength;i++){
          if(pAmpl[i]>0){  /* mark the whole packet */
            pFreq[i]=0;
            pAmpl[i]=0;
          }
        }
      }/* fi lfdr and mfdr */
      else{

        if((nMode&CasRcv_Mask)==CasHfr_Analysis)
          fSampleWidth=CasHfrAnalysis_fSampleWidth(pPkt->pCasRec,nMode);
        else
          fSampleWidth=1.0;
        
        for(i=0;i<nLength;i++){
          if((pTime[i]<=fEnd) && ((pTime[i]+fSampleWidth)>=fBeg)){
            if(pAmpl[i]>0){
              pFreq[i]=0;
              pAmpl[i]=0;
            }
          }/* fi mark with negative */
        }
      }/* esle all others */
      
    }/* fi tag numbers */    

    p=NextMpLnkLst(p);
  }

  /* if the packet is lfdr or mfdr it could be fake data */
  if( ((pPkt->nMode&CasRcv_Mask)==CasLfdr_Normal) ||
      ((pPkt->nMode&CasRcv_Mask)==CasMfdr_Normal) ){
    if(pPkt->nMode&CasWfdr_Fake)
      nFlag|=0x10000000;        /* ground produced data */
    
  }/* fi lfdr & mfdr */ 


return nFlag;
}

/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=- */
/* -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=- */



void sort_lfdr(Ulong nMode)
{
int i,nIdx,nPktCnt;
bool bLog,bLogList;
Ulong nSize,nSizeList;
MpLnkLst *pll,*pList;

  /* lfdr modes will only differ by log/lin and size */
  nPktCnt=0;
  pll=pList=HeadMpLnkLst(pLfdr[0]);  pLfdr[0]=NULL;
  while(pll!=NULL){  
    bLog=CasWfdr_bLog(pll->pCasRec->data);
    nSize=CasWfdr_nSize(pll->pCasRec->data);
    pList=DeleteMpLnkLst(pll);
    nIdx=0;
    while(pLfdr[nIdx]!=NULL){ /* check to see if mode is already on list */
      bLogList=CasWfdr_bLog(pll->pCasRec->data);
      nSizeList=CasWfdr_nSize(pll->pCasRec->data);
      if((nSizeList==nSize) && (bLogList==bLog)){
        pLfdr[nIdx]=AppendMpLnkLst(pLfdr[nIdx],pll);
        /* sanity check */
        if((pLfdr[nIdx]->nMode&0x003FF000)!=(pll->nMode&0x003FF000)){
          fprintf(stderr,"lfdr list=%08X data=%08X\n",pLfdr[nIdx]->nMode,
                  pll->nMode);
          assert(0);
        }
        break;
      }
/*
      if( (pLfdr[nIdx]->nMode&~CasAntMask)==(pll->nMode&~CasAntMask) ){
        pLfdr[nIdx]=AppendMpLnkLst(pLfdr[nIdx],pll);
        break;
      }
*/
      ++nIdx;
    }
    if(pLfdr[nIdx]==NULL){  /* no match => new mode */
      pLfdr[nIdx]=AppendMpLnkLst(pLfdr[nIdx],pll);
      pLfdr[nIdx+1]=NULL;   /* make sure list is always null terminated */
      assert((nIdx+1)<8);
    } 
    pll=pList;  ++nPktCnt;
  }
  

  if(bVerbose==true){
    nIdx=0;
    while(pLfdr[nIdx]!=NULL){
      pLfdr[nIdx]=HeadMpLnkLst(pLfdr[nIdx]);
      ++nIdx;
    }
    fprintf(stderr,"Lfdr %d modes, %d packets - sort_lfdr()\n",nIdx,nPktCnt);
    for(nIdx=0;pLfdr[nIdx]!=NULL;nIdx++){   
      pll=pLfdr[nIdx];  i=0;  /* count packets */
      while(pll!=NULL){
        pll=NextMpLnkLst(pll);
        ++i;
      }
      fprintf(stderr,"  %0X with %d packets  ",pLfdr[nIdx]->nMode,i);
      fprintf(stderr,"%s 0x%08X\n",
        CasSpice_nSclk_to_sScet(pLfdr[nIdx]->nSclkSec,pLfdr[nIdx]->nSclkFine,NULL),
        pLfdr[nIdx]->nSclkSec);

      pll=pLfdr[nIdx];  i=0;
      fprintf(stderr,"    ");
      for(i=0;i<6;i++)   
        fprintf(stderr,"%02X ",pLfdr[nIdx]->pCasRec->data[i]);
      fprintf(stderr,"\n");

    }
  }/* fi bverbose==true */

return;
}



void sort_mfdr(Ulong nMode)
{
int i,nIdx,nPktCnt;
bool bLog,bLogList;
Ulong nSize,nSizeList;
MpLnkLst *pll,*pList;

  nPktCnt=0;
  pll=pList=HeadMpLnkLst(pMfdr[0]);  pMfdr[0]=NULL;
  while(pll!=NULL){  
    bLog=CasWfdr_bLog(pll->pCasRec->data);
    nSize=CasWfdr_nSize(pll->pCasRec->data);
    pList=DeleteMpLnkLst(pll);
    nIdx=0;
    while(pMfdr[nIdx]!=NULL){ /* check to see if mode is already on list */
      bLogList=CasWfdr_bLog(pll->pCasRec->data);
      nSizeList=CasWfdr_nSize(pll->pCasRec->data);
      if((nSizeList==nSize) && (bLogList==bLog)){
        pMfdr[nIdx]=AppendMpLnkLst(pMfdr[nIdx],pll);
        /* sanity check */
        if((pMfdr[nIdx]->nMode&0x003FF000)!=(pll->nMode&0x003FF000)){
          fprintf(stderr,"mfdr list=%08X data=%08X\n",pMfdr[nIdx]->nMode,
                  pll->nMode);
          assert(0);
        }
        break;
      }
/*
      if( (pMfdr[nIdx]->nMode&0x003FF000)==(pll->nMode&0x003FF00) ){
        pMfdr[nIdx]=AppendMpLnkLst(pMfdr[nIdx],pll);
        break; 
      }
*/
      ++nIdx;
    }
    if(pMfdr[nIdx]==NULL){  /* no match => new mode */
      pMfdr[nIdx]=AppendMpLnkLst(pMfdr[nIdx],pll);
      pMfdr[nIdx+1]=NULL;   /* make sure list is always null terminated */
      assert((nIdx+1)<8);
    } 
    pll=pList;  ++nPktCnt;
  }
  
  nIdx=0;
  while(pMfdr[nIdx]!=NULL){
    pMfdr[nIdx]=HeadMpLnkLst(pMfdr[nIdx]);
    ++nIdx;
  }


  if(bVerbose==true){
    nIdx=0;
    while(pMfdr[nIdx]!=NULL){
      pMfdr[nIdx]=HeadMpLnkLst(pMfdr[nIdx]);
      ++nIdx;
    }
    fprintf(stderr,"Mfdr %d modes, %d packets - sort_mfdr()\n",nIdx,nPktCnt);
    for(nIdx=0;pMfdr[nIdx]!=NULL;nIdx++){   
      pll=pMfdr[nIdx];  i=0;  /* count packets */
      while(pll!=NULL){
        pll=NextMpLnkLst(pll);
        ++i;
      }
      fprintf(stderr,"  %0X with %d packets  ",pMfdr[nIdx]->nMode,i);
      fprintf(stderr,"%s 0x%08X\n",
        CasSpice_nSclk_to_sScet(pMfdr[nIdx]->nSclkSec,pMfdr[nIdx]->nSclkFine,NULL),
        pMfdr[nIdx]->nSclkSec);

      pll=pMfdr[nIdx];  i=0;
      fprintf(stderr,"    ");
      for(i=0;i<6;i++)   
        fprintf(stderr,"%02X ",pMfdr[nIdx]->pCasRec->data[i]);
      fprintf(stderr,"\n");

    }
  }/* fi bverbose==true */
 
return;
}



void sort_mfr(Ulong nMode)
{
int i,nIdx,nPktCnt;
MpLnkLst *pll,*pList;

  /* mfr modes will only differ by normal/fast toggle mode */
  nPktCnt=0;
  pll=pList=HeadMpLnkLst(pMfr[0]);  pMfr[0]=NULL;
  while(pll!=NULL){  
    pList=DeleteMpLnkLst(pll);
    nIdx=0;
    while(pMfr[nIdx]!=NULL){ /* check to see if mode is already on list */
      if( (pMfr[nIdx]->nMode&~CasAntMask)==(pll->nMode&~CasAntMask) ){
        pMfr[nIdx]=AppendMpLnkLst(pMfr[nIdx],pll);
        break;
      }
      ++nIdx;
    }
    if(pMfr[nIdx]==NULL){  /* no match => new mode */
      pMfr[nIdx]=AppendMpLnkLst(pMfr[nIdx],pll);
      pMfr[nIdx+1]=NULL;   /* make sure list is always null terminated */
      assert((nIdx+1)<8);
    } 
    pll=pList;  ++nPktCnt;
  }
  
  nIdx=0;
  while(pMfr[nIdx]!=NULL){
    pMfr[nIdx]=HeadMpLnkLst(pMfr[nIdx]);
    ++nIdx;
  }


  if(bVerbose==true){
    nIdx=0;
    while(pMfr[nIdx]!=NULL){
      pMfr[nIdx]=HeadMpLnkLst(pMfr[nIdx]);
      ++nIdx;
    }
    fprintf(stderr,"Mfr %d modes, %d packets - sort_mfr()\n",nIdx,nPktCnt);
    for(nIdx=0;pMfr[nIdx]!=NULL;nIdx++){   
      pll=pMfr[nIdx];  i=0;  /* count packets */
      while(pll!=NULL){
        pll=NextMpLnkLst(pll);
        ++i;
      }
      fprintf(stderr,"  %0X with %d packets  ",pMfr[nIdx]->nMode,i);
      fprintf(stderr,"%s 0x%08X\n",
        CasSpice_nSclk_to_sScet(pMfr[nIdx]->nSclkSec,pMfr[nIdx]->nSclkFine,NULL),
        pMfr[nIdx]->nSclkSec);

      pll=pMfr[nIdx];  i=0;
      fprintf(stderr,"    ");
      for(i=0;i<5;i++)   
        fprintf(stderr,"%02X ",pMfr[nIdx]->pCasRec->data[i]);
      fprintf(stderr,"\n");

    }
  }/* fi bverbose==true */

return;
}



void sort_hfr(Ulong nMode)
{
int nIdx,nPktCnt,i;
MpLnkLst *pll,*pList;

  /* hfr modes differ by comparison of mp header */
  nPktCnt=0;
  pll=pList=HeadMpLnkLst(pHfr[0]);  pHfr[0]=NULL;
  while(pll!=NULL){  
    pList=DeleteMpLnkLst(pll);
    nIdx=0;
    while(pHfr[nIdx]!=NULL){ /* check to see if mode is already on list */
      if( (pHfr[nIdx]->nMode&~CasAntMask)==(pll->nMode&~CasAntMask) ){
        /* possible match, need to compare headers */
        for(i=7;i<24;i++){
          if(pHfr[nIdx]->pCasRec->data[i]!=pll->pCasRec->data[i])  
            break;
        }
        if(i==24){
          pHfr[nIdx]=AppendMpLnkLst(pHfr[nIdx],pll);  /* looks like a match */
          break;
        }
      }/* possible match */
      ++nIdx;
    }
    if(pHfr[nIdx]==NULL){  /* no match => new mode */
      pHfr[nIdx]=AppendMpLnkLst(pHfr[nIdx],pll);
      pHfr[nIdx+1]=NULL;   /* make sure list is always null terminated */
      assert((nIdx+1)<128);
    } 
    pll=pList;  ++nPktCnt;
  }
  
  if(bVerbose==true){
    nIdx=0;
    while(pHfr[nIdx]!=NULL){
      pHfr[nIdx]=HeadMpLnkLst(pHfr[nIdx]);
      ++nIdx;
    }
    fprintf(stderr,"Hfr %d modes, %d packets - sort_hfr()\n",nIdx,nPktCnt);
    for(nIdx=0;pHfr[nIdx]!=NULL;nIdx++){   
      pll=pHfr[nIdx];  i=0;  /* count packets */
      while(pll!=NULL){
        pll=NextMpLnkLst(pll);
        ++i;
      }
      fprintf(stderr,"  %0X with %d packets  ",pHfr[nIdx]->nMode,i);
      fprintf(stderr,"%s 0x%08X\n",
        CasSpice_nSclk_to_sScet(pHfr[nIdx]->nSclkSec,pHfr[nIdx]->nSclkFine,NULL),
        pHfr[nIdx]->nSclkSec);

      pll=pHfr[nIdx];  i=0;
      fprintf(stderr,"    ");
      for(i=0;i<25;i++)   
        fprintf(stderr,"%02X ",pHfr[nIdx]->pCasRec->data[i]);
      fprintf(stderr,"\n");

    }
  }/* fi bverbose==true */
 

return;
}



void sort_hfrmsc(Ulong nMode)
{
/* bool bMatch; */
int nIdx,nPktCnt,i;
MpLnkLst *pll,*pList;


  
  /* hfr msc modes differ by length of capture, for pds archiving */
  /* for now, sort by sample rate, freq, and number samples */
  nPktCnt=0;
  pll=pList=HeadMpLnkLst(pHfrMsc[0]);  pHfrMsc[0]=NULL;
  while(pll!=NULL){  
    pList=DeleteMpLnkLst(pll);
    nIdx=0;  /* bMatch=false; */
    while(pHfrMsc[nIdx]!=NULL){ /* check to see if mode is already on list */
      for(i=4;i<CasHfrMsc_HeaderLength;i++){
        if(pHfrMsc[nIdx]->pCasRec->data[i]!=pll->pCasRec->data[i])
          break;
      } 
      
      if(i==CasHfrMsc_HeaderLength){/* match */
        pHfrMsc[nIdx]=AppendMpLnkLst(pHfrMsc[nIdx],pll);
        break;
      }
      ++nIdx;
    }
    if(pHfrMsc[nIdx]==NULL){  /* no match => new mode */
      pHfrMsc[nIdx]=AppendMpLnkLst(pHfrMsc[nIdx],pll);
      pHfrMsc[nIdx+1]=NULL;   /* make sure list is always null terminated */
      assert((nIdx+1)<128);
    } 
    pll=pList;  ++nPktCnt;
  }
  
  if(bVerbose==true){
    nIdx=0;
    while(pHfrMsc[nIdx]!=NULL){
      pHfrMsc[nIdx]=HeadMpLnkLst(pHfrMsc[nIdx]);
      ++nIdx;
    }

    fprintf(stderr,"Msc %d modes, %d packets - sort_msc()\n",nIdx,nPktCnt);
    for(nIdx=0;pHfrMsc[nIdx]!=NULL;nIdx++){   

      pll=pHfrMsc[nIdx];  i=0;  /* count packets */
      while(pll!=NULL){
        pll=NextMpLnkLst(pll); ++i;}

      fprintf(stderr,"  %0X with %d packets  ",pHfrMsc[nIdx]->nMode,i);
      fprintf(stderr,"%s 0x%08X\n",
      CasSpice_nSclk_to_sScet(pHfrMsc[nIdx]->nSclkSec,pHfrMsc[nIdx]->nSclkFine,NULL),
      pHfrMsc[nIdx]->nSclkSec);

      pll=pHfrMsc[nIdx];  i=0;
      fprintf(stderr,"    ");
      for(i=0;i<CasHfrMsc_HeaderLength;i++)   
        fprintf(stderr,"%02X ",pHfrMsc[nIdx]->pCasRec->data[i]);
      fprintf(stderr,"\n");

    }
  }/* fi bverbose==true */
  
return;
}


/* Assume everything on the list will be written to a file.
   -> Mfr Normal and Mfr Fast Toggle should be on different lists 
   -> Mfr Fast Toggle 
   -> Hfr Bands should be called one at a time; BandABC, BandHf1, BandHf2
   -> Direction Finding 
*/
Ulong write_pdsrecord(FILE *h,MpLnkLst *pHead,Ulong nWrtMode)
{
Ulong nMode,nCnt;
MpLnkLst *p,*pTail;

Uchar *pBuf;
Ulong nRecLen,nNumRec,nItems,nRecMode;
Ulong *pDword;
float *pFloat;

Uchar *pMp;

int i;
int (*calibrate)();
int nLen;
char sScet[32];
/* Ulong nSclkSecBeg,nSclkFineBeg,nSclkSecEnd,nSclkFineEnd; */
Ulong nSec,nFine,nDays,nMsec;
Ulong nTmp,nAnts,nAnt;
float *pTime,*pFreq,fTdif;
bool bHfrDF=false,bMfrFT=false;
int nFilters,nSteps,s;

  /* enforce calling the hfr by bands */
  if((nWrtMode&CasRcv_Mask)==CasHfr_Analysis){
    nCnt=0;
    if(nWrtMode&CasHfr_BandABC)  ++nCnt;
    if(nWrtMode&CasHfr_BandHF1)  ++nCnt;
    if(nWrtMode&CasHfr_BandHF2)  ++nCnt;
    assert(nCnt==1);
  }

  /* check for to see if the data called for exists */
  /* this is of questional value */
  nRecMode=0;  nNumRec=0;
  p=pHead=HeadMpLnkLst(pHead);
  while(p!=NULL){
    nRecMode|=p->nMode;
    ++nNumRec;
    p=NextMpLnkLst(p);
  }
  p=pHead=HeadMpLnkLst(pHead);
  pTail=TailMpLnkLst(pHead);

  /* validation for lfdr,mfdr,mfr: hfr might erronously pass */
  if( (nRecMode&CasMp_Mask)!=(nWrtMode&CasMp_Mask) )  /* right mini-packet ? */
    return 0;  /* high order  nibble 0xF0000000 */
  if( !((nRecMode&0x0F000000)&nWrtMode) )        /* right mode ? */
    return 0;  /* 2nd highest nibble 0x0F000000 */
  if( !((nRecMode&0x00FFF000)&nWrtMode) )        /* rcv specific bands/modes */
    return 0;  /* nibbles 0x00FFF000, mabey look for error packets */
  if( !((nRecMode&nWrtMode)&CasAntMask) )        /* any antennas ? */
    return 0;  /* nibble 0x00000FFF */
  nWrtMode&=nRecMode;  /* modify call to reflect list contents */

  switch(nWrtMode&CasRcv_Mask){
    case CasLfdr_Normal:
    case CasMfdr_Normal:
      calibrate=CasWfdr_GetPhysicalUnits;
      break;
    case CasMfr_Normal:
    case CasMfr_FastToggle:
    case CasMfr_AnyMode:    /* Normal & FastToggle */
      calibrate=CasMfr_GetPhysicalUnits;
      break;
    case CasHfr_Analysis:
      calibrate=CasHfrAnalysis_GetPhysicalUnits;
      break;
    case CasHfr_Millisecond:
      calibrate=CasHfrMillisecond_GetPhysicalUnits;
      break;
    default:
      assert(0);
      break;
  }
 
/* 
fprintf(stderr,"nMode=%08X %08X %s\n",nMode&p->nMode,p->nSclkSec,
        CasSpice_nSclk_to_sScet(p->nSclkSec,p->nSclkFine,NULL) );
*/
  /* determine the record length, accept any antenna */
  nMode=nWrtMode|CasAntAll;
  assert((nItems=calibrate(p->pCasRec,nMode,arTime,arFreq,arAmpl)) > 0);

/* raj March 8, 2005
CasHfr_DumpAgc();
CasHfr_DumpAuto();
CasHfr_DumpAutoMag();
*/

  bHfrDF=false;  bMfrFT=false;
  pMp=p->pCasRec->data;
  if((nWrtMode&CasRcv_Mask)==CasHfr_Analysis){
    /* call cal again to see if there really is some data */
    if((nTmp=calibrate(p->pCasRec,nWrtMode,arTime,arFreq,arAmpl)) == 0)  
      return 0;/* nWrtMode should be a union with the call and pkt contents */

    nCnt=0;
    if(nWrtMode&CasHfr_BandABC){
      if(CasHfr_bDirectionFindingABC(pMp)==true)  bHfrDF=true;
      else{  /* see which antennas are turned on */
        if(CasHfr_bEwOnABC(pMp)==true){  ++nCnt;  nAnt=CasAntEz;}
        if(CasHfr_bEuOnABC(pMp)==true){  ++nCnt;  nAnt=CasAntEu;}
        if(CasHfr_bEvOnABC(pMp)==true){  ++nCnt;  nAnt=CasAntEv;}
        if(CasHfr_bExOnABC(pMp)==true){  ++nCnt;  nAnt=CasAntEx;}
      }
      nFilters=CasHfr_nFiltersABC(pMp);
      nSteps=1;
    }
    else if(nWrtMode&CasHfr_BandHF1){
      if(CasHfr_bDirectionFindingHF1(pMp)==true)  bHfrDF=true;
      else{  /* see which antennas are turned on */
        if(CasHfr_bEwOnHF1(pMp)==true){  ++nCnt;  nAnt=CasAntEw;}
        if(CasHfr_bEuOnHF1(pMp)==true){  ++nCnt;  nAnt=CasAntEu;}
        if(CasHfr_bEvOnHF1(pMp)==true){  ++nCnt;  nAnt=CasAntEv;}
        if(CasHfr_bExOnHF1(pMp)==true){  ++nCnt;  nAnt=CasAntEx;}
      }
      nFilters=CasHfr_nFiltersHF1(pMp);
      nSteps=CasHfr_nNumberStepsHF1(pMp);
    }
    else if(nWrtMode&CasHfr_BandHF2){
      if(CasHfr_bDirectionFindingHF2(pMp)==true)  bHfrDF=true;
      else{  /* see which antennas are turned on */
        if(CasHfr_bEwOnHF2(pMp)==true){  ++nCnt;  nAnt=CasAntEw;}
        if(CasHfr_bEuOnHF2(pMp)==true){  ++nCnt;  nAnt=CasAntEu;}
        if(CasHfr_bEvOnHF2(pMp)==true){  ++nCnt;  nAnt=CasAntEv;}
        if(CasHfr_bExOnHF2(pMp)==true){  ++nCnt;  nAnt=CasAntEx;}
      }
      nFilters=CasHfr_nFiltersHF2(pMp);
      nSteps=CasHfr_nNumberStepsHF2(pMp);
    }
    else{
      assert(0);
    }

    if(bHfrDF==true){
      nItems/=4;/* Eu,Ew,Ev,Ew */
      nNumRec*=4;
      nAnt=CasAntEu;
    }
    else{
      assert((nCnt==1) || (nCnt==2));
      nItems/=nCnt;
      nNumRec*=nCnt;
    }

    /* preload the frequency and time arrays */
    nMode=nWrtMode&~CasAntAll;  nMode|=nAnt;
    nTmp=calibrate(p->pCasRec,nMode,arTime,arFreq,arAmpl);
  }/* yet another hfr special case */
  else if((nWrtMode&CasRcv_Mask)==CasMfr_FastToggle){
    bMfrFT=true;
    nItems/=2;             /* two different antennas */
    nNumRec*=2;
    nAnt=CasMfr_n1stAnt(p->pCasRec->data);
    switch(nAnt){
      case 0x00:  nAnt=CasAntEx;  break;/* need to check for Eu/Ev */
      case 0x01:  nAnt=CasAntEw;  break;
      case 0x02:  nAnt=CasAntBx;  break;
      case 0x03:  nAnt=CasAntBz;  break;
      default: assert(0);       break;
    }
    nMode=nWrtMode&~CasAntMask;  nMode|=nAnt;
    nTmp=calibrate(p->pCasRec,nMode,arTime,arFreq,arAmpl);
  }
  else{
    nNumRec=nNumRec;  /* do nothing */
  }
  nRecLen=(nItems+4)*4;  /* record length in bytes */
  
  if( (nNumRec*nRecLen)>OUT_BUF_MAX ){  /* nNumRec is list packet count */
    fprintf(stderr,"write_pdsrecord() buffer overrun...bailing\n");
    exit(1);
  }


  /* Format the first record: CORPWS01 header */
  for(i=0;i<nRecLen;i++)   /* zero out the record header */
    OutBuf[i]=0x00;
  pBuf=OutBuf;
  pBuf+=sprintf((char*)pBuf,"CORPWS01");
  pDword=(void*)pBuf;
  *pDword++=nRecLen; 
  *pDword++=nNumRec; 
  *pDword++=nRecMode; 
  *pDword++=0x00;      /* zero pad */
  pBuf=(Uchar*)pDword;
  nLen=CasMp_nHeaderLength(pHead->pCasRec);
  for(i=0;i<nLen;i++)  
    pBuf[i]=pHead->pCasRec->data[i];                   
  pBuf+=24;  /* hfr header may be 25, so stomp on it with the next write */
  /* sclk/scet pair for beginning of the day,         1999-230T02:30:59.375 
     zero hr,min,sec,msec from first packet time.     0123456789ABCDEF01234 */
  CasSpice_nSclk_to_sScet(pHead->nSclkSec,pHead->nSclkFine,sScet);
  sScet[ 9]=sScet[10]='0'; sScet[12]=sScet[13]='0';
  sScet[15]=sScet[16]='0'; sScet[18]=sScet[19]=sScet[20]='0'; 
  CasSpice_sScet_to_nSclk(sScet,&nSec,&nFine);
  pBuf+=sprintf((char*)pBuf,"%.14s  ",sScet);
  pBuf+=sprintf((char*)pBuf,"  %.14s",CasSpice_nSclk_to_sSclk(nSec,nFine,NULL));



  /* second record : time offset, first sclk */
  pBuf=OutBuf+nRecLen;
  pDword=(void*)pBuf;  /* should be (Ulong*), but (void*) give no errors */
  *pDword++=pHead->nSclkSec;
  pBuf=(Uchar*)pDword;
  *pBuf++=0x00;  
  *pBuf++=pHead->nSclkFine&0x0FF;
  *pBuf++=pHead->nScetDays>>8;
  *pBuf++=pHead->nScetDays&0x0FF;
  pDword=(void*)pBuf;  /* should be (Ulong*), but (void*) give no errors */
  *pDword++=pHead->nScetMsec;
  *pDword++=0x00;
  pFloat=(float*)pDword;  pTime=(float*)pDword;
  for(i=0;i<nItems;i++)
    *pFloat++=arTime[i];



  /* third record : frequency, last sclk */
  pBuf=OutBuf+2*nRecLen;
  pDword=(void*)pBuf;  /* should be (Ulong*), but (void*) give no errors */
  *pDword++=pTail->nSclkSec;
  pBuf=(Uchar*)pDword;
  *pBuf++=0x00;  
  *pBuf++=pTail->nSclkFine&0x0FF;
  *pBuf++=pTail->nScetDays>>8;
  *pBuf++=pTail->nScetDays&0x0FF;
  pDword=(void*)pBuf;  /* should be (Ulong*), but (void*) give no errors */
  *pDword++=pTail->nScetMsec;
  *pDword++=0x00;
  pFloat=(float*)pDword;  pFreq=(float*)pDword;
  for(i=0;i<nItems;i++)
    *pFloat++=arFreq[i];



  /* fourth record : amplitude event sclk */
  p=pHead;
  pBuf=OutBuf+3*nRecLen;
  nNumRec=nRecMode=0;

  /* begins special cases for hfr directions finding and mfr fast toggle */
  if(bHfrDF==true){  /* got to love all of those hfr modes */
    while(p!=NULL){
      nAnts=nWrtMode&(CasAntEu|CasAntEv|CasAntEw);
      while(nAnts){
        if(nAnts&CasAntEu)        nAnt=CasAntEu;  /* list is order specific */
        else if(nAnts&CasAntEw){  nAnt=CasAntEw;  nAnts|=0x80000000;}
        else if(nAnts&CasAntEv)   nAnt=CasAntEv; /* set msb for 2nd Ew set*/
        else{                     nAnt=CasAntEw;  nAnts=0;}

        nMode=nWrtMode&~CasAntMask;  nMode|=nAnt;
        nTmp=calibrate(p->pCasRec,nMode,arTime,arFreq,arAmpl);
        if(nTmp==0) assert(0);
        if((nAnt==CasAntEw) && (nTmp!=(2*nItems)))  assert(0);
        if((nAnt!=CasAntEw) && (nTmp!=nItems))      assert(0);
        nMode=p->nMode&nMode;  /* reflects the returned data */
        nRecMode|=nMode;

        if(nAnt==CasAntEw){  /* shuffle 1st Ew set down in array */
        Ulong nSrc,nDst;
          if(nAnts&0x80000000){  /* Ew 1st data set */
            nSrc=2*nFilters*nSteps;  nDst=nFilters*nSteps;
            while(nSrc<nTmp){
              for(s=0;s<nSteps;s++){
              for(i=0;i<nFilters;i++){
                arTime[i+nDst]=arTime[i+nSrc];
                arFreq[i+nDst]=arFreq[i+nSrc];
                arAmpl[i+nDst]=arAmpl[i+nSrc];
              }
              nSrc+=nFilters;  nDst+=nFilters;
              }
              nSrc+=nFilters*nSteps; 
            }
            assert(nDst==nItems);
            nSec=p->nSclkSec;    nFine=p->nSclkFine;
            nDays=p->nScetDays;  nMsec=p->nScetMsec;
          }
          else{  /* Ew 2nd data set */
            nSrc=nFilters*nSteps;  nDst=0;
            while(nSrc<nTmp){
              for(s=0;s<nSteps;s++){
              for(i=0;i<nFilters;i++){
                arTime[i+nDst]=arTime[i+nSrc];
                arFreq[i+nDst]=arFreq[i+nSrc];
                arAmpl[i+nDst]=arAmpl[i+nSrc];
              }
                nDst+=nFilters;  nSrc+=nFilters;
              }/*steps */
              nSrc+=nFilters*nSteps;
            }
            assert(nDst==nItems);
            fTdif=arTime[1]-pTime[1];/* 1st sample will never line up */
            assert(fTdif>0.0); /* for hf1 & hf2 */
            /* fake sclk to be the start of Ev/Ew capture */
            nSec=p->nSclkSec;    nFine=p->nSclkFine;
            nSec+=(Ulong)fTdif;
            nFine+=(fTdif-(Ulong)fTdif)*256.0;
            if(nFine/256){ nSec+=(nFine/256); nFine=nFine%256;}
            CasSpice_nSclk_to_nScet(nSec,nFine,&nDays,&nMsec);
            /* normalize to new sclk start time */
            for(i=0;i<nItems;i++)
              arTime[i]-=fTdif;
          }
        }
        else if(nAnt==CasAntEv){
          fTdif=arTime[1]-pTime[1];
          assert(fTdif>0.0);
          /* fake sclk to be the start of Ev/Ew capture */
          nSec=p->nSclkSec;    nFine=p->nSclkFine;
          nSec+=(Ulong)fTdif;
          nFine+=(fTdif-(Ulong)fTdif)*256.0;
          if(nFine/256){ nSec+=(nFine/256); nFine=nFine%256;}
          CasSpice_nSclk_to_nScet(nSec,nFine,&nDays,&nMsec);
          /* normalize to new sclk start time */
          for(i=0;i<nItems;i++)
            arTime[i]-=fTdif;
        }
        else{
          nSec=p->nSclkSec;    nFine=p->nSclkFine;
          nDays=p->nScetDays;  nMsec=p->nScetMsec;
        }

        pDword=(void*)pBuf;  /* (Ulong*) shouldn't give warnings */
        *pDword++=nSec;
        nTmp=nFine<<16;  nTmp|=nDays;
        *pDword++=nTmp;
        *pDword++=nMsec;
        
        nTmp=FlagData(p,nMode,nItems,arTime,arFreq,arAmpl);
        switch(nAnt){
          case CasAntEu:  nTmp|=0x01;  break;
          case CasAntEv:  nTmp|=0x02;  break;
          case CasAntEw:  nTmp|=0x03;  break;
          default:        assert(0);   break;
        }

        *pDword++=nTmp;  /* jump over flag and sensor bits */

        pFloat=(float*)pDword;
        for(i=0;i<nItems;i++)
          *pFloat++=arAmpl[i];
        pBuf=(Uchar*)pFloat;
        ++nNumRec;

        if(bVerify==true){ /* floating point compare should fail sometime */
          for(i=1;i<nItems;i++){
            if( ((pTime[i]-arTime[i])<-fTimeMargin) || 
                ((pTime[i]-arTime[i])> fTimeMargin) || 
                                         (pFreq[i]!=arFreq[i]) ){
              fprintf(stderr,"\nerror time/frequency mismatch hfr df "
                "nNumRec=%d nMode=%08X\n   sclk=%08X, %s\n",
                nNumRec,nMode,p->nSclkSec,
                CasSpice_nSclk_to_sScet(p->nSclkSec,p->nSclkFine,NULL));
              fprintf(stderr,"   %d  %.3f!=%.3f (sec) or %.3f!=%.3f (Hz)\n",
                      i,pTime[i],arTime[i],pFreq[i],arFreq[i]);
              fprintf(stderr,"Header Time\n");
              for(i=0;i<nItems;i++){
                fprintf(stderr,"%5d %9.3f (%9.3f)  %9.3f (%9.3f) %9.3E\n",
                i,arTime[i],pTime[i],arFreq[i],pFreq[i],arAmpl[i]);
              }
              for(i=0;i<25;i++)
                fprintf(stderr,"%02X ",p->pCasRec->data[i]);
              fprintf(stderr,"\n");
              assert(0);
            }
          }
        }/* verify */
        nAnts&=~nAnt;
      }/* df antennas */

      p=NextMpLnkLst(p);
    }/* ampl data to buffer */
  }/* got to love those hfr modes */ 
  else if(bMfrFT==true){

    while(p!=NULL){             /* valid mfr antennas Ex+,Ex-,Ex,Ez,Bx,Bz */

      nMode=p->nMode&nWrtMode;
      nAnts=nMode&CasAntMask;
      if(((nMode&CasMfr_BandMask)==0) || (nAnts==0)){
fprintf(stderr,"mfr fast toggle skipping...%08X\n",p->nMode);
        p=NextMpLnkLst(p);
        continue;
      }

      while(nAnts){

        if((nAnts&0x80000000)==0){  /* 1st half of sweep */
          nAnt=CasMfr_n1stAnt(p->pCasRec->data);
          nAnts|=0x80000000;
        }
        else{                       /* 2nd antenna marker */ 
          nAnt=CasMfr_n2ndAnt(p->pCasRec->data);
          nAnts=0;
        }
        switch(nAnt){
          case 0x00:  nAnt=CasAntEx;  break;/* need to check for Eu/Ev */
          case 0x01:  nAnt=CasAntEw;  break;
          case 0x02:  nAnt=CasAntBx;  break;
          case 0x03:  nAnt=CasAntBz;  break;
          default: assert(0);       break;
        }
        nMode=p->nMode&nWrtMode;
        if((nAnt&nMode)==0){fprintf(stderr,"skipping packet\n");  continue;}
        nMode&=~CasAntMask;  nMode|=nAnt;
        nTmp=calibrate(p->pCasRec,nMode,arTime,arFreq,arAmpl);
        if(nTmp!=nItems){
          fprintf(stderr,"\nMFR Packet Length Error\n");
          fprintf(stderr,"nTmp=%d, nItems=%d\n",nTmp,nItems);  
          fprintf(stderr,"nMode=%08X, pkt=%08X\n",nMode,p->nMode);
          fprintf(stderr,"%08X %s\n",p->nSclkSec,
                  CasSpice_nSclk_to_sScet(p->nSclkSec,p->nSclkFine,NULL));
          assert(0);
        }
        nMode&=p->nMode;  /* reflects the returned data */
        nRecMode|=nMode;
        if(nAnts==0){/* sweep 2 of fast toggle */
          fTdif=arTime[0]-pTime[0];  /* time shift 2nd sweep */
          assert(fTdif>0.0);
          /* fake sclk to be the start of Ev/Ew capture */
          nSec=p->nSclkSec;    nFine=p->nSclkFine;
          nSec+=(Ulong)fTdif;
          nFine+=(fTdif-(Ulong)fTdif)*256.0;
          if(nFine/256){ nSec+=(nFine/256); nFine=nFine%256;}
          CasSpice_nSclk_to_nScet(nSec,nFine,&nDays,&nMsec);
          /* normalize to new sclk start time */
          for(i=0;i<nTmp;i++)
            arTime[i]-=fTdif; 
        }
        else{
          nSec=p->nSclkSec;    nFine=p->nSclkFine;
          nDays=p->nScetDays;  nMsec=p->nScetMsec;
        } 

        pDword=(void*)pBuf;  /* (Ulong*) shouldn't give warnings */
        *pDword++=nSec;
        nTmp=nFine<<16;  nTmp|=nDays;
        *pDword++=nTmp;
        *pDword++=nMsec;

        nTmp=FlagData(p,nMode,nItems,arTime,arFreq,arAmpl);
        switch(nAnt){
          case CasAntEx:  nTmp|=0x00;  break;
          case CasAntEu:  nTmp|=0x01;  break;
          case CasAntEv:  nTmp|=0x02;  break;
          case CasAntEw:  nTmp|=0x03;  break;
          case CasAntBx:  nTmp|=0x04;  break;
          case CasAntBz:  nTmp|=0x06;  break;
          default:        assert(0);   break;
        }

        *pDword++=nTmp;   

        pFloat=(float*)pDword;
        for(i=0;i<nItems;i++)
          *pFloat++=arAmpl[i];
        pBuf=(Uchar*)pFloat;
        ++nNumRec;
      
        if(bVerify==true){
          for(i=0;i<nItems;i++){
            if((pTime[i]!=arTime[i]) || (pFreq[i]!=arFreq[i])){
              fprintf(stderr,"\nerror time/frequency mismatch mfr ft "
                "nNumRec=%d nMode=%08X\n   sclk=%08X, %s\n",
                nNumRec,nMode,p->nSclkSec,
                CasSpice_nSclk_to_sScet(p->nSclkSec,p->nSclkFine,NULL));
              fprintf(stderr,"   %d %.3f!=%.3f (sec) or %.3f!=%.3f (Hz)\n",
                      i,pTime[i],arTime[i],pFreq[i],arFreq[i]);
              assert(0);
            }
          }
        }
        nAnts&=~nAnt;
      }/* while all antennas */
      
      p=NextMpLnkLst(p);
    }/* ampl data to buffer */
  }/* elseif mfr fast toggle mode */
  else{ /* catch all for all of the others */
    while(p!=NULL){
      nAnts=nWrtMode&CasAntMask;
      while(nAnts){
  
        if(nAnts&CasAntEx){         nAnt=CasAntEx;    pBuf[15]=0x00;}
        else if(nAnts&CasAntEu){    nAnt=CasAntEu;    pBuf[15]=0x01;}
        else if(nAnts&CasAntEv){    nAnt=CasAntEv;    pBuf[15]=0x02;}
        else if(nAnts&CasAntEw){    nAnt=CasAntEw;    pBuf[15]=0x03;}
        else if(nAnts&CasAntBx){    nAnt=CasAntBx;    pBuf[15]=0x04;}
        else if(nAnts&CasAntBy){    nAnt=CasAntBy;    pBuf[15]=0x05;}
        else if(nAnts&CasAntBz){    nAnt=CasAntBz;    pBuf[15]=0x06;}
        else if(nAnts&CasAntHF){    nAnt=CasAntHF;    pBuf[15]=0x08;}
        else if(nAnts&CasAntLMRp){  nAnt=CasAntLMRp;  pBuf[15]=0x09;}
        else if(nAnts&CasAntLMRm){  nAnt=CasAntLMRm;  pBuf[15]=0x0A;}
        else if(nAnts&CasAntLPs){   nAnt=CasAntLPs;   pBuf[15]=0x0B;}
        else{                       nAnt=0x0800;      pBuf[15]=0x0F;}
  
        nMode=nWrtMode&~CasAntMask;  nMode|=nAnt;
        nTmp=calibrate(p->pCasRec,nMode,arTime,arFreq,arAmpl);
        if(nTmp==0){  nAnts&=~nAnt;  continue;}
  
        /* check arTime[0] to see if it is zero */
  
        if(nTmp!=nItems){
          fprintf(stderr,"error nTmp=%d and nItems=%d, nMode=%08X\n",
                          nTmp,nItems,nMode);
          exit(1);
        }
        nMode&=p->nMode;  /* reflects the returned data */
        nRecMode|=nMode;
        
      
        pDword=(void*)pBuf;  /* (Ulong*) shouldn't give warnings */
        *pDword++=p->nSclkSec;
        pBuf=(Uchar*)pDword;
        *pBuf++=0x00;  
        *pBuf++=p->nSclkFine&0x0FF;
        *pBuf++=p->nScetDays>>8;
        *pBuf++=p->nScetDays&0x0FF;
        pDword=(void*)pBuf;  /* (Ulong*) shouldn't give warnings */
        *pDword++=p->nScetMsec;

        /* write antenna and data quality information */
        nTmp=FlagData(p,nMode,nItems,arTime,arFreq,arAmpl);
        switch(nAnt){
          case CasAntEx:    nTmp|=0x00;  break;
          case CasAntEu:    nTmp|=0x01;  break;
          case CasAntEv:    nTmp|=0x02;  break;
          case CasAntEw:    nTmp|=0x03;  break;
          case CasAntBx:    nTmp|=0x04;  break;
          case CasAntBy:    nTmp|=0x05;  break;
          case CasAntBz:    nTmp|=0x06;  break;
          case CasAntHF:    nTmp|=0x08;  break;
          case CasAntLMRp:  nTmp|=0x09;  break;
          case CasAntLMRm:  nTmp|=0x0A;  break;
          case CasAntLPs:   nTmp|=0x0B;  break;
          default:          nTmp|=0x0F;  break;
        }
        *pDword++=nTmp;     /* write data quality flags and sensor ids */
/*
if(nTmp&0xFFFF0000)  fprintf(stderr,"normal nTmp=%08X\n",nTmp);
*/
        pFloat=(float*)pDword;
        for(i=0;i<nItems;i++)
          *pFloat++=arAmpl[i];
        pBuf=(Uchar*)pFloat;
        ++nNumRec;
      
        if(bVerify==true){
          for(i=0;i<nItems;i++){
            if((pTime[i]!=arTime[i]) || (pFreq[i]!=arFreq[i])){
              fprintf(stderr,"\nerror time/frequency mismatch general case "
                "nNumRec=%d nMode=%08X\n   sclk=%08X, %s\n",
                nNumRec,nMode,p->nSclkSec,
                CasSpice_nSclk_to_sScet(p->nSclkSec,p->nSclkFine,NULL));
              fprintf(stderr,"   %d %.3f!=%.3f (sec) or %.3f!=%.3f (Hz)\n",
                      i,pTime[i],arTime[i],pFreq[i],arFreq[i]);
              assert(0);
/* raj */
for(i=0;i<nItems;i++)
  fprintf(stderr,"%2d %9.4f (%9.4f) %9.2f %.3e\n",i,arTime[i],pTime[i],
          arFreq[i],arAmpl[i]);
              assert(0);
            }
          }
        }

        nAnts&=~nAnt;
      }/* different antennas */
      p=NextMpLnkLst(p);
    }/* ampl data to buffer */
  }/* else general case */

  nNumRec+=3;  /* add in Header,Time,Freq Records */

  /* clean-up portion, nNumRec,nRecLen,nMode,nMini-Packet Header */
  /* nNumRec*nRecLen */
  pDword=(void*)OutBuf;
  pDword+=2;          /* skip "CORPWS01" */
  *pDword++=nRecLen; 
  *pDword++=nNumRec; 
  *pDword++=nRecMode; 

  if(nNumRec<4){
    assert(nNumRec>3);  /* bail for now */
    return 0;
  }

  if(1){ /* record reporting */
  char sFileName[256];
  int nHskIdx;
  PdsHsk **pHsk;

  /* sclk/scet pair for beginning of the day,         1999-230T02:30:59.375 
     zero hr,min,sec,msec from first packet time.     0123456789ABCDEF01234 */
  sScet[4]=sScet[5];sScet[5]=sScet[6];sScet[6]=sScet[7];sScet[7]='\0';

  switch(nRecMode&CasRcv_Mask){
    case CasLfdr_Normal:  
      sprintf(sFileName,"T%s_LFR",sScet);  pHsk=hskLfdr;  break;
    case CasMfdr_Normal:
      sprintf(sFileName,"T%s_MFDR",sScet);  pHsk=hskMfdr;  break;
    case CasMfr_Normal:
    case CasMfr_FastToggle:
      sprintf(sFileName,"T%s_MFR",sScet);   pHsk=hskMfr;   break;
    case CasHfr_Analysis:
      sprintf(sFileName,"T%s_HFR",sScet);   pHsk=hskHfr;   break;
    case CasHfr_Millisecond:
      sprintf(sFileName,"T%s_MSC",sScet);   pHsk=hskHfrMsc;   break;
    default: fprintf(stderr,"Unknow receiver mode %08X\n",nRecMode);
             fprintf(stderr,"  %08X %s\n",pHead->nSclkSec,
             CasSpice_nSclk_to_sScet(pHead->nSclkSec,pHead->nSclkFine,NULL));
    exit(1);
  }
  nHskIdx=0;
  while(pHsk[nHskIdx]!=NULL)  ++nHskIdx;
  pHsk[nHskIdx+1]=NULL;
  pHsk[nHskIdx]=calloc(1,sizeof(PdsHsk));
  pHsk[nHskIdx]->sFileName=calloc(1,32);  assert(strlen(sFileName)<24);
  sprintf(pHsk[nHskIdx]->sFileName,"%s%d.DAT",sFileName,nHskIdx);
  pHsk[nHskIdx]->nSecBeg=pHead->nSclkSec;
  pHsk[nHskIdx]->nFineBeg=pHead->nSclkFine;
  pHsk[nHskIdx]->nDaysBeg=pHead->nScetDays;
  pHsk[nHskIdx]->nMsecBeg=pHead->nScetMsec;
  pHsk[nHskIdx]->nSecEnd=pTail->nSclkSec;
  pHsk[nHskIdx]->nFineEnd=pTail->nSclkFine;
  pHsk[nHskIdx]->nDaysEnd=pTail->nScetDays;
  pHsk[nHskIdx]->nMsecEnd=pTail->nScetMsec;
  pHsk[nHskIdx]->nNumRec=nNumRec;
  pHsk[nHskIdx]->nRecLen=nRecLen;
  pHsk[nHskIdx]->nRecMode=nRecMode;

  if(h==NULL){  /* auto file generation */
    if((h=fopen(pHsk[nHskIdx]->sFileName,"wb"))==NULL){
      fprintf(stderr,"error opening %s\n", pHsk[nHskIdx]->sFileName);
      exit(1);
    }
    assert( fwrite(OutBuf,sizeof(Uchar),nNumRec*nRecLen,h)==nNumRec*nRecLen);
    fclose(h);
  }
  else{
    assert( fwrite(OutBuf,sizeof(Uchar),nNumRec*nRecLen,h)==nNumRec*nRecLen);
  }

}/* end record report */

  if(bVerbose==true){
    fprintf(stderr,"writing pds record...\n");
    fprintf(stderr,"mode=%08X nRecLen=%d(0x%X) nNumRec=%d(0x%X)\n",
           nRecMode,nRecLen,nRecLen,nNumRec,nNumRec);
  }


return nNumRec;
}



Ulong write_daspacket(FILE *h,MpLnkLst *p)
{
int i;
Ulong nLength,nWrtMode,nOutLen;
Ulong nScetDaysBeg,nScetMsecBeg,nScetDaysEnd,nScetMsecEnd;
Ulong nScetDays,nScetMsec;
float *pF,fEvtTime;
MpLnkLst *pHead;
int (*calibrate)();


  nScetDaysBeg=tStart.nScetDays;  nScetMsecBeg=tStart.nScetMsec;
  nScetDaysEnd=tStop.nScetDays;  nScetMsecEnd=tStop.nScetMsec;

  pHead=HeadMpLnkLst(p);
  while(p!=NULL){
  
    switch(p->nMode&CasRcv_Mask){
      case CasLfdr_Normal:
        nWrtMode=nLfdrMode;
        calibrate=CasWfdr_GetPhysicalUnits;
        break;
      case CasMfdr_Normal:
        nWrtMode=nMfdrMode;
        calibrate=CasWfdr_GetPhysicalUnits;
        break;
      case CasMfr_Normal:
      case CasMfr_FastToggle:
      case CasMfr_AnyMode:    /* Normal & FastToggle */
        nWrtMode=nMfrMode;
        calibrate=CasMfr_GetPhysicalUnits;
        break;
      case CasHfr_Analysis:
        nWrtMode=nHfrMode;
        calibrate=CasHfrAnalysis_GetPhysicalUnits;
        break;
      case CasHfr_Millisecond:
        nWrtMode=nHfrMscMode;
        calibrate=CasHfrMillisecond_GetPhysicalUnits;
        break;
      default:
        assert(0);
        break;
    }
    if(nWrtMode==0){
      p=NextMpLnkLst(p);
      continue;
    }

    nScetDays=p->nScetDays;
    nScetMsec=p->nScetMsec;
    /* filter by scet */ 
    if( nScetDays < nScetDaysBeg )       fEvtTime=-1000.0;
    else if( nScetDays > nScetDaysEnd )  fEvtTime=-1000.0;
    else if( (nScetDays==nScetDaysBeg) &&
             (nScetMsec<nScetMsecBeg) )  fEvtTime=-1000.0;
    else if( (nScetDays==nScetDaysEnd) &&
             (nScetMsec>nScetMsecEnd) )  fEvtTime=-1000.0;
    else if(nScetDays==nScetDaysBeg){  /* same day */
      fEvtTime=nScetMsec-nScetMsecBeg;
    }
    else if(nScetDays==nScetDaysEnd){  /* time spans at least two days */
      fEvtTime=24*60*60*1000-nScetMsecBeg;                  /* first day */
      fEvtTime+=(nScetDays-(nScetDaysBeg+1))*24*60*60*1000; /* mid days */
      fEvtTime+=nScetMsec;                                  /* last day */
    }
    else{
      fEvtTime=24*60*60*1000-nScetMsecBeg;                  /* first day */
      fEvtTime+=(nScetDays-(nScetDaysBeg+1))*24*60*60*1000; /* mid days */
      fEvtTime+=nScetMsec;                                  /* today */
    }
    fEvtTime/=1000.0;
    if(fEvtTime<0.0){
      p=NextMpLnkLst(p);
      continue;
    }

/* 
need to implement a better filter, rather than relying on calibrate()
*/    
/*
typedef struct mp_lnk_lst_tag{
  struct mp_lnk_lst_tag *next,*prev;
  Ulong nSclkSec,nSclkFine;
  Ulong nSclkSecEnd,nSclkFineEnd;
  Ulong nScetDays,nScetMsec;
  Ulong nMode;          
  CasRecord *pCasRec;
}MpLnkLst;
fprintf(stderr,"%s nMode=%08X\n",
  CasSpice_nSclk_to_sScet(p->nSclkSec,p->nSclkFine,NULL),p->nMode );
fprintf(stderr,"  nLength=%d\n",nLength);
*/


    nLength=calibrate(p->pCasRec,nWrtMode,arTime,arFreq,arAmpl);



    if((nLength!=0) && (fEvtTime>0.0)){

      /* flag data for hfr sounder and langmuir probe interferance */
      FlagDataDas(p,p->nMode,nLength,arTime,arFreq,arAmpl);

      if(nLength>5440){     /* 5440*3*4=0xFF00 assert(nOutLen < 0x0FFFF); */
      int j;                /* 5440 % 32 for hfr ABC hf1&2 1,2,4,8 filters */
        i=0; 
        while((nLength-i)>5440){
          nOutLen=5440*sizeof(float)*3;
          sprintf((char*)OutBuf,":b0:%04X",nOutLen);
          pF=(void*)(OutBuf+8);
          for(j=0;j<5440;i++,j++){
            *pF++=arTime[i]+fEvtTime;
            *pF++=arFreq[i];
            *pF++=arAmpl[i];
          }
          nOutLen+=8;  /* packet length in bytes */
          assert(nOutLen==fwrite(OutBuf,sizeof(Uchar),nOutLen,h));
        }/* elihw 5440 */
        if(i<nLength){
          assert((nLength-i)<5440);
          nOutLen=(nLength-i)*sizeof(float)*3;
          sprintf((char*)OutBuf,":b0:%04X",nOutLen);
          pF=(void*)(OutBuf+8);
          for(;i<nLength;i++){
            *pF++=arTime[i]+fEvtTime;
            *pF++=arFreq[i];
            *pF++=arAmpl[i];
          }
          nOutLen+=8;  /* packet length in bytes */
          assert(nOutLen==fwrite(OutBuf,sizeof(Uchar),nOutLen,h));
        }/* fi <5440 */
      }
      else{
        nOutLen=nLength*sizeof(float)*3;
        sprintf((char*)OutBuf,":b0:%04X\n",nOutLen);  
        pF=(void*)(OutBuf+8);
        for(i=0;i<nLength;i++){
          *pF++=arTime[i]+fEvtTime;
          *pF++=arFreq[i];
          *pF++=arAmpl[i];
        }
        nOutLen+=8;  /* packet length in bytes */
        assert(nOutLen==fwrite(OutBuf,sizeof(Uchar),nOutLen,h));
      }/* esle */
    }
    
    p=NextMpLnkLst(p);
  }

return 0;
}



Ulong write_minipacket(FILE *h,MpLnkLst *p)
{
Uchar *pByte;
Ulong nNumRec,nFlength,nLength,nTmp;
size_t n;

  /*   4 bytes - forward_length (bytes in the packet) */
  /*   8 bytes - record status */
  /* 256 bytes - ancillary data header */
  /* nnn bytes - mini-packet length */ 
  /*   4 bytes - reverse (bytes in the packet) */

  
  nNumRec=0;
  p=HeadMpLnkLst(p);
  while(p!=NULL){
    nFlength=p->pCasRec->forward_length;
    assert(fwrite(&nFlength,sizeof(Ulong),1,h)==1);
    nTmp=p->pCasRec->fill0;
    assert(fwrite(&nTmp,sizeof(Ulong),1,h)==1);
    nTmp=p->pCasRec->fill1;
    assert(fwrite(&nTmp,sizeof(Ulong),1,h)==1);

    pByte=(void*)&(p->pCasRec->status.cds_time);
    assert(fwrite(pByte,sizeof(Uchar),256,h)==256);

    nLength=p->pCasRec->status.packet_length+3;
    assert(fwrite(p->pCasRec->data,sizeof(Uchar),nLength,h)==nLength);

    nLength+=256+12; 
    n=nFlength-nLength;
    nTmp=0; 
    if((n=nFlength-nLength) != 0)
      assert(fwrite(&nTmp,sizeof(Uchar),n,h)==n);

    assert(fwrite(&nFlength,sizeof(Ulong),1,h)==1);

    ++nNumRec;
    p=NextMpLnkLst(p);
  }

return nNumRec;
}


void print_packet_report(FILE *h)
{
char sTitle[32];
int i,nIdx;
PdsHsk **pHsk;

  for(i=0;i<5;i++){  
    switch(i){
      case 0:  pHsk=hskLfdr;    sprintf(sTitle,"Lfdr");    break;
      case 1:  pHsk=hskMfdr;    sprintf(sTitle,"Mfdr");    break;
      case 2:  pHsk=hskMfr;     sprintf(sTitle,"Mfr");     break;
      case 3:  pHsk=hskHfr;     sprintf(sTitle,"Hfr");     break;
      case 4:  pHsk=hskHfrMsc;  sprintf(sTitle,"HfrMsc");  break;
      default: assert(0);                                  break;
    }
    nIdx=0;
      fprintf(h,"%s\n",sTitle);
    while(pHsk[nIdx]!=NULL){
      fprintf(h,"  %3d %7d %7d %08X ",nIdx,pHsk[nIdx]->nNumRec,
        pHsk[nIdx]->nRecLen,pHsk[nIdx]->nRecMode);
      fprintf(h,"%s ",CasSpice_nSclk_to_sScet(pHsk[nIdx]->nSecBeg,
        pHsk[nIdx]->nFineBeg,NULL));
      fprintf(h,"%s\n",CasSpice_nSclk_to_sScet(pHsk[nIdx]->nSecEnd,
        pHsk[nIdx]->nFineEnd,NULL));
      ++nIdx;
    }
  }

return;
}



void show_help(void)
{

  fprintf(stderr,
  "\nSYNOPSIS\n"
  "     rpws_lr_lrscal -h\n"
  "     rpws_lr_lrscal -version\n"
  "     rpws_lr_lrscal [-d] [-n NIX] [-lfr MODES] [-mfdr MODES] [-mfr MODES]\n"
  "                    [-hfr MODES] [-msc MODES] [-o pds|das|mpr] [-s] [-r] [-v]\n"
  "                    [-tStart START] [-tStop STOP] [FILE1 FILE2 FILE3 ...]\n"
  );
  fprintf(stderr,"\n");

  fprintf(stderr,
  "OPTIONS\n"
  "     -d        Don't use the minipacket database to find files.\n\n"
  "     -n NIX    Ignore certian record types, may be specified more than once.\n"
  "               Known values for NIX are 'hfr_snd' and 'lp_rswp'\n\n"
  "     -lfr MODES\n"
  "               Select which LFR packets to inclued in the output.  The MODES\n"
  "               value used for PDS data production is 'ExEwBxByBz'.  The string\n"
  "               'EuEv' may be added to get all LFR data.\n\n"
  "     -mfdr MODES\n"
  "               Select which MFDR packets to include in the output.  The MODES\n"
  "               value used for PDS data production is 'ExEwBzByBz'.  The string\n"
  "               'EuEv' may be added to get all MFDR data.\n\n"
  "     -mfr MODES\n"
  "               Select which MFR packets to include in the output.  The MODES\n"
  "               value used for PDS data production is '123ExEwBxBz', which \n"
  "               includes all MFR bands but leaves out the 'Eu' and 'Ev' sensors.\n\n"
  "     -hfr MODES\n"
  "               Select which HFR packets to include in the output.  The MODES\n"
  "               value used for PDS data production is 'ABC12EuEvEwEx', which\n"
  "               includes all bands and all sensors.\n\n"
  "     -msc MODES\n"
  "               HFR Millisecond modes, max selection is '12EuEvExEw'.  By default\n"
  "               these are not included during PDS data creation.\n\n"
  "     -o OUTSTYLE\n"
  "               Select the output format.  Choices are 'pds', 'das', & 'mpr'.  The\n"
  "               The default is to output PDS data files.\n\n"
  "     -r        Report details of the data processing before exit.\n\n"
  "     -v        Be verbose, includes -r as a subset.\n\n"
  "     -s        Be silent\n\n"
  "     -tStart START\n"
  "               All records with a SCET less that this value will no be output.\n"
  "               The default is 1980-001\n\n"
  "     -tStop  STOP\n"
  "               All records with a SCET equal to or greater than this value will\n"
  "               not be output.  The default is 2012-001\n\n"
  "     FILE1 FILE2 FILE3 ...\n"
  "               The input files to process.  If -d is specified, these are required.\n\n"
  );
  
  fprintf(stderr,
  "ENVIRONMENT\n"
  "     RPWS_MPDB is taken to be the name of the minipacket database file.  When\n"
  "     -d is specified, this environment variable is not used.\n\n"
  );

  fprintf(stderr,"%s\n\n",sVersion);
  return;
}

unsigned long AntStrToInt(char **pOpt)
{
char *pStr=*pOpt;
unsigned long nAntenna=0;
  
  pStr=*pOpt;
/* fprintf(stderr,"In pStr=%s,%p\n",pStr,pStr); */
  while(*pStr!='-' && *pStr!='+' && *pStr!='\0'){
    if(*pStr=='E' && *(pStr+1)=='x' && *(pStr+2)=='p'){  
      nAntenna|=CasAntExp;
      pStr+=strlen("Exp");
    }
    else if(*pStr=='E' && *(pStr+1)=='u'){  /* new name for Ex+ */
      nAntenna|=CasAntEu;
      pStr+=strlen("Eu");
    }
    else if(*pStr=='E' && *(pStr+1)=='x' && *(pStr+2)=='m'){  
      nAntenna|=CasAntExm;
      pStr+=strlen("Exm");
    }
    else if(*pStr=='E' && *(pStr+1)=='v'){  /* new name for Ex- */
      nAntenna|=CasAntEv;
      pStr+=strlen("Ev");
    }
    else if(*pStr=='E' && *(pStr+1)=='x' && 
           *(pStr+2)=='A' && *(pStr+3)=='n' && *(pStr+4)=='y'){  
      nAntenna|=CasAntExAny;
      pStr+=strlen("ExAny");
    }
    else if(*pStr=='E' && *(pStr+1)=='x'){  
      nAntenna|=CasAntEx;
      pStr+=strlen("Ex");
    }
    else if(*pStr=='E' && *(pStr+1)=='z'){  
      nAntenna|=CasAntEz;
      pStr+=strlen("Ez");
    }
    else if(*pStr=='E' && *(pStr+1)=='w'){  /* new name for Ez */
      nAntenna|=CasAntEw;
      pStr+=strlen("Ew");
    }
 
    else if(*pStr=='B' && *(pStr+1)=='x'){  
      nAntenna|=CasAntBx;
      pStr+=strlen("Bx");
    }
    else if(*pStr=='B' && *(pStr+1)=='y'){  
      nAntenna|=CasAntBy;
      pStr+=strlen("By");
    }
    else if(*pStr=='B' && *(pStr+1)=='z'){  
      nAntenna|=CasAntBz;
      pStr+=strlen("Bz");
    }
    else{
      ++pStr;
    }
  }

  if(*pStr=='-' || *pStr=='+')
    --pStr;
  *pOpt=pStr;

/* fprintf(stderr,"pStr=%s, nAntenna=%08X\n",pStr,nAntenna); */

return nAntenna;
}


Ulong RcvStrToMode(const char *sRcv,char *sArg)
{
char *p;
Ulong nType=0;

  p=(char*)sArg;  assert(p!=NULL);

  if( !strcmp("lfr",sRcv) || !strcmp("lfdr",sRcv) ){ 
    nType=CasLfdr_Normal;
    while( isalnum((int)(*p)) ){
      switch(*p){
        case 'E':  ++p;
          if(*p=='u')       nType|=CasAntEu;
          else if(*p=='v')  nType|=CasAntEv;
          else if(*p=='x')  nType|=CasAntEx;
          else if(*p=='w')  nType|=CasAntEw;
          else              fprintf(stderr,"invalid lfdr antenna E%c\n",*p);
          break;
        case 'B':  ++p;
          if(*p=='x')       nType|=CasAntBx;
          else if(*p=='y')  nType|=CasAntBy;
          else if(*p=='z')  nType|=CasAntBz;
          else              fprintf(stderr,"invalid lfdr antenna B%c\n",*p);
          break;
        default:  
          fprintf(stderr,"invalid lfdr option %c\n",*p);  
          break;
      }
      ++p;
    }/* elihw */
  }
  else if(!strcmp("mfdr",sRcv)){
    nType=CasMfdr_Normal;
    while( isalnum((int)(*p)) ){
      switch(*p){
        case 'E':  ++p;
          if(*p=='u')       nType|=CasAntEu;
          else if(*p=='v')  nType|=CasAntEv;
          else if(*p=='x')  nType|=CasAntEx;
          else if(*p=='w')  nType|=CasAntEw;
          else              fprintf(stderr,"invalid mfdr antenna E%c\n",*p);
          break;
        case 'B':  ++p;
          if(*p=='x')       nType|=CasAntBx;
          else if(*p=='y')  nType|=CasAntBy;
          else if(*p=='z')  nType|=CasAntBz;
          else              fprintf(stderr,"invalid mfdr antenna B%c\n",*p);
          break;
        default:
          fprintf(stderr,"invalid mfdr option %c\n",*p);
          break;
      }
      ++p;
    }/* elihw */
  }
  else if(!strcmp("mfr",sRcv)){
    nType=CasMfr_AnyMode;                   /* Normal and Fast Toggle */
    nType=CasMfr_Normal|CasMfr_FastToggle;  /* Normal and Fast Toggle */
    while( isalnum((int)(*p)) ){
      switch(*p){
        case '1':  nType|=CasMfr_Band1;  break;
        case '2':  nType|=CasMfr_Band2;  break;
        case '3':  nType|=CasMfr_Band3;  break;
        case 'E':  ++p;
          if(*p=='u')       nType|=CasAntEu;
          else if(*p=='v')  nType|=CasAntEv;
          else if(*p=='x')  nType|=CasAntEx;
          else if(*p=='w')  nType|=CasAntEw;
          else              fprintf(stderr,"invalid mfr antenna E%c\n",*p);
          break;
        case 'B':  ++p;
          if(*p=='x')       nType|=CasAntBx;
          else if(*p=='z')  nType|=CasAntBz;
          else              fprintf(stderr,"invalid mfr antenna B%c\n",*p);
          break;
        default:
          fprintf(stderr,"invalid mfr option %c\n",*p);
          break;
      }
      ++p;
    }/* elihw */
  }
  else if(!strcmp("hfr",sRcv)){  /* hfr analysis mode */
    nType=CasHfr_Analysis;
    while( isalnum((int)(*p)) ){
      switch(*p){
        case 'A':  nType|=CasHfr_BandA;    break;
        case 'B':  nType|=CasHfr_BandB;    break;
        case 'C':  nType|=CasHfr_BandC;    break;
        case '1':  nType|=CasHfr_BandHF1;  break;
        case '2':  nType|=CasHfr_BandHF2;  break;
        case 'E':  ++p;
          if(*p=='u')       nType|=CasAntEu;
          else if(*p=='v')  nType|=CasAntEv;
          else if(*p=='x')  nType|=CasAntEx;
          else if(*p=='w')  nType|=CasAntEw;
          else              fprintf(stderr,"invalid hfr antenna E%c\n",*p);
          break;
        default:
          fprintf(stderr,"invalid hfr option %c\n",*p);
          break;
      }
      ++p;
    }/* elihw */
  }
  else if(!strcmp("msc",sRcv)){  /* hfr millisecond mode */
    nType=CasHfr_Millisecond;
    while( isalnum((int)(*p)) ){
      switch(*p){
        case '1':  nType|=CasHfr_BandHF1;  break;
        case '2':  nType|=CasHfr_BandHF2;  break;
        case 'E':  ++p;
          if(*p=='u')       nType|=CasAntEu;
          else if(*p=='v')  nType|=CasAntEv;
          else if(*p=='x')  nType|=CasAntEx;
          else if(*p=='w')  nType|=CasAntEw;
          else              fprintf(stderr,"invalid msc antenna E%c\n",*p);
          break;
        default:
          fprintf(stderr,"invalid msc option %c\n",*p);
          break;
      }
      ++p;
    }/* elihw */
  }
  else{
    assert(0);
    exit(1);
  }

return nType;
}
