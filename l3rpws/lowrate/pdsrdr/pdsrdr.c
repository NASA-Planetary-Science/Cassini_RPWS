/*
                        pdsrdr.c
                     Robert Johnson
                     December 9, 2002

  This program pdsrdr() reads the data files produced for pds archiving and 
output the format in triplicate form (time,frequency,magnitude) to das.
Version 1.0 is a quick and dirty attempt at parsing the crude label files,
with emphasis on the frequency component only.

December 9, 2002 version 1.0
  Parse label files for the frequency and generate a crude event time from
  the SCLK using the standard unix utilities.  There should be a time 
  regression of about 6 minutes in 1999 to approximately 12 minutes in 2002.
  The proper fix for this is the incorporation of the SCLK/SCET file or the
  SCLK/SCET pairs in the SFDU packets.

December 18, 2002 version 1.1
  - increase data buffer from 4MB to 32MB to accomodate HFR Freeze Mode, 17MB

Janurary 2, 2002 version 1.2
  Begin to implement the a reader for the new file format
  Need to address the b0 limitaion - with internal write b0 packet stuff

  -> Parse Header Function - determine the type of CORPWS header
  -> Read Time,Read Frequency, Read Data
  -> Flags for the data types, nix mfr sounder etc. sub mfdr

Feburary xx
  Filter mfr by bands for mfdr replacement

March 6, 2003
  Fix mfr filter by band, subroutine assumes wrong index for mfr fast toggle
  mode.

April 23, 2003
  Implement Background Division

June 10, 2003 version 1.5
  modify command args to accept Eu,Ev,Ew the new names for Ex+,Ex-,Ez; which is
    localized to the function AntStrToInt().

June 13, 2003 - bug1 :: assert(0) for 1999-001, comment out for now

Version 1.6
October 10, 2003 :: implement new header format and order, nMode ....

Version 1.65

Version 1.7
December 2, 2003  :: eliminate hfr sounding interference 

Version 1.8
December 30, 2003  :: Perform background division with data far from 
     Jupiter Fly-by.
Janurary 20, 2003  :: Add one to the doy if time span is longer than 24 hrs.
  pds_get_file() sees 2003-314 12 to 2003-316 12 as 2003-314 to 2003-316 
  instead of 317.

Version 1.9 - 2004-02-12 :: 
  fix mfr fast toggle mode problem 
  implement file searching system 
  fix mfdr separating real and fake data into different files

Version 2.0
  2.01 Feb 20, 2004
  fix file list for days with 0 hr,min,sec - multiple days

Version 2.1
  Feb 23, 2004
    Implement dasII shim 

Version 2.2
  March 22, 2004
    2003-223 to 2003-325 pdsrdr.c:991: failed assertion `0' in 
    CasPdsFileList* CasPds_LRfull_SortFileList(char *sList[]), assume all
    lfdr packets are the same.  Fails for different antennas (lfdr,mfdr,mfr).
    
Version 2.2a
  March 26, 2004
    fEvtTime failed for 51 day plots: ulong integral time offset.

Version 2.3
  April 28, 2004
    Implement a command line option to calculate the background for integral 
    days, not just the command line time span.

Version 2.31
  xxx xx, 2004
    ???

Version 2.4
  May 27, 2004
    Read new zero fill information from the data quality header ???.
  December 2, 2004
    Command line options to undo base capacitance and antenna length.

Version 2.5
  Thursday, Janurary 6, 2005
    Command line options to pitch bad packets and lfdr/mfdr packets where the
    digital gain factor is zero.

Version 2.6
	2012-10-23 C. Piker
      Switch to C99 and stdbool, remove hard coded paths in favor of
		evironment variables.

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

#include <das2/das1.h>

#include <casephem/CasSpice.h>

#include <rpwstlm/CasType.h>
#include <rpwstlm/CasCmdParse.h>
#include <rpwstlm/CasPds.h>

#include "pdsrdr.h"
#include "pdshist.h"



/* 
  Files size so far have been approxiamately 40MB.
  Record size is limited by the memory on the Low Rate Processor.
  das b0 records are limited to 0xFFFF hex.
*/


/* command line options  0x87654321  */
#define NIX_FREQUENCIES  0x0000FFFF  /* lfdr      0.19Hz to         24.31Hz */ 
#define NIX_MFR3forHFRA  0x00000001  /* mfdr     13.93Hz to      1,736.86Hz */
#define NIX_HFRAforMFR3  0x00000002  /* mfr1     23.89Hz to        169.00Hz */
#define NIX_HFRCforHF1   0x00000004  /* mfr2    192.11Hz to      1,470.09Hz */
#define NIX_HF1forHFRC   0x00000008  /* mfr3  1,536.89Hz to     11,799.33Hz */
#define NIX_HFRCforHF2   0x00000010  /* hfrA  3,685.61Hz to     15,823.72Hz */
#define NIX_HF2forHFRC   0x00000020  /* hfrB 16,585.23Hz to     71,206.74Hz */
#define NIX_HF1PWR       0x00000040  /* hfrC 74,633.53Hz to    320,430.31Hz */
#define NIX_HF2PWR       0x00000080  /* hf1       0.00Hz to  4,300,000.00Hz */
#define NIX_MFR2forMFDR  0x00000100  /* hf2      25.00Hz to 16,175,000.00Hz */
/* note: bands hf1 & hf2 occasionaly range outside the upper limits.     */ 

#define NIX_DATASETS       0xFFFF0000  /**/
#define NIX_BadData        0x80000000  /**/
#define NIX_HfrSndActive   0x40000000  /**/ 
#define NIX_LpRawSwpActive 0x20000000  /**/
#define NIX_FakeData       0x10000000  /* Data the was produced on the ground */
#define NIX_DpfZero        0x08000000  /**/
/*
#define NIX_ZeroFill       0x08000000  
*/

const char *sVersion="pdsrdr() Version 2.6";



typedef struct daspdsrec_tag{
  struct daspdsrec_tag *next,*prev;    /* stuff for xml */
  char *sFileName;                     /* stuff for xml */
  FILE *h;                             /* stuff for xml */
  unsigned long nNumRec,nRecLen,nFrq;
  float *arFrq;
  unsigned nType,nMode;                /* Type=lfr,mfr,hfr; Mode=ABCEx... */
}dasPdsRec;

/*
typedef struct caspdsfilelist_tag{
  char *sList[1024];
  Uchar nMpHdr[1024][32];
  Ulong nType[1024],nRecLen[1024],nId[1024],nMax;
}CasPdsFileList;
*/

Uchar arPdsFileBuf[CAS_PDS_MAX_FILE_SIZE];  /* 64MB buffer */

CasPdsFileList* CasPds_LRfull_SortFileList(char *sList[]);
/* void CasPds_Dump_LogBinHist(CasHistogram *pHgram); */
Ulong CasPds_LRfull_FrequencyFilter(Uchar *arPds,Ulong *arFrqIdx);


/* Global Command Line Arguments */
bool bSilent=false;
bool bVerbose=false;
Ulong nLfdrMode,nMfdrMode,nMfrMode,nHfrMode,nMscMode;
Ulong nNixFrq,cmdln_nNixBadPkt;
bool bHardSort=false;
bool bBaseCap=true,bAntLen=true;


void delta_event_time(char *sScet,unsigned long *pDays, unsigned long *pFine);
unsigned long AntStrToInt(char **pStr);
unsigned long CmdLn_AntStrToInt(char *pStr); /* new version */
void show_help(FILE *h);


bool bNewDatFile=true;  /* keeps track of when a new data file is opened */

/* ************************************************************************* */
int main(int argc,char *argv[])
{
char sPdsDir[512] = {'\0'};
char *sFiles[PDS_FILE_LIST_MAX];
Ulong nFiles;
float fEvtTime;
double dEvtTime;

char *sBeg,*sEnd;
DasTime tBeg,tEnd;

CasPdsFileList *pFiles;


/* Global Command Line Options */
bool bConglomerate=true;  /* don't sort by like sensors */
bool bCorrectAntennaResonance=false;
bool bCmdLine_BgndDay=false;
bool bCmdLine_NoFill=false;
int nBackGroundDivision=0;  /* file by file for now */
int nAbsoluteBgnd=0; 
Ulong nScetDaysBeg,nScetMsecBeg,nScetDaysEnd,nScetMsecEnd;
Ulong nSensor,nSensorList;

Uchar arDasb0[DAS_B0_PACKET_MAX_SIZE];
Ulong arFrqIdx[CAS_PDS_MAX_RECORD_ITEMS],nMaxFrqIdx;  /* 64K items */

Ulong nSclkSec,nSclkFine,nScetDays,nScetMsec,nDataQuality,nSensorId;
Ulong nRecLen,nNumRec,nFreq,nMode,nRec;
float *pTime,*pFreq,*pAmpl,*pDasb0;
CasHistogram *pHist;

char *pChar;
Uchar *pByte;
int i,nIdx;
Ulong nDasLen;
Ulong *pDword;
/* void *pVoid; */

/* housekeeping */
clock_t tProcTime;
time_t tElaspedTime;
Ulong nWrote,nBytesRead,nBytesWrote;


  tProcTime=clock();
  tElaspedTime=time(NULL);

  nLfdrMode=nMfdrMode=nMfrMode=nHfrMode=nMscMode=0;
  nScetDaysBeg=nScetMsecBeg=0;  nScetDaysEnd=nScetMsecEnd=UINT_MAX;
  nNixFrq=0;  cmdln_nNixBadPkt=0;

  sBeg=malloc(32);  strcpy(sBeg,"1992-355T00:00");
  sEnd=malloc(32);  strcpy(sEnd,"1992-355T00:00");

  /* Removed the default, the is no hard coded path, environment variable 
     must be set, or else the command line option must be used. -cwp */
  
  if((pChar=getenv("RPWS_SUPERVOL"))!=NULL)
    strcpy(sPdsDir,pChar);

  nFiles=0; 
  while(--argc){
    ++argv;
    if(!strcmp("-a",*argv)){
      bCorrectAntennaResonance=true;
    }
    else if(!strcmp("-b",*argv)){
      if( (argc>1) && (**(argv+1)!='-') )
        {--argc;  ++argv;  nBackGroundDivision=strtol(*argv,NULL,0);}
      else
        nBackGroundDivision=3;
    }
    else if(!strcmp("-bgm",*argv)){
      --argc;  ++argv;  
      nAbsoluteBgnd=strtol(*argv,NULL,0);
    }
    else if(!strcmp("-bgday",*argv)){
      bCmdLine_BgndDay=true;
    }
    else if(!strcmp("-conglomerate",*argv))
      bConglomerate=false;              /* don't group all antennas together */
    else if(!strcmp("-d",*argv)){
      --argc;  ++argv;
      strncpy(sPdsDir,*argv, 511);
    }
    else if(!strcmp("-nofill",*argv))
      bCmdLine_NoFill=true;
    else if(!strcmp("-f",*argv)){      /* all args upto "-" are files */
      while( (argc>1) && (**(argv+1)!='-') ){
        --argc;  ++argv;  sFiles[nFiles++]=*argv;}
    }
    else if( (!strcmp("-h",*argv)) || (!strcmp("-help",*argv)) )
      {show_help(stdout);exit(0);}
    else if(!strcmp("-s",*argv))
      bSilent=true;
    else if(!strcmp("-v",*argv))
      bVerbose=true;
    else if(!strcmp("-ver",*argv) || !strcmp("-version",*argv)){
      fprintf(stderr,"%s\n",sVersion);
      fprintf(stderr,"  %s\n",CasTlm_Version());
    }
    else if(!strcmp("-n",*argv)){     /* nix overlapping frequencies */
      while( (argc>1) && (**(argv+1)!='-') ){
        --argc;  ++argv;
        if(!strcmp("mfdr_mfr2",*argv))       nNixFrq|=NIX_MFR2forMFDR;
        else if(!strcmp("mfr3_hfra",*argv))  nNixFrq|=NIX_MFR3forHFRA;
        else if(!strcmp("hfra_mfr3",*argv))  nNixFrq|=NIX_HFRAforMFR3;
        else if(!strcmp("hfrc_hf1",*argv))   nNixFrq|=NIX_HFRCforHF1;
        else if(!strcmp("hf1_hfrc",*argv))   nNixFrq|=NIX_HF1forHFRC;
        else if(!strcmp("hfrc_hf2",*argv))   nNixFrq|=NIX_HFRCforHF2;
        else if(!strcmp("hf2_hfrc",*argv))   nNixFrq|=NIX_HF2forHFRC;
        else if(!strcmp("hf1pwr",*argv))     nNixFrq|=NIX_HF1PWR;
        else if(!strcmp("hf2pwr",*argv))     nNixFrq|=NIX_HF2PWR;
        else if(!strcmp("bad_data",*argv))    
          cmdln_nNixBadPkt|=NIX_BadData;
        else if(!strcmp("hfr_snd",*argv))    
          cmdln_nNixBadPkt|=NIX_HfrSndActive;
        else if(!strcmp("lp_rswp",*argv))    
          cmdln_nNixBadPkt|=NIX_LpRawSwpActive;
        else if(!strcmp("fake_data",*argv))    
          cmdln_nNixBadPkt|=NIX_FakeData;
        else if(!strcmp("dpf_zero",*argv))    
          cmdln_nNixBadPkt|=NIX_DpfZero;
/*
        else if(!strcmp("zero_fill",*argv))    
          cmdln_nNixBadPkt|=NIX_ZeroFill;
*/
        else if(!strcmp("bascap",*argv))    
          bBaseCap=false;
        else if(!strcmp("antlen",*argv))    
          bAntLen=false;
        else fprintf(stderr,"Invalid nix option: %s.\n",*argv);
      }
    }
    else if(!strcmp("-tStart",*argv)){
      --argc;  ++argv;
      sBeg=*argv;
    }
    else if(!strcmp("-tStop",*argv)){
      --argc;  ++argv;
      sEnd=*argv;
    }
    else if(!strcmp("-lfdr",*argv)){
      --argc;  ++argv;
      nLfdrMode=CasCmd_RcvStrToMode("lfdr",*argv);
    }/* else lfdr */
    else if(!strcmp("-mfdr",*argv)){
      --argc;  ++argv;
      nMfdrMode=CasCmd_RcvStrToMode("mfdr",*argv);
    }/* else mfdr */
    else if(!strcmp("-mfr",*argv)){
      --argc;  ++argv;
      nMfrMode=CasCmd_RcvStrToMode("mfr",*argv);
    }/* else mfr */
    else if(!strcmp("-hfr",*argv)){
      --argc;  ++argv;
      nHfrMode=CasCmd_RcvStrToMode("hfr",*argv);
    }/* else hfr */
    else if(!strcmp("-msc",*argv)){
      --argc;  ++argv;
      nMscMode=CasCmd_RcvStrToMode("msc",*argv);
    }/* else hfrmsc */
    else{
      fprintf(stderr,"Invalid option %s.\n",*argv);
    }
  }/* while parsing command line argurments */


  /* make sure sPdsDir was set */
  if(sPdsDir[0] == '\0'){
		fprintf(stderr, "ERROR: Supervolume not specified, either set "
		        "RPWS_SUPERVOL or used the -d command line option.\n");
		return 13;
  }
  

  /* init some things */
  CasSpice_Init(NULL);
  CasCmd_ParseTime(sBeg,&tBeg);
  CasCmd_ParseTime(sEnd,&tEnd);


  /* plot time interval */
  nScetDaysBeg=tBeg.nDays;  nScetMsecBeg=tBeg.nMsec;
  nScetDaysEnd=tEnd.nDays;  nScetMsecEnd=tEnd.nMsec;

  /* background division time interval */
  if(bCmdLine_BgndDay==true){
    nBgdScetDaysBeg=tBeg.nDays;  nBgdScetMsecBeg=0;
    if(tEnd.nMsec==0)  nBgdScetDaysEnd=tEnd.nDays;
    else               nBgdScetDaysEnd=tEnd.nDays+1;
    nBgdScetMsecEnd=0;
  }
  else{
    nBgdScetDaysBeg=tBeg.nDays;  nBgdScetMsecBeg=tBeg.nMsec;
    nBgdScetDaysEnd=tEnd.nDays;  nBgdScetMsecEnd=tEnd.nMsec;
  }

  if(bSilent==false){
    fprintf(stderr,"Plot Time: %s, Sclk=%08X.%04X, Scet=%04X.%08X\n",
      tBeg.sScet,tBeg.nSclk,tBeg.nFine,nScetDaysBeg,nScetMsecBeg);
    fprintf(stderr,"           %s, Sclk=%08X.%04X, Scet=%04X.%08X\n",
      tEnd.sScet,tEnd.nSclk,tEnd.nFine,nScetDaysEnd,nScetMsecEnd);
    fprintf(stderr,"Bgd  Time: %.17s, %19s Scet=%04X.%08X\n",
      CasSpice_nScet_to_sScet(nBgdScetDaysBeg,nBgdScetMsecBeg,NULL),
      " ",nBgdScetDaysBeg,nBgdScetMsecBeg);
    fprintf(stderr,"           %.17s, %19s Scet=%04X.%08X\n",
      CasSpice_nScet_to_sScet(nBgdScetDaysEnd,nBgdScetMsecEnd,NULL),
      " ",nBgdScetDaysEnd,nBgdScetMsecEnd);

    fprintf(stderr,"Lfdr=%08X, Mfdr=%08X, Mfr=%08X, Hfr=%08X, Msc=%08X\n",
            nLfdrMode,nMfdrMode,nMfrMode,nHfrMode,nMscMode);

    fprintf(stderr,"filtering options=%08X, Bad Packets=%08X\n",nNixFrq,
            cmdln_nNixBadPkt);
    fprintf(stderr,"bConglomerate=%s, nBackGroundDivision=%d\n",
           bConglomerate==true?"True":"False",nBackGroundDivision);
    fprintf(stderr,"pds root dir=%s\n",sPdsDir);
  }


  nSensor=0;
  strcat(sPdsDir,"/DATA/RPWS_LOW_RATE_FULL");
  if(nFiles==0){  /* build null terminated file list */
    nFiles=CasPds_LRfull_GetFiles(sFiles,tBeg.sScet,tEnd.sScet,sPdsDir,
                         nLfdrMode,nMfdrMode,nMfrMode,nHfrMode,nMscMode);
    assert(nFiles<PDS_FILE_LIST_MAX);
  }
  if(bVerbose==true){
    fprintf(stderr,"Number of Files=%d\n",nFiles);
    for(i=0;i<nFiles;i++)
      fprintf(stderr,"%s\n",sFiles[i]);
  }
  if(nFiles==0){
    fprintf(stderr,"No files found for the time period:\n");
    fprintf(stderr,"  %s to %s\n",tBeg.sScet,tEnd.sScet);
    exit(1);
  }
  pFiles=CasPds_LRfull_SortFileList(sFiles);
  if(pFiles==NULL){
    fprintf(stderr,"lost the files, pFiles=%p\n",pFiles);
    exit(1);
  } 


/* errors in the hfr data */
  nFiles=0;  nBytesRead=nBytesWrote=0;
  while(pFiles->sList[nFiles]!=NULL){

/* 
  at this point, there should be a time ordered list of files for
each of the logical instruments with a mode mask, for now assiciate
one antenna for all receivers.
*/



/*
typedef struct caspdsfilelist_tag{
  char *sList[1024];
  Uchar nMpHdr[1024][32];
  Ulong nType[1024],nRecLen[1024],nId[1024],nMax;
}CasPdsFileList;
*/

    /* calculate background across arbitrary interval */
    if(nBackGroundDivision){
    static int nOldId;
      if(bVerbose)  fprintf(stderr,"performing background division %d\n",
                            nBackGroundDivision);
      if(nOldId!=pFiles->nId[nFiles]){
        if((nAbsoluteBgnd==1) && /* Galactic Background Noise, Jupiter fly-by */
           (CasHfr_Analysis==(pFiles->nType[nFiles]&CasRcv_Mask)) ){
        }
        else{
          CasPds_LRfull_LogBinHist(pFiles,nFiles,nBackGroundDivision);
          /* CasPds_Dump_LogBinHist(pHstEx);*/
        }
        nOldId=pFiles->nId[nFiles];
      }
    }/* fi background division */

    if(CasPds_LRfull_ReadArchive(pFiles->sList[nFiles],arPdsFileBuf)==NULL){
      fprintf(stderr,"CasPds_LRfull_ReadArchive(%s) failed.\n",
              pFiles->sList[nFiles]);
      exit(1); 
    }
    if(bVerbose==true)  
      fprintf(stderr,"%3d data read %s\n",nFiles,pFiles->sList[nFiles]);

    /* assign pointers to stuff */
/*
    pByte=arPdsFileBuf+8;
    nRecLen=*((Ulong*)pByte);  pByte+=4;  
    nNumRec=*((Ulong*)pByte);  pByte+=4;
*/
	 /* Get's the first three words in little endian if needed */
	 swapBufIfHostLE(arPdsFileBuf + 8, 4, 3);

    pDword=(void*)(arPdsFileBuf+8);  /* CORPWS01  bytes 0-7 */
    nRecLen=*pDword++;               /* record length bytes 8-11 */
    nNumRec=*pDword++;               /* number of records bytes 12-15 */
    nMode=*pDword;                   /* mode 16-19, no increment */
    pByte=(void*)pDword;
    pTime=(void*)(pByte+1*nRecLen);  /* start of time skipping sclk/scet */
    pFreq=(void*)(pByte+2*nRecLen);  /* start of freq skipping sclk/scet */
    nFreq=(nRecLen-16)/4;            /* number of data points in the record */
	 
	 /* Convert times and frequencies to little endian if needed */
	 swapBufIfHostLE(pTime, 4, nFreq);
	 swapBufIfHostLE(pFreq, 4, nFreq);

/*    nMode=*((Ulong*)&arPdsFileBuf[16]); */
    nSensorList=nMode&CasAntMask;
    switch(nMode&CasRcv_Mask){
      case CasLfdr_Normal:      nSensorList&=nLfdrMode;  break;
      case CasMfdr_Normal:      nSensorList&=nMfdrMode;  break;
      case CasMfr_Normal:
      case CasMfr_FastToggle:   nSensorList&=nMfrMode;   break;
      case CasHfr_Analysis:     nSensorList&=nHfrMode;   break;
      case CasHfr_Millisecond:  nSensorList&=nMscMode;   break;
      default:  assert(0);
    }

    nBytesRead+=(nNumRec*nRecLen);
    
    if(0 && bVerbose==true){
      fprintf(stderr,"dOut rd...%s.\n",pFiles->sList[nFiles]);
      fprintf(stderr,"  nRecLen=%08X (%d), nNumRec=%08X (%d)\n",
              nRecLen,nRecLen,nNumRec,nNumRec);
      fprintf(stderr,"  nMode=%08X, nSensorList=%08X\n",
              nMode,nSensorList);    
    }


    while(nSensorList){

      if(bConglomerate==true)          nSensor=nSensorList;
      else if(nSensorList&CasAntEu)    nSensor=CasAntEu;
      else if(nSensorList&CasAntEv)    nSensor=CasAntEv;
      else if(nSensorList&CasAntEx)    nSensor=CasAntEx;
      else if(nSensorList&CasAntEz)    nSensor=CasAntEz;
      else if(nSensorList&CasAntBx)    nSensor=CasAntBx;
      else if(nSensorList&CasAntBy)    nSensor=CasAntBy;
      else if(nSensorList&CasAntBz)    nSensor=CasAntBz;
      else if(nSensorList&CasAntLMRp)  nSensor=CasAntLMRp;
      else if(nSensorList&CasAntLMRm)  nSensor=CasAntLMRm;
      else if(nSensorList&CasAntLPs)   nSensor=CasAntLPs;
      else if(nSensorList&CasAntHF)    nSensor=CasAntHF;
      else assert(0);
  

      /* filter frequencies by receiver bands and options */
      nMaxFrqIdx=CasPds_LRfull_FrequencyFilter(arPdsFileBuf,arFrqIdx);

      if(bVerbose==true){/* raj */
        fprintf(stderr,"filter by band nMaxFrqIdx=%d\n",nMaxFrqIdx);
        for(i=0;i<nMaxFrqIdx;i++)
          fprintf(stderr,"%4d %4d %13.2f\n",i,arFrqIdx[i],pFreq[arFrqIdx[i]]);
      }

      pByte=arPdsFileBuf+2*nRecLen;  /* next record is the first data record */
      for(nRec=3;nRec<nNumRec;++nRec){

        pByte+=nRecLen;                /* start of the next data record */
		  
		  swapBufIfHostLE(pByte, 4, 1);   /* Swap: Sclk */
		  swapBufIfHostLE(pByte+6, 2, 1); /* Swap: Scet-day */
		  swapBufIfHostLE(pByte+8, 4, 2); /* Swap: Scet-milli, Data Quality */
		  
        pDword=(void*)pByte;
        nSclkSec= *pDword++;               /* nSclkSec bytes 0-3 */
        nSclkFine= pByte[5];               /* nSclkFine byte 5 */
		  nScetDays = *((Ushort*)(pByte+6)); /* nScetDays bytes 6-7  */ 
		  
		  pDword++;  
        nScetMsec     = *pDword++;      /* nScetMsec bytes 8-11 */
        nDataQuality  = *pDword++;
		  
		  /* Sensor Id lower nibble of byte 15 */
        nSensorId     = nDataQuality & 0x0000000F;
        nDataQuality &= 0xFFFFFFF0;    /* Data Quality bytes 12 - 14 */
			 
        pAmpl=(float*)(pDword);        /* density record starts at byte 16 */
		  
		  swapBufIfHostLE(pAmpl, 4, nFreq);  /* Swap: Data records */

        switch(nSensorId){             /* transform the bit field */
          case  0:  nSensorId=CasAntEx;   break;
          case  1:  nSensorId=CasAntEu;   break;
          case  2:  nSensorId=CasAntEv;   break;
          case  3:  nSensorId=CasAntEz;   break;
          case  4:  nSensorId=CasAntBx;   break;
          case  5:  nSensorId=CasAntBy;   break;
          case  6:  nSensorId=CasAntBz;   break;
          case  8:  nSensorId=CasAntHF;   break;
          case 11:  nSensorId=CasAntLPs;  break;
          default:  nSensorId=0x00;       break;
        }      

        /* filter by sensor, allow for any combination of sensors */
        if(!(nSensorId&nSensor))  continue;

        /* filter by scet */ 
        if( nScetDays < nScetDaysBeg )       continue;
        else if( nScetDays > nScetDaysEnd )  continue;
        else if( (nScetDays==nScetDaysBeg) &&
                 (nScetMsec<nScetMsecBeg) )  continue;
        else if( (nScetDays==nScetDaysEnd) &&
                 (nScetMsec>nScetMsecEnd) )  continue;
        else if(nScetDays==nScetDaysBeg){  /* same day */
          dEvtTime=nScetMsec-nScetMsecBeg;
        }
        else if(nScetDays==nScetDaysEnd){  /* time spans at least two days */
          dEvtTime=24*60*60*1000.0-nScetMsecBeg;                 /* first day */
          dEvtTime+=(nScetDays-(nScetDaysBeg+1))*24*60*60*1000.0;/* mid days */
          dEvtTime+=nScetMsec;                                  /* last day */
        }
        else{
          dEvtTime=24*60*60*1000.-nScetMsecBeg;                 /* first day */
          dEvtTime+=(nScetDays-(nScetDaysBeg+1))*24*60*60*1000.0;/* mid days */
          dEvtTime+=nScetMsec;                                  /* today */
        }
        dEvtTime/=1000.0;
        fEvtTime=dEvtTime;

        /* bad packets and digital prescale factor = zero are not sources of 
           interference and therefore, are not marked in the data set.  If 
           cmdln, skip the entire record */
        if( (nDataQuality&cmdln_nNixBadPkt) & (NIX_BadData|NIX_DpfZero) )
          continue;

        /* filter out bad packets, hfr sounder, lp raw sweep */
        /* bad data is marked by -1, ie < 0. */
        if(nDataQuality){  /* (nDataQuality&NIX_DATASETS) */
          if(!(nDataQuality&cmdln_nNixBadPkt)){/* data to keep */
            for(i=0;i<nMaxFrqIdx;i++){
              nIdx=arFrqIdx[i];
              if(pAmpl[nIdx]<0.0)  pAmpl[nIdx]*=-1.0;
            }
          }
        }/* fi data quality flag */


        if(0 && bVerbose==true){
        int hour,min,sec,msc;
          msc=nScetMsec;
          hour=msc/(1000*60*60);  msc-=(hour*1000*60*60);
          min= msc/(1000*60);     msc-= (min*1000*60);
          sec= msc/(1000);        msc-= (sec*1000);
          fprintf(stderr,"%3d sclk=%08X %04X, scet=%04X %08X "
                 "(%02d:%02d:%02d.%03d), ant=%03X\n",nRec,nSclkSec,nSclkFine,
                 nScetDays,nScetMsec,hour,min,sec,msc,nSensorId);

          fprintf(stderr,"   evtm=%.10E\n",fEvtTime);

        }

        /* actual part of the work loop */
        if( ((bCorrectAntennaResonance==false) || (nAbsoluteBgnd>0)) && 
            ((nMode&CasRcv_Mask)==CasHfr_Analysis) ){ 
        float fResCor;
          if(nSensorId&CasAntEx){
            for(i=0;i<nMaxFrqIdx;i++){
              nIdx=arFrqIdx[i];
              fResCor = 1.0-pFreq[nIdx]*pFreq[nIdx]/((8.775e6)*(8.775e6));
              fResCor = (1.0+0.58/27.49)/(fResCor*fResCor + (0.58/27.49) );
              pAmpl[nIdx] *= fResCor;/* multiply by factor instead of divide */
            }
          }
          else{
            for(i=0;i<nMaxFrqIdx;i++){
              nIdx=arFrqIdx[i];
              fResCor=1.0-pFreq[nIdx]*pFreq[nIdx]/((9.575e6)*(9.575e6));
              fResCor=(1.0+(0.1255*0.1255))/(fResCor*fResCor+(0.1255*0.1255));
              pAmpl[nIdx] *= fResCor;/* multiply by factor instead of divide */
            }
          } 
          /* pFreq is in Hz and pAmpl is in v^2 units 
          Dipole
            ant_corr = 1.0-fcenter*fcenter/((8.75e6)*(8.75e6));
            ant_corr = (1.0+0.58/27.49)/(ant_corr*ant_corr + (0.58/27.49) );
          Monopole
            ant_corr=1.0-fcenter*fcenter/((9.36e6)*(9.36e6));
            ant_corr=(1.0+(0.1255*0.1255))/(ant_corr*ant_corr+(0.1255*0.1255));
           */
        }
       
        /* december 3, 2004 */
        if(bBaseCap==false){
          if(nSensorId&CasAntEx){
            for(i=0;i<nMaxFrqIdx;i++){
              nIdx=arFrqIdx[i];
              pAmpl[nIdx] /= (2.49459 * 2.49459);
            }
          }
          else{
            for(i=0;i<nMaxFrqIdx;i++){
              nIdx=arFrqIdx[i];
              pAmpl[nIdx] /= (2.51478 * 2.51478);
            } 
          } 
        }

        if(bAntLen==false){
          if(nSensorId&CasAntEx){
            for(i=0;i<nMaxFrqIdx;i++){
              nIdx=arFrqIdx[i];
              pAmpl[nIdx] *= (9.26 * 9.26);
            }
          }
          else{
            for(i=0;i<nMaxFrqIdx;i++){
              nIdx=arFrqIdx[i];
              pAmpl[nIdx] *= (5.0 * 5.0);
            } 
          } 
        }

        if(nBackGroundDivision){
          switch(nSensorId){
            case CasAntEu:  pHist=pHstEu;  break;
            case CasAntEv:  pHist=pHstEv;  break;
            case CasAntEx:  pHist=pHstEx;  break;
            case CasAntEz:  pHist=pHstEz;  break;
            case CasAntBx:  pHist=pHstBx;  break;
            case CasAntBy:  pHist=pHstBy;  break;
            case CasAntBz:  pHist=pHstBz;  break;
            case CasAntHF:  pHist=pHstHf;  break;
            case CasAntLPs: pHist=pHstLs;  break;
            default: assert(0);  break;
          } 
          assert(nFreq==pHist->nItems);
          for(i=0;i<nFreq;i++){
            pAmpl[i]*=pHist->arBgnd[i];
          }

        }



        if(bCmdLine_NoFill==true){  /* write b0 packets to das1 */
          pChar=(char*)arDasb0;     /* beginning of b0 buffer ":b0:XXXX" */
          pDasb0=(void*)(pChar+8);  /* (float*) beginning of triplicates */
          nDasLen=0;
          for(i=0;i<nMaxFrqIdx;i++){
            nIdx=arFrqIdx[i];
            if(pAmpl[nIdx]<0.0)  continue;  /* delete data for das 1 */
            *pDasb0++=fEvtTime;             /* pDasb0++=pTime[nIdx]+fEvtTime */
            *pDasb0++=pFreq[nIdx]; 
            *pDasb0++=pAmpl[nIdx];
            ++nDasLen;
          }
        }/* fi das1 */
        else{                       /* write b0 packets to das2 */
          pChar=(char*)arDasb0;     /* beginning of b0 buffer ":b0:XXXX" */
          pDasb0=(void*)(pChar+8);  /* (float*) beginning of triplicates */
          nDasLen=0;
          for(i=0;i<nMaxFrqIdx;i++){
            nIdx=arFrqIdx[i];
            if(pAmpl[nIdx]<0.0)  pAmpl[nIdx]=1E-30;  /* fill values for das2 */
            *pDasb0++=fEvtTime;             /* pDasb0++=pTime[nIdx]+fEvtTime */
            *pDasb0++=pFreq[nIdx]; 
            *pDasb0++=pAmpl[nIdx];
            ++nDasLen;
          }
        }/* esle das2 */

        if(nDasLen>0){  /* trap zero length packets */
        char sTmp[32];
          nDasLen*=3*sizeof(float);
          sprintf(sTmp,":b0:%04X",nDasLen);
          strncpy(pChar,sTmp,8);
          nDasLen+=8;                   /* eight bytes of header */
          assert( nDasLen < 0x010000 );
			 
			 /* Swap to big endian to write the das1 stream */
#ifdef HOST_IS_LSB_FIRST
			 if((nDasLen-8) % 4 != 0){
				fprintf(stderr, "ERROR: can't byte swap output, write length, %d, "
						  "is not a multiple of 4", nDasLen);
				return 13;
			 }
#endif
			 swapBufIfHostLE(arDasb0 + 8, 4, (nDasLen / 4) - 2 );
			 
          nWrote=fwrite(arDasb0,sizeof(char),nDasLen,stdout);
			 /* Flush after each packet */
			 fflush(stdout);
          if(nWrote != nDasLen){
            fprintf(stderr,"nWrote != nDasLen (%d != %d)\n",nWrote,nDasLen);
            fprintf(stderr,"Total Bytes = %d\n",nBytesWrote+nWrote);
          }
          assert(nWrote==nDasLen);
          nBytesWrote+=nWrote;
        }/* fi zero length packets */

      }/* for processing each record from the archive file */
      nSensorList&=~nSensor;
    }/* end of antenna */
    ++nFiles;
  }/* while reading file list */
  fflush(stdout);

  fprintf(stderr,"Bytes Read %d and Wrote %d to idl\n",
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


/* ************************************************************************* */
unsigned long CmdLn_AntStrToInt(char *sArg)
{
char *p=sArg;
unsigned long nAntenna=0;

  while(*p!='\0'){
    if(p[0]=='E' && p[1]=='u'){       /* Ex+ monopole */
      nAntenna|=CasAntEu;  p+=2;
    }
    else if(p[0]=='E' && p[1]=='v'){  /* Ex- monopole */
      nAntenna|=CasAntEv;  p+=2;
    }
    else if(p[0]=='E' && p[1]=='x'){  /* Ex dipole  */
      nAntenna|=CasAntEx;  p+=2;
    }
    else if(p[0]=='E' && p[1]=='w'){  /* Ew monopole  */
      nAntenna|=CasAntEw;  p+=2;
    }
    else if(p[0]=='B' && p[1]=='x'){  /* Bx search coil */
      nAntenna|=CasAntBx;  p+=2;
    }
    else if(p[0]=='B' && p[1]=='y'){  /* By search coil */
      nAntenna|=CasAntBy;  p+=2;
    }
    else if(p[0]=='B' && p[1]=='z'){  /* Bz search coil */
      nAntenna|=CasAntBz;  p+=2;
    }
    else{
      ++p;
    }
  }

return nAntenna;
}


/* ************************************************************************* */
void show_help(FILE *h)
{

  fprintf(h,"\n%s\n\n",sVersion);
  fprintf(h,
" -a         Correct for the antenna resonance in hf2\n"
" -b N       Background division using the Nth percentile as the background\n"
" -c         process all sensors as one big conglomeration;  this option is\n"
"              incompatable with background division.\n"
"  -f FILES   read the archives named in files\n"
"  -n OPTIONS eliminate a range of frequencies in a particular receiver\n"
"               OPTIONS:\n"
"                 mfdr_mfr2 - keep only the mfdr frequencies which are\n"
"                   within the bandwidth of mfr band 2.\n"
"                 mfr3_hfra - nix the mfr band 3 frequencies which conflict\n"
"                   with hfr band A.\n"
"                 hfra_mfr3 - nix the hfr band A frequencies which conflict\n"
"                   with mfr band 3.\n"
"                 hfrc_hf1 - nix the hfr band C frequencies which conflict\n"
"                   with hfr band HF1.\n"
"                 hf1_hfrc - nix the hfr band hf1 frequencies which conflict\n"
"                   with hfr band C.\n"
"                 hfrc_hf2 - nix the hfr band C frequencies which conflict\n"
"                   with hfr band HF2.\n"
"                 hf2_hfrc - nix the hfr band HF2 frequencies which conflict\n"
"                   with hfr band C.\n"
"                 hf1pwr - nix the hfr band HF1 frequencies that are power\n"
"                   supply harmonics, 50Khz.\n"
"                 hf2pwr - nix the hfr band HF2 frequencies that are power\n"
"                   supply harmonics, 50Khz.\n"
"                 bad_data - corrupted data packets\n"
"                 hfr_snd - data captured during the hfr sounder\n"
"                 lp_rswp - data captured during the langmuir probe raw sweep\n"
"                 fake_data - data that was constructed on the gound\n"
"                 dpf_zero - lfdr/mfdr data which had a digital prescale \n"
"                            of zero\n"
"                 bascap - nix the antenna base capacitance correction\n"
"                 antlen - nix the antenna length, units are V^2/Hz\n"
"  -s         suppress messages to stderr\n"
"  -v         verbose output of messages\n" 
"  -tStart DATE DATE is a string acceptable to parsetime()\n"
"  -tStop DATE DATE is a string acceptable to parsetime()\n"
"  -lfdr LHExEzBxByBz\n"
"  -mfr 123ExEzBxBz\n"
"  -hfr ABC12EuEvExEz\n"
  );    



return;
}


/* ************************************************************************* */
/* sScet must be = to "2000-153T08:30:36.123" */
void delta_event_time(char *sScet,unsigned long *pDays,unsigned long *pFine)
{
int nYear,nDoy,nHour,nMin,nSec,nMs,i;
unsigned long nDays,nFine;


  nYear=strtol( sScet+0,NULL,10);   /* parse 2002-355T12:32:16.123*/
  nDoy= strtol( sScet+5,NULL,10);   /*       0123456789ABCDEF01234*/
  nHour=strtol( sScet+9,NULL,10);
  nMin= strtol(sScet+12,NULL,10);
  nSec= strtol(sScet+15,NULL,10);
  nMs=  strtol(sScet+18,NULL,10);

  nDays=0;
  for(i=1958;i<nYear;i++){
    if(i%100)                  /* Year is NOT a century year */
      nDays+=(i%4)?365:366;    /* if evenly divisible by 4, leap year */
    else                       /* Year is a century year */
      nDays+=(i%400)?365:366;  /* if evenly divisible by 400, leap year */
  }
  nDays+=(nDoy-1);   /* doy is number 1-365 (366) */

  nFine=0;
  nFine+=nHour*60*60*1000;
  nFine+=nMin*60*1000;
  nFine+=nSec*1000;
  nFine+=nMs;

  if(pDays!=NULL)  *pDays=nDays;
  if(pFine!=NULL)  *pFine=nFine;


return;
}





/* ************************************************************************* */
/*   New Header Format 
  
    8        4        4
  CORPWS01 nRecLen nNumRec
    4      4           8
   nMode nSpare  Mini-Packet Header
           16  
   Mini-Packet Header Cont.
*/
CasPdsFileList* CasPds_LRfull_SortFileList(char *sList[])
{
int i,j,nMax,nIdx,nId;
Ulong *pDword;
CasPdsFileList *p,*q;

Uchar arHdr[128];
Ulong nRecLen,nNumRec,nType,nSpare;
Ulong nRead,nHdrSize=128;
FILE *h;


  if(bVerbose==true)  fprintf(stderr,"sorting file list...");

  nMax=0;
  while(sList[nMax]!=NULL)
    ++nMax;  /* count the items in the list */
 
  if( (p=calloc(1,sizeof(CasPdsFileList)))==NULL ){
    fprintf(stderr,"CasPds_LRfull_SortFileList(), calloc failed.\n");
    assert(0);
    exit(1);
  }

  for(nIdx=0;nIdx<nMax;nIdx++){

    if( (h=fopen(sList[nIdx],"r"))==NULL ){
      fprintf(stderr,"\n   fopen(%s) failed.\n",sList[nIdx]);
      exit(1);
    }
    if( (nRead=fread(arHdr,sizeof(Uchar),nHdrSize,h))!=nHdrSize ){
      fprintf(stderr,"fread(%s) failed, returned %d of %d\n",
                      sList[nIdx],nRead,nHdrSize);
      exit(1);
    }
    fclose(h);

    /* record Length in bytes and number of records */
    assert(strncmp("CORPWS01",(char*)arHdr,8)==0);  /* CORPWS01 */
	 
	 swapBufIfHostLE(arHdr + 8, 4, 3);
	 
    pDword=(void*)(arHdr+ 8);   /* (Ulong*) */
    nRecLen=*pDword++;
    nNumRec=*pDword++;
    nType=*pDword++;
    nSpare=swapFloatIfHostLE(*pDword++);

    p->sList[nIdx]=sList[nIdx]; 
    for(i=0;i<24;i++)
      p->nMpHdr[nIdx][i]=arHdr[i+24]; 
    p->nType[nIdx]=nType;
    p->nRecLen[nIdx]=nRecLen;
    p->nId[nIdx]=0;

    /* raj modify to new format */
    switch(nType&CasRcv_Mask){
      case CasLfdr_Normal:
      case CasMfdr_Normal:
      case CasMfr_Normal:
      case CasMfr_FastToggle:  /* raj 2004-02-12 */
      case CasHfr_Analysis:
      case CasHfr_Millisecond:
        break;
      default:
        fprintf(stderr,"error unknown. nType=%08X\n  ",nType);
        for(i=0;i<24;i++)
          fprintf(stderr,"%02X ",arHdr[i+24]); 
        fprintf(stderr,"\n");
        assert(0);  /* bug1 failed for 1999-001 */
        break;
    }


  }/* for */
  p->sList[nIdx]=NULL;
  p->nMax=nIdx;

  /* hard sort of the file list, according to like data sets */
  for(nIdx=0,nId=0;nIdx<p->nMax;nIdx++){
    if(p->nId[nIdx]==0)  /* unmatched data set */
      p->nId[nIdx]=++nId;
    else                 /* data set is matched already */
      continue;         

    for(i=nIdx+1;i<nMax;i++){  /* look for a possible match */

      if( (p->nId[i]==0) &&                     /* unmatched data set */
          (p->nRecLen[nIdx]==p->nRecLen[i]) &&  /* record lengths the same */
         ((p->nType[nIdx]&CasRcv_Mask)==(p->nType[i]&CasRcv_Mask)) ){
        
        /* check to see if the frequencies are the same */
/*
        nFreq=(p->nRecLen-16)/4;
        for(j=0;j<nFreq;j++){
          if(p->

        }
*/ 
        if(bHardSort==true){
          assert(0);
        }
        else{
          switch(p->nType[i]&CasRcv_Mask){
            case CasLfdr_Normal:  
              p->nId[i]=nId;    /* assume all lfdr packets are the same */
/*
              if( (p->nType[nIdx]&~CasAntMask)!=(p->nType[i]&~CasAntMask) ){
                fprintf(stderr,"SortFiles(): Lfdr modes differ %08X %08X\n",
                        p->nType[nIdx],p->nType[i]);
              }
*/
              break;
            case CasMfdr_Normal:
              p->nId[i]=nId;    /* assume all mfdr packets are the same */
/*
              if( (p->nType[nIdx]&~CasAntMask)!=(p->nType[i]&~CasAntMask) ){
                fprintf(stderr,"SortFiles(): Lfdr modes differ %08X %08X\n",
                        p->nType[nIdx],p->nType[i]);
              }
*/
              break;
            case CasMfr_Normal:   
            case CasMfr_FastToggle:   
              if( (p->nType[nIdx]&CasMfr_BandMask)==
                  (p->nType[i]&CasMfr_BandMask) )
                p->nId[i]=nId;  /* assume either normal or toggle mode */ 
              break;
            case CasHfr_Analysis:
              if(p->nType[i]&CasHfr_BandABC){
/* mode */      if( (p->nMpHdr[nIdx][7]&0x60)==(p->nMpHdr[i][7]&0x60) &&
/* ABC sel*/        (p->nMpHdr[nIdx][9]&0x07)==(p->nMpHdr[i][9]&0x07) &&
/* repeat */        (p->nMpHdr[nIdx][10])==(p->nMpHdr[i][10]) &&
/* rep all*/        (p->nMpHdr[nIdx][13])==(p->nMpHdr[i][13]) &&
/* int,df,filt */   (p->nMpHdr[nIdx][14]&0xE3)==(p->nMpHdr[i][14]&0xE3) )
                  p->nId[i]=nId;  
              }
              else if(p->nType[i]&CasHfr_BandHF1){
/* mode */      if( (p->nMpHdr[nIdx][7]&0x60)==(p->nMpHdr[i][7]&0x60) &&
/* hf1 sel*/        (p->nMpHdr[nIdx][9]&0x08)==(p->nMpHdr[i][9]&0x08) &&
/* repeat */        (p->nMpHdr[nIdx][11])==(p->nMpHdr[i][11]) &&
/* rep all*/        (p->nMpHdr[nIdx][13])==(p->nMpHdr[i][13]) &&
/* int,df,filt */   (p->nMpHdr[nIdx][15]&0xE3)==(p->nMpHdr[i][15]&0xE3) && 
/* start freq */    (p->nMpHdr[nIdx][17])==(p->nMpHdr[i][17]) && 
/* num step */      (p->nMpHdr[nIdx][18])==(p->nMpHdr[i][18]) && 
/* freq step */     (p->nMpHdr[nIdx][19])==(p->nMpHdr[i][19]) )
                  p->nId[i]=nId;  
              }
              else if(p->nType[i]&CasHfr_BandHF2){
/* mode */      if( (p->nMpHdr[nIdx][7]&0x60)==(p->nMpHdr[i][7]&0x60) &&
/* hf1 sel*/        (p->nMpHdr[nIdx][9]&0x10)==(p->nMpHdr[i][9]&0x10) &&
/* repeat */        (p->nMpHdr[nIdx][12])==(p->nMpHdr[i][12]) &&
/* rep all*/        (p->nMpHdr[nIdx][13])==(p->nMpHdr[i][13]) &&
/* int,df,filt */   (p->nMpHdr[nIdx][16]&0xE3)==(p->nMpHdr[i][16]&0xE3) && 
/* start freq */    (p->nMpHdr[nIdx][20])==(p->nMpHdr[i][20]) && 
/* num step */      (p->nMpHdr[nIdx][21])==(p->nMpHdr[i][21]) && 
/* freq step */     (p->nMpHdr[nIdx][22])==(p->nMpHdr[i][22]) &&
/* msbs */          (p->nMpHdr[nIdx][23]&0xC0)==(p->nMpHdr[i][23]&0xC0) )
                  p->nId[i]=nId;  
              }
              else{
                assert(0);
              }
              break;
            case CasHfr_Millisecond:
              p->nId[i]=nId;  /* assume all hfr msc packets are the same */
              break;
            default:  
              fprintf(stderr,"packet type=%08X  masked=%08X\n",
                      p->nType[i],p->nType[i]&CasRcv_Mask);
              assert(0);  
              break;
          }
        }/* else soft sort */

      }/* fi possible match */
    }/* for i, look for a possible match */  
  }/* for nIdx, matched/unmatched data sets */

/*
  for(i=0;i<p->nMax;i++){
    fprintf(stderr,"id=%2ld, %08X, %4d :: ",
            p->nId[i],p->nType[i],p->nRecLen[i]);
    for(j=0;j<16;j++)
      fprintf(stderr,"%02X ",p->nMpHdr[i][j]);
    fprintf(stderr,"\n     ");
    for(j=16;j<25;j++)
      fprintf(stderr,"%02X ",p->nMpHdr[i][j]);
    fprintf(stderr,"\n");
    fprintf(stderr,"     %s\n",p->sList[i]);
  }
*/

/* 
  all data sets should have an id no. greater than zero.  Collect all
  the elements, starting with one and group them together.  Filter with
  the by command line arguments. 
*/
  if( (q=calloc(1,sizeof(CasPdsFileList)))==NULL ){
    fprintf(stderr,"CasPds_LRfull_SortFileList(), calloc failed.\n");
    return NULL;
  }
  nId=0;  q->nMax=0;
  for(nIdx=0,nId=0;nIdx<p->nMax;nIdx++){
    switch(p->nType[nIdx]&CasRcv_Mask){
      case CasLfdr_Normal:
        /* ignore log/lin/fake for now, antenna only */
        if( !((p->nType[nIdx]&nLfdrMode)&CasLfdr_AntMask) )
          p->nId[nIdx]=0;  /* unwanted data set */
        break;
      case CasMfdr_Normal:
        /* ignore log/lin/fake for now, antenna only */
        if( !((p->nType[nIdx]&nMfdrMode)&CasMfdr_AntMask) )
          p->nId[nIdx]=0;  /* unwanted data set */
        break;
      case CasMfr_Normal:
      case CasMfr_FastToggle:
        /* ignore fast toggle/normal, antenna only */
        if( !((p->nType[nIdx]&nMfrMode)&CasMfr_AntMask) )
          p->nId[nIdx]=0;  /* unwanted data set */
        break;
      case CasHfr_Analysis:
        if( !((p->nType[nIdx]&nHfrMode)&CasHfr_AntMask) )
          p->nId[nIdx]=0;  /* unwanted data set */
        if( !((p->nType[nIdx]&nHfrMode)&CasHfr_BandMask) )
          p->nId[nIdx]=0;  /* unwanted data set */
        break;
      case CasHfr_Millisecond:
        if( !((p->nType[nIdx]&nMscMode)&CasHfr_AntMask) )
          p->nId[nIdx]=0;  /* unwanted data set */
        if( !((p->nType[nIdx]&nMscMode)&CasHfr_BandMask) )
          p->nId[nIdx]=0;  /* unwanted data set */
        break;
      default:
        fprintf(stderr,"%s, bad type %08X.\n",p->sList[nIdx],p->nType[nIdx]);
        assert(0);  break;     
    }
    if(p->nId[nIdx]==0)  /* unwanted data set */
      continue; 
    
    /* if(p->nId[nIdx]!=0) */
    nId=p->nId[nIdx];
    q->sList[q->nMax]=p->sList[nIdx];
    for(i=0;i<24;i++)
      q->nMpHdr[q->nMax][i]=p->nMpHdr[nIdx][i];
    q->nType[q->nMax]=p->nType[nIdx];
    q->nRecLen[q->nMax]=p->nRecLen[nIdx];
    q->nId[q->nMax]=p->nId[nIdx];
    q->nMax++;


    for(i=nIdx+1;i<p->nMax;i++){  /* copy to sorted list */
      if(p->nId[i]==nId){  /* match the ids */
        q->sList[q->nMax]=p->sList[i];
        for(j=0;j<24;j++)
          q->nMpHdr[q->nMax][j]=p->nMpHdr[i][j];
        q->nType[q->nMax]=p->nType[i];
        q->nRecLen[q->nMax]=p->nRecLen[i];
        q->nId[q->nMax]=p->nId[i];
        q->nMax++;
        p->nId[i]=0;  /* remove element from the old list */
      }      
    }/* for i=.. */
  }

  if(bVerbose==true){
    fprintf(stderr,"\n");
    for(i=0;i<q->nMax;i++){
      fprintf(stderr,"id=%2d, %08X, %4d :: ",
              q->nId[i],q->nType[i],q->nRecLen[i]);
      for(j=0;j<16;j++)
        fprintf(stderr,"%02X ",q->nMpHdr[i][j]);
      fprintf(stderr,"\n     ");
      for(j=16;j<25;j++)
        fprintf(stderr,"%02X ",q->nMpHdr[i][j]);
      fprintf(stderr,"\n");
      fprintf(stderr,"     %s\n",q->sList[i]);
    }
  }

  if(bVerbose==true)  fprintf(stderr,"done\n");

  free(p);


return q;
}

/* ************************************************************************* */
Ulong CasPds_LRfull_FrequencyFilter(Uchar *arPds,Ulong *arFrqIdx)
{
Ulong i,nIdx,nMaxIdx,nMaxFrqIdx;
Ulong nType,nRecLen,nFreq;  /* Pds archive file stuff */
float *pFreq;



  nRecLen=*((Ulong*)&arPds[8]);
  nFreq=(nRecLen-16)/4;
  nType=*((Ulong*)&arPds[16]);
  pFreq=(float*)(arPds+16+2*nRecLen);

  if(bVerbose)  
    fprintf(stderr,"freq filter nType=%08X\n",nType);

  /* assume all frequencies are processed */
  for(i=0;i<nFreq;i++) 
    arFrqIdx[i]=i;
  nMaxFrqIdx=i;

  if(bVerbose==true) 
    fprintf(stderr," max frq=%d\n",nMaxFrqIdx);

  /* filter by receiver bands first */
  if( (nType&CasRcv_Mask)==CasLfdr_Normal ){/* nLfdrMode from cmd line */
    if( !(nLfdrMode&CasLfdr_Normal) )
      nMaxFrqIdx=0;  /* don't process lfdr data */
  }
  else if( (nType&CasRcv_Mask)==CasMfdr_Normal ){/* nMfdrMode from cmd line */
    if( !(nMfdrMode&CasMfdr_Normal) )
      nMaxFrqIdx=0;  /* don't process mfdr data */
  }
  else if( ((nType&CasRcv_Mask)==CasMfr_Normal) ||
           ((nType&CasRcv_Mask)==CasMfr_FastToggle) ){
    if( !(nMfrMode&CasMfr_Band1) ){  /* mfr1 23.89Hz to 169.00Hz */
      nMaxIdx=0;                   
      for(i=0;i<nMaxFrqIdx;i++){
	nIdx=arFrqIdx[i];
	if( pFreq[nIdx]>170 )
	  arFrqIdx[nMaxIdx++]=nIdx;
      }  
      nMaxFrqIdx=nMaxIdx;
    }
    if( !(nMfrMode&CasMfr_Band2) ){  /* mfr2 192.11Hz to 1,470.09Hz */
      nMaxIdx=0;                   
      for(i=0;i<nMaxFrqIdx;i++){
	nIdx=arFrqIdx[i];
	if( (pFreq[nIdx]<192) || (pFreq[nIdx]>1471) )
	  arFrqIdx[nMaxIdx++]=nIdx;
      }  
      nMaxFrqIdx=nMaxIdx;
    }
    if( !(nMfrMode&CasMfr_Band3) ){  /* mfr3 1,536.89Hz to 11,799.33Hz */
      nMaxIdx=0;                   
      for(i=0;i<nMaxFrqIdx;i++){
	nIdx=arFrqIdx[i];
	if( pFreq[nIdx]<1536 )
	  arFrqIdx[nMaxIdx++]=nIdx;
      }  
      nMaxFrqIdx=nMaxIdx;
    }
  }
  else if( ((nType&CasRcv_Mask)==CasHfr_Analysis) && (nType&CasHfr_BandABC) ){
    if( !(nHfrMode&CasHfr_BandA) ){/* hfrA 3,685.61Hz to 15,823.72Hz */
      nMaxIdx=0;                   
      for(i=0;i<nMaxFrqIdx;i++){
	nIdx=arFrqIdx[i];
	if( pFreq[nIdx]>15824 )
	  arFrqIdx[nMaxIdx++]=nIdx;
      }  
      nMaxFrqIdx=nMaxIdx;
    }
    if( !(nHfrMode&CasHfr_BandB) ){/* hfrB 16,585.23Hz to 71,206.74Hz */
      nMaxIdx=0;                   
      for(i=0;i<nMaxFrqIdx;i++){
	nIdx=arFrqIdx[i];
	if( (pFreq[nIdx]<16585) || (pFreq[nIdx]>71207) )
	  arFrqIdx[nMaxIdx++]=nIdx;
      }  
      nMaxFrqIdx=nMaxIdx;
    }
    if( !(nHfrMode&CasHfr_BandC) ){/* hfrC 74,633.53Hz to 320,430.31Hz */
      nMaxIdx=0;                   
      for(i=0;i<nMaxFrqIdx;i++){
	nIdx=arFrqIdx[i];
	if( (pFreq[nIdx]<74633) || (pFreq[nIdx]>320431) )
	  arFrqIdx[nMaxIdx++]=nIdx;
      }  
      nMaxFrqIdx=nMaxIdx;
    }
  }/* fi hfr bands abc */
  else if( ((nType&CasRcv_Mask)==CasHfr_Analysis) && (nType&CasHfr_BandHF1) ){
    if( !(nHfrMode&CasHfr_BandHF1) ){/* hf1 0.00Hz to 4,300,000.00Hz */
      nMaxFrqIdx=0;
    }
  }
  else if( ((nType&CasRcv_Mask)==CasHfr_Analysis) && (nType&CasHfr_BandHF2) ){
    if( !(nHfrMode&CasHfr_BandHF2) ){/* hf2 25.00Hz to 16,175,000.00Hz */
      nMaxFrqIdx=0;
    }
  }
  else{  /* assume all frequencies are processed */
    for(i=0;i<nFreq;i++)  
      arFrqIdx[i]=i;
    nMaxFrqIdx=i;
  }



  /* filter by processing options */ 
  if(nNixFrq&NIX_FREQUENCIES){
    if((nType&CasRcv_Mask)==CasLfdr_Normal){
      ;  /* null statement */
    }  
    else if((nType&CasRcv_Mask)==CasMfdr_Normal){/* replace mfr2 with mfdr frq*/
      if(nNixFrq&NIX_MFR2forMFDR){/* mfdr  13.93Hz to 1,736.86Hz */
	nMaxIdx=0;                /* mfr2 192.11Hz to 1,470.09Hz */
	for(i=0;i<nMaxFrqIdx;i++){
	  nIdx=arFrqIdx[i];
	  if( (pFreq[nIdx]>192.0) && (pFreq[nIdx]<1471.0) )     
	    arFrqIdx[nMaxIdx++]=nIdx;
	}  
	nMaxFrqIdx=nMaxIdx;
      }/* if replace mfr2 with mfdr */  
    }/* fi mfdr */  
    else if( ((nType&CasRcv_Mask)==CasMfr_Normal) || /* nix mfr frqs w/ hfrA */
             ((nType&CasRcv_Mask)==CasMfr_FastToggle) ){
      if(nNixFrq&NIX_MFR3forHFRA){/* mfr3 1,536.89Hz to 11,799.33Hz */
	nMaxIdx=0;                /* hfrA 3,685.61Hz to 15,823.72Hz */
	for(i=0;i<nMaxFrqIdx;i++){
	  nIdx=arFrqIdx[i];
	  if( pFreq[nIdx]<3685.0 )  /* freqs to keep */
	    arFrqIdx[nMaxIdx++]=nIdx;
	}  
	nMaxFrqIdx=nMaxIdx;
      }/* if replace mfr3 with hfrA */  
    }/* fi mfr */
    else if((nType&CasRcv_Mask)==CasHfr_Analysis){
      if( (nNixFrq&NIX_HFRAforMFR3) && (nType&CasHfr_BandA) ){
	nMaxIdx=0;                /* mfr3 1,536.89Hz to 11,799.33Hz */
	for(i=0;i<nMaxFrqIdx;i++){/* hfrA 3,685.61Hz to 15,823.72Hz */
	  nIdx=arFrqIdx[i];
	  if( pFreq[nIdx]>11800.0 )  /* freqs to keep */
	    arFrqIdx[nMaxIdx++]=nIdx;
	}  
	nMaxFrqIdx=nMaxIdx;
      }/* fi replace hfrA with mfr 3 */  
      if( (nNixFrq&NIX_HFRCforHF1) && (nType&CasHfr_BandC) ){
	nMaxIdx=0;             
	for(i=0;i<nMaxFrqIdx;i++){
	  nIdx=arFrqIdx[i];
	  if( pFreq[nIdx]<100000 )  /* keep everything below 100KHz */
	    arFrqIdx[nMaxIdx++]=nIdx;
	}  
	nMaxFrqIdx=nMaxIdx;
      }/* fi delete hfrc above 100Khz */
      if( (nNixFrq&NIX_HF1forHFRC) && (nType&CasHfr_BandHF1) ){
	nMaxIdx=0;             
	for(i=0;i<nMaxFrqIdx;i++){
	  nIdx=arFrqIdx[i]; 
	  if( pFreq[nIdx]>320431)  /* keep everything above 320,431Hz */
	    arFrqIdx[nMaxIdx++]=nIdx;
	}  
	nMaxFrqIdx=nMaxIdx;
      }/* fi delete hf1 below 320Khz */
/* to do list NIX_HFRCforHF2 NIX_HF2forHFRC raj */
      if( 0 && (nNixFrq&NIX_HF1PWR) && (nType&CasHfr_BandHF1) ){
	assert(0);
	nMaxIdx=0;             /* delete all 100KHz harmonics */     
	for(i=0;i<nMaxFrqIdx;i++){
	  nIdx=arFrqIdx[i];
	  if( (int)pFreq[nIdx]%(int)50E3 != 0 )  /* freqs to keep */
	    arFrqIdx[nMaxIdx++]=nIdx;
	}  
	nMaxFrqIdx=nMaxIdx;
fprintf(stderr,"Nixing power supply lines\n");
      }/* fi delete 50Khz harmonics, power supply noise */
    }/* fi hfr */  
  }/* if filter frequencies, processing options */  




return nMaxFrqIdx;
}
