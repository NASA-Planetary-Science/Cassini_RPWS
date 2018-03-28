#include <stdio.h> 
#include <stdlib.h>
#include <stdarg.h>
#include <signal.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <assert.h>

#include "CasType.h"
#include "CasSpice.h" 
#include "CasMiniPacket.h" 
#include "CasCmdParse.h" 
#include "CasPds.h" 



#define NIX_DATASETS       0xFFFF0000  /**/
#define NIX_BadData        0x80000000  /**/
#define NIX_HfrSndActive   0x40000000  /**/ 
#define NIX_LpRawSwpActive 0x20000000  /**/
#define NIX_FakeData       0x10000000  /* Data the was produced on the ground */
#define NIX_ZeroFill       0x08000000  /**/



const char *sVersion="pdsmark() ver 1.0";



#define PDS_FILE_LIST_MAX  16384 
typedef struct caspdsfilelist_tag{
  char *sList[PDS_FILE_LIST_MAX];
  Uchar nMpHdr[PDS_FILE_LIST_MAX][32];
  Ulong nType[PDS_FILE_LIST_MAX],nRecLen[PDS_FILE_LIST_MAX];
  Ulong nId[PDS_FILE_LIST_MAX],nMax;
}CasPdsFileList;

#define PDS_MARK_LIST_MAX  1024 
typedef struct cas_pds_mark_list_tag{
  char  sName[PDS_MARK_LIST_MAX][32];
  Ulong nSclk[PDS_MARK_LIST_MAX];
  Ulong nRti[PDS_MARK_LIST_MAX];
  Ulong nEvt[PDS_MARK_LIST_MAX];
  Ulong nMark[PDS_MARK_LIST_MAX];
  Ulong nMax;
}CasPdsMarkList;

Uchar arPdsFileBuf[CAS_PDS_MAX_FILE_SIZE];  /* 64MB buffer */

void CasPds_ReadMark(char *sFileName,CasPdsMarkList *pList);

CasPdsFileList* CasPds_LRfull_SortFileList(char *sList[]);


/* Global Command Line Arguments */
Bool bSilent=False;
Bool bVerbose=False;
Ulong nLfdrMode,nMfdrMode,nMfrMode,nHfrMode,nMscMode;
Ulong nNixFrq,cmdln_nNixBadPkt;
Bool bHardSort=False;


void show_help(FILE *h);


Bool bNewDatFile=True;  /* keeps track of when a new data file is opened */

int main(int argc,char *argv[])
{
char sPdsDir[512];
char *sFiles[PDS_FILE_LIST_MAX];
Ulong nFiles;
/*
float fEvtTime;
double dEvtTime;
*/
char *sBeg,*sEnd;
DasTime tBeg,tEnd;

CasPdsFileList *pFiles;
CasPdsMarkList *pList;


/* Global Command Line Options */
/* Bool bConglomerate=True;  
Bool bCorrectAntennaResonance=False;
Bool bCmdLine_BgndDay=False;
Bool bCmdLine_NoFill=False;
int nBackGroundDivision=0;
int nAbsoluteBgnd=0; 
*/
/*
Ulong nScetDaysBeg,nScetMsecBeg,nScetDaysEnd,nScetMsecEnd;
Ulong nSensor,nSensorList;
*/

/*
Uchar arDasb0[DAS_B0_PACKET_MAX_SIZE];
Ulong arFrqIdx[CAS_PDS_MAX_RECORD_ITEMS],nMaxFrqIdx;
*/

Ulong nSclkSec,nSclkFine,nScetDays,nScetMsec,nDataQuality,nSensorId;
Ulong nRecLen,nNumRec,nMode,nRec;
/* float *pTime,*pFreq,*pAmpl,*pDasb0;*/

char *pChar;
Uchar *pByte;
int i;
Ulong *pDword;

/* housekeeping */
clock_t tProcTime;
time_t tElaspedTime;
Ulong nWrote,nBytesRead,nBytesWrote;


  fprintf(stderr,"%s\n",sVersion);

  tProcTime=clock();
  tElaspedTime=time(NULL);

  nLfdrMode=nMfdrMode=nMfrMode=nHfrMode=nMscMode=0;

  strcpy(sPdsDir,"/net/spica/export/data13/cassini/pds");
  if((pChar=getenv("CAS_PDS_ROOT_DIR"))!=NULL)
    strcpy(sPdsDir,pChar);

  nFiles=0;  sBeg=sEnd=NULL; 
  while(--argc){
    ++argv;
    if(!strcmp("-d",*argv)){
      --argc;  ++argv;
      strcpy(sPdsDir,*argv);
    }
    else if( (!strcmp("-h",*argv)) || (!strcmp("-help",*argv)) )
      {show_help(stdout);exit(0);}
    else if(!strcmp("-s",*argv))
      bSilent=True;
    else if(!strcmp("-v",*argv)){
      bVerbose=True;
    }
    else if(!strcmp("-tBeg",*argv)){
      --argc;  ++argv;
      sBeg=*argv;
    }
    else if(!strcmp("-tEnd",*argv)){
      --argc;  ++argv;
      sEnd=*argv;
    }
    else if(!strcmp("-lfdr",*argv)){
      nLfdrMode=CasLfdr_Normal|CasAntAll;
    }/* else lfdr */
    else if(!strcmp("-mfdr",*argv)){
      nMfdrMode=CasMfdr_Normal|CasAntAll;
    }/* else mfdr */
    else if(!strcmp("-mfr",*argv)){
      nMfrMode=CasMfr_AnyMode|CasAntAll;
    }/* else mfr */
    else if(!strcmp("-hfr",*argv)){
      nHfrMode=CasHfr_Analysis|CasHfr_BandAll|CasAntAll;
    }/* else hfr */
    else if(!strcmp("-msc",*argv)){
      nMscMode=CasHfr_Millisecond|CasHfr_BandHF1|CasHfr_BandHF2|CasAntAll;
    }/* else hfrmsc */
    else{
      sFiles[nFiles++]=*argv;
      sFiles[nFiles]=NULL;
    }/* esle */
  }/* while parsing command line argurments */
  if(bVerbose==True)  bSilent=False;

  pList=calloc(1,sizeof(CasPdsMarkList));
fprintf(stderr,"CasPdsMarkList = %d bytes\n",sizeof(CasPdsMarkList));

  /* init some things */
  CasSpice_Init(NULL);
  if(sBeg!=NULL)
    CasCmd_ParseTime(sBeg,&tBeg);
  if(sEnd!=NULL)
    CasCmd_ParseTime(sEnd,&tEnd);


  if(bSilent==False){
    fprintf(stderr,"Plot Time: %s, Sclk=%08lX.%04lX, Scet=%04lX.%08lX\n",
      tBeg.sScet,tBeg.nSclk,tBeg.nFine,tBeg.nDays,tBeg.nMsec);
    fprintf(stderr,"           %s, Sclk=%08lX.%04lX, Scet=%04lX.%08lX\n",
      tEnd.sScet,tEnd.nSclk,tEnd.nFine,tEnd.nDays,tEnd.nMsec);
    fprintf(stderr,"Lfdr=%08lX, Mfdr=%08lX, Mfr=%08lX, Hfr=%08lX, Msc=%08lX\n",
            nLfdrMode,nMfdrMode,nMfrMode,nHfrMode,nMscMode);
    fprintf(stderr,"filtering options=%08lX, Bad Packets=%08lX\n",nNixFrq,
            cmdln_nNixBadPkt);
    fprintf(stderr,"pds root dir=%s\n",sPdsDir);
  }

  
  strcat(sPdsDir,"/DATA/RPWS_LOW_RATE_FULL");
  if(nFiles==0){  /* build null terminated file list */
    nFiles=CasPds_LRfull_GetFiles(sFiles,tBeg.sScet,tEnd.sScet,sPdsDir,
                         nLfdrMode,nMfdrMode,nMfrMode,nHfrMode,nMscMode);
    assert(nFiles<PDS_FILE_LIST_MAX);
  
    if(bVerbose==True){
      fprintf(stderr,"Number of Files=%ld\n",nFiles);
      for(i=0;i<nFiles;i++)
        fprintf(stderr,"%s\n",sFiles[i]);
    }
    if(nFiles==0){
      fprintf(stderr,"No files found for the time period:\n");
      fprintf(stderr,"  %s to %s\n",tBeg.sScet,tEnd.sScet);
      exit(1);
    }
  }

  pFiles=CasPds_LRfull_SortFileList(sFiles);
  if(pFiles==NULL){
    fprintf(stderr,"lost the files, pFiles=%p\n",pFiles);
    exit(1);
  } 


  /* create the mark list */
  CasPds_ReadMark("./mperr.log",pList);

  nFiles=0;  nBytesRead=nBytesWrote=0;
  while(pFiles->sList[nFiles]!=NULL){

    if(CasPds_LRfull_ReadArchive(pFiles->sList[nFiles],arPdsFileBuf)==NULL){
      fprintf(stderr,"CasPds_LRfull_ReadArchive(%s) failed.\n",
              pFiles->sList[nFiles]);
      exit(1); 
    }
    if(bVerbose==True)  
      fprintf(stderr,"%3ld data read %s\n",nFiles,pFiles->sList[nFiles]);

    /* assign pointers to stuff */
    pDword=(void*)(arPdsFileBuf+8);  /* CORPWS01  bytes 0-7 */
    nRecLen=*pDword++;               /* record length bytes 8-11 */
    nNumRec=*pDword++;               /* number of records bytes 12-15 */
    nMode=*pDword;                   /* mode 16-19, no increment */


    nBytesRead+=(nNumRec*nRecLen);
    
    pByte=arPdsFileBuf+2*nRecLen;  /* next record is the first data record */
    for(nRec=3;nRec<nNumRec;++nRec){

      pByte+=nRecLen;                /* start of the next data record */
      nSclkSec= *pDword++;           /* nSclkSec bytes 0-3 */
      nSclkFine=*pDword++;
      nScetDays=nSclkFine;
        nSclkFine>>=16;              /* nSclkFine bytes 4-5  */
        nScetDays&=0x0FFFF;          /* nScetDays bytes 6-7  */ 
      nScetMsec=*pDword++;           /* nScetMsec bytes 8-11 */
      nDataQuality=*pDword++;          
      nSensorId=nDataQuality;
        nDataQuality&=0xFFFFFFF0;    /* Data Quality bytes 12 - 14 */
        nSensorId&=   0x0000000F;    /* Sensor Id lower nibble of byte 15 */


    }/* for processing each record from the archive file */

    nWrote=fwrite(arPdsFileBuf,sizeof(Uchar),nNumRec*nRecLen,stdout);
    assert(nWrote==(nNumRec*nRecLen));

    ++nFiles;
  }/* while reading file list */
  fflush(stdout);

  fprintf(stderr,"Bytes Read %ld and Wrote %ld to idl\n",
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
"  -s         suppress messages to stderr\n"
"  -v         output of messages\n" 
"  -tStart DATE DATE is a string acceptable to parsetime()\n"
"  -tStop DATE DATE is a string acceptable to parsetime()\n"
"  -lfdr LHExEzBxByBz\n"
"  -mfr 123ExEzBxBz\n"
"  -hfr ABC12EuEvExEz\n"
  );    



return;
}



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


  if(bVerbose==True)  fprintf(stderr,"sorting file list...");

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
      fprintf(stderr,"fread(%s) failed, returned %ld of %ld\n",
                      sList[nIdx],nRead,nHdrSize);
      exit(1);
    }
    fclose(h);

    /* record Length in bytes and number of records */
    assert(strncmp("CORPWS01",(char*)arHdr,8)==0);  /* CORPWS01 */
    pDword=(void*)(arHdr+ 8);   /* (Ulong*) */
    nRecLen=*pDword++;
    nNumRec=*pDword++;
    nType=*pDword++;
    nSpare=*pDword++;

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
        fprintf(stderr,"error unknown. nType=%08lX\n  ",nType);
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
        if(bHardSort==True){
          assert(0);
        }
        else{
          switch(p->nType[i]&CasRcv_Mask){
            case CasLfdr_Normal:  
              p->nId[i]=nId;    /* assume all lfdr packets are the same */
/*
              if( (p->nType[nIdx]&~CasAntMask)!=(p->nType[i]&~CasAntMask) ){
                fprintf(stderr,"SortFiles(): Lfdr modes differ %08lX %08lX\n",
                        p->nType[nIdx],p->nType[i]);
              }
*/
              break;
            case CasMfdr_Normal:
              p->nId[i]=nId;    /* assume all mfdr packets are the same */
/*
              if( (p->nType[nIdx]&~CasAntMask)!=(p->nType[i]&~CasAntMask) ){
                fprintf(stderr,"SortFiles(): Lfdr modes differ %08lX %08lX\n",
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
              fprintf(stderr,"packet type=%08lX  masked=%08lX\n",
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
    fprintf(stderr,"id=%2ld, %08lX, %4ld :: ",
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
        fprintf(stderr,"%s, bad type %08lX.\n",p->sList[nIdx],p->nType[nIdx]);
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

  if(bVerbose==True){
    fprintf(stderr,"\n");
    for(i=0;i<q->nMax;i++){
      fprintf(stderr,"id=%2ld, %08lX, %4ld :: ",
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

  if(bVerbose==True)  fprintf(stderr,"done\n");

  free(p);


return q;
}



void CasPds_ReadMark(char *sFileName,CasPdsMarkList *pList)
{
char *p,s[256],*pB,*pE,sSub[32];
int i,nIdx,nLineCnt;
FILE *h;
Ulong nSclk,nRti;

  if((h=fopen(sFileName,"rt"))==NULL){
    fprintf(stderr,"unable to open %s\n",sFileName);
    exit(1);
  }
/*
typedef struct cas_pds_mark_list_tag{
  char *sName[PDS_MARK_LIST_MAX];
  Ulong nSclk[PDS_MARK_LIST_MAX];
  Ulong nRti[PDS_MARK_LIST_MAX];
  Ulong nEvt[PDS_MARK_LIST_MAX];
  Ulong nSts[PDS_MARK_LIST_MAX];
  Ulong nLen;
}CasPdsMarkList;
*/

/*
Ulong nLfdrMode,nMfdrMode,nMfrMode,nHfrMode,nMscMode;
LFDR     568F6833.4003   0x23 00808004
MFDR     568F6A93.5303   0x23 00808004
MFR nrm  568F6A93.5300  0x104 00808004
HFR anl  568F3033.8131  0x317 00E88000 
*/

  nLineCnt=0;  nIdx=0;
  while((pB=fgets(s,256,h))!=NULL){
    pE=pB;
    sSub[0]='\0';

    while(*pE!=' ' && *pE!='\0') ++pE;  /* find end of rcv field */
    *pE++='\0';
    strcpy(pList->sName[nIdx],pB);
    pB=pE;
 
    while(*pB<0x30 || *pB>0x39){
      if((*pB>0x40 && *pB<0x5B) || (*pB>0x60 && *pB<0x7B) ){ /* sub mode */
        pE=pB;
        while(*pE!=' ' && *pE!='\0')  ++pE;
        *pE++='\0';
        strcpy(sSub,pB);
        pB=pE;
      }
      else{
        ++pB;  /* find beginning of numbers */
      }
    }
    pList->nSclk[nIdx]=strtoul(pB,&pE,16);
    pB=++pE;

    pList->nRti[nIdx]=strtoul(pB,&pE,16);
    pB=pE;

    pList->nMark[nIdx]=strtoul(pB,&pE,16);  /* packet length */
    pB=pE;

    pList->nMark[nIdx]=strtoul(pB,&pE,16);
 
    nSclk=pList->nSclk[nIdx];
    nRti =pList->nRti[nIdx];

    pList->nEvt[nIdx]=GetEventTime(nSclk,nRti);

    if(!strcmp("LFDR",pList->sName[nIdx])){
      if(nLfdrMode)  ++nIdx;
      else           continue;
    }
    else if(!strcmp("MFDR",pList->sName[nIdx])){
      if(nMfdrMode)  ++nIdx;
      else           continue;
    }
    else if(!strcmp("MFR",pList->sName[nIdx])){
      if(nMfrMode)  ++nIdx;
      else          continue;
    }
    else if(!strcmp("HFR",pList->sName[nIdx])){
      if(nHfrMode)  ++nIdx;
      else          continue;
    }
    else                                         continue;

    --nIdx;
fprintf(stderr,"name=%s ",pList->sName[nIdx]);
if(strlen(sSub))
  fprintf(stderr,"sub=%s ",sSub);
fprintf(stderr,"sclk=%08lX ",pList->nSclk[nIdx]);
fprintf(stderr,"rti=%04lX ",pList->nRti[nIdx]);
fprintf(stderr,"mark=%08lX\n",pList->nMark[nIdx]);


    ++nIdx;
    ++nLineCnt;
  }

fprintf(stderr,"nLineCnt=%d\n",nLineCnt);

return;
}
