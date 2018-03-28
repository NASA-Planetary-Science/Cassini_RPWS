/* 

Version 0.0
  Monday, June 28, 2004

Version 1.0
  Tuesday, Janurayr 18, 2004
  
Version 1.1
  2012-10-25, cwp: Make C99 compliant

*/

/* Which POSIX standard version dose this code match */
#define _POSIX_C_SOURCE 200112L

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include <string.h>
#include <strings.h>



#include <rpwstlm/RecordFile.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasSpice.h>
#include <rpwstlm/CasMiniPacket.h>
#include <rpwstlm/CasCmdParse.h>

#include "rajTime.h"


void show_help(FILE *h);
static const char *sVersion="mpflist(), ver 1.1";

int main(int argc,char *argv[])
{
bool bVerbose=false,bSilent=false,bUfile=true,bOutFile=false;
bool bHourBracket=false;
char *cmdln_beg=NULL,*cmdln_end=NULL;

char *sFiles[65536];
int nFiles=0;
char sBeg[128],sEnd[128];
DasTime tBeg,tEnd;

char *p,sTmp[128];
int n,nInCnt,nOutCnt;
Ulong nRecLen;
RecordFile *hInRec,*hOutRec;   /* Handles to the in & out files */
CasRecord *pInRec,*pOutRec;

rajTime t1,t2;


  /* Initialize Command Line Variables */
  cmdln_beg=cmdln_end=NULL;

  while(--argc){
    ++argv;
    if((!strcmp("-help",*argv)) || (!strcmp("-help",*argv))){
      show_help(stdout);
      exit(0);
    }
    else if(!strcmp("-tBeg",*argv)){
      --argc;  ++argv;
      cmdln_beg=*argv;
    }
    else if(!strcmp("-tEnd",*argv)){
      --argc;  ++argv;
      cmdln_end=*argv;
    }
    else if(!strcmp("-b",*argv)){
      bHourBracket=true;
    }
    else if(!strcmp("-o",*argv)){
      bOutFile=true;
    }
    else if(!strcmp("-r",*argv)){
      bUfile=false;
    }
    else if(!strcmp("-u",*argv)){
      bUfile=true;
    }
    else if(!strcmp("-s",*argv)){
      bSilent=true;
    }
    else if(!strcmp("-v",*argv)){
      bVerbose=true;
    }
    else if((!strcmp("-version",*argv)) || (!strcmp("-ver",*argv))){
      fprintf(stderr,"%s\n",sVersion);
      fprintf(stderr,"  %s\n",CasTlm_Version());
    }
    else{
      if(cmdln_beg==NULL)  cmdln_beg=*argv;
      else                 cmdln_end=*argv;
    }/* else */
  }
  if(bVerbose==true)
    bSilent=false;

  if(cmdln_beg==NULL){
    fprintf(stderr,"mpflist() - error - no time specified\n");
    exit(1);
  }
  else{
    fxParseTime(cmdln_beg,&t1);
  } 
  if(cmdln_end==NULL){  /* assume one day */  
    fxParseTime(cmdln_beg,&t2);
    t2.dDoy+=1.0;
    fxNormalize(&t2);
  }
  else{
    fxParseTime(cmdln_end,&t2);
  } 
  if(bHourBracket==true){
    t1.dHour-=1.0;
    t2.dHour+=1.0;
    fxNormalize(&t1);
    fxNormalize(&t2);
  }

  sprintf(sBeg,"%04d-%03dT%02d:%02d:%02d",t1.nYear,t1.nDoy,
          t1.nHour,t1.nMinute,t1.nSecond);
  sprintf(sEnd,"%04d-%03dT%02d:%02d:%02d",t2.nYear,t2.nDoy,
          t2.nHour,t2.nMinute,t2.nSecond);

  CasSpice_Init(NULL);
  CasCmd_ParseTime(sBeg,&tBeg);
  CasCmd_ParseTime(sEnd,&tEnd);

  if(bVerbose==true){
    fprintf(stderr,"%04d-%03dT%02d:%02d  %04d-%03dT%02d:%02d\n",
            tBeg.nYear,tBeg.nDayOfYear,tBeg.nHour,tBeg.nMinute,
            tEnd.nYear,tEnd.nDayOfYear,tEnd.nHour,tEnd.nMinute);
  }

  nFiles=CasMp_ParseDataBase(NULL,tBeg.sScet,tEnd.sScet,sFiles);

  if(bOutFile==false){
    nFiles=0;
    while(sFiles[nFiles]!=NULL){
      if(bUfile==false){
        n=strlen(sFiles[nFiles]);
        p=sFiles[nFiles]+n-3;
        *p='r';
      }
      fprintf(stdout,"%s\n",sFiles[nFiles]);
      ++nFiles;
    }

    exit(0);
  }



  pInRec=CasRecord_Constructor(NULL);
  hInRec=RecordFile_Constructor(NULL);
  hOutRec=RecordFile_Constructor(NULL);
  if(RecordFile_Open(hOutRec,"stdout","wb")==false){
    fprintf(stderr,"Unable to write stdout\n");
    exit(1);
  }

  nFiles=nInCnt=nOutCnt=0;
  while(sFiles[nFiles]!=NULL){
    if(RecordFile_Open(hInRec,sFiles[nFiles],"rb")==false){
      fprintf(stderr,"Unable to read %s.\n",sFiles[nFiles]);
      exit(1);
    }
    if(bVerbose==true)  
      fprintf(stderr,"%s\n",sFiles[nFiles]);
    while((nRecLen=RecordFile_ReadNextRecord(hInRec,pInRec))>0){
      ++nInCnt;
      RecordFile_WriteRecord(hOutRec,pInRec);
      ++nOutCnt;
    }
    
    RecordFile_Close(hInRec);
    ++nFiles;
  }
  fflush(stdout);

  if(bSilent==false)  
    fprintf(stderr,"%d files, read/wrote %d/%d records\n",
            nFiles,nInCnt,nOutCnt);



return 0;
}


void show_help(FILE *h)
{
fprintf(h,"%s\n",sVersion);
fprintf(h,"  mpflist [OPTIONS] tBeg tEnd\n");

fprintf(h," mpflist() generates a list files or outputs all of the files\n");
fprintf(h,"within the specified time period.\n");

fprintf(h,"OPTIONS\n");
fprintf(h,"  -b     bracket time by one hour, before and after\n");
fprintf(h,"  -o     output the filenames only\n");
fprintf(h,"  -r     R files\n");
fprintf(h,"  -u     U files, default\n");
fprintf(h,"  -s     be silent\n");
fprintf(h,"  -v     be verbose\n");
fprintf(h,"  -h | -help show help\n");

return;
}
