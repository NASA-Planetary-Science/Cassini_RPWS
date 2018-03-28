/* 
  Reads HFR Calibration mini-packets and outputs in a form suitable for pds 

Version 1.0
  Friday August 27, 2004

Version 1.1
   ???
	
Version 1.2 (cwp)
   Convert to stdbool, use cassini file locations from the compiler
	designated path
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include <string.h>
#include <strings.h>

#include <Cext.h>

#include <casephem/CasSpice.h>

#include <rpwstlm/RecordFile.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasHfr.h>


extern const char *sCasHfrMeanderVersion;


void Help(void);

static const char *sVersion="hfrcp(), ver 1.2";



int main(int argc,char *argv[])
{
bool bVerbose=false,bSilent=false;
bool bRaw=false,bMp=false,bPds=false;
char *sFiles[65536];
int nFiles=0;

int nInCnt,nErrCnt,nCalCnt=0;
Ulong nRecLen;

char sPds[128];
RecordFile *hInRec,*hOutRec;   /* Handles to the in & out files */
CasRecord *pInRec,*pOutRec;
FILE *hIn,*hOut,*hErr,*hPds;

char *p,sCalHdr[128];
Ulong nSclk,nRti,nEvtSclk;

  /* Initialize Command Line Variables */
  hIn=stdin;  hOut=stdout;  hErr=stderr; 

  while(--argc){
    ++argv;
    if((!strcmp("-h",*argv))||(!strcmp("-help",*argv))){
      Help();
      exit(1);
    }
    else if(!strcmp("-help",*argv)){
      Help();
      exit(1);
    }
    else if(!strcmp("-raw",*argv)){
      bRaw=true;
    }
    else if(!strcmp("-pds",*argv)){
      bPds=true;
    }
    else if(!strcmp("-mp",*argv)){
      bMp=true;
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
      sFiles[nFiles++]=*argv;
      sFiles[nFiles]=NULL;
    }/* else */
  }
  if(bVerbose==true)
    bSilent=false;



  CasSpice_Init(NULL);
  CasHfr_Init();

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



  nInCnt=nErrCnt=nCalCnt=0;
  nFiles=0;
  while(sFiles[nFiles]!=NULL){
    if(RecordFile_Open(hInRec,sFiles[nFiles],"rb")==false){
      fprintf(stderr,"Unable to read %s\n",sFiles[nFiles]);
      exit(1);
    }

    while((nRecLen=RecordFile_ReadNextRecord(hInRec,pInRec))>0){
      ++nInCnt;

      if(CasHfr_bHfrPacket(pInRec->data)==false)
        continue;
      if(CasHfr_bCalibration(pInRec->data)==false)
        continue;

      nSclk=pInRec->status.cds_time;
      nRti=pInRec->data[3];  nRti<<=8;  nRti|=pInRec->data[2];
      nEvtSclk=GetEventTime(nSclk,nRti);
      nRti<<=5;  nRti&=0x0FF;

      p=sCalHdr;
      p+=sprintf(p,"%s %08X ",CasMp_sStdHdr(pInRec),pInRec->status.gnd_status);
      p+=sprintf(p,"%02X %08X ",
                pInRec->status.cds_fill,pInRec->status.cmprs_status);
      p+=sprintf(p,"%s",CasSpice_nSclk_to_sScet(nEvtSclk,nRti,NULL));

      if(CasHfrCalibration_bValidPacket(stderr,pInRec)==false){
        ++nErrCnt;
        continue;
      }
      

      if(bMp==true)
        RecordFile_WriteRecord(hOutRec,pInRec);

      if((bRaw==true)||(bPds==true)){
        /* This loads the packet data into global values within librpwstlm.a */
        CasHfrCalibration_XtractRaw(pInRec);
      }
      if(bRaw==true)
        CasHfrCalibration_DumpRaw(stderr);
      if(bPds==true){
        CasHfrCalibration_dBMagnitudePhase();
        sprintf(sPds,"cal%d.txt",nCalCnt);
        if((hPds=fopen(sPds,"wt"))==NULL){
          fprintf(stderr,"unable to open %s\n",sPds);
          exit(1);
        }
        fprintf(hPds,"%s\n",sCalHdr);
		  /* This writes from the global area setup by the previous functions */
        CasHfrCalibration_WritePdsFormatABC(hPds);
        CasHfrCalibration_WritePdsFormatHF1(hPds);
        CasHfrCalibration_WritePdsFormatHF2(hPds);
        fclose(hPds);
      } 
      ++nCalCnt;
    
    }/* elihw records */

    RecordFile_Close(hInRec);
    ++nFiles;
  }/* elihw files */

  if(bVerbose==true)
    fprintf(hErr,"%s read %d, errors %d, Calibration Packets %d\n",
            sVersion,nInCnt,nErrCnt,nCalCnt);
 
return 0;
}


void Help(void)
{
  fprintf(stderr,"%s\n",sVersion);
  fprintf(stderr,"   Outputs hfr calibration packets as U-files, PDS archive or Raw Data\n");

fprintf(stderr,
  "-s    silent operation, don't output hfr cal packet header to stderr\n"
  "-v    silent operation, don't output hfr cal packet header in pds file\n"
  "-mp   output mini-packets on stdout\n"
  "-pds  create a pds archive file\n"
  "-raw  output the raw data on stdout\n"
);

return;
}
