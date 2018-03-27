/*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

  File Name: mpus.c   
  Program: mpus 
  Author: Robert Johnson
  Creation: May 25, 1995
  Revisions:
    Janurary 20, 1999; version 8.00
	 Revamp the whole program to ..... 
	 
  cwp 2012-10-09: Switched to standard bool
  

  ---------------------------------------------------------------------------
  ---------------------------------------------------------------------------
  ---------------------------------------------------------------------------


  Inputs: 
	  CasRecord* pInPkt  - Cassini record structure containing segmented 
					   mini-packets.
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
#include <time.h>
#include <stdbool.h>
#include <string.h>
#include <strings.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <Cext.h>

#include <rpwstlm/CasType.h>
#include <rpwstlm/RecordFile.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasMiniPacket.h>

#include "mpus.h"


extern char *sMpusVersion;
extern int nMpusDebug;

void mpusSleep(double dSeconds);
void DumpPacketHeader(CasRecord *pPkt);
char *format_header(CasRecord *pPkt);


int main(int argc,char *argv[])
{
bool bStatus,cmdln_eof=false,cmdln_version=false,bSilent=false,bVerbose=false;
ULONG nLength;
RecordFile *pInRec,*pOutRec;
CasRecord *pInPkt,*pOutPkt;

int cmdln_Count,cmdln_Stop;
int nInCnt,nOutCnt,nWarnCnt,nErrCnt;


  cmdln_Count=0;
  cmdln_Stop=0x7FFFFFFF;
  nInCnt=nOutCnt=nErrCnt=nWarnCnt=0;
  bSilent=false;
  bVerbose=false;


  while(--argc){
    ++argv;
    if(!strcmp("+eof",*argv))
	 cmdln_eof=true;
    else if(!strcmp("-c",*argv)){
      --argc;  ++argv;
      cmdln_Count=strtoul(*argv,NULL,0);
    }
    else if(!strcmp("-e",*argv)){
      --argc;  ++argv;
      cmdln_Stop=strtoul(*argv,NULL,0);
    }
    else if(!strcmp("-d",*argv)){
      --argc;  ++argv;
      if((*argv[0]!='-') && (*argv[0]!='+'))  nMpusDebug=strtoul(*argv,NULL,0);
      else{ nMpusDebug=1; ++argc;  --argv;}
    }
    else if(!strcmp("-s",*argv)){
      bSilent=true;
    }
    else if(!strcmp("-v",*argv)){
      bVerbose=true;
    }
    else if((!strcmp("-ver",*argv)) || (!strcmp("-version",*argv))){
      cmdln_version=true;
    }
    else{
      fprintf(stderr,"%s\n",sMpusVersion);
      fprintf(stderr,"  %s\n",CasTlm_Version());
      fprintf(stderr,"  +eof      terminate on eof\n"
		     "  -c NNN    terminate after reading NNN packets\n"
		     "  -e NNN    terminate after writting NNN packets\n"
                     "  -d        mpus verbose/debuggind\n"
                     "  -s        silent operation\n"
                     "  -v        be verbose\n");
      exit(0);
    }
  }

  if(bVerbose==true)  bSilent=false;

  pInRec=RecordFile_Constructor(NULL);
  if(RecordFile_Open(pInRec,"stdin","rb")==false)
    exit(1);
  pOutRec=RecordFile_Constructor(NULL);
  if(RecordFile_Open(pOutRec,"stdout","wb")==false)
    exit(1);

  pInPkt=CasRecord_Constructor(NULL);
  pOutPkt=CasRecord_Constructor(NULL);

  if(cmdln_version==true){
    fprintf(stderr,"%s\n",sMpusVersion);
    fprintf(stderr,"  %s\n",CasTlm_Version());
  }

  while(1){

    if(nInCnt<cmdln_Count){
      RecordFile_ReadNextRecord(pInRec,pInPkt);
      ++nInCnt;
    } 

    nLength=RecordFile_ReadNextRecord(pInRec,pInPkt);
    if(nLength>0){
      ++nInCnt;
      if(bVerbose==true){
        fprintf(stderr,"In %4d : %s\n",nInCnt,format_header(pInPkt));
      }
      if((bStatus=CasMpus_UnsegmentMiniPacket(pInPkt,pOutPkt))==true){
        RecordFile_WriteRecord(pOutRec,pOutPkt);
        ++nOutCnt;

        if(pOutPkt->status.gnd_status&CasMpus_ErrorMask){
          if(pOutPkt->status.gnd_status&CasMpus_Error)  ++nErrCnt;
          else                                          ++nWarnCnt;
          if(bSilent==false)
            fprintf(stderr,"%s %s\n",CasMp_sStdHdr(pOutPkt),
                    CasMpus_DecodeGndStatus(pOutPkt->status.gnd_status));
        }
	if(bVerbose==true){
          fprintf(stderr,"Out%4d : %s\n",nOutCnt,format_header(pOutPkt));
        }

	if(nOutCnt>cmdln_Stop)
          break;
      }
    }
    else{
      if(cmdln_eof==true)  break;
      else                 mpusSleep(0.125);
    }
  }/* while for ever */



  if(bVerbose==true)
    fprintf(stderr,"flushing last packets\n");

  while((bStatus=CasMpus_UnsegmentMiniPacket(NULL,pOutPkt))==true){
    RecordFile_WriteRecord(pOutRec,pOutPkt);
    ++nOutCnt;

    if(pOutPkt->status.gnd_status&CasMpus_ErrorMask){
      if(pOutPkt->status.gnd_status&CasMpus_Error)  ++nErrCnt;
      else                                          ++nWarnCnt;
      if(bSilent==false)
        fprintf(stderr,"%s %s\n",CasMp_sStdHdr(pOutPkt),
                CasMpus_DecodeGndStatus(pOutPkt->status.gnd_status));
    }

    if(bVerbose==true)
      fprintf(stderr,"Out%4d : %s\n",nOutCnt,format_header(pOutPkt));
  }

  fflush(stdout);

  fprintf(stderr,"%s: InPkt=%d, OutPkt=%d :: %d warnings and %d errors\n",
          sMpusVersion,nInCnt,nOutCnt,nWarnCnt,nErrCnt);

  fflush(stderr);

return 0;
}



void mpusSleep(double dSeconds)
{
struct timeval timeout;
fprintf(stderr,"mpus...\n");
  timeout.tv_sec=(unsigned long)dSeconds;
  timeout.tv_usec=(long)((dSeconds-timeout.tv_sec)*1000000);
  select(0,0,0,0,&timeout);
  
return;
}



char *format_header(CasRecord *p)
{
int i;
unsigned long nType,nSize,nSeg,nHdrOfs;
static char sStr[128],*pStr;

  nType=(p->data[0]>>4)&0x0F; 
  switch(nType){
    case 0x02 : /* High Frequency Receiver Packets */
      nSeg=p->data[4]&0x7F;   
      if(p->data[4]&0x80)    /* EOF bit is set on last segment*/
        nSize=nSeg;          /* hammer hfr into general case */
      else
        nSize=0x7F;          /* don't know stream length */
      nHdrOfs=5;
      break;
    case 0x04 : /* Langmuir Probe Packets */
      nSeg= (p->data[4]>>4)&0x03;   /* 0 ... 3 */
      nSize=(p->data[4]>>6)&0x03;   /* 0 ... 3 */
      nHdrOfs= 10;
      break;
    case 0x08 : /* Waveform Receiver Packets */
      nSeg= (p->data[4])   &0x0F;   /* 0 ... 15 */
      nSize=(p->data[4]>>4)&0x0F;   /* 0 ... 15 */
      if(p->data[6]&0x08)  /* msf bit, more stuff follows */
        nHdrOfs=10;
      else
        nHdrOfs=8;
      break;
    case 0x0E : /* Wideband Receiver Packets */
      nSeg= (p->data[4])   &0x0F;   /* 0 ... 15 */
      nSize=(p->data[4]>>4)&0x0F;   /* 0 ... 15 */
      if(p->data[6]&0x08)  /* msf bit, more stuff follows */
        nHdrOfs=10;
      else
        nHdrOfs=8;
      break;
    default :   /* Unsegmented Variety: stim,mfr,lfdr,dust,mro,fill */
      nHdrOfs=4;
      nSeg=nSize=0;
      break;
  }
 
  pStr=sStr; 
  pStr+=sprintf(pStr,"Type=%lX  %ld of %ld : ",nType,nSeg,nSize);
  pStr+=sprintf(pStr,"0x%04X ",p->status.packet_length);
  for(i=0;i<nHdrOfs;i++)
    pStr+=sprintf(pStr,"%02X ",p->data[i]);

  if(nType==0x02){
    for(i=nHdrOfs;i<9;i++)
      pStr+=sprintf(pStr,"%02X ",p->data[i]);
    if(nSeg==0){
      switch((p->data[7]>>4)&0x03){
        case 0x00:  pStr+=sprintf(pStr," Analysis ");        break;
        case 0x01:  pStr+=sprintf(pStr," Sounder ");         break;
        case 0x02:  pStr+=sprintf(pStr," Calibration ");     break;
        case 0x03:  pStr+=sprintf(pStr," Millisecond ");     break;
        default:    pStr+=sprintf(pStr," hfr mode error ");  break;
      }
    }
    else{
      pStr+=sprintf(pStr," cont. ");
    }
  }/* fi hfr */

  if(p->status.gnd_status&CasMpus_ErrorMask)
    pStr+=sprintf(pStr,"%08X mpus error",p->status.gnd_status);
  else
    pStr+=sprintf(pStr,"%08X",p->status.gnd_status);


return sStr;
}
