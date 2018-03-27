/*
                        dasIhist.c
                     Robert Johnson
                     April 16, 2004

  Makes a histogram of the das1 stream B0 triplicate packets.

Version 0.0
  April 16, 2004

*/

#include <stdio.h> 
#include <stdlib.h>
#include <assert.h>
#include <math.h>

const char *sVersion="dasIhist() Version 0.0";

#define MAX_B0_SIZE (64*1024)
#define MAX_ITEMS   (64*1024)
#define B0_HIST_MAX_BINS (3001)

typedef struct b0_histogram_tag{
  unsigned long nFreq,nNumRec;
  unsigned long (*arHist)[B0_HIST_MAX_BINS];
  float *arBgnd,*arFreq;
}B0Histogram;


int read_b0_packet(unsigned char *p);
B0Histogram* CasDasI_CreateHistogram(unsigned char *p);  /* whole b0 packet */
void CasDasI_DestroyHistogram(B0Histogram *p);
void show_help(FILE *h);


static unsigned long buf[MAX_B0_SIZE/4];
static FILE *hIn=stdin,*hOut=stdout,*hErr=stderr;



int main(int argc,char *argv[])
{
char *sBeg=NULL;
int nStreamType=1,nTimeFormat=0,bSilent=0;

int bNoFill=0;  /* don't list freqeuncy for a count of zero */

char sName[128];
int bStatus,f,i;
unsigned long nRecLen,nItems,nNumPkt,nHistIdx,nCnt,nIdx; 
float arTime[MAX_ITEMS],arFreq[MAX_ITEMS],arAmpl[MAX_ITEMS],*pF;
double dTmp;
B0Histogram *arHist[1024];

  fprintf(hErr,"%s\n",sVersion);

  while(--argc){
    ++argv;
    if( !strcmp("-h",*argv) || !strcmp("-help",*argv)){
      show_help(hOut);
      exit(0);
    }
    else if(!strcmp("-nofill",*argv)){
      bNoFill=1;
    }
    else if(!strcmp("-s",*argv)){
      bSilent=1;
    }
    else if(!strcmp("-t",*argv)){
      --argc;  ++argv;
      nStreamType=strtoul(*argv,NULL,10);
    }
    else if(!strcmp("-tBeg",*argv)){
      --argc;  ++argv;
      sBeg=*argv;
    }
    else if(!strcmp("-t2000",*argv)){
      nTimeFormat=1;
    }
    else{
      fprintf(hErr,"argc=%d, argv=%s\n",argc,*argv);
    }
  }/* elihw parsing command line argurments */



  nRecLen=read_b0_packet((unsigned char*)buf);  
  nItems=nRecLen/12;
  nNumPkt=1;

  nHistIdx=0; 
  arHist[nHistIdx]=CasDasI_CreateHistogram((unsigned char*)buf);
  arHist[nHistIdx+1]=NULL;
  arHist[nHistIdx]->nNumRec++;

fprintf(stderr,"seeding histogram\n");

  while((nRecLen=read_b0_packet((unsigned char*)buf))!=0){
    nItems=nRecLen/12;
    pF=(float*)(buf+2);  /* time */
    for(i=0;i<nItems;i++){
      arTime[i]=*pF++;
      arFreq[i]=*pF++;
      arAmpl[i]=*pF++;
    } 

    bStatus=memcmp(arFreq,arHist[nHistIdx]->arFreq,(size_t)(nItems*4));

    if(bStatus!=0){
      nHistIdx=0;
      while(arHist[nHistIdx]!=NULL){
        bStatus=memcmp(arFreq,arHist[nHistIdx]->arFreq,(size_t)(nItems*4));
        if(bStatus==0)  break;
        else            ++nHistIdx;
      }
      if(arHist[nHistIdx]==NULL){  /* no match */
        arHist[nHistIdx]=CasDasI_CreateHistogram((unsigned char*)buf);
        arHist[nHistIdx+1]=NULL;
        fprintf(stderr,"no match, creating new :: %.3f %.3f %.3f\n",
          arHist[nHistIdx]->arFreq[0],arHist[nHistIdx]->arFreq[1],
          arHist[nHistIdx]->arFreq[2]);
      }
      else{
        fprintf(stderr,"list match :: %.3f %.3f %.3f\n",
          arHist[nHistIdx]->arFreq[0],arHist[nHistIdx]->arFreq[1],
          arHist[nHistIdx]->arFreq[2]);
      }
    }


    assert(nItems==arHist[nHistIdx]->nFreq);
    for(f=0;f<nItems;f++){
      dTmp=arAmpl[f];
      if(dTmp<1.0E-22)    dTmp=1E-22;  /* trap for zero, nIdx=3000 */
      dTmp=-100.0*log10(dTmp);
      dTmp+=1000;
      if(dTmp>3000)       nIdx=3000;
      else if(dTmp<0)     nIdx=0;
      else                nIdx=(unsigned long)dTmp;
      arHist[nHistIdx]->arHist[f][nIdx]+=1;
    }


    arHist[nHistIdx]->nNumRec++;
    ++nNumPkt;
  }/* elihw */


  fprintf(hErr,"Total Packets: %6ld\n",nNumPkt);
  nHistIdx=0;
  while(arHist[nHistIdx]!=NULL){
    fprintf(stderr,"type=%ld, packets=%ld :: %.2f %.2f %.2f\n",nHistIdx,
            arHist[nHistIdx]->nNumRec,arHist[nHistIdx]->arFreq[0],
            arHist[nHistIdx]->arFreq[1],arHist[nHistIdx]->arFreq[2]);
    ++nHistIdx;
  }


  nHistIdx=0;
  while(arHist[nHistIdx]!=NULL){
    sprintf(sName,"hist_%03ld_freq.txt",nHistIdx);
    if((hOut=fopen(sName,"wt"))==NULL){
      fprintf(stderr,"unable to open %s\n",sName);
      exit(1);
    }
    for(f=0;f<arHist[nHistIdx]->nFreq;f++){
      fprintf(hOut,"%3d  %.5E\n",f,arHist[nHistIdx]->arFreq[f]);
    }
    fclose(hOut);

    for(f=0;f<arHist[nHistIdx]->nFreq;f++){
      sprintf(sName,"hist_%03ld_f%03d.txt",nHistIdx,f);
      hOut=fopen(sName,"wt");
      if(hOut==NULL){
        fprintf(stderr,"unable to open %s\n",sName);
        exit(1);
      }
      for(i=0;i<3001;i++){
        nCnt=arHist[nHistIdx]->arHist[f][i];
        if((nCnt==0)&&(bNoFill!=0))
          continue;
        dTmp=i;
        dTmp-=1000.0;
        dTmp/=-100.0; 
        dTmp=pow(10.0,dTmp);
        fprintf(hOut,"%.5E  %6ld\n",dTmp,nCnt);
      }
      fclose(hOut);
    }
    ++nHistIdx;
  }



return 0;
}


int read_b0_packet(unsigned char *p)
{
char sHdr[32];
unsigned long nLen;
size_t nRead;

  if((nRead=fread(p,sizeof(char),8,hIn))!=8)
    return 0;
  memcpy(sHdr,p,8);
  sHdr[8]='\0';
  nLen=strtoul(sHdr+4,NULL,16);
  sHdr[4]='\0';
  if(strcmp(sHdr,":b0:")){
    fprintf(hErr,"not a das1 b0 packet, %s.\n",sHdr);
    return 0;
  }  
  if(nLen>65532){
    fprintf(hErr,"bad length for das1 b0 packet, len=%08lX\n",nLen);
    return 0;
  }  
  if((nRead=fread(p+8,sizeof(char),nLen,hIn))!=nLen)
    return 0;

return nLen;
}



void show_help(FILE *h)
{
fprintf(h,"%s\n",sVersion);
fprintf(h,"sorry, no help available (yet).\n");
exit(1);
fprintf(h,
"  -h               show help.\n");
fprintf(h,
"  -fill DOUBLE     fill value to be used for bad data, default -1E31\n");
fprintf(h,
"  -s               silent operation, don't ouput to stderr\n");
fprintf(h,
"  -t INTEGER       stream type output: 0=ascii,1=float,2=double, default 1\n"
"                   (float)\n");
fprintf(h,
"  -tBeg STRING     begin time of data capture\n");
fprintf(h,
"  -t2000           time tags; use days since Jan. 1, 2000, default sec from\n"
"                   Jan. 1, 1958\n");
fprintf(h,
"  -w DOUBLE        sample width, DDD seconds to interpolate over, default \n"
"                   128.0 seconds\n");
return;
}



B0Histogram* CasDasI_CreateHistogram(unsigned char *pPkt)
{
char sHdr[32];
int i;
unsigned long nFreq;
float *pFreq;
B0Histogram *p;

  memcpy(sHdr,pPkt,8);  
  sHdr[8]='\0';
  nFreq=strtoul(sHdr+4,NULL,16);  nFreq/=12; 
  sHdr[4]='\0';
  if(strcmp(sHdr,":b0:")!=0){
    fprintf(stderr,"not a b0 packet, %s\n",sHdr);
    exit(1); 
  }
  
  fprintf(stderr,"CasDasI_CreateHistogram(), nFrq=%ld(0x%lX)\n",nFreq,nFreq);

  assert((p=calloc(1,sizeof(B0Histogram))) != NULL );
  p->arHist=calloc(nFreq*B0_HIST_MAX_BINS,sizeof(unsigned long));
    assert(p->arHist!=NULL); 
  assert( (p->arBgnd=calloc(nFreq,sizeof(float))) !=NULL );
  assert( (p->arFreq=calloc(nFreq,sizeof(float))) !=NULL );

  p->nFreq=nFreq;
  p->nNumRec=0;

  pFreq=(float*)(pPkt+8);  /* time */
  ++pFreq;                 /* freq */
  for(i=0;i<p->nFreq;i++){
    p->arFreq[i]=*pFreq;
    pFreq+=3;
  }

return p;
}



void CasDasI_DestroyHistogram(B0Histogram *p)
{

  p->nFreq=0;
  p->nNumRec=0;

  free(p->arFreq);
  free(p->arBgnd);
  free(p->arHist);
  free(p);

return;
}
