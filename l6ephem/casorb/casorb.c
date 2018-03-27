/*
                      casorb.c
                     Robert Johnson
                     July 5, 2005

$ casorb tBegin tEnd
    tBegin is of the form YYYY-DOYTHH:MM:SS.MSC   
    tEnd is of the form YYYY-DOYTHH:MM:SS.MSC   
    returns a string sutible for pds "A" 
	 
	 Edited on 2012-10-25 to use stdbool, and to get the casorb.h 
	 constructed via the makefile 
*/


#include <stdio.h> 
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>

#include <CasSpice.h>

#include <casorb.h>

char* CasPds_sOrbitName(char *sScetBeg, char *sScetEnd);


int main(int argc,char *argv[])
{
char *sBeg,*sEnd,*sTar;


  sBeg=sEnd=sTar=NULL;
  while(--argc){
    ++argv;
    if(sBeg==NULL)       sBeg=*argv;
    else if(sEnd==NULL)  sEnd=*argv;
    else{
      fprintf(stderr,"invalid argument %s\n",*argv);
    }
  }

  if(sBeg==NULL){
    fprintf(stderr,"missing begin time\n");
    exit(1);
  }
  if(sEnd==NULL){
    sEnd=sBeg;
  }

  /* init the spice kernel */
  CasSpice_Init(NULL);

  sTar=CasPds_sOrbitName(sBeg,sEnd);
  if(strlen(sTar)>0)
    fprintf(stdout,"%s\n",sTar);

return 0;
}



/* expecting sScet = yyyy-doyThh:mm:ss.msc
                     0123456789ABCDEF01234 */
char* CasPds_sOrbitName(char *sScetBeg, char *sScetEnd)
{
char *p;
int i,nIdx,arIdx[128];
size_t nCmpLen;
/* CasMissionCat *x=MissionPhaseName;  */
CasOrbitName *x=CasOrbit;
static char sStr[256];

  /* use shortest length for comparison */
  nCmpLen=strlen(x[0].sScetBeg);
  if(nCmpLen>strlen(sScetBeg))  nCmpLen=strlen(sScetBeg);
  if(nCmpLen>strlen(sScetEnd))  nCmpLen=strlen(sScetEnd);

  nIdx=i=0;
  while(strlen(x[nIdx].sName)>0){
    if( (strncmp(x[nIdx].sScetEnd,sScetBeg,nCmpLen)>=0) &&
        (strncmp(sScetEnd,x[nIdx].sScetBeg,nCmpLen)>=0) )
        arIdx[i++]=nIdx;
    ++nIdx;
  }

  p=sStr;
  if(i==0)
    sStr[0]='\0';
  else if(i==1){
    p+=sprintf(p,"\"%s\"",x[arIdx[--i]].sName);
  }
  else{
    p+=sprintf(p,"{\"%s\"",x[arIdx[--i]].sName);
    while(i>0)
      p+=sprintf(p,",\"%s\"",x[arIdx[--i]].sName);
    p+=sprintf(p,"}");
  }


return sStr;
}
