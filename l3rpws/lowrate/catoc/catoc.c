/*
                      catoc.c
                     Robert Johnson
                     September 22, 2003
*/


#include <stdio.h> 
#include <stdlib.h>  /* strtoul() */
#include <string.h>  /* strstr() */

const char *sVersion="catoc() Version 1.0";
char *sTourSeq="/home/raj/project/Cassini/pds/cat/CasTourSeq.txt";
char *sSatEnc= "/home/raj/project/Cassini/pds/cat/CasSatEnc.txt";
char* fud(int yr,int doy);

int main(int argc,char *argv[])
{
char *p,s[256],o[256];
char date[64];
int n,yr,doy,dur;
FILE *h;



/* Sequence  Rev     Epoch (SCET)     DOY   Duration 
                                            In days  
   --------  ---  -----------------   ---   -------- 
      S1      -   2004-May-15 00:00   136      35    
      S2      0   2004-Jun-19 01:38   171      42    
      S3      0   2004-Jul-30 23:05   212      43     */

  if((h=fopen(sTourSeq,"rt"))==NULL){
    fprintf(stderr,"unable to open %s\n",sTourSeq);
    exit(0);
  }
  while((p=fgets(s,256,h))!=NULL){

    while( (*p!='#') && (*p!='\n') && (*p!='\0') )  ++p;
    *p='\0';  /* nix any comments beginning with # */  

    if((p=strtok(s," "))==NULL)  continue;

    sprintf(o,"  {\"%s\",",p);
    if((n=strlen(o)) < 10)  
      strncat(o,"                                       ",(10-n));
    p=strtok(NULL," ");  /* Rev */
    p=strtok(NULL," ");  /* Epoch */ 
      yr=strtoul(p,NULL,0);
    p=strtok(NULL," ");  /* hh:mm */
    p=strtok(NULL," ");  /* DOY */
      doy=strtoul(p,NULL,0);
    p=strtok(NULL," ");  /* Duration */
      dur=strtoul(p,NULL,0);
    
    sprintf(date,"\"%04d-%03d\", ",yr,doy);  strcat(o,date);
    sprintf(date,"\"%s\"},",fud(yr,doy+dur-1)); strcat(o,date);

    fprintf(stdout,"%s\n",o); 
  } 
  fclose(h);

                                                                           
/* Rev   Name   Satellite  Epoch (SCET)        DOY   Alt   in/  Speed Phase
                                                     km    out  km/s   deg
   ----  -----  ---------  ----------------    ---   ---   ---  -----  ----
   0     0PH    Phoebe     2004-Jun-11 19:33   163   1997  in    6.4   25
   0     0MI*   Mimas      2004-Jul-01 00:30   183  76424  in   22.3   80   */

  if((h=fopen(sSatEnc,"rt"))==NULL){
    fprintf(stderr,"unable to open %s\n",sSatEnc);
    exit(0);
  }
  while((p=fgets(s,256,h))!=NULL){

    while( (*p!='#') && (*p!='\n') && (*p!='\0') )  ++p;
    *p='\0';  /* nix any comments beginning with # */  

    if((p=strtok(s," "))==NULL)  continue;
    p=strtok(NULL," ");  /* Name */
    p=strtok(NULL," ");  /* Satellite */ 
    sprintf(o,"  {\"%s\",",p);
    if((n=strlen(o)) < 20)  
      strncat(o,"                                       ",(20-n));
    p=strtok(NULL," ");  /* Epoch */ 
      yr=strtoul(p,NULL,0);
    p=strtok(NULL," ");  /* hh:mm */
    p=strtok(NULL," ");  /* DOY */
      doy=strtoul(p,NULL,0);
      dur=doy;
    
    sprintf(date,"\"%04d-%03d\", ",yr,doy);  strcat(o,date);
    sprintf(date,"\"%04d-%03d\"},",yr,doy);  strcat(o,date);

    fprintf(stdout,"%s\n",o); 
  } 
  fclose(h);



return n;
}

char* fud(int yr,int doy)
{
int diy;
static char out[65];

  if(yr%100){            /* Year is NOT a century year */
    if(yr%4)  diy=365;   /* NOT evenly divisible by 4 */
    else      diy=366;
  }
  else{                     /* Year IS a century year */
    if(yr%400)  diy=365;    /* 1900 is not a leap year */
    else      diy=366;      /* 2000 is a leap year */
  }

  if(doy>diy){
    doy-=diy;
    ++yr;
  }
  sprintf(out,"%04d-%03d",yr,doy);

return out;
}


