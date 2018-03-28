/*
                      viecho.c
                     Robert Johnson
                     August 13, 2004

*/


#include <stdio.h> 
#include <stdlib.h>
#include <string.h>



int main(int argc,char *argv[])
{
char *pInLine,sInLine[1024];
int nTruncate=0;

  while(--argc){
    ++argv;
    if(!strcmp("-t",*argv))  nTruncate=1;
  }

  while((pInLine=fgets(sInLine,1024,stdin))!=NULL){
    if(nTruncate){
      pInLine+=strlen(sInLine);
      pInLine-=5;
      *pInLine='\0';
    }
    fprintf(stdout,"%s",sInLine);
  }

return 0;
}
