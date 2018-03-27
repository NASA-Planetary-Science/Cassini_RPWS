/*
                      casclk.c
                     Robert Johnson
                     August 12, 2004

$ casclk tBegin 
    tBegin is of the form YYYY-DOYTHH:MM:SS.MSC   
    returns a string sutible for pds "1/1213256345.034"

*/


#include <stdio.h> 
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include <CasSpice.h>



int main(int argc,char *argv[])
{
char *sScet,*sSclk;
Ulong nSclk,nFine;

  sScet=sSclk=NULL;
  while(--argc){
    ++argv;
    if(sScet==NULL)      sScet=*argv;
    else{
      fprintf(stderr,"invalid argument %s\n",*argv);
    }
  }

  if(sScet==NULL){
    fprintf(stderr,"missing scet time\n");
    exit(1);
  }

  /* init the spice kernel */
  CasSpice_Init(NULL);

/*
      CasSpice_nScet_to_sScet(nDaysBeg,nMilsBeg,sScetBeg); 
      CasSpice_nSclk_to_GOOFY_sSclk(nSclkBeg,nFineBeg,sSclkBeg); 

      CasSpice_nScet_to_sScet(nDaysEnd,nMilsEnd,sScetEnd); 
      CasSpice_sScet_to_nSclk(sScetEnd,&nSclkEnd,&nFineEnd);  
      CasSpice_nSclk_to_GOOFY_sSclk(nSclkEnd,nFineEnd,sSclkEnd); 
*/

  CasSpice_sScet_to_nSclk(sScet,&nSclk,&nFine);  
  fprintf(stdout,"\"1/%d.%03d\"\n",nSclk,nFine);

return 0;
}
