/* Tiny wrapper around get_target_name 

    tBegin is of the form YYYY-DOYTHH:MM:SS.MSC   
    tEnd is of the form YYYY-DOYTHH:MM:SS.MSC   
    returns a string sutible for pds {"EARTH","SOLAR SYSTEM"} 

*/


#include <stdio.h> 

#include "CasTables.h"


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

  if(sBeg==NULL || sEnd==NULL){
    fprintf(stderr,"missing begin/end times\n");
    return 0;
  }

  fprintf(stdout,"%s\n",get_target_name(sBeg,sEnd));
  return 0;
}

