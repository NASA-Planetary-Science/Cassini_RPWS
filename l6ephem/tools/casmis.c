/* Tiny wrapper around get_mission_phase_name */

#include <stdio.h> 
#include <assert.h>

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
    return 1;
  }

  fprintf(stdout,"%s\n",get_mission_phase_name(sBeg,sEnd));

  return 0;
}
