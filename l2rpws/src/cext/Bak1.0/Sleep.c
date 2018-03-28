#include <stdlib.h>
#include <stdio.h>
#include <time.h>   /* use the Unix time.h not ANSI time.h */


void SleepFor_Seconds(double dSeconds)
{
struct timeval timeout;

  timeout.tv_sec=(unsigned long)dSeconds;
  timeout.tv_usec=(long)((dSeconds-timeout.tv_sec)*1000000);
  select(0,0,0,0,&timeout);
  
return;
}
