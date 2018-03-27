#include <stdlib.h>
#include <stdint.h>
#include <sys/time.h>
#include <sys/select.h>

/* There was a note here that said:
  
   "use the Unix time.h not ANSI time.h"
	
 but Unix time.h didn't define struct timeval, so I switched to
 <sys/time.h> anyway to get the source to build.
 
 --cwp 2012-06-05
*/

void delay_seconds(double dly)
{
struct timeval timeout;

  timeout.tv_sec=(uint32_t)dly;
  timeout.tv_usec=(int32_t)((dly-timeout.tv_sec)*1000000);
  select(0,0,0,0,&timeout);
  
return;
}
