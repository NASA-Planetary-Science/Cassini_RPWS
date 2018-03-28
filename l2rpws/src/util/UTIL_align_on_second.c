
#ifdef _RPWS_
#include <sys/timers.h>
#include <time.h>
#endif

/*  	The first argument is the micro second difference between S/C second 
  boundary and HP Workstation second boundary (allways positive).
    	The second argument is the delay from the S/C second boundary to delay
  from.
  
    * denotes an even second boundary
    
  S/C time        *				*
  HP time		*				*
                        |----------first arg----|
                        	second arg  |---|
                         delay occurs here->+
*/


void UTIL_align_on_second (double delta_sc_ws_msec,
                           double delay_from_boundary_msec)
{

#ifdef _RPWS_
  double dt;
  struct timeval timeout;
  struct timespec current_time;

  delta_sc_ws_msec *= 1000000.0;        /* nanoseconds */
  delay_from_boundary_msec *= 1000000.0;        /* nanoseconds */

  timeout.tv_sec = 0;
  getclock (TIMEOFDAY, &current_time);
  dt = delta_sc_ws_msec - current_time.tv_nsec - delay_from_boundary_msec;
  if (dt >= 0.0) {
    timeout.tv_usec = (long) (dt / 1000.0);
    select (0, 0, 0, 0, &timeout);
  } else {
    timeout.tv_usec =
      (long) (1000000000 - current_time.tv_nsec + delta_sc_ws_msec -
              delay_from_boundary_msec);
    select (0, 0, 0, 0, &timeout);
  }
#endif

  return;
}
