#include <time.h>

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#include <sys/types.h>


#include <fg.h>

#include "socket_calls.h"

FILE *dbg = NULL;
int dc = 0;

/*
 *      CHDO_CLIENT_READ.C
 */
char *current_date ()
{
  static char data_string[128];
  time_t clock = 0;

  clock = time (NULL);
  ctime_r (&clock, data_string, 127);
  data_string[strlen (data_string) - 1] = 0;
  return data_string;
}
void process_caller_data (char *buf, int bytes_read)
{
  int i;
  static int icnt = 1;

  fwrite (buf, bytes_read, 1, stdout);
  fflush (stdout);
  if (dbg)
    if (dc) {
      fprintf (dbg, "chdo_common: %6d: len:%5d\n", dc++, bytes_read);
      fflush (dbg);
    }
}

void next_caller (int server)
{
  address_t caller_addr;
  int caller;
  int bytes_read;
  char buf[1025];

  caller = accept_connection (server, &caller_addr);
  toggle_buffer_small_packets (caller, 0);

  if (dbg) {
    fprintf (dbg, "%s ", current_date ());
    fprintf (dbg, "chdo_common: got connection from %s port %d\n",
             get_other_host (caller), get_other_port (caller));
    fflush (dbg);
  }


  while ((bytes_read = read (caller, buf, 1024)) > 0) {
    process_caller_data (buf, bytes_read);
  }

  if (bytes_read < 0)
    perror ("read");

  close (caller);
}
