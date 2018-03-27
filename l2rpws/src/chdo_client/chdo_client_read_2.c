#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>

#include <fg.h>

#include "socket_calls.h"
#include "chdo_common.h"

extern FILE *dbg;
extern int dc;
char *Version = { "V2.0" };

/*
 *      CHDO_CLIENT_READ.C
 */

int main (int argc, char *argv[])
{
  char host[128] = "hal15";
  int port = 5005;
  int fd;
  int bytes_read;
  char buf[1025];

  fg_flags (argc, argv);

  if (fg_flag ("host")) {
    strcpy (host, fg_flagc ("host"));
  }
  port = fg_int ("port", port);
  if (fg_flag ("more"))
    dc = 1;
  if (fg_flag ("debug")) {
    dbg = fopen ("chdo_client_read.log", "wa");
    fprintf (dbg, "chdo_client_read: %s\n", Version);
    fflush (dbg);
  }

  if (fg_flag ("h") | fg_flag ("help")) {
    if (strlen (host) < 8) {
      strcat (host, "        ");
      host[8] = 0;
    }
    fprintf (stderr, "chdo_client_read: %s\n", Version);
    fprintf (stderr, "   cTOT set to 'ACCEPT' connection\n");
    fprintf (stderr, " \n");
    fprintf (stderr, "    -host %s   specify host name\n", host);
    fprintf (stderr, "    -port %4.4d       specify host port\n", port);
    fprintf (stderr, " \n");
    fprintf (stderr, "    -debug           debugging file\n");
    fprintf (stderr, "    -more            debugging file\n");
    fprintf (stderr, " \n");
    fprintf (stderr, " Notes for IE WorkStation\n");
    fprintf (stderr, "   In the TPC/IP Socket setup\n");
    fprintf (stderr, "    specify 'hal1_host' as the nodename\n");
    fprintf (stderr, "    to cause IE/WS to listen on the\n");
    fprintf (stderr, "    correct interface\n");
    fprintf (stderr, "   Aborting client on RPWS W/S may\n");
    fprintf (stderr, "    cause cTOT to fail in an infinite loop\n");
    fprintf (stderr, " \n");
    fprintf (stderr, " Notes for the SOPC\n");
    fprintf (stderr, "   In the TPC/IP Socket setup\n");
    fprintf (stderr, "    specify 'casrpws' as the nodename\n");
    fprintf (stderr, " Notes\n");
    fprintf (stderr, " \n");
    exit (1);
  } else
    fprintf (stderr, "Chdo_client_read: %s (%s/%d)\n", Version, host, port);

  while ((fd = connect_to_server (host, port, NULL)) < 0) {
    sleep (5);
  }
  /*
   * if ((fd = connect_to_server(host, port, NULL)) < 0) {
   * fprintf(stderr,"chdo_client_READ: could not connect to server\n");
   * perror("connect_to_server");
   * exit (-1);
   * }
   */
  toggle_buffer_small_packets (fd, 0);

  while ((bytes_read = read (fd, buf, 1024)) > 0) {
    process_caller_data (buf, bytes_read);
  }
}
