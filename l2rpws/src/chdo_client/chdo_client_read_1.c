#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>

#include <fg.h>
#include "socket_calls.h"

FILE *dbg = NULL;
int dc = 0;

/*
 *      CHDO_CLIENT_READ.C
 */
void process_caller_data (char *buf, int bytes_read)
{
  int i;
  static int icnt = 1;

  fwrite (buf, bytes_read, 1, stdout);
  fflush (stdout);
  if (dbg) {
    fprintf (dbg, "%6d: len:%5d\n", dc++, bytes_read);
    fflush (dbg);
  }
}

int main (int argc, char *argv[])
{
  char host[128] = "casrpws";
  int port = 1251;
  int fd;
  int bytes_read;
  char buf[1025];

  fg_flags (argc, argv);
  if (fg_flag ("h") | fg_flag ("help")) {
    fprintf (stderr, "chdo_client_read_1:\n");
    fprintf (stderr, " \n");
    fprintf (stderr, "    -host %s       specify host name\n", host);
    fprintf (stderr, "    -port %d          specify host port\n", port);
    fprintf (stderr, " \n");
    fprintf (stderr, "    -debug              debugging file\n");
    fprintf (stderr, " \n");
    exit (1);
  }

  if (fg_flag ("host")) {
    strcpy (host, fg_flagc ("host"));
  }
  port = fg_int ("port", port);
  if (fg_flag ("debug"))
    dbg = fopen ("chdo_client_read.log", "wa");

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
