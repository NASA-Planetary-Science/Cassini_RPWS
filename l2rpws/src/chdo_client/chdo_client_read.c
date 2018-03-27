#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <strings.h>

#include "socket_calls.h"

/*
 *      CHDO_CLIENT_READ.C
 */
void process_caller_data (char *buf, int bytes_read)
{
  int i;
  static int icnt = 1;

  fwrite (buf, bytes_read, 1, stdout);
  fflush (stdout);
}

int main (argc, argv)
int argc;
char **argv;
{
  char host[128];
  int port;
  int fd;
  int bytes_read;
  char buf[1025];

  if (argc != 3) {
    fprintf (stderr,
             "chdo_client_read1: usage: sample_client_read host port\n");
    exit (1);
  }
  strcpy (host, argv[1]);
  port = atoi (argv[2]);
  if (!port)
    port = 1251;
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
