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
char *Version = { "V2.31a" };

int main (int argc, char *argv[])
{
  int port = 1251;
  int fd;
  int rt_flag = 1;
  int active = 1;
  int bytes_read;
  char buf[1025];
  int server;
  char *server_address;
  address_t server_addr;
  int server_port;

  fg_flags (argc, argv);
  fprintf (stderr, "%s chdo_listner: %s\n", current_date (), Version);
  if (fg_flag ("h") | fg_flag ("help")) {
    fprintf (stderr, "   cTOT set to 'CONNECT'\n");
    fprintf (stderr, " \n");
    fprintf (stderr, "    -port %d       specify host port\n", port);
    fprintf (stderr, "    -nert          one-shot mode (for post-pass)\n",
             port);
    fprintf (stderr, " \n");
    fprintf (stderr, "    -debug         debugging file\n");
    fprintf (stderr, "    -more          additional debugging\n");
    fprintf (stderr, " \n");
    fprintf (stderr, " Notes for IE WorkStation\n");
    fprintf (stderr, " Notes for the SOPC\n");
    fprintf (stderr, "   In the TPC/IP Socket setup\n");
    fprintf (stderr, "    specify the name of this workstation\n");
    fprintf (stderr, "    change port numbers for each run\n");
    fprintf (stderr, "    make sure port numbers match\n");
    fprintf (stderr, " \n");
    exit (1);
  }

  port = fg_int ("port", port);

  if (fg_flag ("nert"))
    rt_flag = 0;
  if (fg_flag ("more"))
    dc = 1;
  if (fg_flag ("debug")) {
    dbg = fopen ("chdo_listner.log", "a");
    fprintf (dbg, "\n********        ********        ********\n");
    fprintf (dbg, "%s chdo_listner: %s\n", current_date (), Version);
    fflush (dbg);
  }

  if (dbg) {
    fprintf (dbg, "%s Starting ", current_date ());
    fflush (dbg);
  }

  server = new_server (port, &server_addr);
  if (server < 0)
    if (dbg) {
      fprintf (dbg, "%d=", server);
      switch (server) {
       case E_BAD_SOCKET:
         fprintf (dbg, "E_BAD_SOCKET");
         break;
       case E_BAD_BIND:
         fprintf (dbg, "E_BAD_BIND");
         break;
       case E_BAD_GETSOCKNAME:
         fprintf (dbg, "E_BAD_GETSOCKNAME");
         break;
       case E_BAD_GETHOSTNAME:
         fprintf (dbg, "E_BAD_GETHOSTNAME");
         break;
       case E_BAD_GETHOSTBYNAME:
         fprintf (dbg, "E_BAD_GETHOSTBYNAME");
         break;
       case E_BAD_LISTEN:
         fprintf (dbg, "E_BAD_LISTEN");
         break;
       default:
         fprintf (dbg, "UNKNONW ERROR");
      }
      fflush (dbg);
    }
  if (dbg) {
    fprintf (dbg, "\n");
    fflush (dbg);
  }

  server_address = get_addr_host (&server_addr);
  server_port = get_addr_port (&server_addr);
  toggle_buffer_small_packets (server, 0);
  if (dbg) {
    fprintf (dbg, "%s ", current_date ());
    fprintf (dbg, "I am %s  ", server_address);
    fflush (dbg);

    fprintf (dbg, "listening on %d", server_port);
    fflush (dbg);

    fprintf (dbg, "(%d)\n", port);
    fflush (dbg);
  }

  if (!strcmp ("0.0.0.0", server_address)) {
    if (dbg) {
      fprintf (dbg, "Port seems to be in use, aborting (%s)\n",
               server_address);
      fflush (dbg);
    }
    fprintf (stderr, "Port seems to be in use, aborting (%s)\n",
             server_address);
    exit (0);
  }

  if (port != server_port) {
    if (dbg) {
      fprintf (dbg, "Port seems to be in use, aborting (%s) %d/%d\n",
               server_address, port, server_port);
      fflush (dbg);
    }
    fprintf (stderr, "Port seems to be in use, aborting (%s) %d/%d\n",
             server_address, port, server_port);
    exit (0);
  }

  while (active) {
    if (dbg) {
      fprintf (dbg, "%s %s ", current_date (), server_address);
      fprintf (dbg, "waiting for next caller\n");
      fflush (dbg);
    }
    next_caller (server);
    if (dbg) {
      fprintf (dbg, "%s %s", current_date (), server_address);
      fprintf (dbg, " done listening; ");
      if (rt_flag)
        fprintf (dbg, "waiting for next connect");
      else
        fprintf (dbg, "process exit");
      fprintf (dbg, "\n");
      fflush (dbg);
    }
    active = rt_flag;
  }

  close (server);
  exit (0);
}
