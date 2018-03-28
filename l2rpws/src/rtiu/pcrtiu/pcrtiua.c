#pragma TITLE "PC-RTIU network code"
#pragma AUTOPAGE ON

/*
 * pcrtiu.c
 */
#pragma LIST OFF
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>

#include <rtiu.h>
#pragma LIST ON
struct linger socket_linger;

int init_socket ();
int status, ist;
int pkt_size, remaining_pkt;
int cmd_port, tlm_port;                 /* Socket ports on PC-RTIU      */
int cmd_sckt, tlm_sckt = 0;             /* Socket file descriptors for  */

                                        /*
                                         * command and telemetry ports. 
                                         */

int linger_len;
int type;

 /***********************
  *  buffer copy	*
  ***********************/
void b_copy (char *old, char *new, int length)
{
  int i;
  char *source, *dest;

  source = old;
  dest = new;

  for (i = 0; i < length; ++i)
    *(dest++) = *(source++);

}

void pcrtiu_sockopt (int period)
{
  socket_linger.l_linger = period;
  if (setsockopt (tlm_sckt,
                  SOL_SOCKET,
                  SO_LINGER,
                  (char *) (&socket_linger), sizeof (socket_linger))) {
    perror ("pcrtiu_sockopt: Error setting the linger time on a socket");
    exit (EXIT_FAILURE);
  }

  linger_len = sizeof (socket_linger);
  if (getsockopt (tlm_sckt,
                  SOL_SOCKET,
                  SO_LINGER, (char *) (&socket_linger), &linger_len) < 0) {
    perror ("pcrtiu:_sockopt Error getting the linger time on a socket");
    exit (EXIT_FAILURE);
  }
}

void pcrtiu_close_socket (void)
{
  close (tlm_sckt);
  tlm_sckt = 0;
}

/******************************************************/
int pcrtiu_getCDS (char *tlm_pkt)
{
#define P_LEN 32
  int data_len;
  int pkt_body;
  int pkt_tot;
  int pkt_body_t;
  int pkt_len;
  int i;

  if (!tlm_sckt)
    return -1;
  data_len = recv (tlm_sckt, tlm_pkt, P_LEN, 0);
  if (data_len != P_LEN) {
    perror ("pcrtiu_getCDS: error reading data from socket");
    pcrtiu_close_socket ();
    exit (EXIT_FAILURE);
  }

  pkt_size = ((unsigned char) tlm_pkt[3] << 8) | (unsigned char) tlm_pkt[2];

  /*
   * Since packet size may not be currently included in the packet, 
   */
  /*
   * force it to be the expected size
   */
  /*
   * Read in the remaining packet size 
   */

  pkt_body = pkt_size - P_LEN;
  pkt_tot = 0;

  if ((pkt_body > 2048) | (pkt_body < 0))
    pkt_body = 0;
  else {
    pkt_tot = 0;
    pkt_body_t = pkt_body;
    while (pkt_tot < pkt_body) {
      i = P_LEN + pkt_tot;
      pkt_len = recv (tlm_sckt, &tlm_pkt[i], pkt_body_t, 0);
      pkt_tot += pkt_len;
      pkt_body_t -= pkt_len;
    }
  }


  /*
   * /*       if (( data_len != 0 ) && ( pkt_size > P_LEN ) )  
   * /*         {
   * /*      data_len = recv( tlm_sckt, tlm_pkt+P_LEN, pkt_size - P_LEN, 0);
   * /*
   * /*            if ( data_len != pkt_size - P_LEN) 
   * /*              {
   * /*                char temp[128];
   * /*                sprintf(temp, "pcrtiu_getCDS: Error reading from data socket %d %d",
   * /*                    pkt_size - P_LEN, data_len);
   * /*          perror( temp);
   * /*                pcrtiu_close_socket();
   * /*          exit(EXIT_FAILURE);
   * /*                 }
   * /*    }
   * /*  
   */
  return data_len;
}

/******************************************************/
int pcrtiu_putCDS (char *tlm_pkt)
{
  if (!tlm_sckt)
    return -1;
  pkt_size = ((unsigned char) tlm_pkt[3] << 8) | (unsigned char) tlm_pkt[2];
  if (pkt_size != send (tlm_sckt, tlm_pkt, pkt_size, 0)) {
    perror ("pcrtiu_putCDS: Error writing to data socket");
    pcrtiu_close_socket ();
    exit (EXIT_FAILURE);
  }
  return pkt_size;
}

/********************************************************
 * Returns: >0 - successful				*
 *          -2 - host name could not be found 		*
 *          -3 - could not create socket		*
 *          -4 - could not connect to socket on host	*
 ********************************************************/
int pcrtiu_init_socket (char *host, int port_no)
{
  int skt;
  struct sockaddr_in server;
  struct hostent *hp, *gethostbyname ();

    /********************************************
    * translate host name to host address  	*
    *********************************************/

  server.sin_family = AF_INET;
  hp = gethostbyname (host);
  if (hp == 0)
    return (-2);

        /************************
	* Create a socket	*
	*************************/

  tlm_sckt = socket (AF_INET, SOCK_STREAM, 0);
  if (skt < 0) {
    perror ("pcrtiu_init_socket: Error opening stream socket");
    return (-3);
  }

        /********************************
	* Connect to socket on 'host'	*
	*********************************/

  b_copy ((char *) hp->h_addr, (char *) &server.sin_addr, hp->h_length);
  server.sin_port = htons (port_no);

  if (connect (tlm_sckt, (struct sockaddr *) &server, sizeof (server)) < 0) {
    perror ("pcrtiu_init_socket: error connecting to socket");
    pcrtiu_close_socket ();
    return (-4);
  }

  return tlm_sckt;
}
char *pcrtiu_hostname (char *def, int argc, char *argv[])
{
  static char deflt[] = { "pcrtiu" };
  char *name = 0;
  int i;

  name = getenv ("PCRTIU");
  if (name)
    return name;

  for (i = 1; i < argc - 1; i++)
    if (!strcmp (argv[i], "-host"))
      return argv[i + 1];

  if (def)
    if (def[0])
      return def;

  return deflt;
}
