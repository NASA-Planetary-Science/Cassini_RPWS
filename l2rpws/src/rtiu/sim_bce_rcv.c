
/****************************************************************
 * File Name :  sim_bce_rcv.c
 *
 * Purpose :    Program to run on Unix machine, which interfaces
 *              with the actual RTIU. This program receives data
 *              from the RTIU (i.e. packets, handshake echoes, etc).
 *              This program receives the data from BCE Output.
 *
 * Synopsis :
 *      This program reads the Ethernet address of the Unix machine on which
 *      it runs. It uses that address to initialize its own port, enabling
 *      this program to run on any Sun. However, this program uses a hard-coded
 *      Ethernet address to reference the RTIU. Should the RTIU ever be given a
 *      new address, this program must be recompiled to reference that new address.
 *
 * SCCS / RCS:
 *   
 *  $RCSfile$
 *  $Revision$ 
 *  $State$
 *  $Date$
 *  $Author$
 *  $Source$
 *  $Locker$
 *
 *  @(#) FILE ID 1.4 sim_bce_rcv.c
 *  @(#) FROM    -r1.4 /uhome/hernandez/bce_sim_dir/SCCS/s.sim_bce_rcv.c 11/22/94 08:09:08
 *  @(#) ID STR  sim_bce_rcv.c 1.4 11/22/94 08:09:08
 *  
 *
 * Requirements :
 *
 * References:
 *
 * Limitations:
 *
 * Revision:
 *
 ****************************************************************
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <strings.h>

#define CONN_PORT       5555
#define OK              0
#define ERROR   (-1)
struct bce_header
{
  unsigned short length;
  unsigned short function_code;
  unsigned short status;
  unsigned short sequence_number;
  unsigned short value;
  unsigned short seconds[2];
  unsigned short rti_and_subrti;
};

#define ACTUAL_REQ_HDR_SIZE     (sizeof (struct bce_header))
#define MIN_BCE_MSG_SIZE        ACTUAL_REQ_HDR_SIZE

extern int errno;
static int sock;                        /* socket descriptor */
static int af = AF_INET;                /* address family */
static int type = SOCK_STREAM;          /* socket type */
static int protocol = 0;                /* IP protocol */
static FILE *fd;
char rtiu_address[] = { "128.255.10.37" };
struct hostent *hp;

int determine_port_number(int argc, char** argv, int* port_number);
int read_the_rtiu_data (int socket);

/* ************************************************************************* */
int main(int argc, char* argv[])
{
  int status;
  char machine_name[MAXHOSTNAMELEN + 1];
  int struct_size = sizeof (struct sockaddr_in);
  int port_number;
  struct sockaddr_in me, him;
  struct hostent *orig;

  if (determine_port_number (argc, argv, &port_number) != OK)
    exit (1);
  fprintf (stderr, "Assuming RTIU PORT number of %d\n", port_number);

  status = gethostname (machine_name, MAXHOSTNAMELEN);
  printf ("Sim BCE Rcv: starting on %s\n", machine_name);
  printf ("Min = %d, structure size = %d\n", MIN_BCE_MSG_SIZE,
          ACTUAL_REQ_HDR_SIZE);
  bzero ((char *) &me, struct_size);
  if ((sock = socket (af, type, protocol)) < 0) {
    perror ("Sim BCE Rcv:socket");
    exit (1);
  }
  printf ("sock = %d\n", sock);

  orig = gethostbyname (machine_name);
  bcopy (*(orig->h_addr_list), (char *) &me.sin_addr.s_addr, orig->h_length);
  me.sin_family = AF_INET;
  me.sin_port = 0;

  status = bind (sock, (struct sockaddr*)&me, struct_size);
  if (status < 0) {
    perror ("Sim BCE Rcv:Unable to bind socket");
    close (sock);
    printf ("Quitting\n");
    exit (1);
  }

  printf ("Sim BCE_Rcv: The bind was successful\n");

  bzero ((char *) &him, struct_size);
  him.sin_addr.s_addr = inet_addr (rtiu_address);
  him.sin_family = AF_INET;
  him.sin_port = port_number;

  status = connect (sock, (struct sockaddr*)&him, struct_size);
  if (status) {
    char text[128];

    printf ("ERR = %d\n", errno);
    sprintf (text, "Unable to connect to %s:%d ", rtiu_address, him.sin_port);
    perror (text);
    shutdown (sock, 2);
    close (sock);
    printf ("Quitting\n");
    exit (1);
  }
  printf ("SIM_BCE_RCV\n");

  do {
    printf ("status=read_the_rtiu_data(sock)\n");
    status = read_the_rtiu_data (sock);
  } while (status == OK);

  printf ("SIM_BCE_RCV: ERROR DURING READ, now closing socket\n");
  shutdown (sock, 2);
  close (sock);
  printf ("Socket shutdown and closed\n");

  printf ("Goodbye\n");

  return 0;
}

/*********************************************
 *
 * Func:        read_the_ritu_data
 *
 *********************************************
 */

int read_the_rtiu_data (int socket)
{
#define BUF_WORD_SIZE   1500
#define BUF_BYTE_SIZE   (BUF_WORD_SIZE<<1)
  char *cptr;
  fd_set readmask;
  int n_read, bytes_to_read, nfds, n_trailing, segmented;
  int last_read = 0;
  unsigned short length;
  struct bce_header *bce_ptr;
  static unsigned short msg_buf[BUF_WORD_SIZE];

  segmented = 0;

  printf ("read_the_rtiu_data\n");

  n_read = read (socket, &length, sizeof (short));
  if (n_read != sizeof (short)) {
    printf ("Bad initial read of %d bytes\n", n_read);
    return (-1);
  }
  if (length > BUF_BYTE_SIZE) {
    printf ("Bad length spec of %d bytes\n", length);
    return (-1);
  }
  msg_buf[0] = length;
  bytes_to_read = n_trailing = length - 2;

  cptr = (char *) (msg_buf + 1);
  n_read = read (socket, cptr, bytes_to_read);
  if (n_read == bytes_to_read)
    goto ready_to_display;

  if (n_read != ERROR)
    last_read = n_read;
  else if (errno != EWOULDBLOCK)
    return (~OK);

  bytes_to_read -= last_read;

  FD_ZERO (&readmask);
  FD_SET (socket, &readmask);

  do {
    if ((nfds = select (FD_SETSIZE, &readmask, NULL, NULL, NULL)) < 0) {
      printf ("SELECT failed\n");
      return (~OK);
    }

    n_read = read (socket, cptr + last_read, bytes_to_read);
    if (n_read != ERROR) {
      last_read += n_read;
      bytes_to_read -= n_read;
    }
    if (n_read == 0) {
      printf ("READ ZERO BYTES\n");
      return (~OK);
    }
    if (n_read == ERROR && errno != EWOULDBLOCK)
      return (~OK);

  } while (bytes_to_read > 0);
  segmented = ~0;

ready_to_display:
  bce_ptr = (struct bce_header *) msg_buf;
  if (bce_ptr->length != MIN_BCE_MSG_SIZE) {
    printf
      ("%cLEN: %04x   ID: %04x   STAT: %04x   SEQ: %04x  VAL: %04x  TIME: %04x%04x:%x   FINAL: %04x\n",
       (segmented) ? '-' : ' ', bce_ptr->length, bce_ptr->function_code,
       bce_ptr->status, bce_ptr->sequence_number, bce_ptr->value,
       bce_ptr->seconds[0], bce_ptr->seconds[1],
       bce_ptr->rti_and_subrti >> 13, msg_buf[n_trailing >> 1]);
  } else
    printf
      ("%cLEN: %04x   ID: %04x   STAT: %04x   SEQ: %04x  VAL: %04x  TIME: %04x%04x:%x\n",
       (segmented) ? '-' : ' ', bce_ptr->length, bce_ptr->function_code,
       bce_ptr->status, bce_ptr->sequence_number, bce_ptr->value,
       bce_ptr->seconds[0], bce_ptr->seconds[1],
       bce_ptr->rti_and_subrti >> 13);

  return (OK);
}

/****************************************************************
 * Name :       determine_port_number
 *
 * Purpose:     If operator provides command line argument, this function
 *              assumes it is an alternate port number to use. This function
 *              guarantees that if any arguments appear, only exactly one
 *              appears, and the number is verified to be in the range
 *              of 1024 to 65536 (i.e. 0400h to FFFFh).
 *
 * Inputs :
 *
 * Outputs :
 *      returns OK if no arguments appear
 *
 * Limitations:
 *
 * Pseudo Code:
 *
 ****************************************************************
 */

int determine_port_number(int argc, char** argv, int* port_number)
{
  int ret_value = OK;

  *port_number = CONN_PORT;
  if (argc == 2) {
    sscanf (argv[1], "%d", port_number);
    if (*port_number < 0x0400 || *port_number > 0xFFFF) {
      fprintf (stderr, "Port number (%d) is out of range\n", *port_number);
      ret_value = ~OK;
    }
  } else if (argc != 1) {
    fprintf (stderr, "Too many command line arguments\n");
    ret_value = ~OK;
  }
  return (ret_value);
}
