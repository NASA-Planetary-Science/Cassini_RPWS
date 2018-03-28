#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <unistd.h>

#define _SOCKET_CALLS_
#include "socket_calls.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN (64)
#endif /* MAXHOSTNAMELEN */

#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff
#endif /* INADDR_NONE */

/* CWP: Standard library has these ...
 static void bzero(char *dest, int nbytes)
  {
     int i;
     for(i=0; i<nbytes; i++)
       dest[i] = 0;
    }
 static void bcopy(char *src, char *dest, int nbytes)
   {
     int i;
     for(i=0; i<nbytes; i++)
       dest[i] = src[i];
    }
*/

int sockerr_return (errcode)
int errcode;
{
  return (errcode);
}

int new_server (port, address_ptr)
int port;
address_t *address_ptr;
{
  int socksize;
  int server_sock;
  struct hostent *host_ptr;
  char hostname[MAXHOSTNAMELEN];
  struct sockaddr_in default_addr;
  struct sockaddr_in *server_addr;
  int toggle_reuse_address ();

  if (address_ptr == NULL)
    server_addr = &default_addr;
  else
    server_addr = (struct sockaddr_in *) address_ptr;

  server_addr->sin_family = AF_INET;
  server_addr->sin_addr.s_addr = INADDR_ANY;
  server_addr->sin_port = port;

  socksize = sizeof (struct sockaddr_in);

  if ((server_sock = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
    return (sockerr_return (E_BAD_SOCKET));

  if (toggle_reuse_address (server_sock, 1))    /* for quick cleanup time */
    perror ("toggle_reuse_address");

  if (bind (server_sock, (struct sockaddr *) server_addr, socksize) < 0)
    return (sockerr_return (E_BAD_BIND));

  if (getsockname (server_sock, (struct sockaddr *) server_addr, &socksize) <
      0)
    return (sockerr_return (E_BAD_GETSOCKNAME));

  if (gethostname (hostname, MAXHOSTNAMELEN))
    return (sockerr_return (E_BAD_GETHOSTNAME));

  if ((host_ptr = gethostbyname (hostname)) == NULL)
    return (sockerr_return (E_BAD_GETHOSTBYNAME));

  bcopy (host_ptr->h_addr, (char *) &server_addr->sin_addr,
         host_ptr->h_length);

  /*
   * This "magic number" 5 is the number of calls that can be waiting 
   */
  if (listen (server_sock, 5) == -1)
    return (sockerr_return (E_BAD_LISTEN));

  return (server_sock);
}

int accept_connection (server_sock, address_ptr)
int server_sock;
address_t *address_ptr;
{
  int caller = -1;
  int socksize = 1;
  struct sockaddr_in default_addr;
  struct sockaddr_in *caller_addr;

  if (address_ptr == NULL)
    caller_addr = &default_addr;
  else
    caller_addr = (struct sockaddr_in *) address_ptr;;

  /*
   * continue the accept() if the system call is interrupted 
   */
  while (caller < 0) {
    if ((caller = accept (server_sock,
                          (struct sockaddr *) caller_addr, &socksize)) < 0)
      if (errno != EINTR)
        return (sockerr_return (E_BAD_ACCEPT));
  }

  return (caller);
}

int make_address (host, port, addr)
char *host;
int port;
struct sockaddr_in *addr;
{
  long num_addr;                        /* representation of a numeric address */
  struct hostent *host_ptr;             /* pointer to a host entry */

  if (addr == NULL)
    return (sockerr_return (E_BAD_ARGS));
  bzero ((char *) addr, sizeof (struct sockaddr_in));
  addr->sin_family = AF_INET;

  /*
   *     First try to interpret host as a numeric domain address,
   *     such as "128.1.2.3", then try to get a host entry by name.
   */

  if ((num_addr = inet_addr (host)) != INADDR_NONE) {
    bcopy ((char *) &num_addr, (char *) &addr->sin_addr, sizeof (num_addr));
  } else {
    if ((host_ptr = gethostbyname (host)) == NULL)
      return (sockerr_return (E_BAD_GETHOSTBYNAME));

    bcopy (host_ptr->h_addr, (char *) &addr->sin_addr, host_ptr->h_length);
  }

  /*
   * Adding the port number is trivial. 
   */
  addr->sin_port = (htons (port));

  return (0);
}

int connect_to_server (host, port, address_ptr)
char *host;
int port;
address_t *address_ptr;
{
  int fd;
  struct sockaddr_in default_addr;
  struct sockaddr_in *host_addr_ptr;

  if (address_ptr == NULL)
    host_addr_ptr = &default_addr;
  else
    host_addr_ptr = (struct sockaddr_in *) address_ptr;

  if (make_address (host, port, host_addr_ptr) < 0)
    return (sockerr_return (E_BAD_ADDRESS));

  if ((fd = socket (AF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
    return (sockerr_return (E_BAD_SOCKET));

  if (connect (fd, (struct sockaddr *) host_addr_ptr,
               sizeof (struct sockaddr_in)) < 0) {
    (void) close (fd);
    return (sockerr_return (E_BAD_CONNECT));
  }

  return (fd);
}

int str_write (fd, buf)
int fd;
char *buf;
{
  int bytes_sent;
  int remaining;

  if (buf == NULL)
    return (sockerr_return (E_BAD_ARGS));

  remaining = strlen (buf);

  while (remaining > 0) {
    bytes_sent = write (fd, buf, remaining);
    if (bytes_sent < 0) {
      if (errno != EINTR)
        return (sockerr_return (E_BAD_WRITE));
    } else
      remaining -= bytes_sent;
  }
  return (0);
}

int buf_write (fd, buf, len)
int fd;
char *buf;
int len;
{
  int bytes_sent;
  int remaining;
  char *p;

  if (buf == NULL)
    return (sockerr_return (E_BAD_ARGS));

  remaining = len;
  p = buf;

  while (remaining > 0) {
    bytes_sent = write (fd, p, remaining);
    if (bytes_sent < 0) {
      if (errno != EINTR)
        return (sockerr_return (E_BAD_WRITE));
    } else {
      remaining -= bytes_sent;
      p += bytes_sent;
    }
  }
  return (0);
}

int buf_read (fd, buf, len)
int fd;
char *buf;
int len;
{
  int bytes_read;
  int remaining;
  char *p;

  if (buf == NULL)
    return (sockerr_return (E_BAD_ARGS));

  remaining = len;
  p = buf;

  while (remaining > 0) {
    bytes_read = read (fd, p, remaining);

    if (bytes_read > 0) {
      remaining -= bytes_read;
      p += bytes_read;
    } else if (bytes_read == 0)
      return (len - remaining);
    else if ((bytes_read < 0) && (errno != EINTR))
      return (sockerr_return (E_BAD_READ));
  }
  return (len);
}

int str_read (fd, buf, maxlen)
int fd;
char *buf;
int maxlen;
{
  int bytes_read = 0;

  if (buf == NULL)
    return (sockerr_return (E_BAD_ARGS));

  while (bytes_read < 0) {
    bytes_read = read (fd, buf, maxlen - 1);    /* saves room for trailing \0 */
    if ((bytes_read < 0) && (errno != EINTR))
      return (sockerr_return (E_BAD_READ));
  }
  if (bytes_read)
    buf[bytes_read] = '\0';             /* make sure the string is null-terminated */
  return (bytes_read);
}

char *get_addr_host (address_ptr)
address_t *address_ptr;
{
  struct sockaddr_in *addr_ptr = (struct sockaddr_in *) address_ptr;

  return (inet_ntoa (addr_ptr->sin_addr));
}

int get_addr_port (address_ptr)
address_t *address_ptr;
{
  struct sockaddr_in *addr_ptr = (struct sockaddr_in *) address_ptr;

  return (ntohs (addr_ptr->sin_port));
}

char *get_other_host (sock)
int sock;
{
  struct sockaddr_in addr;
  /**/ int size = sizeof (struct sockaddr_in);
  static struct hostent *hp;

  if (getpeername (sock, (struct sockaddr *) &addr, &size) < 0) /* <--------- */
    return ("");
  hp = gethostbyaddr ((char *) &addr.sin_addr,
                      sizeof (struct in_addr), addr.sin_family);
  if (hp)
    return (hp->h_name);
  else
    return (inet_ntoa (addr.sin_addr));
}

int get_other_port (sock)
int sock;
{
  struct sockaddr_in addr;
  /**/ int size = sizeof (struct sockaddr_in);

  if (getpeername (sock, (struct sockaddr *) &addr, &size) < 0) /* <--------- */
    return (sockerr_return (E_BAD_GETPEERNAME));
  return (ntohs (addr.sin_port));
}

/*
 * toggle_buffer_small_packets(sock, toggle)
 *
 * DESCRIPTION
 *     TCP normally bundles small packets together before sending them out.
 *     This is bad in situations where a small message mus be sent out soon.
 *     This functions tells TCP whether or not to delay small packets.
 * INPUT
 *     sock - fd of socket on which to operate
 * RETURN VALUE
 *     0     success
 *     -1     failure, errno is set to indicate the error
 */
int toggle_buffer_small_packets (s, toggle)
int s;
int toggle;
{
  int i;

  i = toggle ? 1 : 0;
  if (setsockopt (s, IPPROTO_TCP, TCP_NODELAY, (char *) &i, sizeof (i)) < 0)
    return (sockerr_return (E_BAD_SETSOCKOPT));
  else
    return (0);
}

int toggle_reuse_address (s, toggle)
int s;
int toggle;
{
  int i;

  i = toggle ? 1 : 0;
  if (setsockopt (s, SOL_SOCKET, SO_REUSEADDR, (char *) &i, sizeof (i)) < 0)
    return (sockerr_return (E_BAD_SETSOCKOPT));
  else
    return (0);
}

/*
 *     socket buffer sizes:
 *     TCP usually defaults to 4096 bytes
 *     4.3BSD usually has a maximum of around 52000 bytes
 */
int sndbuf_size (fd, size)
int fd;
int size;
{
  int r;                                /* return value */
  int optlen;                           /* length of value returned by getsockopt() */

  if (size > 0) {
    r = setsockopt (fd, SOL_SOCKET, SO_SNDBUF, (char *) &size, sizeof (int));
  } else {
    optlen = sizeof (int);
    if (getsockopt (fd, SOL_SOCKET, SO_SNDBUF, (char *) &r, &optlen) < 0)
      r = 0;
  }
  return (r);
}

int rcvbuf_size (fd, size)
int fd;
int size;
{
  int r;                                /* return value */
  int optlen;                           /* length of value returned by getsockopt() */

  if (size > 0) {
    r = setsockopt (fd, SOL_SOCKET, SO_RCVBUF, (char *) &size, sizeof (int));
  } else {
    optlen = sizeof (int);
    if (getsockopt (fd, SOL_SOCKET, SO_RCVBUF, (char *) &r, &optlen) < 0)
      r = 0;
  }
  return (r);
}
