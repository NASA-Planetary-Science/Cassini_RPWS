
#ifndef _SOCKET_CALLS_H
#define _SOCKET_CALLS_H

#include <sys/types.h>
#include <netinet/in.h>
typedef struct sockaddr_in address_t;

#define E_BAD_ARGS          (-1)
#define E_BAD_SOCKET          (-2)
#define E_BAD_BIND          (-3)
#define E_BAD_GETSOCKNAME     (-4)
#define E_BAD_GETHOSTNAME     (-5)
#define E_BAD_GETHOSTBYNAME     (-6)
#define E_BAD_LISTEN          (-7)
#define E_BAD_ACCEPT          (-8)
#define E_BAD_SETSOCKOPT     (-9)
#define E_BAD_ADDRESS          (-10)
#define E_BAD_CONNECT          (-11)
#define E_BAD_WRITE          (-12)
#define E_BAD_READ          (-13)
#define E_BAD_GETPEERNAME     (-14)

#ifndef _SOCKET_CALLS_
extern int sockerr_return (int errcode);
extern int new_server (int port, address_t * address_ptr);
extern int accept_connection (int server_sock, address_t * address_ptr);
extern int make_address (char *host, int port, struct sockaddr_in *addr);
extern int connect_to_server (char *host, int port, address_t * address_ptr);
extern int str_write (int fd, char *buf);
extern int buf_write (int fd, char *buf, int len);
extern int buf_read (int fd, char *buf, int len);
extern int str_read (int fd, char *buf, int maxlen);
extern char *get_addr_host (address_t * address_ptr);
extern int get_addr_port (address_t * address_ptr);
extern char *get_other_host (int sock);
extern int get_other_port (int sock);
extern int toggle_buffer_small_packets (int s, int toggle);
extern int toggle_reuse_address (int s, int toggle);
extern int sndbuf_size (int fd, int size);
extern int rcvbuf_size (int fd, int size);
#endif /* _SOCKET_CALLS_ */
#endif /* _SOCKET_CALLS_H */
