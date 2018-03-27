
/* PCRTIU.H */

#ifndef _pcrtiu_h
#define _pcrtiu_h
void pcrtiu_sockopt (int);
int pcrtiu_getCDS (unsigned char *);
int pcrtiu_putCDS (unsigned char *);
int pcrtiu_init_socket (char *, int);
char *pcrtiu_hostname (char *, int, char **);
#endif
