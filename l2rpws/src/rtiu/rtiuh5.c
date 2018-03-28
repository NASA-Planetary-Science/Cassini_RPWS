
/****************************************************************
 * File Name :  rtiuh.c
 *
 * Purpose :    Program to run on Unix machine, which interfaces
 *              with the HEURIKON RTIU. This program receives data
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
 *  @(#) FILE ID 0.0 rtiuh.c
 *  @(#) FROM    -r0.0 /users/robison/cassini/rtiu/rtiuh.c 09/11/95
 *  @(#) ID STR  rtiuh.c 0.0 09/11/95
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

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include <time.h>
#include <strings.h>
#include <unistd.h>

#include "utilo.h"

#define RPWS_RTIU_PORT      5555        /* default port */
#define RPWS_PPC_PORT       2007        /* default port */
#define RPWS_RTIU_HOST  "128.255.10.165"        /* default host */
#define BUFFER_SIZE 8192
#define _rtiuh_

#include "rtiuh.h"

#define ACTUAL_REQ_HDR_SIZE     (sizeof (struct rtiu_header))
#define MIN_BCE_MSG_SIZE        ACTUAL_REQ_HDR_SIZE

/****************************************************************************/
/* Compiled in config file directory */

#ifndef CFG
#error Compiled in configuration directory is missing.
#endif

/****************************************************************************/

extern int errno;

static int rtiu_debug_flag = 0;

static char *rtiu_version = { "rtiuh V5.0" };
static int xmit_socket = 0;             /* rtiu_socketet descriptor */
static int ppc_socket = 0;              /* new rtiu_socket descriptor */
static int recv_socket = 0;             /* rtiu_socketet descriptor */
static char af_host[256] = { RPWS_RTIU_HOST };
static int af_type = SOCK_STREAM;       /* rtiu_socketet af_type */
static int af_protocol = 0;             /* IP af_protocol */
static int af_port = RPWS_RTIU_PORT;    /* IP port */
static int ppc_port = RPWS_PPC_PORT;    /* IP port */
static struct hostent *hp;
static struct hostent *rtiu;
static struct hostent *ppc;
static char host_machine_name[MAXHOSTNAMELEN + 1];
static char transaction_filename[256];

static unsigned short current_mode[FL_define_TLM_mode / 2] = { FL_define_TLM_mode,      /* length */
  FC_define_TLM_mode,                   /* functon code */
  0,                                    /* status */
  0,                                    /* sequence */
  0,                                    /* value */
  0, 0,                                 /* seconds */
  0,                                    /* rti */
  60 * 0
};
static unsigned short set_sct[FL_define_SCT / 2] = { FL_define_SCT,     /* length */
  FC_define_SCT,                        /* functon code */
  0,                                    /* status */
  0,                                    /* sequence */
  0,                                    /* value */
  0, 0,                                 /* seconds */
  0,                                    /* rti */
  3 * 0
};
static unsigned short run_sct[FL_define_SCT_enable / 2] = { FL_define_SCT_enable,       /* length */
  FC_define_SCT_enable,                 /* functon code */
  0,                                    /* status */
  0,                                    /* sequence */
  1,                                    /* value */
  0, 0,                                 /* seconds */
  0
};                                      /* rti */
static unsigned short stop_sct[FL_define_SCT_enable / 2] = { FL_define_SCT_enable,      /* length */
  FC_define_SCT_enable,                 /* functon code */
  0,                                    /* status */
  0,                                    /* sequence */
  0,                                    /* value */
  0, 0,                                 /* seconds */
  0
};                                      /* rti */


static unsigned short rtad[FL_define_RT_address / 2] = { FL_define_RT_address,  /* length */
  FC_define_RT_address,                 /* functon code */
  0,                                    /* status */
  0,                                    /* sequence */
  25,                                   /* value */
  0, 0,                                 /* seconds */
  0
};                                      /* rti */

static unsigned short echo[FL_define_handshake / 2] = { FL_define_handshake,    /* length */
  FC_define_handshake,                  /* functon code */
  0,                                    /* status */
  0,                                    /* sequence */
  1,                                    /* value */
  0, 0,                                 /* seconds */
  0
};                                      /* rti */
static unsigned short noecho[FL_define_handshake / 2] = { FL_define_handshake,  /* length */
  FC_define_handshake,                  /* functon code */
  0,                                    /* status */
  0,                                    /* sequence */
  0,                                    /* value */
  0, 0,                                 /* seconds */
  0
};                                      /* rti */
static unsigned short run[FL_define_mode / 2] = { FL_define_mode,       /* length */
  FC_define_mode,                       /* functon code */
  0,                                    /* status */
  0,                                    /* sequence */
  1,                                    /* value */
  0, 0,                                 /* seconds */
  0
};                                      /* rti */
static unsigned short data[FL_define_collection / 2] = { FL_define_collection,  /* length */
  FC_define_collection,                 /* functon code */
  0,                                    /* status */
  0,                                    /* sequence */
  0,                                    /* value */
  0, 0,                                 /* seconds */
  0
};                                      /* rti */

static unsigned short result[16];

static struct mssg
{
  int status;
  char *message;
  char *details;
};
static struct mssg msgs[] = {
  1, "normal", NULL,
  1, "normal status", NULL,
  RTIU_BAD_SOCKET, "RTIU_BAD_SOCKET", NULL,
  RTIU_BAD_SOURCE, "RTIU_BAD_SOURCE", NULL,
  RTIU_BAD_TARGET, "RTIU_BAD_TARGET", NULL,
  RTIU_BAD_CONNECT, "RTIU_BAD_CONNECT", NULL,
  RTIU_BAD_TRANSACTION, "RTIU_BAD_TRANSACTION", NULL,
  RTIU_BAD_SEND, "RTIU_BAD_SEND", NULL,
  RTIU_BAD_RECV, "RTIU_BAD_RECV", NULL,
  RTIU_BAD_TRANS_LOAD, "RTIU_BAD_TRANS_LOAD", NULL,
  RTIU_BAD_TRANS_FILE, "RTIU_BAD_TRANS_FILE", NULL,
  RTIU_BAD_BIND, "RTIU_BAD_BIND", NULL,
  RTIU_REDUNDANT_INIT, "RTIU_REDUNDANT_INIT", NULL,
  RTIU_BAD_TRANS_TABLE, "RTIU_BAD_TRANS_TABLE", NULL,
  RTIU_BAD_DATA, "RTIU_BAD_DATA", NULL,
  0, "unknown status", NULL
};

static struct mssg sa_mne[] = {
  0, "", NULL,
  1, "BIU memory", NULL,
  2, "BIU memory", NULL,
  3, "BIU discrete", NULL,
  7, "nominal commands", NULL,
  8, "critical commands", NULL,
  9, "fault protection commands", NULL,
  10, "ancillary data", NULL,
  11, "science data", NULL,
  12, "housekeeping", NULL,
  29, "mode & time", NULL,
  30, "data wrap", NULL,
  0, "", NULL
};

static struct fcmssg
{
  int status;
  char *message;
  char *val_dis;
  char *val_ena;
  char *details;
};
static struct fcmssg fc[] = {
  0, "FC_failure", NULL, NULL, "rtiu_fc failure",
  FC_return_data, "FC_return_data", NULL, NULL, "data record from RTIU",
  FC_return_mode, "FC_return_mode", NULL, NULL, "mode code from RTIU",
  FC_return_hand, "FC_return_hand", "disable ", "enable ",
    "handshake from RTIU",
  FC_define_RT_address, "FC_define_RT_address", NULL, NULL,
    "define 1553 RT address",
  FC_define_SCT, "FC_define_SCT", NULL, NULL, "set 32 bit S/C time",
  FC_define_SCT_enable, "FC_define_SCT_enable", "disable ", "enable ",
    "autonomous time b-cast",
  FC_define_handshake, "FC_define_handshake", "disable ", "enable ",
    "rtiu handshake",
  FC_define_TLM_mode, "FC_define_TLM_mode", NULL, NULL,
    "setup data collection schedule",
  FC_define_collection, "FC_define_collection", "disable ", "enable ",
    "return of 1553 traffic",
  FC_define_mode, "FC_define_mode", "pause ", "run ", "mode",
  FC_send_CDS_command, "FC_send_CDS_command", NULL, NULL, "send command(ALF)",
  FC_send_mode_code, "FC_send_mode_code", NULL, NULL, "mode code to BIU",
  0, "FC_unknown", NULL, NULL, "unknown function code"
};

int rtiu_debug (int flag)
{
  int temp;

  temp = rtiu_debug_flag;
  rtiu_debug_flag = flag;
  return temp;
}

  /*
   *    misc conversion  routines...
   */
long rtiu_flag (unsigned short *buf)
{
  if (buf[FP_value])
    return 1;
  else
    return 0;
}
long rtiu_short (unsigned short *buf)
{
  return buf[0] & 0xFFFF;
}
long rtiu_long (unsigned short *buf)
{
  return buf[0] << 16 | buf[1] & 0xFFFF;
}
long rtiu_word (unsigned char *buf)
{
  return buf[0] << 8 | buf[1] & 0xFF;
}
long rtiu_time_bump (unsigned short *src, unsigned short *dest, int delta)
{
  long time;

  time = src[0] << 16 | src[1] << 0;
  time = time + delta;
  if (dest) {
    dest[0] = time >> 16;
    dest[1] = time & 0xFFFF;
  }
  return time;
}
long rtiu_time_sct (long delta, long now)
{
  long time;
  long temp[2];

  if (now) {
    time = now;
  } else {
    temp[0] = set_sct[FP_time_seconds_lsw];
    temp[0] = temp[0] & 0xFFFF;
    temp[1] = set_sct[FP_time_seconds_msw] << 16;
    time = temp[0] | temp[1];
  }
  time = time + labs (delta);
  return time;
}
int rtiu_swap (unsigned short *buf)
{
  int i;
  int start, len;
  unsigned char msb, lsb;
  static unsigned short obuf[BUFFER_SIZE];

  len = buf[FP_length] / 2;
  switch (buf[FP_function_code]) {
   default:
     return -1;
   case FC_return_data:
     start = FP_telemetry;
     if (len < FP_telemetry)
       return 0;
     break;
   case FC_return_mode:
   case FC_return_hand:
   case FC_define_RT_address:
   case FC_define_SCT:
   case FC_define_SCT_enable:
   case FC_define_handshake:
   case FC_define_TLM_mode:
   case FC_define_collection:
   case FC_define_mode:
   case FC_send_mode_code:
     return 0;
     break;
   case FC_send_CDS_command:
     start = FP_command;
     if (len < FP_command)
       return 0;
     break;
  }

  for (i = start; i < len; i++) {
    msb = buf[i] >> 8 & 0xFF;
    lsb = buf[i] >> 0 & 0xFF;
    buf[i] = msb << 0 | lsb << 8;
  }
  return start - len + 1;
}

  /*
   *    misc dump routines...
   */
char *rtiu_mssg (int status)
{
  int index = 1;

  while (1) {
    if (status > 0)
      return msgs[index].message;
    if (!msgs[index].status)
      return msgs[index].message;
    if (msgs[index].status == status)
      return msgs[index].message;
    index += 1;
  }
  /* return msgs[1].message; */
}
char *rtiu_fc (unsigned short *buf)
{
  int index = 1;
  int func;

  func = rtiu_short (&buf[FP_function_code]);
  while (1) {
    if (!fc[index].status)
      return fc[index].message;
    if (fc[index].status == func)
      return fc[index].message;
    index += 1;
  }
  /* return fc[0].message; */
}
char *ppc_fcd (unsigned short *buf)
{
  static char *code[] = { "Unknown",
    "RTIUCMD",
    "SECMD",
    "Unknown",
    ""
  };
  int cmd_type;

  cmd_type = buf[1];
  switch (cmd_type) {
   case 0x00AA:
     return code[1];
   case 0x00BB:
     return code[2];
   default:
     return code[0];
  }
}
char *rtiu_fcd (unsigned short *buf)
{
  int index = 1;
  int func;
  static char result[256];

  func = rtiu_short (&buf[FP_function_code]);
  result[0] = 0;
  while (1) {
    if (!fc[index].status)
      return fc[index].details;
    if (fc[index].status == func) {
      switch (rtiu_flag (buf)) {
       default:
         break;
       case 0:
         if (fc[index].val_dis)
           strcat (result, fc[index].val_dis);
         break;
       case 1:
         if (fc[index].val_ena)
           strcat (result, fc[index].val_ena);
         break;
      }
      strcat (result, fc[index].details);
      return result;
    }
    index += 1;
  }
  /* return fc[0].message; */
}
void rtiu_dump_packet_char (unsigned char *header, char *string)
{
  int i;
  int wid = 16;
  static char dash[] = { "--------------------" };
  char *stg = dash;

  if (string)
    stg = string;
  printf ("rtiu packet dump %s%s\n", dash, stg);
  printf ("  Lgth  F.C.  Sts   Seq   Val   msb Sc lsb  RTI    %s\n",
          rtiu_fcd ((unsigned short *) header));
  for (i = 0; i < rtiu_word (&header[FP_length]); i += 2) {
    printf ("  %4.4X", rtiu_word (&header[i]));
    if ((i % wid) == (wid - 2))
      printf ("\n");
  }
  if (rtiu_word (&header[FP_length * 2]) % wid)
    printf ("\n");
  return;
}
void rtiu_dump_packet (unsigned short *header, char *string)
{
  int i;
  int wid = 8;
  int len;
  static char dash[] = { "--------------------" };
  char *stg = dash;

  if (string)
    stg = string;
  printf ("rtiu packet dump %s%s\n", dash, stg);
  printf ("  Lgth  F.C.  Sts   Seq   Val   msb Sc lsb  RTI  %s\n",
          rtiu_fcd (header));
  len = rtiu_short (&header[FP_length]) / 2;
  if (len < 8)
    len = 8;
  if (len > 1024)
    len = 8;
  for (i = 0; i < len; i++) {
    printf ("  %4.4X", rtiu_short (&header[i]));
    if ((i % wid) == (wid - 1))
      printf ("\n");
  }
  if (rtiu_short (&header[FP_length]) / 2 % wid)
    printf ("\n");
  return;
}
void ppc_dump_packet (unsigned short *header, char *string)
{
  int i;
  int wid = 8;
  int len;
  static char dash[] = { "--------------------" };
  char *stg = dash;

  if (string)
    stg = string;
  printf ("ppc packet dump %s%s\n", dash, stg);
  printf ("  Lgth  Type  msb Sc lsb   RTI  strlen %s\n", ppc_fcd (header));
  for (i = 0; i < 5; i++) {
    printf ("  %4.4X", header[i]);
  }
  printf ("  %6d [%s]\n", strlen ((char *) &header[5]), &header[5]);
  return;
}
char *rtiu_dump_mode_mne (int sa)
{
  int index = 1;

  while (1) {
    if (!sa_mne[index].status)
      return sa_mne[index].message;
    if (sa_mne[index].status == sa)
      return sa_mne[index].message;
    index += 1;
  }
  /* return sa_mne[0].message; */
}
void rtiu_dump_mode (unsigned short *header)
{
  int sa;
  int index;
  int bits, pdelta, ndelta, delta;
  char *sdelta;

  printf ("  SubAddress    Word Count    Bit Rate      Delta RTI\n");
  for (sa = 1; sa < 31; sa++) {
    index = ((sa - 1) * 3) + FP_data;
    if (header[index]) {
      bits = rtiu_short (&header[index]) * 16 * 8;
      pdelta = bits / rtiu_long (&header[index + 1]);
      ndelta = rtiu_long (&header[index + 1]) / bits;
      if (ndelta) {
        delta = ndelta;
        sdelta = "/RTI";
      }
      if (pdelta) {
        delta = pdelta;
        sdelta = "    ";
      }
      printf ("    %4d       %8d      %8d    %8d%s   %s\n",
              sa,
              rtiu_short (&header[index]),
              rtiu_long (&header[index + 1]),
              delta, sdelta, rtiu_dump_mode_mne (sa));
    }
  }
  return;
}
void rtiu_dump_header (unsigned short *header, char *string)
{
  int i;
  int wid = 8;
  static char dash[] = { "--------------------" };
  char *stg = dash;

  if (string)
    stg = string;
  printf ("rtiu header dump %s%s\n", dash, stg);
  printf ("  Lgth  F.C.  Sts   Seq   Val   msb Sc lsb  RTI  %s\n",
          rtiu_fcd (header));
  for (i = 0; i < 8; i++) {
    printf ("  %4.4X", rtiu_short (&header[i]));
  }
  printf ("\n");
  return;
}
char *rtiu_dfa (char *addr)
{
  static char res[256];

  sprintf (res, "%d.%d.%d.%d",
           (unsigned char) addr[0],
           (unsigned char) addr[1],
           (unsigned char) addr[2], (unsigned char) addr[3]);
  return res;
}
void rtiu_dump_time (unsigned short *time)
{
  printf ("struct rtiu_time\n");
  printf ("    seconds          %4d\n",
          (time[FP_seconds_msw] << 16) + time[FP_seconds_lsw]);
  printf ("    rti              %4d\n", time[FP_rti]);
  return;
}
char *rtiu_dump (int flag)
{
  switch (flag) {
   default:
     break;
   case RTIU_DUMP_current_transaction:
     rtiu_dump_header (current_mode, NULL);
   case RTIU_DUMP_current_transaction_table:
     rtiu_dump_mode (current_mode);
     break;
   case RTIU_DUMP_sct:
     rtiu_dump_header (set_sct, NULL);
   case RTIU_DUMP_sct_table:
     rtiu_dump_time (&set_sct[FP_to_time]);
     break;
   case RTIU_DUMP_targetname:
     if (af_host)
       printf ("name       rtiu_target_name: %s\n", af_host);
     else
       printf ("name       rtiu_target_name: UNKNOWN\n");
     break;
   case RTIU_DUMP_trans_file:
     printf ("file           transaction file: %s\n", transaction_filename);
     break;
   case RTIU_DUMP_hostname:
     if (host_machine_name)
       printf ("name         rtiu_host_name: %s\n", host_machine_name);
     else
       printf ("name         rtiu_host_name: UNKNOWN\n");
     break;
   case RTIU_DUMP_targetaddr:
     if (rtiu)
       printf ("address rtiu_target_address: %s\n", rtiu_dfa (rtiu->h_addr));
     else
       printf ("address rtiu_target_address: UNKNOWN\n");
     break;
   case RTIU_DUMP_hostaddr:
     if (hp)
       printf ("address   rtiu_host_address: %s\n", rtiu_dfa (hp->h_addr));
     else
       printf ("address   rtiu_host_address: UNKNOWN\n");
     break;
   case RTIU_STRING_targetname:
     return af_host;
   case RTIU_STRING_hostname:
     return host_machine_name;
   case RTIU_STRING_version:
     return rtiu_version;
  }
  return NULL;
}

/****************************************************************
 * Returns: >0 - successful (packet size)			*
 *          RTIU_BAD_SOCKET - network problem (rtiu_socket) 	*
 *	    RTIU_BAD_RECV - network problem (send) 		*
 ****************************************************************/
int rtiu_get (unsigned short *tlm_pkt)
{
  int pkt_hdr = 0;
  int pkt_body = 0;
  int pkt_body_t = 0;
  int pkt_len = 0;
  int pkt_tot = 0;
  int i;

#ifdef debug001
  fprintf (stderr, "--------------------------------------\n");
#endif

  if (!recv_socket)
    return RTIU_BAD_SOCKET;

  if (rtiu_debug_flag & 0x02)
    fprintf (stderr, "rtiu_get\n");
  memset (&tlm_pkt[FP_data], 0x00, 128);

  pkt_hdr = recv (recv_socket, tlm_pkt, FL_header_size, 0);

#ifdef debug001
  fprintf (stderr, "rtiuh.c 1 %4d=recv(sck,pkt[   0],%d,0)\n                ",
           pkt_hdr, FL_header_size);
  for (i = 0; i < pkt_hdr / 2; i++)
    fprintf (stderr, "%4.4X ", tlm_pkt[i]);
  fprintf (stderr, "\n");
#endif

  if (pkt_hdr < 0) {
    fprintf (stderr,
             "%s=rtiu_get_header(%d, %p, %d, %X)\n",
             strerror (errno), recv_socket, tlm_pkt, FL_header_size, 0);
    return RTIU_BAD_RECV;
  }
  if (pkt_hdr != FL_header_size)
    return RTIU_BAD_DATA;
  pkt_body = tlm_pkt[FP_length] - FL_header_size;

  if ((pkt_body > 2048) | (pkt_body < 0))
    pkt_body = 0;
  else {
    pkt_tot = 0;
    pkt_body_t = pkt_body;
    while (pkt_tot < pkt_body) {
      i = FL_header_size + pkt_tot;
      pkt_len = recv (recv_socket, &tlm_pkt[i / 2], pkt_body_t, 0);
      pkt_tot += pkt_len;
      pkt_body_t -= pkt_len;

#ifdef debug001
      fprintf (stderr, "  %4d    %4d=recv(sck,pkt[%4d],%d,0)\n",
               pkt_tot, pkt_len, i, pkt_body_t);
#endif
    }
  }

#ifdef debug001
  fprintf (stderr, "                data  ");
  for (i = 0; i < 8; i++)
    fprintf (stderr, "%4.4X ", tlm_pkt[i + FP_data]);
  fprintf (stderr, "\n");
  fprintf (stderr, "                telem ");
  for (i = 0; i < 8; i++)
    fprintf (stderr, "%4.4X ", tlm_pkt[i + FP_telemetry]);
  fprintf (stderr, "\n");
#endif


  if (rtiu_debug_flag & 0x02)
    rtiu_dump_packet (tlm_pkt, "rtiu_get");

  if (pkt_len < 0) {
    int i;

    for (i = 0; i < 16; i += 2)
      fprintf (stderr, "%2.2X%2.2X ", tlm_pkt[i], tlm_pkt[i + 1]);
    fprintf (stderr,
             "%s=rtiu_get_body(%d, %p, %d, %X)\n",
             strerror (errno), recv_socket, tlm_pkt, pkt_body, 0);
    fprintf (stderr, "%d = FL_header_size\n", FL_header_size);
    fprintf (stderr, "%d = tlm_pkt[FP_length]\n", tlm_pkt[FP_length]);
    fprintf (stderr,
             "%d = tlm_pkt[FP_length]-FL_header_size\n",
             tlm_pkt[FP_length] - FL_header_size);
    return RTIU_BAD_RECV;
  }

  return pkt_hdr + pkt_len;
}

/****************************************************************
 *	rtiu_put and secmd_put may require breakup into		*
 *	several lines.  This routine attempts to break on a	*
 *	space boundary.  Breaks are clean with only the "\"	*
 *	character added to the string				*
 ****************************************************************/
static int Line_Len = 960;
static int send_long (int socket,       /* socket */
                      char *tlm_pkt,    /* buffer */
                      size_t pkt_size,  /* size */
                      int flags)
{                                       /* flags */
  int status = 0;
  int length;                           /* length if 1 packet !!! */
  int i;
  int index = 0;
  union Buffer
  {
    char c[BUFFER_SIZE];
    short s[1];
  } buffer;

  length = strlen (&tlm_pkt[10]);
  for (i = 0; i < 10; i++)              /* copy packet   */
    buffer.c[i] = tlm_pkt[index++];     /*  header       */

  while (index < pkt_size) {
    memset (&buffer.c[10], 0, Line_Len);
    for (i = 0; i < Line_Len; i++) {    /* begining of line  *//*                   */
      buffer.c[i + 10] = tlm_pkt[index++];      /* zero terminated   */
      buffer.c[i + 11] = 0;             /*                   */
      if (buffer.c[i + 10] == 0)        /* string; 0 at end  */
        break;                          /*  we're done       */
    }                                   /*                   */
    if (buffer.c[i + 9]) {              /* end of string ?   *//*                   */
      for (i = Line_Len; i < Line_Len + 16; i++) {      /* scan for space    *//*                   */
        buffer.c[i + 10] =              /* move text&space   */
          tlm_pkt[index++];             /*  to output buff.  */
        if (buffer.c[i + 10] == ' ')    /*  test for space   */
          break;                        /*                   */
      }                                 /*                   */
      buffer.c[i + 11] = '\\';          /* insert cont       */
      buffer.c[i + 12] = 0;             /* delimit string    */
    }

    /*
     * send partial line 
     */
    /*
     * text string, so   
     */
    /*
     * strlen is OK      
     */
    buffer.s[0] = strlen (&buffer.c[10]) + 10;
    i = buffer.s[0];
    switch (buffer.s[1]) {
     case 0xAA:
       fprintf (stdout, "RTIUCMD ");
       break;
     case 0xBB:
       fprintf (stdout, "SECMD ");
       break;
     default:
       fprintf (stdout, "unknown ");
       break;
    }
    fprintf (stdout, "%02X %02X %04X%04X.%01X " "| " "%s\n", buffer.s[0] & 0xFFFF,      /* Length */
             buffer.s[1] & 0xFFFF,      /* Cmd Type AA or BB */
             buffer.s[2] & 0xFFFF,      /* Time MSB */
             buffer.s[3] & 0xFFFF,      /* Time LSB */
             buffer.s[4] & 0x7,         /* Time RTI */
             &buffer.c[10]);            /*  */
    status = send (socket, buffer.s, i, flags); /*  */
    if (status < 1)
      return status;
  }
  return length + 10;                   /* add in header */
}

/****************************************************************
 * Returns: >0 - successful (packet size)			*
 *          RTIU_BAD_SOCKET - network problem (xmit_socket) 	*
 *          RTIU_BAD_SEND - network problem (send) 		*
 ****************************************************************/
int rtiu_put (unsigned short *tlm_pkt)
{
  int pkt_size = 0;
  int status;

  if (rtiu_debug_flag & 0x01)
    rtiu_dump_packet (tlm_pkt, "rtiu_put");
  if (!xmit_socket)
    return RTIU_BAD_SOCKET;
  pkt_size = rtiu_short (&tlm_pkt[FP_length]);
  if (pkt_size >= 16) {
    status = send (xmit_socket, (char *) tlm_pkt, pkt_size, 0);
    if (status < 0)
      perror (strerror (errno));
    if (pkt_size != status) {
      perror (strerror (errno));
      return RTIU_BAD_SEND;
    }
  }
  return pkt_size;
}


int ppc_put (short *tlm_pkt)
{
  int pkt_size = 0;
  int status;

  if (rtiu_debug_flag & 0x01)
    ppc_dump_packet ((unsigned short *) tlm_pkt, "ppc_put");
  if (!ppc_socket)
    return RTIU_BAD_SOCKET;
  pkt_size = tlm_pkt[FPppc_Length];
  if (pkt_size > 10) {
    status = send_long (ppc_socket, (char *) tlm_pkt, pkt_size, 0);
    if (status < 0)
      perror (strerror (errno));
    if (pkt_size != status) {
      perror (strerror (errno));
      return RTIU_BAD_SEND;
    }
  }
  return pkt_size;
}
int striplen (char *cmd)
{
  int i;

  for (i = 0; i < strlen (cmd); i++)
    switch (cmd[i]) {
     case 0x0A:
     case 0x0D:
       cmd[i] = 0;
       break;
     default:
       break;
    }
  return strlen (cmd);
}
int rtiucmd_put5 (char *command, int line_len, int time)
{
  int status;

  struct TLM
  {
    short length;
    short type;
    int time_seconds;
    short time_rti;
    char command[2048];
  };
  static struct TLM tlm;

  memset (tlm.command, 0, sizeof (tlm.command));

  if (line_len)
    Line_Len = line_len;

  tlm.length = striplen (command) + 10;
  tlm.type = 0x00AA;
  tlm.time_seconds = time;
  tlm.time_rti = 0;
  strcpy (tlm.command, command);
  status = ppc_put (&tlm.length);
  return status;
}
int cdsseq_put5 (char *command, int line_len, int time)
{
  int status;

  struct TLM
  {
    short length;
    short type;
    int time_seconds;
    short time_rti;
    char command[2048];
  };
  static struct TLM tlm;

  memset (tlm.command, 0, sizeof (tlm.command));

  if (line_len)
    Line_Len = line_len;

  tlm.length = striplen (command) + 10;
  tlm.type = 0x00BB;
  tlm.time_seconds = time;
  tlm.time_rti = 0;
  strcpy (tlm.command, command);
  status = ppc_put (&tlm.length);
  return status;
}
int rtiu_put5 (unsigned short *tlm_pkt)
{
  static char buffer[16384];
  int pkt_length;
  int ilen;
  int i;
  int pkt_time;

  if (rtiu_debug_flag & 0x01)
    /**/ rtiu_dump_packet (tlm_pkt, "rtiu_put");

  pkt_length = rtiu_short (&tlm_pkt[FP_length]) - 16;
  pkt_length = pkt_length / 2;          /* command word count */
  pkt_time = ((tlm_pkt[FP_seconds_msw] & 0xFFFF) << 16) |
    (tlm_pkt[FP_seconds_lsw] & 0xFFFF);
  switch (tlm_pkt[FP_function_code]) {
   case 0xFC0A:
     if (pkt_length) {
       sprintf (buffer, "CDSCMD %2d %2d %3d",
                tlm_pkt[FP_value] >> 8 & 0xFF,
                tlm_pkt[FP_value] >> 0 & 0xFF, pkt_length);
       for (i = 0; i < pkt_length; i++) {
         ilen = strlen (buffer);
         sprintf (&buffer[ilen], " %04X", tlm_pkt[i + 8] & 0xFFFF);
       }
       cdsseq_put5 (buffer, 0, pkt_time);
     }
     break;
   default:
     fprintf (stderr,
              "Undecoded Heurikon function code %04X using rtiu_put5\n",
              tlm_pkt[FP_function_code]);
     rtiu_put (tlm_pkt);
     break;
  }
  return pkt_length;
}
int rtiu_time5 (unsigned short *to_time,
                unsigned short *from_time, int enable)
{
  int status = 0;
  char buffer[256];
  int pkt_time = 0;

  if (from_time)
    pkt_time = (from_time[0] << 16) | from_time[1];
  if (to_time)
    sprintf (buffer, "SETSCT 0x%04X%04X", to_time[0], to_time[1]);
  else
    sprintf (buffer, "SETSCT 0x%08X", time (NULL));
  if (enable)
    status = cdsseq_put5 (buffer, 0, pkt_time);
  return status;
}
int rtiu_init5 (char *table)
{
  return 0;
}
int rtiu_table_scan5 (char *file, char *mode, long when, int enable)
{
  return 0;
}

/****************************************************************
 * SETS/CLEARS misc flags					*
 * Returns: rtiu_put status					*
 ****************************************************************/
int rtiu_rtad (int addr)
{
  int status;

  if (addr)
    rtad[FP_value] = addr;
  status = rtiu_put (rtad);
  return status;
}
int rtiu_echo (int flag)
{
  int status;

  echo[FP_value] = flag;
  status = rtiu_put (echo);
  return status;
}
int rtiu_data (int flag)
{
  int status;

  data[FP_value] = flag;
  status = rtiu_put (data);
  return status;
}
int rtiu_run (int flag)
{
  int status;

  run[FP_value] = flag;
  status = rtiu_put (run);
  return status;
}

/****************************************************************
 * Argument(s):							*
 *	1. flag to shut down rtiu, normally 1			*
 *								*
 * Returns: >0 - successful (packet size)			*
 *          RTIU_BAD_SOCKET - network problem (rtiu_socket) 	*
 ****************************************************************/
int rtiu_close (int flag)
{
  if (!(recv_socket | xmit_socket))
    return RTIU_BAD_SOCKET;
  if (flag) {
    if (echo[FP_value])
      rtiu_put (noecho);
    echo[FP_value] = 0;
    rtiu_data (0);
  }
  if (rtiu_debug_flag & 0x08)
    fprintf (stderr, "rtiu_close\n");
  if (recv_socket)
    shutdown (recv_socket, 2);
  if (xmit_socket)
    shutdown (xmit_socket, 2);
  if (recv_socket)
    close (recv_socket);
  if (xmit_socket)
    close (xmit_socket);
  if (ppc_socket)
    close (ppc_socket);
  recv_socket = 0;
  xmit_socket = 0;
  return 1;
}

/****************************************************************
 * Set S/C time (using epoch)					*
 ****************************************************************/
time_t rtiu_ut (time_t * now)
{
  time_t ut;

  time (&ut);
  now[0] = ut - utilo_epoch (NULL);
  return now[0];
}

/****************************************************************
 * Returns: rtiu_put status					*
 ****************************************************************/
int rtiu_command (unsigned short *command, unsigned short *reply)
{
  int status_c;
  int status_r;

  status_c = rtiu_put (command);
  if (status_c < 0) {
    rtiu_close (0);
    return status_c;
  }
  if (echo[FP_value]) {
    status_r = rtiu_get (reply);
    if (status_r < 0) {
      rtiu_close (0);
      return status_r;
    }
  } else
    bzero (reply, FL_header_size);
  return status_c;
}

  /****************************************************************
   *	load collection schedule
   *		1st. argument is transaction table
   *			NULL uses default file
   *		2nd. argument is telemetry mode
   *			NULL uses 1st. table
   *		3rd. argument is enable flag
   *			for I/O activity
   *
   *	return 1 is successful load
   *	return RTIU_BAD_TRANS_FILE  file not found
   *	return RTIU_BAD_TRANS_LOAD  file not decoded correctly
   *	return RTIU_BAD_TRANS_TABLE entry not found
   ****************************************************************/
int rtiu_table (char *file, char *mode, long when, int enable)
{

 /* 
#ifdef __RPWSHP2__
  static char deflt[] = { "/usr/ cassini/ cfg/ rtiuh.trs" };
#else
  static char deflt[] = { "/opt/ project/ cassini/ cfg/ rtiuh.trs" };
#endif
 */
  
  static char deflt[] = CFG "/rtiuh.trs";
 
  static char delim[] = { " ,\t\n" };
  static char temp[128];

  char i_mode[32] = { "" };
  char i_comment[128] = { "" };
  char i_rate[16] = { "" };

  int flag = 0;
  FILE *fp = NULL;
  char line[81];
  char *token[9];
  int sub_address;
  int word_count;
  int word_rate;
  int status;
  int active;
  int index;
  int i;
  int table_flag;
  long delta;

  time_t now;

  bzero (&current_mode[FP_data], FD_define_TLM_mode);

  if (file)
    strcpy (transaction_filename, file);
  else
    strcpy (transaction_filename, deflt);

  active = 0;
  table_flag = 0;

  current_mode[FP_seconds_msw] = 0;
  current_mode[FP_seconds_lsw] = 0;
  current_mode[FP_rti] = 0;
  if (when > 0) {
    current_mode[FP_seconds_msw] = when >> 16;
    current_mode[FP_seconds_lsw] = when & 0xFFFF;
    current_mode[FP_rti] = 0;
  }
  if (when == 0) {
    rtiu_ut (&now);
    delta = rtiu_time_sct (30, now);
    current_mode[FP_seconds_msw] = delta >> 16;
    current_mode[FP_seconds_lsw] = delta & 0xFFFF;
    current_mode[FP_rti] = 0;
  }
  if (when < 0) {
    delta = rtiu_time_sct (-when, now);
    current_mode[FP_seconds_msw] = delta >> 16;
    current_mode[FP_seconds_lsw] = delta & 0xFFFF;
    current_mode[FP_rti] = 0;
  }
  fp = fopen (transaction_filename, "r");
  if (fp) {
    while (fgets (line, 80, fp)) {
      strcpy (temp, line);
      temp[strlen (temp) - 1] = 0;
      token[0] = strtok (line, delim);

      if (token[0]) {
        for (i = 1; i < 8; i++) {
          token[i] = NULL;
          if (token[i - 1])
            token[i] = strtok (NULL, delim);
        }
        if (!strcasecmp (token[0], "table")) {
          if (!mode)
            active = 1;
          if (!strcasecmp (token[1], mode))
            active = 1;
          table_flag = 1;
        }

        if (!strcasecmp (token[0], "endtable")) {
          table_flag = 0;
        }

        if (table_flag) {
          if (!strcasecmp (token[0], "mode"))
            if (mode[0] != '.')
              if (!strcasecmp (token[1], mode))
                active = 1;
        }

        if (active) {
          if (!strcasecmp (token[0], "endtable")) {
            if (i_rate[0])
              fprintf (stderr, "MODE %s %s b/S [%s] %8X\n",
                       i_mode, i_rate, i_comment, when);
            else
              fprintf (stderr, "MODE %s no telem [%s] %8X\n",
                       i_mode, i_comment, when);
            break;
          }
          if (!strcasecmp (token[0], "comment")) {
            for (i = 0; i < strlen (temp); i++)
              if (temp[i] == 0x09)
                temp[i] = 0x20;
            strcpy (i_comment, temp);
          }
          if (!strcasecmp (token[0], "mode")) {
            current_mode[FP_value] = 0;
            current_mode[FP_status] = 0;
            if (token[2])
              current_mode[FP_value] = strtol (token[2], (char **) NULL, 0);
            if (token[3])
              current_mode[FP_status] = strtol (token[3], (char **) NULL, 0);
            strcpy (i_mode, token[1]);
          }
          if (!strcasecmp (token[0], "entry")) {
            if (!strcasecmp (token[1], "telem"))
              strcpy (i_rate, token[4]);
            word_rate = strtol (token[4], (char **) NULL, 0);
            word_count = strtol (token[3], (char **) NULL, 0);
            sub_address = strtol (token[2], (char **) NULL, 0);
            index = FP_data + ((sub_address - 1) * 3);
            if (sub_address < 32 && sub_address > 0) {
              current_mode[index + 0] = word_count;
              current_mode[index + 1] = word_rate >> 16;
              current_mode[index + 2] = word_rate & 0xFFFF;
              flag += 1;
            }
          }
        }
      }
    }
    fclose (fp);
  } else
    return RTIU_BAD_TRANS_FILE;

  if (flag) {
    if (enable) {
      status = rtiu_put (current_mode);
      if (status < 0)
        return status;
    }
  } else
    return RTIU_BAD_TRANS_TABLE;

  return 1;
}
int rtiu_table_scan (char *file, char *mode, long when, int enable)
{

/*
#ifdef __RPWSHP2__
  static char deflt[] = { "/usr/ cassini/ cfg/ rtiuh.trs" };
#else
  static char deflt[] = { "/opt/ project /cassini /cfg /rtiuh.trs" };
#endif
  */

  static char deflt[] = CFG "/rtiuh.trs";
		  
  static char delim[] = { " ,\t\n" };
  static int item_count;
  static char *m_list;

  FILE *fp = NULL;
  char line[81];
  int i;
  char *token[8];

  if (!m_list) {
    if (file)
      strcpy (transaction_filename, file);
    else
      strcpy (transaction_filename, deflt);


    fp = fopen (transaction_filename, "r");
    if (fp) {
      while (fgets (line, 80, fp))
        if (strlen (line) > 1)
          if (line[0] != '#') {
            token[0] = strtok (line, delim);
            for (i = 1; i < 4; i++) {
              token[i] = NULL;
              if (token[i - 1])
                token[i] = strtok (NULL, delim);
            }
            if (token[0]) {
              if (!strcasecmp (token[0], "table"))
                item_count += 1;
              if (!strcasecmp (token[0], "mode"))
                item_count += 1;
            }
          }
      rewind (fp);
      i = (item_count * 16) + 128;
      m_list = (char *) malloc (i);
      item_count = 0;
      bzero (m_list, 16 * item_count + 128);
      while (fgets (line, 80, fp))
        if (strlen (line) > 1)
          if (line[0] != '#') {
            token[0] = strtok (line, delim);
            for (i = 1; i < 4; i++)
              token[i] = strtok (NULL, delim);

            if (token[0]) {
              if (!strcasecmp (token[0], "table")) {
                strcpy (&m_list[item_count * 16], token[1]);
                m_list[(item_count * 16) + 15] = 0;
                item_count += 1;
              }
              if (!strcasecmp (token[0], "mode")) {
                strcpy (&m_list[item_count * 16], token[1]);
                m_list[(item_count * 16) + 15] = 0;
                item_count += 1;
              }
            }
          }
    }
    fclose (fp);

  }
  for (i = 0; i < item_count; i++) {
    if (!strcasecmp (&m_list[i * 16], mode)) {
      if (enable)
        return rtiu_table (file, mode, when, enable);
      else
        return 1;
    }
  }
  return RTIU_INVALID_MODE;
}

/****************************************************************
 * set rtiu time						*
 *	to_time and from_time as defined in RTIU document	*
 * Returns: status from rtiu_put		 		*
 ****************************************************************/
 /*
  * rtiu_time - set time in rtiu (Heurikon)
  * *
  */
int rtiu_time (unsigned short *to_time, /* new SCLK */
               unsigned short *from_time,       /* when to change */
               int enable)
{                                       /* D0 set D1 run */
  time_t now;
  int status = 0;

  if (to_time)
    bcopy (to_time, &set_sct[FP_to_time], FL_time);
  else {
    rtiu_ut (&now);
    bcopy (&now, &set_sct[FP_to_time], FL_time_second);
  }

  if (from_time)
    bcopy (from_time, &set_sct[FP_from_time], FL_time);
  else
    bzero (&set_sct[FP_from_time], FL_time);

  rtiu_time_bump (&set_sct[FP_to_time], &current_mode[FP_seconds_msw], 10);
  if (!enable) {
    rtiu_dump_packet (set_sct, "Set time string");
  }
  if (enable & 1) {
    status = rtiu_put (set_sct);
    if (status < 0)
      return status;
  }
  if (enable & 2) {
    status = rtiu_put (run_sct);
    if (status < 0)
      return status;
  }
  return status;
}

/***************************************************************************
 * arguments:								   *
 *	char *host	host name					   *
 *			NULL uses default host				   *
 *			"DEFAULT" uses default host			   *
 *			any other string specifies rtiu name		   *
 *	char *table	transaction table				   *
 *			NULL skips processing (no table loaded)		   *
 *			"DEFAULT" uses default				   *
 *			any other string specifies file			   *
 *	int port_no	port number					   *
 *			zero uses some default				   *
 *	int dir         data direction                                     *
 *                      1 = receive                                        *
 *                      2 = transmit                                       *
 *									   *
 * Returns: >0 - successful (socket number)				   *
 *          RTIU_BAD_SOURCE - network problem (gethostname) 		   *
 *          RTIU_BAD_TARGET - host name could not be found 		   *
 *          RTIU_BAD_SOCKET - could not create rtiu_socketet		   *
 *          RTIU_BAD_CONNEC - could not connect to rtiu_socketet on host   *
 *          RTIU_BAD_TRANSACTION - transaction table not loaded		   *
 ***************************************************************************/
int rtiu_tlm_init (char *host, int port_no, int dir)
{
  int status;
  struct sockaddr_in hp_address;
  struct sockaddr_in rtiu_address;
  struct sockaddr_in ppc_address;
  static char deflt[] = { "default" };

  if (rtiu_debug_flag & 0x04)
    fprintf (stderr, "rtiu_tlm_init\n");

  if (recv_socket | xmit_socket)
    return RTIU_REDUNDANT_INIT;
  /*
   *      Who the hell are we anyway ?!?
   */
  status = gethostname (host_machine_name, MAXHOSTNAMELEN);
  if (status)
    return RTIU_BAD_SOURCE;

  /*
   *      Apply default port number and address
   */
  if (port_no)
    af_port = port_no;
  if (host)
    if (strcasecmp (host, deflt))
      strcpy (af_host, host);

  bzero (&hp, sizeof (struct hostent));
  bzero (&rtiu, sizeof (struct hostent));
  bzero (&ppc, sizeof (struct hostent));

  bzero (&rtiu_address, sizeof (struct sockaddr_in));
  bzero (&ppc_address, sizeof (struct sockaddr_in));
  bzero (&hp_address, sizeof (struct sockaddr_in));
  /*
   *      Translate name to address & get record
   *        from nameserver database
   */
  hp = gethostbyname (host_machine_name);
  if (hp == 0)
    return RTIU_BAD_SOURCE;

  rtiu = gethostbyname (af_host);
  if (rtiu == 0)
    return RTIU_BAD_TARGET;

  ppc = gethostbyname (af_host);
  if (ppc == 0)
    return RTIU_BAD_TARGET;


  /*
   *      OK, now start doing network shit
   */
  recv_socket = socket (AF_INET, af_type, af_protocol);
  if (recv_socket <= 0) {
    fprintf (stderr, "recv_socket=socket() fail\n");
    return RTIU_BAD_SOCKET;
  }

  xmit_socket = socket (AF_INET, af_type, af_protocol);
  if (xmit_socket <= 0) {
    fprintf (stderr, "xmit_socket=socket() fail\n");
    return RTIU_BAD_SOCKET;
  }

  ppc_socket = socket (AF_INET, af_type, af_protocol);
  if (ppc_socket <= 0) {
    fprintf (stderr, "ppc_socket=socket() fail\n");
    return RTIU_BAD_SOCKET;
  }

        /***************************************/
  hp_address.sin_family = AF_INET;
  bcopy (hp->h_addr_list, &hp_address.sin_addr, hp->h_length);
  hp_address.sin_port = af_port;

  rtiu_address.sin_family = AF_INET;
  bcopy (rtiu->h_addr, &rtiu_address.sin_addr, rtiu->h_length);
  rtiu_address.sin_port = af_port;

  ppc_address.sin_family = AF_INET;
  bcopy (ppc->h_addr, &ppc_address.sin_addr, ppc->h_length);
  ppc_address.sin_port = ppc_port;

        /***************************************/
  if (dir & RTIU_direction_in) {
    status = connect (recv_socket,
                      (struct sockaddr *) &rtiu_address,
                      sizeof (struct sockaddr_in));
    if (status) {
      rtiu_close (0);
      return RTIU_BAD_CONNECT;
    }

    if (rtiu_debug_flag & 0x10)
      fprintf (stderr, "rtiu_tlm_init connect(recv_socket);\n");
  }

        /***************************************/
  rtiu_address.sin_family = AF_INET;
  bcopy (rtiu->h_addr, &rtiu_address.sin_addr, rtiu->h_length);
  rtiu_address.sin_port = af_port + 1;
  if (dir & RTIU_direction_out) {
    status = connect (xmit_socket,
                      (struct sockaddr *) &rtiu_address,
                      sizeof (struct sockaddr_in));

    if (status) {
      rtiu_close (0);
      return RTIU_BAD_CONNECT;
    }

    status = connect (ppc_socket,
                      (struct sockaddr *) &ppc_address,
                      sizeof (struct sockaddr_in));

    if (status) {
      fprintf (stderr, "ppc_socket connect failure\n");
      rtiu_close (0);
      return RTIU_BAD_CONNECT;
    }
  }

  if (rtiu_debug_flag & 0x20)
    fprintf (stderr, "rtiu_tlm_init connect(xmit_socket);\n");
  /*
   *      Well, it must have worked, so let them know
   */
  if (dir & RTIU_direction_in)
    return recv_socket;
  if (dir & RTIU_direction_out)
    return xmit_socket;
  return 0;
}
int rtiu_tlm_init5 (char *host, int port_no, int dir)
{
  int status;
  struct sockaddr_in hp_address;
  struct sockaddr_in ppc_address;
  static char deflt[] = { "default" };

  if (rtiu_debug_flag & 0x04)
    fprintf (stderr, "rtiu_tlm_init\n");

  if (recv_socket | xmit_socket)
    return RTIU_REDUNDANT_INIT;
  /*
   *      Who the hell are we anyway ?!?
   */
  status = gethostname (host_machine_name, MAXHOSTNAMELEN);
  if (status)
    return RTIU_BAD_SOURCE;

  /*
   *      Apply default port number and address
   */
  if (port_no)
    af_port = port_no;
  if (host)
    if (strcasecmp (host, deflt))
      strcpy (af_host, host);

  bzero (&hp, sizeof (struct hostent));
  bzero (&ppc, sizeof (struct hostent));

  bzero (&ppc_address, sizeof (struct sockaddr_in));
  bzero (&hp_address, sizeof (struct sockaddr_in));
  /*
   *      Translate name to address & get record
   *        from nameserver database
   */
  hp = gethostbyname (host_machine_name);
  if (hp == 0)
    return RTIU_BAD_SOURCE;

  ppc = gethostbyname (af_host);
  if (ppc == 0)
    return RTIU_BAD_TARGET;

  /*
   *      OK, now start doing network shit
   */

  ppc_socket = socket (AF_INET, af_type, af_protocol);
  if (ppc_socket <= 0) {
    fprintf (stderr, "ppc_socket=socket() fail\n");
    return RTIU_BAD_SOCKET;
  }

        /***************************************/
  ppc_address.sin_family = AF_INET;
  bcopy (ppc->h_addr, &ppc_address.sin_addr, ppc->h_length);
  ppc_address.sin_port = ppc_port;

        /***************************************/
  if (dir & RTIU_direction_out) {
    status = connect (ppc_socket,
                      (struct sockaddr *) &ppc_address,
                      sizeof (struct sockaddr_in));

    if (status) {
      fprintf (stderr, "ppc_socket connect failure\n");
      rtiu_close (0);
      return RTIU_BAD_CONNECT;
    }
  }

  if (rtiu_debug_flag & 0x20)
    fprintf (stderr, "rtiu_tlm_init connect(xmit_socket);\n");
  /*
   *      Well, it must have worked, so let them know
   */
  if (dir & RTIU_direction_out)
    return xmit_socket;
  return 0;
}


/***************************************************************************
 * arguments:								   *
 *	char *host	host name					   *
 *			NULL uses default host				   *
 *			"DEFAULT" uses default host			   *
 *			any other string specifies rtiu name		   *
 *	char *table	transaction table				   *
 *			NULL skips processing (no table loaded)		   *
 *			"DEFAULT" uses default				   *
 *			any other string specifies file			   *
 *	int port_no	port number					   *
 *			zero uses some default				   *
 *									   *
 * Returns: >0 - successful (socket number)				   *
 *          RTIU_BAD_SOURCE - network problem (gethostname) 		   *
 *          RTIU_BAD_TARGET - host name could not be found 		   *
 *          RTIU_BAD_SOCKET - could not create rtiu_socketet		   *
 *          RTIU_BAD_CONNEC - could not connect to rtiu_socketet on host   *
 *          RTIU_BAD_TRANSACTION - transaction table not loaded		   *
 ***************************************************************************/
int rtiu_init (char *table)
{
  int status;
  static char deflt[] = { "default" };
  unsigned short tm[3] = { 0, 0, 0 };

  /*
   *      stop telemetry
   */
  status = rtiu_run (0);
  if (status < 0) {
    rtiu_close (0);
    return status;
  }
  /*
   *      stop data feed
   */
  status = rtiu_data (0);
  if (status < 0) {
    rtiu_close (0);
    return status;
  }
  /*
   *      stop time broadcast
   */
  status = rtiu_put (stop_sct);
  if (status < 0) {
    rtiu_close (0);
    return status;
  }
  /*
   *      disable handshaking
   */
  status = rtiu_echo (0);
  if (status < 0) {
    rtiu_close (0);
    return status;
  }

  /*
   *      Set RT address
   */
  status = rtiu_rtad (0);
  if (status < 0) {
    rtiu_close (0);
    return status;
  }

  /*
   *      Set time to 0
   *        3rd. argument WAS 0 (should be 3 to set time)
   */
  status = rtiu_time (tm, tm, 0);
  if (status < 0) {
    rtiu_close (0);
    return status;
  }


  /*
   *      OK, see if transaction table needs to be loaded
   */
  status = 1;
  if (table) {
    if (strcasecmp (table, deflt))
      status = rtiu_table (table, NULL, 0, 1);
    else
      status = rtiu_table (NULL, NULL, 0, 1);
  }

  if (status != 1) {
    rtiu_close (0);
    return RTIU_BAD_TRANSACTION;
  }

  /*
   *      Well, it must have worked, so let them know
   */
  return recv_socket;
}
