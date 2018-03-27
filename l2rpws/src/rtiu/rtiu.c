
/*#
  #	rtiu.c for the Heurikon RTIU
  #*/
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

#include <fg.h>

#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilmp.h>

#include "rtiuh.h"


static char version[] = { "rtiu-68K V1.1" };
static char cds_dump = 0;
static long epoch = 0;

void set_epoch (struct CDS_buffer *rtiu, long epoch)
{
  rtiu->packet.cds_tag.epoch[0] = ((unsigned long) epoch >> 24) & 0xFF;
  rtiu->packet.cds_tag.epoch[1] = ((unsigned long) epoch >> 16) & 0xFF;
  rtiu->packet.cds_tag.epoch[2] = ((unsigned long) epoch >> 8) & 0xFF;
  rtiu->packet.cds_tag.epoch[3] = ((unsigned long) epoch >> 0) & 0xFF;
  return;
}
void dumpbuffer (unsigned char *buf, int len, int address, int sub_address)
{
  static int width = 12;
  int wid;
  int i;

  if (address | sub_address)
    printf ("CDS record (%d)  RT:%d SA:%d\n", len / 2, address, sub_address);
  wid = width - 1;
  for (i = 0; i < len; i += 2) {
    printf (" %2.2X%2.2X", buf[i], buf[i + 1]);
    if (wid)
      wid = wid - 1;
    else {
      wid = width - 1;
      printf ("\n");
    }
  }
  printf ("\n");
}
int putbuffer_CDS (unsigned short *rtiu)
{
  static struct CDS_buffer buffer;
  int packet_length;
  int write_flag = 0;
  int size;
  int i;

  switch (rtiu[FP_function_code]) {
   default:
   case FC_return_data:
     packet_length = rtiu[FP_value] * 2;
     break;
  }
  if (packet_length > 2048)
    packet_length = 256;
  memcpy (&buffer.packet.rtiu.header, &rtiu[0], FL_header_size);
  memcpy (&buffer.packet.rtiu.status, &rtiu[FP_1553_status], FL_header_size);
  memcpy (&buffer.packet.cds.header, &rtiu[FP_telemetry], packet_length);

  size = 0;                             /* length computation */
  size = size + 12;                     /* control fields */
  size = size + 256;                    /* header area */
  size = size + rtiu[FP_length] - (FP_telemetry * 2);   /* data length */


  if (rtiu[FP_function_code] == FC_return_data)
    switch (rtiu[FP_sequence_number] & 0x1F) {
     default:
       buffer.record_type = DATA_RTIU;
       write_flag = 1;
       break;
     case BIU_sub_address_telem:
     case BIU_sub_address_hsk:
       buffer.record_type = DATA_RTIU_telem;
       if (rtiu[FP_tlm_len] > 16)
         write_flag = 1;
       break;
     case BIU_sub_address_biust:
       buffer.record_type = DATA_RTIU_biust;
       write_flag = 1;
       break;
     case BIU_sub_address_ancil:
       buffer.record_type = DATA_RTIU_ancil;
       write_flag = 1;
       break;
    }
  if (cds_dump) {
    rtiu_dump_header((struct rtiu_command*)rtiu, NULL);
	 
    dumpbuffer ((unsigned char*) (rtiu + FP_data), 36, 0, 0);
	 
    dumpbuffer ((unsigned char*) &(buffer.packet.cds.header[0]),
                packet_length,
                rtiu[FP_sequence_number] >> 8,
                rtiu[FP_sequence_number] & 0x7F);
  } else if (write_flag)
    set_epoch (&buffer, epoch);
  UTIL_get_time (&(buffer.packet.ws_tag.A));
  return UTIL_putbuffr2_CDS (&buffer, stdout, size);
  /*
   * return UTIL_putbuffr2_CDS(&buffer,stdout,sizeof(struct CDS_buffer)-4); /*
   */

}
void main (int argc, char *argv[])
{
  char *hostname = NULL;
  int portnumber = 0;
  char d_help = 0;
  char d_status = 0;
  char d_echo = 0;
  char d_early = 0;
  char d_dump = 0;
  int status;
  int index;
  char *dflt = { "default" };
  static unsigned short rtiu[FL_msg_size];
  static struct rtiu_header *result;

  fg_flags (argc, argv);                /* setup command line parser */
  epoch = utilo_epoch (NULL);
  hostname = fg_flagc ("host");
  portnumber = strtol (fg_flagc ("port"), NULL, 0);
  d_status = fg_flag ("d_status");
  d_help = fg_flag ("help") | fg_flag ("h");
  d_echo = fg_flag ("d_echo");
  if (fg_flag ("debug")) {
    d_status = 1;
    d_echo = 1;
  }
  d_early = fg_flag ("d_early");
  d_dump = fg_flag ("d_dump");
  cds_dump = fg_flag ("cds_dump");

  if (d_help) {
    printf ("---------------------------------------\n%s (%s)\n",
            version, rtiu_dump (RTIU_STRING_version));
    printf ("  Command line flags\n");
    printf ("    -host xxx   select alternate rtiu\n");
    printf ("    -port nnn   select alternat port\n");
    printf ("\n");
    printf ("  debugging aids\n");
    printf ("    -d_status   display network status\n");
    printf ("    -d_echo     display '.' for each record\n");
    printf ("    -debug      all debug features on\n");
    printf ("    \n");
    exit (1);
  } else
    fprintf (stderr, "rtiu.c %s (%s) ************************************\n",
             version, rtiu_dump (RTIU_STRING_version));
  if (hostname)
    fprintf (stderr, "rtiu.c    host name: %s\n", hostname);
  if (portnumber)
    fprintf (stderr, "rtiu.c    port number: %d\n", portnumber);
  if (d_status)
    fprintf (stderr, "rtiu.c     d_status\n");
  if (d_echo)
    fprintf (stderr, "rtiu.c     d_echo\n");


  status = rtiu_tlm_init (hostname,     /* host name   */
                          portnumber,   /* port number */
                          RTIU_direction_in);

  if (d_status || status < 0)
    fprintf (stderr, "rtiu.c (%s) %d = rtiu_tlm_init();\n",
             rtiu_mssg (status), status);

  /*
   * status = rtiu_data(1);
   * if(d_status || status<0)
   * fprintf(stderr,"rtiu.c (%s) %d = rtiu_data();\n", 
   * rtiu_mssg(status),
   * status);
   * status = rtiu_run(1);
   * if(d_status || status<0)
   * fprintf(stderr,"rtiu.c (%s) %d = rtiu_run();\n", 
   * rtiu_mssg(status),
   * status);
   */

  if (d_early)
    exit (0);

  status = 1;
  while (status > 0) {
    status = rtiu_get ((struct rtiu_command*)rtiu);
    while (1) {
      if (status != RTIU_BAD_DATA)
        break;
      fprintf (stderr, ".");
      status = rtiu_get ((struct rtiu_command*)rtiu);
    }

    if (status > 0) {
      rtiu_swap ((struct rtiu_command*)rtiu);
      /**/ if (d_dump)
        rtiu_dump_packet (rtiu, rtiu_fc (rtiu));
      else {
        putbuffer_CDS (rtiu);
      /**/}
    }
  }

  status = rtiu_close (1);
  if (d_status || status < 0)
    fprintf (stderr, "rtiu.c test: (%s) %d = rtiu_close();\n",
             rtiu_mssg (status), status);
}
