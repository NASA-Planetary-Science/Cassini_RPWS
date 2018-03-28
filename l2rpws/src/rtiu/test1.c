#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <string.h>
#include "fg.h"
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "utilmp.h"
#include "rtiuh.h"


static char cds_dump = 0;
char version[] = { "test1-68K V1.0" };

void dumpbuffer (unsigned char *buf, int len, int address, int sub_address)
{
  static width = 12;
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
  int write_flag;

  switch (rtiu[FP_function_code]) {
   default:
   case FC_return_data:
     packet_length = rtiu[FP_value] * 2;
     break;
  }
  memcpy (&buffer.packet.cds.header,
          &rtiu[FP_telemetry], rtiu[FP_length] - FP_telemetry * 2);
  memcpy (&buffer.packet.cds.header,
          &rtiu[FP_telemetry], rtiu[FP_length] - FP_telemetry * 2);

  memset (buffer.packet.trailer.data, 0x00, 256);
  if (rtiu[FP_function_code] == FC_return_data)
    switch (rtiu[FP_sequence_number] & 0x7F) {
     default:
       buffer.record_type = DATA_RTIU;
       write_flag = 1;
       break;
     case BIU_sub_address_telem:
       buffer.record_type = DATA_RTIU_telem;
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
    rtiu_dump_header (rtiu, NULL);
    dumpbuffer (&rtiu[FP_data], 36, 0, 0);
    dumpbuffer (&buffer.packet.cds.header,
                packet_length,
                rtiu[FP_sequence_number] >> 8,
                rtiu[FP_sequence_number] & 0x7F);
  } else if (write_flag)
    return UTIL_putbuffr2_CDS (&buffer, stdout,
                               sizeof (struct CDS_buffer) - 4);

}
void main (int argc, char *argv[])
{
  char *hostname = NULL;
  char *transfile = NULL;
  char *mode = NULL;
  char ans[256];
  int portnumber = 0;
  int rtad = 0;
  char d_help = 0;
  char d_status = 0;
  char d_trans = 0;
  char d_echo = 0;
  char d_early = 0;
  char d_dump = 0;
  int status;
  int index;
  char *dflt = { "default" };
  static unsigned short rtiu[2048];
  static struct rtiu_header *result;

  fg_flags (argc, argv);                /* setup command line parser */
  hostname = fg_flagc ("host");
  transfile = fg_flagc ("trans");
  portnumber = strtol (fg_flagc ("port"), NULL, 0);
  rtad = strtol (fg_flagc ("rtad"), NULL, 0);
  mode = fg_flagc ("mode");
  d_trans = fg_flag ("d_trans");
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
    printf ("    -trans xxx  select alternate transaction file\n");
    printf ("    -mode xxx   select alternate telemetry mode\n");
    printf ("    -rtad xxx   select alternate terminal address\n");
    printf ("\n");
    printf ("  debugging aids\n");
    printf ("    -d_trans    display transaction table\n");
    printf ("                  then exit\n");
    printf ("    -d_status   display network status\n");
    printf ("    -d_echo     display '.' for each record\n");
    printf ("    -debug      all debug features on\n");
    printf ("    \n");
    exit (1);
  } else
    fprintf (stderr, "%s (%s)\n", version, rtiu_dump (RTIU_STRING_version));
  if (hostname)
    fprintf (stderr, "           host name: %s\n", hostname);
  if (portnumber)
    fprintf (stderr, "         port number: %d\n", portnumber);
  if (transfile)
    fprintf (stderr, "    transaction file: %s\n", transfile);
  if (mode)
    fprintf (stderr, "        initial mode: %s\n", mode);
  if (d_status)
    fprintf (stderr, "        d_status\n");
  if (d_echo)
    fprintf (stderr, "        d_echo\n");


  if (d_trans) {
    printf ("\nTransaction table dump(%s) of mode(%s)\n", transfile, mode);
    status = rtiu_time (NULL, NULL, 0);
    status = rtiu_table (transfile, mode, 0, 0);
    rtiu_dump (RTIU_DUMP_current_transaction);
    exit (1);
  }


  status = rtiu_tlm_init (hostname,     /* host name */
                          portnumber,   /* port number */
                          RTIU_direction_both);
  if (d_status || status < 0) {
    fprintf (stderr, "(%s) %d = rtiu_tlm_init(%s, %d);\n",
             rtiu_mssg (status), status, hostname, portnumber);
  }
  if (status < 0) {
    rtiu_close (0);
    return;
  }

  /*
   *      stop telemetry
   */
  status = rtiu_run (0);
  if (d_status || status < 0) {
    fprintf (stderr, "(%s) %d = rtiu_run(0);\n", rtiu_mssg (status), status);
  }
  if (status < 0) {
    rtiu_close (0);
    return;
  }
  /*
   *      disable handshaking
   */
  status = rtiu_echo (0);
  if (d_status || status < 0) {
    fprintf (stderr, "(%s) %d = rtiu_echo(0);\n", rtiu_mssg (status), status);
  }
  if (status < 0) {
    rtiu_close (0);
    return;
  }

  /*
   *      Set RT address
   */
  status = rtiu_rtad (rtad);
  if (d_status || status < 0) {
    fprintf (stderr, "(%s) %d = rtiu_rtad(%d);\n",
             rtiu_mssg (status), status, rtad);
  }
  if (status < 0) {
    rtiu_close (0);
    return;
  }

  /*
   *      OK, see if transaction table needs to be loaded
   */
  status = rtiu_table (transfile, mode, 0, 1);
  if (d_status || status < 0) {
    fprintf (stderr, "(%s) %d = rtiu_table(%s, %s, 1);\n",
             rtiu_mssg (status), status, transfile, mode);
  }

  if (status != 1) {
    rtiu_close (0);
    return;
  }

  /*
   *      Set time to NOW
   */
  status = rtiu_time (NULL, NULL, 0);
  if (d_status || status < 0) {
    fprintf (stderr, "(%s) %d = rtiu_time(NULL, NULL, 0);\n",
             rtiu_mssg (status), status);
  }
  if (status < 0) {
    rtiu_close (0);
    return;
  }


  printf ("hit ANY key (if you can find it)\n");
  gets (ans);

  status = rtiu_close (0);
  if (d_status || status < 0)
    fprintf (stderr, "test: (%s) %d = rtiu_close(0);\n",
             rtiu_mssg (status), status);
}
