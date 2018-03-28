
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "rtiu.h"
#include "util.h"
#include "utilf.h"

/* #include "mpus.h"		/* uses definitions in util.h */
#include "fg.h"

#include "telemetry.h"

int RICE_DEBUG = 0, RICE_DEBUG_ALL = 0, RICE_STATS = 0;

extern Stats;

static char USDC_Version[] = "usdc( ) Version 8.00";



/* Assumes the complete data set */
long dcc_decompress (TELEMETRY * tlmin);
long dcc_unleave (TELEMETRY * tlmin, TELEMETRY *** tlmout);
void delay_real (double delay_seconds);



int main (int argc, char *argv[])
{
  char infile[512] = "stdin", outfile[512] = "stdout";
  unsigned long nStatus;

  static TELEMETRY tlmin, **tlmout;     /* on the SUN this being 'static' elimainates */

  /*
   * a compile error 
   */

  TELEMETRY *tlminit[] = { &tlmin, NULL };      /* this is a constant */

  UNSEGMENTED_MINI_PACKET *usmp = &(tlmin.packet.usmp);

  int nCount = 0;

  int eof_flag = 0;
  int loop_flag = 1;

  {                                     /* Start William Robison's Stuff */

    int local_flag = FILE_LOCATE_DEFAULT;


    FILE *input = stdin;
    FILE *output = stdout;

    fprintf (stderr, "%s\n", USDC_Version);

    fg_flags (argc, argv);
    if (fg_flag ("get"))                /* alias for getmp is "get" */
      fg_flagx ("+getmpus", "");
    if (fg_flag ("put"))                /* alias for putmp is "put" */
      fg_flagx ("+putmpus", "");

    if (fg_flag ("find")) {
      input = UTIL_find_open (fg_flagc ("find"), "rb");
      if (input) {
        fprintf (stderr, "mpus find: %s\n", UTIL_find_name ());
        strcpy (infile, UTIL_find_name ());     /* raj mod. 2/11/98 */
      } else {
        fprintf (stderr, "mpus find: file not found: %s\n",
                 fg_flagc ("find"));
        exit (0);
      }
    } else {
      input = UTIL_FILEname (FILE_MPUS, local_flag);
      if (input) {
        fprintf (stderr, "usdc source: %s\n", UTIL_fname (FILE_OLD));
        strcpy (infile, UTIL_fname (FILE_OLD)); /* raj mod. 2/11/98 */
      } else {
        input = stdin;
        fprintf (stderr, "usdc source: stdin\n");
      }
    }

    if (!output) {
      output = stdout;
    } else {
      output = UTIL_FILEnew (FILE_MPUS, local_flag);
      fprintf (stderr, "usdc destination:%s\n", UTIL_fname (FILE_NEW));
      strcpy (outfile, UTIL_fname (FILE_NEW));
    }

    if (fg_flag ("eof"))
      eof_flag = 1;

    if (fg_flag ("help") || fg_flag ("h")) {
      printf
        (" usdc - mini packet Data Compressin Chip/Rice decompression\n");
      printf ("         Version %s\n", USDC_Version);
      printf (" \n");
      printf ("      +find fn   find archive file\n");
      printf ("      +getmpus   get current file\n");
      printf ("                  else stdin\n");
      printf ("      +putmpus   put new file\n");
      printf ("                  else stdout\n");
      printf ("      +eof       non real-time\n");
      printf ("      help       (help menu)\n");
      exit (0);
      printf (" \n");
    }
  }                                     /* End William Robison's Stuff */


  while (--argc) {
    ++argv;
    if (!strcmp (*argv, "-debug")) {
      RICE_DEBUG = 1;
    } else if (!strcmp (*argv, "-debugall")) {
      RICE_DEBUG = 1;
      RICE_DEBUG_ALL = 1;
    } else if (!strcmp (*argv, "-stats")) {
      RICE_STATS = 1;
    } else if (!strcmp (*argv, "-filter")) {
      ++argv;
      --argc;
    } else if (!strcmp (*argv, "-help")) {
      fprintf (stderr, "\n-debug, -debugall, -stats, -filter\n");
    }
  }

  open_telemetry_infile (infile);
  open_telemetry_outfile (outfile);

  while (loop_flag) {
    if (PACKET_READ_SUCCESS == get_next_packet (&tlmin)) {

      switch (get_mini_packet_type (usmp)) {
       case MINI_PACKET_TYPE_wbr:
         if (MP_COMPRESSION_dcc_wcin == get_mp_wbr_compression (usmp) ||
             MP_COMPRESSION_dcc_wcout == get_mp_wbr_compression (usmp)) {
           dcc_decompress (&tlmin);
         } else if (get_mp_wbr_compression (usmp) & 0x080) {    /* Willy's Compression Status 1xxx */
           dcc_decompress (&tlmin);     /* 1=Attempt to take out 1st word */
         } /* of junk decompression */
         else {
           /*
            * do nothing 
            */ ;
         }
         tlmout = tlminit;
         break;
       case MINI_PACKET_TYPE_wfr:
         if (MP_COMPRESSION_dcc_wcin == get_mp_wfr_compression (usmp) ||
             MP_COMPRESSION_dcc_wcout == get_mp_wfr_compression (usmp)) {
           dcc_decompress (&tlmin);
           dcc_unleave (&tlmin, &tlmout);       /* a NULL terminated array with seperated channels */
         } else {
           tlmout = tlminit;
         }
         break;
       default:
         tlmout = tlminit;
         break;
      }

      while (*tlmout != NULL) {
        put_packet (*tlmout);
        ++tlmout;
      }
    }

    else {
      if (eof_flag)
        loop_flag = 0;
      else
        delay_real (0.125);
    }

  }                                     /* While 1 */

  close_telemetry_infile (infile);
  close_telemetry_outfile (outfile);

  return 1;
}



void delay_real (double dly)
{
  struct timeval timeout;

  timeout.tv_sec = (unsigned long) dly;
  timeout.tv_usec = (long) ((dly - timeout.tv_sec) * 1000000);
  select (0, 0, 0, 0, &timeout);

  return;
}
