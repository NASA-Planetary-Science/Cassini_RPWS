
/***************************************************/

/****	adbwrap, the wrapper for adb library	****/

/***************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <time.h>
#include "CasType.h" /**/
#include "cds_status.h"
#include "rtiu.h"
#include "util.h"
#include "adb.h"

#include <SpiceUsr.h>


/*****************************************************************************/

static char arglist[] = { "hqs:p:n:f:d" };
static char *Version = { "V1.00" };
extern char *optarg;
extern int optind, opterr, optopt;
static int file_type = ADB_DATA_STIM;
static char *select_opts[] = {

#define CasCdsRpws_HSK   ((ULONG)0x00080000)
#define CasCdsRpws_LRS   ((ULONG)0x00040000)
#define CasCdsRpws_HRS   ((ULONG)0x00020000)

#define				 SELECT_S_FILE	0
  "STIM",

#define				 SELECT_E_FILE	1
  "ENG",

#define				 SELECT_B_FILE	2
  "BFDL",

#define				 SELECT_D_FILE	3
  "DUST",

#define				 SELECT_F_FILE	4
  "HOUSE",

#define				 SELECT_S_file	5
  "stim",

#define				 SELECT_E_file	6
  "eng",

#define				 SELECT_B_file	7
  "bfdl",

#define				 SELECT_D_file	8
  "dust",

#define				 SELECT_F_file	9
  "house",

  NULL
};

static char *select_opts_text[] = {
  "Stim records",
  "engineering records",
  "WBR/WFR command stats",
  "DUST records",
  "HOUSEKEEPING records",
  NULL
};


static int adbwrap_help (char *argv, FILE * file)
{
  int i = 0;

  fprintf (file, "    \n");
  fprintf (file, "%s Wrapper Version %s, ADB Library Version %s\n", argv,
           Version, ADB_Version);
  fprintf (file, "    \n");
  fprintf (file, "  usage: %s -[%s] start_time [stop_time]\n", argv, arglist);
  fprintf (file, "    \n");
  fprintf (file,
           "    Example code to make use of the 'adb' library routines\n");
  fprintf (file, "      to access the Cassini/RPWS minipacket data.  All\n");
  fprintf (file, "      that is required is a start time, stop time is\n");
  fprintf (file, "      optional (end-of-data is assumed when no stop\n");
  fprintf (file,
           "      time if provided).  U-files are extracted directly\n");
  fprintf (file, "      from the database file, all others are formed by\n");
  fprintf (file, "      removing text following the \".\" in the filename\n");
  fprintf (file, "      and replacing it with the indicated string (this\n");
  fprintf (file, "      means other files are assumed to be in the same\n");
  fprintf (file,
           "      directory as the U-file, with the indicated file type.\n");
  fprintf (file, "      \n");
  fprintf (file, "    flags/switches:\n");
  fprintf (file, "    \n");
  fprintf (file, "      -p path    Database pathname\n");
  fprintf (file, "      -n file    Database filename\n");
  fprintf (file, "      -d         Debug dump (file open/close)\n");
  fprintf (file, "    \n");
  fprintf (file, "      -s type    Select file type\n");
  i = 0;
  while (select_opts_text[i]) {
    fprintf (file, "          -s %6s  %s", select_opts[i],
             select_opts_text[i]);
    if (!i)
      fprintf (file, " default");
    fprintf (file, "\n");
    i++;
  }
  fprintf (file, "    \n");
  fprintf (file, "      -h         Terse Help <stdout>\n");
  fprintf (file, "      -q         Verbose Help <stdout>\n");
  fprintf (file, "          \n");
  fprintf (file,
           "            start and stop time may be specified as any \n");
  fprintf (file, "                of the following:\n");
  fprintf (file,
           "              SCET, spice compatible time string, such as\n");
  fprintf (file, "                  yyyy-dddThh:mm:ss.mmm\n");
  fprintf (file,
           "                  Year/day and the \"T\" are required.  Hours\n");
  fprintf (file, "                  minutes and seconds default to zero.\n");
  fprintf (file, "              SCLK, spice compatible as follows\n");
  fprintf (file, "                  p/ssssssssss.fff\n");
  fprintf (file,
           "                  Partition is required, as is the slash.\n");
  fprintf (file, "                  Seconds is expressed in decimal.\n");
  fprintf (file, "              SCLK, RPWS hexadecimal (8 digits)\n");
  fprintf (file, "                  XXXXXXXX\n");
  fprintf (file, "                  0xXXXXXXXX\n");
  fprintf (file, "                  Exactly 8 hexadecimal digits.\n");
  fprintf (file,
           "                  Exactly 10 hexadecimal digits (leading 0x).\n");
  fprintf (file, "        Stop Time may be omitted\n");
  fprintf (file, "    \n");
  fprintf (file, "    The source for this wrapper is \n");
  fprintf (file, "              %s\n", __FILE__);
  fprintf (file, "    \n");
  fprintf (file, "    This help file is routed to <stdout> only when the\n");
  fprintf (file, "      -h/-q option is specified, all other \n");
  fprintf (file, "      messages are routed to <stderr>.\n");
  fprintf (file, "    \n");
  fprintf (file, "    Source file: %s\n", __FILE__);
  fprintf (file, "    \n");
  return 0;
}
int main (int argc, char *argv[])
{
  struct ADB *adb_file;                 /* pointer to adb control structure */
  unsigned char *buf;                   /* pointer to data buffer */
  int filter = 0;
  int count = 0;
  int i;
  int sclk;
  int c;
  int we_should_write_this_record = 1;
  char *start_time = NULL;
  char *stop_time = NULL;
  char *database_path = NULL;
  char *options;
  char *value;
  char *time = { "1997-288T00:00:00" };

  if (strcmp (ADB_Version, ADB_VERSION)) {
    fprintf (stderr, "%s/%s\n", ADB_Version, ADB_VERSION);
    fprintf (stderr, "Header/Library mismatch please recompile");
    exit (0);
  }
  if (argc < 2) {
    adbwrap_help (argv[0], stderr);
    exit (0);
  }

        /********************************
	 *	Load SPICE kernels	*
	 *  adb makes use of FTN spice	*
	 *    routines, caller must 	*
	 *       initialize spice	*
	 ********************************/
  fprintf (stderr, "ldpool_ %s\n", leapfile);
  ldpool_ (leapfile, strlen (leapfile));

  fprintf (stderr, "ldpool_ %s\n", sclkfile);
  ldpool_ (sclkfile, strlen (sclkfile));

  if (0) {
    i = MDB_time (time);
    /**/ fprintf (stderr, "%08X=MDB_time(\"%s\");\n", i, time);
  /**/}

        /****************************************
	 *  Select file type to                 *
	 *   access,  We can do minipackets	*
	 *   as well as CDS packets             *
	 ****************************************/
  while ((c = getopt (argc, argv, arglist)) != EOF) {
    switch (c) {
     case 'd':
       ADB_debug = -1;
       break;
     case 'p':
       database_path = optarg;
       break;
     case '?':
       adbwrap_help (argv[0], stderr);
       fprintf (stderr, "Invalid Command Line Arguments (-?)\n");
       exit (0);
     case 'h':
     case 'q':
       adbwrap_help (argv[0], stdout);
       fputc ('\f', stdout);
       exit (0);
     case 's':
       options = optarg;
       while (*options != '\0') {
         int ix;

         ix = getsubopt (&options, select_opts, &value);
         switch (ix) {
          case SELECT_S_FILE:
          case SELECT_S_file:
            file_type = ADB_DATA_STIM;
            break;
          case SELECT_E_FILE:
          case SELECT_E_file:
            file_type = ADB_HSK_ENG;
            break;
          case SELECT_B_FILE:
          case SELECT_B_file:
            file_type = ADB_HSK_BFDL;
            break;
          case SELECT_D_FILE:
          case SELECT_D_file:
            file_type = ADB_HSK_DUST;
            break;
          case SELECT_F_FILE:
          case SELECT_F_file:
            file_type = ADB_HSK_HOUSE;
            break;
          default:
            fprintf (stderr, "Invalid Command Line Arguments (-s %s)\n",
                     optarg);
            exit (0);
         }
       }
       break;
    }
  }


        /********************************
	 *  Start/Stop time             *
	 *   Accept start only          *
	 *   as well as start/stop      *
	 ********************************/
  switch (argc - optind) {
   case 2:
     start_time = argv[optind + 0];
     stop_time = argv[optind + 1];
     fprintf (stderr, "start and stop time specified\n");
     break;
   case 1:
     start_time = argv[optind + 0];
     stop_time = NULL;
     fprintf (stderr, "start time specified\n");
     break;
   default:
     adbwrap_help (argv[0], stderr);
     fprintf (stderr, "Invalid Command Line Arguments (not enough)\n");
     exit (0);
  }


  if (start_time) {
    fprintf (stderr, "start time (\"%s\")\n", start_time);
    /**/ i = MDB_time (start_time);
    /**/
      fprintf (stderr, "start time %08X=MDB_time(\"%s\")\n", i, start_time);
  /**/} else {
    fprintf (stderr, "No start time specified\n");
  }

  if (stop_time) {
    i = MDB_time (stop_time);
    /**/ fprintf (stderr, "stop time %08X=MDB_time(\"%s\");\n", i, stop_time);
  /**/} else {
    fprintf (stderr, "No stop time specified\n");
  }

        /****************************************
	 *  ADB_open with start/stop and        *
	 *   indication of which type to read   *
	 *   the 2 null arguments are path      *
	 *   and filename for the database      *
	 *   index.  We'll simply use the       *
	 *   defaults (they're correct).        *
	 ****************************************/
  adb_file = ADB_open (start_time, stop_time, database_path, file_type);
  /**/

        /****************************************
	 *  OK, ADB_open has prepared everyting *
	 *    needed to access the data, so all *
	 *    we need to do is read             *
	 *  The read routine returns a pointer  *
	 *    to an RPWS data structure, this   *
	 *    is one of these guys that has     *
	 *    a bunch on ancillary data before  *
	 *    the data.                         *
	 ****************************************/
    buf = ADB_read (adb_file);

  while (buf) {                         /* buf will be NULL when no more data */
    puts ((char *) buf);
    /**/ buf = ADB_read (adb_file);
  }

        /****************************************
	 *  NO more data, so release resources	*
	 *    (we could call ADB_open again and *
	 *    read something else).             *
	 ****************************************/
  ADB_close (adb_file);
}
