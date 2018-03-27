
/***************************************************/

/****	mdbwrap, the wrapper for mdb library	****/

/***************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <time.h>

#include <SpiceUsr.h>

#include <rpwstlm/CasType.h>                    /*RPWS Stuff */
#include <cds_status.h>
#include <rtiu.h>
#include <util.h>
#include <mdb.h>


/*****************************************************************************/
static char arglist[] = { "hqs:p:n:f:d" };
static char *Version = { "V1.16" };
extern char *optarg;
extern int optind, opterr, optopt;
static int file_type = MDB_U_FILE;
static char *select_opts[] = {

#define CasCdsRpws_HSK   ((ULONG)0x00080000)
#define CasCdsRpws_LRS   ((ULONG)0x00040000)
#define CasCdsRpws_HRS   ((ULONG)0x00020000)

#define				 SELECT_U_FILE	0
  "U_FILE",

#define				 SELECT_R_FILE	1
  "R_FILE",

#define				 SELECT_S_FILE	2
  "S_FILE",

#define				 SELECT_H_FILE	3
  "H_FILE",

#define				 SELECT_L_FILE	4
  "L_FILE",

#define				 SELECT_U_file	5
  "u_file",

#define				 SELECT_R_file	6
  "r_file",

#define				 SELECT_S_file	7
  "s_file",

#define				 SELECT_H_file	8
  "h_file",

#define				 SELECT_L_file	9
  "l_file",

  NULL
};

static char *select_opts_text[] = {
  "unsegmented minipackets",
  "CDS records",
  "Stim minipackets",
  "HFR minipackets",
  "Langmuir Probe minipackets",
  NULL
};

static char *filter_opts[] = {
#define				 FILTER_MFR      0
  "MFR",

#define				 FILTER_HFR      1
  "HFR",

#define				 FILTER_DUST     2
  "DUST",

#define				 FILTER_LFDR     3
  "LFDR",

#define				 FILTER_WBR      4
  "WBR",

#define				 FILTER_WFR      5
  "WFR",

#define				 FILTER_LP       6
  "LP",

#define				 FILTER_STIM     7
  "STIM",

#define				 FILTER_MRO      8
  "MRO",

#define				 FILTER_HSK      9
  "HSK",

#define				 FILTER_LRS      10
  "LRS",

#define				 FILTER_HRS      11
  "HRS",

#define				 FILTER_mfr      12
  "mfr",

#define				 FILTER_hfr      13
  "hfr",

#define				 FILTER_dust     14
  "dust",

#define				 FILTER_lfdr     15
  "lfdr",

#define				 FILTER_wbr      16
  "wbr",

#define				 FILTER_wfr      17
  "wfr",

#define				 FILTER_lp       18
  "lp",

#define				 FILTER_stim     19
  "stim",

#define				 FILTER_mro      20
  "mro",

#define				 FILTER_hsk      21
  "hsk",

#define				 FILTER_lrs      22
  "lrs",

#define				 FILTER_hrs      23
  "hrs",

  NULL
};

static char *filter_opts_text[] = {
  "Medium Frequency Receiver",
  "High Frequency Receiver",
  "Dust Impact Detector Algorithm",
  "Low Frequency Digital Receiver",
  "Wide Band Receiver",
  "Wave Form Receiver",
  "Langmuir Probe",
  "Stimulus (IEB trigger)",
  "Memory Read Out",
  "Housekeeping",
  "Low Rate Science",
  "High Rate Science",
  NULL
};

static int filter_test_mp (int filter, struct MDB *mpb)
{
  int record_type = 0;

  if (file_type != MDB_U_FILE)
    return 1;
  record_type = UTIL_extract_MP_type ((struct MP_buffer *) mpb->buffer);
  switch (record_type) {
   case PACKET_TYPE_mfr:
     if (filter & CasSciMFR) {
       return 1;
     }
     break;
   case PACKET_TYPE_hfr:
     if (filter & CasSciHFR) {
       return 1;
     }
     break;
   case PACKET_TYPE_dust:
     if (filter & CasSciDust) {
       return 1;
     }
     break;
   case PACKET_TYPE_lp:
     if (filter & CasSciLP) {
       return 1;
     }
     break;
   case PACKET_TYPE_wbr:
     if (filter & CasSciWBR) {
       return 1;
     }
     break;
   case PACKET_TYPE_wfr:
     if (filter & CasSciWFR) {
       return 1;
     }
     break;
   case PACKET_TYPE_lfdr:
     if (filter & CasSciLFDR) {
       return 1;
     }
     break;
   case PACKET_TYPE_stim:
     if (filter & CasSciStim) {
       return 1;
     }
     break;
   case PACKET_TYPE_mro:
     if (filter & CasSciMRO) {
       return 1;
     }
     break;
  }
  return 0;
}

static int filter_test_rp (int filter, struct MDB *mpb)
{
  int record_type = 0;

  if (file_type != MDB_R_FILE)
    return 1;
  record_type = UTIL_extract_CDS_type ((struct CDS_buffer *) mpb->buffer);

  switch (record_type) {
   case CDS_Packet_ID_Housekeeping_ROM:
   case CDS_Packet_ID_Housekeeping_Maintenance:
   case CDS_Packet_ID_Housekeeping_Science:
     if (filter & CasCdsRpws_HSK) {
       return 1;
     }
     break;
   case CDS_Packet_ID_UnSegmented_LRS_I:
   case CDS_Packet_ID_Segmented_LRS_I:
     if (filter & CasCdsRpws_LRS) {
       return 1;
     }
     break;
   case CDS_Packet_ID_UnSegmented_HRS_I:
   case CDS_Packet_ID_Segmented_HRS_I:
     if (filter & CasCdsRpws_HRS) {
       return 1;
     }
     break;
  }
  return 0;
}

static int mdbwrap_help (char *argv, FILE * file)
{
  int i = 0;

  fprintf (file, "    \n");
  fprintf (file, "%s Wrapper Version %s, MDB Library Version %s\n", argv,
           Version, MDB_Version);
  fprintf (file, "    \n");
  fprintf (file, "  usage: %s -[%s] start_time [stop_time]\n", argv, arglist);
  fprintf (file, "    \n");
  fprintf (file,
           "    Example code to make use of the 'mdb' library routines\n");
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
  fprintf (file, "      -f filter  Packet type filter\n");
  i = 0;
  while (filter_opts_text[i]) {
    char temp[32];

    strcpy (temp, filter_opts[i]);
    strcat (temp, "      ");
    temp[6] = 0;
    fprintf (file, "          -f %s  %s\n", temp, filter_opts_text[i]);
    i++;
  }
  fprintf (file, "                  HSK, LRS, and HRS set the R_FILE\n");
  fprintf (file, "                  flag, as this is the only set of \n");
  fprintf (file, "                  files in which these packet types\n");
  fprintf (file, "                  appear (in other words, it isn't\n");
  fprintf (file, "                  necessary to use -sR_FILE with -fHSK\n");
  fprintf (file, "                  -fLRS or -FHRS).\n");
  fprintf (file, "                  \n");
  fprintf (file, "                  Also, you may set multiple packet\n");
  fprintf (file, "                  filters, as long as they come from\n");
  fprintf (file,
           "                  the same file (i.e. -fWBR -fWFR is OK).\n");
  fprintf (file, "          \n");
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
  struct MDB *mdb_file;                 /* pointer to mdb control structure */
  struct RPWS_buffer *buf;              /* pointer to data buffer */
  int filter = 0;
  int count = 0;
  int i;
  int sclk;
  int c;
  int we_should_write_this_record = 1;
  char *start_time = NULL;
  char *stop_time = NULL;
  char *database_path = NULL;
  char *database_file = NULL;
  char *options;
  char *value;
  char *time = { "1997-288T00:00:00" };
  char* metafile = NULL;

  if (strcmp (MDB_Version, MDB_VERSION)) {
    fprintf (stderr, "%s/%s\n", MDB_Version, MDB_VERSION);
    fprintf (stderr, "Header/Library mismatch please recompile");
    exit (0);
  }
  if (argc < 2) {
    mdbwrap_help (argv[0], stderr);
    exit (0);
  }

        /********************************
	 *	Load SPICE kernels	*
	 *  mdb makes use of FTN spice	*
	 *    routines, caller must 	*
	 *       initialize spice	*
	 ********************************/
  if( getenv("CAS_TIME_KERNELS") == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		return 13;
  }
  metafile = getenv("CAS_TIME_KERNELS");
  
  fprintf (stderr, "furnsh_c %s\n", metafile);
  furnsh_c(metafile);

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
       MDB_debug = -1;
       break;
     case 'p':
       database_path = optarg;
       break;
     case 'n':
       database_file = optarg;
       break;
     case '?':
       mdbwrap_help (argv[0], stderr);
       fprintf (stderr, "Invalid Command Line Arguments (-?)\n");
       exit (0);
     case 'h':
     case 'q':
       mdbwrap_help (argv[0], stdout);
       fputc ('\f', stdout);
       if (c == 'q')
         MDB_help (stdout);
       exit (0);
     case 's':
       options = optarg;
       while (*options != '\0') {
         int ix;

         ix = getsubopt (&options, select_opts, &value);
         switch (ix) {
          case SELECT_U_FILE:
          case SELECT_U_file:
            file_type = MDB_U_FILE;
            break;
          case SELECT_R_FILE:
          case SELECT_R_file:
            file_type = MDB_R_FILE;
            break;
          case SELECT_H_FILE:
          case SELECT_H_file:
            file_type = MDB_H_FILE;
            break;
          case SELECT_L_FILE:
          case SELECT_L_file:
            file_type = MDB_L_FILE;
            break;
          case SELECT_S_FILE:
          case SELECT_S_file:
            file_type = MDB_S_FILE;
            break;
          default:
            fprintf (stderr, "Invalid Command Line Arguments (-s %s)\n",
                     optarg);
            exit (0);
         }
       }
       break;
     case 'f':
       options = optarg;
       while (*options != '\0') {
         switch (getsubopt (&options, filter_opts, &value)) {
          case FILTER_HSK:
          case FILTER_hsk:
            filter |= CasCdsRpws_HSK;
            file_type = MDB_R_FILE;
            break;
          case FILTER_LRS:
          case FILTER_lrs:
            filter |= CasCdsRpws_LRS;
            file_type = MDB_R_FILE;
            break;
          case FILTER_HRS:
          case FILTER_hrs:
            filter |= CasCdsRpws_HRS;
            file_type = MDB_R_FILE;
            break;
          case FILTER_MFR:
          case FILTER_mfr:
            filter |= CasSciMFR;
            break;
          case FILTER_HFR:
          case FILTER_hfr:
            filter |= CasSciHFR;
            break;
          case FILTER_DUST:
          case FILTER_dust:
            filter |= CasSciDust;
            break;
          case FILTER_LFDR:
          case FILTER_lfdr:
            filter |= CasSciLFDR;
            break;
          case FILTER_WBR:
          case FILTER_wbr:
            filter |= CasSciWBR;
            break;
          case FILTER_WFR:
          case FILTER_wfr:
            filter |= CasSciWFR;
            break;
          case FILTER_LP:
          case FILTER_lp:
            filter |= CasSciLP;
            break;
          case FILTER_STIM:
          case FILTER_stim:
            filter |= CasSciStim;
            break;
          case FILTER_MRO:
          case FILTER_mro:
            filter |= CasSciMRO;
            break;
          default:
            fprintf (stderr, "Invalid Command Line Arguments (-f %s)\n",
                     optarg);
            exit (0);
            break;
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
     mdbwrap_help (argv[0], stderr);
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
	 *  MDB_open with start/stop and        *
	 *   indication of which type to read   *
	 *   the 2 null arguments are path      *
	 *   and filename for the database      *
	 *   index.  We'll simply use the       *
	 *   defaults (they're correct).        *
	 ****************************************/
  mdb_file =
    MDB_open (start_time, stop_time, database_path, database_file, file_type);
  /**/

        /****************************************
	 *  OK, MDB_open has prepared everyting *
	 *    needed to access the data, so all *
	 *    we need to do is read             *
	 *  The read routine returns a pointer  *
	 *    to an RPWS data structure, this   *
	 *    is one of these guys that has     *
	 *    a bunch on ancillary data before  *
	 *    the data.                         *
	 ****************************************/
    buf = MDB_read (mdb_file);

  while (buf) {                         /* buf will be NULL when no more data */
    if (filter) {
      if (file_type == MDB_R_FILE)
        we_should_write_this_record = filter_test_rp (filter, mdb_file);
      else
        we_should_write_this_record = filter_test_mp (filter, mdb_file);
    }

    if (we_should_write_this_record)
      UTIL_putbuffer_RPWS ((struct RPWS_buffer *) mdb_file->buffer, stdout);
    buf = MDB_read (mdb_file);
  }

        /****************************************
	 *  NO more data, so release resources	*
	 *    (we could call MDB_open again and *
	 *    read something else).             *
	 ****************************************/
  MDB_close (mdb_file);
}
