
/***************************************************/

/****   mdbwrap, the wrapper for mdb library    ****/

/***************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <time.h>

/*#include "CasTlmDef.h"/**/
#include "cds_status.h"
#include "rtiu.h"
#include "util.h"
#include "pdsdb.h"

extern char *optarg;
extern int optind, opterr, optopt;

/****************************************************************************/

static char *select_opts[] = {
#define SELECT_NONE 		0
  "NULL",
#define SELECT_KEY		1
  "KEY",
#define SELECT_KEY_HDR		2
  "KEY_HDR",
#define SELECT_LRS		3
  "LRS",
#define SELECT_LRS_LRFULL	4
  "LRS_LRFULL",
#define SELECT_LRS_TIME		5
  "LRS_TIME",
#define SELECT_LRS_FREQ		6
  "LRS_FREQ",
#define SELECT_RAW		7
  "RAW",
#define SELECT_WFR		8
  "WFR",
#define SELECT_WBR		9
  "WBR",

  "",
  NULL
};

static char *PDSdb_Type[] = {
  "",
  PDSDB_FILE_RPWS_KEY_PARAMETERS,       /*  1 */
  PDSDB_FILE_RPWS_KEY_PARAMETERS,       /*  2 */
  PDSDB_FILE_RPWS_LOW_RATE_FULL,        /*  3 */
  PDSDB_FILE_RPWS_LOW_RATE_FULL,        /*  4 */
  PDSDB_FILE_RPWS_LOW_RATE_FULL,        /*  5 */
  PDSDB_FILE_RPWS_LOW_RATE_FULL,        /*  6 */
  PDSDB_FILE_RPWS_RAW_COMPLETE,         /*  7 */
  PDSDB_FILE_RPWS_WAVEFORM_FULL,        /*  8 */
  PDSDB_FILE_RPWS_WIDEBAND_FULL,        /*  9 */
  ""
};
static char *PDSdb_Record[] = {
  "",
  PDSDB_RECORDNAME_RPWS_KEY_PARAMETERS, /*  1 */
  PDSDB_RECORDNAME_RPWS_KEY_HEADER,     /*  2 */
  PDSDB_RECORDNAME_RPWS_LOW_RATE_FULL,  /*  3 */
  PDSDB_RECORDNAME_RPWS_LOW_RATE_LRFULL,        /*  4 */
  PDSDB_RECORDNAME_RPWS_LOW_RATE_TIME,  /*  5 */
  PDSDB_RECORDNAME_RPWS_LOW_RATE_FREQUENCY,     /*  6 */
  "",                                   /*  7 */
  PDSDB_RECORDNAME_RPWS_WAVEFORM_FULL,  /*  8 */
  PDSDB_RECORDNAME_RPWS_WIDEBAND_FULL,  /*  9 */
  ""
};
static int PDSdb_Stream[] = {
  0,
  PDSDB_STREAM_RPWS_KEY_PARAMETERS,     /*  1 */
  PDSDB_STREAM_RPWS_KEY_HEADER,         /*  2 */
  PDSDB_STREAM_RPWS_LOW_RATE_FULL,      /*  3 */
  PDSDB_STREAM_RPWS_LOW_RATE_LRFULL,    /*  4 */
  PDSDB_STREAM_RPWS_LOW_RATE_TIME,      /*  5 */
  PDSDB_STREAM_RPWS_LOW_RATE_FREQUENCY, /*  6 */
  0,                                    /*  7 */
  PDSDB_STREAM_RPWS_WAVEFORM_FULL,      /*  8 */
  PDSDB_STREAM_RPWS_WIDEBAND_FULL,      /*  9 */
  0
};

/**/
static int wrap_help (FILE * out)
{
  int i = 1;

  fprintf (out, "    -d      debug\n");
  fprintf (out, "    -p xx   database path\n");
  fprintf (out, "    -n xx   database name\n");
  fprintf (out, "  Following arguments to PDSDB_open:\n");
  fprintf (out, "    -t xx   type\n");
  fprintf (out, "    -s xx   subtype\n");
  fprintf (out, "    -r xx   record\n");
  fprintf (out, "    \n");
  fprintf (out, "  Sets type/subtype\n");
  fprintf (out, "    -u xx   From following\n");
  while (select_opts[i])
    fprintf (out, "              %s\n", select_opts[i++]);
  fprintf (out, "    \n");
  fprintf (out, "    \n");
  return 0;
}
int main (int argc, char *argv[])
{
  int debug_flag = 0;
  char c;
  unsigned char *buffer;
  struct PDSDB *pdsdb;
  char *start_time = NULL;
  char *stop_time = NULL;
  char *type = NULL;
  char *subtype = NULL;
  char *database_path = NULL;
  char *database_file = NULL;
  char *record = NULL;
  char *options;
  char *value;
  int pdsdb_stream = 0;
  char* metafile = NULL;

  if (strcmp (PDSDB_Version, PDSDB_VERSION)) {
    fprintf (stderr, "%s/%s\n", PDSDB_Version, PDSDB_VERSION);
    fprintf (stderr, "Header/Library mismatch please recompile");
    exit (0);
  }

  if (argc < 2) {
    /*
     * pdsdb_help(argv[0], stderr); /*
     */
    exit (0);
  }

        /****************************************************************
	 *	Like all these programs, we'll make use of SPICE	*
	 *	to do time, so initialize it...				*
	 ****************************************************************/
  if( getenv("CAS_TIME_KERNELS") == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		return 13;
  }
  metafile = getenv("CAS_TIME_KERNELS");
  
  fprintf (stderr, "furnsh_c %s\n", metafile);
  furnsh_c(metafile);


     /****************************************
      *  Select file type to                 *
      *   access,  We can do minipackets     *
      *   as well as CDS packets             *
      ****************************************/
  while ((c = getopt (argc, argv, "hdp:n:s:t:u:")) != EOF) {
    switch (c) {
     default:
       fprintf (stderr, "Arguments suck\n");
     case 'h':
       wrap_help (stdout);
       exit (0);
     case 'd':
       debug_flag = 1;
       break;
     case 'p':
       database_path = optarg;
       break;
     case 'n':
       database_file = optarg;
       break;
     case 't':
       type = optarg;
       break;
     case 's':
       subtype = optarg;
       break;
     case 'r':
       record = optarg;
       break;
     case 'u':
       options = optarg;
       while (*options != '\0') {
         int ix;

         ix = getsubopt (&options, select_opts, &value);
         type = PDSdb_Type[ix];
         record = PDSdb_Record[ix];
         pdsdb_stream = PDSdb_Stream[ix];
       }

    }
  }

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
     fprintf (stderr, "Invalid Command Line Arguments (not enough)\n");
     exit (0);
  }

        /****************************************************************
	 *	Open things, we might need to know where the PDS
	 *	  archive is located (although at Iowa, we have a
	 *	  "super" archive with everyting, so that is the
	 *	  default (3rd. & 4th. arguments are the location
	 *	  of the database index, TAKE CAREFUL NOTE of how
	 *	  this example is done, the path is to the top
	 *	  of the archive, and the database name has the
	 *	  portion of the pathname that is onthe archive 
	 *	  volume.
	 *	Similar to the "MDB" library, the start and stop time
	 *	  may be supplied as SCLK, SCET, or HEX SCLK.  These
	 *	  are the 1st. and 2nd. arguments.
	 *	Since there is a little bit of everything on the
	 *	  archive volume, the 5th. and 6th. arguments
	 *	  are file selection keys.  Note which fields in
	 *	  the label files we search for a match with these
	 *	  keys.  NULL means match everything.
	 ****************************************************************/

  if (debug_flag)
    fprintf (stderr, "\n\n");
  fprintf (stderr, "_open(");

  if (start_time)
    fprintf (stderr, "\"%s\", ", start_time);
  else
    fprintf (stderr, "NULL, ");

  if (stop_time)
    fprintf (stderr, "\"%s\", ", stop_time);
  else
    fprintf (stderr, "NULL, ");

  if (database_path)
    fprintf (stderr, "\"%s\", ", database_path);
  else
    fprintf (stderr, "NULL, ");

  if (database_file)
    fprintf (stderr, "\"%s\", ", database_file);
  else
    fprintf (stderr, "NULL, ");

  if (type)
    fprintf (stderr, "\"%s\", ", type);
  else
    fprintf (stderr, "NULL, ");

  if (subtype)
    fprintf (stderr, "\"%s\", ", subtype);
  else
    fprintf (stderr, "NULL, ");

  if (record)
    fprintf (stderr, "\"%s\");\n", record);
  else
    fprintf (stderr, "NULL);\n");



  pdsdb = PDSDB_open (start_time,       /* start time */
                      stop_time,        /* stop time */
                      database_path,    /* path to data */
                      database_file,    /* index file */
                      type,             /* */
                      subtype,          /* substring in FILE_SPECIFICATION_NAME */
                      record);          /* */

        /****************************************************************
	 *	Debugging, this just dumps the data structure
	 ****************************************************************/

  if (debug_flag)
    PDSDB_dump (stderr, pdsdb, PDSDB_DUMP_PDSDB + PDSDB_DUMP_INDEX);

/**/

        /****************************************************************
	 *	Now, on to the read.
	 *	  Several of the dataproducts have header records
	 *	  that aren't data.  The 2nd. argument to the
	 *	  read routine is the OBJECT keyword (from the label
	 *	  file) that points to the data.  This causes the
	 *	  read to start at the record specified in the label
	 ****************************************************************/
  fprintf (stderr, "_read(%p, %d)\n", pdsdb, pdsdb_stream);
  buffer = PDSDB_read_stream (pdsdb, pdsdb_stream);
  while (buffer) {
    if (debug_flag)
      PDSDB_dump (stderr, pdsdb, PDSDB_DUMP_TIME);
    else
      fwrite (buffer, pdsdb->datalabel_bytes, 1, stdout);
    buffer = PDSDB_read_stream (pdsdb, pdsdb_stream);
  }
}
