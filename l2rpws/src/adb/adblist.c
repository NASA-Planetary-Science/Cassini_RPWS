
/*********************************/

/****	adblist, detail lines ****/

/*********************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <time.h>

#include <SpiceUsr.h>

#include "cds_status.h"
#include "rtiu.h"
#include "util.h"
#include "adb.h"


/*****************************************************************************/

static char *Version = { "V1.8a" };
extern char *ADB_Version;
extern char *optarg;
extern int optind, opterr, optopt;
static int file_type = ADB_U_FILE;

char *et2utc (double *et)
{
  static char scet_temp[32];
  char format[] = { "D" };
  int prec = 3;

  et2utc_ (et, format, &prec, scet_temp, strlen (format), 32);

              /******************************************
	       * Reformat to out standard string format *
	       ******************************************/
  scet_temp[8] = 'T';
  strcpy (&scet_temp[9], &scet_temp[12]);
  scet_temp[21] = 0;
  return scet_temp;
}

int main (int argc, char *argv[])
{
  struct ADB *adb_file;                 /* pointer to adb control structure */
  struct ADB_DETAIL *adb_detail;
  char c;
  int i;
  int error;
  int list_flag;
  char *start_time = NULL;
  char *stop_time = NULL;
  char *database_path = NULL;
  char *database_file = NULL;
  char filename[256];
  char detail[1024];
  double start;
  double stop;
  char* metafile = NULL;

  fprintf (stdout, "%d=sizeof(void*));\n", sizeof (void *));
  fprintf (stdout, "%d=sizeof(int*));\n", sizeof (int *));
  fprintf (stdout, "%d=sizeof(float*));\n", sizeof (float *));
  fprintf (stdout, "%d=sizeof(double*));\n", sizeof (double *));
  fprintf (stdout, "%d=sizeof(int));\n", sizeof (int));
  fprintf (stdout, "%d=sizeof(float));\n", sizeof (float));
  fprintf (stdout, "%d=sizeof(double));\n", sizeof (double));

        /********************************
	 *	Load SPICE kernels	*
	 *  adb makes use of FTN spice	*
	 *    routines, caller must 	*
	 *       initialize spice	*
	 ********************************/
  if( getenv("CAS_TIME_KERNELS") == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		return 13;
  }
  metafile = getenv("CAS_TIME_KERNELS");
  
  fprintf (stderr, "ldpool_ %s\n", leapfile);
  furnsh_c(metafile);


        /****************************************
	 *  Select file type to                 *
	 *   access,  We can do minipackets	*
	 *   as well as CDS packets             *
	 ****************************************/
  while ((c = getopt (argc, argv, "hqlp:n:")) != EOF) {
    switch (c) {
     case 'p':
       database_path = optarg;
       break;
     case 'n':
       database_file = optarg;
       break;
     case 'l':
       list_flag = 1;
       break;
     case '?':
       fprintf (stderr, "Invalid Command Line Arguments\n");
     case 'h':
     case 'q':
       fprintf (stderr, "%s <start> <stop>\n", argv[0]);
       fprintf (stderr, "\n");
       fprintf (stderr, "        Database maintenance program.\n");
       fprintf (stderr, "    Scan the database for time regressions\n");
       fprintf (stderr, "\n");
       fprintf (stderr, "\n");
       fprintf (stderr, "    -p name    Database Path (%s)\n", database_path);
       fprintf (stderr, "    -n name    Database Name (%s)\n", database_file);
       fprintf (stderr, "    -l         List database entries\n");
       fprintf (stderr, "    \n");
       fprintf (stderr, "    \n");
       fprintf (stderr, "    \n");
       fprintf (stderr, "\n");
       exit (0);
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
     break;
   case 1:
     start_time = argv[optind + 0];
     stop_time = NULL;
     break;
   default:
     fprintf (stderr, "Invalid Command Line Arguments (not enough)\n");
     fprintf (stderr, "<%s -h> for help\n", argv[0]);
     exit (0);
  }
  if (start_time) {
    i = ADB_SCET_strg (start_time);
    /**/
      fprintf (stderr, "start time %08X=ADB_SCET_strg(\"%s\");\n", i,
               start_time);
  /**/}
  if (stop_time) {
    i = ADB_SCET_strg (stop_time);
    /**/
      fprintf (stderr, "stop time %08X=ADB_SCET_strg(\"%s\");\n", i,
               stop_time);
  /**/}

        /****************************************
	 *  ADB_open with start/stop and        *
	 *   indication of which type to read   *
	 *   the 2 null arguments are path      *
	 *   and filename for the database      *
	 *   index.  We'll simply use the       *
	 *   defaults (they're correct).        *
	 ****************************************/
  adb_file =
    ADB_open (start_time, stop_time, database_path, database_file, file_type);
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
    adb_detail = ADB_detail (adb_file);
  start = -365. * 3.0 * 86400. - 1.0;
  stop = -365. * 3.0 * 86400.;
  while (adb_detail) {                  /* buf will be NULL when no more data */
    error = 0;

    /*
     *     Check for overlapping start/stop times
     */
    if (adb_detail->start_et < start)
      error |= 0x01;
    if (adb_detail->start_et < stop)
      error |= 0x02;
    if (adb_detail->stop_et < start)
      error |= 0x04;
    if (adb_detail->stop_et < stop)
      error |= 0x08;
    if (adb_detail->start_et > adb_detail->stop_et)
      error |= 0x10;

    /*
     *     Check for unusually long filenames
     */
    sprintf (filename, "%s/%s", adb_detail->pathname, adb_detail->filename);
    if (strlen (filename) > 59)
      error |= 0x100;
    strcat (filename, "                                ");
    filename[59] = 0;
    /*
     *     Rebuild fixed-length detail line
     */
    i = 0;
    sprintf (&detail[i], "%s ", et2utc (&adb_detail->start_et));
    i = strlen (detail);
    sprintf (&detail[i], "%s ", et2utc (&adb_detail->stop_et));
    i = strlen (detail);
    sprintf (&detail[i], "%08X ", adb_detail->start_sclk);
    i = strlen (detail);
    sprintf (&detail[i], "%08X ", adb_detail->stop_sclk);
    i = strlen (detail);
    sprintf (&detail[i], "%s ", filename);
    i = strlen (detail);
    sprintf (&detail[i], "%08X ", adb_detail->flags);
    i = strlen (detail);

    if (list_flag) {
      fprintf (stdout, "%d %s\n", i, detail);
    } else if (error)
      fprintf (stdout, "Error %d  %s\n", error, adb_detail->detail_line);

    start = adb_detail->start_et;
    stop = adb_detail->stop_et;
    adb_detail = ADB_detail (adb_file);
  }

        /****************************************
	 *  NO more data, so release resources	*
	 *    (we could call ADB_open again and *
	 *    read something else).             *
	 ****************************************/
  ADB_close (adb_file);
}
