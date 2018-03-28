/*********************************/

/****	mdblist, detail lines ****/

/*********************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include <SpiceUsr.h>

/* rpws includes */
#include <cds_status.h>
#include <rtiu.h>
#include <util.h>
#include <mdb.h>

#include "mdb_time.h"

/*****************************************************************************/

static char *Version = { "V1.8a" };
extern char *MDB_Version;
extern char *optarg;
extern int optind, opterr, optopt;
static int file_type = MDB_U_FILE;

char *et2utc (double et)
{
  static char scet_temp[32];
  const char* format = "D";
  int prec = 3;

  et2utc_c(et, format, prec, 32, scet_temp);

  /******************************************
	* Reformat to out standard string format *
	******************************************/
  scet_temp[8] = 'T';
  strcpy (&scet_temp[9], &scet_temp[12]);
  scet_temp[21] = 0;
  return scet_temp;
}

void prnHelp()
{
	fprintf (stderr, 
"\nrpws_mdb_list - RPWS master file table maintenance.\n"
"\n"
"USAGE:\n"
"    rpws_mdb_list [options] BEGIN_TIME END_TIME\n"
"\n"
"DESCRIPTION:\n"
"    Many Cassini RPWS programs process data within a given time range.  In\n"
"    order to determine which input files correspond with a given range a\n"
"    master location table is consulted.  It is assumed by many programs that\n"
"    master tables are in time order.  rpws_mdb_list may be used to test for\n"
"    time regressions an a master file table as well as for listing table\n"
"    entries.\n"
"\n"
"OPTIONS:\n"
"    -p dir    Look in an alternate directory for the master file table.  By\n"
"              default the environment variable RPWS_MPDB is used to determine\n"
"              directory containing the master file table.\n"
"\n"
"    -n name   Open a different master file table.  By default the environment\n"
"              variable RPWS_MPDB is used to determine the master file table's\n"
"              file name.\n"
"\n"
"    -l        List table entries\n"
"\n"
"ENVIRONMENT:\n"
"    The following environment variables affect rpws_mdb_list operations\n"
"\n"
"       RPWS_MPDB - Sets the default RPWS file lookup table.\n"
"       CAS_TIME_KERNELS - The location of the Cassini SCLK/SCET metakernel\n"
"\n"
"SEE ALSO:\n"
"    rpws_mdb_wrap\n"
"\n");

}

int main (int argc, char *argv[])
{
  struct MDB *mdb_file;                 /* pointer to mdb control structure */
  struct MDB_DETAIL *mdb_detail;
  char c;
  int i;
  int error;
  int list_flag;
  char *start_time = NULL;
  char *stop_time = NULL;
  const char *database_path = NULL;
  const char *database_file = NULL;
  char filename[256];
  char detail[1024];
  double start;
  double stop;
  char* metafile = NULL;

  /* fprintf (stdout, "%d=sizeof(void*));\n", sizeof (void *));
  fprintf (stdout, "%d=sizeof(int*));\n", sizeof (int *));
  fprintf (stdout, "%d=sizeof(float*));\n", sizeof (float *));
  fprintf (stdout, "%d=sizeof(double*));\n", sizeof (double *));
  fprintf (stdout, "%d=sizeof(int));\n", sizeof (int));
  fprintf (stdout, "%d=sizeof(float));\n", sizeof (float));
  fprintf (stdout, "%d=sizeof(double));\n", sizeof (double));
  */

	char sDefaultDbPath[128] = {'\0'};
	
	if(getenv("RPWS_MPDB") != NULL){
		strncpy(sDefaultDbPath, getenv("RPWS_MPDB"), 127);
		
		int iLast = strlen(sDefaultDbPath) - 1;
		for(i = iLast; i > 0; i--){
			if(sDefaultDbPath[i] != '/') continue;
			
			if(i == 0){
				database_path = "/";
				if(iLast > 1) database_file = sDefaultDbPath + 1;
				break;
			}

			database_path = sDefaultDbPath;
			sDefaultDbPath[i] = '\0';
			
			if(i == iLast) database_file = NULL;
			else           database_file = sDefaultDbPath + i + 1;
			
			break;
		}
		
		if(database_path == NULL) database_file = sDefaultDbPath;
		
		/* fprintf(stderr, "DEBUG: RPWS_MPDB=%s, database_path=%s, database_file=%s\n",
				  getenv("RPWS_MPDB"), database_path, database_file);
		*/
	}


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
		  prnHelp();
		  return 0;
		  break;		
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
  
  /* Load SPICE kernels mdb makes use of FTN spice routines */
		  
	if( getenv("CAS_TIME_KERNELS") == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		return 13;
	}
	metafile = getenv("CAS_TIME_KERNELS");
  
	fprintf (stderr, "furnsh_c %s\n", metafile);
	furnsh_c(metafile);
  
  
  if (start_time) {
    i = MDB_SCET_strg (start_time);
    /**/
      fprintf (stderr, "start time %08X=MDB_SCET_strg(\"%s\");\n", i,
               start_time);
  /**/}
  if (stop_time) {
    i = MDB_SCET_strg (stop_time);
    /**/
      fprintf (stderr, "stop time %08X=MDB_SCET_strg(\"%s\");\n", i,
               stop_time);
  /**/}

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
    mdb_detail = MDB_detail (mdb_file);
  start = -365. * 3.0 * 86400. - 1.0;
  stop = -365. * 3.0 * 86400.;
  while (mdb_detail) {                  /* buf will be NULL when no more data */
    error = 0;

    /*
     *     Check for overlapping start/stop times
     */
    if (mdb_detail->start_et < start)
      error |= 0x01;
    if (mdb_detail->start_et < stop)
      error |= 0x02;
    if (mdb_detail->stop_et < start)
      error |= 0x04;
    if (mdb_detail->stop_et < stop)
      error |= 0x08;
    if (mdb_detail->start_et > mdb_detail->stop_et)
      error |= 0x10;

    /*
     *     Check for unusually long filenames
     */
    sprintf (filename, "%s/%s", mdb_detail->pathname, mdb_detail->filename);
    if (strlen (filename) > 59)
      error |= 0x100;
    strcat (filename, "                                ");
    filename[59] = 0;
    /*
     *     Rebuild fixed-length detail line
     */
    i = 0;
    sprintf (&detail[i], "%s ", et2utc (mdb_detail->start_et));
    i = strlen (detail);
    sprintf (&detail[i], "%s ", et2utc (mdb_detail->stop_et));
    i = strlen (detail);
    sprintf (&detail[i], "%08X ", mdb_detail->start_sclk);
    i = strlen (detail);
    sprintf (&detail[i], "%08X ", mdb_detail->stop_sclk);
    i = strlen (detail);
    sprintf (&detail[i], "%s ", filename);
    i = strlen (detail);
    sprintf (&detail[i], "%08X ", mdb_detail->flags);
    i = strlen (detail);

    if (list_flag) {
      fprintf (stdout, "%d %s\n", i, detail);
    } else if (error)
      fprintf (stdout, "Error %d  %s\n", error, mdb_detail->detail_line);

    start = mdb_detail->start_et;
    stop = mdb_detail->stop_et;
    mdb_detail = MDB_detail (mdb_file);
  }

        /****************************************
	 *  NO more data, so release resources	*
	 *    (we could call MDB_open again and *
	 *    read something else).             *
	 ****************************************/
  MDB_close (mdb_file);
}
