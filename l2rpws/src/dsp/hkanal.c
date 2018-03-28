
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

/* Cassini Stuff */

/* #include "CasTlmDef.h"*/
#include <cds_status.h>
#include <hsk_status.h>
#include <rtiu.h>
#include <util.h>
#include <mdb_time.h>
#include <mdb.h>
#include <rpws_status.h>


/*****************************************************************************/

static int SpaceCraft_ID = -82;
static char *Version = { "V0.0" };
extern char *MDB_Version;
extern char *optarg;
extern int optind, opterr, optopt;
static int file_type = MDB_R_FILE;

static int hkanal_help (char *argv, FILE * file)
{
  int i = 0;

  fprintf (file, "    \n");
  fprintf (file, "%s Housekeeping analysis %s, MDB Library Version %s\n",
           argv, Version, MDB_Version);
  fprintf (file, "    \n");
  fprintf (file, "  usage: %s start_time [stop_time]\n", argv);
  fprintf (file, "    \n");
  fprintf (file, "    \n");
  return 0;
}
int main (int argc, char *argv[])
{
  struct MDB *mdb_file;                 /* pointer to mdb control structure */
  struct MDB mdb_previous;
  struct MDB mdb_delta;
  struct RPWS_buffer *buf;              /* pointer to data buffer */
  int filter = 0;
  int count = 0;
  int i;
  int sclk;
  int c;
  int prec = 3;
  int previous_Command_Byte_Count;
  int previous_HSK_Valid_Command;
  double et;
  char *start_time = NULL;
  char *stop_time = NULL;
  char *database_path = NULL;
  char *database_file = NULL;
  char *options;
  char *value;
  char *time = { "1997-288T00:00:00" };
  char sclk_string[64];
  char scet_string[64];
  char format[] = { "D" };
  char* metafile = NULL;

  memset (&mdb_previous, 0, sizeof (struct MDB));
  memset (&mdb_delta, 0, sizeof (struct MDB));

  if (argc < 2) {
    hkanal_help (argv[0], stderr);
    exit (0);
  }

        /********************************
	 *	Load SPICE kernels	*
	 *  mdb makes use of FTN spice	*
	 *    routines, caller must 	*
	 *       initialize spice	*
	 ********************************/
  fprintf(stderr, "furnsh_c %s\n", metafile);
  
  if( getenv("CAS_TIME_KERNELS") == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		return 13;
  }
  metafile = getenv("CAS_TIME_KERNELS");
  furnsh_c(metafile);


  /*
   * i = MDB_SCET_strg(time); /*
   */
  /*
   * fprintf(stderr, "%08X=MDB_SCET_strg(\"%s\");\n", i, time); /*
   */

        /****************************************
	 *  Select file type to                 *
	 *   access,  We can do minipackets	*
	 *   as well as CDS packets             *
	 ****************************************/
  while ((c = getopt (argc, argv, "qh")) != EOF) {
    switch (c) {
     case 'h':
     case 'q':
       hkanal_help (argv[0], stdout);
       exit (0);
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
     hkanal_help (argv[0], stderr);
     fprintf (stderr, "Invalid Command Line Arguments (not enough)\n");
     exit (0);
  }
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
    buf = MDB_read_stream (mdb_file, CDS_Packet_ID_Housekeeping_Science);

  while (buf) {                         /* buf will be NULL when no more data */
    sprintf (sclk_string, "%d/%d:%03d",
             mdb_file->current_part,
             mdb_file->current_sclk, mdb_file->current_fine);
    scs2e_c(SpaceCraft_ID, sclk_string, &et);
    
	 et2utc_c(et, format, prec, 32, scet_string);
	 
    scet_string[23] = 0;
    strcpy (&scet_string[9], &scet_string[12]);
    scet_string[8] = 'T';

    mdb_delta.current_part =
      mdb_file->current_part - mdb_previous.current_part;
    mdb_delta.current_sclk =
      mdb_file->current_sclk - mdb_previous.current_sclk;
    mdb_delta.current_fine =
      mdb_file->current_fine - mdb_previous.current_fine;
    mdb_delta.current_RTI = mdb_file->current_RTI - mdb_previous.current_RTI;
    if (mdb_delta.current_RTI & 0xF8) {
      mdb_delta.current_RTI += 7;
      mdb_delta.current_sclk -= 1;
    }

    fprintf (stdout, "(%s) ", scet_string);
    fprintf (stdout, "%08X.%d ",
             mdb_file->current_sclk, mdb_file->current_RTI);

    fprintf (stdout, "%5d/%3d ",
             get_status (buf->packet.mpp.mini_packet,
                         HSK_Command_Byte_Count,
                         HSK_Command_Byte_Count_MSB),
             get_status (buf->packet.mpp.mini_packet, HSK_Valid_Command, 0));
    c = get_status (buf->packet.mpp.mini_packet,
                    HSK_Command_Byte_Count,
                    HSK_Command_Byte_Count_MSB) - previous_Command_Byte_Count;
    if (c)
      fprintf (stdout, "BYTE(%d) ", c);
    if (c < 0)
      c += 256;
    c = get_status (buf->packet.mpp.mini_packet,
                    HSK_Valid_Command, 0) - previous_HSK_Valid_Command;
    if (c < 0)
      c += 256;
    if (c)
      fprintf (stdout, "CMD(%d) ", c);
    fprintf (stdout, "\n");

    previous_Command_Byte_Count = get_status (buf->packet.mpp.mini_packet,
                                              HSK_Command_Byte_Count,
                                              HSK_Command_Byte_Count_MSB);
    previous_HSK_Valid_Command = get_status (buf->packet.mpp.mini_packet,
                                             HSK_Valid_Command, 0);
    mdb_previous.current_part = mdb_file->current_part;
    mdb_previous.current_sclk = mdb_file->current_sclk;
    mdb_previous.current_fine = mdb_file->current_fine;
    mdb_previous.current_RTI = mdb_file->current_RTI;
    buf = MDB_read_stream (mdb_file, CDS_Packet_ID_Housekeeping_Science);
  }

        /****************************************
	 *  NO more data, so release resources	*
	 *    (we could call MDB_open again and *
	 *    read something else).             *
	 ****************************************/
  MDB_close (mdb_file);
}
