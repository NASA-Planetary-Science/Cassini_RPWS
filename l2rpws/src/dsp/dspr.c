
#include <SpiceUsr.h>

/*
 * dsp4.c
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <term.h>
#include <strings.h>

/* Cas Stuff */
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <cds_status.h>
#include <rpws_status.h>
#include <mdb.h>

/*****************************************************************************/

extern char *optarg;
extern int optind, optopt;

static char *Version = { "V1.0" };
static char *Title = { "CASSINI CDS data recovery analysis  (DSPR)" };
static char *database_path = NULL;
static char *database_file = NULL;
static int file_type[2] = { MDB_U_FILE, MDB_R_FILE };
struct RPWS_buffer *buffer;

static char *packet[] = { " 0 STIM",
  " 1 MFR ",
  " 2 HFR ",
  " 3     ",
  " 4 LP  ",
  " 5     ",
  " 6     ",
  " 7 LFDR",
  " 8 WFR ",
  " 9     ",
  "10     ",
  "11 DUST",
  "12     ",
  "13 MRO ",
  "14 WBR ",
  "15     ",
  "16     ",
  "17     ",
  "18     ",
  "19     ",
  "20     ",
  "21     ",
  "22     ",
  "23     ",
  "24     ",
  "25     ",
  "26     ",
  "27     ",
  "28     ",
  "29     ",
  "30     ",
  "31     ",
  "32  ALL",
  "33  RAW",
  ""
};

char *format_time (int seconds)
{
  static char result[32];
  int dys;
  int hrs;
  int min;
  int sec;

  dys = seconds / 86400;
  hrs = (seconds - (dys * 86400)) / 3600;
  min = (seconds - (dys * 86400) - (hrs * 3600)) / 60;
  sec = (seconds - (dys * 86400) - (hrs * 3600) - (min * 60));
  if (dys)
    sprintf (result, "%d-%02d:%02d:%02d", dys, hrs, min, sec);
  else
    sprintf (result, "%02d:%02d:%02d", hrs, min, sec);
  return result;
}
int main (int argc, char *argv[])
{
  struct MDB *mdb_old_file;
  struct MDB *mdb_new_file;
  int old_count[34] = { 34 * 0 };
  int new_count[34] = { 34 * 0 };
  int type;
  int start_time;
  int stop_time;
  int i;
  char c;
  
	if( getenv("CAS_TIME_KERNELS") == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		return 13;
  }
  
  furnsh_c(getenv("CAS_TIME_KERNELS"));

  if (strcmp (MDB_VERSION, MDB_Version)) {
    fprintf (stdout, "%s %s\n", MDB_VERSION, MDB_Version);
    fprintf (stdout, "There is a problem with MDB that MUST be resolved\n");
    exit (0);
  }
  while ((c = getopt (argc, argv, "p:n:h")) != EOF) {
    switch (c) {

     case 'p':
       database_path = optarg;
       break;
     case 'n':
       database_file = optarg;
       break;
     case 'h':
     default:
       fprintf (stdout, "Display data recovery quality statistics\n");
       fprintf (stdout, "     %s %s\n", Title, Version);
       fprintf (stdout, "     MDB library %s\n", MDB_Version);
       fprintf (stdout, "     \n");
       fprintf (stdout, " %s -flags <start_time> <stop_time>\n", argv[0]);
       fprintf (stdout, "     \n");
       fprintf (stdout, " -flags\n");
       fprintf (stdout, "     \n");
       fprintf (stdout, "     -p      database path\n");
       fprintf (stdout, "     \n");
       fprintf (stdout, "     -n      database name\n");
       fprintf (stdout, "     \n");
       fprintf (stdout, "     \n");
       fprintf (stdout, "     \n");
       fprintf (stdout, "     \n");
       fprintf (stdout, "     \n");
       exit (0);
    }
  }
  if (!database_path) {
    fprintf (stdout, " Specify Database Path\n");
    exit (0);
  }
  if (!database_file) {
    fprintf (stdout, " Specify Database Name\n");
    exit (0);
  }
  if (argc < 5) {
    fprintf (stdout, " %s -h\n", argv[0]);
    exit (0);
  }
  mdb_old_file = MDB_open (argv[argc - 2],
                           argv[argc - 1], NULL, NULL, file_type[0]);
  /**/ if (!mdb_old_file) {
    printf ("SHIT OLD\n");
    exit (0);
  }

  mdb_new_file = MDB_open (argv[argc - 2],
                           argv[argc - 1],
                           database_path, database_file, file_type[0]);
  /**/ if (!mdb_new_file) {
    printf ("SHIT NEW\n");
    exit (0);
  }
  fprintf (stdout, "\n");
  fprintf (stdout, "%s \n", argv[0]);
  for (i = 1; i < argc; i++)
    fprintf (stdout, "        %s\n", argv[i]);
  fprintf (stdout, "\n");
  start_time = MDB_time (argv[argc - 2]);
  stop_time = MDB_time (argv[argc - 1]);
  fprintf (stdout, "Delta-T  %s\n", format_time (stop_time - start_time));
  fprintf (stdout, "\n");
  fprintf (stdout, "Database %s/%s\n", database_path, database_file);
  fprintf (stdout, "\n");
  while (buffer = MDB_read (mdb_old_file)) {
    old_count[32]++;
    old_count[UTIL_extract_MP_type ((struct MP_buffer *) buffer)]++;
    if (old_count[32] % 1000 == 0)
      fprintf (stderr, "OLD U %6d\r", old_count[32]);
  }

  while (buffer = MDB_read (mdb_new_file)) {
    new_count[32]++;
    new_count[UTIL_extract_MP_type ((struct MP_buffer *) buffer)]++;
    if (new_count[32] % 1000 == 0)
      fprintf (stderr, "NEW U %6d\r", new_count[32]);
  }
  if (!MDB_rewind (mdb_old_file, file_type[1]))
    printf ("SHIT OLD\n");
  if (!MDB_rewind (mdb_new_file, file_type[1]))
    printf ("SHIT NEW\n");
  while (buffer = MDB_read (mdb_old_file)) {
    old_count[33]++;
    if (old_count[33] % 1000 == 0)
      fprintf (stderr, "OLD R %6d\r", old_count[33]);
  }

  while (buffer = MDB_read (mdb_new_file)) {
    new_count[33]++;
    if (new_count[33] % 1000 == 0)
      fprintf (stderr, "NEW R %6d\r", new_count[33]);
  }

  for (i = 0; i < 34; i++) {
    if (old_count[i] + new_count[i])
      fprintf (stdout, "%16s OLD:%5d  NEW:%5d", packet[i], old_count[i],
               new_count[i]);
    if (new_count[i] - old_count[i])
      fprintf (stdout, "  DIFF:%5d", new_count[i] - old_count[i]);
    if (old_count[i] + new_count[i])
      fprintf (stdout, "\n");
  }
  fflush (stdout);

  MDB_close (mdb_new_file);
  MDB_close (mdb_old_file);
  return 0;
}
