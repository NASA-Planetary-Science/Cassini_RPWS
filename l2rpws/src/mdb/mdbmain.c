#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <time.h>

#include <SpiceUsr.h>

/* Cas Local */
#include <rtiu.h>
#include <mdb.h>
#include <rpws_status.h>


/*****************************************************************************/
int main (int argc, char *argv[])
{
  int count = 0;
  int i;
  int sclk;
  struct MDB *mdb_file;
  struct RPWS_buffer *buf;
  double usec;
  int packet_ID_value = CDS_Packet_ID_Housekeeping_Science;

  char *start_time = NULL;
  char *stop_time = NULL;

  fprintf (stderr, "argc %d, ", argc);
  for (i = 0; i < argc; i++)
    fprintf (stderr, "%s  ", argv[i]);
  fprintf (stderr, "\n");

  if (argc < 3)
    exit (0);
  fprintf (stderr, "ldpool_ %s\n", leapfile);
  fprintf (stderr, "ldpool_ %s\n", sclkfile);
  ldpool_ (leapfile, strlen (leapfile));
  ldpool_ (sclkfile, strlen (sclkfile));

  start_time = argv[argc - 1];
  if (argc > 2) {
    start_time = argv[argc - 2];
    stop_time = argv[argc - 1];
  }


  mdb_file = MDB_open (start_time, stop_time, NULL, NULL, MDB_R_FILE);
  /**/
    fprintf (stderr, "%s/%d start_time %08X %s\n",
             __FILE__, __LINE__, mdb_file->requested_start_sclk, start_time);
  if (stop_time)
    fprintf (stderr, "%s/%d stop_time  %08X %s\n",
             __FILE__, __LINE__, mdb_file->requested_stop_sclk, stop_time);


  buf = MDB_read (mdb_file);

  usec = (mdb_file->read_time.tv_sec - mdb_file->open_time.tv_sec) * 1000000;
  usec += mdb_file->read_time.tv_usec - mdb_file->open_time.tv_usec;

  fprintf (stderr, "database scan time %.3f sec\n", usec / 1000000.0);

  buf = MDB_read_stream (mdb_file, packet_ID_value);
  while (buf) {
    UTIL_putbuffer_RPWS ((struct RPWS_buffer *) mdb_file->buffer, stdout);
    buf = MDB_read_stream (mdb_file, packet_ID_value);
  }
  MDB_close (mdb_file);
}
