#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <time.h>

#include <SpiceUsr.h>

#include "rtiu.h"
#include "adb.h"
#include "UTIL_status.h"

/*****************************************************************************/

int main (int argc, char *argv[])
{
  int count = 0;
  int i;
  int sclk;
  struct ADB *adb_file;
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
  
  fprintf (stderr, "furnsh_c %s\n", leapfile);
  furnsh_c(leapfile);

  start_time = argv[argc - 1];
  if (argc > 2) {
    start_time = argv[argc - 2];
    stop_time = argv[argc - 1];
  }


  adb_file = ADB_open (start_time, stop_time, NULL, NULL, ADB_R_FILE);
  /**/
    fprintf (stderr, "%s/%d start_time %08X %s\n",
             __FILE__, __LINE__, adb_file->requested_start_sclk, start_time);
  if (stop_time)
    fprintf (stderr, "%s/%d stop_time  %08X %s\n",
             __FILE__, __LINE__, adb_file->requested_stop_sclk, stop_time);


  buf = ADB_read (adb_file);

  usec = (adb_file->read_time.tv_sec - adb_file->open_time.tv_sec) * 1000000;
  usec += adb_file->read_time.tv_usec - adb_file->open_time.tv_usec;

  fprintf (stderr, "database scan time %.3f sec\n", usec / 1000000.0);

  buf = ADB_read_stream (adb_file, packet_ID_value);
  while (buf) {
    UTIL_putbuffer_RPWS ((struct RPWS_buffer *) adb_file->buffer, stdout);
    buf = ADB_read_stream (adb_file, packet_ID_value);
  }
  ADB_close (adb_file);
}
