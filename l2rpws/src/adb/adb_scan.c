#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "rtiu.h"
#define __adb__
#include "adb.h"

extern int ADB_debug;

/**/

  /**********************************************************************
   *    Strip CR or LF                                                  *
   **********************************************************************/
static int ADB_strip (char *buffer)
{
  char *temp;

  temp = strchr (buffer, '\n');
  if (temp)
    *temp = 0;
  temp = strchr (buffer, '\r');
  if (temp)
    *temp = 0;
}

/**/

  /**********************************************************************
   *	ADB_scan loads the ADB structure with the filename of the	*
   *	  next file from the index that is within the specified start	*
   *	  and stop times.						*
   *	Returns								*
   *		1	the ADB datafile_name has the name of the	*
   *				next file that should be processed	*
   *		0	No more files to process (i.e. EOF)		*
   **********************************************************************/
int ADB_scan (struct ADB *adb)
{
  int i;
  char *status;

  if (adb->datafile_file) {
    if (ADB_debug & ADB_DEBUG_DATAFILE)
      fprintf (stderr, "%s/%4d datafile fclose(%p) database line count(%d)\n",
               __FILE__, __LINE__,
               adb->datafile_file, adb->database_line_count);
    fclose (adb->datafile_file);
  }

  adb->database_start_sclk = 0;
  adb->database_stop_sclk = 0;
  adb->datafile_file = NULL;
  memset (&adb->status_buffer, 0, sizeof (struct stat));
  for (i = 1; i < ADB_TOKENS; i++)
    adb->database_line_tokens[i] = NULL;

  memset (adb->database_line, 0, ADB_LINE_BUFFER + 1);
  status = fgets (adb->database_line, ADB_LINE_BUFFER, adb->database_file);

/*   fprintf(stderr, "%4d Line %5d adb:%08X file:%08X line:%08X %s\n", 
			__LINE__, 
			adb->database_line_count+1, 
			adb, 
			adb->database_file, 
			adb->database_line, 
			adb->database_line);/**/
  adb->database_line_count++;
  if (strlen (adb->database_line))
    if (strlen (adb->database_line) < 10) {
      fprintf (stderr, "%s/%4d DATABASE EOF line %5d: \"%s\" %02X\n",
               __FILE__, __LINE__,
               adb->database_line_count,
               adb->database_line, adb->database_line[0] & 0xFF);
    }
  if (strlen (adb->database_line) < 10)
    fprintf (stderr, "%s/%4d DATABASE LINE ERROR %d: \"%s\"\n", __FILE__,
             __LINE__, adb->database_line_count, adb->database_line);

  strncpy (adb->database_detail_line, adb->database_line,
           ADB_LINE_BUFFER - 1);
  if (adb->database_detail_line[strlen (adb->database_detail_line) - 1] ==
      '\n')
    adb->database_detail_line[strlen (adb->database_detail_line) - 1] = 0;

  ADB_strip (adb->database_line);

  /*
   *      OK scan for appropriate file while
   *      we still have records in the database
   *      ADB_scan return 1 when we should process a file
   */
  while ((!ADB_scan1 (adb, status)) && status) {
    memset (adb->database_line, 0, ADB_LINE_BUFFER + 1);
    status = fgets (adb->database_line, ADB_LINE_BUFFER, adb->database_file);

/*       fprintf(stderr, "%4d Line %5d adb:%08X file:%08X line:%08X %s\n", 
				__LINE__, 
				adb->database_line_count+1, 
				adb, 
				adb->database_file, 
				adb->database_line, 
				adb->database_line);/**/
    if (status) {
      adb->database_line_count++;
      if (strlen (adb->database_line))
        if (strlen (adb->database_line) < 10) {
          fprintf (stderr, "%s/%4d DATABASE EOF line %5d: \"%s\" %02X\n",
                   __FILE__, __LINE__,
                   adb->database_line_count,
                   adb->database_line, adb->database_line[0] & 0xFF);
        }
      strncpy (adb->database_detail_line, adb->database_line,
               ADB_LINE_BUFFER - 1);
      if (adb->database_detail_line[strlen (adb->database_detail_line) - 1] ==
          0x0A)
        adb->database_detail_line[strlen (adb->database_detail_line) - 1] = 0;

      ADB_strip (adb->database_line);
    }
  }
  /*
   *      We either hit EOF or have a file to process
   */
  if (status) {
    return 1;                           /* file to precess */
  } else {
    return 0;                           /* EOF */
  }
}
