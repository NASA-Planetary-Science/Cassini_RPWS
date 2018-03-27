#include <stdlib.h>
#include <stdio.h>
#include <string.h>


#include <rtiu.h>
#define __mdb__
#include <mdb.h>

extern int MDB_debug;

/**/

  /**********************************************************************
   *    Strip CR or LF                                                  *
   **********************************************************************/
static int MDB_strip (char *buffer)
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

  /**************************************************************************************
   *											*
   *	Scan through the database, looking for the appropriate				*
   *	record based on requested start time.  As long as the database stop time	*
   *	is less than the requested start time, we aren't quite there yet...		*
   *											*
   *	start-scet stop-scet start-sclk stop-sclk filename flags			*
   *	1999-254T14:00:26.831|          |         |        |				*
   *               1999-254T14:59:38.807|         |        |				*
   *                         4E6CC0A0   |         |        |				*
   *                                    4E6CCE80  |        |				*
   *                                              /opt/ project/ cassini /rework/		*
   *							earth_flyby_1/t19992541400.u00	*
   *                                                       00000086			*
   *											*
   **************************************************************************************/
static int MDB_scan1 (struct MDB *mdb, char *status)
{
  int flag = 0;
  int i;

  if (!status)
    return 0;
  if (!mdb)
    return 0;
  mdb->database_line_tokens[0] = strtok (mdb->database_line, " ");
  for (i = 1; i < MDB_TOKENS; i++) {
    mdb->database_line_tokens[i] = NULL;
    if (mdb->database_line_tokens[i - 1])
      mdb->database_line_tokens[i] = strtok (NULL, " ");
  }
  mdb->database_start_sclk = 0;
  if (mdb->database_line_tokens[2])
    mdb->database_start_sclk =
      strtol (mdb->database_line_tokens[2], NULL, 16);
  mdb->database_stop_sclk = 0;
  if (mdb->database_line_tokens[3])
    mdb->database_stop_sclk = strtol (mdb->database_line_tokens[3], NULL, 16);

        /************************************/
  /*
   * Discard data prior to start time 
   */

        /************************************/
  if ((mdb->requested_start_sclk > mdb->database_stop_sclk) && status)
    return 0;

        /****************************************/
  /*
   * Discard data following stop time     
   */
  /*
   * (only if a stop time was specified   
   */

        /****************************************/
  if (mdb->requested_stop_sclk)
    if ((mdb->requested_stop_sclk < mdb->database_start_sclk) && status)
      return 0;

  return 1;
}

/**/

  /**********************************************************************
   *	MDB_scan loads the MDB structure with the filename of the	*
   *	  next file from the index that is within the specified start	*
   *	  and stop times.						*
   *	Returns								*
   *		1	the MDB datafile_name has the name of the	*
   *				next file that should be processed	*
   *		0	No more files to process (i.e. EOF)		*
   **********************************************************************/
int MDB_scan (struct MDB *mdb)
{
  int i;
  char *status;

  if (mdb->datafile_file) {
    if (MDB_debug & MDB_DEBUG_DATAFILE)
      fprintf (stderr, "%s/%4d datafile fclose(%p) database line count(%d)\n",
               __FILE__, __LINE__,
               mdb->datafile_file, mdb->database_line_count);
    fclose (mdb->datafile_file);
  }

  mdb->database_start_sclk = 0;
  mdb->database_stop_sclk = 0;
  mdb->datafile_file = NULL;
  memset (&mdb->status_buffer, 0, sizeof (struct stat));
  for (i = 1; i < MDB_TOKENS; i++)
    mdb->database_line_tokens[i] = NULL;

  memset (mdb->database_line, 0, MDB_LINE_BUFFER + 1);
  status = fgets (mdb->database_line, MDB_LINE_BUFFER, mdb->database_file);

/*   fprintf(stderr, "%4d Line %5d mdb:%08X file:%08X line:%08X %s\n", 
			__LINE__, 
			mdb->database_line_count+1, 
			mdb, 
			mdb->database_file, 
			mdb->database_line, 
			mdb->database_line);/**/
  mdb->database_line_count++;
  if (strlen (mdb->database_line))
    if (strlen (mdb->database_line) < 10) {
      fprintf (stderr, "%s/%4d DATABASE EOF line %5d: \"%s\" %02X\n",
               __FILE__, __LINE__,
               mdb->database_line_count,
               mdb->database_line, mdb->database_line[0] & 0xFF);
    }

/*     if(strlen(mdb->database_line) < 10)
         fprintf(stderr, "%s/%4d DATABASE LINE ERROR %d: \"%s\"\n", __FILE__, __LINE__, mdb->database_line_count, mdb->database_line); /**/

  strncpy (mdb->database_detail_line, mdb->database_line,
           MDB_LINE_BUFFER - 1);
  if (mdb->database_detail_line[strlen (mdb->database_detail_line) - 1] ==
      '\n')
    mdb->database_detail_line[strlen (mdb->database_detail_line) - 1] = 0;

  MDB_strip (mdb->database_line);

  /*
   *      OK scan for appropriate file while
   *      we still have records in the database
   *      MDB_scan return 1 when we should process a file
   */
  while ((!MDB_scan1 (mdb, status)) && status) {
    memset (mdb->database_line, 0, MDB_LINE_BUFFER + 1);
    status = fgets (mdb->database_line, MDB_LINE_BUFFER, mdb->database_file);

/*       fprintf(stderr, "%4d Line %5d mdb:%08X file:%08X line:%08X %s\n", 
				__LINE__, 
				mdb->database_line_count+1, 
				mdb, 
				mdb->database_file, 
				mdb->database_line, 
				mdb->database_line);/**/
    if (status) {
      mdb->database_line_count++;
      if (strlen (mdb->database_line))
        if (strlen (mdb->database_line) < 10) {
          fprintf (stderr, "%s/%4d DATABASE EOF line %5d: \"%s\" %02X\n",
                   __FILE__, __LINE__,
                   mdb->database_line_count,
                   mdb->database_line, mdb->database_line[0] & 0xFF);
        }
      strncpy (mdb->database_detail_line, mdb->database_line,
               MDB_LINE_BUFFER - 1);
      if (mdb->database_detail_line[strlen (mdb->database_detail_line) - 1] ==
          0x0A)
        mdb->database_detail_line[strlen (mdb->database_detail_line) - 1] = 0;

      MDB_strip (mdb->database_line);
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
