
/*
 * read_raw.c
 */

#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <das2/das1.h>

/* Other Cas module includes */
#include <mdb.h>
#include <fg.h>
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>
#include <UTIL_status.h>

#include "rpws_sclk.h"
#include "sort_merge.h"
#include "debug.h"

enum
{ SOURCE_UKNOWN = 0,
  SOURCE_USER = 1,
  SOURCE_STDIN = 2,
  SOURCE_MISC = 3,
  SOURCE_DATABASE = 4
};

char *source_[] = { "SOURCE_UKNOWN",
  "SOURCE_USER",
  "SOURCE_STDIN",
  "SOURCE_MISC",
  "SOURCE_DATABASE"
};
char *stdin_ = { "<stdin>" };

#define BUFFER_SIZE 1292
char *read_raw_Version = { "V2.1" };

extern char database[];

struct SORT_ELEMENT *sort_element_head = NULL;

static int cds_time_bits[4] = { CDS_Time_Bits_07_00,
  CDS_Time_Bits_15_08,
  CDS_Time_Bits_23_16,
  CDS_Time_Bits_31_24
};

extern int debug_flag;

int raw_read_stat_read = 0;
int raw_read_stat_dupe = 0;
int raw_read_stat_head = 0;
int raw_read_stat_tail = 0;
int raw_read_stat_inst = 0;
int raw_read_stat_null = 0;

  /*
   * 
   */

  /**********************************************************************
   *	Pick the better record 						*
   *	NON-zero if the second_element is better than the first_element.*
   *--------------------------------------------------------------------*
   *	We'll probaby suck in the new data first, to get an idea of	*
   *	  what to merge, SO for now, we'll prefer the new data 		*
   *	     (from the 1st. pass)					*
   **********************************************************************/
#define CDS_RECORD_LENGTH 1
#define NON_FILL_LENGTH 2
static int select_better_record (struct SORT_ELEMENT *first_element,
                                 struct SORT_ELEMENT *second_element)
{
  int result = 0;
  int first_cds_record_length, second_cds_record_length;
  int first_cds_error_flags, second_cds_error_flags;
  int first_non_fill_length, second_non_fill_length;

/*     first_	= first_element->buffer->packet.chdo_ancillary.type_92. /**/

/*     second_	= second_element->buffer->packet.chdo_ancillary.type_92. /**/


  first_cds_error_flags =
    first_element->buffer->packet.chdo_ancillary.type_94.cds_error_flags;
  second_cds_error_flags =
    second_element->buffer->packet.chdo_ancillary.type_94.cds_error_flags;

  first_non_fill_length =
    first_element->buffer->packet.chdo_ancillary.type_94.non_fill_length;
  second_non_fill_length =
    second_element->buffer->packet.chdo_ancillary.type_94.non_fill_length;

  first_cds_record_length =
    get_status (first_element->buffer->packet.cds.header, CDS_Length_LSB,
                CDS_Length_MSB);
  second_cds_record_length =
    get_status (second_element->buffer->packet.cds.header, CDS_Length_LSB,
                CDS_Length_MSB);

     /***************************************************
      *   (Why is the second record better?)		*
      ***************************************************/
  if (second_non_fill_length > first_non_fill_length)
    result |= NON_FILL_LENGTH;

  return result;
}


  /*
   * 
   */

  /**********************************************************************
   *	After collecting a filename from the database, we have to	*
   *	rebuild it into a raw filename, scan for the 'dot' delimiter	*
   *	in the filename string, replacing it with a zero, and then	*
   *	append the raw filetype ".r00".					*
   **********************************************************************/
static char *rebuild_filename (char *filename)
{
  static char new_filename[256] = { "" };
  char *temp;

  if (filename)
    strcpy (new_filename, filename);
  temp = strrchr (new_filename, '.');
  if (temp)
    *temp = 0;
  strcat (new_filename, ".r00");
  return new_filename;
}

  /*
   * 
   */

  /**********************************************************************
   *	Extract a filename from the database file			*
   *		the database file is a simple list of files we 		*
   *		have available.  Each detail line has a start and 	*
   *		stop time expressed in both SCLK and SCET.		*
   **********************************************************************/
static char *extract_filename (char *buf, unsigned int *sclk, char **start,
                               char **stop, int source_flag)
{
  static char *delim = { " \t" };
  static char buffer[256];
  char *tokens[6];
  int i;

  memset (buffer, 0, 256);              /* pre clear */
  strcpy (buffer, buf);                 /* line from database */
  tokens[0] = strtok (buffer, delim);
  for (i = 1; i < 6; i++) {
    tokens[i] = NULL;
    if (tokens[i - 1]) {
      tokens[i] = strtok (NULL, delim);
    }
  }
  sclk[0] = 0;
  *start = tokens[0];
  *stop = tokens[1];
  if (tokens[3])
    sclk[0] = strtol (tokens[3], NULL, 16);
  i = 0;
  switch (source_flag) {
   case SOURCE_USER:                   /* only thing online */
     i = 0;
     break;
   case SOURCE_STDIN:                  /* 1 fileanme per line ??? */
     i = 0;
     break;
   case SOURCE_MISC:                   /* not used */
     i = 0;
     break;
   case SOURCE_DATABASE:               /* 4th. item on line */
     i = 4;
     break;
  }
  return tokens[i];
}

  /*
   * 
   */

  /**********************************************************************
   *	Discard any data records with a SCLK that is prior to		*
   *	  launch.  The database contains some pre-launch test data.	*
   *	ALSO any bench model data is discarded here as the UNIX time	*
   *	appears to be prior to launch (i.e. SCLK epoch differ enought	*
   *	 to make UNIX time-tags look like they're before launch)	*
   **********************************************************************/
static char *discard_prelaunch (char *filename, unsigned int *sclk)
{
  if (sclk[0] < CASSINI_FLIGHT_SCLK)
    return NULL;
  return filename;
}

  /*
   * 
   */

  /**********************************************************************
   *	Time Comparison							*
   *	 compare two times, kind of like strcmp				*
   *	 we go to the work of normalizing things to make life easier	*
   **********************************************************************/
static int compare_times (char *time1, char *time2)
{
  double d_time1, d_time2;
  struct DATABASE_TIME_TAG p_time1, p_time2;

  if (!time1 & !time2)
    return 0;
  if (!time1)
    return -1;
  if (!time2)
    return 1;

  parsetime (time1,
             &p_time1.year,
             &p_time1.month,
             &p_time1.mday,
             &p_time1.yday, &p_time1.hour, &p_time1.minute, &p_time1.second);
  d_time1 = ttime (&p_time1.year,
                   &p_time1.month,
                   &p_time1.mday,
                   &p_time1.yday,
                   &p_time1.hour, &p_time1.minute, &p_time1.second);

  parsetime (time2,
             &p_time2.year,
             &p_time2.month,
             &p_time2.mday,
             &p_time2.yday, &p_time2.hour, &p_time2.minute, &p_time2.second);

  d_time2 = ttime (&p_time2.year,
                   &p_time2.month,
                   &p_time2.mday,
                   &p_time2.yday,
                   &p_time2.hour, &p_time2.minute, &p_time2.second);
  if (d_time1 < d_time2)
    return -1;
  if (d_time1 > d_time2)
    return 1;
  return 0;
}

static char *discard_before_start (char *filename, char *db_stop_time,
                                   char *start_time, int source_flag)
{
  if (source_flag == SOURCE_DATABASE)
    if (compare_times (db_stop_time, start_time) < 0)
      return NULL;
  return filename;
}

static char *discard_after_stop (char *filename, char *db_start_time,
                                 char *stop_time, int source_flag)
{
  if (source_flag == SOURCE_DATABASE)
    if (compare_times (db_start_time, stop_time) > 0)
      return NULL;
  return filename;
}


  /*
   * 
   */

  /**********************************************************************
   *	Sort record into list						*
   *	To speed things up, we keep track of the insertion point	*
   *	 int the list.  Next time through, we can check to see that	*
   *	 the next record belongs here (i.e. SCLK increasing) and skip	*
   *	 a scan.  If it doesn't belong, we go back to the begining	*
   *	 of the lsit and perform a scan.				*
   **********************************************************************/

static int sort_record (struct SORT_ELEMENT *new_element)
{
  struct SORT_ELEMENT *sort_temp[2];
  static struct SORT_ELEMENT *starting_point[3][2] = { 3 * 2 * NULL };
  int starting_point_index;
  double element_sclk;
  static double old_sclk = 0.0;
  double list_sclk[2];
  int element_CDS_Packet_ID;
  int list_CDS_Packet_ID[2];
  int iundex;

  static int sort_record_count = 0;

  /**********************************************
   *	Don't call us with a NULL argument !!!	*
   **********************************************/
  if (!new_element) {
    if (debug_flag & DEBUG_INSERT) {
      fprintf (stdout, "%s/%3d NULL\n", __FILE__, __LINE__);
    }
    raw_read_stat_null++;
    if (debug_flag & DEBUG_INSERT) {
      fprintf (stdout, "\n");
      fflush (stdout);
    }
    return 0;
  }

  sort_record_count++;

  /**********************
   *	WHO & WHEN	*
   **********************/
  element_sclk =
    (double) get_status_32 (new_element->buffer->packet.cds.header,
                            cds_time_bits, __LINE__);
  element_sclk +=
    (double) get_status (new_element->buffer->packet.cds.header,
                         CDS_Time_Bits_SUB, 0) / 256.0;
  element_CDS_Packet_ID =
    get_status (new_element->buffer->packet.cds.header, CDS_Packet_ID, 0);
  /*
   *      Jan 2004
   *      Feb 2004 (this hack occurs elsewhere)
   *
   *      ONE other thing we're up to here is to make
   *      HSK time unique by moving it 102 mSec into
   *      the RTI period.  Even though we don't really
   *      keep track of it that way, in this routine 
   *      we use the sequence number to force HRS to
   *      sort correctly (and LRS/HSK don't ever end
   *      up with the same time as HRS, but HSK and LRS
   *      can have identical time-stamps.  NOW I know
   *      that HRS will only mark the sub-RTI portion
   *      of the time field (bits 4,3,2,1 specifically)
   *      with the patterns 000 through 1100, while
   *      LRS and HSK always mark this part with the
   *      pattern 000, what I'm doing, effectively, is
   *      marking HSK with a pattern of 1101 in the SUB-RTI
   *      field when calculating a time to be used when
   *      inserting records...
   */
  switch (element_CDS_Packet_ID) {
   case CDS_Packet_ID_Housekeeping_ROM:
   case CDS_Packet_ID_Housekeeping_Deploy:
   case CDS_Packet_ID_Housekeeping_Science:
     element_sclk += .102;              /* make housekeeping sort at the end 2/04 */
     break;
  }

  /****************************************************************
   *	We keep track of the previous HSK, LRS & LRS time	  *
   *	  separately.  Time regressions are normal between	  *
   *	  these three type of data, but won't occur frequently	  *
   *	  in real-time (LRS and HRS are released asynchrounously  *
   *	  with respect to eachother, so real-time WILL HAVE	  *
   *	  time discontinuities).				  *
   ****************************************************************/

  switch (element_CDS_Packet_ID) {
   case CDS_Packet_ID_Housekeeping_ROM:
   case CDS_Packet_ID_Housekeeping_Deploy:
   case CDS_Packet_ID_Housekeeping_Science:
     starting_point_index = 0;
     break;
   case CDS_Packet_ID_Segmented_LRS_I:
   case CDS_Packet_ID_UnSegmented_LRS_I:
     starting_point_index = 1;
     break;
   case CDS_Packet_ID_Segmented_HRS_I:
   case CDS_Packet_ID_UnSegmented_HRS_I:
     starting_point_index = 2;
     break;
  }

  /**********************
   **********************/

  if (debug_flag & (DEBUG_INSERT | DEBUG_SCLK)) {
    fprintf (stdout, "%s/%3d sort_record %08p ",
             __FILE__, __LINE__, new_element);
    if (debug_flag & DEBUG_SCLK) {
      fprintf (stdout, "%08X.%d:%d-%d  %.3f ",
               get_status_32 (new_element->buffer->packet.cds.header,
                              cds_time_bits,
                              __LINE__),
               get_status (new_element->buffer->packet.cds.header,
                           CDS_Time_Bits_RTI, 0),
               get_status (new_element->buffer->packet.cds.header,
                           CDS_Time_Bits_SEQ, 0),
               get_status (new_element->buffer->packet.cds.header,
                           CDS_Time_Bits_TQF, 0), element_sclk);
      fprintf (stdout, "%7.3f ", element_sclk - old_sclk);
      old_sclk = element_sclk;
      if (UTIL_extract_HRS (new_element->buffer))
        fprintf (stdout, "HRS ");
      if (UTIL_extract_LRS (new_element->buffer))
        fprintf (stdout, "LRS ");
      if (UTIL_extract_HSK (new_element->buffer))
        fprintf (stdout, "HSK ");
    }
    fprintf (stdout, "\n");
    fflush (stdout);
  }


     /***********************************************************
      *	First time through?					*
      *	    IF so, just stick it in the head and return...	*
      ***********************************************************/
  if (!sort_element_head) {
    sort_element_head = new_element;
    if (debug_flag & DEBUG_INSERT) {
      fprintf (stdout, "%s/%3d HEAD, NEW\n", __FILE__, __LINE__);
    }
    raw_read_stat_head++;
    if (debug_flag & DEBUG_INSERT) {
      fprintf (stdout, "\n");
      fflush (stdout);
    }
    return 1;
  }


    /********************************************************************
     *	See if we can start up where we left off ???			*
     *	  starting_point is left over from last time around...		*
     *    look at the time of the new packet, if it is			*
     *	  greater than the starting point from the last insert, simply	*
     *	  take off from there (by hotwiring "sort_temp" with the	*
     *	  poniters from last time around).				*
     ********************************************************************/

  sort_temp[0] = NULL;
  sort_temp[1] = sort_element_head;

  if (starting_point[starting_point_index][0]) {
    list_sclk[0] =
      (double) get_status_32 (starting_point[starting_point_index][0]->
                              buffer->packet.cds.header, cds_time_bits,
                              __LINE__);
    list_sclk[0] +=
      (double) get_status (starting_point[starting_point_index][0]->buffer->
                           packet.cds.header, CDS_Time_Bits_SUB, 0) / 256.0;
    list_CDS_Packet_ID[0] =
      get_status (starting_point[starting_point_index][0]->buffer->packet.cds.
                  header, CDS_Packet_ID, 0);
    switch (list_CDS_Packet_ID[0]) {
     case CDS_Packet_ID_Housekeeping_ROM:
     case CDS_Packet_ID_Housekeeping_Deploy:
     case CDS_Packet_ID_Housekeeping_Science:
       list_sclk[0] += .102;            /* make housekeeping sort at the end 2/04 */
       break;
    }
    list_sclk[1] =
      (double) get_status_32 (starting_point[starting_point_index][1]->
                              buffer->packet.cds.header, cds_time_bits,
                              __LINE__);
    list_sclk[1] +=
      (double) get_status (starting_point[starting_point_index][1]->buffer->
                           packet.cds.header, CDS_Time_Bits_SUB, 0) / 256.0;
    list_CDS_Packet_ID[1] =
      get_status (starting_point[starting_point_index][1]->buffer->packet.cds.
                  header, CDS_Packet_ID, 0);
    switch (list_CDS_Packet_ID[1]) {
     case CDS_Packet_ID_Housekeeping_ROM:
     case CDS_Packet_ID_Housekeeping_Deploy:
     case CDS_Packet_ID_Housekeeping_Science:
       list_sclk[1] += .102;            /* make housekeeping sort at the end 2/04 */
       break;
    }
    if (debug_flag & DEBUG_INSERT) {
      fprintf (stdout, "%s/%3d", __FILE__, __LINE__);
      fprintf (stdout, " (new)%08X.%03.0f >",
               (int) element_sclk,
               (element_sclk - (int) element_sclk) * 1000.);
      fprintf (stdout, " %08X.%03.0f(prev) ",
               (int) list_sclk[1],
               (list_sclk[1] - (int) list_sclk[1]) * 1000.);
      fprintf (stdout, "%8.3f ", element_sclk - list_sclk[1]);
      fflush (stdout);
    }

    if (element_sclk > list_sclk[1]) {
      sort_temp[0] = starting_point[starting_point_index][0];
      sort_temp[1] = starting_point[starting_point_index][1];
      if (debug_flag & DEBUG_INSERT) {
        fprintf (stdout, "MID ");
        fflush (stdout);
      }
    } else {
      if (debug_flag & DEBUG_PERF) {
        fprintf (stdout, ".");
        fflush (stdout);
      }
    }

    if (debug_flag & DEBUG_INSERT) {
      fprintf (stdout, "***\n");
      fflush (stdout);
    }

  }

     /*******************************************
      *	  OK, sort_temp is all set up so we'll	*
      *	either start at the begining, or after 	*
      *	the last record we read.		*
      *******************************************/
  iundex = 1;
  while (sort_temp[1]) {

    if (debug_flag & DEBUG_INSERT)
      fprintf (stdout, "%d", iundex++);

    /****************************************************
     *							*
     *	[0]	<- we have to mess with his		*
     *			link to get a new record	*
     *			correctly inserted...		*
     *	[1]	<- this is the one we're looking at	*
     *			for the time comparison		*
     *							*
     ****************************************************/
    if (sort_temp[0]) {
      list_sclk[0] =
        (double) get_status_32 (sort_temp[0]->buffer->packet.cds.header,
                                cds_time_bits, __LINE__);
      list_sclk[0] +=
        (double) get_status (sort_temp[0]->buffer->packet.cds.header,
                             CDS_Time_Bits_SUB, 0) / 256.0;
      list_CDS_Packet_ID[0] =
        get_status (sort_temp[0]->buffer->packet.cds.header, CDS_Packet_ID,
                    0);
      switch (list_CDS_Packet_ID[0]) {
       case CDS_Packet_ID_Housekeeping_ROM:
       case CDS_Packet_ID_Housekeeping_Deploy:
       case CDS_Packet_ID_Housekeeping_Science:
         list_sclk[0] += .102;          /* make housekeeping sort at the end 2/04 */
         break;
      }
    } else {
      list_sclk[0] = 0.0;
      list_CDS_Packet_ID[0] = 0;
    }

    list_sclk[1] =
      (double) get_status_32 (sort_temp[1]->buffer->packet.cds.header,
                              cds_time_bits, __LINE__);
    list_sclk[1] +=
      (double) get_status (sort_temp[1]->buffer->packet.cds.header,
                           CDS_Time_Bits_SUB, 0) / 256.0;
    list_CDS_Packet_ID[1] =
      get_status (sort_temp[1]->buffer->packet.cds.header, CDS_Packet_ID, 0);
    switch (list_CDS_Packet_ID[1]) {
     case CDS_Packet_ID_Housekeeping_ROM:
     case CDS_Packet_ID_Housekeeping_Deploy:
     case CDS_Packet_ID_Housekeeping_Science:
       list_sclk[1] += .102;            /* make housekeeping sort at the end 2/04 */
       break;
    }

         /***********************************************************************
          *	Check to see if these records are unique			*
          *	IF they match, we pick the better of the two,			*
          *	  and possibly copy the incoming record into the existing	*
          *	  element.  Either way, we return ZERO to tell caller		*
          *	  to release the buffer...					*
          ***********************************************************************/
    if ((fabs (element_sclk - list_sclk[1]) < 0.001) &&
        (element_CDS_Packet_ID == list_CDS_Packet_ID[1])) {
      if (debug_flag & DEBUG_INSERT)
        fprintf (stdout, "%s/%3d DUPE\n", __FILE__, __LINE__);
      if (select_better_record (sort_temp[1], new_element)) {
        if (debug_flag & DEBUG_SELECT)
          fprintf (stdout, "%s/%d selected old record\n", __FILE__, __LINE__);
        memcpy ((void *) sort_temp[1]->buffer,
                (void *) new_element->buffer,
                sizeof (struct SORT_ELEMENT) - sizeof (void *) + BUFFER_SIZE);
      /**/}
      raw_read_stat_dupe++;
      if (debug_flag & DEBUG_INSERT) {
        fprintf (stdout, "\n");
        fflush (stdout);
      }
      return 0;
    }

         /***************************************************
          *	We may have just found the insertion point of this
          *	record.  It has a time that is less than the current
          *	element:
          *		  Can't be less than the previous, as it didn't
          *		trip this test last time around.
          *		  Can't be equal or we'd have tripped the test
          *		just prior to this test.
          *		  THEREFORE, it must be greater than this element.
          */
    if ((element_sclk > list_sclk[0]) && (element_sclk < list_sclk[1])) {
      if (sort_temp[0]) {               /* insert after [0] */
        if (debug_flag & DEBUG_INSERT)
          fprintf (stdout, "%s/%3d INSERT "
                   "[0]->%08X.%03.0f ",
                   __FILE__, __LINE__,
                   get_status_32 (sort_temp[0]->buffer->packet.cds.header,
                                  cds_time_bits,
                                  __LINE__),
                   (double) get_status (sort_temp[0]->buffer->packet.cds.
                                        header, CDS_Time_Bits_SUB_Second,
                                        0) / 256.0 * 1000.0);

        new_element->link = sort_temp[0]->link;
        sort_temp[0]->link = new_element;

        if (debug_flag & DEBUG_INSERT)
          fprintf (stdout, "%p ", sort_temp[0]->link);
        raw_read_stat_inst++;
      } else {                          /* new head */

        if (debug_flag & DEBUG_INSERT)
          fprintf (stdout, "%s/%3d NEW HEAD ", __FILE__, __LINE__);
        new_element->link = sort_element_head;
        sort_element_head = new_element;
        raw_read_stat_head++;
      }
      if (debug_flag & DEBUG_INSERT) {
        fprintf (stdout, "[n]->%08X.%03.0f ",
                 get_status_32 (new_element->buffer->packet.cds.header,
                                cds_time_bits,
                                __LINE__),
                 (double) get_status (new_element->buffer->packet.cds.header,
                                      CDS_Time_Bits_SUB_Second,
                                      0) / 256.0 * 1000.);

        fprintf (stdout, "%p ", new_element->link);
        fprintf (stdout, "[1]->");
        if (sort_temp[1]) {
          fprintf (stdout, "%08X.%03.0f ",
                   get_status_32 (sort_temp[1]->buffer->packet.cds.header,
                                  cds_time_bits,
                                  __LINE__),
                   (double) get_status (sort_temp[1]->buffer->packet.cds.
                                        header, CDS_Time_Bits_SUB_Second,
                                        0) / 256.0 * 1000.);
          fprintf (stdout, "%p ", sort_temp[1]->link);
        }
        fprintf (stdout, "\n\n");
      }                                 /* if(debug_flag & DEBUG_INSERT) */
      return 2;
    }                                   /* if(element_sclk < list_sclk[1]) */
#define _FAST_

#ifdef _FAST_
    starting_point[starting_point_index][0] = sort_temp[0];
    starting_point[starting_point_index][1] = sort_temp[1];
#endif

    sort_temp[0] = sort_temp[1];
    sort_temp[1] = sort_temp[1]->link;
  }                                     /* while(sort_temp[1]) */
  /*
   *        sort_temp[1] is now NULL, so we're at the
   *        end of the list.  sort_temp[0].link must
   *        be NULL, so update it...
   */
  sort_temp[0]->link = new_element;     /* new tail */
  new_element->link = NULL;             /* POINTS NOWHERE */
  if (debug_flag & (DEBUG_INSERT || DEBUG_TAIL)) {
    fprintf (stdout, "%s/%3d TAIL(%d) ", __FILE__, __LINE__,
             sort_record_count);
    fprintf (stdout, "%08X.%d:%d-%d  ",
             get_status_32 (new_element->buffer->packet.cds.header,
                            cds_time_bits,
                            __LINE__),
             get_status (new_element->buffer->packet.cds.header,
                         CDS_Time_Bits_RTI, 0),
             get_status (new_element->buffer->packet.cds.header,
                         CDS_Time_Bits_SEQ, 0),
             get_status (new_element->buffer->packet.cds.header,
                         CDS_Time_Bits_TQF, 0));
    fprintf (stdout, "\n\n");
  }
  raw_read_stat_tail++;
  return 3;
}

  /*
   * 
   */

  /**********************************************************************
   *	Read next record						*
   *		skip bad records in the raw file (shouldn't occur)	*
   *	BAD RECORD Criteria						*
   *		ZERO length in CDS header				*
   *		Valid CDS Packet ID (5 bits) 2xHRS and 2xLRS and 3xHSK	*
   **********************************************************************/
static int util_getbuffer_CDS (struct CDS_buffer *buffer, FILE * input,
                               int eof_flag)
{
  int ilen = 0;
  int bad_record = 1;
  int i;
  int cds_packet_length;
  int local_eof_flag = eof_flag;
  static char t_buffer[65536];

  while (bad_record) {
    bad_record = 0;
    ilen =
      UTIL_getbuffer_CDS ((struct CDS_buffer *) t_buffer, input,
                          local_eof_flag);
    memcpy (buffer, t_buffer, BUFFER_SIZE);
    if (ilen > 0) {                     /* data to read ??? */
      cds_packet_length = get_status (buffer->packet.cds.header,
                                      CDS_Length_LSB, CDS_Length_MSB);
      if (cds_packet_length == 0)       /* funny record encountered ? */
        bad_record = 1;                 /*   then dump it 1 */
      else {                            /* cds_packet_length > 0 */

        switch (get_status (buffer->packet.cds.header, CDS_Packet_ID, 0)) {
         case CDS_Packet_ID_UnSegmented_LRS_I:
         case CDS_Packet_ID_UnSegmented_HRS_I:
         case CDS_Packet_ID_Segmented_LRS_I:
         case CDS_Packet_ID_Segmented_HRS_I:
         case CDS_Packet_ID_Housekeeping_ROM:
         case CDS_Packet_ID_Housekeeping_Deploy:
         case CDS_Packet_ID_Housekeeping_Science:
           bad_record = 0;              /* accept this record */
           break;
         default:
           bad_record = 1;              /* not interested */
           break;                       /* in this record */
        }
      }                                 /* cds_packet_length > 0 */
    } /* if (ilen>0) */
    else {                              /* if(ilen>0)  */
      /*
       * EOF EOF EOF EOF 
       */
      bad_record = 0;
      ilen = 0;
    }                                   /* if(ilen>0)  */
  }                                     /* bad record */
  return ilen;
}

  /*
   * 
   */

  /**********************************************************************
   *	Memory management, this is (should be) the only occurence of 	*
   *	  malloc, so if it fails, puke and die (we're basically		*
   *	  screwed if we can't acquire memory).				*
   **********************************************************************/
static void *Malloc (int size)
{
  void *buffer;

  buffer = malloc (size);
  if (!buffer) {
    fprintf (stderr,
             "SHIT! malloc failed, out of memory you cheap bastard\n");
    exit (0);
  }
  return buffer;
}
static void Free (void *buf)
{
  free (buf);
  return;
}

  /*
   * 
   */

  /**********************************************************************
   *	Read 1 File							*
   *	This is the ONLY place records are read				*
   *	 and inserted into the sorted list (i.e. we'll sort everything	*
   *	 even if we think it's already sorted)				*
   **********************************************************************/
static int main_file (FILE * input)
{
  int record_count = 0;
  int ilen;
  int eof_flag = UTIL_GET_NON_BLOCKING;
  struct SORT_ELEMENT *new_element;

  new_element = Malloc (sizeof (struct SORT_ELEMENT) + BUFFER_SIZE);
  new_element->buffer = (struct CDS_buffer *) new_element->data;
  ilen = util_getbuffer_CDS (new_element->buffer, input, eof_flag);
  if (new_element->buffer->f_length > BUFFER_SIZE)
    new_element->buffer->f_length = BUFFER_SIZE;
  while (ilen > 0) {
    raw_read_stat_read++;

    record_count++;                     /**********************************/
    if (sort_record (new_element)) {    /* if record inserted into buffer *//* we need another, othrewise     */
      new_element = Malloc (sizeof (struct SORT_ELEMENT) + BUFFER_SIZE);
      new_element->buffer = (struct CDS_buffer *) new_element->data;
    }
    /*
     * we'll clear it out an reuse it 
     */
    if (debug_flag & DEBUG_COUNT)
      if (!(record_count % 1000))
        fprintf (stdout, "%s/%d   rec cnt %d\n", __FILE__, __LINE__,
                 record_count);
    memset (new_element->buffer, 0, 256 + 12);  /* clear out header */
    ilen = util_getbuffer_CDS (new_element->buffer, input, eof_flag);
    if (new_element->buffer->f_length > BUFFER_SIZE)
      new_element->buffer->f_length = BUFFER_SIZE;
  }
  return record_count;
}

  /**********************************************************************
   *	MDB_read							*
   *	Access the database using start and stop times...		*
   *	(too much of a pain to read the database file and do each	*
   *	  individual file, even though we have the code in place...)	*
   **********************************************************************/
struct SORT_ELEMENT *read_mdb_file (char *start, char *stop)
{
  struct SORT_ELEMENT *new_element;
  struct RPWS_buffer *r_buf;
  struct MDB *mdb;
  int length;
  int record_count = 0;

  new_element = Malloc (sizeof (struct SORT_ELEMENT) + BUFFER_SIZE);
  new_element->buffer = (struct CDS_buffer *) new_element->data;
  mdb = MDB_open (start, stop, NULL, NULL, MDB_R_FILE);

  r_buf = MDB_read (mdb);
  while (r_buf) {
    length = r_buf->f_length + 4;
    if (length > BUFFER_SIZE + 4)
      length = BUFFER_SIZE - 4;
    memcpy (new_element->buffer, r_buf, length);
    raw_read_stat_read++;
    record_count++;
#define SHIT

#ifdef SHIT
    if (sort_record (new_element)) {    /* if record inserted into buffer *//* we need another, othrewise     */
      new_element = Malloc (sizeof (struct SORT_ELEMENT) + BUFFER_SIZE);
      new_element->buffer = (struct CDS_buffer *) new_element->data;
    }                                   /* we'll clear it out an reuse it */
    memset (new_element->buffer, 0, 256 + 12);  /* clear out header */
#endif

    if (debug_flag & DEBUG_COUNT)
      if (!(record_count % 1000))
        fprintf (stdout, "%s/%d   rec cnt %d\n", __FILE__, __LINE__,
                 record_count);
    r_buf = MDB_read (mdb);
  }
  if (debug_flag & DEBUG_COUNT)
    fprintf (stdout, "%s/%d   Rec Cnt %d\n", __FILE__, __LINE__,
             record_count);
  return sort_element_head;
}

  /*
   * 
   */

  /**********************************************************************
   *	Strip CR or LF							*
   **********************************************************************/
int read_raw_strip (char *buffer)
{
  char *temp;

  temp = strchr (buffer, '\n');
  if (temp)
    *temp = 0;
  temp = strchr (buffer, '\r');
  if (temp)
    *temp = 0;
}

  /*
   * 
   */

  /**********************************************************************
   *	Process list of files						*
   *		list_file = NULL -> read database			*
   *		list_file contains list of files to process		*
   **********************************************************************/
struct SORT_ELEMENT *read_raw (char *input_list_file, char *start_time,
                               char *stop_time)
{
  FILE *list = NULL;
  FILE *input = NULL;
  char *list_file = NULL;
  char *fp;
  char *u_filename;
  char *r_filename;
  char *db_start_time;
  char *db_stop_time;
  char buffer[512];
  int source_flag = 0;
  int status;
  unsigned int sclk;
  int record_count = 0;

  if (input_list_file) {
    if (strcmp (input_list_file, "-")) {
      source_flag = SOURCE_USER;
      list_file = input_list_file;
      list = fopen (list_file, "r");
    } else {                            /* user said "-", so stdin is the one we want */

      source_flag = SOURCE_STDIN;
      list_file = stdin_;
      list = NULL;
    }
  } else {
    source_flag = SOURCE_DATABASE;
    list_file = database;
    list = fopen (list_file, "r");
  }

  if (list_file) {
    fprintf (stdout, "list file \"%s\" %s\n", list_file,
             source_[source_flag]);
    fflush (stdout);
  } else {
    fprintf (stdout, "list file %s %s  NOT FOUND\n", list_file,
             source_[source_flag]);
    fflush (stdout);
    return NULL;
  }

  if (list) {
    while (1) {
      fp = fgets (buffer, 256, list);
      read_raw_strip (buffer);
      if (!fp)
        break;
      u_filename =
        extract_filename (buffer, &sclk, &db_start_time, &db_stop_time,
                          source_flag);
      r_filename = rebuild_filename (u_filename);
      if (source_flag == SOURCE_DATABASE) {
        r_filename = discard_prelaunch (r_filename, &sclk);
        r_filename =
          discard_before_start (r_filename, db_stop_time, start_time,
                                source_flag);
        r_filename =
          discard_after_stop (r_filename, db_start_time, stop_time,
                              source_flag);
      }

      if (r_filename) {
        if (r_filename[0]) {            /* WTF ? */
          input = fopen (r_filename, "rb");
          if (input) {
            fprintf (stderr, "data file %s %s\n", r_filename,
                     strrchr (u_filename, '/') + 1);
            fflush (stderr);
            status = main_file (input);
            record_count += status;
            fclose (input);
            u_filename = NULL;
            r_filename = NULL;
            input = NULL;
          } else {
            fprintf (stderr, "data file ");
            fprintf (stderr, "R:%s ", r_filename);
            fflush (stderr);
            if (u_filename)
              fprintf (stderr, "U:%s ", u_filename);
            fprintf (stderr, "NOT FOUND\n");
            fflush (stderr);
          }
        }
      }
    }
  } else {
    status = main_file (stdin);
    record_count += status;
  }
  if (debug_flag & DEBUG_COUNT)
    fprintf (stdout, "%s/%d   Rec Cnt %d\n", __FILE__, __LINE__,
             record_count);
  return sort_element_head;
}

 /* This function is suspect, it might not be able to handle database 
  * relocation -cwp 2012-03-22
  *
  *     Figure out pathnames, they take th form:
  * st-scet sp-scet st-sclk sp-sclk /opt /project /cassini /data /cxxflight_n flags
  * we want this part               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  *
  * when type is NULL it is a kronos file, located in the prime
  *  directoy, so strip off starting at underscore to get
  *                     /opt /project /cassini /data /cxxflight
  *   
  */
char *read_raw_find_path (char *line, char *type)
{
  static char result[256];
  char buffer[256];
  char *temp[2] = { NULL, NULL };
  FILE *list;

  list = fopen (database, "r");
  if (!list)
    return NULL;
  while (fgets (buffer, 255, list)) {
    if (strstr (buffer, line)) {
      temp[0] = strchr (buffer, '/');
      if (temp[0])
        strcpy (result, temp[0]);
      temp[0] = strchr (result, '/');
      while (temp[0]) {
        temp[1] = temp[0];
        temp[0] = strchr (temp[0] + 1, '/');
      }
      if (temp[1])
        if (temp[1][0])
          temp[1][0] = 0;
    }
  }
  fclose (list);
  if (!type) {
    temp[0] = strchr (result, '_');
    while (temp[0]) {
      temp[1] = temp[0];
      temp[0] = strchr (temp[0] + 1, '_');
    }
    if (temp[1])
      if (temp[1][0])
        temp[1][0] = 0;
  }
  return result;
}
