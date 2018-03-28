#include <SpiceUsr.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <rtiu.h>
#include <util.h>
#include <utilt.h>

#define __mdb__
#include <mdb.h>
#include <mdb_time.h>

#include "mdb_scan.h"

/*****************************************************************************/


/* No compiled in default, uses the environment var RPWS_DATA to set the
   the default */

/* static const char *MDB_database_pathname =
  { "/opt /project /cassini /data /database/" };
  
static const char *MDB_database_filename = { "CassiniJPL.db" };
*/

char *MDB_Version = { MDB_VERSION };
int MDB_debug = 0;
static int mdb_stat_status = 0;

 /*******************************************************
  *	Version Information				*
  *							*
  *	V2.9						*
  *		MDB_rewind				*
  *		fix MDB_read_stream so it only does 	*
  *		    start-stop				*
  *	V2.8						*
  *		MDB_debug & friends			*
  *	V2.7						*
  *		clean up some MALLOC issues		*
  *		(weren't releasing everything)		*
  *	V2.6						*
  *		check FILE prior to open		*
  *		call.  Close it if it's there		*
  *	V2.5						*
  *		MDB_stat bugfix				*
  *	V2.3						*
  *		Add MDB_detail				*
  *	V2.0-V2.2					*
  *		Add SCLK/FINE				*
  *		(Took several tries to get it right	*
  *		  for both u-FIles and R-Files)		*
  *	V1.6						*
  *		MDB_time_SCET				*
  *	V1.5						*
  *		MDB_stat				*
  *	V1.2						*
  *		MDB_read_stream				*
  *							*
  *	V1.1a documentation update			*
  *							*
  *	V1.1 is the first release version		*
  *		MDB_open				*
  *		MDB_close				*
  *		MDB_read				*
  *		MDB_read_all				*
  *							*
  *******************************************************/

 /**/

  /**************************************************************
   *								*
   *	start time string					*
   *								*
   *		"2000-100T10:20:30.000"		SCET		*
   *		"5430F1A7"			SCLK hex	*
   *		"1/1234567890.123"		SCLK spice	*
   *								*
   **************************************************************/
   /**/

/**/

  /**********************************************************************
   *	Read next record						*
   *		skip bad records in the minipacket files		*
   *		skip after output accumulation is exceeded		*
   **********************************************************************/
static int util_getbuffer_RPWS (struct RPWS_buffer *buffer, FILE * input,
                                int eof_flag, int type_flag)
{
  int ilen = 0;
  int bad_record = 1;
  int i;
  int mp_len;
  int local_eof_flag = eof_flag;

  if (!input)
    return 0;

  while (bad_record) {
    bad_record = 0;
    ilen = UTIL_getbuffer_RPWS (buffer, input, local_eof_flag);
    if (ilen > 0) {                     /* data to read ??? */
      if (type_flag == MDB_R_FILE)
        mp_len = UTIL_extract_CDS_length ((struct CDS_buffer *) buffer);
      else
        mp_len = UTIL_MP_length ((struct MP_buffer *) buffer);
      if (mp_len == 0)                  /* funny record encountered ? */
        bad_record = 1;                 /*   then dump it 1 */
      else {                            /* mp_len > 0 */

        /*
         * fprintf(debug_file, "Record %8s %02X\n",
         * UTIL_extract_MP_packet_type((struct MP_buffer*)buffer),
         * UTIL_extract_MP_bits((struct MP_buffer *)buffer) ); /*
         */
        if (type_flag == MDB_R_FILE) {
          bad_record = 0;               /* accept this record */
          break;
        } else
          switch (UTIL_extract_MP_bits ((struct MP_buffer *) buffer)) {
           case PACKET_TYPE_stim:
           case PACKET_TYPE_lp:
           case PACKET_TYPE_hfr:
           case PACKET_TYPE_wbr:
           case PACKET_TYPE_wfr:
           case PACKET_TYPE_mfr:
           case PACKET_TYPE_lfdr:
           case PACKET_TYPE_dust:
             bad_record = 0;            /* accept this record */
             break;
           default:
             bad_record = 1;            /* not interested */
             break;                     /* in this record */
          }
      }                                 /* mp_len > 0 */
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

/**/

  /**************************************
   *	Handle non-U_file file naming	*
   **************************************/

static int MDB_rename (struct MDB *mdb)
{
  if (mdb->database_line_tokens[4])
    strcpy (mdb->datafile_name, mdb->database_line_tokens[4]);
  else
    return 0;
  switch (mdb->mdb_packet_type) {
   case MDB_U_FILE:
     break;
   case MDB_R_FILE:
     *strchr (mdb->datafile_name, '.') = 0;
     strcat (mdb->datafile_name, ".r00");
     break;
   case MDB_S_FILE:
     *strchr (mdb->datafile_name, '.') = 0;
     strcat (mdb->datafile_name, ".s00");
     break;
   case MDB_H_FILE:
     *strchr (mdb->datafile_name, '.') = 0;
     strcat (mdb->datafile_name, ".h00");
     break;
   case MDB_L_FILE:
     *strchr (mdb->datafile_name, '.') = 0;
     strcat (mdb->datafile_name, ".l00");
     break;
  }
  return 1;
}

/**/

  /**********************************************
   *	Compare record SCLK with start/stop	*
   *	  times.				*
   *	Return					*
   *		1	Record in time range	*
   *		0	Record out of range	*
   **********************************************/

static int MDB_compare (struct MDB *mdb)
{
  int sclk;

  if (mdb->current_sclk < mdb->requested_start_sclk)
    return 0;

     /******************************
      * NOTE: there might not be a *
      *   stop time specified!     *
      * account for sclk.fine > 0  *
      * by adding 1 to stop time   *
      ******************************/
  if (mdb->requested_stop_sclk)
    if ((mdb->current_sclk) > mdb->requested_stop_sclk + 1)
      return 0;
  return 1;
}

/**/

  /******************************************************
   *	Compare record type 				*
   *	Return						*
   *		1	Record is of specified type	*
   *		0	Record doesn't match		*
   ******************************************************/
static int MDB_stream (struct MDB *mdb, int stream)
{
  int type;

  if (mdb->mdb_packet_type == MDB_R_FILE)
    type = UTIL_extract_CDS_type ((struct CDS_buffer *) mdb->buffer);
  else
    type = UTIL_extract_MP_type ((struct MP_buffer *) mdb->buffer);
  return (type == stream);
}

/**/

  /**********************************************
   *	OPEN					*
   *	Pukes if can't allocate memory		*
   *	Returns address of allocated MDB	*
   *	    structure				*
   **********************************************/

struct MDB *MDB_open (const char *start_time, /*       desired stat time */
                      const char *stop_time,  /* (opt) desired stop time */
                      const char *path,       /* (opt) path to database index file */
                      const char *name,       /* (opt) name of database index file */
                      int type)
{                                       /*       type of file to read */
  struct MDB *mdb_element;
  int clk;
  int i;

     /****************************************
      *	Allocate memory for data structure,  *
      *   filename buffers, and data buffer  *
      ****************************************/
  mdb_element = malloc (sizeof (struct MDB));
  if (!mdb_element) {
    fprintf (stderr, "%s/%d malloc(struct MDB) failed\n", __FILE__, __LINE__);
    return NULL;                        /* exit(0); */
  }
  mdb_element->database_detail_line = malloc (MDB_LINE_BUFFER + 1);
  if (!mdb_element->database_detail_line) {
    fprintf (stderr, "%s/%d malloc(database_detail_line) failed\n", __FILE__,
             __LINE__);
    return NULL;                        /* exit(0); */
  }
  mdb_element->database_line = malloc (MDB_LINE_BUFFER + 1);
  if (!mdb_element->database_line) {
    fprintf (stderr, "%s/%d malloc(database_line) failed\n", __FILE__,
             __LINE__);
    return NULL;                        /* exit(0); */
  }
  mdb_element->database_name = malloc (MDB_NAME_BUFFER + 1);
  if (!mdb_element->database_name) {
    fprintf (stderr, "%s/%d malloc(database_name) failed\n", __FILE__,
             __LINE__);
    return NULL;                        /* exit(0); */
  }
  mdb_element->datafile_name = malloc (MDB_NAME_BUFFER + 1);
  if (!mdb_element->datafile_name) {
    fprintf (stderr, "%s/%d malloc(datafile_name) failed\n", __FILE__,
             __LINE__);
    return NULL;                        /* exit(0); */
  }
  mdb_element->buffer = malloc (MDB_DATA_BUFFER + 1);
  if (!mdb_element->buffer) {
    fprintf (stderr, "%s/%d malloc(record buffer) failed\n", __FILE__,
             __LINE__);
    return NULL;                        /* exit(0); */
  }

     /*******************************
      *	Initialize data structure,  *
      *  loading start & stop times *
      *******************************/
  mdb_element->link = NULL;             /* reserved for user */

  mdb_element->requested_start_sclk = MDB_time (start_time);

  mdb_element->requested_stop_sclk = 0;
  mdb_element->mdb_packet_type = type;
  gettimeofday (&mdb_element->open_time, NULL);
  mdb_element->read_time.tv_sec = 0;
  mdb_element->read_time.tv_usec = 0;
  mdb_stat_status = 1;

  if (stop_time)
    mdb_element->requested_stop_sclk = MDB_time (stop_time);
  else
    mdb_element->requested_stop_sclk = 0x7FFFFFFE;

  mdb_element->database_start_sclk = 0;
  
  if(!path && !name){
		if(! getenv("RPWS_MPDB"))
		{
			fprintf(stderr, "%s/%d No database specified\n" __FILE__,__LINE__);
			return NULL;
		}
		strcpy(mdb_element->database_name, getenv("RPWS_MPDB"));
  }
  else{
  
	  if (path) {
	    strcpy (mdb_element->database_name, path);
	    strcat (mdb_element->database_name, "/");
	  } else {
		 fprintf(stderr, "%s/%d Database path is NULL, but name is not\n",
				    __FILE__,__LINE__);
       return NULL;
	  }
	
	  if (name){
	    strcat (mdb_element->database_name, name);
	 }
	  else{
		 fprintf(stderr, "%s/%d Database name is NULL, but path is not\n",
				    __FILE__,__LINE__);
       return NULL;
	 }
  }

     /********************************************************
      *	This open if first after data structure is allocated *
      ********************************************************/
  
  /* To allow for testing in an empty area, create a blank database
     if none exist -cwp */
	if( access( mdb_element->database_name, F_OK) != 0 ){
		
		/* TODO: make the directories leading to this database name, if needed.
		 * There's code to handle this in: 
		 *
		 *    util/C/trunk/librpwgse/rpwgse/GseFile.c
		 */
		 
		FILE* fTmp = fopen(mdb_element->database_name, "ab");
		if(fTmp == NULL){
			fprintf(stderr, "%s/%d failed to initialize a blank minipacket "
			        "index file at %s", __FILE__, __LINE__, 
			        mdb_element->database_name);
			return NULL;
		}
		fflush(fTmp);
		fclose(fTmp);
	}
  
  mdb_element->database_file = fopen (mdb_element->database_name, "r");
  mdb_element->database_line_count = 0;
  if (!mdb_element->database_file) {
    fprintf (stderr, "%s/%d fopen(%s) failed\n", __FILE__, __LINE__,
             mdb_element->database_name);
    return NULL;                        /* exit(0); */
  }
  if (MDB_debug & MDB_DEBUG_DATABASE)
    fprintf (stderr, "MDB %4d database %p=fopen(%s)\n",
             __LINE__,
             mdb_element->database_file, mdb_element->database_name);

  for (i = 0; i < MDB_TOKENS; i++)
    mdb_element->database_line_tokens[i] = NULL;
  mdb_element->datafile_file = NULL;
  mdb_element->database_start_sclk = 0;
  mdb_element->database_stop_sclk = 0;

     /**********************************************************************
      *									   *
      *		Database index is now open and ready for action		   *
      *									   *
      *	We can now scan in database index to find the 1st. file that	   *
      *   will contain data for the requested period			   *
      **********************************************************************/
  while (MDB_scan (mdb_element)) {      /* get next file to read           *//*     drop out when EOF           */
    if (mdb_element->datafile_file) {   /*     close the file prior 2.7    */
      if (MDB_debug & MDB_DEBUG_DATAFILE)
        fprintf (stderr, "MDB %4d datafile fclose(%p)\n",
                 __LINE__, mdb_element->datafile_file);
      fclose (mdb_element->datafile_file);      /* to open...              */
    }
    MDB_rename (mdb_element);           /* reform name, get file           */
    stat (mdb_element->datafile_name,   /* status BUT check to     */
          &mdb_element->status_buffer); /* see if we need to       */
    mdb_element->datafile_file = fopen (mdb_element->datafile_name, "r");
    if (MDB_debug & MDB_DEBUG_DATAFILE)
      fprintf (stderr, "MDB %4d datafile %p=fopen(%s)\n", __LINE__,
               mdb_element->datafile_file, mdb_element->datafile_name);
    if (mdb_element->datafile_file)     /* if if worked, we're done        */
      break;                            /*     else scan more              */
  }                                     /* Once we drop out mdb_element    */
  return mdb_element;                   /*     will have an open data file */
}                                       /*     (unless no more data)       */

                                        /***********************************/

/**/

  /**********************************************
   *	REWIND					*
   *	Rewind everything MDB structure		*
   **********************************************/

struct MDB *MDB_rewind (struct MDB *mdb, int type)
{
  int i;

  if (!mdb)                             /* sent us an invalid structure */
    return NULL;
  /*
   * Close open files...
   */
  if (mdb->datafile_file) {             /* data */
    if (MDB_debug & MDB_DEBUG_DATAFILE)
      fprintf (stderr, "MDB %4d datafile fclose(%p)\n",
               __LINE__, mdb->datafile_file);
    fclose (mdb->datafile_file);
  }
  mdb->datafile_file = NULL;
  if (mdb->database_file) {             /* List of files */
    if (MDB_debug & MDB_DEBUG_DATABASE)
      fprintf (stderr, "MDB %4d database fclose(%p)\n",
               __LINE__, mdb->database_file);
    fclose (mdb->database_file);
    mdb->database_line_count = 0;
  }
  mdb->database_file = NULL;
  mdb->database_line_count = 0;

  /*
   * Open databse file...
   */
  mdb->database_file = fopen (mdb->database_name, "r");
  mdb->database_line_count = 0;
  if (!mdb->database_file) {
    fprintf (stderr, "%s/%d fopen(%s) failed\n", __FILE__, __LINE__,
             mdb->database_name);
    return NULL;                        /* exit(0); */
  }
  if (MDB_debug & MDB_DEBUG_DATABASE)
    fprintf (stderr, "MDB %4d database %p=fopen(%s)\n",
             __LINE__, mdb->database_file, mdb->database_name);

  /*
   * Prepare for datafile read...
   */
  for (i = 0; i < MDB_TOKENS; i++)
    mdb->database_line_tokens[i] = NULL;
  mdb->datafile_file = NULL;
  mdb->mdb_packet_type = type;
  mdb->database_start_sclk = 0;
  mdb->database_stop_sclk = 0;
  mdb->current_sclk = 0;
  mdb->current_fine = 0;
  mdb->current_part = 1;
  mdb->current_RTI = 0;
  gettimeofday (&mdb->open_time, NULL);

     /**********************************************************************
      *									   *
      *		Database index is now open and ready for action		   *
      *									   *
      *	We can now scan in database index to find the 1st. file that	   *
      *   will contain data for the requested period			   *
      **********************************************************************/
  while (MDB_scan (mdb)) {              /* get next file to read           *//*     drop out when EOF           */
    if (mdb->datafile_file) {           /*     close the file prior 2.7    */
      if (MDB_debug & MDB_DEBUG_DATAFILE)
        fprintf (stderr, "MDB %4d datafile fclose(%p)\n",
                 __LINE__, mdb->datafile_file);
      fclose (mdb->datafile_file);      /* to open...              */
    }
    MDB_rename (mdb);                   /* reform name, get file           */
    stat (mdb->datafile_name,           /* status BUT check to     */
          &mdb->status_buffer);         /* see if we need to       */
    mdb->datafile_file = fopen (mdb->datafile_name, "r");
    if (MDB_debug & MDB_DEBUG_DATAFILE)
      fprintf (stderr, "MDB %4d datafile %p=fopen(%s)\n", __LINE__,
               mdb->datafile_file, mdb->datafile_name);
    if (mdb->datafile_file)             /* if if worked, we're done        */
      break;                            /*     else scan more              */
  }                                     /* Once we drop out mdb    */
  return mdb;                           /*     will have an open data file */

}

/**/

  /**********************************************
   *	CLOSE					*
   *	Release MDB structure			*
   *      and associated buffers		*
   **********************************************/

int MDB_close (struct MDB *mdb)
{
  if (!mdb)                             /* sent us an invalid structure */
    return 0;
  if (mdb->datafile_file) {             /* data */
    if (MDB_debug & MDB_DEBUG_DATAFILE)
      fprintf (stderr, "MDB %4d datafile fclose(%p)\n",
               __LINE__, mdb->datafile_file);
    fclose (mdb->datafile_file);
  }
  mdb->datafile_file = NULL;
  if (mdb->database_file) {             /* List of files */
    if (MDB_debug & MDB_DEBUG_DATABASE)
      fprintf (stderr, "MDB %4d database fclose(%p)\n",
               __LINE__, mdb->database_file);
    fclose (mdb->database_file);
    mdb->database_line_count = 0;
  }
  mdb->database_file = NULL;
  mdb->database_line_count = 0;

  if (mdb->buffer)                      /* 2.7 */
    free (mdb->buffer);
  if (mdb->datafile_name)               /* 2.7 */
    free (mdb->datafile_name);
  if (mdb->database_name)               /* 2.7 */
    free (mdb->database_name);
  if (mdb->database_line)               /* 2.7 */
    free (mdb->database_line);
  if (mdb->database_detail_line)        /* 2.7 */
    free (mdb->database_detail_line);
  free (mdb);

  mdb_stat_status = 0;
  return 0;
}

/**/

  /******************************************************
   *	File Statistice:				*
   *	      Return the "struct stat" buffer for	*
   *		each file in the included time period	*
   *	      Also, send back the filename...		*
   *	      Return 0 when done			*
   *							*
   *	We leave the file open with this call, so	*
   *		it is reasonable to switch to read	*
   *		at any point				*
   ******************************************************/

int MDB_stat (struct MDB *mdb, struct stat *stat_buf, char *filename)
{

  if (!mdb_stat_status)
    return 0;
  filename[0] = 0;
  if (strrchr (mdb->datafile_name, '/'))
    strcpy (filename, strrchr (mdb->datafile_name, '/') + 1);
  memcpy (stat_buf, &mdb->status_buffer, sizeof (struct stat));

  mdb_stat_status = MDB_scan (mdb);
  if (mdb_stat_status) {
    MDB_rename (mdb);
    stat (mdb->datafile_name, &mdb->status_buffer);
    if (mdb->datafile_file) {           /* 2.6 */
      if (MDB_debug & MDB_DEBUG_DATAFILE)
        fprintf (stderr, "MDB %4d datafile fclose(%p)\n",
                 __LINE__, mdb->datafile_file);
      fclose (mdb->datafile_file);      /* 2.6 */
    }
    mdb->datafile_file = fopen (mdb->datafile_name, "r");
    if (MDB_debug & MDB_DEBUG_DATAFILE)
      fprintf (stderr, "MDB %4d datafile %p=fopen(%s)\n",
               __LINE__, mdb->datafile_file, mdb->datafile_name);
  }
  return mdb_stat_status;
}

/* ************************************************************************* */
/* Database Detail Lines:                                                    */
/*   Return the database line buffer for each file in the included time      */
/*   period. Return 0 when done                                              */
/*                                                                           */
/*  We allocate space for this ONCE! (user doesn't have to keep track of it) */

struct MDB_DETAIL *MDB_detail (struct MDB *mdb)
{
  static struct MDB_DETAIL *mdb_detail = NULL;
  int i;

  if (!mdb_detail) {
    mdb_detail = malloc (sizeof (struct MDB_DETAIL));
    if (!mdb_detail) {
      fprintf (stderr, "%s/%d malloc(struct MDB_DETAIL) failed\n", __FILE__,
               __LINE__);
      return NULL;                      /* exit(0); */
    }
    mdb_detail->pathname = malloc (MDB_LINE_BUFFER + 1);
    if (!mdb_detail->pathname) {
      fprintf (stderr, "%s/%d malloc(mdb_detail->pathname) failed\n",
               __FILE__, __LINE__);
      return NULL;                      /* exit(0); */
    }
    mdb_detail->filename = malloc (MDB_LINE_BUFFER + 1);
    if (!mdb_detail->filename) {
      fprintf (stderr, "%s/%d malloc(mdb_detail->filename) failed\n",
               __FILE__, __LINE__);
      return NULL;                      /* exit(0); */
    }
    mdb_detail->detail_line = malloc (MDB_LINE_BUFFER + 1);
    if (!mdb_detail->detail_line) {
      fprintf (stderr, "%s/%d malloc(mdb_detail->detail_line) failed\n",
               __FILE__, __LINE__);
      return NULL;                      /* exit(0); */
    }
  }
  if (!mdb_stat_status)
    return NULL;
  if (!strlen (mdb->database_detail_line))
    return NULL;

  strcpy (mdb_detail->detail_line, (char *) mdb->database_detail_line);
  strcpy (mdb_detail->pathname, mdb->database_line_tokens[4]);
  *(strrchr(mdb_detail->pathname, '/')) = '\0';
  if (strrchr (mdb->datafile_name, '/'))
    strcpy (mdb_detail->filename, strrchr (mdb->datafile_name, '/') + 1);
  mdb_detail->start_sclk = strtol (mdb->database_line_tokens[2], NULL, 16);
  mdb_detail->stop_sclk = strtol (mdb->database_line_tokens[3], NULL, 16);
  mdb_detail->flags = strtol (mdb->database_line_tokens[5] + 1, NULL, 16);
  switch (mdb->database_line_tokens[5][0]) {
   case 'F':
   case 'E':
   case 'D':
   case 'C':
   case 'B':
   case 'A':
     mdb_detail->flags |= 0x80000000;
     mdb_detail->flags |= ((mdb->database_line_tokens[5][0] & 7) + 1) << 28;
     break;
   case '9':
   case '8':
     mdb_detail->flags |= 0x80000000;
   case '7':
   case '6':
   case '5':
   case '4':
   case '3':
   case '2':
   case '1':
   case '0':
     mdb_detail->flags |= (mdb->database_line_tokens[5][0] & 7) << 28;
     break;
  }
  utc2et_c(mdb->database_line_tokens[0], &mdb_detail->start_et);
  utc2et_c(mdb->database_line_tokens[1], &mdb_detail->stop_et);

  mdb_stat_status = MDB_scan (mdb);
  return mdb_detail;
}

  /******************************************************
   *	Read record.  This routine returns		*
   *		ALL records from files that		*
   *		are within the start/stop range		*
   *		typically, this results in up to	*
   *		an hours worth of data before and	*
   *		after the selected period.  For		*
   *		real-time data, the files are 6		*
   *		6 hours long				*
   ******************************************************/

struct RPWS_buffer *MDB_read_all (struct MDB *mdb)
{
  int status;
  static int eof_flag = UTIL_GET_NON_BLOCKING;

  if (!mdb->datafile_file)              /* something BAD happened */
    return NULL;                        /*   so tell user we're hosed */

  status =
    util_getbuffer_RPWS ((struct RPWS_buffer *) mdb->buffer,
                         mdb->datafile_file, eof_flag, mdb->mdb_packet_type);
  while (status <= 0) {                 /* empty file ??? */
    if (!MDB_scan (mdb))                /* get next file to read */
      break;                            /* no more files in DB */
    if (mdb->database_line_tokens[0]) {
      if (mdb->database_line_tokens[0][0]) {
        if (mdb->datafile_file) {       /* 2.6 */
          if (MDB_debug & MDB_DEBUG_DATAFILE)
            fprintf (stderr, "MDB %4d datafile fclose(%p)\n",
                     __LINE__, mdb->datafile_file);
          fclose (mdb->datafile_file);  /* 2.6 */
        }

        MDB_rename (mdb);               /* change file type (as needed) */
        stat (mdb->datafile_name,       /* 2.8 (moved) */
              &mdb->status_buffer);     /* 2.8 (moved) */

        mdb->datafile_file = fopen (mdb->datafile_name, "r");
        if (MDB_debug & MDB_DEBUG_DATAFILE)
          fprintf (stderr, "MDB %4d datafile %p=fopen(%s)\n",
                   __LINE__, mdb->datafile_file, mdb->datafile_name);
        if (mdb->datafile_file)         /* open OK ??? */
          status =
            util_getbuffer_RPWS ((struct RPWS_buffer *) mdb->buffer,
                                 mdb->datafile_file, eof_flag,
                                 mdb->mdb_packet_type);
        else                            /* status = 1 if read worked */
          status = -1;                  /* simulate EOF if it didn't */
      }
    }
  }
  if (!mdb->read_time.tv_sec)
    gettimeofday (&mdb->read_time, NULL);
  if (status > 0) {
    switch (mdb->mdb_packet_type) {
     case MDB_R_FILE:
       mdb->current_sclk =
         UTIL_extract_TIME ((struct CDS_buffer *) mdb->buffer);
       mdb->current_fine =
         UTIL_extract_CDS_RTI ((struct CDS_buffer *) mdb->buffer) & 0xE0;
       mdb->current_RTI = mdb->current_fine >> 5;
       mdb->current_part = 1;
       break;
     default:
       mdb->current_sclk =
         UTIL_event_time ((struct MP_buffer *) mdb->buffer, 0);
       mdb->current_RTI =
         UTIL_extract_MP_RTI ((struct MP_buffer *) mdb->buffer);
       mdb->current_fine = mdb->current_RTI << 5;
       mdb->current_part = 1;
       break;
    }
    return (struct RPWS_buffer *) mdb->buffer;
  } else {
    mdb->current_sclk = 0;
    mdb->current_RTI = 0;
    mdb->current_fine = 0;
    mdb->current_part = 0;
    return NULL;
  }
}

/**/

  /******************************************************
   *	Read record.  This routine returns		*
   *		ONLY records that are within the 	*
   *		specified start/stop range.		*
   *		We do return data that is 1 second	*
   *		following the selected period so that	*
   *		we can do time comparisons with		*
   *		SCLK (ignoring fine)			*
   ******************************************************/

struct RPWS_buffer *MDB_read (struct MDB *mdb)
{
  struct RPWS_buffer *rpws_temp;

  rpws_temp = MDB_read_all (mdb);
  while (!MDB_compare (mdb) && rpws_temp) {
    rpws_temp = MDB_read_all (mdb);
  }
  return rpws_temp;
}

/**/

  /******************************************************
   *	Read record from specified stream (i.e. MFR HFR *
   *       etc.).  This routine returns			*
   *		ONLY records that are within the 	*
   *		specified start/stop range.		*
   *		We do return data that is 1 second	*
   *		following the selected period so that	*
   *		we can do time comparisons with		*
   *		SCLK (ignoring fine)			*
   ******************************************************/

struct RPWS_buffer *MDB_read_stream (struct MDB *mdb, int stream)
{
  struct RPWS_buffer *rpws_temp;

  rpws_temp = MDB_read (mdb);
  while (1) {
    if (!rpws_temp)                     /* EOF (no more data)    */
      break;                            /*   return NULL to user */
    if (MDB_stream (mdb, stream) && MDB_compare (mdb))
      break;                            /* interesting data, let user see it */
    rpws_temp = MDB_read (mdb);
  }
  return rpws_temp;
}
