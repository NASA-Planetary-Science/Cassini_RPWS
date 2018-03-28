#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "rtiu.h"
#include "util.h"
#define __adb__
#include "mdb.h"
#include "adb.h"

char *ADB_Version = { ADB_VERSION };
int ADB_debug = 0;
static int adb_stat_status = 0;

/**/

  /**********************************************
   *						*
   *	As we start, build all the file 	*
   *	name stuff				*
   *						*
   **********************************************/
static int ADB_filename_setup (struct ADB *adb)
{
  char temp[64];
  char current_time[64];

#ifdef DBG
  printf ("%4d ADB_filename_setup %X\n", __LINE__, adb->current_sclk);
#endif

  sprintf (current_time, "1/%d.000", adb->current_sclk);
  strcpy (temp, MDB_time_SCET (current_time));

#ifdef DBG
  printf ("%4d ADB_filename_setup %s\n", __LINE__, temp);
#endif

  temp[4] = 0;
  adb->data_year = atol (&temp[0]);
  temp[8] = 0;
  adb->data_day = atol (&temp[5]);

  switch (adb->adb_file_type) {
   case ADB_DATA_STIM:
     sprintf (adb->adb_file_key, "^STIM_TABLE");
     sprintf (adb->datalabel_name,
              "%s/"
              "T%04d%01dXX/"
              "T%04d%01dXX_STIM.LBL",
              adb->datafile_path,
              adb->data_year, adb->data_day / 100,
              adb->data_year, adb->data_day / 100);
     break;
   case ADB_HSK_BFDL:
   case ADB_HSK_DUST:
   case ADB_HSK_HOUSE:
   case ADB_HSK_IPC:
   case ADB_HSK_ENG:
     sprintf (adb->adb_file_key, "^ENG_RECORD");
     sprintf (adb->datalabel_name,
              "%s/"
              "T%04d%01dXX/"
              "T%04d%03d_ENG.LBL",
              adb->datafile_path,
              adb->data_year, adb->data_day / 100,
              adb->data_year, adb->data_day);
     break;
  }

#ifdef DBG
  printf ("%4d ADB_filename_setup %s\n", __LINE__, adb->datalabel_name);
#endif

  return 1;
}

  /**********************************************
   *						*
   *						*
   **********************************************/
static int ADB_day_open (struct ADB *adb)
{
  char buf[129];
  char *temp[2];

  adb->datalabel_file = fopen (adb->datalabel_name, "r");
  if (adb->datalabel_file) {
    while (fgets (buf, 128, adb->datalabel_file)) {
      if (!strncmp (buf, "FILE_RECORDS", 12)) {

#ifdef DBG
        printf ("%4d ADB_day_open %s\n", __LINE__, buf);
#endif

        temp[0] = strchr (buf, '=');
        temp[0]++;
        adb->adb_file_records = atol (temp[0]);
      }
      if (!strncmp (buf, adb->adb_file_key, strlen (adb->adb_file_key))) {
        temp[0] = strchr (buf, '"');
        temp[0]++;
        temp[1] = strchr (temp[0], '"');
        temp[1][0] = 0;;
        sprintf (adb->datafile_name,
                 "%s/"
                 "T%04d%01dXX/"
                 "%s",
                 adb->datafile_path,
                 adb->data_year, adb->data_day / 100, temp[0]);

#ifdef DBG
        printf ("%4d ADB_day_open %s\n", __LINE__, adb->datafile_name);
#endif
      }
    }
    close (adb->datalabel_file);
    if (adb->datafile_file)
      fclose (adb->datafile_file);
    adb->datafile_file = fopen (adb->datafile_name, "r");

#ifdef DBG
    printf ("%4d ADB_day_open %s\n", __LINE__, adb->datafile_name);
#endif

    return 1;
  } else {

#ifdef DBG
    printf ("%4d ADB_day_open %s\n", __LINE__, adb->datalabel_name);
#endif

    return 0;
  }
}

  /**********************************************
   *						*
   *						*
   **********************************************/
static int ADB_next_day (struct ADB *adb)
{
  char temp[64];

#ifdef DBG
  printf ("%4d ADB_next_day %d %d %d\n", __LINE__,
          adb->data_year, adb->data_day,
          adb->adb_file_type & ABD_100_DAY ? 100 : 1);
#endif

  if (adb->adb_file_type & ABD_100_DAY) {
    adb->data_day = (adb->data_day / 100) * 100 + 100;
  } else {
    adb->data_day += 1;
  }
  if (adb->data_day > 366) {
    adb->data_day = 1;
    adb->data_year++;
  }

  sprintf (temp, "%04d-%03dT00:00:00", adb->data_year, adb->data_day);

#ifdef DBG
  printf ("%4d ADB_next_day %X %s\n", __LINE__, adb->current_sclk, temp);
#endif

  adb->current_sclk = MDB_time (temp) + 1;

#ifdef DBG
  printf ("%4d ADB_next_day %X\n", __LINE__, adb->current_sclk);
  printf ("%4d ADB_next_day %s start:%X new:%X stop:%X\n", __LINE__,
          temp,
          adb->requested_start_sclk,
          adb->current_sclk, adb->requested_stop_sclk);
#endif

  if (adb->current_sclk > adb->requested_stop_sclk) {

#ifdef DBG
    printf ("%4d ADB_next_day %s DONE DONE DONE\n", __LINE__, temp);
#endif

    return 0;
  }

#ifdef DBG
  printf ("%4d ADB_next_day %X\n", __LINE__, adb->current_sclk);
#endif

  return adb->current_sclk;
}

static int ADB_scan (struct ADB *adb, int flag)
{
  int status;

#ifdef DBG
  printf ("%4d ADB_scan flag=%d\n", __LINE__, flag);
#endif

  if (flag)
    if (!ADB_next_day (adb))
      return 0;
  ADB_filename_setup (adb);             /* build filename */
  while (!(status = ADB_day_open (adb))) {      /* open that day */
    status = ADB_next_day (adb);
    if (!status)
      break;
  }

#ifdef DBG
  printf ("%4d ADB_scan %d=status\n", __LINE__, status);
#endif

  return status;
}

/**/

  /**********************************************
   *	OPEN					*
   *	Pukes if can't allocate memory		*
   *	Returns address of allocated ADB	*
   *	    structure				*
   **********************************************/

struct ADB *ADB_open (char *start_time, /*       desired stat time */
                      char *stop_time,  /* (opt) desired stop time */
                      char *path,       /* (opt) path to archive */
                      int type)
{                                       /*       type of file to read */
  struct ADB *adb_element;
  int clk;
  int i;

     /****************************************
      *	Allocate memory for data structure,  *
      *   filename buffers, and data buffer  *
      ****************************************/
  adb_element = malloc (sizeof (struct ADB));
  if (!adb_element) {
    fprintf (stderr, "%s/%d malloc(struct ADB) failed\n", __FILE__, __LINE__);
    return NULL;                        /* exit(0); */
  }
  adb_element->datafile_path = malloc (ADB_LINE_BUFFER + 1);
  if (!adb_element->datafile_path) {
    fprintf (stderr, "%s/%d malloc(datafile_path) failed\n", __FILE__,
             __LINE__);
    return NULL;                        /* exit(0); */
  }
  adb_element->datafile_name = malloc (ADB_LINE_BUFFER + 1);
  if (!adb_element->datafile_name) {
    fprintf (stderr, "%s/%d malloc(datafile_name) failed\n", __FILE__,
             __LINE__);
    return NULL;                        /* exit(0); */
  }

  adb_element->datalabel_name = malloc (ADB_LINE_BUFFER + 1);
  if (!adb_element->datalabel_name) {
    fprintf (stderr, "%s/%d malloc(datalabel_name) failed\n", __FILE__,
             __LINE__);
    return NULL;                        /* exit(0); */
  }
  adb_element->buffer = malloc (ADB_DATA_BUFFER + 1);
  if (!adb_element->buffer) {
    fprintf (stderr, "%s/%d malloc(buffer) failed\n", __FILE__, __LINE__);
    return NULL;                        /* exit(0); */
  }


     /*******************************
      *	Initialize data structure,  *
      *  loading start & stop times *
      *******************************/
  adb_element->link = NULL;             /* reserved for user */
  adb_element->adb_file_type = type;
  adb_element->requested_start_sclk = MDB_time (start_time);
  adb_element->current_sclk = adb_element->requested_start_sclk;
  if (stop_time)
    adb_element->requested_stop_sclk = MDB_time (stop_time);
  else
    adb_element->requested_stop_sclk = 0x7FFFFFFE;

#ifdef DBG
  printf ("%4d ADB_open %X\n", __LINE__, adb_element->requested_stop_sclk);
#endif

  gettimeofday (&adb_element->open_time, NULL);
  adb_element->read_time.tv_sec = 0;
  adb_element->read_time.tv_usec = 0;
  adb_stat_status = 1;
  
  
	if(path){
		strcpy(adb_element->datafile_path, path);
	}
	else{
		if(type & ADB_DATA){
		
			if(getenv("RPWS_VOLROOT"){
				strcpy(adb_element->datafile_path, getenv("RPWS_VOLROOT"));
			}
			else{
				if(getenv("RPWS_DATA")){
					strcpy(adb_element->datafile_path, getenv("RPWS_DATA"));
					strcat(adb_element->datafile_path, "/CORPWS_0000");
				}
				else{
					fprintf(stderr, "Can't determine the PDS volume root neither "
					        "RPWS_VOLROOT nor RPWS_DATA are defined.");
					exit(13);
				}
			}
		
			strcat(adb_element->datafile_path, "/DATA/ANCILLARY" );
		}
	 
		if(type & ADB_HSK){
			if(getenv("RPWS_DATA")){
				strcat(adb_element->datafile_path, "/housekeeping");
			}
			else{
				fprintf(stderr, "Can't determine the housekeeping directory "
					             "RPWS_DATA are defined.");
				exit(13);
			}
		}
		
		strcpy (adb_element->datafile_path, ADB_house_path);
	}
	
	ADB_scan (adb_element, 0);
	return adb_element;
}

/**/

  /**********************************************
   *	CLOSE					*
   *	Release ADB structure			*
   *      and associated buffers		*
   **********************************************/

int ADB_close (struct ADB *adb)
{
  if (!adb)                             /* sent us an invalid structure */
    return 0;
  if (adb->datafile_file)
    fclose (adb->datafile_file);
  if (adb->datalabel_file)
    fclose (adb->datalabel_file);
  if (adb->datalabel_name)
    free (adb->datalabel_name);
  if (adb->datafile_name)
    free (adb->datafile_name);
  if (adb->datafile_path)
    free (adb->datafile_path);
  if (adb->buffer)
    free (adb->buffer);
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

int ADB_stat (struct ADB *adb, struct stat *stat_buf, char *filename)
{
  return 0;
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

static unsigned char *ADB_read_internal (struct ADB *adb)
{
  static unsigned char null_buff[32] = { 32 * 0 };
  int len;
  int result = 1;

#ifdef DBG

/*     printf("%4d ADB_read_internal\n", __LINE__); /**/
#endif

  if (!adb->datafile_file) {

#ifdef DBG
    printf ("%4d ADB_read_internal\n", __LINE__);
    /**/
#endif

      result = ADB_scan (adb, 1);
  }
  if (result) {
    if (adb->adb_file_type & ADB_STRING) {
      if (fgets ((char *) adb->buffer, ADB_DATA_BUFFER, adb->datafile_file)) {

#ifdef DBG

/*	         printf("%4d ADB_read_internal\n", __LINE__); /**/
#endif

        return adb->buffer;
      } else {

#ifdef DBG
        printf ("%4d ADB_read_internal\n", __LINE__);
#endif

        if (!ADB_scan (adb, 1)) {

#ifdef DBG
          printf ("%4d ADB_read_internal NULL\n", __LINE__);
#endif

          return NULL;
        }

#ifdef DBG
        printf ("%4d ADB_read_internal null_buff\n", __LINE__);
#endif

        return null_buff;
      }
    } else {
      if (fread (adb->buffer, 32, 0, adb->datafile_file)) {
        len = (adb->buffer[12] << 8) & 0xFF00;
        len |= (adb->buffer[13] << 0) & 0x00FF;
        fread (&adb->buffer[32], len - 32, 0, adb->datafile_file);

#ifdef DBG
        printf ("%4d ADB_read_internal\n", __LINE__);
#endif

        return adb->buffer;
      } else {

#ifdef DBG
        printf ("%4d ADB_read_internal\n", __LINE__);
#endif

        if (!ADB_scan (adb, 1)) {

#ifdef DBG
          printf ("%4d ADB_read_internal\n", __LINE__);
#endif

          return NULL;
        }
        return null_buff;
      }
    }
  } else {

#ifdef DBG
    printf ("%4d ADB_read_internal\n", __LINE__);
#endif

    return NULL;
  }
}

unsigned char *ADB_read (struct ADB *adb)
{
  unsigned char *buff;
  char *temp;
  int status = 1;
  int sclk;

#ifdef DBG
  printf ("%4d ADB_read ************************************\n", __LINE__);
#endif

  while (status) {
    buff = ADB_read_internal (adb);

    if (buff) {                         /* EOF check */
      if (adb->adb_file_type & ADB_STRING) {
        if (strlen ((char *) buff)) {
          status = 0;
          buff[strlen ((char *) buff) - 1] = (unsigned char) 0x00;
          buff[strlen ((char *) buff) - 1] = (unsigned char) 0x00;
        }
      } else {
        if (buff[12] || buff[13])       /* length non-zero */
          status = 0;
      }
    } else
      break;

    if (!status) {                      /* time range Check */
      if (buff[0]) {
        if (adb->adb_file_type & ADB_STRING) {
          temp = strchr ((char *) buff, '/');   /* decimal SCLK p/dddd.f */
          temp++;                       /* skip slash */
          sclk = strtol (temp, NULL, 10);
        } else {
          sclk = buff[0] << 24 | buff[1] << 16 | buff[2] << 8 | buff[3] << 0;
        }
      }

#ifdef DBG
      printf ("%4d ADB_read start:%X sclk:%X stop:%X\n", __LINE__,
              adb->requested_start_sclk, sclk, adb->requested_stop_sclk);
#endif

      sclk--;
      if (sclk < adb->requested_start_sclk)
        status = 1;
      sclk++;
      sclk++;
      if (sclk > adb->requested_stop_sclk)
        status = 1;
    }
  }

#ifdef DBG
  printf ("%4d ADB_read %p **************************\n", __LINE__, buff);
#endif

  return buff;
}
unsigned char *ADB_read_stim (struct ADB *adb, int flag)
{
  int i;
  int status = 1;
  char *temp;
  unsigned char *buff;

  if (adb->adb_file_type != ADB_DATA_STIM)
    return NULL;
  while (status) {
    buff = ADB_read (adb);
    if (buff) {
      temp = (char *) buff;
      for (i = 0; i < 4; i++) {
        temp = strchr (temp, ',') + 1;
      }
      temp++;
      if (flag & ADB_STIM_LPSW) {
        if (!strncmp (temp, "LPSW", 4))
          status = 0;
      }
      if (flag & ADB_STIM_HSND) {
        if (!strncmp (temp, "HSND", 4))
          status = 0;
      }
      if (flag & ADB_STIM_STIM) {
        if (!strncmp (temp, "STIM", 4))
          status = 0;
      }
      if (flag & ADB_STIM_HDMP) {
        if (!strncmp (temp, "HDMP", 4))
          status = 0;
      }
      if (flag & ADB_STIM_HDMP) {
        if (!strncmp (temp, "HDMP", 4))
          status = 0;
      }
    } else
      status = 0;
  }
  return buff;
}
