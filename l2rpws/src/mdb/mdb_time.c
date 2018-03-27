#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include <SpiceUsr.h>

/* Cas Libs Stuff */
#include <rtiu.h>
#define __mdb__
#include <mdb.h>
#include <rpws_sclk.h>
#include <mdb_time.h>

/*****************************************************************************/

static char Version[] = { "1.1" };

 /**/

  /**************************************************************
   *								*
   *								*
   *								*
   *	start time string					*
   *								*
   *		"2000-100T10:20:30.000"		SCET		*
   *		"5430F1A7"			SCLK hex	*
   *		"1/1234567890.123"		SCLK spice	*
   *								*
   **************************************************************/
   /**/ enum
{ TIME_bad = 0,
  SCET_strg = 1,
  SCLK_hex = 2,
  SCLK_spice = 3
};

static int SpaceCraft_ID = -82;


 /***********************************************************************
  *	SCLK in spice format, this simply means that			*
  *	there is a partition number (better be a 1, or we're		*
  *	in a world of hurt), a decimal SCLK and a decimal fine.		*
  *	We'll simply grab the seconds portion to convert		*
  ***********************************************************************/
static int mdb_sclk_spice (const char *time_string, int *part, int *sclk, int *fine)
{
  char *temp;
  int lclpart;
  int lcltime;
  int lclfine;

  lclpart = strtol (time_string, &temp, 10);
  lcltime = strtol (temp + 1, &temp, 10);
  lclfine = strtol (temp + 1, NULL, 10);
  if (part)
    *part = lclpart;
  if (sclk)
    *sclk = lcltime;
  if (fine)
    *fine = lclfine;
  return lcltime;
}
static int MDB_SCLK_spice (const char *time_string)
{
  return mdb_sclk_spice (time_string, NULL, NULL, NULL);
}

/**/

 /***********************************************************************
  *	SCLK in hexadecimal format.  If it contains a dot or colon	*
  *	  that will simply terminate the conversion and be ignored	*
  ***********************************************************************/
static int mdb_sclk_hex (const char *time_string, int *part, int *sclk, int *fine)
{
  int time;

  time = strtol (time_string, NULL, 16);
  if (part)
    *part = 1;
  if (sclk)
    *sclk = time;
  if (fine)
    *fine = 0;
  return time;
}
static int MDB_SCLK_hex (const char *time_string)
{
  return mdb_sclk_hex (time_string, NULL, NULL, NULL);
}

 /***********************************************************************
  *	SCET string:							*
  *	    Here's where spice come in, have to do this silly-ass	*
  *	    conversion to et then to a sclk STRING.			*
  *	    Then the SCLK string is converted back to binary		*
  *									*
  * localize mdb_scet_strg (i.e. static) V1.2				*
  ***********************************************************************/

static int mdb_scet_strg (const char *time_string, int *part, int *sclk, int *fine)
{
  int lclpart;
  int lcltime;
  int lclfine;
  char *temp;
  double et;
  char sclk_stg[64];
  int sclk_size = 32;
  char sTime[32] = {'\0'};

  strncpy(sTime, time_string, 31);
  
  if(strlen(sTime) == 8) strcat(sTime, "T");
  
/* fprintf(stdout, "%s/%d (%d)%s mdb_scet_strg\n", __FILE__, __LINE__, strlen(time_string), time_string);/**/
  utc2et_c(sTime, &et);

/* fprintf(stdout, "%s/%d %.3f\n", __FILE__, __LINE__, et); /**/
  sce2s_c(SpaceCraft_ID, et, sclk_size, sclk_stg);
  lclpart = strtol (sclk_stg, &temp, 10);
  lcltime = strtol (temp + 1, &temp, 10);
  lclfine = strtol (temp + 1, NULL, 10);
  if (part)
    *part = lclpart;
  if (sclk)
    *sclk = lcltime;
  if (fine)
    *fine = lclfine;
  return lcltime;
}
int MDB_SCET_strg (const char *time_string)
{
  return mdb_scet_strg (time_string, NULL, NULL, NULL);
}

 /***********************************************************************
  *	Convert a time to SCLK:						*
  *	  This will accept a SCET string (yyy-dddThh:mm:ss.uuu) or	*
  *	  a SCLK string.  SCLK string can be either			*
  *	  a spice format (partition/sclk.fine)				*
  *	  or a simple hex format (hex-seconds)				*
  *	In the case of SCET, we'lll make use of spice to come		*
  *	  up with the corresponding SCLK				*
  *
  * Willy, the algorithim used to fail with the common time format 2016-001
  * I've updated it to be take more information into accound when 
  * determining the time type.
  *
  ***********************************************************************/
static int mdb_time (const char *time_string, int *part, int *sclk, int *fine)
{
  int time_type = TIME_bad;
  int time_result = 0;
  int i, nLen = 0;

  /* first try HEX SCLK */
  nLen = strlen(time_string);
  time_type = SCLK_hex;
  for(i = 0; i < nLen; i++){
	  if(!isxdigit(time_string[i])){time_type = TIME_bad; break;}
  }
  
  /* Try SPICE time, if that fails default to SCET */
  if(time_type == TIME_bad){
	  time_type = SCLK_spice;
    for(i = 0; i < nLen; i++){
		if( !isdigit(time_string[i]) && (time_string[i] != '/') && 
			 (time_string[i] != '.') && (time_string[i] != ':') ){
			time_type = SCET_strg;
			break;
		}
	 }
  }
  
/* fprintf(stdout, "%s/%d %s %d\n", __FILE__, __LINE__, time_string, time_type); /**/

  if (part)
    *part = 0;
  if (sclk)
    *sclk = 0;
  if (fine)
    *fine = 0;
  switch (time_type) {
   case SCET_strg:

/* fprintf(stdout, "%s/%d %s SCET_strg\n", __FILE__, __LINE__, time_string); /**/
     time_result = mdb_scet_strg (time_string, part, sclk, fine);
     break;
   case SCLK_hex:

/* fprintf(stdout, "%s/%d %s SCLK_hex\n", __FILE__, __LINE__, time_string); /**/
     time_result = mdb_sclk_hex (time_string, part, sclk, fine);
     break;
   case SCLK_spice:

/* fprintf(stdout, "%s/%d %s SCLK_spice\n", __FILE__, __LINE__, time_string); /**/
     time_result = mdb_sclk_spice (time_string, part, sclk, fine);
     break;
  }

  if (time_result < CASSINI_STARTING_SCLK) {
    time_result = CASSINI_STARTING_SCLK;
  }
  return time_result;
}
int MDB_time (const char *time_string)
{
  int result;

/* fprintf(stdout, "%s/%d %s\n", __FILE__, __LINE__, time_string); /**/
  result = mdb_time (time_string, NULL, NULL, NULL);
  return result;
}

int MDB_time_verify (const char *time_string)
{
  int time_type = TIME_bad;
  int time_result = 0;

  if (strlen (time_string) == 8)
    time_type = SCLK_hex;
  if (strchr (time_string, 'T'))
    time_type = SCET_strg;
  if (strchr (time_string, '/'))
    time_type = SCLK_spice;

  switch (time_type) {
   case SCET_strg:
     time_result = MDB_SCET_strg (time_string);
     break;
   case SCLK_hex:
     time_result = MDB_SCLK_hex (time_string);
     break;
   case SCLK_spice:
     time_result = MDB_SCLK_spice (time_string);
     break;
  }

  if (time_result < CASSINI_STARTING_SCLK) {
    time_result = 0;
  }
  return time_result;
}


char *MDB_time_ver (void)
{
  static char version_string[64];

  sprintf (version_string, "%s V%s", __FILE__, Version);
  return version_string;
}
double MDB_time_et (const char *time_string)
{
  int partition;
  int fine;
  static char sclk_temp[32];
  char result[32];
  double et;
  char format[] = { "D" };
  int prec = 3;
  int sclk;


  mdb_time (time_string, &partition, &sclk, &fine);

      /****************************************
       *      in the archive file, we may     *
       *      store the partition as either   *
       *      zero or one (both indicate      *
       *      partition one).  Make it        *
       *      appropriate for SPICE           *
       ****************************************/
  sprintf (sclk_temp, "%d/%d:%03d", partition, sclk, fine);

      /****************************************
       *      SCLK to ET                      *
       ****************************************/
  scs2e_c(SpaceCraft_ID, sclk_temp, &et);
  return et;
}

char *MDB_time_SCET (const char *time_string)
{
  int partition;
  int fine;
  static char sclk_temp[32];
  char result[32];
  double et;
  char format[] = { "D" };
  int prec = 3;
  int sclk;


  et = MDB_time_et (time_string);

      /****************************************
       *       ET to UTC                      *
       ****************************************/
  et2utc_c(et, format, prec, 32, sclk_temp);

      /******************************************
       * Reformat to out standard string format *
       ******************************************/
  sclk_temp[8] = 'T';
  strcpy (&sclk_temp[9], &sclk_temp[12]);
  sclk_temp[21] = 0;

  return sclk_temp;
}
