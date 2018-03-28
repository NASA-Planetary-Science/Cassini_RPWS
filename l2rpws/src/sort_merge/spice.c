#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <SpiceUsr.h>

#include <das2/das1.h>

/* Cassini Stuff */
#include <rtiu.h>

/*****************************************************************************/

static int first_time = 1;
static int SpaceCraft_ID = -82;
static int Partition = 1;

static char result[8][32];
static int master_result_index = 0;
static char format[] = { "D" };
static int prec = 3;
static double et;

static int sclk_index (void)
{
	static char* metafile = NULL;
	if (first_time) {
		first_time = 0;
	 
		if( getenv("CAS_TIME_KERNELS") == NULL){
			fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
			        "is not defined.\n");
			exit(13);
		}
	
		metafile = getenv("CAS_TIME_KERNELS"); 
		furnsh_c(metafile);
	}
	master_result_index++;
	master_result_index &= 7;
	return master_result_index;
}

static double sclk_et (int sclk, int fine)
{
  static char sclk_temp[32];

  sprintf (sclk_temp, "%d/%d:%03d", Partition, sclk, fine);     /* only valid to RTI */

  scs2e_c(SpaceCraft_ID, sclk_temp, &et);
  
  return et;
}

 /*
  *     000000000011111111112222
  *     012345678901234567890123
  *     yyyy-ddd // hh:mm:ss.mmm
  */
char *sclk_2_filename (int sclk, int fine)
{
  char scet_temp[32];
  int result_index;
  int in;
  int out;

  result_index = sclk_index ();
  et = sclk_et (sclk, fine);

  et2utc_c(et, format, prec, 32, scet_temp);
  out = 0;
  result[result_index][out++] = 't';
  for (in = 0; in < 4; in++)
    result[result_index][out++] = scet_temp[in];
  for (in = 5; in < 8; in++)
    result[result_index][out++] = scet_temp[in];
  for (in = 12; in < 14; in++)
    result[result_index][out++] = scet_temp[in];
  result[result_index][out++] = 0;
  strcat (result[result_index], "00");
  strcat (result[result_index], ".r00");
  return result[result_index];
}
char *sclk_2_scet (int sclk, int fine)
{
  int result_index;
  int in;
  int out;

  result_index = sclk_index ();
  et = sclk_et (sclk, fine);

  et2utc_ (&et, format, &prec, result[result_index], strlen (format), 32);
  out = 0;
  for (in = 0; in < 8; in++)
    result[result_index][out++] = result[result_index][in];
  result[result_index][out++] = 'T';
  for (in = 12; in < 24; in++)
    result[result_index][out++] = result[result_index][in];
  result[result_index][out++] = 0;
  return result[result_index];
}

char *scet_border (char *scet, int hours, int offset)
{
  static char result[8][32];
  static int result_index = 0;
  struct DATABASE_TIME_TAG database_time;
  int status;

  result_index++;
  result_index &= 7;

  status = parsetime (scet,
                      &database_time.year,
                      &database_time.month,
                      &database_time.mday,
                      &database_time.yday,
                      &database_time.hour,
                      &database_time.minute, &database_time.second);
  if (hours > 0) {
    database_time.hour /= hours;
    database_time.hour *= hours;
    database_time.hour -= offset;
    database_time.minute = -5;
    database_time.second = 0.0;
  }
  if (hours < 0) {
    database_time.hour /= abs (hours);
    database_time.hour *= abs (hours);
    database_time.hour += abs (hours);
    database_time.hour += offset;
    database_time.minute = +5;
    database_time.second = 0.0;
  }

  tnorm (&database_time.year,
         &database_time.month,
         &database_time.mday,
         &database_time.yday,
         &database_time.hour, &database_time.minute, &database_time.second);

  sprintf (result[result_index], "%04d-%03dT%02d:%02d:%06.3f",
           database_time.year,
           database_time.yday,
           database_time.hour, database_time.minute, database_time.second);
  return result[result_index];
}
