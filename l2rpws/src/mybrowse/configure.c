#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "configure.h"

#define TOKEN 32

struct CONFIG_FLAGS
{
  int count;
  char *key;
  char *values[C_VALUES];
} config_flags[] = { {
0, DSN_FILE, C_VALUES * NULL}, {
0, WAIT, C_VALUES * NULL}, {
0, SLEEP, C_VALUES * NULL}, {
0, CRON_TIME, C_VALUES * NULL}, {
0, CRON_5_DAY, C_VALUES * NULL}, {
0, CRON_5_DAY_5, C_VALUES * NULL}, {
0, CRON_1_DAY, C_VALUES * NULL}, {
0, CRON_5_DAY_15, C_VALUES * NULL}, {
0, CRON_5_DAY_20, C_VALUES * NULL}, {
0, CRON_1_DAY_15, C_VALUES * NULL}, {
0, NERT_5_DAY, C_VALUES * NULL}, {
0, NERT_1_DAY, C_VALUES * NULL}, {
0, "", C_VALUES * NULL}};
struct DSN_FLAGS
{
  int flag;
  char mne[32];
} dsn_flags[] = {
0x1, "ACTIVE",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "",
    0x0, "", 0x0, "", 0x0, "", 0x0, "", 0x0, "", 0x0, "", 0x0, "", 0x0, ""};
struct DSN_RECORD
{
  time_t t_setup;
  time_t t_start;
  time_t t_stop;
  time_t t_los;
  int pass;
  int flags;
  char tracking_text[256];
};
static struct DSN_RECORD *dsn_record = NULL;
static int LOAD = 0;
static char dsn_filename_string[256];
static int dsn_index_count = 0;
static char tracking_delimiter[256] = { "CCSD3RE00000" };

char *hack (time_t * time)
{
  static int index = 0;
  static char result[4][256];

  index++;
  index &= 0x03;
  sprintf (result[index], "%08X %s", time[0], ctime (time));
  result[index][33] = 0;
  return result[index];
}


 /*
  *     Open and load from the configuration file
  *
  *             DSN_FILE=dsn-schedule-file
  */
char *configure (char *filename)
{
  static int first = 1;
  FILE *config_file;
  int i;
  int index;
  char detail[256];
  char *temp;
  char *token[16];

  config_file = fopen (filename, "r");
  if (!config_file)
    return NULL;
  while (fgets (detail, 255, config_file)) {
    temp = strchr (detail, 0x0A);
    if (temp)
      *temp = 0;
    if (detail[0]) {
      if (token[0] = strtok (detail, "= \t\n")) {
        token[1] = strtok (NULL, "\n");
        i = 0;
        while (config_flags[i].key) {
          if (first) {
            if (!strcmp (token[0], config_flags[i].key)) {
              index = config_flags[i].count;
              if (!config_flags[i].values[index])
                config_flags[i].values[index] = malloc (128);
              if (!config_flags[i].values[index]) {
                fprintf (stderr, "SHIT malloc failed\n");
                exit (-1);
              }
              memset (config_flags[i].values[index], 0, 128);
              strcpy (config_flags[i].values[index], token[1]);
              config_flags[i].count++;
            }
          }
          i++;
        }
      }
    }
  }
  fclose (config_file);
  first = 0;
  return dsn_filename_string;
}

 /*
  *     Scan the tracking file
  */

static int load_dsn_record (char *token[])
{
  char buffer[256];
  struct tm tm;
  int i;
  int k;

  dsn_record[dsn_index_count].flags = dsn_flags[0].flag;
  if (LOAD)
    fprintf (stdout, "index(%3d)\n", dsn_index_count);
  /**/ tm.tm_year = atoi (token[0]) + 100;
  tm.tm_mon = 0;
  tm.tm_mday = atoi (token[1]);
  tm.tm_wday = -1;                      /* not used */
  tm.tm_yday = tm.tm_mday - 1;          /* not used */
  tm.tm_isdst = 0;

  tm.tm_sec = 0;
  tm.tm_min = atoi (&token[2][2]);
  token[2][2] = 0;
  tm.tm_hour = atoi (token[2]);
  dsn_record[dsn_index_count].t_setup = mktime (&tm);

/*     dsn_record[dsn_index_count].t_setup += tm.tm_gmtoff; /* seconds east of GMT */
  if (LOAD) {
    fprintf (stdout, "    setup(%s) ",
             hack (&dsn_record[dsn_index_count].t_setup));
    /**/ fprintf (stdout, "\n");
  }

  tm.tm_min = atoi (&token[3][2]);
  token[3][2] = 0;
  tm.tm_hour = atoi (token[3]);
  dsn_record[dsn_index_count].t_start = mktime (&tm);

/*     dsn_record[dsn_index_count].t_setup += tm.tm_gmtoff;/**/
  if (dsn_record[dsn_index_count].t_start <
      dsn_record[dsn_index_count].t_setup)
    dsn_record[dsn_index_count].t_start += 86400;
  if (LOAD) {
    fprintf (stdout, "    start(%s) ",
             hack (&dsn_record[dsn_index_count].t_start));
    /**/ fprintf (stdout, "\n");
  }
  tm.tm_min = atoi (&token[4][2]);
  token[4][2] = 0;
  tm.tm_hour = atoi (token[4]);
  dsn_record[dsn_index_count].t_stop = mktime (&tm);

/*     dsn_record[dsn_index_count].t_setup += tm.tm_gmtoff;/**/
  if (dsn_record[dsn_index_count].t_stop <
      dsn_record[dsn_index_count].t_start)
    dsn_record[dsn_index_count].t_stop += 86400;
  if (LOAD) {
    fprintf (stdout, "    stop(%s) ",
             hack (&dsn_record[dsn_index_count].t_stop));
    /**/ fprintf (stdout, "\n");
  }
  tm.tm_min = atoi (&token[5][2]);
  token[5][2] = 0;
  tm.tm_hour = atoi (token[5]);
  dsn_record[dsn_index_count].t_los = mktime (&tm);

/*     dsn_record[dsn_index_count].t_setup += tm.tm_gmtoff;/**/
  if (dsn_record[dsn_index_count].t_los < dsn_record[dsn_index_count].t_stop)
    dsn_record[dsn_index_count].t_los += 86400;
  if (LOAD) {
    fprintf (stdout, "    los(%s) ",
             hack (&dsn_record[dsn_index_count].t_los));
    /**/ fprintf (stdout, "\n");
  }

  i = 6;
  dsn_record[dsn_index_count].tracking_text[0] = 0;
  while (token[i]) {
    k = atoi (token[i]);
    if (k > 1000)
      dsn_record[dsn_index_count].pass = k;
    strcat (dsn_record[dsn_index_count].tracking_text, token[i]);
    strcat (dsn_record[dsn_index_count].tracking_text, " ");
    i++;
  }
  i = 1;
  while (dsn_flags[i].flag) {
    if (strstr (dsn_record[dsn_index_count].tracking_text, dsn_flags[i].mne))
      dsn_record[dsn_index_count].flags |= dsn_flags[i].flag;
    i++;
  }
  if (LOAD) {
    fprintf (stdout, "    flag(%08X) ", dsn_record[dsn_index_count].flags);
    fprintf (stdout, "\n");
    fprintf (stdout, "    text(%s) ",
             dsn_record[dsn_index_count].tracking_text);
    fprintf (stdout, "\n");
    fprintf (stdout, "\n");
    /**/ fflush (stdout);
  }

  if ((dsn_record[dsn_index_count].t_setup <
       dsn_record[dsn_index_count].t_start)
      && (dsn_record[dsn_index_count].t_start <
          dsn_record[dsn_index_count].t_stop)
      && (dsn_record[dsn_index_count].t_stop <
          dsn_record[dsn_index_count].t_los)) {
    dsn_index_count++;
  } else {
    memset (&dsn_record[dsn_index_count], 0, sizeof (struct DSN_RECORD));
    fprintf (stdout, "%d load_dsn_record times hosed\n");
  }
}
static int load_dsn_file (FILE * dsn_file)
{
  char buffer[256];
  int i;
  int j;
  char *token[TOKEN + 1];

  while (fgets (buffer, 256, dsn_file)) {
    /*
     * fprintf(stdout, "SCAN %s", buffer); /*
     */
    buffer[12] = 0;
    if (strstr (buffer, tracking_delimiter))
      break;
  }
  while (fgets (buffer, 256, dsn_file)) {
    for (i = 0; i < TOKEN + 1; i++)
      token[i] = NULL;
    i = strlen (buffer);
    /*
     * fprintf(stdout, "LOAD %s", buffer); /*
     */
    if (i > 32) {
      token[0] = strtok (buffer, " \t\n");
      i = 1;
      while (token[i - 1]) {
        token[i] = strtok (NULL, " \t\n");
        i++;
        if (i >= TOKEN)
          break;
      }

                /****************************************
		 *	Check for comment lines		*
		 *	All schedule lines start 	*
		 *	with a number (i.e. year)	*
		 *	anything else is a problem	*
		 *					*
		 *	ALSO, look for the text 	*
		 *	string "PROBE" and suppress	*
		 *	the line (as we're MUTE then)	*
		 ****************************************/
      switch (token[0][0]) {
       case '0':
       case '1':
       case '2':
       case '3':
       case '4':
       case '5':
       case '6':
       case '7':
       case '8':
       case '9':
         load_dsn_record (token);
       default:
         break;
      }
    }
  }
}

int dsn_tracking_file (char *dsn_file_name)
{
  static FILE *dsn_file;

  dsn_index_count = 0;                  /* new copy of file */
  dsn_file = fopen (dsn_file_name, "r");
  if (!dsn_file)
    return 0;
  if (!dsn_record) {
    dsn_record = malloc (sizeof (struct DSN_RECORD) * 8192);
  }
  memset (dsn_record, 0, sizeof (struct DSN_RECORD) * 8192);
  if (!dsn_record) {
    fprintf (stdout, "SHIT: can't allocate memory\n");
    exit (0);
  }
  load_dsn_file (dsn_file);
  fclose (dsn_file);
  return 1;
}

 /*
  *     Extract Keys...
  */
char *dsn_tracking_file_key (char *key)
{
  static int i = 0;
  static int index = 0;

  if (!key) {
    return config_flags[i].values[index++];
  }
  i = 0;
  index = 0;
  while (config_flags[i].key) {
    if (!strcmp (key, config_flags[i].key)) {
      return config_flags[i].values[index++];
    }
    i++;
  }
  return NULL;
}

 /*
  *     Return pass number, if we find it
  *     Return 0 if we're not supposed to have data
  *     Return -1 if we don't have the file...
  */
int dsn_pass_number (int time_now)
{
  int i;

  if (time_now < 1)
    return 0;

  if (dsn_index_count) {                                /****************************/
    for (i = 0; i < dsn_index_count; i++) {     /* Scan the table we built  *//*  for where current time  */
      if ((dsn_record[i].t_setup < time_now) && /*  is between setup & los */
          (time_now < dsn_record[i].t_los))
        /*
         * times of some tracking  
         */
        /*
         * pass we know about      
         */

        return dsn_record[i].pass;                      /****************************/

    }
    return 0;                           /* nothing found, skip it */
  } else
    return -1;
}
