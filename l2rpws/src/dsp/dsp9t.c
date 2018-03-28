#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

/****************************************************************************/
/* Compiled in config file directory locations */

#ifndef INST_ETC
#error Compiled in default config directory is missing.
#endif

/****************************************************************************/

#define TOKEN 32
struct DSN_RECORD
{
  time_t t_setup;
  time_t t_start;
  time_t t_stop;
  time_t t_los;
  int pass;
  char time_year[16];
  char time_day[16];
  char time_setup[16];
  char time_start[16];
  char time_stop[16];
  char time_los[16];
  int flags;
  int index;
  char tracking_text[256];
};
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
static int ACTIV = 0;
static int LOAD = 0;
static int flag_yday = 1;
static int dsn_index_count;
static struct DSN_RECORD *dsn_record;
static char tracking_delimiter[256] = { "CCSD3RE00000" };
static int dsn_active_pass = 0;

char *hack_1 (time_t * time)
{
  static int index = 0;
  static char result[8][256];
  struct tm *tm;

  index++;                              /* 0123456789012345678901234 */
  index &= 0x07;                        /* Sat Mar  6 12:06:44 2004  */
  memset (result[index], 0, 256);
  if (time) {
    if (time[0]) {
      tm = gmtime (time);
      sprintf (result[index],
               "%08X %04d-%03dT%02d:%02d:%02dZ (%04d-%02d-%02d)", time[0],
               tm->tm_year + 1900, tm->tm_yday + flag_yday, tm->tm_hour,
               tm->tm_min, tm->tm_sec, tm->tm_year + 1900, tm->tm_mon + 1,
               tm->tm_mday);
    }
  }
  return result[index];
}

char *hack_2 (struct DSN_RECORD *dsn_record)
{
  static int index = 0;
  static char result[8][256];

  index++;                              /* 0123456789012345678901234 */
  index &= 0x07;                        /* Sat Mar  6 12:06:44 2004  */
  memset (result[index], 0, 256);

  if (dsn_record) {
    sprintf (result[index], "20%s %s %s-%s",
             dsn_record->time_year,
             dsn_record->time_day,
             dsn_record->time_start, dsn_record->time_stop);
  }
  return result[index];
}

static int dump_dsn (FILE * loffile)
{
  int i;

  fprintf (loffile, "DSN.allc dump_dsn(%d)\n", dsn_index_count);
  for (i = 0; i < dsn_index_count; i++) {
    fprintf (loffile, "DSN.allc ");
    fprintf (loffile, "%08X ", dsn_record[i].t_setup);
    fprintf (loffile, "%08X ", dsn_record[i].t_start);
    fprintf (loffile, "%08X ", dsn_record[i].t_stop);
    fprintf (loffile, "%08X ", dsn_record[i].t_los);
    fprintf (loffile, "\"%s\" ", dsn_record[i].tracking_text);
    fprintf (loffile, "\n");
  }
}
static int load_dsn_record (char *token[], int index)
{
  char buffer[256];
  struct tm tm;
  int i;
  int k;

  strcpy (dsn_record[dsn_index_count].time_year, token[0]);
  strcpy (dsn_record[dsn_index_count].time_day, token[1]);
  strcpy (dsn_record[dsn_index_count].time_setup, token[2]);
  strcpy (dsn_record[dsn_index_count].time_start, token[3]);
  strcpy (dsn_record[dsn_index_count].time_stop, token[4]);
  strcpy (dsn_record[dsn_index_count].time_los, token[5]);
  dsn_record[dsn_index_count].flags = dsn_flags[0].flag;
  if (LOAD)
    fprintf (stderr, "index(%3d) %s %s\n", dsn_index_count, token[0],
             token[0], token[1]);
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
  dsn_record[dsn_index_count].index = index;
  dsn_record[dsn_index_count].t_setup = mktime (&tm);

/*     dsn_record[dsn_index_count].t_setup += tm.tm_gmtoff; /* seconds east of GMT */
  if (LOAD) {
    fprintf (stderr, "    setup(%s) ",
             hack_1 (&dsn_record[dsn_index_count].t_setup));
    /**/ fprintf (stderr, "\n");
    fflush (stderr);
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
    fprintf (stderr, "    start(%s) ",
             hack_1 (&dsn_record[dsn_index_count].t_start));
    /**/ fprintf (stderr, "\n");
    fflush (stderr);
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
    fprintf (stderr, "    stop(%s) ",
             hack_1 (&dsn_record[dsn_index_count].t_stop));
    /**/ fprintf (stderr, "\n");
    fflush (stderr);
  }
  tm.tm_min = atoi (&token[5][2]);
  token[5][2] = 0;
  tm.tm_hour = atoi (token[5]);
  dsn_record[dsn_index_count].t_los = mktime (&tm);
  /*
   * dsn_record[dsn_index_count].t_setup += tm.tm_gmtoff;/*
   */
  if (dsn_record[dsn_index_count].t_los < dsn_record[dsn_index_count].t_stop)
    dsn_record[dsn_index_count].t_los += 86400;
  if (LOAD) {
    fprintf (stderr, "    los(%s) ",
             hack_1 (&dsn_record[dsn_index_count].t_los));
    /**/ fprintf (stderr, "\n");
    fflush (stderr);
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
    fprintf (stderr, "    flag(%08X) ", dsn_record[dsn_index_count].flags);
    fprintf (stderr, "\n");
    fprintf (stderr, "    text(%s) ",
             dsn_record[dsn_index_count].tracking_text);
    fprintf (stderr, "\n");
    fprintf (stderr, "\n");
    /**/ fflush (stderr);
  }
  dsn_index_count++;
}

static int load_dsn_file (FILE * dsn_file)
{
  char buffer[256];
  int i;
  int j;
  int index = 0;
  char *token[TOKEN + 1];

  while (fgets (buffer, 256, dsn_file)) {
    index++;
    /*
     * fprintf(stderr, "SCAN %s", buffer); /*
     */
    buffer[12] = 0;
    if (strstr (buffer, tracking_delimiter))
      break;
  }
  while (fgets (buffer, 256, dsn_file)) {
    index++;
    for (i = 0; i < TOKEN + 1; i++)
      token[i] = NULL;
    i = strlen (buffer);
    /*
     * fprintf(stderr, "LOAD %s", buffer); /*
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
               *      Check for comment lines         *
               *      All schedule lines start        *
               *      with a number (i.e. year)       *
               *      anything else is a problem      *
               *                                      *
               *      ALSO, look for the text         *
               *      string "PROBE" and suppress     *
               *      the line (as we're MUTE then)   *
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
         load_dsn_record (token, index);
       default:
         break;
      }
    }
  }
}
static int dsn_tracking_file (char *dsn_file_name)
{
  static FILE *dsn_file;

  dsn_file = fopen (dsn_file_name, "r");
  dsn_index_count = 0;                  /* new copy of file */
  if (!dsn_file)
    return 0;
  if (!dsn_record)
    dsn_record = malloc (sizeof (struct DSN_RECORD) * 8192);
  memset (dsn_record, 0, sizeof (struct DSN_RECORD) * 8192);
  if (!dsn_record)
    return 0;
  load_dsn_file (dsn_file);
  fclose (dsn_file);
  return 1;
}

 /*
  *     Return 1 if we are doing tracking setup
  *     Return 2 if we are tracking
  *     Return 3 if we are doing tracking teardown
  *     Return 0 if we're not in a tracking pass
  *     Return -1 if we don't have the file...
  *     Return -2 if the tarcking allocation file is out of date
  */
static int dsn_active (time_t time_now, int *flags)
{
  int i;

  if (dsn_index_count) {
    for (i = 0; i < dsn_index_count; i++) {
      if (ACTIV) {
        fprintf (stderr, "ACTIV %3d (%d)\n", i,
                 (dsn_record[i].t_setup < time_now) &&
                 (time_now < dsn_record[i].t_los)
          );
        fflush (stderr);
      }
      dsn_active_pass = i;
      if (time_now < dsn_record[i].t_setup) {
        return dsn_record[i].index;
      }
      if ((dsn_record[i].t_setup <= time_now) &&
          (time_now <= dsn_record[i].t_los)) {
        if (flags)
          flags[0] = dsn_record[i].flags;
        if (time_now < dsn_record[i].t_start) {
          return dsn_record[i].index;
        }
        if (time_now < dsn_record[i].t_stop) {
          return dsn_record[i].index;
        }
        return dsn_record[i].index;
      }
    }                                   /* for(i=0; i<dsn_index_count; i++) */
    return -dsn_record[i].index;
  } else {
    return -dsn_record[i].index;
  }
}                                       /* if(dsn_index_count) */

char *dsp9t (int offset_in)
{
  char *zone[2];
  time_t ut_time;
  int flags[10];
  int index;
  static char buf[256];
  static char newzone[32];
  static int first = 1;
  static char dsn_tracking_filename[256] = {'\0'};
 
  int offset;

  offset = offset_in;
  ut_time = time (NULL);
  zone[0] = getenv ("TZ");
  putenv ("TZ=UT");
  tzset ();
  zone[1] = getenv ("TZ");

  if (first) {
	  	  
	  strcpy(dsn_tracking_filename, INST_ETC "/cassini.allc" );
	  
    if (!dsn_tracking_file (dsn_tracking_filename))
      return NULL;
    /*
     * dump_dsn(stderr); /*
     */
    first = 0;
  }
  index = dsn_active (ut_time, flags);
  sprintf (buf, "(%s)", hack_2 (&dsn_record[dsn_active_pass + offset]));

  sprintf (newzone, "TZ=%s", zone[0]);
  putenv (newzone);
  tzset ();

  return buf;
}
