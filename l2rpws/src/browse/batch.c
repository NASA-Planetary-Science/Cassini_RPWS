#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include "configure.h"

extern char *optarg;
extern int optind, opterr, optopt;

char *Version = { "V0.0" };
FILE *logfile = stdout;

static int Wait = 30 * 60;
static int Sleep = 60;
static int year;
static int day;
static int hour;
static int minute;
static int second;
static int start_day, stop_day;

 /*
  *     conver ut_time into something easy to use
  */
int current_time (time_t ut_time)
{
  struct tm *tm;

  tm = localtime (&ut_time);
  year = tm->tm_year + 1900;
  day = tm->tm_yday + 1;
  hour = tm->tm_hour;
  minute = tm->tm_min;
  second = tm->tm_sec;
  start_day = day / 5;
  start_day *= 5;
  stop_day = start_day + 5;
  return ut_time;
}

 /*
  *     Conver time-of-day string into seconds of day
  */
int seconds_of_day (char *times)
{
  int now;
  int hour;
  int minute;
  int second;
  char *temp;

  hour = strtol (times, &temp, 10);
  minute = strtol (temp + 1, &temp, 10);
  second = strtol (temp + 1, &temp, 10);

  now = hour * 3600 + minute * 60 + second;
  return now;
}

 /*
  *     *times is a list of seconds-of-day that we want to run at
  *      range is how often we're running.
  *
  *     See if we are withing range seconds of any of the
  *       *times items.
  */
int compare_time (int *times, int range)
{
  int i = 0;
  int now;
  int when;

  /*
   * Seconds of day
   */
  now = hour * 3600 + minute * 60 + second;

  while (times[i] >= 0) {
    /*
     * fprintf(stdout, "%.2f=abs(%d-%d) < %.2f=(float)%d/%.2f\n",
     * (float)(abs(now - times[i])),
     * now, times[i],
     * (float)range/2.0,
     * range, 2.0); /*
     */
    if ((float) (abs (now - times[i])) < ((float) range / 2.0))
      return 1;
    i++;
  }
  return 0;
}
int display_jobs (void)
{
  char *temp;

  temp = dsn_tracking_file_key (CRON_5_DAY);
  while (temp) {
    fprintf (logfile, temp, year, start_day, year, stop_day);
    fprintf (logfile, "\n");
    temp = dsn_tracking_file_key (NULL);
  }

  temp = dsn_tracking_file_key (CRON_5_DAY_5);
  while (temp) {
    fprintf (logfile, temp, year, start_day - 5, year, stop_day - 5);
    fprintf (logfile, "\n");
    temp = dsn_tracking_file_key (NULL);
  }

  temp = dsn_tracking_file_key (CRON_1_DAY);
  while (temp) {
    fprintf (logfile, temp, year, day - 4, year, day + 1);
    fprintf (logfile, "\n");
    temp = dsn_tracking_file_key (NULL);
  }

  temp = dsn_tracking_file_key (CRON_5_DAY_15);
  while (temp) {
    fprintf (logfile, temp, year, start_day - 15, year, stop_day - 15);
    fprintf (logfile, "\n");
    temp = dsn_tracking_file_key (NULL);
  }

  temp = dsn_tracking_file_key (CRON_5_DAY_20);
  while (temp) {
    fprintf (logfile, temp, year, start_day - 20, year, stop_day - 20);
    fprintf (logfile, "\n");
    temp = dsn_tracking_file_key (NULL);
  }

  temp = dsn_tracking_file_key (CRON_1_DAY_15);
  while (temp) {
    fprintf (logfile, temp, year, day - 19, year, day - 14);
    fprintf (logfile, "\n");
    temp = dsn_tracking_file_key (NULL);
  }
}
int main (int argc, char *argv[])
{
  int time_since_last = -1;
  int active = 1;
  int i;
  int current_pass = 0;
  int previous_pass = 0;

  char config_filename[256] = { "" };
  char c;
  char *dsn_tracking_filename;
  char *allocation_filename = NULL;
  char *temp;
  time_t ut_time;
  char *cron_time_string[C_VALUES] = { C_VALUES * NULL };
  int cron_time[C_VALUES] = { C_VALUES * -1 };

  while ((c = getopt (argc, argv, "c:l:?")) != EOF) {
    switch (c) {
     case 'c':
       strcpy (config_filename, optarg);
       break;
     case 'l':
       logfile = fopen (optarg, "a");
       break;
     case '?':
       fprintf (logfile, "%s %s\n", argv[0], Version);
       fprintf (logfile, "    \n");
       fprintf (logfile,
                "    -a xxx      Allocation file (runs after tracking pass)\n");
       fprintf (logfile,
                "    -c xxx      configuration filename (work to do)\n");
       fprintf (logfile, "    -l xxx      logfile (defauilts to logfile)\n");
       fprintf (logfile, "    \n");
       fprintf (logfile, "    \n");
       fprintf (logfile, "    \n");
       fprintf (logfile, "    \n");
    }
  }
  /*
   *  Flip time zone to UTC
   *  So the allocation file works correctly
   */
  putenv ("TZ=UTC");
  tzset ();

  /*
   *  Tell 'em who we are and what we're up to...
   */
  ut_time = current_time (time (NULL));
  fprintf (logfile, "DATE        %04d-%03dT%02d:%02d\n", year, day, hour,
           minute);
  fprintf (logfile, "VERS        %s\n", Version);
  if (config_filename[0])
    fprintf (logfile, "CONFIG     \"%s\"\n", config_filename);


  if (config_filename[0]) {
    configure (config_filename);
    allocation_filename = dsn_tracking_file_key (DSN_FILE);
    if (allocation_filename)
      fprintf (logfile, "ALLOCATION \"%s\"\n", allocation_filename);
    dsn_tracking_file (allocation_filename);
    i = 0;
    cron_time_string[i] = dsn_tracking_file_key (CRON_TIME);
    while (cron_time_string[i]) {
      cron_time[i] = seconds_of_day (cron_time_string[i]);
      i++;
      cron_time[i] = -1;
      cron_time_string[i] = dsn_tracking_file_key (NULL);
    }
    temp = dsn_tracking_file_key (WAIT);
    while (temp) {
      Wait = strtol (temp, NULL, 0);
      fprintf (stdout, "WAIT        %d\n", Wait);
      temp = dsn_tracking_file_key (NULL);
    }
    temp = dsn_tracking_file_key (SLEEP);
    while (temp) {
      Sleep = strtol (temp, NULL, 0);
      fprintf (stdout, "SLEEP       %d\n", Sleep);
      temp = dsn_tracking_file_key (NULL);
    }
    display_jobs ();
  }


  while (active) {
    ut_time = current_time (time (NULL));
    current_pass = dsn_pass_number (ut_time);

    fprintf (logfile, "Track(%02d:%02d:%02d) %d\n", hour, minute, second,
             current_pass);

    if (compare_time (cron_time, Sleep)) {      /* CRON schedule ? */
      fprintf (logfile, "CRON SCHEDULE\n");
      if (time_since_last > 0) {
        fprintf (stdout, "TIME SINCE LAST %d\n", time_since_last);
      } else {
        display_jobs ();
        time_since_last = Wait;
      }
      current_pass = 0;
    } else {
      if (current_pass == 0) {          /*  not tracking   */
        if (previous_pass > 0) {        /* just got done ? */
          fprintf (logfile, "ALLC SCHEDULE\n");
          if (time_since_last > 0) {
            fprintf (stdout, "TIME SINCE LAST %d\n", time_since_last);
          } else {
            display_jobs ();
            time_since_last = Wait;
          }
        }
      }
    }
    if (time_since_last > -1)
      time_since_last -= Sleep;
    previous_pass = current_pass;
    if (!allocation_filename)
      active = 0;
    else
      sleep (Sleep);
  }

  exit (0);
}
