#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

/* Cassini Stuff */
#include <rtiu.h>
#include <util.h>

#include <utilt.h>
#include <Util.h>                /* Boy this is going to mess with Windows */

/*SHIT #include "socket_calls.h" /**/

/*SHIT #include "dsp_web.h" /**/

#define TRACK
#define LOAD

static int flag_yday = 1;
static int timeout = 5;
static int dump_flag = 0;
static int result_flag = 0;
static int offset_flag = 0;
static char *cmd_flag;
static char *sms_flag[3];
static char *ann_flag;
static char tracking_delimiter[256] = { "CCSD3RE00000" };
static int test_flag = 0;
static char speech_pipe[256];
static int delta = 60;
static int noisy_flag = 0;
extern char *optarg;
extern int optind, opterr, optopt;

extern char *tzname[2];
extern long timezone;
extern int daylight;


struct DSN_RECORD
{
  time_t t_setup;
  time_t t_start;
  time_t t_stop;
  time_t t_los;
  char tracking_text[32];
};
static struct DSN_RECORD *dsn_record;
static int dsn_index_count = 0;

static double f_offset = 0.0;
char *hack (time_t * time)
{
  static int index = 0;
  static char result[4][256];

  index++;
  index &= 0x03;
  sprintf (result[index], "%08X %s", time[0], asctime (gmtime (time)));
  result[index][33] = 0;
  return result[index];
}
static int load_dsn_record (char *token[])
{
  char buffer[256];
  struct tm tm;

#ifdef LOAD
  fprintf (stdout, "index(%3d) \n", dsn_index_count);
  /**/
#endif

    tm.tm_year = atoi (token[0]) + 100;
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

#ifdef LOAD
  fprintf (stdout, "    setup(%s) ",
           hack (&dsn_record[dsn_index_count].t_setup));
  /**/ fprintf (stdout, "\n");
#endif

  tm.tm_min = atoi (&token[3][2]);
  token[3][2] = 0;
  tm.tm_hour = atoi (token[3]);
  dsn_record[dsn_index_count].t_start = mktime (&tm);

/*     dsn_record[dsn_index_count].t_setup += tm.tm_gmtoff;/**/
  if (dsn_record[dsn_index_count].t_start <
      dsn_record[dsn_index_count].t_setup)
    dsn_record[dsn_index_count].t_start += 86400;

#ifdef LOAD
  fprintf (stdout, "    start(%s) ",
           hack (&dsn_record[dsn_index_count].t_start));
  /**/ fprintf (stdout, "\n");
#endif

  tm.tm_min = atoi (&token[4][2]);
  token[4][2] = 0;
  tm.tm_hour = atoi (token[4]);
  dsn_record[dsn_index_count].t_stop = mktime (&tm);

/*     dsn_record[dsn_index_count].t_setup += tm.tm_gmtoff;/**/
  if (dsn_record[dsn_index_count].t_stop <
      dsn_record[dsn_index_count].t_start)
    dsn_record[dsn_index_count].t_stop += 86400;

#ifdef LOAD
  fprintf (stdout, "    stop(%s) ",
           hack (&dsn_record[dsn_index_count].t_stop));
  /**/ fprintf (stdout, "\n");
#endif

  tm.tm_min = atoi (&token[5][2]);
  token[5][2] = 0;
  tm.tm_hour = atoi (token[5]);
  dsn_record[dsn_index_count].t_los = mktime (&tm);

/*     dsn_record[dsn_index_count].t_setup += tm.tm_gmtoff;/**/
  if (dsn_record[dsn_index_count].t_los < dsn_record[dsn_index_count].t_stop)
    dsn_record[dsn_index_count].t_los += 86400;

#ifdef LOAD
  fprintf (stdout, "    los(%s) ", hack (&dsn_record[dsn_index_count].t_los));
  /**/ fprintf (stdout, "\n");
#endif

  strcpy (dsn_record[dsn_index_count].tracking_text, token[6]);
  strcat (dsn_record[dsn_index_count].tracking_text, " ");
  strcat (dsn_record[dsn_index_count].tracking_text, token[7]);
  strcat (dsn_record[dsn_index_count].tracking_text, " ");
  strcat (dsn_record[dsn_index_count].tracking_text, token[8]);
  strcat (dsn_record[dsn_index_count].tracking_text, " ");
  strcat (dsn_record[dsn_index_count].tracking_text, token[9]);
  if (!strcmp (token[9], "ARRAY")) {
    strcat (dsn_record[dsn_index_count].tracking_text, " ");
    strcat (dsn_record[dsn_index_count].tracking_text, token[10]);
  }

#ifdef LOAD
  fprintf (stdout, "    text(%s) ",
           dsn_record[dsn_index_count].tracking_text);
  fprintf (stdout, "\n");
  fprintf (stdout, "\n");
  /**/
#endif

    dsn_index_count++;
}
static int load_dsn_file (FILE * dsn_file)
{
  char buffer[256];

  while (fgets (buffer, 256, dsn_file)) {
    /*
     * fprintf(stdout, "SCAN %s", buffer); /*
     */
    buffer[12] = 0;
    if (strstr (buffer, tracking_delimiter))
      break;
  }
  while (fgets (buffer, 256, dsn_file)) {
    int i;
    char *token[12];

    i = strlen (buffer);
    /*
     * fprintf(stdout, "LOAD %s", buffer); /*
     */
    if (i > 32) {
      token[0] = strtok (buffer, " \t");
      i = 1;
      while (token[i - 1]) {
        token[i] = strtok (NULL, " \t");
        i++;
        if (i > 10)
          break;
      }
      load_dsn_record (token);
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
  dsn_record = malloc (sizeof (struct DSN_RECORD) * 65536);
  if (!dsn_record) {
    fprintf (stdout, "SHIT: can't allocate memory\n");
    exit (0);
  }
  load_dsn_file (dsn_file);
  fclose (dsn_file);
  return 1;
}

 /*
  *     Return 1 if we should have data
  *     Return 0 if we're not supposed to have data
  *     Return -1 if we don't have the file...
  */
static int dsn_tracking (int time_now)
{
  int time_start = time_now + f_offset;
  int time_stop = time_now - f_offset;
  int i;

  if (dsn_index_count) {                                /****************************/
    for (i = 0; i < dsn_index_count; i++) {     /* Scan the table we built  *//*  for where current time  */

#ifdef TRACK
      fprintf (stdout, "TRACK %d %08X < %08X &&  %08X < %08X\n", i,
               dsn_record[i].t_start, time_start,
               time_stop, dsn_record[i].t_stop);
#endif

      if ((dsn_record[i].t_start < time_start) &&       /*  is between start & stop */
          (time_stop < dsn_record[i].t_stop))   /*  times of some tracking  */
        return 1;                       /*  pass we know about      */

    }                                                   /****************************/
    return 0;                           /* nothing found, skip it */
  } else
    return -1;
}

 /*
  *     Return 1 if we are in a tracking pass
  *     Return 0 if we're not in a tracking pass
  *     Return -1 if we don't have the file...
  */
static int dsn_active (int time_now)
{
  int time_setup = time_now + f_offset;
  int time_los = time_now - f_offset;
  int i;

  if (dsn_index_count) {                                /****************************/
    for (i = 0; i < dsn_index_count; i++) {     /* Scan the table we built  *//*  for where current time  */

#ifdef TRACK
      fprintf (stdout, "ACTIV %d\n", i);
      fprintf (stdout, "    t_setup    %08X(%s)\n",
               dsn_record[i].t_setup, hack (&dsn_record[i].t_setup));
      fprintf (stdout, "    time_setup %08X(%s)\n",
               time_setup, hack ((time_t *) & time_setup));
      fprintf (stdout, "    time_los   %08X(%s)\n",
               time_los, hack ((time_t *) & time_los));
      fprintf (stdout, "    t_los      %08X(%s)\n",
               dsn_record[i].t_los, hack (&dsn_record[i].t_los));
      fprintf (stdout, "\n");
#endif

      if ((dsn_record[i].t_setup < time_setup) &&       /*  is between setup & los */
          (time_los < dsn_record[i].t_los))     /*  times of some tracking  */
        return 1;                       /*  pass we know about      */

                                                        /****************************/
    }
    return 0;                           /* nothing found, skip it */
  } else
    return -1;
}

 /*
  *     Return tracking text if we should have data
  *     Return NULL          if we're not supposed to have data
  *     Return ""            if we don't have the file...
  */
static char *dsn_tracking_text (int time_now)
{
  int time_start = time_now - f_offset;
  int time_stop = time_now + f_offset;
  int i;
  static char *inactive = { "" };
  static char *setup_text[2] = { "Setup", "Teardown" };
  char *result = NULL;

  if (dsn_index_count) {                                /****************************/
    for (i = 0; i < dsn_index_count; i++) {     /* Scan the table we built  *//*  and pick the text that  */
      if ((dsn_record[i].t_setup < time_now) && /*  tells us something      */
          (time_now < dsn_record[i].t_start))   /*  about what DSN          */
        result = setup_text[0];         /*  resources we're now     */
      if ((dsn_record[i].t_stop < time_now) &&  /*  using.  Also, stick     */
          (time_now < dsn_record[i].t_los))     /*  in "Setup" and          */
        result = setup_text[1];         /*  "Teardown" text into    */
      if ((dsn_record[i].t_start < time_start) &&       /*                          */
          (time_stop < dsn_record[i].t_stop))   /*                          */
        result = dsn_record[i].tracking_text;   /*                          */

    }                                                   /****************************/
  } else
    result = inactive;
  return result;
}

 /*
  *     Put time into SMS message
  */
char *message_text (char *sms_text, char *msg_text, time_t ut_time)
{
  static char outbuf[256];
  struct tm *tm;

  tm = localtime (&ut_time);
  sprintf (outbuf, "%s \"CAS %03d-%02d:%02d %s\"",
           sms_text, tm->tm_yday, tm->tm_hour, tm->tm_min, msg_text);
  return outbuf;
}

 /*
  *     Send out commands (local, SMS (cellphone), speech)
  */
int sms_commands (char *tag, char *text, time_t ut_time)
{
  int count;
  char *buf;
  char *temp;

  fprintf (stdout, "%s\n", tag);
  if (!strcmp (tag, "SMS"))
    buf = sms_flag[0];
  if (!strcmp (tag, "SMS1"))
    buf = sms_flag[1];
  if (!strcmp (tag, "SMS2"))
    buf = sms_flag[2];
  while (buf[0]) {
    count++;
    temp = message_text (buf, text, ut_time);
    fprintf (stdout, ">>> %s\n", temp);
    /**/ system (temp);
    buf += strlen (buf) + 1;
  }
  return count;
}

int cmd_commands ()
{
  int count;
  char *buf = cmd_flag;

  /*
   * fprintf(stdout, "CMD\n"); /*
   */
  while (buf[0]) {
    count++;
    /*
     * fprintf(stdout, ">>> %s\n", buf); /*
     */
    system (buf);
    /**/ buf += strlen (buf) + 1;
  }
  return count;
}

int announce_commands ()
{
  FILE *speech;
  int count;
  char *buf = ann_flag;

  speech = fopen (speech_pipe, "w");
  /*
   * if(!speech)
   * fprintf(stdout, "ANN\n"); /*
   */
  while (buf[0]) {
    count++;
    if (speech)
      fprintf (speech, "%s\n", buf);
    /*
     * else
     * fprintf(stdout, ">>> %s\n", buf); /*
     */

    buf += strlen (buf) + 1;
  }
  if (speech)
    fclose (speech);
  return count;
}
int main2 (time_t ut_time, FILE * debug_file, int tracking,
           int previous_tracking)
{
  static char result[16384];
  int epoch;
  long int cds[6];
  int i;
  int minutes;
  struct RPWS_buffer *buf;
  struct tm pkt_transit;
  struct tm pkt_ut;
  struct tm pkt_lcl;
  struct tm *pkt_ert;
  struct tm *pkt_scet;
  struct event_time ert_tim;
  struct event_time *evt_tim;
  struct event_clock evt_clk;
  time_t cds_time;
  char *text;

  if (debug_file)
    fprintf (debug_file, "Request:\n");

/* SHIT    buf = dsp_web_fetch("~wtr/recent_hsk.dat.vc0.bin", result, debug_file); /**/

  if (result_flag) {
    fprintf (stdout, "Response:\n");
    fprintf (stdout, "%s", result);
    fflush (stdout);
  }

  if (dump_flag) {
    fprintf (stdout, "f_length %08X  record_type %08X   status %08X\n",
             UTIL_MSB_to_long (buf->f_length),
             UTIL_MSB_to_long (buf->record_type),
             UTIL_MSB_to_long (buf->status));
    for (i = 0; i < 256; i++) {
      if (i % 32 == 0)
        fprintf (stdout, "%04X: ", i);
      fprintf (stdout, "%02X ", buf->packet.filler[i] & 0xFF);
      if (i % 32 == 31)
        fprintf (stdout, "\n");
    }
    fprintf (stdout, "\n");
    for (i = 0; i < 192; i++) {
      if (i % 32 == 0)
        fprintf (stdout, "%04X: ", i);

/* SHIT             fprintf(stdout, "%02X ", buf->packet.mp.mini_packet[i] & 0xFF); /**/
      if (i % 32 == 31)
        fprintf (stdout, "\n");
    }
    fflush (stdout);
  }

  memcpy (&pkt_ut, gmtime (&ut_time), sizeof (struct tm));
  memcpy (&pkt_lcl, localtime (&ut_time), sizeof (struct tm));

  epoch = (buf->packet.cds_tag.epoch[0] << 24) |
    (buf->packet.cds_tag.epoch[1] << 16) |
    (buf->packet.cds_tag.epoch[2] << 8) | (buf->packet.cds_tag.epoch[3] << 0);


  /*
   * Willy, you can't do this an int is shorter than a Long, you might
   * over run the memory for the int! -cwp 2012-01-09 
   */

  Util_extract_CDS ((struct CDS_buffer *) buf, cds);
  cds_time = (time_t) Util_extract_TIME ((struct CDS_buffer *) buf);

  evt_clk.seconds = cds_time;
  evt_clk.fine = Util_extract_RTI ((struct CDS_buffer *) buf) << 5;
  evt_tim = Util_event_scet ((struct MP_buffer *) buf, evt_clk);

  pkt_scet = Util_event_scet_tm (*evt_tim, 0);

  ert_tim.days = buf->packet.chdo_tag.ert.days;
  ert_tim.milliseconds = buf->packet.chdo_tag.ert.milliseconds;
  pkt_ert = Util_event_scet_tm (ert_tim, 0);

  pkt_transit.tm_sec = pkt_ut.tm_sec - pkt_ert->tm_sec;
  pkt_transit.tm_min = pkt_ut.tm_min - pkt_ert->tm_min;
  pkt_transit.tm_hour = pkt_ut.tm_hour - pkt_ert->tm_hour;
  pkt_transit.tm_yday = pkt_ut.tm_yday - pkt_ert->tm_yday;

  if (pkt_transit.tm_sec < 0) {
    pkt_transit.tm_sec += 60;
    pkt_transit.tm_min -= 1;
  }
  if (pkt_transit.tm_sec > 60) {
    pkt_transit.tm_sec -= 60;
    pkt_transit.tm_min += 1;
  }

  if (pkt_transit.tm_min < 0) {
    pkt_transit.tm_min += 60;
    pkt_transit.tm_hour -= 1;
  }
  if (pkt_transit.tm_min > 60) {
    pkt_transit.tm_min -= 60;
    pkt_transit.tm_hour += 1;
  }
  pkt_transit.tm_hour += pkt_transit.tm_yday * 24;

  if (offset_flag) {
    text = dsn_tracking_text (ut_time);
    if (text) {
      if (text[0])
        printf ("  Tracking  %s\n", text);
    } else
      printf ("  Not tracking\n");

    printf ("  UT:    %04d-%03dT%2.2d:%2.2d:%2.2d ",
            pkt_ut.tm_year + 1900,
            pkt_ut.tm_yday + flag_yday,
            pkt_ut.tm_hour, pkt_ut.tm_min, pkt_ut.tm_sec);
    printf ("(-%02d:%02d:%02d) ",
            pkt_transit.tm_hour, pkt_transit.tm_min, pkt_transit.tm_sec);
    printf ("\n");
    printf (" ERT:    %04d-%03dT%2.2d:%2.2d:%2.2d.%3.3d ",
            pkt_ert->tm_year + 1900,
            pkt_ert->tm_yday + flag_yday,
            pkt_ert->tm_hour,
            pkt_ert->tm_min, pkt_ert->tm_sec, ert_tim.milliseconds % 1000);

    minutes = pkt_transit.tm_hour * 60 + pkt_transit.tm_min;
    printf ("(-%d.%d)", minutes, pkt_transit.tm_sec / 6);
    printf ("\n");
    if (minutes > timeout)
      printf ("         NO DATA for %.1f Hours\n", minutes / 60.0);
  }

  if (!test_flag) {
    if (!tracking)
      return 0;
    if (minutes < timeout)
      return 0;
  }

  /*
   * Here is where we let the outside world know
   * that things are amiss...
   */
  if ((!previous_tracking) && (tracking)) {
    char temp[256];

    sprintf (temp, "No Data %s", dsn_tracking_text (ut_time));
    sms_commands ("SMS", temp, ut_time);
    cmd_commands ();
    announce_commands ();
  }
  return 1;
}

 /*
  *     Configuration File:
  *
  *     pass times:
  *             DSN_FILE=c42.allc
  *     Who to call when it breaks:
  *             SMS=/here/there/sms_dialer.pl 3193300226
  *     Who to call when it starts
  *             SMS1=/here/there/sms_dialer.pl 3193300226
  *     Who to call when it stops
  *             SMS2=/here/there/sms_dialer.pl 3193300226
  *     What to do when it breaks
  *             CMD=/here/there/anywhere/heyu turn radio on
  *     What to say when it breaks:
  *             ANNOUNCE=some text to speak
  *     How to say it (ANNOUNCE)
  *             ANNOUNCE_PIPE=pipe
  *
  *     Returns the DSN= field
  */
char *config_append (char *buf, char *text)
{
  char *temp = buf;

  strcat (temp, text);
  temp += strlen (text);
  temp[0] = 0;
  temp[1] = 0;
  *temp++;
  return temp;
}
char *configure (char *filename)
{
  FILE *config_file;
  char *temp;
  char *token[2];
  char *sms[3];
  char *cmd = cmd_flag;
  char *ann = ann_flag;
  static char dsn_filename_string[256];
  char detail[256];

  sms[0] = sms_flag[0];
  sms[1] = sms_flag[1];
  sms[2] = sms_flag[2];

  config_file = fopen (filename, "r");
  if (!config_file)
    return NULL;
  while (fgets (detail, 255, config_file)) {
    temp = strchr (detail, 0x0A);
    if (temp)
      *temp = 0;
    if (detail[0]) {
      if (token[0] = strtok (detail, "= \t")) {
        token[1] = strtok (NULL, "\n");
        if (!strcmp (token[0], "DSN_FILE"))
          strcpy (dsn_filename_string, token[1]);
        if (!strcmp (token[0], "DSN_DELIMITER"))
          strcpy (tracking_delimiter, token[1]);
        if (!strcmp (token[0], "ANNOUNCE_PIPE"))
          strcpy (speech_pipe, token[1]);
        if (!strcmp (token[0], "SMS"))
          sms[0] = config_append (sms[0], token[1]);
        if (!strcmp (token[0], "SMS1"))
          sms[1] = config_append (sms[1], token[1]);
        if (!strcmp (token[0], "SMS2"))
          sms[2] = config_append (sms[2], token[1]);
        if (!strcmp (token[0], "CMD"))
          cmd = config_append (cmd, token[1]);
        if (!strcmp (token[0], "ANNOUNCE"))
          ann = config_append (ann, token[1]);
      }
    }
  }
  fclose (config_file);
  return dsn_filename_string;
}
int main (int argc, char *argv[])
{
  int active, tracking;
  int previous_active = 0, previous_tracking = 0;
  int i;
  char c;
  char *text;
  FILE *debug_file = NULL;
  time_t ut_time;
  char config_filename[256];
  char *dsn_tracking_filename;

  cmd_flag = malloc (65536);
  sms_flag[0] = malloc (65536);
  sms_flag[1] = malloc (65536);
  sms_flag[2] = malloc (65536);
  ann_flag = malloc (65536);

  putenv ("TZ=UTC");
  tzset ();
  fprintf (stdout, "tzname %s/%s timezone %d\n", tzname[0], tzname[1],
           timezone);

  while ((c = getopt (argc, argv, "c:Dd:F:hnorTt:?")) != EOF) {
    switch (c) {
     case 'c':
       strcpy (config_filename, optarg);
       break;
     case 'd':
       delta = atoi (optarg);
       break;
     case 'D':
       dump_flag = 1;
       break;
     case 'F':
       f_offset = atof (optarg) * 60.0;
       break;
     case 'n':
       noisy_flag = 1;
       break;
     case 'o':
       offset_flag = 1;
       break;
     case 'r':
       debug_file = stdout;
       result_flag = 1;
       break;
     case 't':
       timeout = atol (optarg);
       break;
     case 'T':
       test_flag = 1;
       break;
     case 'h':
     case '?':
       fprintf (stdout, "Help Text\n");
       fprintf (stdout, "      -c xxx  Configuration file\n");
       fprintf (stdout,
                "      -d nn   delta between checks (def: %d seconds)\n",
                delta);
       fprintf (stdout, "      -D      Hex Dump\n");
       fprintf (stdout, "      -o      Time Offset Dump (hsk pkt)\n");
       fprintf (stdout, "      -r      Result Dump\n");
       fprintf (stdout, "      -t n    Timeout (%d)\n", timeout);
       fprintf (stdout, "      -h      Help\n");
       fprintf (stdout, "      -T      Test (act like timeout occurs)\n");
       fprintf (stdout, "\n");
       fprintf (stdout, "      -F n    Time offset from start/stop\n");
       fprintf (stdout, "               (this will suppress notification\n");
       fprintf (stdout,
                "                messages when not tracking, -F moves\n");
       fprintf (stdout,
                "                start/stop time this many minutes\n");
       fflush (stdout);
       fprintf (stdout, "Config file:\n");
       fprintf (stdout, "    Tracking File:\n");
       fprintf (stdout,
                "             DATA_SET_ID = DSN_EIGHT_WEEK_SCHEDULE;\n");
       fprintf (stdout, "            (get this from the SOPC)\n");
       fprintf (stdout,
                "            DSN_DELIMITER is text from the DSN_FILE\n");
       fprintf (stdout, "            wher we start loading data\n");
       fprintf (stdout, "        DSN_FILE=<filename>\n");
       fprintf (stdout, "        DSN_DELIMITER=<%s>\n", tracking_delimiter);
       fprintf (stdout, "    Cell/Pager notification:\n");
       fprintf (stdout,
                "             This set of entries are used to send\n");
       fprintf (stdout, "            an SMS message to a pager or phone.\n");
       fprintf (stdout, "            Command line (i.e. script to contact\n");
       fprintf (stdout, "            a specific provider along with the\n");
       fprintf (stdout, "            pager or cell number)\n");
       fprintf (stdout, "        SMS=<command>   Data Loss\n");
       fprintf (stdout, "        SMS1=<command>  Start of Track\n");
       fprintf (stdout, "        SMS2=<command>  End of Track\n");
       fprintf (stdout, "   Local notification:\n");
       fprintf (stdout, "             This is intended to allow lights\n");
       fprintf (stdout,
                "            or alarms to be sent (i.e. X-10 command\n");
       fprintf (stdout, "            to turn radio on, for example).\n");
       fprintf (stdout, "        CMD=<command_string>\n");
       fprintf (stdout, "   Verbal notification:\n");
       fprintf (stdout,
                "             Verbal notification.  The text provided\n");
       fprintf (stdout,
                "            on the ANNOUNCE lines are fed to the file\n");
       fprintf (stdout,
                "            (i.e. pipe) specified in ANNOUNCE_PIPE.\n");
       fprintf (stdout, "             A speech synthesis program, such as\n");
       fprintf (stdout, "            Festival would be waiting for speech\n");
       fprintf (stdout, "            text on the other end of the pipe.\n");
       fprintf (stdout, "        ANNOUNCE=<text>\n");
       fprintf (stdout, "        ANNOUNCE_PIPE=<filename>\n");
       exit (0);
       break;
    }
  }
  fprintf (stdout, "CONFIG %s\n", config_filename);
  dsn_tracking_filename = configure (config_filename);
  if (dsn_tracking_filename)
    fprintf (stdout, "TRACK  %s\n", dsn_tracking_filename);
  else {
    fprintf (stdout, "Configuration file or DSN filename missing \n");
    exit (0);
  }

  while (1) {
    dsn_tracking_filename = configure (config_filename);
    if (!dsn_tracking_filename) {
      fprintf (stdout, "Configuration file or DSN filename missing \n");
      exit (0);
    }
    if (!dsn_tracking_file (dsn_tracking_filename)) {
      fprintf (stdout, "DSN tracking file not found(%s)\n",
               dsn_tracking_filename);
      exit (0);
    }

    ut_time = time (NULL);
    active = dsn_active (ut_time);
    if (active < 0)
      fprintf (stdout, "ACTIVE %d\n", active);
    tracking = dsn_tracking (ut_time);
    if (tracking < 0)
      fprintf (stdout, "TRACKING %d\n", tracking);

    if ((!previous_active) && (active)) {
      char temp[256];

      sprintf (temp, "Setup %s", dsn_tracking_text (ut_time));
      sms_commands ("SMS1", temp, ut_time);
    }
    if ((previous_active) && (!active)) {
      char temp[256];

      sprintf (temp, "Teardown %s", dsn_tracking_text (ut_time));
      sms_commands ("SMS2", temp, ut_time);
    }
    fflush (stdout);
    main2 (ut_time, debug_file, tracking, previous_tracking);

    previous_active = active;
    previous_tracking = tracking;
    sleep (delta);
  }
}
