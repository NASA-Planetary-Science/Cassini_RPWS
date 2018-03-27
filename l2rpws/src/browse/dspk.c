#include <getopt.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include "rtiu.h"
#include "utilt.h"
#include "cds_status.h"

#include "socket_calls.h"
#include "dsp_web.h"
#include "dsp_monitor.h"
#include "conv.h"

#define ONLINE

#define TOKEN 32

static char *Version = { "V2.4" };
static int help_flag = 0;
static int flag_yday = 1;
static int timeout = 5;
static int dump_flag = 0;
static int result_flag = 0;
static int offset_flag = 0;
static char *cmd_flag[8];
static char *sms_flag[8];
static char *ann_flag[8];
static char *mail_flag[8];
static int key_send[8];
static int key_supp[8];
static char tracking_delimiter[256] = { "CCSD3RE00000" };
static char probe_delimiter[256] = { "PROBE" };
static int test_flag = 0;
static char speech_pipe[256];
static int delta = 60;
static int noisy_flag = 0;
static int ACTIV = 0;
static int LOAD = 0;
static int TRACK = 0;
static int ONESHOT = 0;
static int SMS_TEST = 0;

static char hostnode[256] = { "spica.physics.uiowa.edu" };
static char hosturl[256] = { "~wtr/recent_hsk.dat.vc0.bin" };


extern char *optarg;
extern int optind, opterr, optopt;

extern char *tzname[2];
extern long timezone;
extern int daylight;

static FILE *logfile = NULL;
static FILE *pidfile = NULL;
static char dsn_filename_string[256];
static char log_filename_string[256];

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
static struct DSN_RECORD *dsn_record;
static int dsn_index_count = 0;

static double f_offset = 0.0;
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

static int load_dsn_record (char *token[])
{
  char buffer[256];
  struct tm tm;
  int i;
  int k;

  dsn_record[dsn_index_count].flags = dsn_flags[0].flag;
  if (LOAD)
    fprintf (logfile, "index(%3d)\n", dsn_index_count);
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
    fprintf (logfile, "    setup(%s) ",
             hack (&dsn_record[dsn_index_count].t_setup));
    /**/ fprintf (logfile, "\n");
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
    fprintf (logfile, "    start(%s) ",
             hack (&dsn_record[dsn_index_count].t_start));
    /**/ fprintf (logfile, "\n");
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
    fprintf (logfile, "    stop(%s) ",
             hack (&dsn_record[dsn_index_count].t_stop));
    /**/ fprintf (logfile, "\n");
  }
  tm.tm_min = atoi (&token[5][2]);
  token[5][2] = 0;
  tm.tm_hour = atoi (token[5]);
  dsn_record[dsn_index_count].t_los = mktime (&tm);

/*     dsn_record[dsn_index_count].t_setup += tm.tm_gmtoff;/**/
  if (dsn_record[dsn_index_count].t_los < dsn_record[dsn_index_count].t_stop)
    dsn_record[dsn_index_count].t_los += 86400;
  if (LOAD) {
    fprintf (logfile, "    los(%s) ",
             hack (&dsn_record[dsn_index_count].t_los));
    /**/ fprintf (logfile, "\n");
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
    fprintf (logfile, "    flag(%08X) ", dsn_record[dsn_index_count].flags);
    fprintf (logfile, "\n");
    fprintf (logfile, "    text(%s) ",
             dsn_record[dsn_index_count].tracking_text);
    fprintf (logfile, "\n");
    fprintf (logfile, "\n");
    /**/ fflush (logfile);
  }
  dsn_index_count++;
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
static int load_dsn_file (FILE * dsn_file)
{
  char buffer[256];
  int i;
  int j;
  char *token[TOKEN + 1];

  while (fgets (buffer, 256, dsn_file)) {
    /*
     * fprintf(logfile, "SCAN %s", buffer); /*
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
     * fprintf(logfile, "LOAD %s", buffer); /*
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
static int dsn_tracking_file (char *dsn_file_name)
{
  static FILE *dsn_file;

  dsn_file = fopen (dsn_file_name, "r");
  dsn_index_count = 0;                  /* new copy of file */
  if (!dsn_file)
    return 0;
  dsn_record = malloc (sizeof (struct DSN_RECORD) * 8192);
  memset (dsn_record, 0, sizeof (struct DSN_RECORD) * 8192);
  if (!dsn_record) {
    fprintf (logfile, "SHIT: can't allocate memory\n");
    exit (0);
  }
  load_dsn_file (dsn_file);
  fclose (dsn_file);
  return 1;
}

 /*
  *     Return pass number, if we find it
  *     Return 0 if we're not supposed to have data
  *     Return -1 if we don't have the file...
  */
static int dsn_pass_number (int time_now)
{
  int time_setup = time_now - f_offset;
  int time_los = time_now + f_offset;
  int i;

  if (dsn_index_count) {                                /****************************/
    for (i = 0; i < dsn_index_count; i++) {     /* Scan the table we built  *//*  for where current time  */
      if ((dsn_record[i].t_setup < time_setup) &&       /*  is between setup & los */
          (time_los < dsn_record[i].t_los))
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

 /*
  *     Return 1 if we are in a tracking pass
  *     Return 0 if we're not in a tracking pass
  *     Return -1 if we don't have the file...
  */
static int dsn_tracking (int time_now, int *flags)
{
  int time_start = time_now - f_offset;
  int time_stop = time_now + f_offset;
  int i;

  if (dsn_index_count) {                                /****************************/
    for (i = 0; i < dsn_index_count; i++) {     /* Scan the table we built  *//*  for where current time  */
      if (TRACK) {
        fprintf (logfile, "TRACK %3d(%d) %08X < %08X &&  %08X < %08X\n", i,
                 (dsn_record[i].t_start < time_start) &&
                 (time_stop < dsn_record[i].t_stop),
                 dsn_record[i].t_start, time_start,
                 time_stop, dsn_record[i].t_stop);
        fflush (logfile);
      }
      if ((dsn_record[i].t_start < time_start) &&       /*  is between start & stop */
          (time_stop < dsn_record[i].t_stop))   /*  times of some tracking  */
        if (flags)
          flags[0] = dsn_record[i].flags;
      return 1;                         /*  pass we know about      */

    }                                                   /****************************/
    return 0;                           /* nothing found, skip it */
  } else {
    return -1;
  }
}

 /*
  *     Return 1 if we are in a tracking pass
  *     Return 0 if we're not in a tracking pass
  *     Return -1 if we don't have the file...
  */
static int dsn_active (int time_now, int *flags)
{
  int time_setup = time_now - f_offset;
  int time_los = time_now + f_offset;
  int i;

  if (dsn_index_count) {                                /****************************/
    for (i = 0; i < dsn_index_count; i++) {     /* Scan the table we built  *//*  for where current time  */
      if (ACTIV) {
        fprintf (logfile, "ACTIV %3d (%d)\n", i,
                 (dsn_record[i].t_setup < time_setup) &&
                 (time_los < dsn_record[i].t_los)
          );
        fprintf (logfile, "    t_setup    %08X(%s)\n",
                 dsn_record[i].t_setup, hack (&dsn_record[i].t_setup));
        fprintf (logfile, "    time_setup %08X(%s)\n",
                 time_setup, hack ((time_t *) & time_setup));
        fprintf (logfile, "    time_los   %08X(%s)\n",
                 time_los, hack ((time_t *) & time_los));
        fprintf (logfile, "    t_los      %08X(%s)\n",
                 dsn_record[i].t_los, hack (&dsn_record[i].t_los));
        fprintf (logfile, "\n");
        fflush (logfile);
      }
      if ((dsn_record[i].t_setup < time_setup) &&       /*  is between setup & los */
          (time_los < dsn_record[i].t_los))     /*  times of some tracking  */
        if (flags)
          flags[0] = dsn_record[i].flags;
      return 1;                         /*  pass we know about      */

                                                        /****************************/
    }
    return 0;                           /* nothing found, skip it */
  } else {
    return -1;
  }
}

 /*
  *
  */
int flag_scan (int key_send, int key_supp, int flags)
{
  if (key_supp & flags)                 /* SUPPRESS flags, if set */
    return 0;                           /* take priority */

  if (key_send & flags)                 /* any enable flagsset ?? */
    return 1;

  if (key_send)                         /* any enable flags left over? */
    return 0;                           /* if so, no traffic */

  return 1;                             /* otherwise... */
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
           sms_text,
           tm->tm_yday + flag_yday, tm->tm_hour, tm->tm_min, msg_text);
  return outbuf;
}

char *mail_text (char *mail_text, char *msg_text, time_t ut_time)
{
  static char temp[1024];
  static char outbuf[1024];
  struct tm *tm;

  tm = localtime (&ut_time);
  sprintf (temp, "\"CAS %03d-%02d:%02d %s\"",
           tm->tm_yday + flag_yday, tm->tm_hour, tm->tm_min, msg_text);
  sprintf (outbuf, mail_text, temp);
  return outbuf;
}

 /*
  *     Send out commands (local, SMS (cellphone), speech)
  */
int announce_commands (char *tag)
{
  int count;
  char *buf;
  char *temp;
  int i;
  char str[32];
  FILE *speech = NULL;

  fprintf (logfile, "%s\n", tag);
  for (i = 0; i < 8; i++) {
    sprintf (str, "ANNOUNCE");
    if (i)
      sprintf (str, "ANNOUNCE%d", i);
    if (!strcmp (tag, str)) {
      buf = ann_flag[i];
      break;
    }
  }
  if (speech_pipe[0])
    speech = fopen (speech_pipe, "w");
  if (speech) {
    fprintf (logfile, "ANN\n");
    /**/ while (buf[0]) {
      count++;
      fprintf (speech, "%s\n", buf);
      fprintf (logfile, "    %s\n", buf);
      buf += strlen (buf) + 1;
    }
    fclose (speech);
  }
  fflush (logfile);
  return count;
}
int cmd_commands (char *tag)
{
  int count;
  char *buf;
  char *temp;
  int i;
  char str[32];

  fprintf (logfile, "%s\n", tag);
  for (i = 0; i < 8; i++) {
    sprintf (str, "CMD");
    if (i)
      sprintf (str, "CMD%d", i);
    if (!strcmp (tag, str)) {
      buf = cmd_flag[i];
      break;
    }
  }
  while (buf[0]) {
    count++;
    fprintf (logfile, "    %s\n", buf);
    /**/

#ifdef ONLINE
      system (buf);
#endif

    buf += strlen (buf) + 1;
  }
  fflush (logfile);
  return count;
}


int sms_commands (char *tag, char *text, time_t ut_time)
{
  int count;
  char *buf;
  char *temp;
  int i;
  char str[32];

  fprintf (logfile, "%s\n", tag);
  for (i = 0; i < 8; i++) {
    sprintf (str, "SMS");
    if (i)
      sprintf (str, "SMS%d", i);
    if (!strcmp (tag, str)) {
      buf = sms_flag[i];
      break;
    }
  }
  while (buf[0]) {
    count++;
    temp = message_text (buf, text, ut_time);
    fprintf (logfile, "    %s\n", temp);
    /**/

#ifdef ONLINE
      system (temp);
#endif

    buf += strlen (buf) + 1;
  }
  fflush (logfile);
  return count;
}

 /*
  *     Send out mail
  */
int mail_commands (char *tag, char *text, time_t ut_time)
{
  int count;
  char *buf;
  char *temp;
  int i;
  char str[32];

  fprintf (logfile, "%s\n", tag);
  for (i = 0; i < 8; i++) {
    sprintf (str, "MAIL");
    if (i)
      sprintf (str, "MAIL%d", i);
    if (!strcmp (tag, str))
      buf = mail_flag[i];
  }
  while (buf[0]) {
    count++;
    temp = mail_text (buf, text, ut_time);
    fprintf (logfile, "    %s\n", temp);
    /**/

#ifdef ONLINE
      system (temp);
#endif

    buf += strlen (buf) + 1;
  }
  fflush (logfile);
  return count;
}

int mail_alarms (char *tag, char *text, time_t ut_time)
{
  int count;
  char *buf;
  static char temp[65536];
  int i;
  char str[32];

  fprintf (logfile, "%s\n", tag);
  for (i = 0; i < 8; i++) {
    sprintf (str, "MAIL");
    if (i)
      sprintf (str, "MAIL%d", i);
    if (!strcmp (tag, str))
      buf = mail_flag[i];
  }
  while (buf[0]) {
    count++;
    sprintf (temp, buf, text);
    /**/ fprintf (logfile, "    %s", temp);

#ifdef ONLINE
    system (temp);
#endif

    buf += strlen (buf) + 1;
  }
  fflush (logfile);
  return count;
}


int main2 (struct RPWS_buffer *rbuf[], time_t ut_time, FILE * debug_file,
           int tracking, int previous_tracking)
{
  static char result[16384];
  static int good_data = 1;
  int i;
  int minutes;
  int length;
  struct tm pkt_transit;
  struct tm pkt_ut;
  struct tm pkt_lcl;
  struct tm *pkt_ert;
  struct tm *pkt_scet;
  struct event_time scet_tim;
  struct event_time ert_tim;
  time_t cds_time;
  char *text;
  char pkt_scet_string[128];
  char pkt_sclk_string[128];
  int WIDTH = 16;
  struct RPWS_buffer *buf;


  if (debug_file)
    fprintf (debug_file, "Request:\n");
  buf = dsp_web_fetch (hostnode, hosturl, result, &length, debug_file);
  rbuf[0] = buf;
  if (!buf)
    return -1;

  if (result_flag) {
    fprintf (logfile, "Response:\n");
    fprintf (logfile, "(%d) %s", length, result);
    if (length != 768)
      fprintf (logfile, "%s\n", buf);
    fflush (logfile);
  }
  if (length != 768)
    return -2;

  if (dump_flag) {
    fprintf (logfile, "f_length %08X  record_type %08X   status %08X\n",
             UTIL_MSB_to_long (buf->f_length),
             UTIL_MSB_to_long (buf->record_type),
             UTIL_MSB_to_long (buf->status));
    for (i = 0; i < 256; i++) {
      if (i % WIDTH == 0)
        fprintf (logfile, "%04X: ", i);
      fprintf (logfile, "%02X ", buf->packet.filler[i] & 0xFF);
      if (i % 4 == 3)
        fprintf (logfile, " ");
      if (i % WIDTH == WIDTH - 1)
        fprintf (logfile, "\n");
    }
    fprintf (logfile, "\n");
    for (i = 0; i < 192; i++) {
      if (i % WIDTH == 0)
        fprintf (logfile, "%04X: ", i);
      fprintf (logfile, "%02X ", buf->packet.mp.mini_packet[i] & 0xFF);
      if (i % WIDTH == WIDTH - 1)
        fprintf (logfile, "\n");
    }
    fflush (logfile);
  }

  memcpy (&pkt_ut, gmtime (&ut_time), sizeof (struct tm));
  memcpy (&pkt_lcl, localtime (&ut_time), sizeof (struct tm));

  sprintf (pkt_sclk_string, "SCLK: %04X%04X:%d %s",
           get_status (buf->packet.mpp.mini_packet, CDS_Time_Bits_23_16,
                       CDS_Time_Bits_31_24),
           get_status (buf->packet.mpp.mini_packet, CDS_Time_Bits_07_00,
                       CDS_Time_Bits_15_08),
           get_status (buf->packet.mpp.mini_packet, CDS_Time_Bits_RTI, 0),
           get_status (buf->packet.mpp.mini_packet, CDS_Time_Bits_TQF,
                       0) ? "TQF" : "");

  scet_tim.days = buf->packet.chdo_tag.scet.days;
  scet_tim.milliseconds = buf->packet.chdo_tag.scet.milliseconds;
  pkt_scet = Util_event_scet_tm (scet_tim, 0);
  sprintf (pkt_scet_string, "SCET: %04d-%03dT%2.2d:%2.2d:%2.2d.%3.3d ",
           pkt_scet->tm_year + 1900,
           pkt_scet->tm_yday + flag_yday,
           pkt_scet->tm_hour,
           pkt_scet->tm_min, pkt_scet->tm_sec, scet_tim.milliseconds % 1000);

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
        fprintf (logfile, "  Tracking  %s\n", text);
    } else
      fprintf (logfile, "  Not tracking\n");

    fprintf (logfile, "  UT:    %04d-%03dT%2.2d:%2.2d:%2.2d ",
             pkt_ut.tm_year + 1900,
             pkt_ut.tm_yday + flag_yday,
             pkt_ut.tm_hour, pkt_ut.tm_min, pkt_ut.tm_sec);
    fprintf (logfile, "(-%02d:%02d:%02d) ",
             pkt_transit.tm_hour, pkt_transit.tm_min, pkt_transit.tm_sec);
    fprintf (logfile, "\n");
    fprintf (logfile, " ERT:    %04d-%03dT%2.2d:%2.2d:%2.2d.%3.3d ",
             pkt_ert->tm_year + 1900,
             pkt_ert->tm_yday + flag_yday,
             pkt_ert->tm_hour,
             pkt_ert->tm_min, pkt_ert->tm_sec, ert_tim.milliseconds % 1000);
    fprintf (logfile, " SCET:    %04d-%03dT%2.2d:%2.2d:%2.2d.%3.3d ",
             pkt_scet->tm_year + 1900,
             pkt_scet->tm_yday + flag_yday,
             pkt_scet->tm_hour,
             pkt_scet->tm_min,
             pkt_scet->tm_sec, scet_tim.milliseconds % 1000);

    minutes = pkt_transit.tm_hour * 60 + pkt_transit.tm_min;
    fprintf (logfile, "(-%d.%d)", minutes, pkt_transit.tm_sec / 6);
    fprintf (logfile, "\n");
    if (minutes > timeout)
      fprintf (logfile, "         NO DATA for %.1f Hours\n", minutes / 60.0);
    fflush (logfile);
  }

  if (!test_flag) {
    if (!tracking)
      return 0;
  }

  if (!test_flag) {

    if (minutes < timeout) {    /*******************************/
      if (!good_data) {                 /* last time through, no data  *//* so put their minds at ease  */
        char *stemp;                    /* 'cause we're getting it now */

        char temp[256];         /*******************************/

        stemp = dsn_tracking_text (ut_time);
        sprintf (temp, "Receiving Data, Pass %d\n%s",
                 dsn_pass_number (ut_time), stemp);
        sms_commands ("SMS", temp, ut_time);
        mail_commands ("MAIL", temp, ut_time);
      }
      good_data = 1;
    } else {
      /*
       * Now, check red & yellow alarms
       */
      text = NULL;
      if (dsn_pass_number (ut_time))    /* really a pass ??? */
        text =
          dsp_monitor (pkt_sclk_string, pkt_scet_string,
                       buf->packet.mpp.mini_packet, 0);
      if (text) {
        if (text[0]) {
          char temp[256];

          sprintf (temp, "Alarm Condition %s", dsn_tracking_text (ut_time));
          sms_commands ("SMS3", temp, ut_time);
          mail_alarms ("MAIL3", text + 1, ut_time);
          cmd_commands ("CMD3");
          announce_commands ("ANNOUNCE3");
          if (text[0] & FLAG_RED) {
            sprintf (temp, "Red Alarm Condition %s",
                     dsn_tracking_text (ut_time));
            sms_commands ("SMS4", temp, ut_time);
            mail_alarms ("MAIL4", text + 1, ut_time);
            cmd_commands ("CMD3");
            announce_commands ("ANNOUNCE3");
          }
          if (text[0] & FLAG_YELLOW) {
            sprintf (temp, "Yellow Alarm Condition %s",
                     dsn_tracking_text (ut_time));
            sms_commands ("SMS5", temp, ut_time);
            mail_alarms ("MAIL5", text + 1, ut_time);
            cmd_commands ("CMD4");
            announce_commands ("ANNOUNCE4");
          }
        }
      }
    }
  }

  /*
   * Here is where we let the outside world know
   * that things are amiss...
   */
  if (                                  /* (!previous_tracking) && /* */
       (tracking) && dsn_pass_number (ut_time)) {
    char temp[256];
    char *stemp = NULL;

    if (good_data)
      stemp = dsn_tracking_text (ut_time);
    if (stemp) {
      if (stemp[0]) {
        sprintf (temp, "No Data, Pass %d\n%s",
                 dsn_pass_number (ut_time), stemp);
        sms_commands ("SMS", temp, ut_time);
        mail_commands ("MAIL", temp, ut_time);
        cmd_commands ("CMD");
        announce_commands ("ANNOUNCE");
      }
    }
    good_data = 0;
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
{                                       /* text=/path/sms_vendor.py number # comment text */
  char *temp = buf;
  char *xxx;

  if (xxx = strchr (text, '#'))
    *xxx = 0;
  strcat (temp, text);
  temp += strlen (text);
  temp[0] = 0;
  temp[1] = 0;
  *temp++;
  return temp;
}

  /*
   * struct DSN_FLAGS {
   * *                  char *mne;
   * *                  int flag;
   * *              } dsn_flags[] = {
   */
int config_mask (char *text)
{                                       /* KEY_x=MNE */
  int i = 1;
  int max;

  while (i < 32) {
    if (!strcmp (dsn_flags[i].mne, text)) {     /* existing key? */
      return dsn_flags[i].flag;         /* return it's flag */
    }
    if (!dsn_flags[i].flag) {           /* new key */
      max = sizeof (dsn_flags[0].mne);
      strncpy (dsn_flags[i].mne, text, max);
      dsn_flags[i].flag = dsn_flags[i - 1].flag << 1;
      return dsn_flags[i].flag;
    }
    i++;
  }
  return 0;
}
char *configure (char *filename)
{
  FILE *config_file;
  char *temp;
  char *token[2];
  char *sms[8];
  char *mail[8];
  char *cmd[8];
  char *ann[8];
  int i;
  char str[32];

  char detail[256];

  for (i = 0; i < 8; i++) {
    sms[i] = sms_flag[i];
    mail[i] = mail_flag[i];
    cmd[i] = cmd_flag[i];
    ann[i] = ann_flag[i];
  }

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
        if (!strcmp (token[0], "DSN_FILE"))
          strcpy (dsn_filename_string, token[1]);
        if (!strcmp (token[0], "OFFSET"))
          f_offset = atoi (token[1]);
        if (!strcmp (token[0], "DELTA"))
          delta = atoi (token[1]);
        if (!strcmp (token[0], "LOGFILE")) {
          if (logfile)
            fclose (logfile);
          strcpy (log_filename_string, token[1]);
          logfile = fopen (log_filename_string, "a");
          if (!logfile) {
            fprintf (stderr, "Unable to open log file %s\n", optarg);
            logfile = stdout;
          }
        }
        if (!strcmp (token[0], "HOSTNODE"))
          strcpy (hostnode, token[1]);
        if (!strcmp (token[0], "HOSTURL"))
          strcpy (hosturl, token[1]);
        if (!strcmp (token[0], "DSN_DELIMITER"))
          strcpy (tracking_delimiter, token[1]);
        if (!strcmp (token[0], "PROBE_DELIMITER"))
          strcpy (probe_delimiter, token[1]);
        if (!strcmp (token[0], "ANNOUNCE_PIPE"))
          strcpy (speech_pipe, token[1]);
        for (i = 0; i < 8; i++) {
          sprintf (str, "SMS");
          if (i)
            sprintf (str, "SMS%d", i);
          if (!strcmp (token[0], str))
            sms[i] = config_append (sms[i], token[1]);
          sprintf (str, "MAIL");
          if (i)
            sprintf (str, "MAIL%d", i);
          if (!strcmp (token[0], str))
            mail[i] = config_append (mail[i], token[1]);
          sprintf (str, "CMD");
          if (i)
            sprintf (str, "CMD%d", i);
          if (!strcmp (token[0], str))
            cmd[i] = config_append (cmd[i], token[1]);
          sprintf (str, "ANNOUNCE");
          if (i)
            sprintf (str, "ANNOUNCE%d", i);
          if (!strcmp (token[0], str))
            ann[i] = config_append (ann[i], token[1]);

          sprintf (str, "KEY_SEND");
          if (i)
            sprintf (str, "KEY_SEND%d", i);
          if (!strcmp (token[0], str))
            key_send[i] |= config_mask (token[1]);
          sprintf (str, "KEY_SUPP");
          if (i)
            sprintf (str, "KEY_SUPP%d", i);
          if (!strcmp (token[0], str))
            key_supp[i] |= config_mask (token[1]);
        }
      }
    }
  }
  fclose (config_file);
  return dsn_filename_string;
}
static int dump_config (FILE * loffile, time_t ut_time)
{
  int i;

  fprintf (loffile, "CONFIG-dump_config %08X\n", ut_time);
  fprintf (loffile, "CONFIG DSN_FILE=%s\n", dsn_filename_string);
  fprintf (loffile, "CONFIG OFFSET=%d\n", f_offset);
  fprintf (loffile, "CONFIG DELTA=%d\n", delta);
  fprintf (loffile, "CONFIG LOGFILE=%s\n", log_filename_string);
  fprintf (loffile, "CONFIG HOSTNODE=%s\n", hostnode);
  fprintf (loffile, "CONFIG HOSTURL=%s\n", hosturl);
  fprintf (loffile, "CONFIG DSN_DELIMITER=%s\n", tracking_delimiter);
  fprintf (loffile, "CONFIG PROBE_DELIMITER=%s\n", probe_delimiter);
  fprintf (loffile, "CONFIG ANNOUNCE_PIPE=%s\n", speech_pipe);
  for (i = 0; i < 8; i++)
    if (cmd_flag[i])
      if (cmd_flag[i][0])
        fprintf (loffile, "CONFIG SMS%d=%s\n", i, cmd_flag[i]);
  for (i = 0; i < 8; i++)
    if (ann_flag[i])
      if (ann_flag[i][0])
        fprintf (loffile, "CONFIG SMS%d=%s\n", i, ann_flag[i]);
  for (i = 0; i < 8; i++)
    if (sms_flag[i])
      if (sms_flag[i][0])
        fprintf (loffile, "CONFIG SMS%d=%s\n", i, sms_flag[i]);
  for (i = 0; i < 8; i++)
    if (mail_flag[i])
      if (mail_flag[i][0])
        fprintf (loffile, "CONFIG MAIL%d=%s\n", i, mail_flag[i]);
  return 0;
}
int pid_data (FILE * pidfile, struct tm *tm, char *version, char *arg)
{
  fprintf (pidfile, "%d  PID=%d  PPID=%d  SID=%d  ",
           getpid (), getpid (), getsid (0), getppid ());
  if (tm)
    fprintf (pidfile, "%04d-%02d-%02dT%02d:%02d:%02dZ  ",
             tm->tm_year + 1900,
             tm->tm_mon + 1,
             tm->tm_mday, tm->tm_hour, tm->tm_min, tm->tm_sec);
  if (version)
    fprintf (pidfile, "Ver=%s  ", version);
  if (arg)
    fprintf (pidfile, "Pgm=%s", arg);
  fprintf (pidfile, "\n");
  fflush (pidfile);
  return 0;
}
int main (int argc, char *argv[])
{
  int active, tracking;
  int previous_active = 0, previous_tracking = 0;
  int i;
  int length;
  int loop_active = 1;
  char c;
  char *text;
  int flags[10];
  FILE *debug_file = NULL;
  char config_filename[256];
  static char result[16384];
  char *dsn_tracking_filename;
  struct RPWS_buffer *buf;
  struct RPWS_buffer test_buf;
  time_t ut_time;
  struct tm *tm;

  logfile = stdout;

  /*
   * Allocate Memory
   */
  for (i = 0; i < 8; i++) {
    cmd_flag[i] = malloc (65536);
    ann_flag[i] = malloc (65536);
    sms_flag[i] = malloc (65536);
    mail_flag[i] = malloc (65536);
    if (!sms_flag[i] || !mail_flag[i]) {
      fprintf (stderr, "memory allocation failed sms(%p) mail(%p)\n",
               sms_flag[i], mail_flag[i]);
    }
  }
  /*
   * Flip time zone to UTC
   */
  putenv ("TZ=UTC");
  tzset ();
  ut_time = time (NULL);
  tm = localtime (&ut_time);

  while ((c = getopt (argc, argv, "Ac:Dd:F:hH:Ll:nOop:rSTt:U:?")) != EOF) {
    switch (c) {
     case 'A':
       ACTIV = 1;
       break;
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
       f_offset = atof (optarg);
       break;
     case 'L':
       LOAD = 1;
       break;
     case 'l':
       logfile = fopen (optarg, "a");
       if (!logfile) {
         fprintf (stderr, "Unable to open log file %s\n", optarg);
         logfile = stdout;
       }
       break;
     case 'n':
       noisy_flag = 1;
       break;
     case 'O':
       ONESHOT = 1;
       break;
     case 'o':
       offset_flag = 1;
       break;
     case 'p':
       pidfile = fopen (optarg, "w");
       if (pidfile) {
         pid_data (pidfile, tm, Version, argv[0]);
         fclose (pidfile);
       } else {
         fprintf (stderr, "Unable to open pid file %s\n", optarg);
         pidfile = logfile;
       }
       break;
     case 'r':
       debug_file = logfile;
       result_flag = 1;
       break;
     case 'S':
       SMS_TEST = 1;
       ONESHOT = 1;
       LOAD = 1;
       TRACK = 1;
       ACTIV = 1;
       break;
     case 't':
       timeout = atol (optarg);
       break;
     case 'T':
       TRACK = 1;
       break;
     case 'H':
       strcpy (hostnode, optarg);
       break;
     case 'U':
       strcpy (hosturl, optarg);
       break;
     case 'h':                         /* help */
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
       fprintf (stdout,
                "                (dumps configuration information)\n");
       fprintf (stdout, "      -H host Host name\n");
       fprintf (stdout, "      -U url  URL\n");
       fprintf (stdout, "      -A      set ACTIV flag\n");
       fprintf (stdout, "      -L      set LOAD flag\n");
       fprintf (stdout, "      -l xx   logfile\n");
       fprintf (stdout, "      -O      set ONESHOT flag\n");
       fprintf (stdout, "      -p xx   PID file\n");
       fprintf (stdout, "      -T      set TRACK flag\n");
       fprintf (stdout, "      -S      set SMS_TEST flag (and -ALTO\n");
       fprintf (stdout, "\n");
       fprintf (stdout, "      -F n    Time offset from start/stop\n");
       fprintf (stdout, "               (this will suppress notification\n");
       fprintf (stdout,
                "                messages when not tracking, -F moves\n");
       fprintf (stdout,
                "                start/stop time this many seconds\n");
       fprintf (stdout, "\n");
       fprintf (stdout, "Time Handling:\n");
       fprintf (stdout,
                "      The DSN_EIGHT_WEEK_SCHEDULE file is expressed in\n");
       fprintf (stdout,
                "    UTC, so we tell the OS to operate in UTC.  This is\n");
       fprintf (stdout,
                "    to allow the conversion from a time string (text) \n");
       fprintf (stdout,
                "    to UNIX time (time_t).  Anything we emit will, therfore,\n");
       fprintf (stdout, "    be shown in terms of UTC.\n");
       fprintf (stdout, "    \n");
       fprintf (stdout, "  Config file:\n");
       fprintf (stdout,
                "          The configuration file has all of the following\n");
       fprintf (stdout,
                "        configuration parameters.  It is read again with\n");
       fprintf (stdout,
                "        each pass of the program (i.e. every -o seconds).\n");
       fprintf (stdout,
                "        Since this file is read each time its contents are\n");
       fprintf (stdout, "        used, you may update it at any time.\n");
       fprintf (stdout, "        \n");
       fprintf (stdout, "    Command Line Argument Override:\n");
       fprintf (stdout,
                "              These command line parameters may also\n");
       fprintf (stdout,
                "            be changed in to configuration file (this\n");
       fprintf (stdout,
                "            allows them to be changed on-the-fly, without\n");
       fprintf (stdout, "            having to restart.\n");
       fprintf (stdout, "            \n");
       fprintf (stdout,
                "        DELTA=n     delta-t between checks (seconds)\n");
       fprintf (stdout, "        OFFSET=n    offset from start/stop time\n");
       fprintf (stdout, "                      to check fo data.\n");
       fprintf (stdout, "        LOGFILE=x   logfile name\n");
       fprintf (stdout, "        HOSTNODE=x  Hostname (where most recent\n");
       fprintf (stdout,
                "                      housekeeping record resides\n");
       fprintf (stdout, "        HOSTURL=x   Remainder of URL that points\n");
       fprintf (stdout,
                "                      to the most recent HSK record.\n");
       fprintf (stdout, "        \n");
       fprintf (stdout, "    Tracking File:\n");
       fprintf (stdout,
                "              DATA_SET_ID = DSN_EIGHT_WEEK_SCHEDULE;\n");
       fprintf (stdout, "            (get this from the SOPC)\n");
       fprintf (stdout,
                "            DSN_DELIMITER is text from the DSN_FILE\n");
       fprintf (stdout, "            wher we start loading data\n");
       fprintf (stdout,
                "              As with the configuration file, the tracking\n");
       fprintf (stdout,
                "            file is re-read each time it is used.  Again, the\n");
       fprintf (stdout,
                "            file can be updated as needed without restarting\n");
       fprintf (stdout, "            the monitoring program.\n");
       fprintf (stdout, "        DSN_FILE=<filename>\n");
       fprintf (stdout,
                "              The DSN_DELIMITER is the begining of a (unique)\n");
       fprintf (stdout,
                "            string that appears in the triacking file just\n");
       fprintf (stdout,
                "            prior to the tracking data.  This allows the use\n");
       fprintf (stdout,
                "            of an unedited file (directly from the SOPC).\n");
       fprintf (stdout, "        DSN_DELIMITER=<%s>\n", tracking_delimiter);
       fprintf (stdout,
                "              Some of the tracking passes are for the exclusive use\n");
       fprintf (stdout,
                "            of Huygens Probe data.  We don't get telemetry (as we\n");
       fprintf (stdout,
                "            have been muted during probe activities).  We will\n");
       fprintf (stdout,
                "            ignore the lines containning this text string so we\n");
       fprintf (stdout, "            don't send false alarms.\n");
       fprintf (stdout, "        PROBE_DELIMITER=<%s>\n", probe_delimiter);
       fprintf (stdout, "            \n");
       fprintf (stdout, "    Cell/Pager notification:\n");
       fprintf (stdout,
                "              This set of entries are used to send\n");
       fprintf (stdout, "            an SMS message to a pager or phone.\n");
       fprintf (stdout, "            Command line (i.e. script to contact\n");
       fprintf (stdout, "            a specific provider along with the\n");
       fprintf (stdout, "            pager or cell number)\n");
       fprintf (stdout,
                "              In the typical case, the specified script\n");
       fprintf (stdout,
                "            (<command>) expects a phone number and some\n");
       fprintf (stdout,
                "            text to send.  INternally, dsph strips comments\n");
       fprintf (stdout,
                "            (using the # delimiter) and appends the comment\n");
       fprintf (stdout, "            text to whatever remains.\n");
       fprintf (stdout,
                "              When data is lost, a \"No Data\" message is\n");
       fprintf (stdout,
                "            sent.  No additional messages will be sent (as\n");
       fprintf (stdout,
                "            this could generate many messages) until data is\n");
       fprintf (stdout,
                "            again received.  If/when the data feed is resotred\n");
       fprintf (stdout,
                "            a message indicating this will again be sent.  \n");
       fprintf (stdout,
                "            Subsequent data loss will cause another message \n");
       fprintf (stdout, "            to be sent.\n");
       fprintf (stdout, "            \n");
       fprintf (stdout, "        SMS=<command> <number>  # Data Loss\n");
       fprintf (stdout, "        SMS1=<command> <number> # Start of Track\n");
       fprintf (stdout, "        SMS2=<command> <number> # End of Track\n");
       fprintf (stdout,
                "        SMS3=<command> <number> # All Alarm Condition\n");
       fprintf (stdout,
                "        SMS4=<command> <number> # RED Alarm Condition\n");
       fprintf (stdout,
                "        SMS5=<command> <number> # YELLOW Alarm Condition\n");
       fprintf (stdout, "        SMS6=<command> <number> # \n");
       fprintf (stdout, "        SMS7=<command> <number> # \n");
       fprintf (stdout, "            \n");
       fprintf (stdout, "    Mail notification:\n");
       fprintf (stdout,
                "              This set of entries are used to send\n");
       fprintf (stdout, "            email to selected users.\n");
       fprintf (stdout,
                "            This is similar to local notification\n");
       fprintf (stdout,
                "            but the notification text is inserted using\n");
       fprintf (stdout, "            a %s anywhere in the parameters.\n",
                "%s");
       fprintf (stdout,
                "              When data is lost, a \"No Data\" message is\n");
       fprintf (stdout,
                "            sent.  No additional messages will be sent (as\n");
       fprintf (stdout,
                "            this could generate many messages) until data is\n");
       fprintf (stdout,
                "            again received.  If/when the data feed is resotred\n");
       fprintf (stdout,
                "            a message indicating this will again be sent.  \n");
       fprintf (stdout,
                "            Subsequent data loss will cause another message \n");
       fprintf (stdout, "            to be sent.\n");
       fprintf (stdout, "            \n");
       fprintf (stdout, "        MAIL=<command> <parameters>  # Data Loss\n");
       fprintf (stdout,
                "        MAIL1=<command> <parameters> # Start of Track\n");
       fprintf (stdout,
                "        MAIL2=<command> <parameters> # End of Track\n");
       fprintf (stdout,
                "        MAIL3=<command> <parameters> # All Alarm Condition\n");
       fprintf (stdout,
                "        MAIL4=<command> <parameters> # RED Alarm Condition\n");
       fprintf (stdout,
                "        MAIL5=<command> <parameters> # YELLOW Alarm Condition\n");
       fprintf (stdout, "        MAIL6=<command> <parameters> # \n");
       fprintf (stdout, "        MAIL7=<command> <parameters> # \n");
       fprintf (stdout, "            \n");
       fprintf (stdout, "   Local notification:\n");
       fprintf (stdout, "              This is intended to allow lights\n");
       fprintf (stdout,
                "            or alarms to be sent (i.e. X-10 command\n");
       fprintf (stdout, "            to turn radio on, for example).\n");
       fprintf (stdout, "              As with SMS commands, the comment\n");
       fprintf (stdout,
                "            delimiter is the # character.  The command\n");
       fprintf (stdout,
                "            is deliverd as supplied, after comment text\n");
       fprintf (stdout, "            has been removed.\n");
       fprintf (stdout,
                "              When data is lost, the command script is\n");
       fprintf (stdout,
                "            delivered only once.  Subsequent data events\n");
       fprintf (stdout, "            are not reported.\n");
       fprintf (stdout, "        CMD=<command_string>         # Data Loss\n");
       fprintf (stdout,
                "        CMD1=<command_string>        # Start of Track\n");
       fprintf (stdout,
                "        CMD2=<command_string>        # End of Track\n");
       fprintf (stdout,
                "        CMD3=<command_string>        # All Alarm Condition\n");
       fprintf (stdout,
                "        CMD4=<command_string>        # RED Alarm Condition\n");
       fprintf (stdout,
                "        CMD5=<command_string>        # YELLOW Alarm Condition\n");
       fprintf (stdout, "        CMD6=<command_string>        # \n");
       fprintf (stdout, "        CMD7=<command_string>        # \n");
       fprintf (stdout, "            \n");
       fprintf (stdout, "   Verbal notification:\n");
       fprintf (stdout,
                "              Verbal notification.  The text provided\n");
       fprintf (stdout,
                "            on the ANNOUNCE lines are fed to the file\n");
       fprintf (stdout,
                "            (i.e. pipe) specified in ANNOUNCE_PIPE.\n");
       fprintf (stdout, "             A speech synthesis program, such as\n");
       fprintf (stdout, "            Festival would be waiting for speech\n");
       fprintf (stdout, "            text on the other end of the pipe.\n");
       fprintf (stdout,
                "              As with SMS and CMD commands, the comment\n");
       fprintf (stdout,
                "            delimiter is the # character.  The speech text\n");
       fprintf (stdout,
                "            is deliverd as supplied, after comment text\n");
       fprintf (stdout, "            has been removed.\n");
       fprintf (stdout,
                "              When data is lost, the announce text is\n");
       fprintf (stdout,
                "            delivered only once.  Subsequent data events\n");
       fprintf (stdout, "            are not reported.\n");
       fprintf (stdout, "        ANNOUNCE=<text>              # Data loss\n");
       fprintf (stdout,
                "        ANNOUNCE1=<text>             # Start of Track\n");
       fprintf (stdout,
                "        ANNOUNCE2=<text>             # End of Track\n");
       fprintf (stdout,
                "        ANNOUNCE3=<text>             # All Alarm Condition\n");
       fprintf (stdout,
                "        ANNOUNCE4=<text>             # RED Alarm Condition\n");
       fprintf (stdout,
                "        ANNOUNCE5=<text>             # YELLOW Alarm Condition\n");
       fprintf (stdout, "        ANNOUNCE6=<text>             # \n");
       fprintf (stdout, "        ANNOUNCE7=<text>             # \n");
       fprintf (stdout, "        ANNOUNCE_PIPE=<filename>\n");
       fprintf (stdout, "\n");
       fprintf (stdout, "Cellphone & Pager messages:\n");
       fprintf (stdout,
                "      The format of the message sent to pager and cellphone\n");
       fprintf (stdout,
                "    is rather trivial.  Format is similar for all messages.\n");
       fprintf (stdout, "\n");
       fprintf (stdout, "  All Messages\n");
       fprintf (stdout,
                "        All messages are printed with the following:\n");
       fprintf (stdout, "            sprintf(temp,\"CAS %s\n",
                "%03d-%02d:%02d %s\",");
       /**/ fprintf (stdout, "                    day-of-year,\n");
       fprintf (stdout, "                    hour,\n");
       fprintf (stdout, "                    minute,\n");
       fprintf (stdout, "                    variable_text);\n");
       fprintf (stdout,
                "      This results in a Cassin identifier and a UTC time\n");
       fprintf (stdout,
                "      string appearing at the begining of all messages\n");
       fprintf (stdout,
                "      generated by this program. the \"variable_text\" is\n");
       fprintf (stdout, "      unique to each message.\n");
       fprintf (stdout, "\n");
       fprintf (stdout, "  Tracking (sent to SMS1 recipients)\n");
       fprintf (stdout, "        \"variable_text\" contains the string:\n");
       fprintf (stdout,
                "      \"Tracking Setup\".  This is nominally about\n");
       fprintf (stdout,
                "      60 minutes prior to expected data acquisition.\n");
       fprintf (stdout, "\n");
       fprintf (stdout, "  Data Collection (sent to SMS recipients)\n");
       fprintf (stdout, "        \"variable_text\" contains the string:\n");
       fprintf (stdout, "      \"No Data \" followed by some text that\n");
       fprintf (stdout, "      is taken from the DSN_EIGHT_WEEK_SCHEDULE.\n");
       fprintf (stdout, "      This usually indicates the specific disk\n");
       fprintf (stdout,
                "      that is being used to track the spacecraft.\n");
       fprintf (stdout, "\n");
       fprintf (stdout, "  Teardown (sent to SMS2 recipients)\n");
       fprintf (stdout, "        \"variable_text\" contains the string:\n");
       fprintf (stdout,
                "      \"Tracking Teardown\".  This message will occur\n");
       fprintf (stdout, "      at expected loss-of-signal.\n");
       fprintf (stdout, "\n");
       fprintf (stdout, "  Testinging (-S flag)\n");
       fprintf (stdout, "        \"variable_text\" contains the strings:\n");
       fprintf (stdout, "      \"Test Message to SMS NO Data\" sent to \n");
       fprintf (stdout, "              SMS recipients\n");
       fprintf (stdout,
                "      \"Test Message to SMS1 Begin Tracking\" sent to\n");
       fprintf (stdout, "              SMS1 recipients\n");
       fprintf (stdout,
                "      \"Test Message to SMS2 End of Tracking\" sent to\n");
       fprintf (stdout, "              SMS2 recipients\n");
       fprintf (stdout, "\n");
       fprintf (stdout,
                "  Alarm Conditions (sent to SMS3/MAIL3 recipients)\n");
       fprintf (stdout,
                "        \"variable_text\" contains a list of RED and YELLOW\n");
       fprintf (stdout,
                "      alarms that appear in the housekeeping data.\n");
       fprintf (stdout,
                "      SMS3 recipients receive notification only as there\n");
       fprintf (stdout,
                "      is not enough room in an SMS message to hold\n");
       fprintf (stdout, "      the body of this mail message.\n");
       fprintf (stdout, "\n");
       fprintf (stdout, "  Log File\n");
       fprintf (stdout, "        \"NO DATA for %s hours\n", "%.1f");
       fprintf (stdout, "        \"NO Net Data\n");
       fprintf (stdout, "        \"NO Net Connection\n");
       fprintf (stdout, "\n");
       fprintf (stdout, "        \n");
       fprintf (stdout, "      \n");
       fprintf (stdout, "\n");
       fprintf (stdout, "  PID File\n");
       fprintf (stdout, "\n");
       fprintf (stdout,
                "    pid PID=pid PPID=ppid SID=sid TIME-mm-ddThh:mm:ss Ver=version Pgm=program\n");
       fprintf (stdout, "\n");
       fprintf (stdout,
                "        Process ID of the most recent invocation of the monitor\n");
       fprintf (stdout,
                "      program.  pid is the process ID of interest.  Parent process ID\n");
       fprintf (stdout,
                "      appears in the ppid fiels and the lead process is in sid.\n");
       fprintf (stdout,
                "      Start time, program version and Program name follow.\n");
       fprintf (stdout, "\n");
       fflush (stdout);
       help_flag = 1;;
       break;
    }
  }
  pid_data (logfile, tm, NULL, NULL);
  fprintf (logfile, "VERS   %s\n", Version);
  fprintf (logfile, "DELTA  %d seconds\n", delta);
  fprintf (logfile, "OFFSET %.0f seconds\n", f_offset);
  fprintf (logfile, "URL    http://%s/%s\n", hostnode, hosturl);
  fprintf (logfile, "CONFIG %s\n", config_filename);

  dsn_tracking_filename = configure (config_filename);

  if (dsn_tracking_filename)
    fprintf (logfile, "TRACK  %s\n", dsn_tracking_filename);
  else {
    fprintf (logfile, "Configuration file or DSN filename missing \n");
    exit (0);
  }
  ut_time = time (NULL);
  fprintf (logfile, "Current time (UTC): %s\n", hack (&ut_time));
  fflush (logfile);
  if (SMS_TEST) {
    sms_commands ("SMS", "Test Message to SMS NO Data", ut_time);
    sms_commands ("SMS1", "Test Message to SMS1 Begin Tracking", ut_time);
    sms_commands ("SMS2", "Test Message to SMS2 End of Tracking", ut_time);

    mail_commands ("MAIL", "Test Message to MAIL NO Data", ut_time);
    mail_commands ("MAIL1", "Test Message to MAIL1 Begin Tracking", ut_time);
    mail_commands ("MAIL2", "Test Message to MAIL2 End of Tracking", ut_time);

    cmd_commands ("CMD");
    cmd_commands ("CMD1");
    cmd_commands ("CMD2");

    announce_commands ("ANNOUNCE");
    announce_commands ("ANNOUNCE1");
    announce_commands ("ANNOUNCE2");

    buf = dsp_web_fetch (hostnode, hosturl, result, &length, debug_file);
    text =
      dsp_monitor ("Test SCLK", "Test Time", buf->packet.mpp.mini_packet, 1);
    if (text[0]) {
      char temp[256];

      sprintf (temp, "Alarm Condition %s", dsn_tracking_text (ut_time));
      sms_commands ("SMS3", "Test Message to SMS3 Alarm Notification",
                    ut_time);
      sms_commands ("SMS4", "Test Message to SMS4 Alarm Notification",
                    ut_time);
      sms_commands ("SMS5", "Test Message to SMS5 Alarm Notification",
                    ut_time);
      mail_alarms ("MAIL3", text + 1, ut_time);
      mail_alarms ("MAIL4", text + 1, ut_time);
      mail_alarms ("MAIL5", text + 1, ut_time);
      cmd_commands ("CMD3");
      cmd_commands ("CMD4");
      cmd_commands ("CMD5");
      announce_commands ("ANNOUNCE3");
      announce_commands ("ANNOUNCE4");
      announce_commands ("ANNOUNCE5");
    }
  }

  while (loop_active) {
    dsn_tracking_filename = configure (config_filename);
    if (!dsn_tracking_filename) {
      fprintf (logfile, "Configuration file or DSN filename missing \n");
      exit (0);
    }
    if (!dsn_tracking_file (dsn_tracking_filename)) {
      fprintf (logfile, "DSN tracking file not found(%s)\n",
               dsn_tracking_filename);
      exit (0);
    }
    if (help_flag) {
      dump_config (logfile, ut_time);
      dump_dsn (logfile);
    }

    ut_time = time (NULL);

    active = dsn_active (ut_time, &flags[1]);
    if (active < 0) {
      fprintf (logfile, "ACTIVE %d\n", active);
      fflush (logfile);
    }
    tracking = dsn_tracking (ut_time, &flags[2]);
    if (tracking < 0) {
      fprintf (logfile, "TRACKING %d\n", tracking);
      fflush (logfile);
    }

    if ((!previous_active) && (active) && dsn_pass_number (ut_time)) {  /* really a pass ??? */
      char temp[256] = "";

      sprintf (temp, "Tracking Setup, Pass %d", dsn_pass_number (ut_time));
      if (flag_scan (key_send[1], key_supp[1], flags[1])) {
        sms_commands ("SMS1", temp, ut_time);
        mail_commands ("MAIL1", temp, ut_time);
      }
    }
    if ((previous_active) && (!active) && dsn_pass_number (ut_time)) {  /* really a pass ??? */
      char temp[256];

      sprintf (temp, "Tracking Teardown");
      if (flag_scan (key_send[2], key_supp[2], flags[2])) {
        sms_commands ("SMS2", temp, ut_time);
        mail_commands ("MAIL2", temp, ut_time);
      }
    }
    fflush (logfile);
    switch (main2 (&buf, ut_time, debug_file, tracking, previous_tracking)) {
     case -1:
       fprintf (logfile, "NO Net Connection  %s UTC\n", hack (&ut_time));
       fprintf (logfile, "    http://%s/%s\n", hostnode, hosturl);
       fflush (logfile);
       break;
     case -2:
       fprintf (logfile, "NO Net Data        %s UTC\n", hack (&ut_time));
       fprintf (logfile, "    http://%s/%s\n", hostnode, hosturl);
       fflush (logfile);
       break;
    }


    previous_active = active;
    previous_tracking = tracking;
    if (ONESHOT)
      break;
    if (help_flag)
      loop_active = 0;
    else
      sleep (delta);
  }
}
