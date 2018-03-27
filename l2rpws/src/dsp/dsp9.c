
/*
 * dsp5.c
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

/* Cassini Stuff */
#include <fg.h>
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>

#define HTML4

static char *title = { " CASSINI MP data dump  (DSP9) 0.10" };
struct MP_buffer *buffer, *buffer2;
static int epoch;
char result[256];
struct stat status_buffer;

int _getbuffer_MP (struct MP_buffer *buffer, FILE * input, int iflg)
{
  int ilen;

  ilen = UTIL_getbuffer_MP (buffer, input, iflg);
  return ilen;
}

#define DETAIL_LEN 256
FILE *_database (time_t * file_tm)
{
  FILE *dbase = NULL;
  FILE *input = NULL;
  int icnt = 0;
  char fname[256] = {'\0'};
  char detail[DETAIL_LEN + 1];
  char *temp;
  int i;

  if (!dbase) {
	 
	 if(!getenv("RPWS_MPDB")){
		fprintf(stderr, "Can't locate the database file, $RPWS_MPDB is not "
		        "defined.\n");
		return NULL;
	 }
	  
	 strcpy(fname, getenv("RPWS_MPDB"));
	 dbase = fopen (fname, "r");
	 if(!dbase){	 
      fprintf (stderr, "Database file not found (%s)\n", fname);
      return NULL;
    }
  }
  while (fgets (detail, DETAIL_LEN, dbase)) {
    temp = strtok (detail, " \t");
    if (temp)
      for (i = 1; i < 5; i++) {
        temp = strtok (NULL, " \t");
        if (!temp)
          break;
        strcpy (result, temp);
      }
    icnt++;
  }
  fclose (dbase);
  input = fopen (result, "rb");
  if (input) {
    stat (result, &status_buffer);
    *file_tm = status_buffer.st_mtime;
  }
  return input;
}

char *make_date (time_t * timeval)
{
  static char date_string[256] = { 256 * 0 };
  struct tm *temp_tm;
  char *format = { "%a, %d %b %Y %T UTC" };

  temp_tm = gmtime (timeval);
  strftime (date_string, 256, format, temp_tm);

  return date_string;
}
int main (int argc, char *argv[])
{
  FILE *input = stdin;
  int ilen;
  char fname[128];
  char str0[1024];
  char str1[1024];
  char date_string[256];
  char last_modified_string[256];

  time_t timeval;
  long cds[6];
  int i;
  time_t pkt_time, pkt_etime, pkt_epoc;
  struct tm *pkt_tm;
  struct tm *pkt_ev;
  char string[128];
  int eof_flag = UTIL_GET_NON_BLOCKING;
  struct event_time *evt_tim;
  struct event_clock evt_clk;
  int current_sclk;
  int newest_sclk = 0;

  buffer = malloc (32768 + 1024);
  buffer2 = malloc (32768 + 1024);
  timeval = time (NULL);
  strcpy (date_string, make_date (&timeval));
  strcpy (last_modified_string, make_date (&timeval));
  fg_flags (argc, argv);
  if (fg_flag ("help") || fg_flag ("h")) {
    fprintf (stdout, "%s   HELP SCREEN\n", title);
    fprintf (stdout, "\n");
    fprintf (stdout, "  TIME\n");
    fprintf (stdout,
             "      When SCLK/SCET information is available it will\n");
    fprintf (stdout,
             "      be used to display the time of the housekeeping\n");
    fprintf (stdout,
             "      record.  The difference between CDS-SCLK adn CHDO-SCLK\n");
    fprintf (stdout,
             "      is applied to the CHDO-SCET and then used to display the\n");
    fprintf (stdout,
             "      time field.  When this 'more or less' accurate time is\n");
    fprintf (stdout,
             "      avaliable, the time field displays as yyyy-dddThh:mm:ss.mmm\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "\n");
    fprintf (stdout, "\n");
    return 0;
  }

  input = _database (&timeval);
  strcpy (last_modified_string, make_date (&timeval));
  ilen = _getbuffer_MP (buffer, input, eof_flag);
  while (ilen > 0) {
    current_sclk = buffer->packet.cds_tag.begin[0] << 24 |
      buffer->packet.cds_tag.begin[1] << 16 |
      buffer->packet.cds_tag.begin[2] << 8 | buffer->packet.cds_tag.begin[3];
    if (current_sclk > newest_sclk) {
      switch (buffer->record_type & 0X0000FF00) {
       case DATA_MP_complete_segment:
       case DATA_MP_large_segment:
       case DATA_MP_segment:
       case DATA_MP_packet:
         ilen = buffer->f_length + 4;
         memcpy (buffer2, buffer, ilen);
         newest_sclk = current_sclk;
         break;
       default:
         break;
      }
    }
    ilen = _getbuffer_MP (buffer, input, eof_flag);
  }
  fclose (input);
  epoch = (buffer->packet.cds_tag.epoch[0] << 24) |
    (buffer->packet.cds_tag.epoch[1] << 16) |
    (buffer->packet.cds_tag.epoch[2] << 8) |
    (buffer->packet.cds_tag.epoch[3] << 0);

  /*
   *      TIME PROCESSING  ********************************************
   */
  pkt_time = UTIL_extract_PKT_TIME (buffer);    /* time from MP */
  pkt_epoc = pkt_time + epoch;          /* adjust to UNIX time */
  pkt_tm = gmtime (&pkt_epoc);          /* format conversion */

  if (1) {
    pkt_etime = UTIL_event_time (buffer, 0);
    evt_clk.seconds = buffer->packet.cds_tag.begin[0] << 24 |
      buffer->packet.cds_tag.begin[1] << 16 |
      buffer->packet.cds_tag.begin[2] << 8 | buffer->packet.cds_tag.begin[3];
    evt_clk.fine = 0;
    evt_tim = UTIL_event_scet (buffer, evt_clk);
    pkt_ev = UTIL_event_scet_tm (*evt_tim, 0);
    if (epoch)
      sprintf (str0, " CDS Time  %02X%02X%02X%02X %04X "
               "(%4d-%03dT%2.2d:%2.2d:%2.2d.%3.3d)",
               buffer->packet.cds_tag.begin[0],
               buffer->packet.cds_tag.begin[1],
               buffer->packet.cds_tag.begin[2],
               buffer->packet.cds_tag.begin[3],
               (pkt_etime & 0x1FFF) << 3,
               pkt_ev->tm_year + 1900,
               pkt_ev->tm_yday + 1,
               pkt_ev->tm_hour,
               pkt_ev->tm_min, pkt_ev->tm_sec, evt_tim->milliseconds % 1000);
    else
      sprintf (str0, " CDS Time  %02X%02X%02X%02X %04X "
               "(%4d-%03d %2.2d:%2.2d:%2.2d.%3.3d)",
               buffer->packet.cds_tag.begin[0],
               buffer->packet.cds_tag.begin[1],
               buffer->packet.cds_tag.begin[2],
               buffer->packet.cds_tag.begin[3],
               (pkt_etime & 0x1FFF) << 3,
               pkt_tm->tm_year + 1900,
               pkt_tm->tm_yday + 1,
               pkt_tm->tm_hour, pkt_tm->tm_min, pkt_tm->tm_sec, 0);
  }


  pkt_etime = UTIL_event_time (buffer, 0);
  pkt_epoc = pkt_etime + epoch;
  if (epoch) {
    evt_clk.seconds = pkt_etime;
    evt_clk.fine = UTIL_extract_MP_RTI (buffer) << 5;
    evt_tim = UTIL_event_scet (buffer, evt_clk);
    pkt_ev = UTIL_event_scet_tm (*evt_tim, 0);
    sprintf (str1,
             " SC Event  %8X %02X%02X (%4d-%03dT%2.2d:%2.2d:%2.2d.%3.3d) new",
             pkt_etime, buffer->packet.mpp.mini_packet[3],
             buffer->packet.mpp.mini_packet[2], pkt_ev->tm_year + 1900,
             pkt_ev->tm_yday + 1, pkt_ev->tm_hour, pkt_ev->tm_min,
             pkt_ev->tm_sec, evt_tim->milliseconds % 1000);
  } else {
    pkt_ev = gmtime (&pkt_epoc);
    sprintf (str1, "SC Event %8X %4X (%2.2d:%2.2d:%2.2d.%3.3d) Epoch %X ",
             pkt_etime,
             (pkt_etime & 0x1FFF) << 3,
             pkt_ev->tm_hour,
             pkt_ev->tm_min,
             pkt_ev->tm_sec, UTIL_extract_MP_RTI (buffer) * 125, epoch);
  }
  printf ("Content-type: text/html\n");
  printf ("Pragma: nocache\n");
  printf ("Date: %s\n", date_string);
  printf ("Last Modified: %s\n", last_modified_string);
  printf ("\n");

#ifdef HTML4
  printf ("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">\n");
  printf ("<HTML>\n");
#endif

  printf ("<HEAD>\n");

#ifdef HTML4
  printf
    ("<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=ISO-8859-1\">\n");
  printf ("<META NAME=\"Date\" CONTENT=\"%sZ\">\n", date_string);
  printf
    ("<META NAME=\"Copyright\" CONTENT=\"&copy; %s University of Iowa\">\n",
     date_string);
  printf ("<META NAME=\"Keywords\" CONTENT=\"Cassini/RPWS\">\n");
  printf ("<META NAME=\"Keywords\" CONTENT=\"HOUSEKEEPING\">\n");
#else
  printf ("<META HTTP_EQUIV=\"Refresh\" CONTENT=\"30\">\n");
#endif

  printf ("<TITLE>%s</TITLE>\n", title);
  printf ("</HEAD>\n");

  printf ("<BODY>\n");
  printf ("<H2>\n");
  printf ("Recent Cassini/RPWS Data\n");
  printf ("</H2>\n");
  printf ("This page queries the RPWS mini-packet database\n");
  printf ("to scan for the most recent science data available.\n");
  printf ("<br>\n");
  printf ("This is a scan of <em>Science Data Only</em>.  \n");
  printf
    ("It does not provide an indication of Housekeeping Data Availability\n");
  printf ("<br>\n");
  printf ("And, as an added bonus, this page might even update \n");
  printf ("about every 30 seconds when left alone.  \n");
  printf ("<br>\n");
  printf
    ("We hope you will find this useful for monitoring real-time data\n");
  printf ("<br>\n");

  printf ("<H3>Data File</H3>\n");
  printf ("last modified %s\n", last_modified_string);
  printf ("<br>\n");
  printf ("file size     %d\n", status_buffer.st_size);
  printf ("<br>\n");
  printf ("<tt>%s</tt>\n", result);
  printf ("<H3>CDS SCLK/SCET</H3>\n");
  printf ("<tt>    %s</tt>\n", str0);
  printf ("<H3>MP SCLK/SCET</H3>\n");
  printf ("<tt>    %s</tt>\n", str1);

#ifdef HTML4
  printf ("   <p>\n");
  printf ("    <a href=\"http://validator.w3.org/check/referer\">\n");
  printf ("Validate with W3C\n");
  printf ("      </a>\n");
  printf ("     </p>\n");
#endif

  printf ("</BODY>\n");

#ifdef HTML4
  printf ("</HTML>\n");
#endif

  return 0;
}
