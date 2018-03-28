 /*
  * rpws_archive.c   
  */
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include <math.h>
#include <ulimit.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <SpiceUsr.h>

#include <fg.h>

#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "utilo.h"
#include "utilt.h"
#include "mp_status.h"
#include "wbr_status.h"
#include "wfr_status.h"
#include "hfr_status.h"
#include "lp_status.h"
#include "stim_status.h"
#include "dust_status.h"
#include "mdb.h"
#include "archive.h"
#include "rpws_status.h"
#include "rpws_label.h"
#include "rpws_fsw_ver.h"
#define SCIOP_TIMEOUT 10
#define BUFFER_SIZE 65536
#define RTI_MASK 0x00E0
#define A_EPOCH 2436204

/****************************************************************************/

char *Title = { "RPWS_BROWSE" };
char *Version = { "V5.7" };
char Delimiter = { ' ' };

static char command[128] = { "ls *_WBR_*" };
static int force_flag = 0;
static int length_flag = 0;
static int nert_flag = -1;
extern char *MDB_Version;
extern char *optarg;
extern int optind, opterr, optopt;
static int data_type = WBR_packet_ID_value;
static int file_type = MDB_U_FILE;
static int Partition = 1;
static int SpaceCraft_ID = -82;
static int max_delta = 60 * 3;
static int daily_offset = 5;
static char path[256];

FILE *verbose_file = NULL;

#define HFR_STRUCT_INDEX 32
struct HFR_STRUCT
{
  int band_edge[2];
  char band[32];
};

#define DUST_STRUCT_INDEX 128
struct DUST_STRUCT
{
  double et;
  char time[32];
};

  /*
   * 
   */

  /*********************************************************
   **** If you're familiar with SPICE S- and P-kernels,	**** 
   **** you know that NAIF codes for spacecraft are 	****
   **** negative integers: -31 for Voyager 1, -32 for	****
   **** Voyager 2, -94 for Mars Observer, and so on. We	****
   **** borrow from this convention in defining		****
   **** instrument codes.				****
   ****							****
   **** Well, who'd-a thunk-it... negative numbers...	****
   **** sheesh, I guess that means Cassini is -82, then	****
   **** isn't it ?!?   Doooh				****
   *********************************************************/
static double spice_time_ext (int seconds, int fine, int partition)
{
  double et;
  char sclk_temp[256];

                /************************************************
		 *	Convert SCLK to string (spice format)	*
		 ************************************************/
  sprintf (sclk_temp, "%d/%d:%03d", partition, seconds, fine & RTI_MASK);       /* only valid to RTI */

                /************************************************
		 *	Conversion to SPICE internal	 	*
		 ************************************************/
  scs2e_ (&SpaceCraft_ID, sclk_temp, &et, strlen (sclk_temp));
  return et;
}

  /*
   * 
   */

  /**********************************************************************
   *	
   *	Use spice to arrive at real times...
   *	
   **********************************************************************/
static double spice_time_x (struct event_clock *sclk,
                            struct SPICE_TIME *spice_time_array,
                            char *new_utcout)
{
  double et;
  int prec = 3;
  char *format[2] = { "D", "J" };
  static char utcout[64];
  static char jdout[64];

  memset (utcout, 0, 33);
  memset (jdout, 0, 33);

                /************************************************
		 *	Convert SCLK to spice format		*
		 ************************************************/
  et = spice_time_ext (sclk->seconds, sclk->fine, Partition);

                /************************************************
		 *	Conversion to data string	 	*
		 *	Both julian ddd.fff to get days		*
		 *	and yyy-ddd // hh:mm:ss.mmm to get	*
		 *	  milliseconds of day			*
		 ************************************************/
  et2utc_ (&et, format[0], &prec, utcout, strlen (format[0]), 32);
  et2utc_ (&et, format[1], &prec, jdout, strlen (format[1]), 32);

                /************************************************
		 *	Goofey-ass, isn't it, BUT...	 	*
		 *	  now we have spice's opinion so	*
		 *	  convert back yo internal		*
		 *	SPICE JULIAN day is epoch 1/1/1 AD	*
		 *	  subtract number of days to 		*
		 *	  CASSINI epoch				*
		 ************************************************/
  spice_time_array->year = strtol (&utcout[0], NULL, 10);
  spice_time_array->doy = strtol (&utcout[5], NULL, 10);
  spice_time_array->d100 = spice_time_array->doy / 100;

  spice_time_array->days = strtol (&jdout[3], NULL, 10) - A_EPOCH;

                /************************************************
		 *	Now use HH:MM:SS.mmm to get 		*
		 *	seconds of day.				*
		 ************************************************/
  spice_time_array->hours = strtol (&utcout[12], NULL, 10);
  spice_time_array->minutes = strtol (&utcout[15], NULL, 10);
  spice_time_array->seconds = strtol (&utcout[18], NULL, 10);
  spice_time_array->milliseconds = strtol (&utcout[21], NULL, 10);

  spice_time_array->msec_of_day = spice_time_array->milliseconds;
  spice_time_array->msec_of_day += spice_time_array->seconds * 1000;
  spice_time_array->msec_of_day += spice_time_array->minutes * 1000 * 60;
  spice_time_array->msec_of_day += spice_time_array->hours * 1000 * 60 * 60;
  if (new_utcout) {
    memset (new_utcout, 0, 32);
    strncat (new_utcout, &utcout[0], 8);
    strncat (new_utcout, "T", 1);
    strncat (new_utcout, &utcout[12], 12);
    if (0) {
      if (verbose_file) {
        fprintf (verbose_file, "%5d                1         2         3\n",
                 __LINE__);
        fprintf (verbose_file, "%5d      012345678901234567890123456789012\n",
                 __LINE__);
        fprintf (verbose_file, "%5d UTC >%s<\n", __LINE__, new_utcout);
        fflush (verbose_file);
      }
    }
  }
  return et;
}

char *now (int local_flag)
{
  time_t timep;
  struct tm *tm_st;
  static char time_string[32] = { "2003-11-01T10:02:01" };

  timep = time (NULL);
  if (local_flag)
    tm_st = localtime (&timep);
  else
    tm_st = gmtime (&timep);
  sprintf (time_string, "%04d-%02d-%02dT%02d:%02d:%02d",
           tm_st->tm_year + 1900,
           tm_st->tm_mon + 1,
           tm_st->tm_mday, tm_st->tm_hour, tm_st->tm_min, tm_st->tm_sec);
  return time_string;
}

  /*
   * 
   */

  /*********************************************************
   ****							****
   ****	Taken from DSP5.  Calculate the down convert	****
   ****	frequency fromt he WBR dsstatus			****
   ****							****
   *********************************************************/
char *frq_hfr_wbr (unsigned char status)
{
  static char text[64];
  float freq;

  if (status & 0x01) {
    sprintf (text, "%dKhz", status * 25);
  } else {
    freq = status * 50;
    freq /= 1000.;
    freq += 4.025;
    sprintf (text, "%.3fMhz", freq);
  }
  return text;
}

  /*
   * 
   */

  /*********************************************************
   ****							****
   ****	Generate new filename                           ****
   ****							****
   *********************************************************/
char *fngen (char *time, int offset)
{
  int temp;
  static char time_string[64] = { "" };
  struct DATABASE_TIME_TAG data_stop_time;

  if (!time)
    return NULL;
  if (!time[0])
    return time_string;
  parsetime (time,
             &data_stop_time.year,
             &data_stop_time.month,
             &data_stop_time.mday,
             &data_stop_time.yday,
             &data_stop_time.hour,
             &data_stop_time.minute, &data_stop_time.second);
  temp = data_stop_time.yday % abs (offset);
  data_stop_time.yday -= temp;
  data_stop_time.yday += offset;
  data_stop_time.month = 1;
  data_stop_time.mday = data_stop_time.yday;
  tnorm (&data_stop_time.year,
         &data_stop_time.month,
         &data_stop_time.mday,
         &data_stop_time.yday,
         &data_stop_time.hour,
         &data_stop_time.minute, &data_stop_time.second);
  sprintf (time_string, "%04d-%03dT%02d:%02d:%06.3f",
           data_stop_time.year,
           data_stop_time.yday,
           data_stop_time.hour, data_stop_time.minute, data_stop_time.second);
  return time_string;
}

  /*
   * 
   */

  /*********************************************************
   ****							****
   ****							****
   *********************************************************/
double pick_ (char *time)
{
  struct DATABASE_TIME_TAG data_time;

  parsetime (time,
             &data_time.year,
             &data_time.month,
             &data_time.mday,
             &data_time.yday,
             &data_time.hour, &data_time.minute, &data_time.second);
  return ttime (&data_time.year,
                &data_time.month,
                &data_time.mday,
                &data_time.yday,
                &data_time.hour, &data_time.minute, &data_time.second);

}
char *pick_start (char *start, char *stop)
{
  double dst, dsp;

  dst = pick_ (start);
  dsp = pick_ (stop);
  if (dsp < dst)
    return stop;
  return start;
}
char *pick_stop (char *start, char *stop)
{
  double dst, dsp;

  dst = pick_ (start);
  dsp = pick_ (stop);
  if (dsp < dst)
    return start;
  return stop;
}
char *build_gif_name (int flag, char *gif_name, struct HFR_STRUCT *hfr_struct,
                      struct DUST_STRUCT *dust_struct)
{
  char temp_name[32];
  static int index = 0;
  static char time_file[16][64];

  index++;
  index &= 0x0F;
  switch (flag) {
   case 0:
     sprintf (time_file[index], "WBR_%s", gif_name);
     break;
   case 1:
     sprintf (time_file[index], "WBR_%s_10Khz", gif_name);
     break;
   case 2:
     sprintf (time_file[index], "WBR_%s_80Khz", gif_name);
     break;
   case 4:
     sprintf (time_file[index], "WBR_%s_%s", gif_name, hfr_struct->band);
     break;
   case 8:
     strcpy (temp_name, dust_struct->time);
     temp_name[4] = '_';
     temp_name[11] = '-';
     temp_name[14] = '-';
     temp_name[17] = 0;
     sprintf (time_file[index], "WBR_%s_DUST", temp_name);
     break;
  }
  return time_file[index];
}

  /*
   * 
   */

  /*********************************************************
   ****							****
   ****							****
   *********************************************************/
char *ephem_time (double et_in, double offset)
{
  double et;
  static char ctemp[33];
  static char format[] = { "D" };
  static int prec = 3;

  if (!et) {
    ctemp[0] = 0;
    return ctemp;
  }
  et = et_in + offset;
  et2utc_ (&et, format, &prec, ctemp, strlen (format), 32);
  strcpy (&ctemp[8], &ctemp[11]);
  ctemp[8] = 'T';
  ctemp[21] = 0;
  return ctemp;
}

char *subgen (char *start)
{
  static char subname[64];

  sprintf (subname, "T%s", start);
  subname[5] = subname[6];
  subname[6] = 'X';
  subname[7] = 'X';
  subname[8] = 0;
  return subname;
}

int arrows (FILE * html_file, char *page_prev, char *page_next)
{
  static int index = 0;

  fprintf (html_file, "<p>\n");

  fprintf (html_file, "<A HREF=\"%s\">\n", page_prev);
  fprintf (html_file, "    <IMG\n");
  fprintf (html_file, "        ALT=\"Previous Page\"\n");
  fprintf (html_file, "        WIDTH=31\n");
  fprintf (html_file, "        HEIGHT=31\n");
  fprintf (html_file, "        SRC=\"../2arrow3.gif\">\n");
  fprintf (html_file, "    </A>\n");
  fprintf (html_file, "    Prev\n");

  fprintf (html_file, "<A HREF=\"../index.html\">\n");
  fprintf (html_file, "    <IMG\n");
  fprintf (html_file, "        ALT=\"Index Page\"\n");
  fprintf (html_file, "        WIDTH=31\n");
  fprintf (html_file, "        HEIGHT=31\n");
  fprintf (html_file, "        SRC=\"../2arrow2.gif\">\n");
  fprintf (html_file, "    </A>\n");
  fprintf (html_file, "    Index\n");

  fprintf (html_file, "<A HREF=\"%s\">\n", page_next);
  fprintf (html_file, "    <IMG\n");
  fprintf (html_file, "        ALT=\"Next Page\"\n");
  switch (nert_flag) {
   default:
     fprintf (html_file, "        WIDTH=31\n");
     fprintf (html_file, "        HEIGHT=31\n");
     fprintf (html_file, "        SRC=\"../2arrow4.gif\">\n");
     fprintf (html_file, "    </A>\n");
     fprintf (html_file, "    Next\n");
     break;
   case 1:
     switch (index) {
      default:
        fprintf (html_file, "        SRC=\"../an_pink_monster.gif\">\n");
        fprintf (html_file, "    </A>\n");
        fprintf (html_file, "    (Don't get eaten)\n");
        index++;
        break;
      case 1:
        fprintf (html_file, "        SRC=\"../cat01.gif\">\n");
        fprintf (html_file, "    </A>\n");
        fprintf (html_file, "    (Aack, Ptooey)\n");
        index++;
        break;
      case 2:
        fprintf (html_file, "        SRC=\"../cat02.gif\">\n");
        fprintf (html_file, "    </A>\n");
        fprintf (html_file, "    (Bill?)\n");
        index++;
        break;
      case 3:
        fprintf (html_file, "        SRC=\"../cat03.gif\">\n");
        fprintf (html_file, "    </A>\n");
        fprintf (html_file, "    (...)\n");
        index++;
        break;
      case 4:
        fprintf (html_file, "        SRC=\"../at_work.gif\">\n");
        fprintf (html_file, "    </A>\n");
        fprintf (html_file, "    (Tomorrow?)\n");
        index++;
        break;
      case 5:
        fprintf (html_file, "        SRC=\"../cat05.gif\">\n");
        fprintf (html_file, "    </A>\n");
        fprintf (html_file, "    (Aack, Ptooey)\n");
        index++;
        break;
      case 6:
        fprintf (html_file, "        SRC=\"../an_strobe.gif\">\n");
        fprintf (html_file, "    </A>\n");
        fprintf (html_file, "    (Tuesday perhaps?)\n");
        index++;
        break;
     }
  }

  fflush (html_file);
  return 1;
}

int arrow2 (FILE * html_file, int count, char *page[], char *text[])
{
  int i;

  for (i = 0; i < count; i++) {
    fprintf (html_file, "<A HREF=\"%s\">\n", page[i]);
    fprintf (html_file, "    <IMG\n");
    fprintf (html_file, "        ALT=\"%s\"\n", text[i]);
    fprintf (html_file, "        WIDTH=31\n");
    fprintf (html_file, "        HEIGHT=31\n");
    fprintf (html_file, "        SRC=\"../4arrow1.gif\">\n");
    fprintf (html_file, "    </A>\n");
    fprintf (html_file, "    %s\n", text[i]);
  }
  fflush (html_file);
  return 1;
}

  /*
   * 
   */

  /*********************************************************
   ****							****
   ****	Generate html for one time period		****
   ****	    HFR_STRUCT has a list of HFR down convert	****
   ****		frequencies in the period		****
   ****	    wbr_mode is a mask of modes the WBR has	****
   ****		been in during the period.		****
   ****	    count is simply a record ocunt		****
   ****							****
   *********************************************************/
int make_html (char *start[],
               char *stop[],
               struct HFR_STRUCT *hfr_struct,
               struct DUST_STRUCT *dust_struct,
               int wbr_mode, int count, char *argv[])
{
  int i;
  static int html_index[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  static int image_count[8] = { 0, 0, 0, 0, 0, 0, 0, 0 };
  char gif_name[64];
  static char page_next[8][64];
  static char page_prev[8][64];
  char *ctemp;
  char *dtemp;
  static FILE *html_file[8] =
    { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL };
  static char *filename[8] =
    { "wbr_html_file.html                                      ",
    "wbr_80khz_html_file.html                                ",
    "wbr_10khz_html_file.html                                ",
    "wbr_hfr_html_file.html                                  ",
    "wbr_dust_html_file.html                                 ",
    "",
    "",
    ""
  };
  static char *titles[8] = { "Index",
    "80Khz",
    "10Khz",
    "HF_WBR",
    "DUST",
    "",
    "",
    ""
  };

  static char *time_string;

  /*
   *      First time tthrough...
   */
  if (!html_file[0]) {
    if (start[0]) {
      strcpy (gif_name, start[0]);
      gif_name[8] = 0;
      sprintf (filename[0], "A_%s.html",
               build_gif_name (0, gif_name, NULL, NULL));
      sprintf (filename[1], "A_%s_80.html",
               build_gif_name (0, gif_name, NULL, NULL));
      sprintf (filename[2], "A_%s_10.html",
               build_gif_name (0, gif_name, NULL, NULL));
      sprintf (filename[3], "A_%s_hf.html",
               build_gif_name (0, gif_name, NULL, NULL));
      sprintf (filename[4], "A_%s_dust.html",
               build_gif_name (0, gif_name, NULL, NULL));
    }
    for (i = 0; i < 5; i++) {
      time_string = now (0);
      html_file[i] = fopen (filename[i], "w");
      if (verbose_file)
        fprintf (verbose_file, "%5d %p=fopen(\"%s\",\"w\");\n", html_file[i],
                 __LINE__, filename[i]);
      /**/ fprintf (html_file[i],
                    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">\n");
      fprintf (html_file[i], "<HTML>\n");
      fprintf (html_file[i], "<HEAD>\n");
      fprintf (html_file[i],
               "<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=ISO-8859-1\">\n");
      fprintf (html_file[i], "<META NAME=\"Date\" CONTENT=\"%sZ\">\n",
               time_string);
      time_string[4] = 0;
      fprintf (html_file[i],
               "<META NAME=\"Copyright\" CONTENT=\"&copy; %s University of Iowa\">\n",
               time_string);
      fprintf (html_file[i],
               "<META NAME=\"Keywords\" CONTENT=\"Cassini/RPWS\">\n");
      fprintf (html_file[i], "<META NAME=\"Keywords\" CONTENT=\"WBR\">\n");
      fprintf (html_file[i],
               "<META NAME=\"Keywords\" CONTENT=\"Quicklook\">\n");
      fprintf (html_file[i], "<TITLE>");
      fprintf (html_file[i], "    Cassini/RPWS WBR Quick Look\n");
      fprintf (html_file[i], "  </TITLE>\n");
      fprintf (html_file[i], "  </HEAD>\n");
      fprintf (html_file[i], "<BODY>\n");
      if (start[0]) {
        strcpy (gif_name, start[0]);
        gif_name[8] = 0;
        fprintf (html_file[i], "<H1>\n");
        fprintf (html_file[i], "WBR Quicklook Dataset (%s)\n", gif_name);
        fprintf (html_file[i], "</H1>\n");
        switch (nert_flag) {
         case -1:
           fprintf (html_file[i], "<p>\n");
           fprintf (html_file[i], "This data page was produced manually.\n");
           fprintf (html_file[i], "<br>\n");
           break;
         case 0:
           fprintf (html_file[i], "<p>\n");
           fprintf (html_file[i],
                    "This data page was produced automatically.\n");
           fprintf (html_file[i], "<br>\n");
           break;
         case 1:
           fprintf (html_file[i], "<p>\n");
           fprintf (html_file[i],
                    "This data page was produced in near-real time.\n");
           fprintf (html_file[i],
                    "Some of the processing programs do not deal with\n");
           fprintf (html_file[i],
                    "the point in the data where we change from \n");
           fprintf (html_file[i],
                    "post-pass to real-time data.  As a result, some\n");
           fprintf (html_file[i],
                    "plots may be missing that you may be able to\n");
           fprintf (html_file[i],
                    "view using the link to the viewer (specify a\n");
           fprintf (html_file[i],
                    "longer time period).  Once post-pass data has\n");
           fprintf (html_file[i],
                    "been collected this is no longer a problem.\n");
           fprintf (html_file[i],
                    "Following post-pass, the missing plots should\n");
           fprintf (html_file[i], "appear here.\n");
           fprintf (html_file[i], "<p>\n");
           fprintf (html_file[i],
                    "As this is the most current data, the link to the\n");
           fprintf (html_file[i], "next page doesn't do anything useful.\n");
           fprintf (html_file[i], "<br>\n");
           break;
         case 2:
           fprintf (html_file[i], "<p>\n");
           fprintf (html_file[i],
                    "This data page was produced 5-10 days after\n");
           fprintf (html_file[i],
                    "the data was collected on the spacecraft.\n");
           fprintf (html_file[i],
                    "We should have post-pass data by this point in\n");
           fprintf (html_file[i],
                    "time so all plots should be visible.  If there\n");
           fprintf (html_file[i],
                    "are missing plots, the post-pass activity may\n");
           fprintf (html_file[i], "have been delayed.\n");
           fprintf (html_file[i], "<br>\n");
           break;
         case 3:
           fprintf (html_file[i], "<p>\n");
           fprintf (html_file[i], "Waiting for 2-week post-pass.\n");
           fprintf (html_file[i], "\n");
           break;
        }
        if (i == 3) {
          fprintf (html_file[i], "<p>\n");
          fprintf (html_file[i],
                   "It is normal to see blank 325Khz plots.  \n");
          fprintf (html_file[i],
                   "The software that produces the html and requests\n");
          fprintf (html_file[i],
                   "the \"das\" plot does not know how to eliminate\n");
          fprintf (html_file[i], "these plots.\n");
          fprintf (html_file[i],
                   "They are a result of the way the trigger is initiated\n");
          fprintf (html_file[i], "and may be ignored.\n");
          fprintf (html_file[i], "\n");
        }

        ctemp = fngen (start[0], -daily_offset);
        ctemp[8] = 0;
        dtemp = subgen (ctemp);
        sprintf (page_prev[0], "../%s/A_%s.html", dtemp,
                 build_gif_name (0, ctemp, NULL, NULL));
        sprintf (page_prev[1], "../%s/A_%s_80.html", dtemp,
                 build_gif_name (0, ctemp, NULL, NULL));
        sprintf (page_prev[2], "../%s/A_%s_10.html", dtemp,
                 build_gif_name (0, ctemp, NULL, NULL));
        sprintf (page_prev[3], "../%s/A_%s_hf.html", dtemp,
                 build_gif_name (0, ctemp, NULL, NULL));
        sprintf (page_prev[4], "../%s/A_%s_dust.html", dtemp,
                 build_gif_name (0, ctemp, NULL, NULL));
        ctemp = fngen (start[0], +daily_offset);
        dtemp = subgen (ctemp);
        ctemp[8] = 0;
        sprintf (page_next[0], "../%s/A_%s.html", dtemp,
                 build_gif_name (0, ctemp, NULL, NULL));
        sprintf (page_next[1], "../%s/A_%s_80.html", dtemp,
                 build_gif_name (0, ctemp, NULL, NULL));
        sprintf (page_next[2], "../%s/A_%s_10.html", dtemp,
                 build_gif_name (0, ctemp, NULL, NULL));
        sprintf (page_next[3], "../%s/A_%s_hf.html", dtemp,
                 build_gif_name (0, ctemp, NULL, NULL));
        sprintf (page_next[4], "../%s/A_%s_dust.html", dtemp,
                 build_gif_name (0, ctemp, NULL, NULL));
        arrows (html_file[i], page_prev[i], page_next[i]);
        arrow2 (html_file[i], 5, filename, titles);
      } else {
        fprintf (html_file[i], "<H1>\n");
        fprintf (html_file[i], "WBR Quicklook Dataset\n");
        fprintf (html_file[i], "</H1>\n");
      }
      fprintf (html_file[i], "<br>\n");
      fprintf (html_file[i], "<TABLE>\n");
    }
  }
  /*
   *      Specify a starting time to cause
   *      record generation.
   */
  if (start[0]) {
    int index = 0;

    strcpy (gif_name, start[0]);
    gif_name[11] = '-';
    gif_name[14] = 0;
    /*
     *      HFR downconver mode(s)
     */
    if (wbr_mode & 4) {
      for (index = 1; index < HFR_STRUCT_INDEX; index++) {
        if ((hfr_struct[index].band_edge[0]) &&
            (hfr_struct[index].band_edge[1])) {
          image_count[0]++;
          if (!(html_index[0] % 4))
            fprintf (html_file[0], "<TR ALIGN=\"CENTER\">\n");
          fprintf (html_file[0], "<TD>\n");
          fprintf (html_file[0], "    <A HREF=\"%s.gif\">\n",
                   build_gif_name (4, gif_name, &hfr_struct[index],
                                   &dust_struct[index]));
          fprintf (html_file[0], "    <IMG \n");
          fprintf (html_file[0], "     ALT=\"GIF would not render\"\n");
          fprintf (html_file[0], "     WIDTH=128\n");
          fprintf (html_file[0], "     HEIGHT=96\n");
          fprintf (html_file[0], "     SRC=\"%s.tn.gif\">\n",
                   build_gif_name (4, gif_name, &hfr_struct[index],
                                   &dust_struct[index]));
          fprintf (html_file[0], "    <BR>%s.gif\n",
                   build_gif_name (4, gif_name, &hfr_struct[index],
                                   &dust_struct[index]));
          fprintf (html_file[0], "    </A>\n");
          fprintf (html_file[0], "    <BR>Start %s\n",
                   pick_start (start[0], stop[0]));
          fprintf (html_file[0], "    <BR>Stop %s\n",
                   pick_stop (start[0], stop[0]));
          fprintf (html_file[0], "    <BR>\n");
          fprintf (html_file[0],
                   "    <A HREF=\"http://cassini.physics.uiowa.edu/~tfa/cas/das-wbr80hf2_new.html\">\n");
          fprintf (html_file[0], "    Web Interface\n");
          fprintf (html_file[0], "    </A>\n");
          fprintf (html_file[0], "    <P>\n");
          html_index[0]++;


          image_count[3]++;
          if (!(html_index[3] % 4))
            fprintf (html_file[3], "<TR ALIGN=\"CENTER\">\n");
          fprintf (html_file[3], "<TD>\n");
          fprintf (html_file[3], "    <A HREF=\"%s.gif\">\n",
                   build_gif_name (4, gif_name, &hfr_struct[index],
                                   &dust_struct[index]));
          fprintf (html_file[3], "    <IMG \n");
          fprintf (html_file[3], "     ALT=\"GIF would not render\"\n");
          fprintf (html_file[3], "     WIDTH=128\n");
          fprintf (html_file[3], "     HEIGHT=96\n");
          fprintf (html_file[3], "     SRC=\"%s.tn.gif\">\n",
                   build_gif_name (4, gif_name, &hfr_struct[index],
                                   &dust_struct[index]));
          fprintf (html_file[3], "    <BR>%s.gif\n",
                   build_gif_name (4, gif_name, &hfr_struct[index],
                                   &dust_struct[index]));
          fprintf (html_file[3], "    </A>\n");
          fprintf (html_file[3], "    <BR>Start %s\n",
                   pick_start (start[0], stop[0]));
          fprintf (html_file[3], "    <BR>Stop %s\n",
                   pick_stop (start[0], stop[0]));
          fprintf (html_file[3], "    <BR>\n");
          fprintf (html_file[3],
                   "    <A HREF=\"http://cassini.physics.uiowa.edu/~tfa/cas/das-wbr80hf2_new.html\">\n");
          fprintf (html_file[3], "    Web Interface\n");
          fprintf (html_file[3], "    </A>\n");
          fprintf (html_file[3], "    <P>\n");
          fflush (html_file[3]);
          html_index[3]++;
        }
      }
    }
    /*
     *      80 Khz modes
     */
    if (wbr_mode & 2) {
      image_count[0]++;
      if (!(html_index[0] % 4))
        fprintf (html_file[0], "<TR ALIGN=\"CENTER\">\n");
      fprintf (html_file[0], "<TD>\n");
      fprintf (html_file[0], "    <A HREF=\"%s.gif\">\n",
               build_gif_name (2, gif_name, hfr_struct, dust_struct));
      fprintf (html_file[0], "    <IMG \n");
      fprintf (html_file[0], "     ALT=\"GIF would not render\"\n");
      fprintf (html_file[0], "     WIDTH=128\n");
      fprintf (html_file[0], "     HEIGHT=96\n");
      fprintf (html_file[0], "     SRC=\"%s.tn.gif\">\n",
               build_gif_name (2, gif_name, hfr_struct, dust_struct));
      fprintf (html_file[0], "    <BR>%s.gif\n",
               build_gif_name (2, gif_name, hfr_struct, dust_struct));

      fprintf (html_file[0], "    </A>\n");
      fprintf (html_file[0], "    <BR>Start %s\n",
               pick_start (start[0], stop[0]));
      fprintf (html_file[0], "    <BR>Stop %s\n",
               pick_stop (start[0], stop[0]));
      fprintf (html_file[0], "    <BR>\n");
      fprintf (html_file[0],
               "    <A HREF=\"http://cassini.physics.uiowa.edu/~tfa/cas/das-wbr80_V4.html\">\n");
      fprintf (html_file[0], "    Web Interface\n");
      fprintf (html_file[0], "    </A>\n");
      fprintf (html_file[0], "    <P>\n");
      html_index[0]++;

      image_count[1]++;
      if (!(html_index[1] % 4))
        fprintf (html_file[1], "<TR ALIGN=\"CENTER\">\n");
      fprintf (html_file[1], "<TD>\n");
      fprintf (html_file[1], "    <A HREF=\"%s.gif\">\n",
               build_gif_name (2, gif_name, hfr_struct, dust_struct));
      fprintf (html_file[1], "    <IMG \n");
      fprintf (html_file[1], "     ALT=\"GIF would not render\"\n");
      fprintf (html_file[1], "     WIDTH=128\n");
      fprintf (html_file[1], "     HEIGHT=96\n");
      fprintf (html_file[1], "     SRC=\"%s.tn.gif\">\n",
               build_gif_name (2, gif_name, hfr_struct, dust_struct));
      fprintf (html_file[1], "    <BR>%s.gif\n",
               build_gif_name (2, gif_name, hfr_struct, dust_struct));

      fprintf (html_file[1], "    </A>\n");
      fprintf (html_file[1], "    <BR>Start %s\n",
               pick_start (start[0], stop[0]));
      fprintf (html_file[1], "    <BR>Stop %s\n",
               pick_stop (start[0], stop[0]));
      fprintf (html_file[1], "    <BR>\n");
      fprintf (html_file[1],
               "    <A HREF=\"http://cassini.physics.uiowa.edu/~tfa/cas/das-wbr80_V4.html\">\n");
      fprintf (html_file[1], "    Web Interface\n");
      fprintf (html_file[1], "    </A>\n");
      fprintf (html_file[1], "    <P>\n");
      fflush (html_file[1]);
      html_index[1]++;
    }
    /*
     *      10Khz modes
     */
    if (wbr_mode & 1) {
      image_count[0]++;
      if (!(html_index[0] % 4))
        fprintf (html_file[0], "<TR ALIGN=\"CENTER\">\n");
      fprintf (html_file[0], "<TD>\n");
      fprintf (html_file[0], "    <A HREF=\"%s.gif\">\n",
               build_gif_name (1, gif_name, hfr_struct, dust_struct));
      fprintf (html_file[0], "    <IMG \n");
      fprintf (html_file[0], "     ALT=\"GIF would not render\"\n");
      fprintf (html_file[0], "     WIDTH=128\n");
      fprintf (html_file[0], "     HEIGHT=96\n");
      fprintf (html_file[0], "     SRC=\"%s.tn.gif\">\n",
               build_gif_name (1, gif_name, hfr_struct, dust_struct));
      fprintf (html_file[0], "    <BR>%s.gif\n",
               build_gif_name (1, gif_name, hfr_struct, dust_struct));

      fprintf (html_file[0], "    </A>\n");
      fprintf (html_file[0], "    <BR>Start %s\n",
               pick_start (start[0], stop[0]));
      fprintf (html_file[0], "    <BR>Stop %s\n",
               pick_stop (start[0], stop[0]));
      fprintf (html_file[0], "    <BR>\n");
      fprintf (html_file[0],
               "    <A HREF=\"http://cassini.physics.uiowa.edu/~tfa/cas/das-wbr10_V4.html\">\n");
      fprintf (html_file[0], "    Web Interface\n");
      fprintf (html_file[0], "    </A>\n");
      fprintf (html_file[0], "    <P>\n");
      html_index[0]++;

      image_count[2]++;
      if (!(html_index[2] % 4))
        fprintf (html_file[2], "<TR ALIGN=\"CENTER\">\n");
      fprintf (html_file[2], "<TD>\n");
      fprintf (html_file[2], "    <A HREF=\"%s.gif\">\n",
               build_gif_name (1, gif_name, hfr_struct, dust_struct));
      fprintf (html_file[2], "    <IMG \n");
      fprintf (html_file[2], "     ALT=\"GIF would not render\"\n");
      fprintf (html_file[2], "     WIDTH=128\n");
      fprintf (html_file[2], "     HEIGHT=96\n");
      fprintf (html_file[2], "     SRC=\"%s.tn.gif\">\n",
               build_gif_name (1, gif_name, hfr_struct, dust_struct));
      fprintf (html_file[2], "    <BR>%s.gif\n",
               build_gif_name (1, gif_name, hfr_struct, dust_struct));

      fprintf (html_file[2], "    </A>\n");
      fprintf (html_file[2], "    <BR>Start %s\n",
               pick_start (start[0], stop[0]));
      fprintf (html_file[2], "    <BR>Stop %s\n",
               pick_stop (start[0], stop[0]));
      fprintf (html_file[2], "    <BR>\n");
      fprintf (html_file[2],
               "    <A HREF=\"http://cassini.physics.uiowa.edu/~tfa/cas/das-wbr10_V4.html\">\n");
      fprintf (html_file[2], "    Web Interface\n");
      fprintf (html_file[2], "    </A>\n");
      fprintf (html_file[2], "    <P>\n");
      fflush (html_file[2]);
      html_index[2]++;
    }
    /*
     *      DUST modes
     */
    if (wbr_mode & 8) {
      for (index = 1; index < DUST_STRUCT_INDEX; index++) {
        if (dust_struct[index].time[0]) {
          image_count[0]++;
          if (!(html_index[0] % 4))
            fprintf (html_file[0], "<TR ALIGN=\"CENTER\">\n");
          fprintf (html_file[0], "<TD>\n");
          fprintf (html_file[0], "    <A HREF=\"%s.gif\">\n",
                   build_gif_name (8, gif_name, &hfr_struct[index],
                                   &dust_struct[index]));
          fprintf (html_file[0], "    <IMG \n");
          fprintf (html_file[0], "     ALT=\"GIF would not render\"\n");
          fprintf (html_file[0], "     WIDTH=136\n");
          fprintf (html_file[0], "     HEIGHT=109\n");
          fprintf (html_file[0], "     SRC=\"%s.tn.gif\">\n",
                   build_gif_name (8, gif_name, &hfr_struct[index],
                                   &dust_struct[index]));
          fprintf (html_file[0], "    <BR>%s.gif\n",
                   build_gif_name (8, gif_name, &hfr_struct[index],
                                   &dust_struct[index]));

          fprintf (html_file[0], "    </A>\n");
          fprintf (html_file[0], "    <BR>Event %s\n",
                   ephem_time (dust_struct[index].et, -0.000));
          fprintf (html_file[0], "    <BR>\n");
          fprintf (html_file[0],
                   "    <A HREF=\"http://cassini.physics.uiowa.edu/~tfa/cas/wbrplot.html\">\n");
          fprintf (html_file[0], "    Web Interface\n");
          fprintf (html_file[0], "    </A>\n");
          fprintf (html_file[0], "    <P>\n");
          html_index[0]++;

          image_count[4]++;
          if (!(html_index[4] % 4))
            fprintf (html_file[4], "<TR ALIGN=\"CENTER\">\n");
          fprintf (html_file[4], "<TD>\n");
          fprintf (html_file[4], "    <A HREF=\"%s.gif\">\n",
                   build_gif_name (8, gif_name, &hfr_struct[index],
                                   &dust_struct[index]));
          fprintf (html_file[4], "    <IMG \n");
          fprintf (html_file[4], "     ALT=\"GIF would not render\"\n");
          fprintf (html_file[4], "     WIDTH=136\n");
          fprintf (html_file[4], "     HEIGHT=109\n");
          fprintf (html_file[4], "     SRC=\"%s.tn.gif\">\n",
                   build_gif_name (8, gif_name, &hfr_struct[index],
                                   &dust_struct[index]));
          fprintf (html_file[4], "    <BR>%s.gif\n",
                   build_gif_name (8, gif_name, &hfr_struct[index],
                                   &dust_struct[index]));

          fprintf (html_file[4], "    </A>\n");
          fprintf (html_file[4], "    <BR>Event %s\n",
                   ephem_time (dust_struct[index].et, -0.000));
          fprintf (html_file[4], "    <BR>\n");
          fprintf (html_file[4],
                   "    <A HREF=\"http://cassini.physics.uiowa.edu/~tfa/cas/wbrplot.html\">\n");
          fprintf (html_file[4], "    Web Interface\n");
          fprintf (html_file[4], "    </A>\n");
          fprintf (html_file[4], "    <P>\n");
          fflush (html_file[4]);
          html_index[4]++;
        }
      }
    }
  } else
    /*
     *      No start time, the close files...
     *      leave the script executable...
     */
  {
    for (i = 0; i < 5; i++) {

      if (image_count[i]) {
        fprintf (html_file[i], "</TABLE>\n");
        arrows (html_file[i], page_prev[i], page_next[i]);
        arrow2 (html_file[i], 5, filename, titles);
      } else {
        fprintf (html_file[i], "<TR ALIGN=\"CENTER\">\n");
        fprintf (html_file[i], "<TD>\n");
        fprintf (html_file[i], "    <IMG \n");
        fprintf (html_file[i], "     ALT=\"NO data for this period\"\n");
        fprintf (html_file[i], "     SRC=\"../robotanim.gif\">\n");

        fprintf (html_file[i], "</TABLE>\n");
        switch (i) {
         case 0:
           fprintf (html_file[i], "<P>\n");
           fprintf (html_file[i], "<H3>DUST</H3>\n");
           fprintf (html_file[i], "It appears that we may not have any \n");
           fprintf (html_file[i], "data during this period.\n");
           break;
         case 1:
           fprintf (html_file[i], "<P>\n");
           fprintf (html_file[i], "It appears that we may not have any \n");
           fprintf (html_file[i], "80Khz data during this period.\n");
           break;
         case 2:
           fprintf (html_file[i], "<P>\n");
           fprintf (html_file[i], "It appears that we may not have any \n");
           fprintf (html_file[i], "10Khz data during this period.\n");
           break;
         case 3:
           fprintf (html_file[i], "<P>\n");
           fprintf (html_file[i], "It appears that we may not have any \n");
           fprintf (html_file[i], "HF-WBR data during this period.\n");
           break;
         case 4:
           fprintf (html_file[i], "<P>\n");
           fprintf (html_file[i], "It appears that we may not have any \n");
           fprintf (html_file[i], "dust reports during this period.\n");
           break;
        }
      }

      fprintf (html_file[i], "  <P>\n");
      fprintf (html_file[i], "  Page rendered by %s Version %s \n", argv[0],
               Version);
      fprintf (html_file[i], "  <br>\n");
      fprintf (html_file[i], "  using MDB library Version %s\n", MDB_Version);
      fprintf (html_file[i], "  <br>\n");
      fprintf (html_file[i], "  %s\n", now (1));
      fprintf (html_file[i], "   <p>\n");
      fprintf (html_file[i],
               "    <a href=\"http://validator.w3.org/check/referer\">\n");
      fprintf (html_file[i], "      <img\n");
      fprintf (html_file[i], "       src=\"../valid_html401.gif\"\n");
      fprintf (html_file[i],
               "       alt=\"Valid HTML 4.01!\" height=\"31\" width=\"88\">\n");
      fprintf (html_file[i], "      </a>\n");
      fprintf (html_file[i], "     </p>\n");
      fprintf (html_file[i], "  </BODY>\n");
      fprintf (html_file[i], "  </HTML>\n");
      fclose (html_file[i]);
      html_file[i] = NULL;
    }
  }
  return 1;
}

int make_script_entry (int flag,
                       FILE * time_file,
                       char *start_utc,
                       char *stop_utc,
                       char *coordinate,
                       char *subdir_name,
                       char *gif_name,
                       struct HFR_STRUCT *hfr_struct,
                       struct DUST_STRUCT *dust_struct)
{

  if (!force_flag) {
    fprintf (time_file, "if [ -s ");
    fprintf (time_file, "${GIF_DIRECTORY}/%s/", subdir_name);
    fprintf (time_file, "%s",
             build_gif_name (flag, gif_name, hfr_struct, dust_struct));
    fprintf (time_file, ".gif");
    fprintf (time_file, " ]; then");
    fprintf (time_file, "\n");
    fprintf (time_file, "    echo \"Already Processed %s.gif\"",
             build_gif_name (flag, gif_name, hfr_struct, dust_struct));
    fprintf (time_file, "\n");
    fprintf (time_file, "  else");
    fprintf (time_file, "\n");
  }

  fprintf (time_file, "    echo \"Processing %s.gif\"",
           build_gif_name (flag, gif_name, hfr_struct, dust_struct));
  fprintf (time_file, "\n");

 /****************************************************************************/

 /************************************************************ Build GIF *****/

 /****************************************************************************/
  switch (flag) {
   case 8:
     fprintf (time_file, "#\n");
     fprintf (time_file, "#    Note that the line plot doesn't always\n");
     fprintf (time_file, "#  work on real-time data.  We seem to have\n");
     fprintf (time_file, "#  difficulty extracting the desired record.\n");
     fprintf (time_file, "#    Probably has something to do with the\n");
     fprintf (time_file, "#  splice point where we transition from\n");
     fprintf (time_file, "#  post-pass to real-time.\n");
     fprintf (time_file, "#\n");
     break;
  }
  fprintf (time_file, "    ${VOLUME_PATH}/${VOLUME_SCRIPT}");
  switch (flag) {
   case 1:
     fprintf (time_file, "/browse_10Khz.perl ");
     break;
   case 2:
     fprintf (time_file, "/browse_80Khz.perl ");
     break;
   case 4:
     fprintf (time_file, "/browse_HFR.perl ");
     break;
   case 8:
     fprintf (time_file, "/wbrplot ");
     break;
  }
  switch (flag) {
   case 1:
   case 2:
   case 4:
     fprintf (time_file, "\"%s\" ", pick_start (start_utc, stop_utc));
     fprintf (time_file, "\"%s\" ", pick_stop (start_utc, stop_utc));
     break;
   case 8:
     fprintf (time_file, "\"%s\" ", ephem_time (dust_struct->et, -1.040));
     fprintf (time_file, "\"%s\" ", ephem_time (dust_struct->et, +1.040));
     break;
  }
  switch (flag) {
   case 1:
   case 2:
   case 4:
     fprintf (time_file, "%s ", coordinate);
     break;
  }
  switch (flag) {
   case 1:
     fprintf (time_file, "\"Cassini/RPWS WBR 10Khz\" ");
     break;
   case 2:
     fprintf (time_file, "\"Cassini/RPWS WBR 80Khz\" ");
     break;
   case 4:
     fprintf (time_file, "\"Cassini/RPWS HF-WBR %s\" ", hfr_struct->band);
     fprintf (time_file, "%d ", hfr_struct->band_edge[0]);
     fprintf (time_file, "%d", hfr_struct->band_edge[1]);
     break;
  }
  fprintf (time_file, "\n");

 /****************************************************************************/

 /****************************************************** Build Thumbnail *****/

 /****************************************************************************/
  fprintf (time_file, "    echo \"(1) thumbnail.sh\"");
  fprintf (time_file, "\n");
  fprintf (time_file, "    ${VOLUME_PATH}/${VOLUME_SCRIPT}/thumbnail.sh ");
  fprintf (time_file, "batch");
  fprintf (time_file, "\n");

 /****************************************************************************/

 /****************************************************** Debugging Stuff *****/

 /****************************************************************************/
  fprintf (time_file, "    ls -l batch*gif\n");

 /****************************************************************************/

 /************************************************************* Move GIF *****/

 /****************************************************************************/
  fprintf (time_file, "    mv batch.gif ");
  fprintf (time_file, "${GIF_DIRECTORY}/%s/", subdir_name);
  fprintf (time_file, "%s.gif",
           build_gif_name (flag, gif_name, hfr_struct, dust_struct));
  fprintf (time_file, "\n");

 /****************************************************************************/

 /******************************************************* Move Thumbnail *****/

 /****************************************************************************/
  fprintf (time_file, "    mv batch.tn.gif ");
  fprintf (time_file, "${GIF_DIRECTORY}/%s/", subdir_name);
  fprintf (time_file, "%s.tn.gif",
           build_gif_name (flag, gif_name, hfr_struct, dust_struct));
  fprintf (time_file, "\n");

 /****************************************************************************/

 /****************************************************************************/

 /****************************************************************************/
  if (!force_flag)
    fprintf (time_file, "  fi\n");

 /****************************************************************************/

 /****************************************************************************/

 /****************************************************************************/
  if (!force_flag) {
    fprintf (time_file, "    echo \"(2) thumbnail.sh\"");
    fprintf (time_file, "\n");
    fprintf (time_file, "#       Take a look at the thumbnail separately\n");
    fprintf (time_file, "if [ -s ");
    fprintf (time_file, "${GIF_DIRECTORY}/%s/", subdir_name);
    fprintf (time_file, "%s",
             build_gif_name (flag, gif_name, hfr_struct, dust_struct));
    fprintf (time_file, ".tn.gif");
    fprintf (time_file, " ]; then");
    fprintf (time_file, "\n");
    fprintf (time_file, "    echo \"Thumbnail Already Processed %s.tn.gif\"",
             build_gif_name (flag, gif_name, hfr_struct, dust_struct));
    fprintf (time_file, "\n");
    fprintf (time_file, "  else");
    fprintf (time_file, "\n");
  }

 /****************************************************************************/

 /**************************************************** Rebuild Thumbnail *****/

 /****************************************************************************/
  fprintf (time_file, "    echo \"Processing %s.tn.gif\"",
           build_gif_name (flag, gif_name, hfr_struct, dust_struct));
  fprintf (time_file, "\n");
  fprintf (time_file, "    ${VOLUME_PATH}/${VOLUME_SCRIPT}/thumbnail.sh ");
  fprintf (time_file, " ${GIF_DIRECTORY}/%s/%s",
           subdir_name,
           build_gif_name (flag, gif_name, hfr_struct, dust_struct));
  fprintf (time_file, "\n");

 /****************************************************************************/

 /****************************************************************************/

 /****************************************************************************/
  if (!force_flag)
    fprintf (time_file, "  fi\n");

 /****************************************************************************/

 /**************************************************** List files we did *****/

 /****************************************************************************/
  fprintf (time_file, "#       Now, let's take a look at our work\n");
  fprintf (time_file,
           "ls -l ${GIF_DIRECTORY}/%s/%s*",
           subdir_name,
           build_gif_name (flag, gif_name, hfr_struct, dust_struct));
  fprintf (time_file, "\n");
  fprintf (time_file, "###################################\n");
  fprintf (time_file, "\n");

 /****************************************************************************/
  fflush (time_file);
  return 1;
}

  /*
   * 
   */

  /*********************************************************
   ****							****
   ****	HFR downconver mode list.  Check element 0	****
   ****	    of the list and insert it if we haven't	****
   ****	    yet seen it					****
   ****							****
   *********************************************************/
int fix_hfr_struct (struct HFR_STRUCT *hfr_struct, int flag)
{
  int index;

  if (flag) {
    for (index = 0; index < HFR_STRUCT_INDEX; index++) {
      memset (hfr_struct[index].band, 0, 32);
      hfr_struct[index].band_edge[0] = 0;
      hfr_struct[index].band_edge[1] = 0;
    }
    return 1;
  } else {
    if (!hfr_struct[0].band_edge[0])
      return 0;
    if (!hfr_struct[0].band_edge[1])
      return 0;
    for (index = 1; index < HFR_STRUCT_INDEX; index++) {
      if (!strcmp (hfr_struct[index].band, hfr_struct[0].band))
        return 1;
      if (!hfr_struct[index].band[0]) {
        strcpy (hfr_struct[index].band, hfr_struct[0].band);
        hfr_struct[index].band_edge[0] = hfr_struct[0].band_edge[0];
        hfr_struct[index].band_edge[1] = hfr_struct[0].band_edge[1];
        return 1;;
      }
    }
  }
  return 0;
}

  /*
   * 
   */

  /*********************************************************
   ****							****
   ****	DUST downconver mode list.  Check element 0	****
   ****	    of the list and insert it if we haven't	****
   ****	    yet seen it					****
   ****							****
   *********************************************************/
int fix_dust_struct (struct DUST_STRUCT *dust_struct, char *time, int flag)
{
  int index;

  if (flag) {
    for (index = 0; index < DUST_STRUCT_INDEX; index++) {
      dust_struct[index].et = 0.0;
      memset (dust_struct[index].time, 0, 32);
    }
    return 1;
  } else {
    if (!time[0])
      return 0;
    for (index = 1; index < DUST_STRUCT_INDEX; index++) {
      if (!strcmp (dust_struct[index].time, time))
        return 1;
      if (dust_struct[index].time[0]) {
        if (!strcmp (dust_struct[index].time, time))
          return 1;
      } else {
        strcpy (dust_struct[index].time, time);
        utc2et_ (dust_struct[index].time,
                 &dust_struct[index].et, strlen (dust_struct[index].time));
        return 1;
      }
    }
  }
  return 0;
}

  /*
   * 
   */

  /*********************************************************
   ****							****
   ****	Generate script for one time period		****
   ****	    HFR_STRUCT has a list of HFR down convert	****
   ****		frequencies in the period		****
   ****	    wbr_mode is a mask of modes the WBR has	****
   ****		been in during the period.		****
   ****	    count is simply a record ocunt		****
   ****							****
   *********************************************************/
int make_script (char *start[],
                 char *stop[],
                 struct HFR_STRUCT *hfr_struct,
                 struct DUST_STRUCT *dust_struct, int wbr_mode, int count)
{
  char start_utc[64];
  char stop_utc[64];
  char dust_utc[64];
  char gif_name[64];
  char gif_dust[64];
  static char subdir_name[64] = "Missing";
  static FILE *time_file = NULL;
  static FILE *move_file = NULL;
  static char filename[64] = { "wbr_time_file.sh" };
  static char move_file_name[64] = { "wbr_time_move.sh" };
  char *coordinate;

  /*
   *      First time through...
   */

  if (!time_file) {
    if (start[0]) {
      strcpy (gif_name, start[0]);
      gif_name[8] = 0;
      sprintf (filename, "A_%s.sh",
               build_gif_name (0, gif_name, hfr_struct, dust_struct));
      sprintf (move_file_name, "a_%s.sh",
               build_gif_name (0, gif_name, hfr_struct, dust_struct));
      strcpy (subdir_name, subgen (start[0]));
    }
    time_file = fopen (filename, "w");
    if (verbose_file)
      fprintf (verbose_file, "%5d %p=fopen(\"%s\",\"w\");\n", __LINE__,
               time_file, filename);
    /**/ fprintf (time_file, "#!/bin/sh\n");
    fprintf (time_file, "#\n");
    fprintf (time_file,
             "# This is a shell script to produce Quick-Look images\n");
    fprintf (time_file, "#    Automatically Generated, do NOT edit!\n");
    fprintf (time_file, "#    Version:  %s\n", Version);
    fprintf (time_file, "#    Creation: %s\n", now (1));
    fprintf (time_file, "#\n");
    fprintf (time_file, "PGPLOT_DIR=\"/opt/local/pgplot\"\n");
    fprintf (time_file, "PGPLOT_FONT=\"/opt/local/pgplot/grfont.dat\"\n");
    fprintf (time_file, "export PGPLOT_DIR  \n");
    fprintf (time_file, "export PGPLOT_FONT \n");
    fprintf (time_file, "GIF_DIRECTORY=/opt/project/cassini/quicklook\n");
    fprintf (time_file, "VOLUME_PATH=/opt/project/cassini/src\n");
    fprintf (time_file, "VOLUME_SCRIPT=browse\n");
    fprintf (time_file, "#--start-- %d\n", __LINE__);

    move_file = fopen (move_file_name, "w");
    if (verbose_file)
      fprintf (verbose_file, "%5d %p=fopen(\"%s\",\"w\");\n", __LINE__,
               move_file, move_file_name);
    /**/ if (move_file) {
      fprintf (move_file, "#!/bin/sh\n");
      fprintf (move_file, "#\n");
      fprintf (move_file,
               "# This is shell script to produce Quick-Look images\n");
      fprintf (move_file, "#    Automatically Generated, do NOT edit!\n");
      fprintf (move_file, "#    Version:  %s\n", Version);
      fprintf (move_file, "#    Creation: %s\n", now (1));
      fprintf (move_file, "#\n");
      fprintf (move_file, "GIF_DIRECTORY=/opt/project/cassini/quicklook\n");
      fprintf (move_file, "VOLUME_PATH=/opt/project/cassini/src\n");
      fprintf (move_file, "VOLUME_SCRIPT=browse\n");
      fprintf (move_file, "#--start-- %d\n", __LINE__);
    }

    if (start[0]) {
      if (0) {
        fprintf (time_file, "# %s %d\n", __FILE__, __LINE__);
        fprintf (time_file, "echo \"Current Directory `pwd`\"\n");

        fprintf (time_file, "mkdir ${GIF_DIRECTORY}/%s\n", subdir_name);

        fprintf (time_file, "mv A_%s.html ${GIF_DIRECTORY}/%s/A_%s.html\n",
                 build_gif_name (0, gif_name, hfr_struct, dust_struct),
                 subdir_name,
                 build_gif_name (0, gif_name, hfr_struct, dust_struct));
        fprintf (time_file,
                 "mv A_%s_80.html ${GIF_DIRECTORY}/%s/A_%s_80.html\n",
                 build_gif_name (0, gif_name, hfr_struct, dust_struct),
                 subdir_name, build_gif_name (0, gif_name, hfr_struct,
                                              dust_struct));
        fprintf (time_file,
                 "mv A_%s_10.html ${GIF_DIRECTORY}/%s/A_%s_10.html\n",
                 build_gif_name (0, gif_name, hfr_struct, dust_struct),
                 subdir_name, build_gif_name (0, gif_name, hfr_struct,
                                              dust_struct));
        fprintf (time_file,
                 "mv A_%s_hf.html ${GIF_DIRECTORY}/%s/A_%s_hf.html\n",
                 build_gif_name (0, gif_name, hfr_struct, dust_struct),
                 subdir_name, build_gif_name (0, gif_name, hfr_struct,
                                              dust_struct));
        fprintf (time_file,
                 "mv A_%s_dust.html ${GIF_DIRECTORY}/%s/A_%s_dust.html\n",
                 build_gif_name (0, gif_name, hfr_struct, dust_struct),
                 subdir_name, build_gif_name (0, gif_name, hfr_struct,
                                              dust_struct));
      }
      if (move_file) {
        fprintf (move_file, "# %s %d\n", __FILE__, __LINE__);
        fprintf (move_file, "mkdir ${GIF_DIRECTORY}/%s\n", subdir_name);
        fprintf (move_file, "mv A_%s.html ${GIF_DIRECTORY}/%s/A_%s.html\n",
                 build_gif_name (0, gif_name, hfr_struct, dust_struct),
                 subdir_name,
                 build_gif_name (0, gif_name, hfr_struct, dust_struct));
        fprintf (move_file,
                 "mv A_%s_80.html ${GIF_DIRECTORY}/%s/A_%s_80.html\n",
                 build_gif_name (0, gif_name, hfr_struct, dust_struct),
                 subdir_name, build_gif_name (0, gif_name, hfr_struct,
                                              dust_struct));
        fprintf (move_file,
                 "mv A_%s_10.html ${GIF_DIRECTORY}/%s/A_%s_10.html\n",
                 build_gif_name (0, gif_name, hfr_struct, dust_struct),
                 subdir_name, build_gif_name (0, gif_name, hfr_struct,
                                              dust_struct));
        fprintf (move_file,
                 "mv A_%s_hf.html ${GIF_DIRECTORY}/%s/A_%s_hf.html\n",
                 build_gif_name (0, gif_name, hfr_struct, dust_struct),
                 subdir_name, build_gif_name (0, gif_name, hfr_struct,
                                              dust_struct));
        fprintf (move_file,
                 "mv A_%s_dust.html ${GIF_DIRECTORY}/%s/A_%s_dust.html\n",
                 build_gif_name (0, gif_name, hfr_struct, dust_struct),
                 subdir_name, build_gif_name (0, gif_name, hfr_struct,
                                              dust_struct));
        fprintf (move_file, "mv A_%s.sh ${GIF_DIRECTORY}/%s/A_%s.sh\n",
                 build_gif_name (0, gif_name, hfr_struct, dust_struct),
                 subdir_name, build_gif_name (0, gif_name, hfr_struct,
                                              dust_struct));
        fprintf (move_file, "cd ${GIF_DIRECTORY}/%s\n", subdir_name);
        fprintf (move_file, "./A_%s.sh\n",
                 build_gif_name (0, gif_name, hfr_struct, dust_struct));
      }
    }
  }
  /*
   *      Specify a starting time to cause
   *      record generation.
   */
  if (start[0]) {
    int index;

    coordinate = rpws_coordinate_name (start[0], start[0], 1, 0);
    strcpy (gif_name, start[0]);
    strcpy (start_utc, start[0]);
    strcpy (stop_utc, stop[0]);
    gif_name[11] = '-';
    gif_name[14] = 0;
    start_utc[18] = '0';
    start_utc[19] = '0';
    start_utc[20] = '0';
    stop_utc[18] = '9';
    stop_utc[19] = '9';
    stop_utc[20] = '9';
    fprintf (time_file, "#\n");
    fprintf (time_file, "#---------------------------\n");
    fprintf (time_file,
             "echo  \"Record Count %d  Time period  %s  %s\"\n",
             count, start_utc, stop_utc);
    fprintf (time_file, "#\n");
    /*
     *      HFR downconver mode(s)
     */
    if (wbr_mode & 4) {
      for (index = 1; index < HFR_STRUCT_INDEX; index++) {
        if ((hfr_struct[index].band_edge[0]) &&
            (hfr_struct[index].band_edge[1])) {
          make_script_entry (4, time_file,
                             start_utc,
                             stop_utc,
                             coordinate,
                             subdir_name,
                             gif_name,
                             &hfr_struct[index], &dust_struct[index]);
        }
      }
    }
    /*
     *      80 Khz modes
     */
    if (wbr_mode & 2) {
      make_script_entry (2, time_file,
                         start_utc,
                         stop_utc,
                         coordinate,
                         subdir_name,
                         gif_name, &hfr_struct[index], &dust_struct[index]);
    }
    /*
     *      10Khz modes
     */
    if (wbr_mode & 1) {
      make_script_entry (1, time_file,
                         start_utc,
                         stop_utc,
                         coordinate,
                         subdir_name,
                         gif_name, &hfr_struct[index], &dust_struct[index]);
    }
    /*
     *      DUST modes
     */
    if (wbr_mode & 8) {
      for (index = 1; index < DUST_STRUCT_INDEX; index++) {
        if (dust_struct[index].time[0]) {
          strcpy (dust_utc, dust_struct[index].time);
          make_script_entry (8, time_file,
                             dust_utc,
                             dust_utc,
                             coordinate,
                             subdir_name,
                             gif_name,
                             &hfr_struct[index], &dust_struct[index]);
        }
      }
    }
  } else
    /*
     *      No start time, the close files...
     *      leave the script executable...
     */
  {
    fprintf (time_file, "#--end-- %d\n", __LINE__);
    fprintf (time_file, "#  %s\n", now (1));
    fprintf (time_file, "#\n");
    fclose (time_file);
    time_file = NULL;
    chmod (filename, S_IRWXU | S_IRWXG | S_IROTH);

    if (move_file) {
      fprintf (move_file, "#--end-- %d\n", __LINE__);
      fclose (move_file);
      move_file = NULL;
      chmod (move_file_name, S_IRWXU | S_IRWXG | S_IROTH);
      sprintf (command, "./%s", move_file_name);
    /**/}
  }
  fix_hfr_struct (hfr_struct, 1);
  fix_dust_struct (dust_struct, NULL, 1);
  return 1;
}

  /*
   * 
   */

  /*********************************************************
   ****							****
   ****		Validity Checks				****
   ****		Look for valid length			****
   ****							****
   *********************************************************/
int browse_read_length_check (struct MP_buffer *buf)
{
  int status;
  int length;
  int band;
  int data_start;
  int data_length;

  status = 0;
  data_start = buf->packet.index.data_start;
  data_length = buf->packet.index.data_length;
  length = UTIL_MP_length (buf);
  band = get_status (buf->packet.mpp.mini_packet, WBR_frequency_band, 0);

   /*************************************************************
    *	Normal packet lengths, 1K, 1.5K, 2K,3K, 4K, and 5K	*
    *************************************************************/
  if ((length >= 1024 + 5) && (length <= 1024 + 9))
    status = 1;
  if ((length >= 1536 + 5) && (length <= 1536 + 9))
    status = 1;
  if ((length >= 2048 + 5) && (length <= 2048 + 9))
    status = 1;
  if ((length >= 3072 + 5) && (length <= 3072 + 9))
    status = 1;
  if ((length >= 4096 + 5) && (length <= 4096 + 9))
    status = 1;
  if ((length >= 5120 + 5) && (length <= 5120 + 9))
    status = 1;

   /*************************************************************
    *	Odd (known) packet lengths, 1450			*
    *************************************************************/
  if ((length >= 1450 + 5) && (length <= 1450 + 9))
    status = 1;

  if (data_start == 1454)               /* Saturn */
    status = 1;

  if (band)
    return status;

   /*************************************************************
    *	Compress, WC-IN, Low Band ONLY!!!			*
    *************************************************************/
  if ((length >= 1200) && (length <= 1800)) {
    if ((data_start > 400) && (data_start < 1000))
      status = 1;
  }
  if ((length >= 2400) && (length <= 3600)) {
    if ((data_start > 800) && (data_start < 2000))
      status = 1;
  }
  if ((length >= 3600) && (length <= 5400)) {
    if ((data_start > 1200) && (data_start < 3000))
      status = 1;
  }
  if ((data_start > 1548) && (data_start < 1552)) {
    if ((length >= 1500) && (length <= 10000))
      status = 1;
  }
  return status;
}
struct MP_buffer *browse_read (struct MDB *mdb_file)
{
  struct MP_buffer *buf;
  int bad_data = 1;
  int i;

  while (bad_data) {
    bad_data = 0;
    buf = (struct MP_buffer *) MDB_read_stream (mdb_file, data_type);
    if (buf) {
      if (!(browse_read_length_check (buf)))
        bad_data = 1;
    }
  }
  if (0) {
    if (verbose_file) {
      fprintf (verbose_file, "%5d browse_read CDS SCLK %02X%02X%02X%02X\n",
               __LINE__,
               buf->packet.cds_tag.begin[0],
               buf->packet.cds_tag.begin[1],
               buf->packet.cds_tag.begin[2], buf->packet.cds_tag.begin[3]);
      for (i = 0; i < 32; i++) {
        if (i % 16 == 0)
          fprintf (verbose_file, "%5d %04X:", __LINE__, i);
        fprintf (verbose_file, " %02X", buf->packet.mpp.mini_packet[i]);
        if (i % 16 == 15)
          fprintf (verbose_file, "\n     ");
      }
      fprintf (verbose_file, "\n");
      fflush (verbose_file);
    }
  }
  return buf;
}

  /*
   * 
   */

  /*********************************************************
   ****							****
   ****		Return 0 if this is dust data		****
   ****							****
   ****							****
   *********************************************************/
this_is_not_dust (struct MP_buffer * buf)
{
  int status = 0;
  int length;
  int msf;
  int i;

/*
 *	Length must be 1024 samples
 */
  length = UTIL_MP_length (buf);
  switch (length) {
   default:
     return 1;
     break;
   case 1029:
   case 1031:
     break;
  }

/*
 *	offset to data, look for lower 4 bits are zero
 */
  msf = get_status (buf->packet.mpp.mini_packet, WBR_MSF, 0) & 0x01;
  msf *= 2;
  status = 1024;
  for (i = 0; i < 1024; i++) {
    if ((buf->packet.mpp.mini_packet[i + 8 + msf] & 0x0F) == 0)
      status--;
  }

  if (status)
    return status;

/*
 *	look for zero-filled packets
 */
  status = 0;
  for (i = 0; i < 1024; i++) {
    if ((buf->packet.mpp.mini_packet[i + 8 + msf] & 0xFF) == 0)
      status++;
  }

  if (status > 1000)
    return status;

  status = 0;
  return status;
}

char *build_time (int offset)
{
  struct DATABASE_TIME_TAG orig_time;
  struct DATABASE_TIME_TAG new_time;
  char *time;
  static char orig_time_string[32];
  static char new_time_string[8][32];
  static int new_time_index = 0;
  int temp;

  new_time_index++;
  new_time_index &= 7;

  time = now (0);
  parsetime (time,
             &orig_time.year,
             &orig_time.month,
             &orig_time.mday,
             &orig_time.yday,
             &orig_time.hour, &orig_time.minute, &orig_time.second);

  memcpy (&new_time, &orig_time, sizeof (struct DATABASE_TIME_TAG));
  temp = new_time.yday % abs (offset);
  new_time.yday -= temp;
  if (offset > 0)
    new_time.yday += offset;
  new_time.month = 1;
  new_time.mday = new_time.yday;

  tnorm (&new_time.year,
         &new_time.month,
         &new_time.mday,
         &new_time.yday, &new_time.hour, &new_time.minute, &new_time.second);

  sprintf (orig_time_string, "%04d-%03dT%02d:%02d:%06.3f",
           orig_time.year, orig_time.yday, 0, 0, 0.0);
  sprintf (new_time_string[new_time_index], "%04d-%03dT%02d:%02d:%06.3f",
           new_time.year, new_time.yday, 0, 0, 0.0);
  return new_time_string[new_time_index];
}

  /*
   * 
   */

  /*********************************************************
   ****							****
   ****		Scan database (using mdb)		****
   ****		between specified start/stop times	****
   ****							****
   *********************************************************/
int main (int argc, char *argv[])
{
  struct MDB *mdb_file;                 /* pointer to mdb control structure */
  struct MP_buffer *buf;                /* pointer to data buffer */
  struct HFR_STRUCT hfr_struct[HFR_STRUCT_INDEX];
  struct DUST_STRUCT dust_struct[DUST_STRUCT_INDEX];
  char *database_path = NULL;
  char *database_file = NULL;
  char *start_time = NULL;
  char *stop_time = NULL;
  char new_utc[64];
  char *start_utc[16];
  char *stop_utc[16];
  char c;
  char *ctemp;
  int wbr_mode = 0;
  int current_mode = 0;
  int antenna;
  int band;
  double fband;
  int i;
  int first = 1;
  int record_count = 0;
  int execute_flag = 0;
  struct SPICE_TIME spice_time_array;
  float f;
  double event_et;
  double delta_t;
  double stop_et = -3.0 * 365.25 * 86400;       /* 3 years of seconds before J2000 */
  struct event_clock sclk;

  for (i = 0; i < 16; i++) {
    start_utc[i] = malloc (64);
    stop_utc[i] = malloc (64);
  }

  if (argc < 2) {
    exit (0);
  }

  while ((c = getopt (argc, argv, "p:n:d:o:xv:flr:h")) != EOF) {
    switch (c) {
     case 'r':
       nert_flag = atoi (optarg);
       break;
     case 'p':
       database_path = optarg;
       break;
     case 'n':
       database_file = optarg;
       break;
     case 'd':
       max_delta = atoi (optarg);
       break;
     case 'o':
       daily_offset = atoi (optarg);
       break;
     case 'x':
       execute_flag = 1;
       break;
     case 'f':
       force_flag = 1;
       break;
     case 'l':
       length_flag = 1;
       break;
     case 'v':
       verbose_file = fopen (optarg, "a");
       break;
     case '?':
       fprintf (stdout, "Invalid Command Line Arguments\n");
     case 'h':
       fprintf (stdout, "    %s %s\n", Title, Version);
       fprintf (stdout, "\n");
       fprintf (stdout, "  %s <flags> <start-time <stop-time>>\n", argv[0]);
       fprintf (stdout, "\n");
       fprintf (stdout, "    -p      database path\n");
       fprintf (stdout, "    -n      database name\n");
       fprintf (stdout, "    -d      maximum delta (%d) seconds\n",
                max_delta);
       fprintf (stdout, "    -o      day offset (%d) days\n", daily_offset);
       fprintf (stdout, "    -v name verbose output to <file>\n");
       fprintf (stdout, "    -x      execute after scan\n");
       fprintf (stdout, "    -f      force regeneration of plots\n");
       fprintf (stdout, "    -r n    real-time (nert_flag)\n");
       fprintf (stdout, "            0: non real-time\n");
       fprintf (stdout, "            1: near real time\n");
       fprintf (stdout, "            2: post/pass and real-time\n");
       fprintf (stdout, "        \n");
       fprintf (stdout,
                "        If no start/stop is specified, the current\n");
       fprintf (stdout,
                "        date and 'day offset' are used to construct\n");
       fprintf (stdout,
                "        start and stop time.  In order to generate\n");
       fprintf (stdout,
                "        start/stop times, at least 1 flag is required.\n");
       fprintf (stdout, "        \n");
       fprintf (stdout,
                "        Time is standard format for Cassini, i.e.\n");
       fprintf (stdout, "           yyy-dddThh:mm:ss  (minimum yyyy-dddT)\n");
       fprintf (stdout, "        \n");
       fflush (stdout);
       exit (0);
    }
  }

  if (verbose_file)
    fprintf (verbose_file, "%5d %s %s\n", __LINE__, Title, Version);

  if (verbose_file)
    fprintf (verbose_file, "%5d ldpool_ %s\n", __LINE__, leapfile);
  ldpool_ (leapfile, strlen (leapfile));
  if (verbose_file)
    fprintf (verbose_file, "%5d ldpool_ %s\n", __LINE__, sclkfile);
  ldpool_ (sclkfile, strlen (sclkfile));
  if (verbose_file)
    fflush (verbose_file);


     /********************************
      *  Start/Stop time             *
      *   Accept start only          *
      *   as well as start/stop      *
      ********************************/
  switch (argc - optind) {
   case 2:
     start_time = argv[optind + 0];
     stop_time = argv[optind + 1];
     break;
   case 1:
     start_time = argv[optind + 0];
     stop_time = NULL;
     break;
   case 0:
     start_time = build_time (-daily_offset);
     stop_time = build_time (+daily_offset);
     break;
   default:
     fprintf (stdout, "Invalid Command Line Arguments\n");
     fflush (stdout);
     exit (0);
  }
  if (start_time) {
    i = MDB_SCET_strg (start_time);
    /**/ if (verbose_file) {
      fprintf (verbose_file, "%5d start time %08X=MDB_SCET_strg(\"%s\");\n",
               __LINE__, i, start_time);
      /**/ fflush (verbose_file);
    }
  }
  if (stop_time) {
    i = MDB_SCET_strg (stop_time);
    /**/ if (verbose_file) {
      fprintf (verbose_file, "%5d stop time %08X=MDB_SCET_strg(\"%s\");\n",
               __LINE__, i, stop_time);
      /**/ fflush (verbose_file);
    }
  }

  if (verbose_file) {
    fprintf (verbose_file, "%5d MDB_open(", __LINE__);
    if (start_time)
      fprintf (verbose_file, "%s, ", start_time);
    else
      fprintf (verbose_file, "NULL, ");
    if (stop_time)
      fprintf (verbose_file, "%s, ", stop_time);
    else
      fprintf (verbose_file, "NULL, ");
    if (database_path)
      fprintf (verbose_file, "%s, ", database_path);
    else
      fprintf (verbose_file, "NULL, ");
    if (database_file)
      fprintf (verbose_file, "%s, ", database_file);
    else
      fprintf (verbose_file, "NULL, ");
    fprintf (verbose_file, "%d);\n", file_type);
    fflush (verbose_file);
  }


  fix_hfr_struct (hfr_struct, 1);
  fix_dust_struct (dust_struct, NULL, 1);
  mdb_file =
    MDB_open (start_time, stop_time, database_path, database_file, file_type);
  /**/ buf = browse_read (mdb_file);
  record_count++;
  if (buf) {
    while (buf) {
      sclk.seconds = UTIL_event_time (buf, 0);
      sclk.fine = UTIL_extract_MP_RTI (buf) << 5;
      event_et = spice_time_x (&sclk, &spice_time_array, new_utc);
      delta_t = event_et - stop_et;

      strcpy (hfr_struct[0].band,
              frq_hfr_wbr (get_status
                           (buf->packet.mpp.mini_packet, WBR_HFR_translate,
                            0)));
      fband = strtod (hfr_struct[0].band, &ctemp);
      if (*ctemp == 'M')
        fband *= 1000.;
      hfr_struct[0].band_edge[0] = fband - 15;
      hfr_struct[0].band_edge[1] = fband + 15;
      antenna = get_status (buf->packet.mpp.mini_packet, WBR_antenna, 0);
      band = get_status (buf->packet.mpp.mini_packet, WBR_frequency_band, 0);
      current_mode = 0;
      if (antenna == WBR_ANTENNA_HF) {  /* HF-WBR */
        current_mode = 4;
        /*
         * new_wbr_mode |= current_mode; /*
         */
        fix_hfr_struct (hfr_struct, 0);
      } else if (band == WBR_BAND_80_KHZ) {     /* band=1 for 80Khz */
        current_mode = 2;
        /*
         * new_wbr_mode |= current_mode; /*
         */
      } else if (this_is_not_dust (buf)) {      /* else band must bew 0 (10Khz) */
        current_mode = 1;
        /*
         * new_wbr_mode |= current_mode; /*
         */
      } else {
        current_mode = 8;
        /*
         * new_wbr_mode |= current_mode; /*
         */
        fix_dust_struct (dust_struct, new_utc, 0);
      }
      if (1) {
        if (verbose_file) {
          fprintf (verbose_file, "%5d %X.%04X  %s\n", __LINE__, sclk.seconds,
                   sclk.fine >> 4, new_utc);
          fprintf (verbose_file,
                   "      10/80/HF/Dust select  %02X=current_mode  %02X=wbr_mode  ",
                   current_mode, wbr_mode);
          fprintf (verbose_file, "%s", current_mode & 1 ? "10Khz " : "");
          fprintf (verbose_file, "%s", current_mode & 2 ? "80Khz " : "");
          fprintf (verbose_file, "%s", current_mode & 4 ? "HF-WBR " : "");
          fprintf (verbose_file, "%s", current_mode & 8 ? "Dust " : "");
          fprintf (verbose_file, "\n");
          fprintf (verbose_file, "      %s  band %d(%.3f)%s  antenna %d\n",
                   hfr_struct[0].band, band, fband, ctemp, antenna);
          fprintf (verbose_file,
                   "      %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X %02X \n",
                   buf->packet.pkt.packet[0], buf->packet.pkt.packet[1],
                   buf->packet.pkt.packet[2], buf->packet.pkt.packet[3],
                   buf->packet.pkt.packet[4], buf->packet.pkt.packet[5],
                   buf->packet.pkt.packet[6], buf->packet.pkt.packet[7],
                   buf->packet.pkt.packet[8], buf->packet.pkt.packet[9],
                   buf->packet.pkt.packet[10], buf->packet.pkt.packet[11],
                   buf->packet.pkt.packet[12], buf->packet.pkt.packet[13],
                   buf->packet.pkt.packet[14], buf->packet.pkt.packet[15]);
          fflush (verbose_file);
        }
      }
      if (first) {
        first = 0;
        wbr_mode |= current_mode;
      }
      if (delta_t < max_delta) {
        wbr_mode |= current_mode;
      } else {
        if (stop_utc[0][0]) {
          if (verbose_file) {
            fprintf (verbose_file,
                     "%5d 10/80/HF/Dust select  %02X=wbr_mode  ", __LINE__,
                     wbr_mode);
            fprintf (verbose_file, "%s", wbr_mode & 1 ? "10Khz " : "");
            fprintf (verbose_file, "%s", wbr_mode & 2 ? "80Khz " : "");
            fprintf (verbose_file, "%s", wbr_mode & 4 ? "HF-WBR " : "");
            fprintf (verbose_file, "%s", wbr_mode & 8 ? "Dust " : "");
            fprintf (verbose_file, "  *************\n");
            fprintf (verbose_file,
                     "                                 to %s (%.3f) %d\n",
                     stop_utc[0], delta_t, record_count);
            fflush (verbose_file);
          }
          make_html (start_utc,
                     stop_utc,
                     hfr_struct, dust_struct, wbr_mode, record_count, argv);
          make_script (start_utc,
                       stop_utc,
                       hfr_struct, dust_struct, wbr_mode, record_count);
          for (i = 1; i < 16; i++) {
            start_utc[i][0] = 0;
            stop_utc[i][0] = 0;
          }
          wbr_mode = current_mode;
          record_count = 0;
        }
        if (delta_t > 990000.)
          delta_t = 0.;
        if (verbose_file) {
          fprintf (verbose_file, "%5d WBR %10.3f %08X.%X  %s", __LINE__,
                   delta_t, sclk.seconds, sclk.fine >> 5, new_utc);
          if (strlen (start_utc[0]))
            fprintf (verbose_file, "%s", start_utc[0]);
          if (strlen (stop_utc[0]))
            fprintf (verbose_file, "%s", stop_utc[0]);
          fprintf (verbose_file, "\n");
          fflush (verbose_file);
        }
        strcpy (start_utc[0], new_utc);
        strcpy (start_utc[current_mode], new_utc);
      }
      stop_et = event_et;
      strcpy (stop_utc[0], new_utc);
      strcpy (stop_utc[current_mode], new_utc);
      buf = browse_read (mdb_file);
      record_count++;
    }
    if (stop_utc[0][0]) {
      if (verbose_file) {
        fprintf (verbose_file, "                              to %s\n",
                 stop_utc[0]);
        fflush (verbose_file);
      }
      make_html (start_utc, stop_utc, hfr_struct, dust_struct, wbr_mode,
                 record_count, argv);
      make_script (start_utc, stop_utc, hfr_struct, dust_struct, wbr_mode,
                   record_count);
      for (i = 1; i < 16; i++) {
        start_utc[i][0] = 0;
        stop_utc[i][0] = 0;
      }
      wbr_mode = current_mode;
      record_count = 0;
    }
  }
  start_utc[0] = NULL;
  make_html (start_utc, stop_utc, hfr_struct, dust_struct, 0, 0, argv);
  make_script (start_utc, stop_utc, hfr_struct, dust_struct, 0, 0);
  for (i = 1; i < 16; i++) {
    start_utc[i][0] = 0;
    stop_utc[i][0] = 0;
  }
  MDB_close (mdb_file);
  if (execute_flag) {
    system (command);
  }
  if (verbose_file) {
    fclose (verbose_file);
    verbose_file = NULL;
  }
}
