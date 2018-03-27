
/********************************************************************
 ****		rpws_master_browse.c				*****
 ****	Build an appropriate browse file for WBR/WFR data	*****
 ****	files.  						*****
 ********************************************************************/
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <das2/das1.h>

#include <fg.h>

/* Cassini Stuff */
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>
#include <mp_status.h>
#include <wbr_status.h>
#include <wfr_status.h>
#include <archive.h>

/* Local Stuff */
#include "bis.h"
#include "rpws_label.h"
#include "rpws_direct.h"
#include "rpws_browse.h"
#include "rpws_master_browse.h"
#include "rpws_hfr_status.h"


static char *Version = { "V3.5" };
static int comment_flag = 0;
extern int bisflag;
extern struct RPWS_LABEL *RPWS_BROWSE_data_record[16];
extern int TIMEFLAG;
static char *type[] = { "",
  "BROWSE_25HZ",
  "BROWSE_2500HZ",
  "BROWSE_10KHZ",
  "BROWSE_75KHZ",
  "file_5" ""
};
static char *type_2[] = { "",
  "25HZ",
  "2_5KHZ",
  "10KHZ",
  "75KHZ",
  "file_5" ""
};
static char *title_type[] = { "",
  "Waveform Receiver 25Hz",
  "Waveform Receiver 2.5kHz",
  "Wideband Receiver 10kHz",
  "Wideband Receiver 75kHz and HF",
  "file_5" ""
};
static char *band_[] = { "_25HZ",
  "_2500HZ",
  "_10KHZ",
  "_75KHZ",
  ""
};

/* ************************************************************************* */
int dump (FILE * shit, struct RPWS_LABEL *detail_record)
{
  fprintf (shit, "<BR");

#ifdef DEBUG
  fprintf (shit, " line=%d", __LINE__);
#endif

  fprintf (shit, ">\n");
  fprintf (shit, "Temporary debugging Information\n");
  fprintf (shit, "<BR><PRE>\n");
  if (detail_record->filepath1)
    fprintf (shit, "   detail_record->filepath1 %s\n",
             detail_record->filepath1);
  if (detail_record->filepath2)
    fprintf (shit, "   detail_record->filepath2 %s\n",
             detail_record->filepath2);
  if (detail_record->filepath3)
    fprintf (shit, "   detail_record->filepath3 %s\n",
             detail_record->filepath3);
  fprintf (shit, "   detail_record->filename  %s\n", detail_record->filename);
  fprintf (shit, "                      THUMB %s\n",
           detail_record->thumbname);
  fprintf (shit, "                       BAND %d\n", detail_record->fband);
  fprintf (shit, "</PRE><BR>\n");
  fflush (shit);
  return 0;
}

/* ************************************************************************* */
char *build_band_comment (struct RPWS_LABEL *detail_record)
{
  static char *band_comment[] = {
    "25 Hz WFR",
    "2.5kHz WFR",
    "10 kHz WBR",
    "75kHz WBR",
    ""
  };
  static char result[128];
  int fcenter = 0;

  memset (result, 0, sizeof (result));
  if (detail_record->hfr_xlate) {
    fcenter = rpws_hfr_xlate_center_freq (detail_record->hfr_xlate);
    if (fcenter)
      sprintf (result, " %dkHz HFWBR", fcenter);
  } else
    strcpy (result, band_comment[detail_record->fband]);
  return result;
}

int html_comment (FILE * html_file, char *text)
{
  if (comment_flag & 0x10000)
    fprintf (html_file, "<!-- %s -->\n", text);
  return 1;
}

/* ************************************************************************* */
/* Makes the header sections of most High-Rate browse HTML files */

int rpws_master_browse_html_header_text(
	FILE * html_file, int index, char *text, char *other_name, char *other_text
){
  if (!html_file) {
    fprintf (stdout, "%s/%dFAIL, html_file not open!!!\n", __FILE__,
             __LINE__);
    exit (0);
  }
  if (comment_flag & 0x100000)
    fprintf (html_file,
             "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">\n");
  else
    fprintf (html_file,
             "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 3.2//EN\">\n");

  fprintf (html_file, 
"<HTML>\n"
"<HEAD>\n"
"  <META NAME=\"Keywords\" CONTENT=\"Cassini/RPWS %s\">\n"
"  <META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=ISO-8859-1\">\n"
"  <META NAME=\"Producer\" CONTENT=\"%s\">\n"
"  <META NAME=\"Version\" CONTENT=\"%s\">\n", type[index], __FILE__, Version
	);

#ifdef DEBUG
  fprintf (html_file, "  <META NAME=\"BISflag\" CONTENT=\"0x%X\">\n",
           bisflag);
#endif

  fprintf (html_file, "  <TITLE>");
  fprintf (html_file, "      %s Browse Image Selection    \n", type[index]);
  fprintf (html_file, "    </TITLE>\n");
  if (comment_flag & 0x100000) {
    fprintf (html_file, "  <STYLE type=\"text/css\">\n");
    fprintf (html_file, "      H1 {text-align: center}\n");
    fprintf (html_file, "      P.right { text-align: right}\n");
    fprintf (html_file, "      P.center { text-align: center}\n");
    fprintf (html_file, "      P.left { text-align: left}\n");
    fprintf (html_file, "      P.hustify { text-align: hustify}\n");
    fprintf (html_file, "    </STYLE>\n");
  }
  fprintf (html_file, "  </HEAD>\n");
  fprintf (html_file, "<BODY>\n");
  fprintf (html_file, "<H1>%s</H1>\n", title_type[index]);
  if (other_name) {
    fprintf (html_file, "<P");
    if (comment_flag & 0x100000)
      fprintf (html_file, " class=\"right\"");
    fprintf (html_file, ">\n");
    if (bisflag & BISPARENT) {
      fprintf (html_file, "  <A Href=\"../\">Parent Directory</A>\n");
      fprintf (html_file, "<BR");

#ifdef DEBUG
      fprintf (html_file, " line=%d", __LINE__);
#endif

      fprintf (html_file, ">\n");
    }
    if (bisflag & BISOTHER) {
      fprintf (html_file, "  <A Href=\"%s\">%s</A>\n", other_name, other_text);
      fprintf (html_file, "  </P>\n");
    }
    fprintf (html_file, "<HR>\n");
  }
  if (text)
    fprintf (html_file, "<%s", text);
  if (bisflag & BISCELLSPACING)
    fprintf (html_file, " cellspacing=10");
  fprintf (html_file, ">\n");
  fflush (html_file);
  return 1;
}

/* ************************************************************************* */
int other (int index)
{
  int result = index;

  switch (index) {
   case 1:
     result = 2;
     break;
   case 2:
     result = 1;
     break;
   case 3:
     result = 4;
     break;
   case 4:
     result = 3;
     break;
  }
  return result;
}

/* ************************************************************************* */

FILE *rpws_master_browse_html_header (int index, char *w_directory[],
                                      struct RPWS_LABEL * detail_record)
{
  char temp[256];
  static FILE *html_file;
  static char pathname[256];
  static char other_name[256];

  strcpy (pathname, w_directory[RPWS_ARCHIVE_BROWSE]);
  strcat (pathname, "/");

  strcat (pathname,
          w_directory[detail_record->instrument + RPWS_ARCHIVE_BROWSE_WBR]);
  strcat (pathname, "/");

  strcat (pathname, detail_record->filepath1);
  strcat (pathname, "/");
  strcat (pathname, type[index]);
  strcat (pathname, "_MA.HTM");

  strcpy (other_name, type[other (index)]);
  strcat (other_name, "_MA.HTM");

  html_file = fopen (pathname, "w");
  rpws_master_browse_html_header_text (html_file,
                                       index,
                                       "UL",
                                       other_name, title_type[other (index)]);
  sprintf (temp, "DBG line:%3d other_name %s", __LINE__, other_name);
  html_comment (html_file, temp);
  sprintf (temp, "DBG line:%3d   pathname %s", __LINE__, pathname);
  html_comment (html_file, temp);
  return html_file;
}

/* ************************************************************************* */
int rpws_master_browse_html_trailer_text (FILE * html_file,
                                          char *text,
                                          int count,
                                          char *other_name, char *other_text)
{
  int i;

  if (text)
    fprintf (html_file, "  </%s>\n", text);

  fprintf (html_file, "<HR>\n");
  fprintf (html_file, "<P");
  if (comment_flag & 0x100000)
    fprintf (html_file, " class=\"right\"");
  fprintf (html_file, ">\n");
  if (bisflag & BISPARENT) {
    fprintf (html_file, "  <A Href=\"../\">Parent Directory</A>\n");
  }
  if (other_name) {
    if (bisflag & BISOTHER) {
      fprintf (html_file, "  <BR");

#ifdef DEBUG
      fprintf (html_file, " line=%d", __LINE__);
#endif

      fprintf (html_file, ">\n");
      fprintf (html_file, "  <A Href=\"%s\">%s</A>\n", other_name,
               other_text);
    }
  }
  fprintf (html_file, "  </P>\n");
  fprintf (html_file, "    ");
  if (bisflag & BISVERSION)
    fprintf (html_file, "<!-- ");
  fprintf (html_file, "Produced by %s %s", __FILE__, Version);
  if (bisflag & BISVERSION)
    fprintf (html_file, " -->");
  fprintf (html_file, "\n");
  fprintf (html_file, "<P>\n");
  fprintf (html_file,
           "  <A Href=\"http://validator.w3.org/check/referer\">\n");
  fprintf (html_file, "    <Img border=\"0\"	\n");
  fprintf (html_file, "     Src=\"");
  for (i = 0; i < count; i++)
    fprintf (html_file, "../");
  if (comment_flag & 0x100000) {
    fprintf (html_file, "ANCILLARY/VALID_HTML401.PNG\"\n");
    fprintf (html_file,
             "     Alt=\"Valid HTML 4.01!\" Height=\"31\" Width=\"88\">\n");
  } else {
    fprintf (html_file, "ANCILLARY/VALID_HTML32.PNG\"\n");
    fprintf (html_file,
             "     Alt=\"Valid HTML 3.2!\" Height=\"31\" Width=\"88\">\n");
  }
  fprintf (html_file, "    </a>\n");
  fprintf (html_file, "  </P>\n");

#ifdef DEBUG
  fprintf (html_file, "    <!-- %s/%d -->\n", __FILE__, __LINE__);
#endif

  fprintf (html_file, "</BODY>\n");
  fprintf (html_file, "</HTML>\n");
  fflush (html_file);
  return 0;
}

/* ************************************************************************* */
/* Generate the HTML table entry for hourly Wideband data pages */

int rpws_master_browse_html_detail_001 (
	FILE * html_file, struct RPWS_LABEL *detail_record
){
  char pathname[256];
  char temp[256];

  strcpy (pathname, detail_record->filepath2);


  sprintf (temp, "DBG line:%3d  detail_record->thumbname  %s", __LINE__,
           detail_record->thumbname);
  html_comment (html_file, temp);
  sprintf (temp, "DBG line:%3d  detail_record->filename   %s", __LINE__,
           detail_record->filename);
  html_comment (html_file, temp);
  sprintf (temp, "DBG line:%3d  band_comment              %s", __LINE__,
           build_band_comment (detail_record));
  html_comment (html_file, temp);
  sprintf (temp, " DBG line:%3d scet start                %s", __LINE__,
           detail_record->scet_start);
  html_comment (html_file, temp);
  sprintf (temp, " DBG line:%3d scet start 2              %s", __LINE__,
           detail_record->scet_start_2);
  html_comment (html_file, temp);
  sprintf (temp, " DBG line:%3d scet start 3              %s", __LINE__,
           detail_record->scet_start_3);
  html_comment (html_file, temp);

#ifdef DEBUG
  fprintf (html_file, "    <!-- %s/%d -->\n", __FILE__, __LINE__);
#endif

  fprintf (html_file, "  <!-- Plot Start: %s -->\n", detail_record->plot_start);
  fprintf (html_file, "  <!-- Plot Stop: %s -->\n", detail_record->plot_stop);
  fprintf (html_file, "  <TD>\n");
  fprintf (html_file, "      <A Href=\"HTML/%s.HTM\">\n", detail_record->thumbname);
  fprintf (html_file, "        <Img\n");
  fprintf (html_file, "          Alt=\"%s thumbnail Spectrogram\"\n", detail_record->thumbname);
  fprintf (html_file, "          Src=\"HTML/%s_TN.PNG\">\n",  detail_record->thumbname);
  /* fprintf (html_file, "          Width=128\n");
  fprintf (html_file, "          Height=96>\n"); */
		  
  if (bisflag & BISDETAIL)
    fprintf (html_file, "            %s\n", detail_record->filename);
  fprintf (html_file, "      </A>\n");

  if (bisflag & BISBAND) {
    fprintf (html_file, "      <BR");

#ifdef DEBUG
    fprintf (html_file, " line=%d", __LINE__);
#endif

    fprintf (html_file, ">\n");
    fprintf (html_file, "          %s\n", build_band_comment (detail_record));
  }

  if ((bisflag & BISDAY) || (bisflag & BISSTART)) {
    fprintf (html_file, "      <BR");

#ifdef DEBUG
    fprintf (html_file, " line=%d", __LINE__);
#endif

    fprintf (html_file, ">\n");
    switch (TIMEFLAG) {
     case 3:
       fprintf (html_file, "          %s\n", detail_record->scet_start_3);
       break;
     case 2:
       fprintf (html_file, "          %s\n", detail_record->scet_start_2);
       break;
     case 1:
       fprintf (html_file, "          %s\n", detail_record->scet_start);
       break;
    }
  }
  if (bisflag & BISSTOP) {
    fprintf (html_file, "      <BR");

#ifdef DEBUG
    fprintf (html_file, " line=%d", __LINE__);
#endif

    fprintf (html_file, ">\n");
    fprintf (html_file, "          Stop %s\n", detail_record->scet_stop);
  }

  /*
   * dump(html_file, detail_record); /*
   */

  fprintf (html_file, "  <BR");

#ifdef DEBUG
  fprintf (html_file, " line=%d", __LINE__);
#endif

  fprintf (html_file, ">\n");
  fflush (html_file);
  return 1;
}

/* ************************************************************************* */
int rpws_master_browse_html_detail_010(
	FILE * html_file, struct RPWS_LABEL *detail_record
){
  char pathname[256];

  strcpy (pathname, detail_record->filepath2);

#ifdef DEBUG
  fprintf (html_file, "    <!-- %s/%d -->\n", __FILE__, __LINE__);
#endif

  fprintf (html_file, "  <!-- Plot Start: %s -->\n",
           detail_record->plot_start);
  fprintf (html_file, "  <!-- Plot Stop: %s -->\n", detail_record->plot_stop);
  fprintf (html_file, "<TD>\n");
  fprintf (html_file, "    <A Href=\"%s/HTML/%s.HTM\">\n", pathname,
           detail_record->thumbname);
  fprintf (html_file, "    <Img\n");
  fprintf (html_file, "      Alt=\"%s thumbnail Spectrogram\"\n",
           detail_record->thumbname);
  fprintf (html_file, "      Src=\"%s/HTML/%s_TN.PNG\"\n", pathname,
           detail_record->thumbname);
  /* fprintf (html_file, "      Width=128\n");
  fprintf (html_file, "      Height=96\n"); */
  fprintf (html_file, "      >\n");
  if (bisflag & BISDETAIL) {
    fprintf (html_file, "    <BR");

#ifdef DEBUG
    fprintf (html_file, " line=%d", __LINE__);
#endif

    fprintf (html_file, ">\n");
    fprintf (html_file, "        %s\n", detail_record->filename);
  }
  fprintf (html_file, "    </A>\n");
  if (bisflag & BISBAND) {
    fprintf (html_file, "    <BR");

#ifdef DEBUG
    fprintf (html_file, " line=%d", __LINE__);
#endif

    fprintf (html_file, ">\n");
    fprintf (html_file, "        %s\n", build_band_comment (detail_record));
  }

  if ((bisflag & BISDAY) || (bisflag & BISSTART)) {
    fprintf (html_file, "      <BR");

#ifdef DEBUG
    fprintf (html_file, " line=%d", __LINE__);
#endif

    fprintf (html_file, ">\n");
    switch (TIMEFLAG) {
     case 3:
       fprintf (html_file, "          %s\n", detail_record->scet_start_3);
       break;
     case 2:
       fprintf (html_file, "          %s\n", detail_record->scet_start_2);
       break;
     case 1:
       fprintf (html_file, "          %s\n", detail_record->scet_start);
       break;
    }
  }
  if (bisflag & BISSTOP) {
    fprintf (html_file, "      <BR");

#ifdef DEBUG
    fprintf (html_file, " line=%d", __LINE__);
#endif

    fprintf (html_file, ">\n");
    fprintf (html_file, "          Stop %s\n", detail_record->scet_stop);
  }

  /*
   * dump(html_file, detail_record); /*
   */

  fprintf (html_file, "  <BR");

#ifdef DEBUG
  fprintf (html_file, " line=%d", __LINE__);
#endif

  fprintf (html_file, ">\n");
  fflush (html_file);
  return 1;
}

/* ************************************************************************* */
int rpws_master_browse_html_wbr_detail_100(
	FILE * html_file, struct RPWS_LABEL *detail_record, int index
){
  static char oldpath[256] = { "" };
  static char pathname[256];
  static char linkname[256];
  int status = 0;

  int data_year;
  int data_month;
  int data_mday;
  int data_yday;
  int data_hour;
  int data_minute;
  double data_second;
  
  

  strcpy (pathname, detail_record->filepath2);
  if (strcmp (pathname, oldpath)) {
    status = 1;
    strcpy (oldpath, pathname);
    strcat (pathname, "/");
    strcat (pathname, detail_record->filepath2);
    strcat (pathname, band_[detail_record->fband]);
    strcat (pathname, "_MA.HTM");
    parsetime (detail_record->scet_start,
               &data_year,
               &data_month,
               &data_mday,
               &data_yday, &data_hour, &data_minute, &data_second);
    tnorm (&data_year,
           &data_month,
           &data_mday, &data_yday, &data_hour, &data_minute, &data_second);
    sprintf (linkname, "%04d-%02d-%02d (%03d)",
             data_year, data_month, data_mday, data_yday);
    fprintf (html_file, "  <LI><A Href=%s> %s </a>", pathname, linkname);
    if (0)
      fprintf (html_file, "&nbsp; %s\n", detail_record->scet_start);
    fprintf (html_file, "\n");
    fflush (html_file);
  }
  return status;
}

/* ************************************************************************* */
/* Make a 1-day WBR HTML file */

int rpws_master_browse_wbr_001 (int index, char *w_directory[])
{
	static char oldpath[256] = { "" };
	static char pathname[256] = {'\0'};
  	static char other_name[256] = {'\0'};
	static char pwdbuf[256] = {'\0'};
	struct RPWS_LABEL *detail_record = NULL;
	FILE *html_file = NULL;
	int count = 0;
	int status = 0;
	char temp[256] = {'\0'};
	
	/* See if the plot is worth making or not */
	das_time_t dtBeg = {0};
	das_time_t dtEnd = {0};
	

	detail_record = RPWS_BROWSE_data_record[index];       /* get the list for this band */
	
	while (detail_record) {
				
		if( ! dt_parsetime(detail_record->plot_start, &dtBeg)  || 
			 ! dt_parsetime(detail_record->plot_stop, &dtEnd)  ){
			fprintf(stderr, "ERROR: in %s, %d: Can't parse plot times\n", __FILE__, __LINE__);
			exit(17);
		}
		
		if(dt_diff(&dtEnd, &dtBeg) <= 1.0){
			fprintf(stderr, "INFO: Skipping link to plot %s, would be less than 1 "
					  "second long\n", detail_record->filename);
			detail_record = detail_record->link;
			continue;
		}
		
		strcpy (pathname, detail_record->filepath2);
		
		/* Path change, start a new file */
		if (strcmp(pathname, oldpath)) {   /* different (0 means they match) */
			status = 1;
			strcpy (oldpath, pathname);

			strcpy (pathname, w_directory[RPWS_ARCHIVE_BROWSE]);
			strcat (pathname, "/");
			strcat (pathname, w_directory[detail_record->instrument + RPWS_ARCHIVE_BROWSE_WBR]);
			strcat (pathname, "/");
			strcat (pathname, detail_record->filepath1);
			strcat (pathname, "/");
			
			strcat (pathname, detail_record->filepath2);
			strcat (pathname, "/");
			strcat (pathname, detail_record->filepath2);
			strcat (pathname, band_[detail_record->fband]);
			strcat (pathname, "_MA.HTM");

			strcpy (other_name, detail_record->filepath2);
			strcat (other_name, band_[detail_record->fband ^ 1]);
			strcat (other_name, "_MA.HTM");
			
			/* Close old file if we were working on one */
			if (html_file) {
				rpws_master_browse_html_trailer_text(
						html_file, "TABLE", 3, other_name, title_type[other (index)]
				);
				fclose (html_file);
			}
			
			html_file = fopen(pathname, "w");
			rpws_master_browse_html_header_text(
					html_file,index, "TABLE",other_name, title_type[other (index)]
			);
			count = 0;
			sprintf (temp, "DBG line:%3d header Count=%d", __LINE__, count);
			html_comment (html_file, temp);
			sprintf(temp, "DBG line:%3d  detail_record->thumbname  %s", __LINE__,
			        detail_record->thumbname);
			html_comment (html_file, temp);
		}
	 
		if (html_file) {
			sprintf (temp, "DBG line:%3d detail Count=%d", __LINE__, count);
			html_comment (html_file, temp);
			if ((count % 4) == 0)
				fprintf (html_file, "  <TR ALIGN=\"CENTER\">\n");
			rpws_master_browse_html_detail_001 (html_file, detail_record);
			count++;
		}
		else {
			fprintf (stdout, "%s/%dFAIL, html_file (%s not open!!!)\n", __FILE__,
               __LINE__, detail_record->filename);
			exit(17);
		}
		detail_record = detail_record->link;
	}
	
	if (html_file) {
		sprintf (temp, "DBG line:%3d trailer Count=%d", __LINE__, count);
		html_comment (html_file, temp);
		rpws_master_browse_html_trailer_text(
				html_file, "TABLE", 3, other_name, title_type[other (index)]
		);
		fclose (html_file);
	}
	
	oldpath[0] = 0;
	return count;
}

/* ************************************************************************* */
/* Make the 100-day WBR HTML file */

/* There is a bug in this file, if a 100-day block contains no usable records */
/* an empty content file is generated anyway */

int rpws_master_browse_wbr_100 (int index, char *w_directory[])
{
	struct RPWS_LABEL *detail_record;
	FILE *html_file;
	int count = 0;

	detail_record = RPWS_BROWSE_data_record[index];       /* get the list for this band */
	if (detail_record) {
		html_file = rpws_master_browse_html_header (index, w_directory, detail_record);
		
		while (detail_record) {
			if(strcmp(detail_record->sclk_start, detail_record->sclk_stop) != 0)
				count += rpws_master_browse_html_wbr_detail_100 (html_file, detail_record, index);
			detail_record = detail_record->link;
		}
		
		rpws_master_browse_html_trailer_text (html_file, "UL", 2, NULL, NULL);
		fclose (html_file);
	}
	return count;
}

/* ************************************************************************* */
int rpws_master_browse_wfr_010 (int index, char *w_directory[])
{
  static char oldpath[256] = { "" };
  static char pathname[256];
  static char other_name[256];
  static char pwdbuf[256];
  struct RPWS_LABEL *detail_record;
  FILE *html_file = NULL;
  char temp[256];
  int count;
  int status = 0;

  count = 0;
  detail_record = RPWS_BROWSE_data_record[index];       /* get the list for this band */
  while (detail_record) {
    strcpy (pathname, detail_record->filepath1);
    if (strcmp (pathname, oldpath)) {   /* different (0 means they match) */
      status = 1;
      strcpy (oldpath, pathname);

      strcpy (pathname, w_directory[RPWS_ARCHIVE_BROWSE]);
      strcat (pathname, "/");

      strcat (pathname,
              w_directory[detail_record->instrument +
                          RPWS_ARCHIVE_BROWSE_WBR]);
      strcat (pathname, "/");

      strcat (pathname, detail_record->filepath1);
      strcat (pathname, "/BROWSE_");
      strcat (pathname, type_2[index]);
      strcat (pathname, "_MA.HTM");

      strcpy (other_name, "BROWSE_");
      strcat (other_name, type_2[other (index)]);
      strcat (other_name, "_MA.HTM");


      if (html_file) {
        sprintf (temp, "DBG line:%3d trailer Count=%d", __LINE__, count);
        html_comment (html_file, temp);
        rpws_master_browse_html_trailer_text (html_file,
                                              "TABLE",
                                              2,
                                              other_name,
                                              title_type[other (index)]);
        fclose (html_file);
      }
      html_file = fopen (pathname, "w");
      rpws_master_browse_html_header_text (html_file,
                                           index,
                                           "TABLE",
                                           other_name,
                                           title_type[other (index)]);
      sprintf (temp, "DBG line:%3d: header Count=%d", __LINE__, count);
      html_comment (html_file, temp);
    }
    if (html_file) {
      sprintf (temp, "DBG line:%3d detail Count=%d", __LINE__, count);
      html_comment (html_file, temp);
      
		if(strcmp(detail_record->scet_start, detail_record->scet_stop) != 0){
			
			if ((count % 4) == 0)
	        fprintf (html_file, "<TR ALIGN=\"CENTER\">\n");
					
	      rpws_master_browse_html_detail_010 (html_file, detail_record);
	      count++;
		}
    } else {
      fprintf (stdout, "%s/%dFAIL, html_file (%s0 not open!!!\n", __FILE__,
               __LINE__, detail_record->filename);
      exit (0);
    }
    detail_record = detail_record->link;
  }
  if (html_file) {
    sprintf (temp, "DBG line:%3d trailer Count=%d", __LINE__, count);
    html_comment (html_file, temp);
    rpws_master_browse_html_trailer_text (html_file,
                                          "TABLE",
                                          2,
                                          other_name,
                                          title_type[other (index)]);
    fclose (html_file);
  }
  oldpath[0] = 0;
  return count;
}

/* ************************************************************************* */
/* Called from rpws_archive, to make all the high-level PDS files.           */

int rpws_master_browse_html (char *w_directory[], int flag)
{
  int count = 0;
  struct RPWS_LABEL *detail_record;
  int index;

  comment_flag = flag;
  fprintf (stdout, "RPWS_MASTER_BROWSE HTML creation\n");
  rpws_master_browse_wbr_100 (3, w_directory);
  rpws_master_browse_wbr_100 (4, w_directory);
  rpws_master_browse_wbr_001 (3, w_directory);
  rpws_master_browse_wbr_001 (4, w_directory);
  rpws_master_browse_wfr_010 (1, w_directory);
  rpws_master_browse_wfr_010 (2, w_directory);
  return count;
}
