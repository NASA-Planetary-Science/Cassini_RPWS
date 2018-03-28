
/*
 * dsp9a.c
 */
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>

/* Cassini Stuff */
#include <fg.h>
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>

/* Local definitions, instead of having header files */
int dsp9x (char *Version, int flag);    /* from dsp9x.c */
int dsp9b (char *filename, int refresh_period); /* from dsp9b.c */
int dsp9c_color_key (void);             /* from dsp9c.c */

#define MAXENTRIES 12
#define HTML4

int REFRESH_PERIOD = 60;

char *title = { "CASSINI MP data dump" };
char *dsp9a_title = { "(dsp9a) 1.10" };
extern char *dsp9b_title;
extern char *dsp9c_title;
extern char *dsp9x_title;
extern char *dsp9t (int);
struct MP_buffer *buffer, *buffer2;
static int epoch;
char result[256];
struct stat status_buffer;

typedef struct
{
  char *name;
  char *val;
} entry;
void unescape_url (char *url);
void plustospace (char *str);
char *makeword (char *line, char stop);
char *fmakeword (FILE * f, char stop, int *len);

int _getbuffer_MP (struct MP_buffer *buffer, FILE * input, int iflg)
{
  int ilen;

  ilen = UTIL_getbuffer_MP (buffer, input, iflg);
  return ilen;
}

#define DETAIL_LEN 256
FILE *_database (time_t * file_tm)
{
  static FILE *input = NULL;
  FILE *dbase = NULL;
  int icnt = 0;
  char *fname = NULL;
  char detail[DETAIL_LEN + 1];
  char *temp = NULL;
  int i = 0;

  fname = getenv("RPWS_MPDB");
  if(fname)
	  dbase = fopen (fname, "r");
  
  if (!dbase) {
    fprintf (stderr, "Database file not found (%s)\n", fname);
    return NULL;
  }
  i = fseek (dbase, -1024, SEEK_END);
  if (!i)
    fgets (detail, DETAIL_LEN, dbase);  /* align to begin of next record */
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
  FILE *fperror;
  int ilen;
  char fname[128];
  char str0[1024];
  char str1[1024];
  char date_string[256];
  char last_modified_string[256];
  char *dump_type = { "" };
  char housekeeping_filename[256] = { '\0' };
  char log_filename[256] = {'\0'};

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
  char *temp;
  int dsp9a_flag = 0;
  int argi = 0;

  if(!getenv("RPWS_DATA")){
    fprintf(stderr, "RPWS_DATA env. var. not specifed");
    return 13;
  }
  strcpy(housekeeping_filename, getenv("RPWS_DATA"));

  if (argc > 2) {
    fprintf(stderr, "usage: rpws_dsp_9a [temp|test|vc0|vc1|all|key|nert|em\n");
    return -1;
  }
  
  if (argc == 1) {
      dump_type = "RECENT DATA";
      housekeeping_filename[0] = '\0';
      REFRESH_PERIOD = 30;
      dsp9a_flag = 0;
  } else if (!strcmp (argv[1], "temp")) {
	 dump_type = "TEMP";
     strcat(housekeeping_filename, "/temp/recent_hsk.dat");
	 REFRESH_PERIOD = 20;
	 dsp9a_flag = 1;
  } else if (!strcmp (argv[1], "test")) {
	 dump_type = "TEST";
     strcat(housekeeping_filename, "/bg_test/recent_hsk.dat");
	 REFRESH_PERIOD = 20;
	 dsp9a_flag = 1;
  } else if (!strcmp (argv[1], "vc0")) {
	 dump_type = "RT/VC0";
	 strcat(housekeeping_filename, "/bg_push/recent_hsk.dat.vc0");
	 REFRESH_PERIOD = 30;
	 dsp9a_flag = 1;
  } else if (!strcmp (argv[1], "vc1")) {
	 dump_type = "RT/VC1";
 	strcat(housekeeping_filename, "/bg_push/recent_hsk.dat.vc1");
	 REFRESH_PERIOD = 30;
	 dsp9a_flag = 1;
  } else if (!strcmp (argv[1], "all")) {
	 dump_type = "RT";
    strcat(housekeeping_filename, "/bg_push/recent_hsk.dat");
	 REFRESH_PERIOD = 30;
	 dsp9a_flag = 1;
  } else if (!strcmp (argv[1], "key")) {
	 dump_type = "RT";
	 strcat(housekeeping_filename, "/bg_push/recent_hsk.dat");
	 REFRESH_PERIOD = 30;
	 dsp9a_flag = 3;
  } else if (!strcmp (argv[1], "nert")) {
	 dump_type = "NERT";
	 strcat(housekeeping_filename, "/bg_nert/recent_hsk.dat");
	 REFRESH_PERIOD = 20;
	 dsp9a_flag = 1;
  } else if (!strcmp (argv[1], "em")) {
	 dump_type = "BENCH";
	 strcat(housekeeping_filename, "/data_em/recent_hsk.dat");
	 REFRESH_PERIOD = 10;
	 dsp9a_flag = 1;
  }

	timeval = 0;
   int successful = 1;
   int cl;
   int x;
   int index;
   entry entries[MAXENTRIES];

   if(!getenv("RPWS_CGI_LOGDIR")){
	  sprintf(log_filename, "%s/temp/dsp9a_web-cgi.log", getenv("RPWS_DATA"));
   } else {
      sprintf(log_filename, "%s/dsp9a_web-cgi.log", getenv("RPWS_CGI_LOGDIR"));
   }
	 
   fperror = fopen (log_filename, "a");
   if (!fperror) {
      fprintf (stderr, "Cannot open log file: %s\n", log_filename);
      printf ("%s: %s\n", "Content-type", "text/plain");
      printf ("\n");
      printf ("Error opening log file: %s\n", strerror(errno));
      return 0;
   }
      
	 
   /**/ timeval = time (NULL);
	fprintf (fperror, "%s\n", make_date (&timeval));
	fprintf (fperror, "    Parsing input (stdin) %s\n", argv[0]);
	
	if (getenv ("CONTENT_LENGTH")) {
		cl = atoi (getenv ("CONTENT_LENGTH"));
		fprintf (fperror, "    Content Length %d\n", cl);
		
		for (x = 0; cl && (!feof (stdin)); x++) {
			entries[x].val = fmakeword (stdin, '&', &cl);
			plustospace (entries[x].val);
			unescape_url (entries[x].val);
			entries[x].name = makeword (entries[x].val, '=');
			fprintf (fperror, "    Entry %d: (%s)=[%s] \n", x,
                 entries[x].name, entries[x].val);
			
			if(!strcmp (entries[x].name, "TEST")) {
				dump_type = "RT";
				sprintf(housekeeping_filename, "%s/bg_test/recent_hsk.dat", getenv("RPWS_DATA"));
			 
				REFRESH_PERIOD = 0;
				dsp9a_flag = 1;
				if (!strcmp (entries[x].val, "VC0")) {
					dump_type = "RT/VC0";
					sprintf(housekeeping_filename, "%s/bg_test/recent_hsk.dat.vc0", getenv("RPWS_DATA"));
				}
				
				if (!strcmp (entries[x].val, "VC1")) {
					dump_type = "RT/VC1";
					sprintf(housekeeping_filename, "%s/bg_test/recent_hsk.dat.vc", getenv("RPWS_DATA"));
				}
				if (!strcmp (entries[x].val, "ALL")) {
					sprintf(housekeeping_filename, "%s/bg_test/recent_hsk.dat", getenv("RPWS_DATA"));
				}
			}
		  
			if(!strcmp (entries[x].name, "PUSH")){
				dump_type = "RT";
				sprintf(housekeeping_filename, "%s/bg_push/recent_hsk.dat", getenv("RPWS_DATA"));
				
				REFRESH_PERIOD = 10;
				dsp9a_flag = 1;
				
				if (!strcmp (entries[x].val, "VC0")) {
					dump_type = "RT/VC0";
					sprintf(housekeeping_filename, "%s/bg_push/recent_hsk.dat.vc0", getenv("RPWS_DATA"));
					REFRESH_PERIOD = 30;
				}
				
				if (!strcmp (entries[x].val, "VC1")) {
					dump_type = "RT/VC1";
					sprintf(housekeeping_filename, "%s/bg_push/recent_hsk.dat.vc1", getenv("RPWS_DATA"));
					REFRESH_PERIOD = 10;
				}
				
				if (!strcmp (entries[x].val, "ALL")) {
					dump_type = "RT";
					sprintf(housekeeping_filename, "%s/bg_push/recent_hsk.dat", getenv("RPWS_DATA"));
					REFRESH_PERIOD = 15;
				}
			}
			
			
			if (!strcmp (entries[x].name, "NERT")) {
				dump_type = "NERT";
				sprintf(housekeeping_filename, "%s/bg_nert/recent_hsk.dat", getenv("RPWS_DATA"));
				REFRESH_PERIOD = 10;
				dsp9a_flag = 1;
				
				if (!strcmp (entries[x].val, "VC0")) {
					dump_type = "NERT/VC0";
					sprintf(housekeeping_filename, "%s/bg_nert/recent_hsk.dat.vc0", getenv("RPWS_DATA"));
					REFRESH_PERIOD = 0;
				}
				
				if (!strcmp (entries[x].val, "VC1")) {
					dump_type = "NERT/VC1";
					sprintf(housekeeping_filename, "%s/bg_nert/recent_hsk.dat.vc1", getenv("RPWS_DATA"));
					REFRESH_PERIOD = 0;
				}
				
				if (!strcmp (entries[x].val, "ALL")) {
					dump_type = "NERT";
					sprintf(housekeeping_filename, "%s/bg_nert/recent_hsk.dat", getenv("RPWS_DATA"));
				}
				
				if (!strcmp (entries[x].val, "TEMP")) {
					dump_type = "TEMP";
					sprintf(housekeeping_filename, "%s/temp/recent_hsk.dat", getenv("RPWS_DATA"));
				}
				
			}
        
		
			if (!strcmp (entries[x].name, "DATA_EM")) {
				dump_type = "BENCH";
				sprintf(housekeeping_filename, "%s/data_em/recent_hsk.dat", getenv("RPWS_DATA"));
				REFRESH_PERIOD = 8;
				dsp9a_flag = 1;
			}
			
			if (!strcmp (entries[x].name, "DATA")) {
				dump_type = "RECENT DATA";
				housekeeping_filename[0] = '\0';
				REFRESH_PERIOD = 30;
				dsp9a_flag = 0;
			}
			if (!strcmp (entries[x].name, "HELP")) {
				dump_type = "HELP PAGE";
				housekeeping_filename[0] = '\0';
				REFRESH_PERIOD = -1;
				dsp9a_flag = 1;
			}
		}
    
		fprintf (fperror, "    housekeeping filename %s\n",
             housekeeping_filename);
		if (REFRESH_PERIOD >= 0)
			fprintf (fperror, "    REFRESH_PERIOD %d\n", REFRESH_PERIOD);
		
		fprintf (fperror, "    dump type %s\n", dump_type);
	}



	buffer = malloc (32768 + 1024);
	buffer2 = malloc (32768 + 1024);
	timeval = time (NULL);
	strcpy (date_string, make_date (&timeval));
	strcpy (last_modified_string, make_date (&timeval));
	fg_flags (argc, argv);
	if (fg_flag ("help") || fg_flag ("h")) {
		dsp9x (dsp9a_title, 0);
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

	pkt_etime = UTIL_event_time (buffer, 0);
	evt_clk.seconds = buffer->packet.cds_tag.begin[0] << 24 |
      buffer->packet.cds_tag.begin[1] << 16 |
      buffer->packet.cds_tag.begin[2] << 8 | buffer->packet.cds_tag.begin[3];
	evt_clk.fine = 0;
	evt_tim = UTIL_event_scet (buffer, evt_clk);
	pkt_ev = UTIL_event_scet_tm (*evt_tim, 0);
	
	if (epoch)
      sprintf (str0, " CDS Time  %02X%02X%02X%02X.%01X %04X "
               "(%4d-%03dT%2.2d:%2.2d:%2.2d.%3.3d)",
               buffer->packet.cds_tag.begin[0],
               buffer->packet.cds_tag.begin[1],
               buffer->packet.cds_tag.begin[2],
               buffer->packet.cds_tag.begin[3],
               buffer->packet.cds_tag.begin[4] & 0x03,
               (pkt_etime & 0x1FFF) << 3,
               pkt_ev->tm_year + 1900,
               pkt_ev->tm_yday + 1,
               pkt_ev->tm_hour,
               pkt_ev->tm_min, pkt_ev->tm_sec, evt_tim->milliseconds % 1000);
	else
      sprintf (str0, " CDS Time  %02X%02X%02X%02X.%01X %04X "
               "(%4d-%03d %2.2d:%2.2d:%2.2d.%3.3d)",
               buffer->packet.cds_tag.begin[0],
               buffer->packet.cds_tag.begin[1],
               buffer->packet.cds_tag.begin[2],
               buffer->packet.cds_tag.begin[3],
               buffer->packet.cds_tag.begin[4] & 0x03,
               (pkt_etime & 0x1FFF) << 3,
               pkt_tm->tm_year + 1900,
               pkt_tm->tm_yday + 1,
               pkt_tm->tm_hour, pkt_tm->tm_min, pkt_tm->tm_sec, 0);
  

	pkt_etime = UTIL_event_time (buffer, 0);
	pkt_epoc = pkt_etime + epoch;
	if (epoch) {
		evt_clk.seconds = pkt_etime;
		evt_clk.fine = UTIL_extract_MP_RTI (buffer) << 5;
		evt_tim = UTIL_event_scet (buffer, evt_clk);
		pkt_ev = UTIL_event_scet_tm (*evt_tim, 0);
		sprintf (str1,
             "  SC Event  %8X.%1X %02X%02X (%4d-%03dT%2.2d:%2.2d:%2.2d.%3.3d) new",
             pkt_etime, buffer->packet.mpp.mini_packet[2] & 0x03,
             buffer->packet.mpp.mini_packet[3],
             buffer->packet.mpp.mini_packet[2], pkt_ev->tm_year + 1900,
             pkt_ev->tm_yday + 1, pkt_ev->tm_hour, pkt_ev->tm_min,
             pkt_ev->tm_sec, evt_tim->milliseconds % 1000);
	} 
	else {
		pkt_ev = gmtime (&pkt_epoc);
		sprintf (str1, "  SC Event %8X %4X (%2.2d:%2.2d:%2.2d.%3.3d) Epoch %X ",
             pkt_etime,
             (pkt_etime & 0x1FFF) << 3,
             pkt_ev->tm_hour,
             pkt_ev->tm_min,
             pkt_ev->tm_sec, UTIL_extract_MP_RTI (buffer) * 125, epoch);
	}

	printf ("%s: %s\n", "Pragma", "nocache");
	printf ("%s: %s\n", "Date", date_string);
	printf ("%s: %s\n", "Last Modified", date_string);
	if (REFRESH_PERIOD > 0)
		printf ("%s: %d\n", "Refresh", REFRESH_PERIOD);
	printf ("%s: %s\n", "Content-type", "text/html");
	printf ("\n");

#ifdef HTML4
	printf ("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">\n");
	printf ("<HTML>\n");
	printf ("<META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=ISO-8859-1\">\n");
	printf ("<META NAME=\"Date\" CONTENT=\"%sZ\">\n", date_string);
	printf ("<META NAME=\"Copyright\" CONTENT=\"&copy; %s University of Iowa\">\n",
       date_string);
	printf ("<META NAME=\"Keywords\" CONTENT=\"Cassini/RPWS\">\n");
	printf ("<META NAME=\"Keywords\" CONTENT=\"HOUSEKEEPING\">\n");
#else
	printf ("<HTML>\n");
#endif

	printf ("<HEAD>\n");

	printf ("<SCRIPT LANGUAGE=\"JavaScript\">\n");
	printf ("<!-- hide from non-javascript browser\n");
	printf ("  function PlaySound() {\n");
	printf ("    window.location=\"/plasma-wave/cassini/wtr/phone.wav\";\n");
	printf ("   }\n");
	printf ("  // -->\n");
	printf ("</SCRIPT>\n");
	printf ("\n");

  printf ("<TITLE>%s %s %s %s %s</TITLE>\n",
          title, dsp9a_title, dsp9b_title, dsp9c_title, dsp9x_title);
  printf ("</HEAD>\n");
  printf ("<BODY bgcolor=#C0C0C0>\n");

  if (dsp9a_flag) {
    printf ("<H4>%s Housekeeping Dump %s</H4>\n", dump_type, dsp9a_title);
    if (REFRESH_PERIOD < 0)
      dsp9x (dsp9a_title, 1);
    else
      dsp9b (housekeeping_filename, REFRESH_PERIOD);
    if (temp = dsp9t (0))
      printf ("next pass <tt> %s</tt> nxp1<br>\n", temp);
    if (temp = dsp9t (1))
      printf ("next pass <tt> %s</tt> nxp2<br>\n", temp);
    if (housekeeping_filename[0])
      printf ("recent housekeeping <tt> %s</tt><br>\n",
              housekeeping_filename);
  }
  else {
    printf ("<H2>\n");
    printf ("Recent Cassini/RPWS Data %s\n", dsp9a_title);
    printf ("</H2>\n");
    printf ("This page queries the RPWS mini-packet database\n");
    printf ("to scan for the most recent science data available.\n");
    printf ("<br>\n");
    printf
      ("The file indications are a scan of <em>Science Data Only</em>.  \n");
    printf ("<br>\n");
    printf ("And, as an added bonus, this page might even update \n");
    printf ("about every %d seconds when left alone.  \n", REFRESH_PERIOD);
    printf ("This make use of a client-side PULL so the browser\n");
    printf (" may not accept the tag used to update the page");
    printf ("<br>\n");
    printf ("This information is also available on the houskeeping page\n");
    printf ("<br>\n");
    printf
      ("We hope you will find this useful for monitoring real-time data\n");
    printf ("<br>\n");
  }

  if (dsp9a_flag) {
    printf ("data file <tt> %s</tt><br>\n", result);
    printf ("file size <tt> %d bytes</tt><br>\n", status_buffer.st_size);
    printf ("last modified <tt> %s</tt><br>\n", last_modified_string);
    printf ("CDS SCLK/SCET <tt> %s</tt><br>\n", str0);
    printf ("MP SCLK/SCET <tt> %s</tt><br>\n", str1);
    if (!(dsp9a_flag & 2)) {
      printf ("This report is intended to fit on a single page\n");
      printf ("%s<br>\n", dsp9a_title);
    }
  } else {
    printf ("<H4>Data File</H4>\n");
    printf ("last modified %s\n", last_modified_string);
    printf ("<br>\n");
    printf ("file size     %d\n", status_buffer.st_size);
    printf ("<br>\n");
    printf ("<tt>%s</tt>\n", result);
    printf ("<H4>CDS SCLK/SCET</H4>\n");
    printf ("<tt>    %s</tt>\n", str0);
    printf ("<H4>MP SCLK/SCET</H4>\n");
    printf ("<tt>    %s</tt>\n", str1);
    printf ("<br><br>%s\n", dsp9a_title);
  }
  if (dsp9a_flag & 2) {
    printf ("<BR>\n");
    dsp9c_color_key ();
  }

#ifdef HTML4
  printf ("   <p>\n");
  printf ("    <a href=\"http://validator.w3.org/check/referer\">\n");
  printf ("Validate with W3C\n");
  printf ("      </a>\n");
  printf ("     </p>\n");
#endif

  printf ("</BODY>\n");
  printf ("</HTML>\n");

  return 0;
}
