/*
        wfrplot.c       written 02/17/99 by TFA to draw line plots
                        of Cassini RPWS WFR raw waveform data
        Modifications:
                        SCET calculation updated. TFA 01/06/99
                        DAS library functions (e.g., ttime). TFA 01/15/99
*/
#include <stdio.h> 
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <rtiu.h>
#include <util.h>
#include <utilt.h>
#include <time.h>
#include <curses.h>
#include <math.h>
#include <das.h>
#include "dec.h"

#define  MAXENTRIES	100
#define  MAXFILES 1024
#define  MAXCHARS 200
#define  True   1
#define  False  0

struct file_structure
{
   char line [MAXCHARS];
   int year;
   int day;
   int hr;
   int mn;
   int num_recs;
   float sec;
   double ms;
   double st_sec;
   double sp_sec;
};

typedef struct
{
   char *name;
   char *val;
}
entry;

static	int	chan_select;
static	FILE	*fperror;
static	char	*progname;
static	char	*dataset="JPL";
static	int	ybottom=0;
static	int	ytop=0;
static	int	num_skip=1;
static	char	MIME_Header[40];
static	char	gif_file[8];

void    unescape_url (char *url);
void    plustospace (char *str);
char    *makeword (char *line, char stop);
char    *fmakeword (FILE *f, char stop, int *len);

void make_dbase (double start, double stop, struct file_structure *dbase, int *num_files);

int process_data (double start, double stop, struct file_structure *dbase, int n_files);

main(int argc,char **argv)
{
int     num_files, cl, x, scaling;
int     year, month, day, doy, hr, mn;
double  sec, start_sec, stop_sec;
struct  file_structure files [MAXFILES];       
entry   entries [MAXENTRIES];

/***************************************************************/

   progname = argv [0];

   if (!(fperror = fopen ("/var/tmp/tfa-cgi.log", "w")))
   {
      fprintf (stderr, "%s:  error opening /var/tmp/tfa-cgi.log\n", progname);
      exit (-1);
   }

   if (!getenv ("REQUEST_METHOD"))
   {
      printf ("This script should be referenced with a METHOD of POST.\n");
      printf ("If you don't understand this, see this ");
      printf ("<A HREF=\"http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/fill-out-forms/overview.html\">forms overview</A>.%c",10);
      exit (1);
   }
   else if (strcmp (getenv ("REQUEST_METHOD"), "POST"))
   {
      printf ("This script should be referenced with a METHOD of POST.\n");
      printf ("If you don't understand this, see this ");
      printf ("<A HREF=\"http://www.ncsa.uiuc.edu/SDG/Software/Mosaic/Docs/fill-out-forms/overview.html\">forms overview</A>.%c",10);
      exit (1);
   }

   if (!getenv ("CONTENT_TYPE"))
   {
      printf ("This script can only be used to decode form results. \n");
      exit (1);
   }
   if (strcmp (getenv ("CONTENT_TYPE"), "application/x-www-form-urlencoded"))
   {
      printf ("This script can only be used to decode form results. \n");
      exit (1);
   }

   fprintf (fperror, "Testing %s\n", progname);
   cl = atoi (getenv ("CONTENT_LENGTH"));
   for (x = 0; cl && (!feof (stdin)); x++)
   {
      entries [x].val = fmakeword (stdin, '&', &cl);
      plustospace (entries [x].val);
      unescape_url (entries [x].val);
      entries [x].name = makeword (entries [x].val,'=');
      fprintf (fperror, " %s %s \n", entries [x].name, entries [x].val );
   }

 sscanf (entries[0].val, "%d", &chan_select);
 if ((chan_select < 0) || (chan_select > 4))
 {
    fprintf (stdout, "Content-type: text/plain\n\n");
    fprintf (stdout, "%s: bad channel # %s\n", argv[0], entries[0].val);
    exit (1);
 }

 if (parsetime (entries[1].val, &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("%s: error parsing %s\n", progname, entries[0].val);
    exit (1);
 }
 start_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );

 fprintf (fperror, " %d %d %d %d %d %d %lf \n",
 		 year, month, day, doy, hr, mn, sec);

 if (parsetime (entries[2].val, &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("%s: error parsing %s\n", progname, entries[1].val);
    exit (1);
 }
 stop_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );

 fprintf (fperror, " %d %d %d %d %d %d %lf \n",
 		 year, month, day, doy, hr, mn, sec);

 sscanf (entries[3].val, "%d", &num_skip);
 num_skip--;
 if (num_skip < 0)
   num_skip = 1;
 sscanf (entries[4].val, "%d", &ybottom);
 sscanf (entries[5].val, "%d", &ytop);
 sscanf (entries[6].val, "%d", &scaling);

 if ( scaling == 0 )			/* Autoscale box checked ? */
 { if (ybottom >= ytop)
   { ybottom = 0;
     ytop = 0;
   }
   if ( (ybottom == 0) && (ytop == 0) )
     fprintf (fperror, "Autoscaling data... \n");
   else
     fprintf (fperror, "Ybottom = %d     Ytop = %d \n", ybottom, ytop);
 }
 else
 { ybottom = 0;
   ytop = 0;
   fprintf (fperror, "Autoscaling data... \n");
 }
/*
	See if the desired output is GIF or PS.
*/
  if (strcmp (entries[7].name, "device_name") )
  {
    fprintf (fperror, "entries[7].name is not equal to device_name\n");
    sprintf (MIME_Header, "Content-type: image/gif");
    sprintf (gif_file, "-/gif");
  }
  else   if ( !strcmp (entries[7].val, "Z") )	/* GIF file ? */
  {
    fprintf (fperror, "entries[7].val is equal to Z\n");
    sprintf (MIME_Header, "Content-type: image/gif");
    sprintf (gif_file, "-/gif");
    if ( !strcmp (entries[8].name, "gif_mime_header") )
      sprintf (MIME_Header, "Content-type: %s", entries[8].val);
  }

  if (strcmp (entries[8].name, "device_name") )
  {
    fprintf (fperror, "entries[8].name is not equal to device_name\n");
  }
  else   if ( !strcmp (entries[8].val, "PS") )	/* PS file ? */
  {
    fprintf (fperror, "entries[8].val is equal to PS\n");
    sprintf (MIME_Header, "Content-type: application/postscript");
    sprintf (gif_file, "-/ps");
    if ( !strcmp (entries[9].name, "ps_mime_header") )
      sprintf (MIME_Header, "Content-type: %s", entries[9].val);
  }

  if (strcmp (entries[9].name, "device_name") )
  {
    fprintf (fperror, "entries[9].name is not equal to device_name\n");
  }
  else   if ( !strcmp (entries[9].val, "TEXT") )	/* Text file ? */
  {
    fprintf (fperror, "entries[9].val is equal to TEXT\n");
    sprintf (MIME_Header, "Content-type: text/plain");
    sprintf (gif_file, "TEXT");
    if ( !strcmp (entries[10].name, "text_mime_header") )
      sprintf (MIME_Header, "Content-type: %s", entries[10].val);
  }

  fprintf (fperror, "MIME Header is %s\n", MIME_Header);
/*
 if (argc == 5)
   dataset = argv[4];
 fprintf (stderr, "Selected data set is %s\n", dataset);
*/
 make_dbase (start_sec, stop_sec, files, &num_files);
 if (num_files == 0)
 {
   fprintf (stdout, "Content-type: text/plain\n\n");
   fprintf (stdout, "No data coverage of the requested time interval.\n");
   fprintf (stderr, "No data coverage of the requested time interval.\n");
   exit (1);
 }

 if (process_data (start_sec, stop_sec, files, num_files) )
 {
   fprintf (stdout, "Content-type: text/plain\n\n");
   fprintf (stdout, "No data coverage of the requested time interval.\n");
   fprintf (stderr, "No data coverage of the requested time interval.\n");
   exit (1);
 }
 return (0);
}

int process_data (double start_sec, double stop_sec, struct file_structure *dbase, int n_files)
{
 int	num_rec;
 int    epoch;
 int    inst_type, mp_len, chan, band;
 int    wfrdata, nsamp, nlast=2048;
 int    i, idx, RTI, igain, agc, iant, first=1;
 int    year, month, day, doy, hr, mn, mon, mday;
 int    Zero=0, One=1, Two=2, Three=3;
 int	black, white;
 double cal_factor, ant_length, gain, dc;
 float  Fzero=0.0, Fone=1.0;
 char	toplbl[64];
 char	time_string[32];
 char	gain_string[16];
 char	sensor_string[16];
 float  x[2048], y[2048], wfrcal[2048];
 float  xmin, xmax, ymin, ymax, deltat;
 double pkt_sec=0;
 double sec;
 time_t pkt_sclk, pkt_epoc, start_sclk, stop_sclk;
 struct tm *pkt_event;
 struct event_time *evt_tim;
 struct event_clock evt_clk;
 struct bigbuf {
                struct RPWS_buffer big1;
                struct RPWS_buffer big2;
               };
 union  {
          struct RPWS_buffer rbuffer;
          struct MP_buffer buffer;
          struct bigbuf banana;
        } m;
   int st_inx, no_data;
   int index, index2;
   float offset, spec [1026], spec_out[1026];
   FILE *finput;

   no_data = True;
   num_rec = -1;
   if (stop_sec < dbase->st_sec)
      return (1);

   if (start_sec > (dbase + (n_files - 1))->sp_sec)
      return (1);

   for (index = 0; index < n_files; index++)
      if (start_sec < (dbase + index)->sp_sec)
         break;

   if (index == n_files)
      return (1);

   if (stop_sec < (dbase + index)->st_sec)
      return (1);

   st_inx = index;

   if (!(finput = fopen ((dbase + st_inx)->line, "r")))
   {
      fprintf (stderr, "%s:  error opening %s\n",
               progname, (dbase + st_inx)->line);
      exit (-1);
   }

 do
 {
   if ((UTIL_getbuffer_MP (&m.buffer, finput, UTIL_GET_NON_BLOCKING)) == EOF_REACHED )
   {
     fclose (finput);           /* done with this file, time for next */
     st_inx++;
     if (st_inx < n_files)
     {
       if (!(finput = fopen ((dbase + st_inx)->line, "r")))
       {
         fprintf (stderr, "%s:  error opening %s\n",
               progname, (dbase + st_inx)->line);
         exit (-1);
       }
       if ((UTIL_getbuffer_MP (&m.buffer, finput, UTIL_GET_NON_BLOCKING)) == EOF_REACHED )
       {
         fclose (finput);
         fprintf (stderr, "%s:  error opening %s\n",
               progname, (dbase + st_inx)->line);
         exit (-1);
       }
     }
     else
       break;                 /* ran out of files, exit do loop */
   }
     inst_type = (m.buffer.packet.mpx.mini_packet[0] >> 4) & 0xf;
     chan = (m.buffer.packet.mpx.mini_packet[7] >> 3) & 0x7;
     if (chan == 7)                                     /* Combined Mode ? */
       chan = m.buffer.packet.mpx.mini_packet[7] & 0x7;
     band = (m.buffer.packet.mpx.mini_packet[5] >> 7) & 0x1;
     if( (inst_type == WFR) && (chan == chan_select) )
     {
         mp_len = UTIL_MP_length (&m.buffer);                      
         if ( (m.buffer.packet.mpp.mini_packet[6] & 0x08) == 0x08 )
       	 { if (mp_len > 0x1007) mp_len=0x1007;  /* truncate, MSF bit set */
         }
         else
       	 { if (mp_len > 0x1005) mp_len=0x1005;  /* truncate, MSF bit clear */
         }
         RTI =  (m.buffer.packet.mpp.mini_packet[2] & 0xff) |
               ((m.buffer.packet.mpp.mini_packet[3] & 0xff) << 8 );
         pkt_sclk = UTIL_event_time (&m.buffer, 0);
         epoch = (m.buffer.packet.cds_tag.epoch[0]  << 24) |
                 (m.buffer.packet.cds_tag.epoch[1]  << 16) |
                 (m.buffer.packet.cds_tag.epoch[2]  <<  8) |
                 (m.buffer.packet.cds_tag.epoch[3]  <<  0) ;
         pkt_epoc = pkt_sclk + epoch;
/*                                                              */
/*       Newest SCET calculation scheme: 6-Jan-1999 TFA         */
/*                                                              */
         if (epoch)                     /* JPL data has non-zero epoch */
         {
           evt_clk.seconds = pkt_sclk;         /* SCLK seconds */
           evt_clk.fine    = UTIL_extract_MP_RTI(&m.buffer)<<5;   /* SCLK fine */
           evt_tim = UTIL_event_scet (&m.buffer, evt_clk);
           pkt_event = UTIL_event_scet_tm (*evt_tim, 0);
           pkt_event->tm_yday++;                /* days after Jan. 1 */
           pkt_event->tm_mon++;                 /* months since Jan */
           year = pkt_event->tm_year + 1900;
           mon  = pkt_event->tm_mon;            /* month, 1...12 */
           mday = pkt_event->tm_mday;           /* day of month */
           doy  = pkt_event->tm_yday;
           hr   = pkt_event->tm_hour;
           mn   = pkt_event->tm_min;
           sec  = (double) pkt_event->tm_sec +
                  (double)(evt_tim->milliseconds % 1000)/1000.;
         }
         else                   /* IOWA data has epoch=0 */
         {
           pkt_event = gmtime(&pkt_epoc);
           pkt_event->tm_yday++;         /* gmtime returns days after Jan. 1 */
           pkt_event->tm_mon++;                 /* months since Jan */
           year = pkt_event->tm_year + 1900;
           mon  = pkt_event->tm_mon;            /* month, 1...12 */
           mday = pkt_event->tm_mday;           /* day of month */
           doy  = pkt_event->tm_yday;
           hr   = pkt_event->tm_hour;
           mn   = pkt_event->tm_min;
           sec  = (double) pkt_event->tm_sec +
                  ((double)(UTIL_extract_MP_RTI(&m.buffer)*125))/1000.;
         }

         pkt_sec = ttime ( &year, &mon, &mday, &doy, &hr, &mn, &sec );

         if (pkt_sec < stop_sec)
         {
           if ((mp_len == 0x0405) || (mp_len == 0x0407) ||      /* 512samp */
               (mp_len == 0x0805) || (mp_len == 0x0807) ||      /* 1Ksamp */
               (mp_len == 0x1005) || (mp_len == 0x1007) )       /* 2Ksamp */
           {
             iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
             if ( (pkt_sec >= start_sec) && (pkt_sec < stop_sec) )
             {
	       num_rec++;
	       if (num_rec == num_skip)
	       {
	         sprintf (time_string, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f",
	       		  year, doy,hr,mn,sec);
	         sprintf (toplbl, "WFR Raw Data ");
	         sprintf (toplbl, "%s%s", toplbl, time_string);

                 if ( band )
	     	 {
		  deltat = 140.e-6;		 /* Hi-Band ? */
                  cal_factor = 9.45;            /* dBmax factor ?? */
		 }
	         else
	     	 {
		  deltat = 0.010;		/* Lo-Band ? */
                  cal_factor = 9.63;            /* dBmax factor ?? */
		 }
                 if ( (m.buffer.packet.mpp.mini_packet[6] & 0x08) == 0x08 )
       	             idx = 10;                     /* MSF bit set */
                 else
	             idx = 8;                      /* MSF bit clear */

                 if (chan_select == 0)
                  igain = 10*(m.buffer.packet.mpp.mini_packet[5] & 0x03);
                 else if (chan_select == 1)
                  igain = 10*((m.buffer.packet.mpp.mini_packet[5]>>2) & 0x03);
                 else
                  igain = 10*((m.buffer.packet.mpp.mini_packet[5]>>4) & 0x03);

                 if (chan_select == 0)
	         { if (m.buffer.packet.mpp.mini_packet[6] & 0x01)
		   {
                     ant_length = -9.26;            /* Ex dipole */
	             strcpy (sensor_string, "  Sensor: EX ");
		   }
		   else
		   {
	             strcpy (sensor_string, "  Sensor: EX+");
                     ant_length = -5.00;            /* Ex+ monopole */
		   }
	         }
                 else if (chan_select == 1)
	         { if (m.buffer.packet.mpp.mini_packet[6] & 0x02)
		   {
	             sprintf (sensor_string, "  Sensor: EZ ");
                     ant_length = -5.00;            /* Ez monopole */
		   }
		   else
		   {
	             sprintf (sensor_string, "  Sensor: EX-");
                     ant_length = -5.00;            /* Ex- monopole */
		   }
	         }
                 else if (chan_select == 2)
                 { ant_length = 1./24.0;           /* mag. pre-amp gain */
                   if ( (m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x80 )
                     ant_length = ant_length*0.1474; /* Bx volts per nT @ 1 KHz */
		   else
                     ant_length = ant_length*0.0461; /* Bx volts per nT @ 10 Hz */
	           if (m.buffer.packet.mpp.mini_packet[6] & 0x04)
	             strcpy (sensor_string, "  Sensor: LP ");
		   else
	             strcpy (sensor_string, "  Sensor: BX ");
		 }
                 else if (chan_select == 3)
                 { ant_length = 1./24.0;           /* mag. pre-amp gain */
                   if ( (m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x80 )
                     ant_length = ant_length*0.1467; /* By volts per nT @ 1 KHz */
		   else
                     ant_length = ant_length*0.0456; /* By volts per nT @ 10 HHz */
	           sprintf (sensor_string, "  Sensor: BY ");
		 }
                 else if (chan_select == 4)
                 { ant_length = 1./24.0;           /* mag. pre-amp gain */
                   if ( (m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x80 )
                     ant_length = ant_length*0.1466; /* Bz volts per nT @ 1 KHz */
		   else
                     ant_length = ant_length*0.0458; /* Bz volts per nT @ 10 Hz */
	           sprintf (sensor_string, "  Sensor: BZ ");
		 }

	         sprintf (toplbl, "%s%s", toplbl, sensor_string);

	         sprintf (gain_string, "  Gain: %2.2d dB", igain);
	         sprintf (toplbl, "%s%s", toplbl, gain_string);

                 gain = igain;
                 if ( ((m.buffer.packet.mpp.mini_packet[5] & 0x80) != 0x80 ) &&
                      igain == 30)
                   gain = 27;                   /* gain @ 10 Hz is only 27 dB */
                 gain = gain + cal_factor;
                 gain = pow (10., gain/20.);
		 gain = gain / sqrt(2.);	/* cal factor was counts/Vrms */

                 nsamp = 512;
                 if ((mp_len == 0x0405) || (mp_len == 0x0407))
                  nsamp = 512;                                    /* 512samp */
                 else if ((mp_len == 0x0805) || (mp_len == 0x0807))
                  nsamp = 1024;                                   /* 1Ksamp */
                 else if ((mp_len == 0x1005) || (mp_len == 0x1007))
                  nsamp = 2048;                                   /* 2Ksamp */

                 ymin=4095.0;
                 ymax=0.0;
		 dc = 0.0;				/* estimate DC value */
                 for (i=idx; i<(idx+2*nsamp); (i=i+2))
                 { wfrdata =  m.buffer.packet.mpp.mini_packet[i] |
                           ( (m.buffer.packet.mpp.mini_packet[i+1]&0x0f) << 8);
                   dc = dc + (double)wfrdata;
		 }
		 dc = dc / (double)nsamp;

                 for (i=idx; i<(idx+2*nsamp); (i=i+2))
                 { wfrdata =  m.buffer.packet.mpp.mini_packet[i] |
                           ( (m.buffer.packet.mpp.mini_packet[i+1]&0x0f) << 8);
                   y[(i-idx)/2] = (float)wfrdata;

/*                   wfrcal[(i-idx)/2] =  ((double)wfrdata - 2047.5) / (2047.5*gain);	*/

                   wfrcal[(i-idx)/2] =  ((double)wfrdata - dc) / (2047.5*gain);


                   wfrcal[(i-idx)/2] = wfrcal[(i-idx)/2] / ant_length;



                   x[(i-idx)/2] = deltat * (float)(i-idx)/2;
                   if ( y[(i-idx)/2] > ymax)
                          ymax = y[(i-idx)/2];
                   if ( y[(i-idx)/2] < ymin)
                          ymin = y[(i-idx)/2];
                 }
                 if ( (ybottom != 0) || (ytop != 0) )
	         { ymin = ybottom;
	           ymax = ytop;
	         }
                 fprintf (fperror, "%s\n\n", MIME_Header);
                 fprintf (stdout, "%s\n\n", MIME_Header);
		 fflush (stdout);
		 if ( strcmp (gif_file, "TEXT") )
		 {
		 pgbegin_ (&Zero, gif_file, &One, &One, strlen(gif_file));

		/* invert Black and White */

	         pgscr_ (&Zero, &Fone, &Fone, &Fone);
		 pgscr_ (&One, &Fzero, &Fzero, &Fzero);
		 pgslw_ (&Three);
		 xmin=0.0; xmax=deltat*(float)nsamp;
		 pgenv_ ( &xmin, &xmax, &ymin, &ymax, &Zero, &One);
		 pglabel_ ("t, seconds", "Data Number", 
			    toplbl, strlen("t, seconds"),
			    strlen("Data Number"), strlen(toplbl));
		 pgline_ (&nsamp, &x[0], &y[0]);
	         pgend_ ();
		 }
		 else
		 {
                 printf ("%s\n", toplbl);
		 if (chan_select <= 1)
		 printf ("Sample #     Milliseconds     Raw Data Value      E,Volts/meter\n");
		 else
		 printf ("Sample #     Milliseconds     Raw Data Value       B,nanoteslas\n");
		 printf ("--------     ------------     --------------     --------------\n");
                 for (i=0; i<nsamp; i++)
                 {
                   printf ("%8d     %12.4f     %14.0f     %14.3e\n", (i+1), 1000.*x[i], y[i], wfrcal[i]);
                 }
		 }
                 no_data = False;
                 return (no_data);
	       }
             } /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
           } /* if ((mp_len == 0x805) || (mp_len == 0x807)) etc. */
         } /* if (pkt_sec < stop_sec) */
     } /* if( inst_type == WFR ) */
 }
 while (pkt_sec < stop_sec);
 fclose (finput);
 return (no_data);
}

void make_dbase (double start_sec, double stop_sec, struct file_structure *dbase, int *num_files)
{
   int          index, year, month, day, doy, hr, mn;
   int          begin_sclk, end_sclk, databits;
   double       sec, total_secs, file_start, file_stop;
   char         *file_name = "/opt/project/cassini/data/cassini.db";
   FILE         *finput;
   char         inputline [MAXCHARS];
   char         start [80], stop [80];

   if (strcmp(dataset,"IOWA") == 0)
     file_name = "/opt/project/cassini/data/CassiniIOWA.db";
   else
     file_name = "/opt/project/cassini/data/CassiniJPL.db";
   if (!(finput = fopen (file_name, "r")))
   {
      fprintf (stderr, "%s:  error opening %s\n", progname, file_name);
      exit (-1);
   }

   *num_files = 0;
   do
   {
      if (fgets (inputline, MAXCHARS, finput) != NULL)
      {
         index = *num_files;
         sscanf (inputline, "%s %s %x %x %s %x",
                                     start, stop, 
                                     &begin_sclk, &end_sclk,
                                     (dbase+index)->line,
                                     &databits);
                                   
/* get file start time and insert into data base */

         if (parsetime (start, &year, &month, &day, &doy, &hr, &mn, &sec))
         {
          fprintf (stderr, "%s: error parsing %s\n", progname, start);
          exit (1);
         }
         (dbase + index)->year = year;
         (dbase + index)->day = doy;
         (dbase + index)->hr = hr;
         (dbase + index)->mn = mn;
         (dbase + index)->sec = sec;
         (dbase + index)->ms  = 1000.*((double)(hr*3600+mn*60)+(double)sec);

         total_secs = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );
         (dbase + index)->st_sec = total_secs;
         file_start = total_secs;

/* get file stop time and insert into data base */

         if (parsetime (stop, &year, &month, &day, &doy, &hr, &mn, &sec))
         {
          fprintf (stderr, "%s: error parsing %s\n", progname, stop);
          exit (1);
         }

         total_secs = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );
         (dbase + index)->sp_sec = total_secs;
         file_stop = total_secs;
         
         if ( (file_start < stop_sec) && (file_stop >= start_sec))
           (*num_files)++;
      }
   }
   while (feof (finput) == 0);

   fclose (finput);

}
