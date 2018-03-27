/*
        wbrplot.c       written 12/07/99 by TFA to process
                        Cassini RPWS 10 KHz WBR data, plot raw data
			
        Modifications:
			Allow odd packet lengths. 3-Mar-03 TFA
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


#define WORD  unsigned short
#define DWORD unsigned long

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

static	FILE	*fperror;
static	char	*progname;
static	char	*dataset="JPL";
static	int	ybottom=0;
static	int	ytop=0;
static	int	num_skip=1;
static	char	MIME_Header[40];
static	char	gif_file[8];
static  int	calibrated;

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
entry   entries [MAXENTRIES];
struct  file_structure files [MAXFILES];       

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

 if (parsetime (entries[0].val, &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("%s: error parsing %s\n", progname, entries[0].val);
    exit (1);
 }
 start_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );

 fprintf (fperror, " %d %d %d %d %d %d %lf \n",
 		 year, month, day, doy, hr, mn, sec);

 if (parsetime (entries[1].val, &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stdout, "Content-type: text/plain\n\n");
    printf ("%s: error parsing %s\n", progname, entries[1].val);
    exit (1);
 }
 stop_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );

 fprintf (fperror, " %d %d %d %d %d %d %lf \n",
 		 year, month, day, doy, hr, mn, sec);

 sscanf (entries[2].val, "%d", &num_skip);
 num_skip--;
 if (num_skip < 0)
   num_skip = 1;
 sscanf (entries[3].val, "%d", &ybottom);
 sscanf (entries[4].val, "%d", &ytop);
 sscanf (entries[5].val, "%d", &scaling);

 calibrated = 0;
 if ( scaling == 0 )			/* Manual box checked ? */
 { if (ybottom >= ytop)
   { ybottom = 0;
     ytop = 0;
   }
   if ( (ybottom == 0) && (ytop == 0) )
     fprintf (fperror, "Autoscaling data... \n");
   else
     fprintf (fperror, "Ybottom = %d     Ytop = %d \n", ybottom, ytop);
 }
 else if ( scaling == 1 )		/* Autoscale box checked ? */
 { ybottom = 0;
   ytop = 0;
   fprintf (fperror, "Autoscaling data... \n");
 }
 else if ( scaling == 2 )		/* Calibrated box checked ? */
 { ybottom = 0;
   ytop = 0;
   calibrated = 1;
   fprintf (fperror, "Calibrating data... \n");
 }
/*
	See if the desired output is GIF or PS.
*/
  if (strcmp (entries[6].name, "device_name") )
  {
    fprintf (fperror, "entries[6].name is not equal to device_name\n");
    sprintf (MIME_Header, "Content-type: image/gif");
    sprintf (gif_file, "-/gif");
  }
  else   if ( !strcmp (entries[6].val, "Z") )	/* GIF file ? */
  {
    fprintf (fperror, "entries[6].val is equal to Z\n");
    sprintf (MIME_Header, "Content-type: image/gif");
    sprintf (gif_file, "-/gif");
    if ( !strcmp (entries[7].name, "gif_mime_header") )
      sprintf (MIME_Header, "Content-type: %s", entries[7].val);
  }

  if (strcmp (entries[7].name, "device_name") )
  {
    fprintf (fperror, "entries[7].name is not equal to device_name\n");
  }
  else   if ( !strcmp (entries[7].val, "PS") )	/* PS file ? */
  {
    fprintf (fperror, "entries[7].val is equal to PS\n");
    sprintf (MIME_Header, "Content-type: application/postscript");
    sprintf (gif_file, "-/ps");
    if ( !strcmp (entries[8].name, "ps_mime_header") )
      sprintf (MIME_Header, "Content-type: %s", entries[8].val);
  }

  if (strcmp (entries[8].name, "device_name") )
  {
    fprintf (fperror, "entries[8].name is not equal to device_name\n");
  }
  else   if ( !strcmp (entries[8].val, "TEXT") )	/* Text file ? */
  {
    fprintf (fperror, "entries[8].val is equal to TEXT\n");
    sprintf (MIME_Header, "Content-type: text/plain");
    sprintf (gif_file, "TEXT");
    if ( !strcmp (entries[9].name, "text_mime_header") )
      sprintf (MIME_Header, "Content-type: %s", entries[9].val);
  }

  fprintf (fperror, "MIME Header is %s\n", MIME_Header);

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
 int	first, num_rec;
 int    epoch;
 float  x[32767], y[32767], wbrcal[32767];
 float  xmin, xmax, ymin, ymax, deltat;
 int    inst_type, mp_len;
 int    wbrdata, nsamp, nlast=2048;
 int    i, idx, RTI, igain, agc, iant, min_gain;
 int    year, month, day, doy, hr, mn, mon, mday;
 long	bytes_output, total_bytes=0;
 int	black, white;
 float  Fzero=0.0, Fone=1.0;
 int    Zero=0, One=1;
 char	*toplbl="WBR Raw Data EZ 50dB SCET 1998-364/00:00:00.000 (SCLK ABCDEF0.0  AGC 00 C 0.0000)";
 char   *antenna[5]={"EX","BX","EZ","HF","LP"};
 char	time_string[32];
 char	gain_string[16];
 double gain, gmin, gain_adjust, value, cal_factor, ant_length, dc;
 double pkt_sec=0, offset=0;
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
   int index;
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
     if( inst_type == WBR )
     {
         mp_len = UTIL_MP_length (&m.buffer);                      
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

/* See if MSF time info (SUB_RTI) is included */
         if ( (m.buffer.packet.mpp.mini_packet[6] & 0x08) == 0x08 )
         {
/*		WBR was started at SUB_RTI count after start of RTI */

            offset = 125. - 0.512*(float)m.buffer.packet.mpp.mini_packet[9];
            pkt_sec = pkt_sec + offset / 1000.0;
            emitt (pkt_sec, &year, &mon, &day, &doy, &hr, &mn, &sec);
         }            
         if (pkt_sec < stop_sec)
         {
/*
           if ((mp_len == 0x0405) || (mp_len == 0x0407) ||      
               (mp_len == 0x0805) || (mp_len == 0x0807) ||      
               (mp_len == 0x0c05) || (mp_len == 0x0c07) ||      
               (mp_len == 0x1005) || (mp_len == 0x1007) ||      
               (mp_len == 0x1405) || (mp_len == 0x1407) )       
*/
           {
             if ( (pkt_sec >= start_sec) && (pkt_sec < stop_sec) )
	     {
	       num_rec++;
	       if (num_rec == num_skip)

	       { sprintf (time_string, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f",
	       		year, doy,hr,mn,sec);
                 iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
      	         sprintf (toplbl, "WBR Raw Data %s %s", antenna[iant], time_string);

	         fprintf(stderr,"%s\n", toplbl);
                 if ( (m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x00 )
		 {
		  deltat = 36.e-3;		 /* 10 KHz ? */
		  cal_factor = 6.33;		 /* dBMax factor */
		 }
	         else
		 {
	     	  deltat = 4.5e-3;		 /* 80 KHz ? */
		  cal_factor = 6.43;		 /* dBMax factor */
		 }
		 ant_length = 1.00;
                 if ( (m.buffer.packet.mpp.mini_packet[6] & 0x07) == 0x00 )
       	             ant_length = 9.26;          /* EX dipole */
                 else if ( (m.buffer.packet.mpp.mini_packet[6] & 0x07) == 0x02 )
	             ant_length = 5.00;		/* EZ monopole */
                 if ( (m.buffer.packet.mpp.mini_packet[6] & 0x08) == 0x08 )
       	             idx = 10;                     /* MSF bit set */
                 else
	             idx = 8;                      /* MSF bit clear */
                 agc = m.buffer.packet.mpp.mini_packet[7];
                 igain = 10*(m.buffer.packet.mpp.mini_packet[5] & 0x07);
                 gain = igain;
		 gain = gain + cal_factor;	/* fold in cal factor */
                 gain = pow (10., gain/20.);
		 gain = gain / sqrt(2.);	/* cal factor was counts/Vrms */
        
	         sprintf (gain_string, "   Gain: %2.2d dB", igain);
                 sprintf (toplbl, "%s%s", toplbl, gain_string);
	       
                 nsamp = 0;
		 nlast = mp_len - idx +3; 
/*
                 if ( (mp_len == 0x0405) || (mp_len == 0x0407) )      
		   nlast = 1024;
	         else if ( (mp_len == 0x0805) || (mp_len == 0x0807) ) 
		   nlast = 2048;
	         else if ( (mp_len == 0x0c05) || (mp_len == 0x0c07) ) 
		   nlast = 3072;
                 else if ( (mp_len == 0x1005) || (mp_len == 0x1007) ) 
	           nlast = 4096;
                 else if ( (mp_len == 0x1405) || (mp_len == 0x1407) ) 
	           nlast = 5120;
*/
                 ymin=255.0;
                 ymax=0.0;
	         nsamp = nlast;
		 dc = 0.0;			/* estimate DC value */
                 for (i=idx; i<(idx+nlast); i=i++)
                 { wbrdata =  m.buffer.packet.mpp.mini_packet[i];
		   dc = dc + (double)wbrdata;
                 }
		 dc = dc / (double)nsamp;

	fprintf (fperror, " Antenna Length = %8.2f ", ant_length);
	fprintf (fperror, "    DC Offset = %8.2f ", dc);
	fprintf (fperror, "    Cal Factor= %8.2f \n", cal_factor);

                 for (i=idx; i<(idx+nlast); i=i++)
                 { wbrdata =  m.buffer.packet.mpp.mini_packet[i];
                   y[(i-idx)] = (float)wbrdata;
                   x[(i-idx)] = deltat * (float)(i-idx);
		   wbrcal[(i-idx)] = ((double)wbrdata - dc) / 
		   		     (127.5*gain*ant_length); /* volts/meter */
/* calibrate if desired */

		  if ( (calibrated == 1) && (ant_length > 1.00) && ( !strcmp (gif_file, "TEXT") ) )
		    y[(i-idx)] = wbrcal[(i-idx)];
		  
                  if ( y[(i-idx)] > ymax)
                          ymax = y[(i-idx)];
                    if ( y[(i-idx)] < ymin)
                          ymin = y[(i-idx)];
	
                 }

                 if ( (ybottom != 0) || (ytop != 0) )
	         { ymin = ybottom;
	           ymax = ytop;
	         }

                 fprintf (stdout, "%s\n\n", MIME_Header);
		 fflush (stdout);
                 if ( strcmp (gif_file, "TEXT") )
		 {
		 pgbegin_ (&Zero, gif_file, &One, &One, strlen(gif_file));

		/* invert Black and White */

	         pgscr_ (&Zero, &Fone, &Fone, &Fone);
		 pgscr_ (&One, &Fzero, &Fzero, &Fzero);

	         xmin = 0.0; xmax = deltat * (float)nsamp;
                 pgenv_ ( &xmin, &xmax, &ymin, &ymax, &Zero, &One);
	         pglabel_ ("t, milliseconds", "Data Number", toplbl,
			    strlen("t, milliseconds"),
			    strlen("Data Number"),
			    strlen(toplbl));
	         pgline_ (&nsamp, &x[0], &y[0]);
	         pgend_ ();
		 }
		 else
		 {
		   if (ant_length == 1.00)	/* can't calibrate ? */
		   { printf ("%s\n", toplbl);
		     printf ("Sample #     Milliseconds     Raw Data Value\n");
		     printf ("--------     ------------     --------------\n");
		     for (i=0; i<nsamp; i++)
		       printf ("%8d     %12.4f     %14.0f\n", (i+1), x[i], y[i]);
		   }
		   else
		   { printf ("%s\n", toplbl);
		     printf ("Sample #     Milliseconds     Raw Data Value      E,Volts/meter\n");
		     printf ("--------     ------------     --------------     --------------\n");
                     for (i=0; i<nsamp; i++)
                       printf ("%8d     %12.4f     %14.0f     %14.3e\n", (i+1), x[i], y[i], wbrcal[i]);
		   }
		   
		 }
                 no_data = False;
                 return (no_data);
	       }
             } /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
           } /* if ((mp_len == 0x805) || (mp_len == 0x807)) */
         } /* if (pkt_sec < stop_sec) */
     } /* if( inst_type == WBR ) */
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
