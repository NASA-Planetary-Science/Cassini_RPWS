/*
        wfrall.c        written 03/20/01 by TFA to print all 5 channels
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
static  char	*units="raw";
static	double	root2;

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

 if (!strcmp (entries[2].val, "vlt") )
   units = "vlt";
 else if (!strcmp (entries[2].val, "raw") )
   units = "raw";
 else
   units = "idk";

root2 = sqrt(2.0);

 fprintf (fperror, " %d %d %d %d %d %d %lf Units: %s\n",
 		 year, month, day, doy, hr, mn, sec, units);

 sprintf (MIME_Header, "Content-type: text/plain");

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
 int    i, idx, RTI, igain, agc, iant, first=True, last_RTI;
 int    year, month, day, doy, hr, mn, mon, mday;
 int    Zero=0, One=1;
 int	black, white;
 double gain, cal_factor;
 float  Fzero=0.0, Fone=1.0;
 unsigned char save_stat[4];
 char	toplbl[64];
 char	last_time_string[32];
 char	time_string[32];
 char	gain_string[16];
 char	sensor_string[16];
 int	zeroes[5];
 float  x[2048], y[2048], y0[2048], y1[2048], y2[2048], y3[2048], y4[2048];
 float  xmin, xmax, ymin, ymax, deltat, dc, ant_length;
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
     if( (inst_type == WFR) )
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
	   chan_select = chan;
           if ((mp_len == 0x0405) || (mp_len == 0x0407) ||      /* 512samp */
               (mp_len == 0x0805) || (mp_len == 0x0807) ||      /* 1Ksamp */
               (mp_len == 0x1005) || (mp_len == 0x1007) )       /* 2Ksamp */
           {
             iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
             if ( (pkt_sec >= start_sec) && (pkt_sec < stop_sec) )
             {
	       num_rec++;
	       {
	         sprintf (time_string, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f",
	       		  year, doy,hr,mn,sec);
	         sprintf (toplbl, "WFR Raw Data ");
	         sprintf (toplbl, "%s%s", toplbl, time_string);

                 if ( band )
	     	 {
		  deltat = 140.e-6;		 /* Hi-Band ? */
		  cal_factor = 9.45;
		 }
	         else
	     	 {
		  deltat = 0.010;		/* Lo-Band ? */
		  cal_factor = 9.63;
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
                 gain = igain;
		 if ( ((m.buffer.packet.mpp.mini_packet[5] & 0x80) != 0x80 ) &&
		      igain == 30)
		   gain = 27;                   /* gain @ 10 Hz is only 27 dB */
		 gain = gain + cal_factor;
		 gain = pow (10., gain/20.);
		 gain = gain*2047.5;      /* gain is relative to peak response */
		 if (chan >= 2)
		   gain = gain/24.0;           /* mag. pre-amp gain */

		 ant_length = 1.00;
                 if (chan_select == 0)
	         { if (m.buffer.packet.mpp.mini_packet[6] & 0x01)
		   {
                     ant_length = 9.26;            /* Ex dipole */
	             strcpy (sensor_string, "  Sensor: EX ");
		   }
		   else
		   {
	             strcpy (sensor_string, "  Sensor: EX+");
                     ant_length = 5.00;            /* Ex+ monopole */
		   }
	         }
                 else if (chan_select == 1)
	         { if (m.buffer.packet.mpp.mini_packet[6] & 0x02)
		   {
	             sprintf (sensor_string, "  Sensor: EZ ");
                     ant_length = 5.00;            /* Ez monopole */
		   }
		   else
		   {
	             sprintf (sensor_string, "  Sensor: EX-");
                     ant_length = 5.00;            /* Ex- monopole */
		   }
	         }
                 else if (chan_select == 2)
                 { 
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
                 {
                   if ( (m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x80 )
                     ant_length = ant_length*0.1467; /* By volts per nT @ 1 KHz */
		   else
                     ant_length = ant_length*0.0456; /* By volts per nT @ 10 HHz */
	           sprintf (sensor_string, "  Sensor: BY ");
		 }
                 else if (chan_select == 4)
                 {
                   if ( (m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x80 )
                     ant_length = ant_length*0.1466; /* Bz volts per nT @ 1 KHz */
		   else
                     ant_length = ant_length*0.0458; /* Bz volts per nT @ 10 Hz */
	           sprintf (sensor_string, "  Sensor: BZ ");
		 }

	         sprintf (toplbl, "%s%s", toplbl, sensor_string);

	         sprintf (gain_string, "  Gain: %2.2d dB", igain);
	         sprintf (toplbl, "%s%s", toplbl, gain_string);

                 nsamp = 512;
                 if ((mp_len == 0x0405) || (mp_len == 0x0407))
                  nsamp = 512;                                    /* 512samp */
                 else if ((mp_len == 0x0805) || (mp_len == 0x0807))
                  nsamp = 1024;                                   /* 1Ksamp */
                 else if ((mp_len == 0x1005) || (mp_len == 0x1007))
                  nsamp = 2048;                                   /* 2Ksamp */

                 ymin=4095.0;
                 ymax=0.0;

		 dc = 0.0;
                 for (i=idx; i<(idx+2*nsamp); (i=i+2))
                 {
		   wfrdata =  m.buffer.packet.mpp.mini_packet[i] |
                           ( (m.buffer.packet.mpp.mini_packet[i+1]&0x0f) << 8);
                   y[(i-idx)/2] = (float)wfrdata;
		   dc = dc + (float)wfrdata;
                 }
		 dc = dc / (float)nsamp;
		 if (!strcmp (units, "vlt") )
		 {
		   for (i=0; i<nsamp; i++)
		   { y[i] = root2*(y[i] - dc)/gain;
		     y[i] = y[i]/ant_length;
		   }
		 }

                 if ( (ybottom != 0) || (ytop != 0) )
	         { ymin = ybottom;
	           ymax = ytop;
	         }
		 if (first)
                 {
                   fprintf (stdout, "%s\n\n", MIME_Header);
		   fflush (stdout);
		   first = False;
		   last_RTI = RTI;
	           sprintf (last_time_string, "%s", time_string);
		   for (i=0; i<2048; i++) y0[i] = 0;
		   for (i=0; i<2048; i++) y1[i] = 0;
		   for (i=0; i<2048; i++) y2[i] = 0;
		   for (i=0; i<2048; i++) y3[i] = 0;
		   for (i=0; i<2048; i++) y4[i] = 0;
		   for (i=0; i<4; i++)
		     save_stat[i] = m.buffer.packet.mpp.mini_packet[i+4];
		 }
                 no_data = False;
		 if (RTI == last_RTI)
		 { if      (chan == 0) for (i=0; i<nsamp; i++) y0[i] = y[i];
		   else if (chan == 1) for (i=0; i<nsamp; i++) y1[i] = y[i];
		   else if (chan == 2) for (i=0; i<nsamp; i++) y2[i] = y[i];
		   else if (chan == 3) for (i=0; i<nsamp; i++) y3[i] = y[i];
		   else if (chan == 4) for (i=0; i<nsamp; i++) y4[i] = y[i];
		   nlast = nsamp;
		 }
		 else if (RTI != last_RTI)
                 { printf ("%s ", last_time_string);
		   if ( (save_stat[1] >> 7) & 0x1 )
		   { printf (" Band: 2.5KHz ");
		     deltat = 140.e-6;		 /* Hi-Band ? */
		   }
		   else
		   { printf (" Band:   40Hz ");
		     deltat = 0.010;		/* Lo-Band ? */
		   }
		   printf (" Gains: %2d %2d %2d",
		   		10*(save_stat[1] & 0x03),
				10*((save_stat[1]>>2) & 0x03),
				10*((save_stat[1]>>4) & 0x03) );
	           if (save_stat[2] & 0x01)
	             printf ("  Sensors: EX ");
		   else
	             printf ("  Sensors: EX+");
	           if (save_stat[2] & 0x02)
	             printf (" EZ ");
		   else
	             printf (" EX-");
	           if (save_stat[2] & 0x04)
	             printf (" LP ");
		   else
	             printf (" BX ");
		   printf ("\n");


/* Do some data gap checking... */
		   zeroes[0] = 0;
		   zeroes[1] = 0;
		   zeroes[2] = 0;
		   zeroes[3] = 0;
		   zeroes[4] = 0;
		   for (i=0; i<nlast; i++)
		     if (y0[i] == 0) zeroes[0]++;
		   for (i=0; i<nlast; i++)
		     if (y1[i] == 0.0) zeroes[1]++;
		   for (i=0; i<nlast; i++)
		     if (y2[i] == 0.0) zeroes[2]++;
		   for (i=0; i<nlast; i++)
		     if (y3[i] == 0.0) zeroes[3]++;
		   for (i=0; i<nlast; i++)
		     if (y4[i] == 0.0) zeroes[4]++;
		   if ( (zeroes[0] >= 512) ||
		        (zeroes[1] >= 512) ||
		        (zeroes[2] >= 512) ||
		        (zeroes[3] >= 512) ||
		        (zeroes[4] >= 512) )
		   {
		      fprintf (fperror, "Missing Data at %s RTI:%4.4X",
		         last_time_string, last_RTI);
		      for (i=0; i<5; i++)
		        fprintf (fperror, "  %4d", zeroes[i]);
		      fprintf (fperror, "\n");
		   }

		   if ( !strcmp (units, "raw") )
		   {  printf ("Sample #     Milliseconds     WFR0 WFR1 WFR2 WFR3 WFR4\n");
		      printf ("--------     ------------     ---- ---- ---- ---- ----\n");
		   }
		   else if ( !strcmp (units, "vlt") )
		   {  printf ("Sample #     Milliseconds      WFR0(V/m)  WFR1(V/m)   WFR2(nT)   WFR3(nT)   WFR4(nT)\n");
		      printf ("--------     ------------     ---------- ---------- ---------- ---------- ----------\n");
		   }
                   for (i=0; i<nlast; i++)
                   {
		     if ( !strcmp (units, "raw") )
                       printf ("%8d     %12.4f     %4.0f %4.0f %4.0f %4.0f %4.0f\n",
		        (i+1), 1000.*deltat*i, y0[i], y1[i], y2[i], y3[i], y4[i]);
		     else  if ( !strcmp (units, "vlt") )
		       printf ("%8d     %12.4f     %10.3e %10.3e %10.3e %10.3e %10.3e\n",
		        (i+1), 1000.*deltat*i, y0[i], y1[i], y2[i], y3[i], y4[i]);
                   }
		 last_RTI = RTI;
		 for (i=0; i<2048; i++) y0[i] = 0;
		 for (i=0; i<2048; i++) y1[i] = 0;
		 for (i=0; i<2048; i++) y2[i] = 0;
		 for (i=0; i<2048; i++) y3[i] = 0;
		 for (i=0; i<2048; i++) y4[i] = 0;
		 { if      (chan == 0) for (i=0; i<nsamp; i++) y0[i] = y[i];
		   else if (chan == 1) for (i=0; i<nsamp; i++) y1[i] = y[i];
		   else if (chan == 2) for (i=0; i<nsamp; i++) y2[i] = y[i];
		   else if (chan == 3) for (i=0; i<nsamp; i++) y3[i] = y[i];
		   else if (chan == 4) for (i=0; i<nsamp; i++) y4[i] = y[i];
		 }
		 for (i=0; i<4; i++)
		   save_stat[i] = m.buffer.packet.mpp.mini_packet[i+4];
	         sprintf (last_time_string, "%s", time_string);
		 }
	       }
             } /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
           } /* if ((mp_len == 0x805) || (mp_len == 0x807)) etc. */
         } /* if (pkt_sec < stop_sec) */
     } /* if( inst_type == WFR ) */
 }
 while (pkt_sec < stop_sec);
 if (!no_data )
 { printf ("%s ", last_time_string);
   if ( (save_stat[1] >> 7) & 0x1 )
     printf (" Band: 2.5KHz ");
   else
     printf (" Band:   40Hz ");
   printf (" Gains: %2d %2d %2d",
  		10*(save_stat[1] & 0x03),
		10*((save_stat[1]>>2) & 0x03),
		10*((save_stat[1]>>4) & 0x03) );
   if (save_stat[2] & 0x01)
     printf ("  Sensors: EX ");
   else
     printf ("  Sensors: EX+");
   if (save_stat[2] & 0x02)
     printf (" EZ ");
   else
     printf (" EX-");
   if (save_stat[2] & 0x04)
     printf (" LP ");
   else
     printf (" BX ");
   printf ("\n");
   if ( !strcmp (units, "raw") )
   {  printf ("Sample #     Milliseconds     WFR0 WFR1 WFR2 WFR3 WFR4\n");
      printf ("--------     ------------     ---- ---- ---- ---- ----\n");
   }
   else if ( !strcmp (units, "vlt") )
   {  printf ("Sample #     Milliseconds      WFR0(V/m)  WFR1(V/m)   WFR2(nT)   WFR3(nT)   WFR4(nT)\n");
      printf ("--------     ------------     ---------- ---------- ---------- ---------- ----------\n");
   }
   for (i=0; i<nlast; i++)
   {
     if ( !strcmp (units, "raw") )
       printf ("%8d     %12.4f     %4.0f %4.0f %4.0f %4.0f %4.0f\n",
        (i+1), 1000.*deltat*i, y0[i], y1[i], y2[i], y3[i], y4[i]);
     else  if ( !strcmp (units, "vlt") )
       printf ("%8d     %12.4f     %10.3e %10.3e %10.3e %10.3e %10.3e\n",
        (i+1), 1000.*deltat*i, y0[i], y1[i], y2[i], y3[i], y4[i]);
   }
 }
 
 fclose (finput);
 return (no_data);
}

void make_dbase (double start_sec, double stop_sec, struct file_structure *dbase, int *num_files)
{
   int          index, year, month, day, doy, hr, mn;
   int          begin_sclk, end_sclk, databits;
   double       sec, total_secs, file_start, file_stop;
   char         *file_name = "/opt/project/cassini/data/database/CassiniIOWA.db";
   FILE         *finput;
   char         inputline [MAXCHARS];
   char         start [80], stop [80];

   if (strcmp(dataset,"IOWA") == 0)
     file_name = "/opt/project/cassini/data/database/CassiniIOWA.db";
   else
     file_name = "/opt/project/cassini/data/database/CassiniJPL.db";
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
