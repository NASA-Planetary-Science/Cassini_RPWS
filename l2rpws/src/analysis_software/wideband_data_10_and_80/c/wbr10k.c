/*
        wbr10k.c        written 10/21/98 by TFA to process
                        Cassini RPWS 10 KHz WBR data for input to das
        Modifications:
                        Ex dipole antenna length changed from 8.66 meters
                        to 9.26 meters. TFA 12/4/98
                        SCET calculation updated. TFA 01/06/99
                        DAS library functions (e.g., ttime). TFA 01/15/99
			RFFT module replaces fft. TFA 09/20/00
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

static char *progname;
static char *dataset="JPL";

void	rffti (int *n, float *wsave);

void	rfftf (int *n, float *r, float *wsave);

int window (double *windo, int lenw, int weight);

void make_dbase (double start, double stop, struct file_structure *dbase, int *num_files);

int process_data (double start, double stop, struct file_structure *dbase, int n_files);

main(int argc,char **argv)
{
int     num_files;
int     year, month, day, doy, hr, mn;
double  sec, start_sec, stop_sec;
struct  file_structure files [MAXFILES];       

/***************************************************************/

 if (argc <= 2)
 { printf ("Usage is: %s start stop [dataset]\n", argv[0]);
   exit(0);
 }
 progname = argv [0];
 if (parsetime (argv[1], &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stderr, "%s: error parsing %s\n", argv[0], argv[1]);
    exit (1);
 }
 start_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );

 if (parsetime (argv[2], &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stderr, "%s: error parsing %s\n", argv[0], argv[2]);
    exit (1);
 }
 stop_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );

 if (argc == 4)
   dataset = argv[3];
 fprintf (stderr, "Selected data set is %s\n", dataset);


 make_dbase (start_sec, stop_sec, files, &num_files);
 if (num_files == 0)
 {
   fprintf (stderr, "No data coverage of the requested time interval.\n");
   exit (1);
 }

 if (process_data (start_sec, stop_sec, files, num_files) )
 {
   fprintf (stderr, "No data coverage of the requested time interval.\n");
   exit (1);
 }
 return (0);
}

int process_data (double start_sec, double stop_sec, struct file_structure *dbase, int n_files)
{
 int    epoch;
 int    inst_type, mp_len;
 int    wbrdata, nsamp, nlast=2048;
 int    i, idx, RTI, igain, agc, iant;
 int    year, month, day, doy, hr, mn, mon, mday;
 double Hanning[16384];
 double pwr, ant_length;
 float  dc;
 float  xreal[2048];
 float  wsave[2*2048+15];
 double fs, deltaf, gain;
 double cal_factor, bandw;
 double TWO_PI = (2.0 * 3.14159265358979324);
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
   float offset, spec [1026];
   FILE *finput;

   no_data = True;
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

 window ( Hanning, 2048, 3);
 nsamp = 2048;				/* default length */
 rffti_ ( &nsamp, wsave );		/* initialize work array */

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

         if (pkt_sec < stop_sec)
         {
           if ((mp_len == 0x0405) || (mp_len == 0x0407) ||      /* 1Ksamp */
               (mp_len == 0x0805) || (mp_len == 0x0807) ||      /* 2Ksamp */
               (mp_len == 0x0c05) || (mp_len == 0x0c07) ||      /* 3Ksamp */
               (mp_len == 0x1005) || (mp_len == 0x1007) ||      /* 4Ksamp */
               (mp_len == 0x1405) || (mp_len == 0x1407) )       /* 5Ksamp */
           {
             iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
             if ( ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) &&
                  ( (iant == 0) || (iant == 2) ) &&     /* Ex or Ez */
                  ((m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x00 )) /* 10 KHz ? */
             {
               if ( (m.buffer.packet.mpp.mini_packet[6] & 0x08) == 0x08 )
       	           idx = 10;                     /* MSF bit set */
               else
	           idx = 8;                      /* MSF bit clear */
               if ( (m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x80 )
               {  fs = 1./4.5e-06;                /* Hi-Band */
                  cal_factor = 6.43;              /* dBmax factor */
               }
               else
       	       {  fs = 1./36.e-06;                /* Lo-Band */
                  cal_factor = 6.33;              /* dBmax factor */
               }
               if (iant == 0)
                  ant_length = 9.26;              /* Ex dipole */
               else
                  ant_length = 5.00;              /* Ez monopole */
               agc = m.buffer.packet.mpp.mini_packet[7];
               bandw = 1.5*fs/2048;                    /* Hanning ENBW */
               igain = 10*(m.buffer.packet.mpp.mini_packet[5] & 0x07);
               gain = igain;
               gain = gain + cal_factor;
               gain = pow (10., gain/20.);
        
               nsamp = 0;
               dc = 0.0;
               if ((mp_len == 0x0405) || (mp_len == 0x0407))      /* 1Ksamp */
               {
                 for (i=idx; i<(idx+1024); i++)
                 { wbrdata =  m.buffer.packet.mpp.mini_packet[i];
	           dc = dc + (float)wbrdata;
                   xreal[nsamp] = (float)wbrdata;
	           nsamp++;
                 }
                 for (i=(idx+1024); i<(idx+2048); i++)
                 { wbrdata =  m.buffer.packet.mpp.mini_packet[i-1024];
	           dc = dc + (float)wbrdata;
                   xreal[nsamp] = (double)wbrdata;
	           nsamp++;
                 }
               }
               else
               {
                 for (i=idx; i<(idx+2048); i++)
                 { wbrdata =  m.buffer.packet.mpp.mini_packet[i];
	           dc = dc + (float)wbrdata;
                   xreal[nsamp] = (float)wbrdata;
	           nsamp++;
                 }
               }

	       dc = dc/(float)nsamp;

               if (nsamp != nlast)
               { window ( Hanning, nsamp, 3);
	         rffti_ ( &nsamp, wsave);
                 nlast = nsamp;
               }
               for (i=0; i<nsamp; i++)             /* window data */
               {
                  xreal[i] = (xreal[i] - dc) * Hanning[i] / (127.5*gain);
                  xreal[i] = xreal[i] / ant_length;     /* volts per meter */
               }

	       rfftf_ ( &nsamp, xreal, wsave );
	       xreal[0] = xreal[0] / (float)nsamp;
	       for (i=1; i<nsamp; i++)
	         xreal[i] = xreal[i] / (float)(nsamp/2);
	       xreal[nsamp-1] = xreal[nsamp-1] / 2.0;

	       spec[1] = xreal[0]*xreal[0] / bandw;
	       spec[1025] = (xreal[nsamp-1]*xreal[nsamp-1]) / bandw;
	       for (i=1; i<(nsamp/2); i++)
                   spec[i+1] = (xreal[2*i-1]*xreal[2*i-1] + 
		                xreal[2*i]*xreal[2*i]) / bandw;

/* first item to be output is seconds after start time */

               spec[0] = (pkt_sec - start_sec);
               fwrite (spec, sizeof(spec), 1, stdout);

               no_data = False;
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
