/*
        mfr.c           written 11/05/98 by TFA to process
                        Cassini RPWS MFR data for input to das
        Modifications:
                        Ex dipole antenna length changed from 8.66 meters
                        to 9.26 meters. TFA 12/4/98
                        SCET calculation updated. TFA 01/06/99
                        DAS library functions (e.g., ttime). TFA 01/13/99
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

static char *dataset="JPL";
static char *progname;
static int  chan_select;
static double  mfrcal[3][32][256];

void make_dbase (double start, double stop, struct file_structure *dbase, int *num_files);

int process_data (double start, double stop, struct file_structure *dbase, int n_files);

void    get_sweep (int data_set, unsigned char minidata[224], int mfrsweep[80]);

void cal_mfr (int chan, int mfrdata[80], float mfrspec[80]);

main(int argc,char **argv)
{
int     num_files;
int     year, month, day, doy, hr, mn;
double  sec, start_sec, stop_sec;
struct  file_structure files [MAXFILES];       

/***************************************************************/

 if (argc <= 3)
 { fprintf (stderr, "Usage is: %s [0/1] start stop [dataset]\n", argv[0]);
   exit(1);
 }
 progname = argv [0];

 chan_select = atol (argv[1]);                  /* 0 means E, 1 means B */
 if ((chan_select < 0) || (chan_select > 1))
 {
    fprintf (stderr, "%s: bad channel # %s\n", argv[0], argv[1]);
    exit (1);
 }

 if (parsetime (argv[2], &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stderr, "%s: error parsing %s\n", argv[0], argv[2]);
    exit (1);
 }
 start_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );

 if (parsetime (argv[3], &year, &month, &day, &doy, &hr, &mn, &sec))
 {
    fprintf (stderr, "%s: error parsing %s\n", argv[0], argv[3]);
    exit (1);
 }
 stop_sec = ttime ( &year, &month, &day, &doy, &hr, &mn, &sec );

 if (argc == 5)
   dataset = argv[4];
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
 int    inst_type, mp_len, chan;
 int    i, RTI, igain, dgf, iant;
 int    year, month, day, doy, hr, mn, mon, mday;
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
   int index, raw[80];
   float offset, spec [81], spec_out[81];
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
     chan = (m.buffer.packet.mpx.mini_packet[4] >> 1) & 0x3;
     if( (inst_type == MFR) && ((chan/2) == chan_select))
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
           doy  = pkt_event->tm_yday;
           mon  = pkt_event->tm_mon;            /* month, 1...12 */
           mday = pkt_event->tm_mday;           /* day of month */
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
           doy  = pkt_event->tm_yday;
           mon  = pkt_event->tm_mon;            /* month, 1...12 */
           mday = pkt_event->tm_mday;           /* day of month */
           hr   = pkt_event->tm_hour;
           mn   = pkt_event->tm_min;
           sec  = (double) pkt_event->tm_sec +
                  ((double)(UTIL_extract_MP_RTI(&m.buffer)*125))/1000.;
         }

         pkt_sec = ttime ( &year, &mon, &mday, &doy, &hr, &mn, &sec );

         if (pkt_sec < stop_sec)
         {
           if ( mp_len == 0x00E2 )       /* standard MFR packet only */
           {
             if (pkt_sec >= start_sec)
             {
               for (i=0; i<4; i++)      /* construct 4 MFR sweeps */
               {
                 get_sweep (i, &m.buffer.packet.mpp.mini_packet[5], raw);
                 cal_mfr (chan, raw, &spec[1]);

                 /* first item to be output is seconds after start time */

                 spec[0] = (pkt_sec + 8*i - start_sec);

                 if ((pkt_sec+8*i) < stop_sec)
                   fwrite (spec, sizeof(spec_out), 1, stdout);
               }
               no_data = False;
             } /* if (pkt_sec >= start_sec) */
           } /* if ((mp_len == 0x00E2) */
         } /* if (pkt_sec < stop_sec) */
     } /* if( inst_type == MFR ) */
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
   char         input_line [MAXCHARS];
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
      if (fgets (input_line, MAXCHARS, finput) != NULL)
      {
         index = *num_files;
         sscanf (input_line, "%s %s %x %x %s %x",
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


void    get_sweep (int data_set, unsigned char mp[224], int mfrsweep[80])
{
  int   i, which;
  
  which = data_set & 3;                         /* 0...3 data sets */
  for (i=0; i<16; i++)
    mfrsweep[i] = mp[i+16*(which/2)];        /* MFR1, data set 1 or 2 */
  for (i=0; i<32; i++)
    mfrsweep[i+16] = mp[i+32+32*(which/2)];  /* MFR2, data set 1 or 2 */
/* Force SFR2/step0 to always use second measurement ... TFA 12/31/98 */
  mfrsweep[16] = mp[32+32];
  for (i=0; i<32; i++)
    mfrsweep[i+48] = mp[i+96+32*which];      /* MFR3, data set 1,2,3 or 4 */
}

void     cal_mfr (int chan, int mfrdata[80], float mfrspec[80])
{
 float        ant_length, bandw[3] = {5.6595,19.406,138.83};
 static       first=1;
 int          i, iband, istep, line_num, max_step;
 float        vout, dn;
 char         *file_name = "/opt/project/cassini/cal/mfr/b1s01.cal";
 FILE         *finput;
 char         input_line [132];
 static double bx_vpernt[80] = {
                                     0.09219,
                                     0.09733,
                                     0.10303,
                                     0.10883,
                                     0.11494,
                                     0.12147,
                                     0.12849,
                                     0.13403,
                                     0.13795,
                                     0.14201,
                                     0.14443,
                                     0.14677,
                                     0.14834,
                                     0.14889,
                                     0.14946,
                                     0.15003,
                                     0.15051,
                                     0.15055,
                                     0.15048,
                                     0.15040,
                                     0.15026,
                                     0.15020,
                                     0.15007,
                                     0.15001,
                                     0.14990,
                                     0.14980,
                                     0.14970,
                                     0.14960,
                                     0.14951,
                                     0.14942,
                                     0.14930,
                                     0.14919,
                                     0.14900,
                                     0.14882,
                                     0.14865,
                                     0.14849,
                                     0.14833,
                                     0.14816,
                                     0.14795,
                                     0.14780,
                                     0.14760,
                                     0.14741,
                                     0.14729,
                                     0.14714,
                                     0.14702,
                                     0.14689,
                                     0.14678,
                                     0.14665,
                                     0.14656,
                                     0.14640,
                                     0.14627,
                                     0.14615,
                                     0.14589,
                                     0.14557,
                                     0.14549,
                                     0.14523,
                                     0.14498,
                                     0.14475,
                                     0.14449,
                                     0.14425,
                                     0.14399,
                                     0.14375,
                                     0.14342,
                                     0.14315,
                                     0.14284,
                                     0.14271,
                                     0.14243,
                                     0.14194,
                                     0.14147,
                                     0.14094,
                                     0.14044,
                                     0.13989,
                                     0.13879,
                                     0.13766,
                                     0.13658,
                                     0.13540,
                                     0.13433,
                                     0.13248,
                                     0.12549,
                                     0.12036 };

 static double bz_vpernt[80] = {
                                     0.09143,
                                     0.09650,
                                     0.10214,
                                     0.10788,
                                     0.11392,
                                     0.12038,
                                     0.12732,
                                     0.13285,
                                     0.13687,
                                     0.14102,
                                     0.14343,
                                     0.14576,
                                     0.14732,
                                     0.14787,
                                     0.14843,
                                     0.14900,
                                     0.14947,
                                     0.14950,
                                     0.14941,
                                     0.14933,
                                     0.14918,
                                     0.14910,
                                     0.14896,
                                     0.14890,
                                     0.14877,
                                     0.14866,
                                     0.14854,
                                     0.14844,
                                     0.14834,
                                     0.14824,
                                     0.14811,
                                     0.14799,
                                     0.14780,
                                     0.14763,
                                     0.14746,
                                     0.14730,
                                     0.14717,
                                     0.14706,
                                     0.14692,
                                     0.14681,
                                     0.14668,
                                     0.14656,
                                     0.14641,
                                     0.14623,
                                     0.14608,
                                     0.14591,
                                     0.14578,
                                     0.14562,
                                     0.14551,
                                     0.14531,
                                     0.14515,
                                     0.14500,
                                     0.14470,
                                     0.14431,
                                     0.14422,
                                     0.14391,
                                     0.14362,
                                     0.14334,
                                     0.14304,
                                     0.14276,
                                     0.14246,
                                     0.14217,
                                     0.14178,
                                     0.14147,
                                     0.14109,
                                     0.14094,
                                     0.14060,
                                     0.13996,
                                     0.13936,
                                     0.13868,
                                     0.13803,
                                     0.13729,
                                     0.13557,
                                     0.13381,
                                     0.13211,
                                     0.13027,
                                     0.12860,
                                     0.12659,
                                     0.12026,
                                     0.11562 };

/*      First time through, load cal tables     */

  if (first)
  {
    first = 0;
    for (iband=1; iband<=3; iband++)
    {
      if (iband==1)
        max_step=16;
      else
        max_step=32;
      for (istep=1; istep<=max_step; istep++)
      {
        sprintf (file_name,
          "/opt/project/cassini/cal/mfr/b%1ds%2.2d.cal", iband,istep);
        if (!(finput = fopen (file_name, "r")))
        {
          fprintf (stderr, "%s:  error opening %s\n", progname, file_name);
          exit (-1);
        }
        line_num=0;
        do
        {
          if (fgets (input_line, 132, finput) != NULL)
          {
            if ((input_line[0] != 0x23) && (input_line[0] != 0x0a))
            {
              sscanf (input_line, "%e %e", &vout, &dn);
              if ( (int)dn == (255-line_num))
                mfrcal[iband-1][istep-1][(int)dn] = vout;
              else
              { fprintf (stderr, "%s: error reading cal file %s\n",
                                progname,  file_name);
                exit (-1);
              }
              line_num++;
            }  
          }
        }
        while (feof (finput) == 0);
        mfrcal[iband-1][istep-1][0] = mfrcal[iband-1][istep-1][1];
        fclose (finput);
      }
    }
  }

/*      Done with first-time-through cal table load     */

  if (chan == 0)
     ant_length = 9.26;            /* Ex dipole */
  else if (chan == 1)
     ant_length = 5.00;            /* Ez monopole */
  else
     ant_length = 1.00;            /* no adjustment for antenna length */
  for (i=0; i<80; i++)
  {
    if (i < 16)
    {
      iband = 0;
      istep = i;
    }
    else if (i < 48)
    {
      iband = 1;
      istep = i-16;
    }
    else
    {
      iband = 2;
      istep = i-48;
    }
    mfrspec[i] = mfrcal[iband][istep][mfrdata[i]];
    mfrspec[i] = mfrspec[i] / ant_length;             /* volts per meter */
    if (chan > 1)
    { mfrspec[i] = 24. * mfrspec[i];
      if (chan == 2)               /* search coil preamplifier gain is 24 */
        mfrspec[i] = mfrspec[i] / bx_vpernt[i];         /* Bx search coil */
      else if (chan == 3)
        mfrspec[i] = mfrspec[i] / bz_vpernt[i];         /* Bz search coil */
    }
    mfrspec[i] = mfrspec[i]*mfrspec[i] / bandw[iband];   /* spectral density */
  }
}
