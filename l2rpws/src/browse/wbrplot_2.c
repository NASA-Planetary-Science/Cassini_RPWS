
/*
        wbrplot.c       written 12/07/99 by TFA to process
        		11/12/2003 Browse images WTR
                        Cassini RPWS 10 KHz WBR data, plot raw data
			
        Modifications:
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

#define  MAXFILES 1024
#define  MAXCHARS 200
#define  True   1
#define  False  0

struct file_structure
{
  char line[MAXCHARS];
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
static char *dataset = "JPL";


int process_data (double start_sec, double stop_sec,
                  struct file_structure *dbase, int n_files)
{
  int first = 1;
  int epoch;
  float x[5120], y[5120];
  float xmin, xmax, ymin, ymax, deltat;
  int inst_type, mp_len;
  int wbrdata, nsamp, nlast = 2048;
  int i, idx, RTI, igain, agc, iant, min_gain;
  int year, month, day, doy, hr, mn, mon, mday;
  long bytes_output, total_bytes = 0;
  int Zero = 0, One = 1;
  char *toplbl = "WBR Dust Data ";
  char *time_string = "0000-000T00:00:00.000";
  char *gain_string = "   Gain: Unk dB";
  char *antenna_string = "  Antenna: Unk";
  double gain, gmin, gain_adjust, value;
  double pkt_sec = 0;
  double sec;
  time_t pkt_sclk, pkt_epoc, start_sclk, stop_sclk;
  struct tm *pkt_event;
  char *antenna[] = { "Ex",
    "Bx",
    "Ez",
    "HF",
    "LP",
    "",
    "",
    ""
  };
  struct event_time *evt_tim;
  struct event_clock evt_clk;
  struct bigbuf
  {
    struct RPWS_buffer big1;
    struct RPWS_buffer big2;
  };
  union
  {
    struct RPWS_buffer rbuffer;
    struct MP_buffer buffer;
    struct bigbuf banana;
  } m;
  int st_inx, no_data;
  int index;
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

  if (!(finput = fopen ((dbase + st_inx)->line, "r"))) {
    fprintf (stderr, "%s:  error opening %s\n",
             progname, (dbase + st_inx)->line);
    exit (-1);
  }

  do {
    if ((UTIL_getbuffer_MP (&m.buffer, finput, UTIL_GET_NON_BLOCKING)) ==
        EOF_REACHED) {
      fclose (finput);                  /* done with this file, time for next */
      st_inx++;
      if (st_inx < n_files) {
        if (!(finput = fopen ((dbase + st_inx)->line, "r"))) {
          fprintf (stderr, "%s:  error opening %s\n",
                   progname, (dbase + st_inx)->line);
          exit (-1);
        }
        if ((UTIL_getbuffer_MP (&m.buffer, finput, UTIL_GET_NON_BLOCKING)) ==
            EOF_REACHED) {
          fclose (finput);
          fprintf (stderr, "%s:  error opening %s\n",
                   progname, (dbase + st_inx)->line);
          exit (-1);
        }
      } else
        break;                          /* ran out of files, exit do loop */
    }
    inst_type = (m.buffer.packet.mpx.mini_packet[0] >> 4) & 0xf;
    if (inst_type == WBR) {
      mp_len = UTIL_MP_length (&m.buffer);
      RTI = (m.buffer.packet.mpp.mini_packet[2] & 0xff) |
        ((m.buffer.packet.mpp.mini_packet[3] & 0xff) << 8);
      pkt_sclk = UTIL_event_time (&m.buffer, 0);
      epoch = (m.buffer.packet.cds_tag.epoch[0] << 24) |
        (m.buffer.packet.cds_tag.epoch[1] << 16) |
        (m.buffer.packet.cds_tag.epoch[2] << 8) |
        (m.buffer.packet.cds_tag.epoch[3] << 0);
      pkt_epoc = pkt_sclk + epoch;

/*                                                              */

/*       Newest SCET calculation scheme: 6-Jan-1999 TFA         */

/*                                                              */
      if (epoch) {                      /* JPL data has non-zero epoch */
        evt_clk.seconds = pkt_sclk;     /* SCLK seconds */
        evt_clk.fine = UTIL_extract_MP_RTI (&m.buffer) << 5;    /* SCLK fine */
        evt_tim = UTIL_event_scet (&m.buffer, evt_clk);
        pkt_event = UTIL_event_scet_tm (*evt_tim, 0);
        pkt_event->tm_yday++;           /* days after Jan. 1 */
        pkt_event->tm_mon++;            /* months since Jan */
        year = pkt_event->tm_year + 1900;
        mon = pkt_event->tm_mon;        /* month, 1...12 */
        mday = pkt_event->tm_mday;      /* day of month */
        doy = pkt_event->tm_yday;
        hr = pkt_event->tm_hour;
        mn = pkt_event->tm_min;
        sec = (double) pkt_event->tm_sec +
          (double) (evt_tim->milliseconds % 1000) / 1000.;
      } else {                          /* IOWA data has epoch=0 */

        pkt_event = gmtime (&pkt_epoc);
        pkt_event->tm_yday++;           /* gmtime returns days after Jan. 1 */
        pkt_event->tm_mon++;            /* months since Jan */
        year = pkt_event->tm_year + 1900;
        mon = pkt_event->tm_mon;        /* month, 1...12 */
        mday = pkt_event->tm_mday;      /* day of month */
        doy = pkt_event->tm_yday;
        hr = pkt_event->tm_hour;
        mn = pkt_event->tm_min;
        sec = (double) pkt_event->tm_sec +
          ((double) (UTIL_extract_MP_RTI (&m.buffer) * 125)) / 1000.;
      }

      pkt_sec = ttime (&year, &mon, &mday, &doy, &hr, &mn, &sec);

      if (pkt_sec < stop_sec) {

/*
           if ((mp_len == 0x0405) || (mp_len == 0x0407) ||      
               (mp_len == 0x0805) || (mp_len == 0x0807) ||      
               (mp_len == 0x0c05) || (mp_len == 0x0c07) ||      
               (mp_len == 0x1005) || (mp_len == 0x1007) ||      
               (mp_len == 0x1405) || (mp_len == 0x1407) )       
*/
        {
          if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) {
            sprintf (time_string, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f",
                     year, doy, hr, mn, sec);
            strcpy (toplbl, "WBR Dust Data ");
            strcat (toplbl, time_string);
            fprintf (stderr, "%s\n", toplbl);
            if ((m.buffer.packet.mpp.mini_packet[5] & 0x80) == 0x00)
              deltat = 36.e-3;          /* 10 KHz ? */
            else
              deltat = 4.5e-3;          /* 80 KHz ? */
            if ((m.buffer.packet.mpp.mini_packet[6] & 0x08) == 0x08)
              idx = 10;                 /* MSF bit set */
            else
              idx = 8;                  /* MSF bit clear */
            agc = m.buffer.packet.mpp.mini_packet[7];
            igain = 10 * (m.buffer.packet.mpp.mini_packet[5] & 0x07);
            iant = m.buffer.packet.mpp.mini_packet[6] & 0x07;
            gain = igain;
            gain = pow (10., gain / 20.);

            sprintf (gain_string, "   Gain: %2.2d dB", igain);
            strcat (toplbl, gain_string);

            sprintf (antenna_string, "   Antenna: %s", antenna[iant]);
            strcat (toplbl, antenna_string);

            nsamp = 0;
            nlast = mp_len - idx + 3;

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
            ymin = 255.0;
            ymax = 0.0;
            nsamp = nlast;
            for (i = idx; i < (idx + nlast); i = i++) {
              wbrdata = m.buffer.packet.mpp.mini_packet[i];
              y[(i - idx)] = (float) wbrdata;
              x[(i - idx)] = deltat * (float) (i - idx);
              if (y[(i - idx)] > ymax)
                ymax = y[(i - idx)];
              if (y[(i - idx)] < ymin)
                ymin = y[(i - idx)];
            }

/* Undo Auto-scaling. 23-Jul-03 TFA */

            ymin = 255.0;
            ymax = 0.0;

            if (first) {
              static int Zero = 0;
              static int One = 1;
              static float Fzero = 0.0;
              static float Fone = 1.0;
              static char devicew[] = { "batch.gif/gif" };
              static int width;

              first = 0;
              pgbegin_ (&Zero, devicew, &One, &One, strlen (devicew));
              pgscr_ (&Zero, &Fone, &Fone, &Fone);
              pgscr_ (&One, &Fzero, &Fzero, &Fzero);
              pgqlw_ (&width);
              width *= 5;
              pgslw_ (&width);
            }
            xmin = 0.0;
            xmax = deltat * (float) nsamp;
            pgenv_ (&xmin, &xmax, &ymin, &ymax, &Zero, &One);
            pglabel_ ("t, milliseconds", "Data Number",
                      toplbl, strlen ("t, milliseconds"),
                      strlen ("Data Number"), strlen (toplbl));
            pgline_ (&nsamp, &x[0], &y[0]);

            no_data = False;
          }                             /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
        }                               /* if ((mp_len == 0x805) || (mp_len == 0x807)) */
      }                                 /* if (pkt_sec < stop_sec) */
    }                                   /* if( inst_type == WBR ) */
  }
  while (pkt_sec < stop_sec);
  if (!no_data)
    pgend_ ();
  fclose (finput);
  return (no_data);
}


void make_dbase (double start_sec, double stop_sec,
                 struct file_structure *dbase, int *num_files)
{
  int index, year, month, day, doy, hr, mn;
  int begin_sclk, end_sclk, databits;
  double sec, total_secs, file_start, file_stop;
  char *file_name = "/opt/project/cassini/data/cassini.db";
  FILE *finput;
  char inline[MAXCHARS];
  char start[80], stop[80];

  if (strcmp (dataset, "IOWA") == 0)
    file_name = "/opt/project/cassini/data/CassiniIOWA.db";
  else
    file_name = "/opt/project/cassini/data/CassiniJPL.db";
  if (!(finput = fopen (file_name, "r"))) {
    fprintf (stderr, "%s:  error opening %s\n", progname, file_name);
    exit (-1);
  }

  *num_files = 0;
  do {
    if (fgets (inline, MAXCHARS, finput) != NULL) {
      index = *num_files;
      sscanf (inline, "%s %s %x %x %s %x",
              start, stop,
              &begin_sclk, &end_sclk, (dbase + index)->line, &databits);

/* get file start time and insert into data base */

      if (parsetime (start, &year, &month, &day, &doy, &hr, &mn, &sec)) {
        fprintf (stderr, "%s: error parsing %s\n", progname, start);
        exit (1);
      }
      (dbase + index)->year = year;
      (dbase + index)->day = doy;
      (dbase + index)->hr = hr;
      (dbase + index)->mn = mn;
      (dbase + index)->sec = sec;
      (dbase + index)->ms =
        1000. * ((double) (hr * 3600 + mn * 60) + (double) sec);

      total_secs = ttime (&year, &month, &day, &doy, &hr, &mn, &sec);
      (dbase + index)->st_sec = total_secs;
      file_start = total_secs;

/* get file stop time and insert into data base */

      if (parsetime (stop, &year, &month, &day, &doy, &hr, &mn, &sec)) {
        fprintf (stderr, "%s: error parsing %s\n", progname, stop);
        exit (1);
      }

      total_secs = ttime (&year, &month, &day, &doy, &hr, &mn, &sec);
      (dbase + index)->sp_sec = total_secs;
      file_stop = total_secs;

      if ((file_start < stop_sec) && (file_stop >= start_sec))
        (*num_files)++;
    }
  }
  while (feof (finput) == 0);

  fclose (finput);

}


main (int argc, char **argv)
{
  int num_files;
  int year, month, day, doy, hr, mn;
  double sec, start_sec, stop_sec;
  struct file_structure files[MAXFILES];

/***************************************************************/

  if (argc <= 2) {
    printf ("Usage is: %s start stop [dataset]\n", argv[0]);
    exit (0);
  }
  progname = argv[0];
  if (parsetime (argv[1], &year, &month, &day, &doy, &hr, &mn, &sec)) {
    fprintf (stderr, "%s: error parsing %s\n", argv[0], argv[1]);
    exit (1);
  }
  start_sec = ttime (&year, &month, &day, &doy, &hr, &mn, &sec);

  if (parsetime (argv[2], &year, &month, &day, &doy, &hr, &mn, &sec)) {
    fprintf (stderr, "%s: error parsing %s\n", argv[0], argv[2]);
    exit (1);
  }
  stop_sec = ttime (&year, &month, &day, &doy, &hr, &mn, &sec);

  if (argc == 4)
    dataset = argv[3];
  fprintf (stderr, "Selected data set is %s\n", dataset);


  make_dbase (start_sec, stop_sec, files, &num_files);
  if (num_files == 0) {
    fprintf (stderr, "No data coverage of the requested time interval.\n");
    exit (1);
  }

  if (process_data (start_sec, stop_sec, files, num_files)) {
    fprintf (stderr, "No data coverage of the requested time interval.\n");
    exit (1);
  }
  return (0);
}
