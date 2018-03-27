
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

#include <SpiceUsr.h>

#include <das.h>

#include "dec.h"
#include "wbr_status.h"
#include "mdb.h"

#define WORD  unsigned short
#define DWORD unsigned long

#define  MAXFILES 1024
#define  MAXCHARS 200
#define  True   1
#define  False  0

/****************************************************************************/
/* Compiled in config file directory locations */

#define _QDEF(x) #x
#define QDEF(x) _QDEF(x)

#ifndef CAS_META_KERNEL
#error Compiled in default spice metakernal file is missing.
#endif
/****************************************************************************/

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


int process_data (struct MDB *wbr_file)
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
  char *time_string = "0000-000T00:00:00.000        ";
  char *gain_string = "   Gain: Unk dB              ";
  char *agc_string = "   (XX)                      ";
  char *antenna_string = "  Antenna: Unk            ";
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
  struct RPWS_buffer *m;
  int st_inx, no_data;
  int index;
  FILE *finput;

  /*
   *       MDB_read_stream fetches the next WBR mini-packet
   *     that is withing the start/stop times...
   *     (nothing more / nothing less)
   *       Returns a NULL pointer when no more data
   *     is available (i.e. EOF)
   */
  m = MDB_read_stream (wbr_file, WBR_packet_ID_value);
  while (m) {

    mp_len = UTIL_MP_length ((struct MP_buffer *) m);
    RTI =
      get_status (m->packet.mpp.mini_packet, WBR_minipacket_RTI,
                  WBR_minipacket_length_MSB);
    pkt_sclk = UTIL_event_time ((struct MP_buffer *) m, 0);
    epoch = (m->packet.cds_tag.epoch[0] << 24) |
      (m->packet.cds_tag.epoch[1] << 16) |
      (m->packet.cds_tag.epoch[2] << 8) | (m->packet.cds_tag.epoch[3] << 0);
    pkt_epoc = pkt_sclk + epoch;

/*                                                              */

/*       Newest SCET calculation scheme: 6-Jan-1999 TFA         */

/*                                                              */
    if (epoch) {                        /* JPL data has non-zero epoch */
      evt_clk.seconds = pkt_sclk;       /* SCLK seconds */
      evt_clk.fine = UTIL_extract_MP_RTI ((struct MP_buffer *) m) << 5; /* SCLK fine */
      evt_tim = UTIL_event_scet ((struct MP_buffer *) m, evt_clk);
      pkt_event = UTIL_event_scet_tm (*evt_tim, 0);
      pkt_event->tm_yday++;             /* days after Jan. 1 */
      pkt_event->tm_mon++;              /* months since Jan */
      year = pkt_event->tm_year + 1900;
      mon = pkt_event->tm_mon;          /* month, 1...12 */
      mday = pkt_event->tm_mday;        /* day of month */
      doy = pkt_event->tm_yday;
      hr = pkt_event->tm_hour;
      mn = pkt_event->tm_min;
      sec = (double) pkt_event->tm_sec +
        (double) (evt_tim->milliseconds % 1000) / 1000.;
    } else {                            /* IOWA data has epoch=0 */

      pkt_event = gmtime (&pkt_epoc);
      pkt_event->tm_yday++;             /* gmtime returns days after Jan. 1 */
      pkt_event->tm_mon++;              /* months since Jan */
      year = pkt_event->tm_year + 1900;
      mon = pkt_event->tm_mon;          /* month, 1...12 */
      mday = pkt_event->tm_mday;        /* day of month */
      doy = pkt_event->tm_yday;
      hr = pkt_event->tm_hour;
      mn = pkt_event->tm_min;
      sec = (double) pkt_event->tm_sec +
        ((double) (UTIL_extract_MP_RTI ((struct MP_buffer *) m) * 125)) /
        1000.;
    }

    pkt_sec = ttime (&year, &mon, &mday, &doy, &hr, &mn, &sec);

    if (1) {

/*
           if ((mp_len == 0x0405) || (mp_len == 0x0407) ||      
               (mp_len == 0x0805) || (mp_len == 0x0807) ||      
               (mp_len == 0x0c05) || (mp_len == 0x0c07) ||      
               (mp_len == 0x1005) || (mp_len == 0x1007) ||      
               (mp_len == 0x1405) || (mp_len == 0x1407) )       
*/
      {
        if (1) {
          sprintf (time_string, "%4.4d-%3.3dT%2.2d:%2.2d:%06.3f",
                   year, doy, hr, mn, sec);
          strcpy (toplbl, "WBR Dust Data ");
          strcat (toplbl, time_string);
          fprintf (stderr, "%s\n", toplbl);
          if (get_status (m->packet.mpp.mini_packet, WBR_frequency_band, 0) ==
              WBR_BAND_10_KHZ)
            deltat = 36.e-3;            /* 10 KHz ? */
          else
            deltat = 4.5e-3;            /* 80 KHz ? */
          if (get_status (m->packet.mpp.mini_packet, WBR_MSF, 0))
            idx = 10;                   /* MSF bit set */
          else
            idx = 8;                    /* MSF bit clear */
          agc = get_status (m->packet.mpp.mini_packet, WBR_AGC_value, 0);
          igain = 10 * get_status (m->packet.mpp.mini_packet, WBR_gain, 0);
          iant = get_status (m->packet.mpp.mini_packet, WBR_antenna, 0);
          gain = igain;
          gain = pow (10., gain / 20.);

          sprintf (gain_string, "   Gain: %2.2d dB", igain);
          strcat (toplbl, gain_string);

          sprintf (agc_string, " (%02X)", agc);
          strcat (toplbl, agc_string);

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
            wbrdata = m->packet.mpp.mini_packet[i];
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
        }                               /* if ((pkt_sec >= start_sec) && (pkt_sec < stop_sec)) */
      }                                 /* if ((mp_len == 0x805) || (mp_len == 0x807)) */
    }                                   /* if (pkt_sec < stop_sec) */
    m = MDB_read_stream (wbr_file, WBR_packet_ID_value);
  }
  if (!no_data)
    pgend_ ();
  fclose (finput);
  return (no_data);
}

main (int argc, char **argv)
{
  struct MDB *wbr_file;
  static char *metafile = QDEF(CAS_META_KERNEL);

/***************************************************************/

  if (argc <= 2) {
    printf ("Usage is: %s start stop\n");
    exit (0);
  }

  /*
   *     MDB routines make use of spice kernel, so 
   *     load SCLK/SCET and Leap Seconds
   */
  fprintf (stdout, "furnsh_c %s\n", leapfile);
  furnsh_c (metafile);

  /*
   *       Open a U-File stream, asking fo data between user
   *     specified start & stop times.
   */
  wbr_file = MDB_open (argv[1], argv[2], NULL, NULL, MDB_U_FILE);
  if (process_data (wbr_file)) {
    fprintf (stderr, "No data coverage of the requested time interval.\n");
    exit (1);
  }
  return (0);
}
