
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

#include <SpiceUsr.h>

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
  int record_count = 0;
  int SpaceCraft_ID = -82;
  int first = 1;
  int epoch;
  float x[5120], y[5120];
  float xmin, xmax, ymin, ymax, deltat;
  int inst_type, mp_len;
  int wbrdata, nsamp, nlast = 2048;
  int i, idx, igain, agc, iant, min_gain;
  long bytes_output, total_bytes = 0;
  int Zero = 0, One = 1;
  float Fzero = 0.25;
  float Xloc = 0.25;
  float Yloc = 0.25;
  float Disp = .75;
  float Coord = 0.5;
  float Angle = 0.0;
  float Fjust = 0.5;
  char *toplbl = "WBR Dust Data ";
  char *sclk_string[2] = { "1/0000000000:000                     ",
    "1/0000000000:000                     "
  };
  char *time_string = "0000-000T00:00:00.000        ";
  char *gain_string = "   Gain: Unk dB              ";
  char *agc_string = "   (XX)                      ";
  char *antenna_string = "  Antenna: Unk            ";
  double gain, gmin, gain_adjust, value;
  double et;
  double sec;
  static char format[] = { "D" };
  int prec = 3;
  char *antenna[] = { "Ex",
    "Bx",
    "Ez",
    "HF",
    "LP",
    "",
    "",
    ""
  };
  struct RPWS_buffer *m;
  int st_inx;
  int index;

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

    sprintf (sclk_string[0], "%d/%d:%03d",
             wbr_file->current_part,
             wbr_file->current_sclk, wbr_file->current_fine);
    sprintf (sclk_string[1], "SCLK: %d/0x%08X.%d",
             wbr_file->current_part,
             wbr_file->current_sclk, wbr_file->current_RTI);
    scs2e_ (&SpaceCraft_ID, sclk_string[0], &et, strlen (sclk_string[0]));
    et2utc_ (&et, format, &prec, time_string, strlen (format), 32);
    strcpy (&time_string[9], &time_string[12]);
    time_string[8] = 'T';
    time_string[22] = 0;


    strcpy (toplbl, "WBR Dust Data ");
    strcat (toplbl, time_string);
    fprintf (stderr, "%s\n", toplbl);
    if (get_status (m->packet.mpp.mini_packet, WBR_frequency_band, 0) ==
        WBR_BAND_10_KHZ)
      deltat = 36.e-3;                  /* 10 KHz ? */
    else
      deltat = 4.5e-3;                  /* 80 KHz ? */
    if (get_status (m->packet.mpp.mini_packet, WBR_MSF, 0))
      idx = 10;                         /* MSF bit set */
    else
      idx = 8;                          /* MSF bit clear */
    agc = get_status (m->packet.mpp.mini_packet, WBR_AGC_value, 0);
    igain = 10 * get_status (m->packet.mpp.mini_packet, WBR_gain, 0);
    iant = get_status (m->packet.mpp.mini_packet, WBR_antenna, 0);
    if (get_status (m->packet.mpp.mini_packet, WBR_MSF, 0)) {
      if (get_status (m->packet.mpp.mini_packet, WBR_sub_RTI, 0)) {
        char temp[32];

        sprintf (temp, "  Sub-RTI:(0x%02X)",
                 get_status (m->packet.mpp.mini_packet, WBR_sub_RTI, 0));
        strcat (sclk_string[1], temp);
      }
    }
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

#ifdef _SHIT_
      static char devicew[] = "/XSERVE";
#else
      static char devicew[] = { "batch.gif/gif" };
#endif
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
    pgmtxt_ ("T", &Disp, &Coord, &Fjust, sclk_string[1], strlen ("T"),
             strlen (sclk_string[1]));
    /*
     * pgmtxt_ ("T", &Disp, &Fzero, &Fjust, sclk_string[0], strlen("T"), strlen(sclk_string[0])); /*
     */
    record_count++;
    m = MDB_read_stream (wbr_file, WBR_packet_ID_value);
  }
  if (record_count)
    pgend_ ();

  return (record_count);
}

/*  */

/***********************************************************/

/***********************************************************/

/****							****/

/****							****/

/***********************************************************/

/***********************************************************/
main (int argc, char **argv)
{
  struct MDB *wbr_file;
  char *metafile = NULL;

  fprintf (stdout, "   wbrplot_mdb\n");
  fflush (stdout);
  if (argc <= 2) {
    printf ("Usage is: %s start stop\n", argv[0]);
    exit (0);
  }

  /*
   *     MDB routines make use of spice kernel, so 
   *     load SCLK/SCET and Leap Seconds
   */
  
  if( getenv("CAS_TIME_KERNELS") == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		return 13;
  }
  
  metafile = getenv("CAS_TIME_KERNELS");
  
  fprintf (stdout, "furnsh_c %s\n", leapfile);
  furnsh_c(metafile);

  /*
   *       Open a U-File stream, asking fo data between user
   *     specified start & stop times.
   */
  wbr_file = MDB_open (argv[1], argv[2], NULL, NULL, MDB_U_FILE);
  if (!process_data (wbr_file))
    fprintf (stderr, "No data coverage of the requested time interval.\n");
  fprintf (stderr, "%s  %s\n", argv[1], argv[2]);
  MDB_close (wbr_file);
  fprintf (stdout, "\n");
  fflush (stdout);
  return (0);
}
