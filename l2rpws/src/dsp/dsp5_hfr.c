/***************************************/

/* dsp5_hfr.c */

/***************************************/

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include "fg.h"
#include "rtiu.h"
#include "util.h"
#include "utilf.h"
#include "utilo.h"
#include "utilt.h"

#include "mp_status.h"
#include "hfr_status.h"
#include "lfdr_status.h"
#include "lp_status.h"
#include "mfr_status.h"
#include "wbr_status.h"
#include "stim_status.h"
#include "wfr_status.h"

#define RTI 8.0

#define STS_HFR_STS "ST_HFR "
#define DS_HFR_SOUNDER_MAX 120.0
#define DS_HFR_MAX 120.0

/****************************************************************************/

static float tABC[4][4][4] = {
  /*
   * 8     16      32    32       filters         
   */
  273, 264, 242, 242,                   /* 1E      *//* 125 mSec */
  364, 365.5, 356, 356,                 /* 2Ea/2Ec */
  399, 395, 378, 378,                   /* 2Eac    */
  896, 892, 864, 864,                   /* DF      */

  546, 530, 504, 504,                   /* 1E      *//* 250 mSec  */
  728, 733, 737, 737,                   /* 2Ea/2Ec */
  798, 792, 782, 782,                   /* 2Eac    */
  1690, 1680, 1660, 1660,               /* DF      */

  1089, 1056, 1008, 1008,               /* 1E      *//* 500 mSec  */
  1456, 1464.5, 1474, 1474,             /* 2Ea/2Ec */
  1596, 1580, 1560, 1560,               /* 2Eac    */
  3280, 3260, 3220, 3220,               /* DF      */

  2180, 2110, 2003, 2003,               /* 1E      *//* 1000 mSec */
  2905, 2925, 2945, 2945,               /* 2Ea/2Ec */
  3192, 3160, 3120, 3120,               /* 2Eac    */
  6420, 6400, 6340, 6340                /* DF      */
    /*
     * 8     16      32    32       filters         
     */
};
static float tH1_1[4][5] = {
  /*
   * 1E   2E      2Ea     2Eac    DF      
   */
  21.2, 24.5, 25.8, 27.4, 56.8,         /*  20 Msec */
  36.4, 42.8, 45.7, 49, 98,             /*  40 mSec */
  66.8, 80, 85.9, 91.8, 181,            /*  80 mSec */
  128.3, 154, 165.5, 178, 347.4         /* 160 mSec */
};
static float tH1_2[4][5] = {
  20.1, 23.1, 24.6, 26.2, 50.4,         /*  20 Msec */
  34.7, 41.9, 41.45, 47.5, 90.2,        /*  40 Msec */
  65.5, 78.2, 84.1, 91.2, 171.7,        /*  80 Msec */
  125.9, 153.6, 163.3, 175.7, 336.7     /* 160 Msec */
};
static float tH2_1[4][5] = {
  /*
   * 1E   2E      2Ea     2Eac    DF      
   */
  12.5, 14.2, 14.75, 15.6, 34.9,        /*  10 Msec */
  20.2, 23.5, 24.9, 26.4, 55.5,         /*  20 Msec */
  35.3, 42, 44.9, 47.6, 96.1,           /*  40 Msec */
  65.7, 79, 84.8, 90.5, 178             /*  80 Msec */
};
static float tH2_2[4][5] = {
  12.3, 13.9, 14.75, 15.2, 29.6,        /*  10 Msec */
  20.3, 23.4, 25, 26.2, 50.3,           /*  20 Msec */
  35.2, 41.4, 44.3, 47.3, 90.4,         /*  40 Msec */
  66, 78, 84.5, 89.5, 172               /*  80 Msec */
    /*
     * 1E   2E      2Ea     2Eac    DF      
     */
};

        /*
         * millisecond mode sample times 
         */
static float tRate[] = { .5, 1, 2, 4,
  8, 16, 32, 32
};

        /*
         * millisecond mode data set size 
         */
static float tSize[] = { 256, 512, 1024, 2048,
  4096, 8192, 16384, 0
};

static int ABC_int[8] = { 125, 250, 500, 1000, 125, 250, 500, 1000 };   /* integration times */
static int ABC_filt[8] = { 8, 16, 32, 64, 8, 16, 32, 64 };      /* n filters */

static int H_filt[8] = { 1, 2, 4, 8, 1, 2, 4, 8 };      /* H1/H2 filters match */

static int H1_int[8] = { 20, 40, 80, 160, 20, 40, 80, 160 };    /*integration times */
static int H2_int[8] = { 10, 20, 40, 80, 10, 20, 40, 80 };      /* integration times */

static char *compression_text[8] = {
  "no comp",
  "meander",
  "unknown",
  "unknown",
  "unknown",
  "unknown",
  "unknown",
  "unknown"
};
static char calibrate_text[256] = { "Cal" };
static char dump_text[258] = { "Memory Dump " };
static char analysis_text[256];

  /**********************************************************************
   *		Extract status fields from mini-packet			*
   *	control1 and control2 are select specifiers where		*
   *		bits 16-23	index into minipacket (byte offset)	*
   *		bits 8-15	contain a shift count			*
   *		bits 0-7	contain a select mask			*
   *	control2 will be zero when the field is 1 to 8 bits long	*
   *		or will specify MSB bits				*
   *									*
   *		Extract the specified bits from the minipacket,		*
   *		shifting and masking to get the bit field justified	*
   *		into the low bits of an integer.  Up to 16 bits of	*
   *		status may be extracted at one time			* 
   *									*
   *		We hope this makes field selection in the rest		*
   *		of the program a little easier to follow through	*
   *		the use of appropriate header files that		*
   *		define the correct patterns for control1/control2	*
   *									*
   *		An additional side-effect, I hope, is that		*
   *		corrections and updates are much easier to handle	*
   *		as the header files (should) occur only in 		*
   *		$(INST_HDR) (NO LOCAL COPIES!)		*
   *									*
   *	  All of the HFR_* patterns used for control1/control2 		*
   *	arguments are taken from the "HFR Header" that came from	*
   *	Pierre...							*
   *									*
   **********************************************************************/
int get_status (unsigned char *mp, int control1, int control2)
{
  int index;
  int shift;
  int mask;
  int result;

  index = (control1 >> 16) & 0xFF;
  shift = (control1 >> 8) & 0xFF;
  mask = (control1 >> 0) & 0xFF;

  result = (mp[index] >> shift) & mask;
  result &= 0xFF;
  if (control2) {
    index = (control2 >> 16) & 0xFF;
    shift = (control2 >> 8) & 0xFF;
    mask = (control2 >> 0) & 0xFF;
    result |= ((mp[index] >> shift) & mask) << 8;
  }
  result &= 0x0000FFFF;
  return result;
}

  /**********************************************************************
   *									*
   *		R T I   Timing						*
   *	Save the time of the previous packet so we can calcualte	*
   *	a delta-T between data-sets.  This works out fine for 		*
   *	regularly spaced data-sets, but tends to be delay-ed 1 RTI	*
   *	for changing timing (know what I'm gett-in ar, Ralph?)		*
   *									*
   **********************************************************************/
static char *opmode_timing (unsigned char *mp)
{
  static int previous_RTI = 0;
  int current_RTI = 0;
  int delta_RTI = 0;
  static char text[256];
  int hfr_type;
  static char *type[5] = { "Anal ",
    "Sound",
    "Cal  ",
    "Msec ",
    "Dump "
  };


  hfr_type = get_status (mp, HFR_packet_type, 0);
  if (get_status (mp, HFR_memory_dump, 0))
    hfr_type = 4;

  current_RTI = get_status (mp, MP_packet_rti_LSB, MP_packet_rti_MSB);

  delta_RTI = current_RTI - previous_RTI;

  if (delta_RTI & 0x80000)
    delta_RTI += 65536;

  sprintf (text, "ST_HFR Mode:%s Delta-RTI:0x%04X(%4d) %6.3f",
           type[hfr_type], delta_RTI, delta_RTI, (float) delta_RTI / 8.0);
  previous_RTI = current_RTI;

  return text;
}

  /**********************************************************************
   *									*
   *		HFR Sounder frequency steps 				*
   *	Easier that doing a calculation, just use the start-F and	*
   *	stop-F from the status as indices into this lookup-table.	*
   *									*
   **********************************************************************/
static float frequency[256] = {
  3.6, 3.8, 4.0, 4.2, 4.4, 4.6,         /* 0 */
  4.8, 5.0, 5.2, 5.4, 5.6, 5.8,
  6.0, 6.2, 6.4, 6.6, 6.8, 7.0,

  7.2, 7.6, 8.0, 8.4, 8.8, 9.2,         /* 18 */
  9.6, 10.0, 10.4, 10.8, 11.2, 11.6,
  12.0, 12.4, 12.8, 13.2, 13.6, 14.0,

  14.4, 15.2, 16.0, 16.8, 17.6, 18.4,   /* 36 */
  19.2, 20.0, 20.8, 21.6, 22.4, 23.2,
  24.0, 24.8, 25.6, 26.4, 27.2, 28.0,

  28.8, 30.4, 32.0, 33.6, 35.2, 36.8,   /* 54 */
  38.4, 40.0, 41.6, 43.2, 44.8, 46.4,
  48.0, 49.6, 51.2, 52.8, 54.4, 56.0,

  57.6, 60.8, 64.0, 67.2, 70.4, 73.6,   /* 72 */
  76.8, 80.0, 83.2, 86.4, 89.6, 92.8,
  96.0, 99.2, 102.4, 105.6, 108.8, 112.0,

  115.2, 0.0, 0.0, 0.0, 0.0, 0.0,       /* 90 */
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0
};

  /**********************************************************************
   *									*
   *		HFR Sounder excitation times for each step		*
   *	As with the frequency table, use start-F/stop-F an start/stop	*
   *	index into this table to determine the duration that the 	*
   *	sounder excites the local environment.				*
   *									*
   **********************************************************************/
static float pulse_duration[256] = { 5.0, 5.0, 5.0, 5.0, 5.0, 5.0,      /* 0 */
  5.0, 5.0, 5.0, 5.0, 5.0, 5.0,
  5.0, 5.0, 5.0, 5.0, 5.0, 5.0,

  2.5, 2.5, 2.5, 2.5, 2.5, 2.5,
  2.5, 2.5, 2.5, 2.5, 2.5, 2.5,
  2.5, 2.5, 2.5, 2.5, 2.5, 2.5,

  1.25, 1.25, 1.25, 1.25, 1.25, 1.25,
  1.25, 1.25, 1.25, 1.25, 1.25, 1.25,
  1.25, 1.25, 1.25, 1.25, 1.25, 1.25,

  0.625, 0.625, 0.625, 0.625, 0.625, 0.625,
  0.625, 0.625, 0.625, 0.625, 0.625, 0.625,
  0.625, 0.625, 0.625, 0.625, 0.625, 0.625,

  .3125, .3125, .3125, .3125, .3125, .3125,
  .3125, .3125, .3125, .3125, .3125, .3125,
  .3125, .3125, .3125, .3125, .3125, .3125,

  .3125, 0.0, 0.0, 0.0, 0.0, 0.0,       /* 90 */
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0
};

  /**********************************************************************
   *									*
   *		HFR Sounder sampling times for each step		*
   *	As with the frequency table, use start-F/stop-F an start/stop	*
   *	index into this table to determine the duration that the 	*
   *	sounder collects the 180 sample data set to use for the		*
   *	auto/AGC calculation						*
   *									*
   **********************************************************************/
static float sample_duration[256] = {
  6.12, 6.12, 6.12, 6.12, 6.12, 6.12,
  6.12, 6.12, 6.12, 6.12, 6.12, 6.12,
  6.12, 6.12, 6.12, 6.12, 6.12, 6.12,

  2.95, 2.95, 2.95, 2.95, 2.95, 2.95,
  2.95, 2.95, 2.95, 2.95, 2.95, 2.95,
  2.95, 2.95, 2.95, 2.95, 2.95, 2.95,

  1.36, 1.36, 1.36, 1.36, 1.36, 1.36,
  1.36, 1.36, 1.36, 1.36, 1.36, 1.36,
  1.36, 1.36, 1.36, 1.36, 1.36, 1.36,

  0.576, 0.576, 0.576, 0.576, 0.576, 0.576,
  0.576, 0.576, 0.576, 0.576, 0.576, 0.576,
  0.576, 0.576, 0.576, 0.576, 0.576, 0.576,

  0.18, 0.18, 0.18, 0.18, 0.18, 0.18,
  0.18, 0.18, 0.18, 0.18, 0.18, 0.18,
  0.18, 0.18, 0.18, 0.18, 0.18, 0.18,

  0.18, 0.0, 0.0, 0.0, 0.0, 0.0,        /* 90 */
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
  0.0, 0.0, 0.0, 0.0
};

  /**********************************************************************
   *									*
   *		HFR Sounder timing calculation				*
   *	This is the "GUTs" of the time-estimate for a sounder sweep	*
   *									*
   *	I have this broken down into many little steps, mostly to	*
   *	make discussions of how sounder works a little easier...	*
   *	Efficiency of the code is NOT an issue, this doesn't run	*
   *	on an 8088 or an old '386...
   *									*
   **********************************************************************/
static int opmode_sounder_guts (unsigned char *mp, int *size, float *sweep)
{
  float t1, t2, t3;
  float setup_time;
  int i;

  for (i = 0; i < 8; i++) {             /* Clear accumulators      *//*                       */
    sweep[i] = 0.0;                     /* time accumulators     */
    size[i] = 0;                        /* data-set size acccum. */
  }
  /*
   *   Calculate expected size of data record
   *        HFR_SND_auto and HFR_SND_AGC extract a bit mask
   *      that indicates the number of bytes associated with 
   *      each frequency step.  Count the number of bits in 
   *      these two mask fields...
   *
   *      size[0] number of frequency steps 
   *      size[1] passive cycles
   *      size[2] active cycles
   *      size[3] total number of data points
   *      size[4] auto_A mask
   *      size[5] AGC_B mask
   *      size[6] bit accumulator
   *Total:size[7] byte count
   *      add 19 for HFR and LRP overhead!!!      
   */
  size[0] = get_status (mp, HFR_SND_stop_frequency, 0) -
    get_status (mp, HFR_SND_start_frequency, 0) + 1;
  size[1] = size[0] * get_status (mp, HFR_SND_number_passive, 0);
  size[2] = size[0] * get_status (mp, HFR_SND_number_active, 0);
  size[3] = (size[1] + size[2]) * get_status (mp, HFR_SND_number_cycles, 0);
  size[4] = (get_status (mp, HFR_SND_auto_A_MSB, 0) << 8) |     /* auto A */
    get_status (mp, HFR_SND_auto_A, 0);
  size[5] = (get_status (mp, HFR_SND_AGC_B_MSB, 0) << 7) |      /* auto B */
    get_status (mp, HFR_SND_AGC_B, 0);

  for (i = 0; i < 9; i++) {             /* Scan the auto and agc *//* mask arrays to count */
    size[6] += size[4] & 0x0001;        /* the number of data   */
    size[6] += size[5] & 0x0001;        /* items in each step   */
    size[4] = size[4] >> 1;             /*                      */
    size[5] = size[5] >> 1;             /*                      */
  }                                     /* this is the number   */
  size[7] = size[6] * size[3];          /* of data bytres       */

  /*
   *   Calculate expected duration of this observation
   */
  t1 = get_status (mp, HFR_SND_T1_delay, 0);    /* extract T1    */
  t2 = get_status (mp, HFR_SND_T2_delay, 0);    /*         T2    */
  t3 = get_status (mp, HFR_SND_T3_delay, 0);    /*         T3    */
  /*
   *      Guess at overhead, this is a WAG (Wild-ass-guess)
   *        of how much CPU time HFR spends during each cycly doing
   *        something other than T1, Pulse, T2, Sample, and T3...
   *
   *      5.0 mSec looks somewhat reasonable for Earth encounter data,
   *        but a little shy when we acquire longer sweeps (seems to
   *        get farther off when we get past 30 sec).
   *      DON'T forget to add LRP overhead, in particular the
   *        1-second sounder warning that we broadcast prior to each and
   *        every sounder sweep (i.e. we can't let sounder disturb the
   *        spacecraft environment without notifying all the other 
   *        instruments).
   */
  setup_time = 5.0;                     /* Sounder setup time    */

  /*
   *      Calculate the length of each sweep
   *
   *      setup_time is an estimate of the code-overhead
   *              required to setup each mode
   *      sample_duration is an estimate of the time required
   *              for the code to perform the sounder analysis,
   *                Does it depend on the number of bytes to be
   *              transferred to LRP?? 
   *
   *      sweep[0] is the time for a passive sweep
   *      sweep[1] is the time for an active sweep, it is calculated
   *              slightly different depending on PAA/AAA setting.
   *      sweep[2] accumulates passive cycles (take repeat-count into account)
   *      sweep[3] accumulates active cycles (PAA/AAA dependant)
   *Total:sweep[4] accumulates over-all repeat count
   */
  for (i = get_status (mp,              /* Accumulate for each  */
                       HFR_SND_start_frequency, 0);     /*  frequency step      */
       i <= get_status (mp,             /* make sure you get    */
                        HFR_SND_stop_frequency, 0);     /*  the last step       */
       i++) {                           /* (scan tables)  */

    /*
     *      Passive sweep calculation:
     *        Patrick's calculation looks like it uses a different
     *              count for T1/T2.  Willy's counts are taken from
     *              6/95 timeframe.
     *
     *Patrick       Passif:  E1+T1+E2+T2+T3         
     *Willy:        Passive: E1+T2+E2+T2+E3+T3
     */
    /*
     * passive sweeps - Patrick 
     */
    /*
     * sweep[0] +=       setup_time +
     * 2.0 * sample_duration[i] +  /* table scan 
     */
    t1 + t2 + t3;
    /**/
      /*
       * passive sweeps - Willy 
       */
      sweep[0] += setup_time + 3.0 * sample_duration[i] +       /* table scan */
      2.0 * t2 + t3;
    /**/
      /*
       *      Active sweep calculation:
       *              We seem to concur
       *
       *      Active  AAA:       Pulse+T1+E1+T2+E2+T2+E3+T3
       *              PAA:    E1+Pulse+T1+E2+T2+E3   +   T3
       *
       *                      any setup time
       *                      3 samples
       *                      
       */
      switch (get_status (mp, HFR_SND_mode, 0)) {
     case 0:                           /* PAA */
       sweep[1] += setup_time +         /* active sweeps */
         3.0 * sample_duration[i] +     /* table scan */
         pulse_duration[i] +            /* table scan */
         t1 + 1.0 * t2 + t3;
       break;
     case 1:                           /* AAA */
       sweep[1] += setup_time +         /* active sweeps */
         3.0 * sample_duration[i] +     /* table scan */
         pulse_duration[i] +            /* table scan */
         t1 + 2.0 * t2 + t3;
       break;
    }
  }
  /*
   *      Accumulate repeat cycles...
   */
  sweep[2] = sweep[0] * (float) get_status (mp, HFR_SND_number_passive, 0);
  sweep[3] = sweep[1] * (float) get_status (mp, HFR_SND_number_active, 0);
  sweep[4] =
    (sweep[2] + sweep[3]) * (float) get_status (mp, HFR_SND_number_cycles, 0);

  return get_status (mp, HFR_SND_number_cycles, 0);
}

  /**********************************************************************
   *									*
   *	HFR Sounder timing:		MILLISECONDS			*
   *		This is a routine available to external users,		*
   *		it is NOT used internally.  Pass it a character array	*
   *		that contains a mini-packet (unsigned char *mp)		*
   *		and it will return:					*
   *			A: overall repeat count (int fuinction return)	*
   *			B: passive sounder timing (how long HFR does	*
   *				passive sweeps prior to exciting the	*
   *				local plasma				*
   *			C: active sounder timing (how long HFR spends	*
   *				exciting the local plasma, disturbing	*
   *				everything (transmitting)		*
   *		B and C repeat "A" times (B C B C B C not B B B C C C)	*
   *									*
   *									*
   *	DO NOT change this interface, it is used by everyone !!!	*								*
   *									*
   **********************************************************************/
float opmode_sounder (unsigned char *mp, float *passive, float *active,
                      int *cycles)
{
  int hfr_type;
  int size[8];
  float sweep[8];
  float total;

  hfr_type = get_status (mp, HFR_packet_type, 0);
  if (get_status (mp, HFR_memory_dump, 0))
    hfr_type = 4;
  switch (hfr_type) {
   default:
     passive[0] = 0.0;
     active[0] = 0.0;
     cycles[0] = 0;
     total = 0.0;
     break;
   case 1:
     opmode_sounder_guts (mp, size, sweep);
     passive[0] = sweep[2];
     active[0] = sweep[3];
     cycles[0] = get_status (mp, HFR_SND_number_cycles, 0);
     total = sweep[4];
     break;
  }
  return total;
}

  /**********************************************************************
   *									*
   *	HFR Sounder timing and size:					*
   *									*
   **********************************************************************/

static char *opmode_sounder_new (unsigned char *mp, int request_index)
{
  float sweep[8];
  float t1, t2, t3;
  float setup_time;
  int size[8];
  int i;
  static char temp[256];
  static char *antenna[2] = { "Ez", "Ex+/-" };
  static char *mode[2] = { "PAA", "AAA" };

  switch (request_index) {
   default:
     break;
   case 0:
     return opmode_timing (mp);
   case 1:
     sprintf (temp, "ST_HFR SND " "%s " /* Antenna */
              "%s "                     /* Mode */
              "cy:%d "                  /* Cycles  */
              "pas:%d "                 /* passive  */
              "act:%d "                 /* active  */
              "freq:" "%.1f-"           /* start f  */
              "%.1f "                   /* stop f  */
              , antenna[get_status (mp, HFR_SND_antenna, 0)], mode[get_status (mp, HFR_SND_mode, 0)], get_status (mp, HFR_SND_number_cycles, 0),        /* cycles */
              get_status (mp, HFR_SND_number_passive, 0),       /* passive */
              get_status (mp, HFR_SND_number_active, 0),        /* active */
              frequency[get_status (mp, HFR_SND_start_frequency, 0)],   /* start */
              frequency[get_status (mp, HFR_SND_stop_frequency, 0) + 1] /* stop */
       );
     return temp;
   case 2:
     sprintf (temp, "ST_HFR SND " "T1:%d "      /* T1  */
              "T2:%d "                  /* T2  */
              "T3:%d "                  /* T3  */
              "auto-A:%X "              /* auto A  */
              "AGC-B:%X "               /* agc B  */
              , get_status (mp, HFR_SND_T1_delay, 0),   /* T1 */
              get_status (mp, HFR_SND_T2_delay, 0),     /* T2 */
              get_status (mp, HFR_SND_T3_delay, 0),     /* T3 */
              (get_status (mp, HFR_SND_auto_A_MSB, 0) << 8) |   /* auto A */
              get_status (mp, HFR_SND_auto_A, 0),       /* auto A */
              (get_status (mp, HFR_SND_AGC_B_MSB, 0) << 7) |    /* agc B */
              get_status (mp, HFR_SND_AGC_B, 0) /* agc B */
       );
     return temp;

     /*
      * HFR-sounder timing calculation     
      */

   case 3:
     opmode_sounder_guts (mp, size, sweep);
     sprintf (temp, "ST_HFR SND "
              "size: %d  "
              "sweep: %.3f(%d) ",
              size[7], sweep[4] / 1000.0, (int) (sweep[4] / 125.0)
       );
     return temp;
  }
  return NULL;
}


  /**********************************************************************
   *									*
   *	HFR millisecond timing and size:				*
   *									*
   **********************************************************************/

static char *opmode_msec (unsigned char *mp, int request_index)
{
  static char temp[256];
  static char *antenna[4] = { "Ez", "Ex+", "Ex-", "Ex+/-" };
  static float sample[8] = { .5, 1., 2., 4., 8., 16., 32., 64. };       /* seems to be off by 1 */
  static int size[8] = { 256, 512, 1024, 2048, 4096, 8192, 16384, -1 };
  static int ms_factor[] = { 25, 100 };
  static int ms_offset[] = { 0, 25 };

  switch (request_index) {
   default:
     break;
   case 0:
     return opmode_timing (mp);
   case 1:
     sprintf (temp, "ST_HFR MS " "H%d_" /* H1 or H2 */
              "1E(%s)  "                /* antenna count */
              "int: %.1f  "             /* integ time (sample rate) */
              "rep: %d  "               /* repeat count */
              "byt: %d(%d)  "           /* byte count or sample size */
              ,
              /*
               * H1/H2  
               */ get_status (mp, HFR_MS_receiver, 0) + 1,
              /*
               * 1E1/2  
               */ antenna[get_status (mp, HFR_MS_antenna, 0)],
              /*
               * int:   
               */ sample[get_status (mp, HFR_MS_sample_rate, 0)],
              /*
               * rep:   
               */ 1,
              /*
               * byt:   
               */ size[get_status (mp, HFR_MS_sample_size, 0)],
              /*
               * byt:() 
               */ get_status (mp, HFR_packet_size_LSB, 0) |
              /*
               */ (get_status (mp, HFR_packet_size_MSB, 0) << 8)
       );
     return temp;
   case 2:
     sprintf (temp, "ST_HFR MS     " "msc: %.0f  "      /* msec (how long) */
              "stp: 1  "                /* steps (N/A) */
              "sze: 0Khz "              /* step size (not stepping) */
              "%s "                     /* receiver */
              "Frq: %dKhz "             /* Frequency */
              ,
              /*
               * msc:         
               */ sample[get_status (mp, HFR_MS_sample_rate, 0)] *
              /*
               */ (float) size[get_status (mp, HFR_MS_sample_size, 0)],
              /*
               * stp:1        
               */
              /*
               * sze:0        
               */
              /*
               * receiver     
               */ get_status (mp, HFR_MS_receiver, 0) ? "H2" : "H1",
              /*
               * Frq:         
               */ get_status (mp, HFR_MS_frequency, 0) *
              /*
               * HF2 50Khz ?  
               */ ms_factor[get_status (mp, HFR_MS_receiver, 0)] +
              /*
               * HF2 50Khz ?  
               */ ms_offset[get_status (mp, HFR_MS_receiver, 0)]
       );
     return temp;
   case 99:
     {
       static int hfr_rti[2] = { 0, 0 };
       float bit_count = 0.0;
       int byte_count;
       int delta_t = 0;
       float delta_time;
       float actual_bitrate = 0.0;

       hfr_rti[1] =
         get_status (mp, HFR_minipacket_RTI, HFR_minipacket_RTI_MSB);

       delta_t = hfr_rti[1] - hfr_rti[0];       /* calculate actual delta */

       if (delta_t < 0)                 /* account for handler and */
         delta_t += 0x00010000;         /* scheduling times */
       delta_time = delta_t;
       delta_time /= 8.0;               /* RTI -> seconds */

       byte_count =
         get_status (mp, HFR_minipacket_Length, HFR_minipacket_Length_MSB);
       bit_count = (byte_count + 3) * 8;
       if (delta_time)
         actual_bitrate = bit_count / delta_time;

       sprintf (temp, "%s"
                "      Actual rates"
                " "
                "%8d bytes"
                "  "
                "%5.0f bps"
                " "
                "%8.3f sec"
                "    ", "ST_HFR MS", byte_count, actual_bitrate, delta_time);
       hfr_rti[0] = hfr_rti[1];         /* update RTI for next time */
       bit_count = 0.0;
       return temp;
     }
  }
  return NULL;
}

  /**********************************************************************
   *									*
   *	HFR status display, french format				*
   *	  returns TEXT STRINGS to DSP5 (might change)			*
   *									*
   **********************************************************************/

char *opmodes_hfr (unsigned char *mp, int request_index)
{

  static float ds_time[8];
  static int nbytes[4] = { 0, 0, 0, 0 };
  static float ds_rate[2], time;
  int hfr_type;
  int Inf, nf;
  int Iintt, intt;
  int Ik;
  int nant;
  int autoc;
  int cross;
  int df;
  int repeat;
  int step;
  int stsize;
  int rate, size;
  int fr[2];
  int ItimeRTI;
  char temp[32];

  if (MP_packet_ID_HFR != (get_status (mp, MP_packet_ID, 0)))
    return NULL;

  hfr_type = get_status (mp, HFR_packet_type, 0);
  if (get_status (mp, HFR_memory_dump, 0))
    hfr_type = 4;

  switch (hfr_type) {
   case 0:                             /* analysis */
     break;
   case 1:                             /* sounder This is a -WILD-ASS-GUESS- */
     return opmode_sounder_new (mp, request_index);

   case 2:                             /* Cal */
     ds_time[6] = 102.0;
     if (request_index)
       return NULL;
     else
       return calibrate_text;

   case 3:                             /* Millisecond */
     return opmode_msec (mp, request_index);

   case 4:                             /* Dump */
     ds_time[4] = 0.25;
     if (request_index)
       return NULL;
     else
       return dump_text;
  }

   /************ ABC *****************************/
  analysis_text[0] = 0;
  switch (request_index) {
   case 0:
     return opmode_timing (mp);
   case 1:
     step = 0;
     nant = 0;

     if (get_status (mp, HFR_ABC_band_A, 0))
       step += 1;
     if (get_status (mp, HFR_ABC_band_B, 0))
       step += 1;
     if (get_status (mp, HFR_ABC_band_C, 0))
       step += 1;
     if (get_status (mp, HFR_ABC_Ex_antenna, 0))        /* Ex in some form */
       nant = 1;

     nant += get_status (mp, HFR_ABC_Ez_antenna, 0);    /* Ez */
     Inf = get_status (mp, HFR_ABC_filters, 0); /* 0 1 2 3 */
     nf = ABC_filt[Inf];                /* 8 16 32 32 */
     autoc = get_status (mp, HFR_ABC_auto_correlation, 0);      /* Auto correlation */
     cross = get_status (mp, HFR_ABC_cross_correlation, 0);     /* cross correlation */
     df = get_status (mp, HFR_ABC_direction_finding, 0);        /* direction finding */
     repeat = get_status (mp, HFR_ABC_repeat_count, 0);
     Iintt = get_status (mp, HFR_ABC_integration_time, 0);      /* 0 1 2 3 */
     intt = ABC_int[Iintt];             /* 125 250 500 1000 */

     nbytes[0] = (1 + df) *
       step *
       nant *
       (1 + nf * (autoc + cross)) + 2 * cross * ((1 + df) * step * nf / 8);
     nbytes[0] *= repeat;
     nbytes[3]++;

     Ik = 0;
     if (nant == 2) {
       if (autoc)
         Ik += 1;
       if (cross)
         Ik += 1;
     }
     if (df)
       Ik = 3;

     if (get_status (mp, HFR_ABC_band_ABC, 0)) {
       ds_time[0] = tABC[Iintt][Ik][Inf] * step / 3.0;
       ds_time[0] *= repeat;            /* ABC repeat count */
     }

     if (nbytes[0] & 0xFFFF8000)
       nbytes[0] |= 0x80000000;
     sprintf (analysis_text, "%s" "%c"  /* A */
              "%c"                      /* B */
              "%c_"                     /* C */
              "%1dE"                    /* antenna count */
              "%c"                      /* auto */
              "%c_"                     /* cross */
              "%2d"                     /* filters */
              "%s "                     /* /DF */
              "%4d "                    /* integ time */
              "%2d "                    /* repeat count */
              "%4d "
              "%6.0f ",
              STS_HFR_STS,
              get_status (mp, HFR_ABC_band_A, 0) ? 'A' : ' ',
              get_status (mp, HFR_ABC_band_B, 0) ? 'B' : ' ',
              get_status (mp, HFR_ABC_band_C, 0) ? 'C' : ' ',
              nant,
              autoc ? 'a' : ' ',
              cross ? 'c' : ' ',
              nf, df ? "/DF" : "   ", intt, repeat, nbytes[0], ds_time[0]);
     break;

   /************ H1 *****************************/
   case 2:
     step = get_status (mp, HFR_H1_band, 0);
     step *= get_status (mp, HFR_H1_step_count, 0);
     step *= get_status (mp, HFR_H1_step_count, 0);
     stsize = get_status (mp, HFR_H1_step_size, 0);
     nant = 0;
     if (get_status (mp, HFR_H1_Ex_antenna, 0))
       nant = 1;
     nant += get_status (mp, HFR_H1_Ez_antenna, 0);

     Inf = get_status (mp, HFR_H1_filters, 0);  /* 0 1 2 3 */
     nf = H_filt[Inf];                  /* 1 2 4 8 */
     autoc = get_status (mp, HFR_H1_auto_correlation, 0);
     cross = get_status (mp, HFR_H1_cross_correlation, 0);
     df = get_status (mp, HFR_H1_direction_finding, 0);
     Iintt = get_status (mp, HFR_H1_integration_time, 0);       /* 0 1 2 3 */
     intt = H1_int[Iintt];              /* 20 40 80 160 */
     repeat = get_status (mp, HFR_H1_repeat_count, 0);

     nbytes[1] = (1 + df) *
       step *
       nant *
       (1 + nf * (autoc + cross)) + 2 * cross * ((1 + df) * step * nf / 8);
     nbytes[1] *= repeat;
     nbytes[3]++;

     Ik = 0;
     if (nant == 2) {
       Ik += 1;
       if (autoc)
         Ik += 1;
       if (cross)
         Ik += 1;
     }
     if (df)
       Ik = 4;


     fr[0] = get_status (mp, HFR_H1_start_frequency, 0) * 25;
     fr[1] = fr[0] + (step - 1) * (stsize * 25);

     if (get_status (mp, HFR_H1_band, 0)) {
       ds_time[1] = tH1_1[Iintt][Ik] + (step - 1) * tH1_2[Iintt][Ik];
       ds_time[1] *= repeat;            /* H1 repeat count */
     }

     sprintf (analysis_text, "%s" "%c" "1_"     /* H1 */
              "%1dE"                    /* antenna count */
              "%c"                      /* auto */
              "%c_"                     /* cross */
              "%2d"                     /* filters */
              "%s  "                    /* /DF */
              "%4d "                    /* integ time */
              "%2d "                    /* repeat count */
              "%4d "
              "%6.0f "
              "%3d "
              "%4dKhz "
              "%4d-%4d ",
              STS_HFR_STS,
              get_status (mp, HFR_H1_band, 0) ? 'H' : ' ',
              nant,
              autoc ? 'a' : ' ',
              cross ? 'c' : ' ',
              nf,
              df ? "/DF" : "   ",
              intt,
              repeat, nbytes[1], ds_time[1], step, stsize * 25, fr[0], fr[1]);
     break;

   /************ H2 *****************************/
   case 3:
     step = get_status (mp, HFR_H2_band, 0);    /* ON/OFF */
     step *= get_status (mp, HFR_H2_step_count, HFR_H2_step_count_MSB);
     ;
     stsize = get_status (mp, HFR_H2_step_size, 0);
     nant = 0;
     if (get_status (mp, HFR_H2_Ex_antenna, 0))
       nant = 1;
     nant += get_status (mp, HFR_H2_Ez_antenna, 0);

     Inf = get_status (mp, HFR_H2_filters, 0);  /* 0 1 2 3 */
     nf = H_filt[Inf];                  /* 1 2 4 8 */
     autoc = get_status (mp, HFR_H2_auto_correlation, 0);
     cross = get_status (mp, HFR_H2_cross_correlation, 0);
     df = get_status (mp, HFR_H2_direction_finding, 0);
     Iintt = get_status (mp, HFR_H2_integration_time, 0);       /* 0 1 2 3 */
     intt = H2_int[Iintt];              /* 10 20 40 80 */
     repeat = get_status (mp, HFR_H2_repeat_count, 0);

     nbytes[2] = (1 + df) *
       step *
       nant *
       (1 + nf * (autoc + cross)) + 2 * cross * ((1 + df) * step * nf / 8);
     nbytes[2] *= repeat;
     nbytes[3]++;

     Ik = 0;
     if (nant == 2) {
       Ik += 1;
       if (autoc)
         Ik += 1;
       if (cross)
         Ik += 1;
     }
     if (df)
       Ik = 4;



     fr[0] =
       get_status (mp, HFR_H2_start_frequency,
                   HFR_H2_start_frequency_MSB) * 50 + 25;
     fr[1] = fr[0] + (step - 1) * (stsize * 50);
     if (get_status (mp, HFR_H2_band, 0)) {
       ds_time[2] = tH2_1[Iintt][Ik] + (step - 1) * tH2_2[Iintt][Ik];
       ds_time[2] *= repeat;            /* H3 repeat count */
     }

     sprintf (analysis_text, "%s" "%c" "2_"     /* H2 */
              "%1dE"                    /* antenna count */
              "%c"                      /* auto */
              "%c_"                     /* cross */
              "%2d"                     /* filters */
              "%s  "                    /* /DF */
              "%4d "                    /* integ time */
              "%2d "                    /* repeat count */
              "%4d "
              "%6.0f "
              "%3d "
              "%4dKhz "
              "%4d-%4d ",
              STS_HFR_STS,
              get_status (mp, HFR_H2_band, 0) ? 'H' : ' ',
              nant,
              autoc ? 'a' : ' ',
              cross ? 'c' : ' ',
              nf,
              df ? "/DF" : "   ",
              intt,
              repeat, nbytes[2], ds_time[2], step, stsize * 50, fr[0], fr[1]);
     break;

   /*********************************************/
   case 4:
     if (nbytes[3] == 3) {
       nbytes[3] = nbytes[0] + nbytes[1] + nbytes[2];
       nbytes[3] *= get_status (mp, HFR_ALL_repeat_count, 0);
       ds_time[3] = ds_time[0] + ds_time[1] + ds_time[2];
       ds_time[3] *= get_status (mp, HFR_ALL_repeat_count, 0);
       ItimeRTI = ((ds_time[3] / 1000.0) * RTI) + 0.99;
       time = (float) ItimeRTI / 8.0;
       ds_rate[0] = (float) (nbytes[3] * 8000) / ds_time[3];
       sprintf (analysis_text, "%s"
                "  "
                "%s"
                "      "
                "rep:%3d"
                " "
                "%4d "
                " "
                "%5.0f"
                " "
                "%5.0f bps"
                " "
                "%8.3f sec    ",
                STS_HFR_STS,
                compression_text[get_status (mp, HFR_compression, 0)],
                get_status (mp, HFR_ALL_repeat_count, 0),
                nbytes[3], ds_time[3], ds_rate[0], time);
       nbytes[3] = 0;
     }
     break;
   case 99:
     {
       static int hfr_rti[2] = { 0, 0 };
       float bit_count = 0.0;
       int byte_count;
       int delta_t = 0;
       float delta_time;
       float actual_bitrate = 0.0;

       hfr_rti[1] =
         get_status (mp, HFR_minipacket_RTI, HFR_minipacket_RTI_MSB);

       delta_t = hfr_rti[1] - hfr_rti[0];       /* calculate actual delta */

       if (delta_t < 0)                 /* account for handler and */
         delta_t += 0x00010000;         /* scheduling times */
       delta_time = delta_t;
       delta_time /= 8.0;               /* RTI -> seconds */

       byte_count =
         get_status (mp, HFR_minipacket_Length, HFR_minipacket_Length_MSB);
       bit_count = (byte_count + 3) * 8;
       if (delta_time)
         actual_bitrate = bit_count / delta_time;

       sprintf (analysis_text, "%s"
                "      Actual rates"
                " "
                "%8d bytes"
                "  "
                "%5.0f bps"
                " "
                "%8.3f sec"
                "    ", STS_HFR_STS, byte_count, actual_bitrate, delta_time);
       hfr_rti[0] = hfr_rti[1];         /* update RTI for next time */
       bit_count = 0.0;
     }
     break;
   default:
     return NULL;
  }

  return analysis_text;
}
