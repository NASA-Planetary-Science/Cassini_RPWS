
/**************************
 * rpws_archive_example.c *
 *     WBR_WFR_LIST.C     *
 **************************/
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <SpiceUsr.h>

/**************************************************/

/*  If you want to use SPICE to display time      */

/*    calculated from SCLK, turn on the following:*/
#define SPICE /**/

/*  This will require, of course, that you have   */

/*    access to the spice library on your system  */

/*    Obviously, if it doesn't like the spice     */

/*    calls, you will need to make this line      */

/*    into a comment an skip use of spice         */

/**************************************************/

/************************************************/

/*  This define enables the display of		*/

/*  raw data.					*/
#define _raw_ /**/

/************************************************/

/************************************************/

/*  This define enables the display of		*/

/*  calibrated data				*/

/* #define _calibrated_			      /**/

/************************************************/

/************************************************/

/*  This define limits the number of records	*/

/*    taht are dumped to 10 			*/

/* #define _limited_			      /**/

/************************************************/

/*****************************************************/

/* #include "$PREFIX/include/archive.h" */

/*****************************************************/

/*
 *	Defines the WBR/WFR header fields
 *	Complier MUST be instructed to pack things, else it
 *	  may place everything on architecture-specific optimal boundaries
 *	  (won't work at all well, if this happens)
 */
#pragma pack(1)
  struct archive_event_time
{
  unsigned short days;
  unsigned long msec_of_day;
};
struct archive_event_clock
{
  unsigned long seconds;
  unsigned char partition;
  unsigned char fine;
};

 /*
  *     The "time_series" union maps both WBR and
  *     WFR samples so we don't have to do the
  *     byte re-arranging
  */
struct ARCHIVE_TIME_SERIES
{
  struct archive_event_clock sclk;      /*  1 */
  struct archive_event_time scet;       /*  7 */
  unsigned short record_bytes;          /* 13 */
  unsigned short samples;               /* 15 */
  unsigned short data_rti;              /* 17 */
  unsigned char validity_flag;          /* 19 */
  unsigned char status_flag;            /* 20 */
  unsigned char frequency_band;         /* 21 */
  unsigned char gain;                   /* 22 */
  unsigned char antenna;                /* 23 */
  unsigned char agc;                    /* 24 */
  unsigned char hfr_xlate;              /* 25 */
  unsigned char sub_rti;                /* 26 */
  unsigned char lp_dac_0;               /* 27 */
  unsigned char lp_dac_1;               /* 28 */
  unsigned char fsw_ver;                /* 29 */
  unsigned char spare[3];               /* 30 */
  union
  {
    unsigned char sample[1];            /* 33 */
    unsigned char wbr_sample[1];        /* 33 */
    unsigned short wfr_sample[1];       /* 33 */
  } time_series;
};

#pragma pack()

/****************************************************************/

/*   1  |    2  |    3  |    4  |    5  |    6  |    7  |    8  */

/****************************************************************\
 *   7  |    6  |    5  |    4  |    3  |    2  |    1  |    0  *
 *	|	|	|	|	|	|	|	*
 *  MSF |   WBR |   WFR | WALSH |   SUB |   HFR |   LP  |   LP  *
 *	|	|	|	|   RTI | XLATE | DAC 0	| DAC 1	*
 *\    / \     / \     / \     / \     / \     / \     / \     /*
 * \  /   \   /   \   /   \   /   \   /   \   /   \   /   \   / *
 *  \/     \ /     \ /     \ /     \ /     \ /     \ /     \ /  *************
 *   \      \       \       \       \       \       \       +--- L/P DAC 0  *
 *    \      \       \       \       \       \       \           valid data *
 *     \      \       \       \       \       \       +--------- L/P DAC 1  *
 *      \      \       \       \       \       \                 valid data *
 *       \      \       \       \       \       +--------------- HFR Xlate  *
 *        \      \       \       \       \                       Freq valid *
 *         \      \       \       \       +--------------------- SUB-RTI    *
 *          \      \       \       \                             valid      *
 *           \      \       \       +--------------------------- Walsh Gain *
 *            \      \       \                                   valid      *
 *             \      \       +--------------------------------- This is WFR*
 *              \      \                                                    *
 *               \      +--------------------------------------- This is WBR*
 *                \                                                         *
 *                 +-------------------------------------------- More Status*
 *                                                               Follows set*
\***************************************************************************/
#define ARCH_VALIDITY_FLAG_MSF			0x00000701
#define ARCH_VALIDITY_FLAG_WBR			0x00000601
#define ARCH_VALIDITY_FLAG_WFR			0x00000501
#define ARCH_VALIDITY_FLAG_STIM			0x00000503
#define ARCH_VALIDITY_FLAG_RAW			0x00000503
#define ARCH_VALIDITY_FLAG_INST_ID		0x00000503
#define ARCH_VALIDITY_FLAG_INST_ID_BIT_WBR	0x00000601
#define ARCH_VALIDITY_FLAG_INST_ID_BIT_WFR	0x00000501
#define ARCH_VALIDITY_FLAG_INST_ID_HSK		3
#define ARCH_VALIDITY_FLAG_INST_ID_WBR		2
#define ARCH_VALIDITY_FLAG_INST_ID_WFR		1
#define ARCH_VALIDITY_FLAG_INST_ID_RAW		0

#define ARCH_VALIDITY_FLAG_WALSH_DGF		0x00000401
#define ARCH_VALIDITY_FLAG_SUB_RTI		0x00000301
#define ARCH_VALIDITY_FLAG_HFR_XLATE		0x00000201
#define ARCH_VALIDITY_FLAG_LP_DAC_0		0x00000101
#define ARCH_VALIDITY_FLAG_LP_DAC_1		0x00000001

/****************************************************************/

/*   1  |    2  |    3  |    4  |    5  |    6  |    7  |    8  */

/****************************************************************\
 *   7  |    6  |    5  |    4  |    3  |    2  |    1  |    0  *
 *	|	|	|	|	|	|	|	*
 *  AGC |  FINE |  WBR  |SUSPECT| HFR/X | HFR/X |   EU  |   EV  *
 *ENABLE|  TQF	|TIMEOUT| DATA	|  H2   |  H1   |CURRENT|CURRENT*
 *\    / \     / \     / \     / \     / \     / \     / \     /*
 * \  /   \   /   \   /   \   /   \   /   \   /   \   /   \   / *
 *  \/     \ /     \ /     \ /     \ /     \ /     \ /     \ /  *************
 *   \      \       \       \       \       \       \       +--- EV antenna *
 *    \      \       \       \       \       \       \            I mode    *
 *     \      \       \       \       \       \       +--------- EU antenna *
 *      \      \       \       \       \       \                  I mode    *
 *       \      \       \       \       \       +--------------- HFR Xlate  *
 *        \      \       \       \       \                        using H1  *
 *         \      \       \       \       +--------------------- HFR Xlate  *
 *          \      \       \       \                              using H2  *
 *           \      \       \       +--------------------------- Data fails *
 *            \      \       \                               integrity test *
 *             \      \       +--------------------------------- BAD WBR    *
 *              \      \                                          (discard) *
 *               \      +------------------------------------ SUB-RTI close *
 *                \                      Clear when SUB-RTI is off by up to *
 *                 \                     about 10mSec due to LFDR synch     *
 *                  +------------------ AGC control is active when set to 1 *
 *                                     AGC control in unknown when set to 0 *
\***************************************************************************/
#define ARCH_STATUS_FLAG_AGC_ENABLE		0x00000701
#define ARCH_STATUS_FLAG_FINE_TIME_QUALITY	0x00000601
#define ARCH_STATUS_FLAG_TIMEOUT		0x00000501
#define ARCH_STATUS_FLAG_SUSPECT		0x00000401
#define ARCH_STATUS_FLAG_HFR_H2			0x00000301
#define ARCH_STATUS_FLAG_HFR_H1			0x00000201
#define ARCH_STATUS_FLAG_EU_CURRENT		0x00000101
#define ARCH_STATUS_FLAG_EV_CURRENT		0x00000001

/****************************************************************/

/*   1  |    2  |    3  |    4  |    5  |    6  |    7  |    8  */

/****************************************************************\
 *   7  |    6  |    5  |    4  |    3  |    2  |    1  |    0  *
 *	 	|		|	|			*
 *	 	| WALSH DIGITAL	|	|  ANALOG GAIN SETTING	*
 *	 	|  GAIN FACTOR	|	|			*
 *               \             /         \                     /*
 *                \           /           \                   / *
 *                 \         /             \                 /  *************
 *                  \       /               +---------------+--- Gain amp   *
 *                   \     /                                     10dB steps *
 *                    \   /                                      WFR 0-30 dB*
 *                     \ /                                       WBR 0-70 dB*
 *                      +--------------- Digital gain applied during Walsh  *
 *                                       transform during compression step  *
\***************************************************************************/
#define ARCH_GAIN_WALSH_DGF                     0x00000403
#define ARCH_GAIN_ANALOG_GAIN                   0x00000007

#define ARCH_GAIN_00_DB				0
#define ARCH_GAIN_10_DB				1
#define ARCH_GAIN_20_DB				2
#define ARCH_GAIN_30_DB				3
#define ARCH_GAIN_40_DB				4
#define ARCH_GAIN_50_DB				5
#define ARCH_GAIN_60_DB				6
#define ARCH_GAIN_70_DB				7

#define ARCH_WALSH_GAIN_0			0
#define ARCH_WALSH_GAIN_1			1
#define ARCH_WALSH_GAIN_2			2
#define ARCH_WALSH_GAIN_3			3

#define ARCH_BAND_25HZ				0
#define ARCH_BAND_2500HZ			1
#define ARCH_BAND_10KHZ				2
#define ARCH_BAND_75KHZ				3
#define ARCH_BAND_UNKNOWN			15

#define ARCH_ANTENNA_EX				0
#define ARCH_ANTENNA_EU				1
#define ARCH_ANTENNA_EV				2
#define ARCH_ANTENNA_EW				3
#define ARCH_ANTENNA_BX				4
#define ARCH_ANTENNA_BY				5
#define ARCH_ANTENNA_BZ				6
#define ARCH_ANTENNA_HF				8
#define ARCH_ANTENNA_LP				11
#define ARCH_ANTENNA_UNKNOWN			15

/************************************************************/

/* end of #include "$PREFIX/include/archive.h" */

/************************************************************/

/************************************************************************
 *	Calibration tables						*
 *		Convert from raw counts to field strength		*
 ************************************************************************/
                             /*
                              * --- WFR  ---   --- WBR  ---             
                              */
float Calibration_Factor[4] = {         /* [j_band] */
  /*
   * 25Hz 2.5Khz   10Khz   75Khz     /*      
   */
  9.63,                                 /*WFR LO */
  9.45,                                 /*WFR HI */
  6.33,                                 /*WBR LO */
  6.43                                  /*WBR HI */
};

                             /*
                              * --- WFR  ---   --- WBR  ---             
                              */
float search_coil_calibration_factor[16][4] = { /* [j_ant][j_band] */
  /*
   * 25Hz  2.5Khz   10Khz   75Khz    /*      
   */
  1.0, 1.0, 1.0, 1.0,                   /* 0 Ex */
  1.0, 1.0, 1.0, 1.0,                   /* 1 Eu */
  1.0, 1.0, 1.0, 1.0,                   /* 2 Ev */
  1.0, 1.0, 1.0, 1.0,                   /* 3 Ew */
  .0461, .1474, .1325, 1.0,             /* 4 Bx */
  .0456, .1467, 1.0, 1.0,               /* 5 By */
  .0458, .1466, 1.0, 1.0,               /* 6 Bz */
  1.0, 1.0, 1.0, 1.0,                   /* 7    */
  1.0, 1.0, 1.0, 1.0,                   /* 8 Hf */
  1.0, 1.0, 1.0, 1.0,                   /* 9    */
  1.0, 1.0, 1.0, 1.0,                   /*10    */
  1.0, 1.0, 1.0, 1.0,                   /*11 Lp */
  1.0, 1.0, 1.0, 1.0,                   /*12    */
  1.0, 1.0, 1.0, 1.0,                   /*13    */
  1.0, 1.0, 1.0, 1.0,                   /*14    */
  1.0, 1.0, 1.0, 1.0                    /*15 UNK */
};
float gain_amp_setting[8][4] = {        /* [j_gain][j_band] */
  /*
   * 25Hz  2.5Khz   10Khz   75Khz    /*      
   */
  0.0, 0.0, 0.0, 0.0,                   /* 00dB */
  10.0, 10.0, 10.0, 10.0,               /* 10dB */
  20.0, 20.0, 20.0, 20.0,               /* 20dB */
  27.0, 30.0, 30.0, 30.0,               /* 30dB */
  1.0, 1.0, 40.0, 40.0,                 /* 40dB */
  1.0, 1.0, 50.0, 50.0,                 /* 50dB */
  1.0, 1.0, 60.0, 60.0,                 /* 60dB */
  1.0, 1.0, 70.0, 70.0                  /* 70dB */
};

float geometric_antenna_length[16] = {
  9.26,                                 /* 0 Ex */
  5.0,                                  /* 1 Eu */
  5.0,                                  /* 2 Ev */
  5.0,                                  /* 3 Ew */
  1.0,                                  /* 4 Bx */
  1.0,                                  /* 5 By */
  1.0,                                  /* 6 Bz */
  1.0,                                  /* 7    */
  1.0,                                  /* 8 Hf */
  1.0,                                  /* 9    */
  1.0,                                  /*10    */
  1.0,                                  /*11 Lp */
  1.0,                                  /*12    */
  1.0,                                  /*13    */
  1.0,                                  /*14    */
  1.0                                   /*15 UNK */
};

char *measurement[16] = {
  "E Field",                            /* 0 Ex */
  "E Field",                            /* 1 Eu */
  "E Field",                            /* 2 Ev */
  "E Field",                            /* 3 Ew */
  "B Field",                            /* 4 Bx */
  "B Field",                            /* 5 By */
  "B Field",                            /* 6 Bz */
  "",                                   /* 7    */
  "",                                   /* 8 Hf */
  "",                                   /* 9    */
  "",                                   /*10    */
  "",                                   /*11 Lp */
  "",                                   /*12    */
  "",                                   /*13    */
  "",                                   /*14    */
  "",                                   /*15 UNK */
};
char *instrument_id[4] = {
  "RAW",
  "WFR",
  "WBR",
  "HSK"
};

/****************************************************************
 *	These determine the number of samples we display	*
 *	in the output file					*
 ****************************************************************/
#define	WIDE   24
#define WIDE8  48
#define WIDE12 32

#undef WIDE
#undef WIDE8
#undef WIDE12

#define	WIDE   24
#define WIDE8  32
#define WIDE12 32

static char *title = { " CASSINI/RPWS "
    "Wideband/Waveform " "Archive Dataset Example"
};
static char *author = { "William-Robison@uiowa.edu" };
static char *Ver = { "1.6a" };
static char *Date = { "08 October 2003" };
int SpaceCraft_ID = -82;

#define RECORD_COUNT 10
int Record_Count = 0;

#define BUFFER_SIZE 65536

 /***************************************************************
  *	Data buffers.  We'll allocate storage at run time	*
  ***************************************************************/

static struct ARCHIVE_TIME_SERIES *archive;
static float *raw_counts;
static float *zero_raw_counts;
static float *calibrated_field_strength;
static float *calibrated_e_field_strength;
static float *calibrated_b_field_strength;

    /************************************************************************
     *	Text representations of some of the status fields		    *
     *	"Frequency_Band" have the same meaning for both WBR and WFR datasets*
     *	"Antenna" has the same meaning for all the archive datasets	    *
     ***********************************************************************/

char *Frequency_Band[] = { " 26 Hz",
  "2.5Khz",
  " 10Khz",
  " 75Khz"
};
float Sample_Time[] = { 100e-3,
  140e-6,
  36e-6,
  4.5e-6
};

char *Antenna[] = { "Ex      ",
  "Eu (Ex+)",
  "Ev (Ex-)",
  "Ew (Ez) ",
  "Bx      ",
  "By      ",
  "Bz      ",
  "        ",
  "HF      ",
  "        ",
  "        ",
  "LP      ",
  "        ",
  "        ",
  "        ",
  "XX      "
};


 /***********************************************
  *	just like it says, dump in hex format	*
  ***********************************************/
void hexdump (char *buf, int start, int stop, int flag)
{
  int i;
  int mask = flag;

  for (i = start; i < stop; i++) {
    fprintf (stdout, " %02X", buf[i] & 0xFF);
    if (mask & 0x80000000)
      fprintf (stdout, " ");
    mask = mask << 1;
  }
}

 /*******************************
  *	Handle endian-ness	*
  *******************************/
unsigned short msb_short (unsigned short sh_in)
{
  int j;
  union
  {
    unsigned long lo[1];
    unsigned short sh[2];
    unsigned char ch[4];
  } i;

  i.sh[0] = sh_in;
  j = (i.ch[0] << 8) & 0xFF00;
  j |= (i.ch[1] << 0) & 0x00FF;
  return j;
}

unsigned long msb_long (unsigned long lo_in)
{
  int j;
  union
  {
    unsigned long lo[1];
    unsigned short sh[2];
    unsigned char ch[4];
  } i;

  i.lo[0] = lo_in;
  j = (i.ch[0] << 24) & 0xFF000000;
  j |= (i.ch[1] << 16) & 0x00FF0000;
  j |= (i.ch[2] << 8) & 0x0000FF00;
  j |= (i.ch[3] << 0) & 0x000000FF;
  return j;
}

 /***********************************************
  *	An examaple of converting SCLK to UT	*
  *	using the spice kernel			*
  *	THIS PROCEDURE MAY BE ELIMINATED	*
  *	IF YOU DO NOT HAVE ACCESS TO SPICE	*
  ***********************************************/

#ifdef SPICE
void emit_time (int seconds, int fine, int partition)
{
  static int first = 1;
  char sclk_temp[64];
  char *temp;
  int part = 1;
  int year;
  int month;
  int mday;
  int yday;
  int hour;
  int min;
  double dsec;
  double et;
  char format[] = { "D" };
  int prec = 3;

	/****************************************
		 *	Load the leap-seconds and	*
		 *	spacecraft clock files into	*
		 *	SPICE				*
		 ****************************************/
  
  char* metafile = getenv("CAS_TIME_KERNELS");
  
  if(metafile == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		exit(13);
  }
    
  if (first) {
    first = 0;
    furnsh_c(metafile);
  }

                /****************************************
		 *	in the archive file, we may	*
		 *	store the partition as either	*
		 *	zero or one (both indicate	*
		 *	partition one).  Make it	*
		 *	appropriate for SPICE		*
		 ****************************************/
  if (partition)
    part = partition;
  fprintf (stdout, "SPICE Time ");
  sprintf (sclk_temp, "%d/%d:%03d", part, seconds, fine);

                /****************************************
		 *	SCLK to ET			*
		 ****************************************/
  scs2e_c(SpaceCraft_ID, sclk_temp, &et);

                /****************************************
		 *	 ET to UTC			*
		 ****************************************/
  et2utc_c(et, format, prec, 32, sclk_temp);

                /****************************************
		 *	Convert UTC (text) to binary	*
		 ****************************************/
  year = strtol (sclk_temp, &temp, 10); /* int    */
  yday = strtol (temp + 1, &temp, 10);  /* int    */
  mday = yday;                          /* int    */
  month = 1;                            /* int    */
  hour = strtol (temp + 3, &temp, 10);  /* int    */
  min = strtol (temp + 1, &temp, 10);   /* int    */
  dsec = strtod (temp + 1, &temp);      /* double */

                /****************************************
		 *	Normalize time fields:		*
		 *	For example:			*
		 *		1999-366T10:23:100.9	*
		 *	is normalized to		*
		 *		2000-001T10:24:40.9	*
		 *--------------------------------------*
		 *	SPICE won't cause this, so	*
		 *	it's not necessary		*
		 ****************************************/

  /*
   * tnorm(&year, &month, &mday, &yday, &hour, &min, &dsec); /*
   */

  fprintf (stdout, "%04d-%03dT%02d:%02d:%06.3f ",
           year, yday, hour, min, dsec);
  fprintf (stdout, "\n");
  fflush (stdout);
}
#endif

  /**********************************************************************
   *            Extract status fields					*
   *    control1 and control2 are select specifiers where               *
   *            bits 16-23      index into packet (byte offset)         *
   *            bits 8-15       contain a shift count                   *
   *            bits 0-7        contain a select mask                   *
   *    control2 will be zero when the field is 1 to 8 bits long        *
   *            or will specify MSB bits                                *
   *                                                                    *
   *            Extract the specified bits from the packet,             *
   *            shifting and masking to get the bit field justified     *
   *            into the low bits of an integer.  Up to 16 bits of      *
   *            status may be extracted at one time                     *
   *                                                                    *
   *            We hope this makes field selection in the rest          *
   *            of the program a little easier to follow through        *
   *            the use of appropriate header files that                *
   *            define the correct patterns for control1/control2       *
   *                                                                    *
   **********************************************************************/
int get_status (unsigned char *mp, int control1, int control2)
{
  int index;
  int shift;
  int mask;
  int result;

  index = (control1 >> 16) & 0x7FFF;
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
   *	All we're up to here is reading the Archive file and dumping 	*
   *	status.  This provides a quick&dirty verificatiuon that the 	*
   *	archive has been successfully written.				*
   *									*
   *	NOTE that the various fields are built in such a way that we	*
   *	do not need to do anything unique for either WFR or WBR,	*
   *	simply decode all status.  Inappropriate fields are coded	*
   *	so they can be easily ignored when appropriate			*
   **********************************************************************/
int main (int argc, char *argv[])
{
  FILE *input = stdin;
  int inst_id;
  int status;
  int i;
  int j_band, j_ant, j_gain;
  int seconds;
  int hour, min, sec, msec;
  int rec_byt;
  int samp;
  int fsw;
  static int count = 1;
  int ptmp;
  char *format[2] = { "D", "J" };
  char utcout[64];
  float gain_setting, cal_factor;
  double db_full_scale;
  double linear_scale;
  float dc, DC_value;
  float ymax, ymin, maximum_amplitude_sine_wave;
  int raw_sample;
  float time_sample;

  if (1) {
    fprintf (stdout, "\n");
    fprintf (stdout, "%s %s %s\n", title, Ver, Date);
    fprintf (stdout, "  This example code dumps archive data\n");
    fprintf (stdout, "      \n");
  }

    /************************************************************
     *	Check the calibartion tables (we have to have them	*
     *      arranged in the right order, or you'll have		*
     *      rather strange calibration results 			*
     ************************************************************/
  if (0) {
    fprintf (stdout, "search_coil_calibration_factor\n");
    for (i = 0; i < 16; i++) {
      fprintf (stdout, "  %6.3f  %6.3f  %6.3f  %6.3f  \n",
               search_coil_calibration_factor[i][0],
               search_coil_calibration_factor[i][1],
               search_coil_calibration_factor[i][2],
               search_coil_calibration_factor[i][3]);
    }
    fprintf (stdout, "gain_amp_setting\n");
    for (i = 0; i < 8; i++) {
      fprintf (stdout, "  %6.3f  %6.3f  %6.3f  %6.3f  \n",
               gain_amp_setting[i][0],
               gain_amp_setting[i][1],
               gain_amp_setting[i][2], gain_amp_setting[i][3]);
    }
  }

    /************************************************************
     ************************************************************/



    /************************************************************
     *	  Although we don't need to keep all these steps	*
     *  separate, it makes checking intermediate results a	*
     *	little easier.  Memory is rarely a consideration	*
     *	these days, so away we go...				*
     ************************************************************/
  archive = malloc (BUFFER_SIZE);
  raw_counts = malloc (BUFFER_SIZE);
  zero_raw_counts = malloc (BUFFER_SIZE);
  calibrated_field_strength = malloc (BUFFER_SIZE);
  calibrated_e_field_strength = malloc (BUFFER_SIZE);
  calibrated_b_field_strength = malloc (BUFFER_SIZE);

    /************************************************************
     *	This read method will work for all WBR and WFR files.	*
     *  The octet count is contained in the header and will 	*
     *	be used to read the remaining portion of the record	*
     *	(i.e. the time-series data)				*
     ************************************************************/
  status = fread (archive, 32, 1, input);

    /**********************************************************
     *	Sanity checking, set these values for the 1st. record *
     *    so we can perform sanity checks later               *
     **********************************************************/
  rec_byt = msb_short (archive->record_bytes);
  samp = msb_short (archive->samples);
  fsw = archive->fsw_ver;

  while (status) {
    if (msb_long (archive->sclk.seconds)) {

                /************************************************
		*	THIS PROCEDURE CALL MAY BE ELIMINATED	*
		*	IF YOU DO NOT HAVE ACCESS TO SPICE	*
		*************************************************/

#ifdef SPICE
      emit_time (msb_long (archive->sclk.seconds),
                 archive->sclk.fine, archive->sclk.partition);
#endif

      /*
       *      Only 1 of these bits is set, so this should work OK
       */
      inst_id = get_status (&archive->validity_flag,
                            ARCH_VALIDITY_FLAG_INST_ID, 0);
      fprintf (stdout, "%s ", instrument_id[inst_id]);
      /*
       *      WBR timeout (See .FMT file for details)
       */
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
        fprintf (stdout, "%s", get_status (&archive->status_flag,
                                           ARCH_STATUS_FLAG_TIMEOUT,
                                           0) ? "TMO " : " ");
      fprintf (stdout, "cnt:%4d  ", count++);
      /*
       *      SCLK is 40 bits.  Se .FMT files and user guide for
       *        discussion of the lower 5 bits of sclk.fine.
       *      NOTE that partition may show up as ZERO, we must take
       *        care to fix it for SPICE.
       */
      fprintf (stdout, "%d/", archive->sclk.partition); /* SPICE partition  */
      fprintf (stdout, "%08X", msb_long (archive->sclk.seconds));       /* 32 bit seconds   */
      fprintf (stdout, ".%d", (archive->sclk.fine >> 5) & 0x03);        /* 3 bits RTI       */
      fprintf (stdout, ".%d", (archive->sclk.fine >> 1) & 0x0F);        /* 4 bits sequence  */
      fprintf (stdout, "%s", ((archive->sclk.fine >> 0) & 0x01) ? "*" : " ");
      /*
       * 1 bit time update
       */
      /*
       *      Status flag that indicates fine time (sub-RTI) is available.
       */
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK) {
        fprintf (stdout, "%s", get_status (&archive->status_flag,
                                           ARCH_STATUS_FLAG_FINE_TIME_QUALITY,
                                           0) ? "Q" : " ");
        fprintf (stdout, " ");
      }
      /*
       *      SCET (from CHDO records, we use same format)
       */
      seconds = msb_long (archive->scet.msec_of_day) / 1000;
      hour = seconds / 3600;
      min = seconds / 60 - hour * 60;
      sec = seconds % 60;
      msec = msb_long (archive->scet.msec_of_day) % 1000;
      fprintf (stdout, "%5d-", msb_short (archive->scet.days));
      fprintf (stdout, "%02d:%02d:%02d.%03d ", hour, min, sec, msec);
      /*
       *      16 bit RTI (8/RTI 13/seconds)
       */
      fprintf (stdout, "rti:%04X ", msb_short (archive->data_rti));
      /*
       *      See FMT for description, this is the number
       *      of octets in the data record (the first 32
       *      of which have already been read).
       *
       *      Sanity Check:  archive->record_bytes should be 
       *        the same throughout the file (this is so the
       *        fixed-length nature described in the PDS
       *        label is true.  Keep track of this value 
       *        and display the "rec byt" string in 
       *        uppercase is there is an error...
       */
      i = msb_short (archive->record_bytes);
      fprintf (stdout, "%s:%d ", (rec_byt == i) ? "rec byt" : "REC BYT", i);
      rec_byt = i;
      /*
       *      Number of data samples (8 bits for WBR, 16 bit for WFR)
       *
       *      Sanity Check:  We expect most records to be the 
       *        same length.  Instrument operating modes and 
       *        lost CDS records can alter this, but we generally
       *        should have all records the same sample count.
       */
      i = msb_short (archive->samples);
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
        fprintf (stdout, "%s:%d  ", (samp == i) ? "samples" : "SAMPLES", i);
      else
        fprintf (stdout, "%s:%d  ", (samp == i) ? "hsk byt" : "HSK BYT", i);

      samp = i;
      /*
       *      Effectively, tells us the sampling rate that the A/D runs at
       *        and therefore, the filter selected.
       */
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK) {
        fprintf (stdout, "%s ",
                 Frequency_Band[archive->frequency_band & 0x03]);
      }
      /*
       *      Gain amplifier selection
       */
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK) {
        fprintf (stdout, "%d0dB ", get_status (&archive->gain,
                                               ARCH_GAIN_ANALOG_GAIN, 0));
      }
      /*
       *      AGC enabled?
       */
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK) {
        fprintf (stdout, "%s", get_status (&archive->status_flag,
                                           ARCH_STATUS_FLAG_AGC_ENABLE,
                                           0) ? "AGC " : " ");
      }
      /*
       *      Antenna selection
       */
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK) {
        fprintf (stdout, "%s", Antenna[archive->antenna & 0x0F]);
      }
      /*
       *      HFR/H2 downconvert enabled ?
       *      (H1/H2 are mutually exclusive)
       */
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK) {
        fprintf (stdout, "%s", get_status (&archive->status_flag,
                                           ARCH_STATUS_FLAG_HFR_H2,
                                           0) ? "/H2 " : " ");
      }
      /*
       *      HFR/H1 downconvert enabled ?
       *      (H1/H2 are mutually exclusive)
       */
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK) {
        fprintf (stdout, "%s", get_status (&archive->status_flag,
                                           ARCH_STATUS_FLAG_HFR_H1,
                                           0) ? "/H1 " : " ");
      }
      /*
       *      SUB-RTI: WBR can free-run and this field has additional
       *              timing information.  This shows how to determinbe
       *              the millisecond offset from the start of RTI when
       *              this field is marked valid.
       */

      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK) {
        if (get_status (&archive->validity_flag,
                        ARCH_VALIDITY_FLAG_SUB_RTI, 0)) {
          fprintf (stdout, "%3dmS ", archive->sub_rti);
          if (!get_status (&archive->validity_flag,
                           ARCH_STATUS_FLAG_FINE_TIME_QUALITY, 0))
            fprintf (stdout, "+10/-0 mS ");
        }
      }

      /*
       *      HFR frequency translation:
       *              When HFR delivers a down-converted signal to
       *              WBR, this fragment illustrates how to determine
       *              the mixing frequency.  Least signifigant bit
       *              of the frequency holds H1/H2 flag.
       */
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK) {
        if (get_status (&archive->validity_flag,
                        ARCH_VALIDITY_FLAG_HFR_XLATE, 0)) {
          if (get_status (&archive->status_flag, ARCH_STATUS_FLAG_HFR_H1, 0))
            fprintf (stdout, "%6.3fMhz ", (float) archive->hfr_xlate * 0.025);
          if (get_status (&archive->status_flag, ARCH_STATUS_FLAG_HFR_H2, 0))
            fprintf (stdout, "%6.3fMhz ",
                     4.025 + (float) archive->hfr_xlate * 0.050);
        }
      }
      /*
       *      L/P bias, when connected to WBR or WFR
       */
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK) {
        if (get_status (&archive->validity_flag,
                        ARCH_VALIDITY_FLAG_LP_DAC_0, 0))
          fprintf (stdout, "DAC0:%03d ", archive->lp_dac_0);
      }
      /*
       *      L/P bias, when connected to WFR
       */
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK) {
        if (get_status (&archive->validity_flag,
                        ARCH_VALIDITY_FLAG_LP_DAC_1, 0))
          fprintf (stdout, "%DAC1:%03d ", archive->lp_dac_1);
      }
      /*
       *      MSF bit (More Status Follows).  All we want is to see if it
       *        is set.  It is used to set some of the previous status bits
       *        in the archive program.  We're looking at it here as a
       *        sanity check.
       */
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK) {
        if (get_status (&archive->validity_flag, ARCH_VALIDITY_FLAG_MSF, 0))
          fprintf (stdout, "MSF ");
      }

      /*
       *      Software version loaded into the RPWS instrument.
       *
       *      More Sanity Checking:
       *        Software version shouldn't be changing (well,
       *        at least no too often, and at some very fixed
       *        points).  If it's flitting around, we need to
       *        investigate a bit...
       */
      i = archive->fsw_ver;
      if (i)
        fprintf (stdout, "%s V%d.%d ",
                 (fsw == i) ? "fsw" : "FSW", i / 100, i % 100);
      fsw = i;
      /*
       *
       */
      if (inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK) {
        if (get_status (&archive->validity_flag, ARCH_STATUS_FLAG_SUSPECT, 0))
          fprintf (stdout, "Suspect Data");
      }
      fprintf (stdout, "\n");

        /*********************************************************************
	 *********************************************************************
	 **	Now read the remaining portion of the record in		    **
	 **	NOTE the record length contained within the header is used  **
	 **	  this makes theis read method universal for WBR/WFR archive**
	 **	  data (do not need to know length in advance nor is there  **
	 **	  a need to consult the label file).			    **
	 **	AND, if the data file is screwed up, we can still read it   **
	 **	 without problem using this method.			    **
	 *********************************************************************
	 *********************************************************************/
      memset (archive->time_series.sample, 0xFF, 1024);
      status = fread (archive->time_series.sample,
                      msb_short (archive->record_bytes) - 32, 1, input);
      if (!status)                      /* ERROR condition */
        return 0;                       /* not much grace here, eh? */

      fprintf (stdout, "hdr");
      hexdump ((char *) archive, 0, 32, 0x15154008);
      fprintf (stdout, ": ");

#ifdef _raw_
      if (WIDE8 > 10) {
        fprintf (stdout, "\n");
        fprintf (stdout, "raw  ");
      }

        /************************************************
	 *	DUMP raw values				*
	 *	Accomodate differing sample sizes	*
	 ************************************************/
      if (1) {                          /* this is a wide style dump */
        for (i = 0; i < msb_short (archive->samples); i++) {
          switch (get_status (&archive->validity_flag,
                              ARCH_VALIDITY_FLAG_INST_ID, 0)) {
           case ARCH_VALIDITY_FLAG_INST_ID_HSK:
           case ARCH_VALIDITY_FLAG_INST_ID_RAW:
           case ARCH_VALIDITY_FLAG_INST_ID_WBR:
             fprintf (stdout, " %02X", archive->time_series.wbr_sample[i]);
             ptmp = WIDE8;
             break;
           case ARCH_VALIDITY_FLAG_INST_ID_WFR:
             fprintf (stdout, " %03X",
                      msb_short (archive->time_series.wfr_sample[i])
               );
             ptmp = WIDE12;
             break;
          }
          if ((i % 8 == 7) && (i < msb_short (archive->samples) - 1))
            fprintf (stdout, " ");
          if ((i % ptmp == ptmp - 1) &&
              (i < msb_short (archive->samples) - 1)) {
            fprintf (stdout, "\n");
            fprintf (stdout, "%4d:", i + 1);
          }
        }
        fprintf (stdout, "\n");
      }
#endif

#ifdef _calibrated_

        /************************************************
	 *	Apply calibration			*
	 *	Simple full-band cal (band center)	*
	 *						*
	 *	  See the WFRWBR.TXT file in the	*
	 *	EXTRAS directory for a discussion of	*
	 *	the method used to calibrate the data.	*
	 *	  This code uses the steps described	*
	 *	without any shortcuts/optimizations.	*
	 ************************************************/

                /********************************
		 *	Step 5 will do table	*
		 *	lookups based on the	*
		 *	following status	*
		 ********************************/
      j_band = archive->frequency_band; /* band & receiver */
      j_gain = get_status (&archive->gain, ARCH_GAIN_ANALOG_GAIN, 0);
      j_ant = archive->antenna;

                /********************************
		 *	Step 5a Average Value	*
		 ********************************/
      dc = 0.0;
      ymax = 0.0;
      ymin = 0.0;
      switch (get_status (&archive->validity_flag,
                          ARCH_VALIDITY_FLAG_INST_ID, 0)) {
       case ARCH_VALIDITY_FLAG_INST_ID_WBR:
         ymax = 255.0;
         ymin = 0.0;
         for (i = 0; i < msb_short (archive->samples); i++) {
           raw_counts[i] = (float) archive->time_series.wbr_sample[i];
           dc += raw_counts[i];
         }
         break;
       case ARCH_VALIDITY_FLAG_INST_ID_WFR:
         ymax = 4095.0;
         ymin = 0.0;
         for (i = 0; i < msb_short (archive->samples); i++) {
           raw_counts[i] =
             (float) msb_short (archive->time_series.wfr_sample[i]
             );
           dc += raw_counts[i];
         }
         break;
      }
      DC_value = dc / (float) msb_short (archive->samples);
      maximum_amplitude_sine_wave = (ymax - ymin) / 2.0;

      fprintf (stdout, "DC_value %.3f  "
               "maximum_amplitude_sine_wave %.3f\n",
               DC_value, maximum_amplitude_sine_wave);

                /********************************
		 *	Step 5b Normalize	*
		 *	   about zero		*
		 ********************************/
      for (i = 0; i < msb_short (archive->samples); i++) {
        zero_raw_counts[i] = raw_counts[i] - DC_value;
      }

                /********************************
		 *	Step 5c Apply the	*
		 *	Calibration Factor	*
		 ********************************/
      cal_factor = Calibration_Factor[j_band];

      fprintf (stdout, "cal_factor %.3f\n", cal_factor);

                /********************************
		 *	Step 5d Gain Amp	*
		 *	    setting 		*
		 ********************************/
      gain_setting = gain_amp_setting[j_gain][j_band];

      fprintf (stdout, "gain_setting %.3f\n", gain_setting);

                /********************************
		 *	Step 5e Convert dB	*
		 *	    to linear		*
		 ********************************/
      db_full_scale = cal_factor + gain_setting;
      linear_scale = pow (10., db_full_scale / 20.);
      linear_scale = linear_scale / sqrt (2.);

      fprintf (stdout, "db_full_scale %.3f linear_scale %.3f\n",
               db_full_scale, linear_scale);

                /********************************
		 *	Step 5f Nommalize	*
		 ********************************/
      for (i = 0; i < msb_short (archive->samples); i++) {
        calibrated_field_strength[i] =
          zero_raw_counts[i] / maximum_amplitude_sine_wave;
        calibrated_field_strength[i] =
          calibrated_field_strength[i] / linear_scale;
      }

                /********************************
		 *	Step 5g convert to	*
		 *    electric field strength	*
		 ********************************/
      for (i = 0; i < msb_short (archive->samples); i++) {
        calibrated_e_field_strength[i] =
          calibrated_field_strength[i] / geometric_antenna_length[j_ant];
      }

                /********************************
		 *	Step 5h convert to	*
		 *    magnetic field strength	*
		 ********************************/
      for (i = 0; i < msb_short (archive->samples); i++) {
        calibrated_b_field_strength[i] = calibrated_field_strength[i] * 24.0;
        calibrated_b_field_strength[i] =
          calibrated_b_field_strength[i] /
          search_coil_calibration_factor[j_ant][j_band];
      }

        /************************************************
	 *	DUMP calibrated values			*
	 ************************************************/
      fprintf (stdout, "calibrated samples\n");
      fprintf (stdout, "   sample # ");
      fprintf (stdout, "   mSec ");
      fprintf (stdout, " raw counts");
      fprintf (stdout, "     %s", measurement[j_ant]);
      fprintf (stdout, "\n");
      time_sample = 0.0;
      for (i = 0; i < msb_short (archive->samples); i++) {
        raw_sample = -1;
        switch (get_status (&archive->validity_flag,
                            ARCH_VALIDITY_FLAG_INST_ID, 0)) {
         case ARCH_VALIDITY_FLAG_INST_ID_WBR:
           raw_sample = archive->time_series.wbr_sample[i] & 0xFF;
           break;
         case ARCH_VALIDITY_FLAG_INST_ID_WFR:
           raw_sample = msb_short (archive->time_series.wfr_sample[i]
             ) & 0xFFF;
           break;
        }
        fprintf (stdout, "   %5d ", i);
        fprintf (stdout, "   %7.3f ", time_sample * 1000.);
        time_sample += Sample_Time[archive->frequency_band & 0x03];
        fprintf (stdout, " %4d ", raw_sample);
        fprintf (stdout, " %4.2X ", raw_sample);
        if (measurement[j_ant][0] == 'E')
          fprintf (stdout, "  %11.3e", calibrated_e_field_strength[i]);
        if (measurement[j_ant][0] == 'B')
          fprintf (stdout, "  %11.3e", calibrated_b_field_strength[i]);
        fprintf (stdout, "\n");
      }
#endif
    } else
      /*
       *      We end up here is sclk.seconds is zero...
       *              (This must be a bad thing,
       *               as the computer seems to print "BAD"
       *               if this occurs :-).
       */
    {
      fprintf (stdout, "BAD");
      hexdump ((char *) archive, 0, 32, 0);
      fprintf (stdout, ": ");
      hexdump ((char *) archive, 32, 48, 0);
    }
    fprintf (stdout, "\n");
    /*
     *      Just a few records...
     */

#ifdef _limited_
    if (Record_Count++ > RECORD_COUNT)
      return 0;
#endif

    status = fread (archive, 32, 1, input);     /* next HEADER */
  }
  return 0;
}
