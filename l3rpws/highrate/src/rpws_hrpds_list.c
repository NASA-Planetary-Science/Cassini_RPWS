/* This program has hand different names: */
/* rpws_archive_example.c, WBR_WFR_LIST.C, rpws_hrpds_list.c */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>

#include <SpiceUsr.h>

/************************************************************************/
/*                                                                      */
/* Compiling                                                            */
/* Sun (sparc)                                                          */
/*  cc Ae -g -I/path/to/cspice/include rpws_hrpds_list.c \              */
/*     /path/to/cspice.a -o rpws_hrpds_list                             */
/*                                                                      */
/* Linux (x86_64)                                                       */
/*  gcc -Wall -g -I/path/to/cspice/include rpws_hrpds_list.c \          */
/*      /path/to/cspice.a -o rpws_hrpds_list                            */
/*                                                                      */
/************************************************************************/

/* Turns on spice time conversions for the output */
#define SPICE


/* Use typedefs the keep the data width sizes straight */
typedef unsigned short int uint2;
typedef unsigned int uint4;
typedef unsigned char byte;

/**********************************************
 **                                          **
 **   #pragma pack(1) tells the compiler     **
 ** to pack the structure into memory with   **
 ** NO PADDING.  Since we have settled on    **
 ** ARCHIVE_TIME_V1, the structure           **
 ** ARCHIVE_TIME_SERIES must be exactly 32   **
 ** bytes in length or things won't work    
 **
 ** TODO: Example code needs to be fixed to not be so compiler dependent.
 **       -cwp 2012-03-29
 **                                          **
 ** Later, #pragma pack() tells the          **
 ** compiler that it is free to resume       **
 ** performing memory allocation             **
 ** optmizations                             **
 **********************************************/
#pragma pack(1)
struct archive_event_time {
	uint2 days;
	uint4 msec_of_day;
};

struct archive_event_clock {
	uint4 seconds;
	byte partition;
	byte fine;
};

struct SPICE_TIME {
	int year;
	int doy;
	int d100;
	int days;
	int msec_of_day;
	int hours;
	int minutes;
	int seconds;
	int milliseconds;
};

/********************************************************************
 *****  ARCHIVE_TIME_V1         selected 3/2003                 *****
 *****          Expresses time in the format supplied in the    *****
 *****          CHDO headers attached to raw data.  It is       *****
 *****          days/milliseconds(of day) from 1/1/1958         *****
 *****                                                          *****
 ********************************************************************/
#define ARCHIVE_TIME_SERIES_EXPECTED_SIZE 224
struct ARCHIVE_TIME_SERIES {
	struct archive_event_clock sclk;	/* 1 */
	struct archive_event_time scet;	/* 7 */
	uint2 record_bytes; /* 13 */
	uint2 samples;		  /* 15 */
	uint2 data_rti;	  /* 17 */
	byte validity_flag; /* 19 */
	byte status_flag;	  /* 20 */
	byte frequency_band;	/* 21 */
	byte gain;			  /* 22 */
	byte antenna;		  /* 23 */
	byte agc;			  /* 24 */
	byte hfr_xlate;	  /* 25 */
	byte sub_rti;		  /* 26 */
	byte lp_dac_0;		  /* 27 */
	byte lp_dac_1;		  /* 28 */
	byte fsw_ver;		  /* 29 */
	byte spare[3];		  /* 30 */
	union {
		byte byte_sample[192];	/* 33 */
		uint2 word_sample[96];	/* 33 */
		byte hsk_sample[192];	/* 33 */
		byte wbr_sample[192];	/* 33 */
		uint2 wfr_sample[96];	/* 33 */
	} time_series;
};

#pragma pack()

  /* WFR data frame offsets, shifts and mask values */
  /* INDEX values are reative to the minipacket */

  /* 
   *     bits 16-23      Index into minipacket
   *     bits 8-15       Shift count
   *     bits 0-7        Mask
   */

/*****************************************************************/
/*   1  |    2  |    3  |    4  |    5  |    6  |    7  |    8   */
/*****************************************************************\
 *   7  |    6  |    5  |    4  |    3  |    2  |    1  |    0  |*
 *      |       |       |       |       |       |       |       |*
 *  MSF |   WBR |   WFR | WALSH |   SUB |   HFR |   LP  |   LP  |*
 *      |       |       |       |   RTI | XLATE | DAC 0 | DAC 1 |*
 *\    / \     / \     / \     / \     / \     / \     / \     / *
 * \  /   \   /   \   /   \   /   \   /   \   /   \   /   \   /  *
 *  \/     \ /     \ /     \ /     \ /     \ /     \ /     \ /   *************
 *   \      \       \       \       \       \       \       +--- L/P DAC ch 0*
 *    \      \       \       \       \       \       \    contains valid data*
 *     \      \       \       \       \       \       +--------- L/P DAC ch 1*
 *      \      \       \       \       \       \          contains valid data*
 *       \      \       \       \       \       +--------------- HFR Xlate   *
 *        \      \       \       \       \                FRQ cont valid data*
 *         \      \       \       \       +--------------------- SUB-RTI fld *
 *          \      \       \       \                      contains valid data*
 *           \      \       \       +--------------------------- Walsh Gain  *
 *            \      \       \                            contains valid data*
 *             \      \       +--------------------------------- This is WFR *
 *              \      \                                                     *
 *               \      +--------------------------------------- This is WBR *
 *                \                                                          *
 *                 +-------------------------------------------- More Status *
 *                                                        Follows is set     *
\****************************************************************************/
#define ARCH_VALIDITY_FLAG_MSF                  0x00000701
#define ARCH_VALIDITY_FLAG_WBR                  0x00000601
#define ARCH_VALIDITY_FLAG_WFR                  0x00000501
#define ARCH_VALIDITY_FLAG_STIM                 0x00000503
#define ARCH_VALIDITY_FLAG_RAW                  0x00000503
#define ARCH_VALIDITY_FLAG_INST_ID              0x00000503
#define ARCH_VALIDITY_FLAG_INST_ID_BIT_WBR      0x00000601
#define ARCH_VALIDITY_FLAG_INST_ID_BIT_WFR      0x00000501
#define ARCH_VALIDITY_FLAG_INST_ID_WBR          2
#define ARCH_VALIDITY_FLAG_INST_ID_WFR          1
#define ARCH_VALIDITY_FLAG_INST_ID_HSK          3
#define ARCH_VALIDITY_FLAG_INST_ID_RAW          0

#define ARCH_VALIDITY_FLAG_WALSH_DGF            0x00000401
#define ARCH_VALIDITY_FLAG_SUB_RTI              0x00000301
#define ARCH_VALIDITY_FLAG_HFR_XLATE            0x00000201
#define ARCH_VALIDITY_FLAG_LP_DAC_0             0x00000101
#define ARCH_VALIDITY_FLAG_LP_DAC_1             0x00000001

/*****************************************************************/
/*   1  |    2  |    3  |    4  |    5  |    6  |    7  |    8   */
/*****************************************************************\
 *   7  |    6  |    5  |    4  |    3  |    2  |    1  |    0  |*
 *      |       |       |       |       |       |       |       |*
 *  AGC |  FINE |  WBR  |SUSPECT| HFR/X | HFR/X |   EU  |   EV  |*
 *ENABLE|  TQF  |TIMEOUT| DATA  |  H2   |  H1   |CURRENT|CURRENT|*
 *\    / \     / \     / \     / \     / \     / \     / \     / *
 * \  /   \   /   \   /   \   /   \   /   \   /   \   /   \   /  *
 *  \/     \ /     \ /     \ /     \ /     \ /     \ /     \ /   *************
 *   \      \       \       \       \       \       \       +--- EV antenna  *
 *    \      \       \       \       \       \       \           current mode*
 *     \      \       \       \       \       \       +--------- EU antenna  *
 *      \      \       \       \       \       \                 current mode*
 *       \      \       \       \       \       +--------------- HFR X-late  *
 *        \      \       \       \       \                       using H1    *
 *         \      \       \       \       +--------------------- HFR X-late  *
 *          \      \       \       \                             using H2    *
 *           \      \       \       +--------------------------- Suspect Data*
 *            \      \       \                                   Data failed *
 *             \      \       \                                  integrity   *
 *              \      \       \                                 checks.     *
 *               \      \       +------------------------------- BAD WBR data*
 *                \      \                                        (discard)  *
 *                 \      +------------------------------------- Set when    *
 *                  \                      SUB-RTI  is close, Clear when     *
 *                   \                     SUB-RTI is  off by up to about    *
 *                    \                    10mSec due to LFDR synch          *
 *                     +--------------- AGC control is active when set to 1  *
 *                                      AGC control in unknown when set to 0 *
\****************************************************************************/
#define ARCH_STATUS_FLAG_AGC_ENABLE             0x00000701
#define ARCH_STATUS_FLAG_FINE_TIME_QUALITY      0x00000601
#define ARCH_STATUS_FLAG_TIMEOUT                0x00000501
#define ARCH_STATUS_FLAG_SUSPECT                0x00000401
#define ARCH_STATUS_FLAG_HFR_H2                 0x00000301
#define ARCH_STATUS_FLAG_HFR_H1                 0x00000201
#define ARCH_STATUS_FLAG_EU_CURRENT             0x00000101
#define ARCH_STATUS_FLAG_EV_CURRENT             0x00000001

/*****************************************************************/
/*   1  |    2  |    3  |    4  |    5  |    6  |    7  |    8   */
/*****************************************************************\
 *   7  |    6  |    5  |    4  |    3  |    2  |    1  |    0  |*
 *              |               |       |                       |*
 *              | WALSH DIGITAL |       |  ANALOG GAIN SETTING  |*
 *              |  GAIN FACTOR  |       |                       |*
 *               \             /         \                     / *
 *                \           /           \                   /  *
 *                 \         /             \                 /   *************
 *                  \       /               +---------------+--- Gain amp    *
 *                   \     /                           setting in 10dB steps *
 *                    \   /                            WBR range is 0-70 dB  *
 *                     \ /                             WFR range is 0-30 dB  *
 *                      +---------------   Digital gain applied during Walsh *
 *                                         transform during compression step *
\****************************************************************************/
#define ARCH_GAIN_WALSH_DGF                     0x00000403
#define ARCH_GAIN_ANALOG_GAIN                   0x00000007

#define ARCH_GAIN_00_DB                         0
#define ARCH_GAIN_10_DB                         1
#define ARCH_GAIN_20_DB                         2
#define ARCH_GAIN_30_DB                         3
#define ARCH_GAIN_40_DB                         4
#define ARCH_GAIN_50_DB                         5
#define ARCH_GAIN_60_DB                         6
#define ARCH_GAIN_70_DB                         7

#define ARCH_WALSH_GAIN_0                       0
#define ARCH_WALSH_GAIN_1                       1
#define ARCH_WALSH_GAIN_2                       2
#define ARCH_WALSH_GAIN_3                       3

#define ARCH_BAND_25HZ                          0
#define ARCH_BAND_2500HZ                        1
#define ARCH_BAND_10KHZ                         2
#define ARCH_BAND_75KHZ                         3
#define ARCH_BAND_UNKNOWN                       15

#define ARCH_ANTENNA_EX                         0
#define ARCH_ANTENNA_EU                         1
#define ARCH_ANTENNA_EV                         2
#define ARCH_ANTENNA_EW                         3
#define ARCH_ANTENNA_BX                         4
#define ARCH_ANTENNA_BY                         5
#define ARCH_ANTENNA_BZ                         6
#define ARCH_ANTENNA_HF                         8
#define ARCH_ANTENNA_LP                         11
#define ARCH_ANTENNA_UNKNOWN                    15
/********************************************/
/* "/opt/project/cassini/include/archive.h" */
/* included line count 375                  */
/********************************************/

/***********************************
 ****   Argument parsing        ****
 ***********************************/
extern char *optarg;
extern int optind;

/************************************************************************
 *      Calibration tables                                              *
 *              Convert from raw counts to field strength               *
 ************************************************************************/
  /* --- WFR --- --- WBR --- */
float Calibration_Factor[4] = { /* [j_band] */
	/* 25Hz 2.5Khz 10Khz 75Khz */
	9.63,								  /* WFR LO */
	9.45,								  /* WFR HI */
	6.33,								  /* WBR LO */
	6.43								  /* WBR HI */
};

  /* --- WFR --- --- WBR --- */
float search_coil_calibration_factor[16][4] = {	/* [j_ant][j_band] */
	/* 25Hz 2.5Khz 10Khz 75Khz */
	{1.0, 1.0, 1.0, 1.0},		  /* 0 Ex */
	{1.0, 1.0, 1.0, 1.0},		  /* 1 Eu */
	{1.0, 1.0, 1.0, 1.0},		  /* 2 Ev */
	{1.0, 1.0, 1.0, 1.0},		  /* 3 Ew */
	{.0461, .1474, .1325, 1.0},  /* 4 Bx */
	{.0456, .1467, 1.0, 1.0},	  /* 5 By */
	{.0458, .1466, 1.0, 1.0},	  /* 6 Bz */
	{1.0, 1.0, 1.0, 1.0},		  /* 7 */
	{1.0, 1.0, 1.0, 1.0},		  /* 8 Hf */
	{1.0, 1.0, 1.0, 1.0},		  /* 9 */
	{1.0, 1.0, 1.0, 1.0},		  /* 10 */
	{1.0, 1.0, 1.0, 1.0},		  /* 11 Lp */
	{1.0, 1.0, 1.0, 1.0},		  /* 12 */
	{1.0, 1.0, 1.0, 1.0},		  /* 13 */
	{1.0, 1.0, 1.0, 1.0},		  /* 14 */
	{1.0, 1.0, 1.0, 1.0}			  /* 15 UNK */
};
float gain_amp_setting[8][4] = {	/* [j_gain][j_band] */
	/* 25Hz 2.5Khz 10Khz 75Khz */
	{0.0, 0.0, 0.0, 0.0},		  /* 00dB */
	{10.0, 10.0, 10.0, 10.0},	  /* 10dB */
	{20.0, 20.0, 20.0, 20.0},	  /* 20dB */
	{27.0, 30.0, 30.0, 30.0},	  /* 30dB */
	{1.0, 1.0, 40.0, 40.0},		  /* 40dB */
	{1.0, 1.0, 50.0, 50.0},		  /* 50dB */
	{1.0, 1.0, 60.0, 60.0},		  /* 60dB */
	{1.0, 1.0, 70.0, 70.0}		  /* 70dB */
};
float geometric_antenna_length[16] = { 9.26,	/* 0 Ex */
	5.0,								  /* 1 Eu */
	5.0,								  /* 2 Ev */
	5.0,								  /* 3 Ew */
	1.0,								  /* 4 Bx */
	1.0,								  /* 5 By */
	1.0,								  /* 6 Bz */
	1.0,								  /* 7 */
	1.0,								  /* 8 Hf */
	1.0,								  /* 9 */
	1.0,								  /* 10 */
	1.0,								  /* 11 Lp */
	1.0,								  /* 12 */
	1.0,								  /* 13 */
	1.0,								  /* 14 */
	1.0								  /* 15 UNK */
};
char *measurement[16] = { "E Field",	/* 0 Ex */
	"E Field",						  /* 1 Eu */
	"E Field",						  /* 2 Ev */
	"E Field",						  /* 3 Ew */
	"B Field",						  /* 4 Bx */
	"B Field",						  /* 5 By */
	"B Field",						  /* 6 Bz */
	"",								  /* 7 */
	"",								  /* 8 Hf */
	"",								  /* 9 */
	"",								  /* 10 */
	"",								  /* 11 Lp */
	"",								  /* 12 */
	"",								  /* 13 */
	"",								  /* 14 */
	"",								  /* 15 UNK */
};

/******************************************************************
 ***    Housekeeping data is archived using thje same format    ***
 ***    header as the WBR/WFR data so we can make use of the    ***
 ***    same methods to read it...                              ***
 ******************************************************************/
char *instrument_id[4] = { "RAW", "WFR", "WBR", "HSK"
};

/****************************************************************
 *      These determine the number of samples we display        *
 *      in the output file                                      *
 ****************************************************************/
#define WIDE   24
#define WIDE8  48
#define WIDE12 32

#undef WIDE
#undef WIDE8
#undef WIDE12

#define WIDE   24
#define WIDE8  32
#define WIDE12 32
static char *Title[] =
	 { "CASSINI/RPWS", "Wideband/Waveform", "Archive Dataset Example", NULL
};
static char *Types[] =
	 { "Tyyyyddd_hh_nnKHZn_WBRFR.DAT", "Tyyyyddd_nnHZn_WFRFR.DAT",
	"Tyyyyddd_hh_nnKHZn_WBRFR.LBL", "Tyyyyddd_nnHZn_WFRFR.LBL", NULL
};
static char *Author = { "William-Robison@UIowa.edu" };
static char *Ver = { "V1.14" };
static char *Date = { "2004-03-09" };
int SpaceCraft_ID = -82;
static int verbose_flag = 0;

#define RECORD_COUNT 10
int Record_Count = 0;

#define BUFFER_SIZE 65536

 /***************************************************************
  *     Data buffers.  We'll allocate storage at run time       *
  ***************************************************************/
static struct ARCHIVE_TIME_SERIES *archive;
static float *raw_counts;
static float *zero_raw_counts;
static float *calibrated_field_strength;
static float *calibrated_e_field_strength;
static float *calibrated_b_field_strength;

 /***************************************************************
  *     Information from the LABEL RECORD                       *
  *     IF the user gives us the name of a label record,        *
  *       we can easily grab these 2 fields and verify          *
  *       that the file matches the label                       *
  ***************************************************************/
static int Record_Bytes = 0;
static int File_Records = 0;
static int force_record_length = 0;

	 /************************************************************************
     *  Text representations of some of the status fields                   *
     *  "Frequency_Band" have the same meaning for both WBR and WFR datasets*
     *  "Antenna" has the same meaning for all the archive datasets         *
     ***********************************************************************/
char *Frequency_Band[] = { " 26 Hz", "2.5kHz", " 10kHz", " 75kHz"
};
float Sample_Time[] = { 10e-3, 140e-6, 36e-6, 4.5e-6
};
char *Antenna[] = { 
	"Ex      ", 
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

/*******************************
  *     Handle endian-ness      *
  * This is what should make    *
  * this work on PC. MAC or SUN *
  *******************************/
uint2 msb_short(uint2 sh_in)
{
	int j;
	union {
		uint4 lo[1];
		uint2 sh[2];
		byte ch[4];
	} i;
	i.sh[0] = sh_in;
	j = (i.ch[0] << 8) & 0xFF00;
	j |= (i.ch[1] << 0) & 0x00FF;
	return j;
}
uint4 msb_long(uint4 lo_in)
{
	int j;
	union {
		uint4 lo[1];
		uint2 sh[2];
		byte ch[4];
	} i;
	i.lo[0] = lo_in;
	j = (i.ch[0] << 24) & 0xFF000000;
	j |= (i.ch[1] << 16) & 0x00FF0000;
	j |= (i.ch[2] << 8) & 0x0000FF00;
	j |= (i.ch[3] << 0) & 0x000000FF;
	return j;
}

 /***********************************************
  *     just like it says, dump in hex format   *
  ***********************************************/
void hexdump(char *buf, int start, int stop, int flag)
{
	int i;
	int mask = flag;
	for(i = start; i < stop; i++)
	{
		fprintf(stdout, " %02X", buf[i] & 0xFF);
		if(mask & 0x80000000)
			fprintf(stdout, " ");
		mask = mask << 1;
	}
}

  /***********************************************
  *     An examaple of converting SCLK to UT    *
  *     using the spice kernel                  *
  *     THIS PROCEDURE MAY BE ELIMINATED        *
  *     IF YOU DO NOT HAVE ACCESS TO SPICE      *
  ***********************************************/
void emit_time(int seconds, int fine, int partition, int newline_flag)
{

#ifdef SPICE
	static int nCalls = 0;
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

	/* Load the leap-seconds and spacecraft clock files into SPICE.  Use
	 * 'env CAS_TIME_KERNELS="/path/to/your/localtion" rpws_hrpds_list' on 
	 * the commandline.
	 */
	
	if(nCalls == 0){ 
		if(! getenv("CAS_TIME_KERNELS")){
			fprintf(stderr, "Environment var. CAS_TIME_KERNELS not defined\n");
			exit(13);
		}
		furnsh_c(getenv("CAS_TIME_KERNELS"));
	}

	nCalls += 1;
	
	/****************************************
	 * 	  in the archive file, we may 	 *
	 * 	  store the partition as either   *
	 * 	  zero or one (both indicate  	 *
	 * 	  partition one).  Make it 		 *
	 * 	  appropriate for SPICE 			 *
	 ****************************************/
	if(partition)
		part = partition;
	sprintf(sclk_temp, "%d/%d:%03d", part, seconds, fine);
	fprintf(stdout, "SPICE Time ");
	
	/* SCLK to ET */
	scs2e_c(SpaceCraft_ID, sclk_temp, &et);

	/* ET to UTC */
	et2utc_c(et, format, prec, 32, sclk_temp);

	
	/* Convert UTC (text) to binary 	*/
	year = strtol(sclk_temp, &temp, 10);	/* int */
	yday = strtol(temp + 1, &temp, 10);	/* int */
	mday = yday;					  /* int */
	month = 1;						  /* int */
	hour = strtol(temp + 3, &temp, 10);	/* int */
	min = strtol(temp + 1, &temp, 10);	/* int */
	dsec = strtod(temp + 1, &temp);	/* double */

	/****************************************
	 * 	  Normalize time fields:			 *
	 * 	  For example: 						 *
	 * 				 1999-366T10:23:100.9	 *
	 * 	  is normalized to					 *
	 * 				 2000-001T10:24:40.9 	 *
	 *--------------------------------------*
	 * 	  SPICE won't cause this, so  	 *
	 * 	  it's not necessary 				 *
	 ****************************************/

	/* tnorm(&year, &month, &mday, &yday, &hour, &min, &dsec); */
	fprintf(stdout, "%04d-%03dT%02d:%02d:%06.3f ", year, yday, hour, min,
			  dsec);
	if(newline_flag)
		fprintf(stdout, "\n");
	fflush(stdout);

#endif								  /* */
	return;
}

  /**********************************************************************
   *            Extract status fields                                   *
   *                                                                    *
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
int get_status(byte *mp, int control1, int control2)
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
	if(control2)
	{
		index = (control2 >> 16) & 0xFF;
		shift = (control2 >> 8) & 0xFF;
		mask = (control2 >> 0) & 0xFF;
		result |= ((mp[index] >> shift) & mask) << 8;
	}
	result &= 0x0000FFFF;
	return result;
}

  /**********************************************************************
   *    File name shenanigans,                                          *
   *    Three possibilities:                                            *
   *            1. List file containing names of files to read          *
   *                    (lowercase acceptable)                          *
   *            2. Label file containing the name of a single           *
   *                    data file (Uppercase filenames)                 *
   *            3. Data file of (xxx.PKT or xxx.DAT)                    *
   *                                                                    *
   *      If we get a list file, open it and pass the filenames         *
   *    back, one by one, until we reach the end                        *
   *                                                                    *
   *      If they give us a LABEL file, find the                        *
   *    filename and use it...  We might even do some label file        *
   *    verification here (see if file size matches what the label      *
   *    says it should be, etc.).                                       *
   *                                                                    *
   *    This label file checking ASSUMES that the label has been        *
   *    produced by the RPWS archive software, which implies that       *
   *    the labels are in a specific order.                             *
   *                                                                    *
   *      And finally, if all we get is a DAT or PKT file, simply       *
   *    process it.                                                     *
   *                                                                    *
   *    NOTE:                                                           *
   *      This section of code is not important to the example          *
   *    it is here only to make using this example a little more        *
   *    convenient for us here at Iowa.                                 *
   **********************************************************************/

  /* 
   *      Extract the decimal value from a detail line
   */
static int label_value(char *buf)
{
	char *name;
	int result = 0;				  /* in case it fails... */
	name = strchr(buf, '=');	  /* PARAMETER = VALUE */
	if(name)
	{									  /* ^ *//* extract value */
		result = strtol(name + 1, NULL, 10);
	}
	return result;
}

  /* 
   *      Extact a quotes string from a detail line
   *      (We must keep a local copy of the filename)
   */
static char *label_filename(char *filename)
{
	static char buf[1024];
	char *name;						  /* begining quote */
	char *end;						  /* ending quote */
	strncpy(buf, filename, 1023);
	name = strchr(buf, '"');
	name++;							  /* skip past quote */
	end = strchr(name, '"');
	end[0] = 0;						  /* remove quote */
	if(strlen(name))
		return name;
	return NULL;
}

  /* 
   *      Accept a list file
   */
char *list_open(char *filename, int flag)
{
	struct stat buf;
	static FILE *list_file;
	static char buffer[1024];
	char *filetype;
	if(filename)
	{									  /* open request */
		filetype = strrchr(filename, '.');
		if(!strcasecmp(filetype, ".LST"))
		{
			list_file = fopen(filename, "r");
			if(!list_file)
				return filename;
			if(fgets(buffer, 1023, list_file))
			{
				buffer[strlen(buffer) - 1] = 0;	/* strip new-line */
				return buffer;
			}

			else
			{
				fclose(list_file);
				return filename;
			}
		}

		else
			return filename;
	}

	else
	{									  /* next file request */

		if(list_file)
		{								  /* we are processing a list file ? */
			while(fgets(buffer, 1023, list_file))
			{
				buffer[strlen(buffer) - 1] = 0;	/* strip new-line */
				if((stat(buffer, &buf)))
				{
					if(flag)
						fprintf(stdout, " Null File %d=%s\n", (int) buf.st_size,
								  buffer);
				}

				else if(buf.st_size)
				{
					if(flag)
						fprintf(stdout, " Open File %d=%s\n", (int) buf.st_size,
								  buffer);
					return buffer;
				}

				else
				{
					if(flag)
						fprintf(stdout, "Empty File %d=%s\n", (int) buf.st_size,
								  buffer);
			}} fclose(list_file);
			return NULL;
		}
	}
	return NULL;
}

  /* 
   *      Accept either a LABEL FILE or a PACKET FILE
   */
FILE *label_open(char *filename, char *mode)
{
	static int first_time = 1;
	FILE *file = NULL;
	char *filetype;
	char *data_filename = NULL;
	char buf[128];
	filetype = strrchr(filename, '.');

	/* 
	 *        Label File
	 */
	if(!strcmp(filetype, ".LBL"))
	{
		file = fopen(filename, mode);	/* for now, just open the file */
		if(file)
		{
			fprintf(stdout, "Label File %s\n", filename);
			while(fgets(buf, 80, file))
			{
				if(strstr(buf, "PDS_VERSION_ID"))
					if(!strstr(buf, "PDS3"))
					{
						fprintf(stdout, "LABEL Inconsistent %s\n",
								  "PDS_VERSION_ID");
						if(verbose_flag)
							fprintf(stdout, "    Expected PDS_VERSION_ID=PDS3\n");
					}
				if(strstr(buf, "RECORD_TYPE"))
					if(!strstr(buf, "FIXED_LENGTH"))
					{
						fprintf(stdout, "LABEL Inconsistent %s\n",
								  "RECORD_TYPE");
						if(verbose_flag)
							fprintf(stdout,
									  "    Expected RECORD_TYPE=FIXED_LENGTH\n");
					}
				if(!Record_Bytes)
					if(strstr(buf, "RECORD_BYTES"))
						Record_Bytes = label_value(buf);
				if(!File_Records)
					if(strstr(buf, "FILE_RECORDS"))
						File_Records = label_value(buf);
				if(!data_filename)
					if(strchr(buf, '^'))
					{					  /* filename shows up here *//* (1st. pointer we encounter) */
						data_filename = label_filename(buf);
					}
			}
			fclose(file);
			fprintf(stdout, "RECORD_BYTES  = %d ", Record_Bytes);
			if(verbose_flag)
				if(!Record_Bytes)
					fprintf(stdout, "Should ne non-zero");
			fprintf(stdout, "\n");
			fprintf(stdout, "FILE_RECORDS  = %d", File_Records);
			if(verbose_flag)
				if(!File_Records)
					fprintf(stdout, "Should ne non-zero");
			fprintf(stdout, "\n");
			filetype = strrchr(data_filename, '.');
		}
	}

	else
		data_filename = filename;
	if(filetype)
	{
		if((!strcmp(filetype, ".PKT")) || (!strcmp(filetype, ".DAT")))
		{								  /* for now, just open the file */
			file = fopen(data_filename, mode);
			if(!file)
			{
				fprintf(stdout, " NOT Found");
			}
		}
	}
	if(!file)
	{
		fprintf(stderr, "Expecting input on <stdin>\n");
		if(verbose_flag)
			fprintf(stdout,
					  "    You probably need to \"change directory\" to where\n"
					  "    the data files are located (the LBL files do not \n"
					  "    contain a path, and this program doesn't extract\n"
					  "    a path from the file name).\n");
		file = stdin;
	}
	first_time = 0;
	return file;
}

  /*  Eject Page 0x0C */
  /**********************************************************************
   *    Simple check to see if this really looks like WBR/WFR data      *
   *    12 bit data should not have bits 12-15 set and 8 bit data       *
   *    should have bits 4-7 set in some words of even/odd pairs        *
   **********************************************************************/
static int sanity_WFR(struct ARCHIVE_TIME_SERIES *archive)
{
	char result = 0;
	int mask = 0;
	int i;
	for(i = 16; i < msb_short(archive->samples); i++)
		mask |= msb_short(archive->time_series.wfr_sample[i]);
	if(mask & 0xF000)
		result = 1;
	return result;
}
static char sanity_WBR(struct ARCHIVE_TIME_SERIES *archive)
{
	char result = 2;
	int mask[2] = { 0, 0 };
	int i;
	for(i = 16; i < msb_short(archive->samples); i++)
	{
		mask[i & 1] |= archive->time_series.wbr_sample[i];
	}
	if((mask[0] & 0x00F0) && (mask[1] & 0x00F0))
	{
		result = 0;
		if(!(mask[0] & 0x0F) && !(mask[1] & 0x0F))
			result = 3;				  /* Methinks this is DUST */
	}
	return result;
}

/**********************************************************************
 *    All we're up to here is reading the Archive file and dumping    *
 *    status.  This provides a quick&dirty verificatiuon that the     *
 *    archive has been successfully written.                          *
 *                                                                    *
 *    NOTE that the various fields are built in such a way that we    *
 *    do not need to do anything unique for WFR, WBR, HSK;            *
 *    simply decode all status.  Inappropriate fields are coded       *
 *    so they can be easily ignored when appropriate                  *
   **********************************************************************/
int main(int argc, char *argv[])
{
	char c;
	char *list;
	int sanity_status = 0;
	FILE *input = stdin;
	static int count = 1;
	int inst_id;
	int status;
	int i;
	int j_band, j_ant, j_gain;
	int seconds;
	int hour, min, sec, msec;
	int rec_byt;
	int samp;
	int fsw;
	int remaining_length;
	int ptmp = 0;
	int raw_sample;
	int calibrated_data_flag = 0;
	int raw_data_flag = 0;
	int limited_dump_flag = 0;
	int header_flag[] = { 0, 0 };
	int newline_flag = 1;
	int additional_sanity_checks = 0;
	int test_flag = ARCHIVE_TIME_SERIES_EXPECTED_SIZE;
	float gain_setting, cal_factor;
	float dc, DC_value;
	float ymax, ymin, maximum_amplitude_sine_wave;
	float time_sample;
	double db_full_scale;
	double linear_scale;

/**************************************
 * These flags aren't used/needed for *
 * Archive data supplied on CD/DVD    *
 **************************************/
	int dust_data_flag = 0;
	int bfdl_data_flag = 0;
	int ipc_data_flag = 0;
	i = 0;
	while(Title[i])
		fprintf(stdout, "%s, ", Title[i++]);
	fprintf(stdout, " %s, %s ", Ver, Date);

#ifdef SPICE
	fprintf(stdout, "SPICE");

#endif								  /* */
	fprintf(stdout, "\n");
	while((c = getopt(argc, argv, "?haecrdbilvxzst")) != EOF) {
		switch (c) {
		case 'v':
			verbose_flag = 1;
			break;
		case 'l':
			limited_dump_flag = 1;
			break;
		case 'a':
			header_flag[0] = 1;
			break;
		case 'e':
			header_flag[1] = 1;
			break;
		case 'c':
			calibrated_data_flag = 1;
			break;
		case 'r':
			raw_data_flag = 1;
			break;
		case 'z':
			newline_flag = 0;
			break;
		case 't':
			test_flag = 0;
			break;
		case '?':
		case 'h':
			fprintf(stdout, "        Problem Contact: %s\n", Author);
			fprintf(stdout, "\n");
			fprintf(stdout,
					  "  This example code dumps the\n"
					  "  following archive data products\n");
			i = 0;
			while(Types[i])
				fprintf(stdout, "    %s\n", Types[i++]);
			fprintf(stdout, "  \n");
			fprintf(stdout, "  usage: %s <-flags> <filename>" "\n", argv[0]);
			fprintf(stdout, "  \n");
			fprintf(stdout, "      -flags\n");
			fprintf(stdout, "  \n");
			fprintf(stdout, "          -a    header decoded dump" "\n");
			fprintf(stdout,
					  "                    " "WFR  cnt:  19  1/4E4D8240."
					  "3.0   15205-21:14:16.286 " "\n");
			fprintf(stdout, "          -e    header hex dump" "\n");
			fprintf(stdout,
					  "                    " "hdr 4E 4C 8B 4D  01 60  "
					  "3B 64  00 C9 FD C5  08 20" "\n");
			fprintf(stdout, "          -r    raw data dump" "\n");
			fprintf(stdout, "                    " "raw   7C7 791 76E 76B 772 "
					  "77C 78E 7A1  7A7 78B 768 " "\n");
			fprintf(stdout,
					  "                    " " 32: 7B3 7CD 7E0 7F3 7FC "
					  "800 808 815  82F 845 84D " "\n");
			fprintf(stdout, "          -c    calibrated " "data dump\n");
			fprintf(stdout,
					  "                    " "DC_value 2053.599  "
					  "maximum_amplitude_" "sine_wave 2047.500\n");
			fprintf(stdout, "                    " "cal_factor 9.450\n");
			fprintf(stdout, "                    " "gain_setting 30.000\n");
			fprintf(stdout,
					  "                    " "db_full_scale 39.450 "
					  "linear_scale 66.372\n");
			fprintf(stdout, "                    " "calibrated samples\n");
			fprintf(stdout,
					  "                    " "sample #    mSec  raw "
					  "counts     " "B Field\n");
			fprintf(stdout,
					  "                    " "    0      0.000  2021   "
					  "7E5    -3.906e-02\n");
			fprintf(stdout,
					  "                    " "    1      0.140  2024   "
					  "7E8    -3.546e-02\n");
			fprintf(stdout,
					  "                    " "    2      0.280  2029   "
					  "7ED    -2.947e-02\n");
			fprintf(stdout, "          -l    limit dump to " "%d records\n",
					  RECORD_COUNT);

			/* 
			 *      This flag is used to debug the archive software
			 *      back at Iowa.
			 */
			fprintf(stdout,
					  "          -x    " "force record length from file" "\n");
			fprintf(stdout,
					  "                " "  (rather than from the labels)"
					  "\n");
			fprintf(stdout,
					  "          -z    " "suppress new line after SPICE time"
					  "\n");
			fprintf(stdout, "          -v    " "verbose error messages" "\n");
			fprintf(stdout,
					  "          -t    " "test data structure (#pragma pack)"
					  "\n");
			fprintf(stdout,
					  "                " "error message (this flag causes the"
					  "\n");
			fprintf(stdout,
					  "                " "error message to be displayed)" "\n");
			fprintf(stdout, "  \n");
			fprintf(stdout, "    data may be read from " "<stdin>\n");
			fprintf(stdout, "  \n");
			fprintf(stdout, "  \n");
			exit(0);
			break;
		case 'x':
			force_record_length = -1;
			break;
		case 's':
			additional_sanity_checks = 1;
			break;

	 /**************************************
     * These flags aren't used/needed for *
     * Archive data supplied on CD/DVD    *
     **************************************/
		case 'd':
			dust_data_flag = 1;
			break;
		case 'b':
			bfdl_data_flag = 1;
			break;
		case 'i':
			ipc_data_flag = 1;
			break;
		}
	}
	fprintf(stdout, "\n");

	 /****************************************************
     *  I don't know why I'm doing this, but...         *
     *  Check to see if the structure is the            *
     *  expected length (if NOT compiler won't          *
     *  work for this example code)                     *
     ****************************************************/
	if(sizeof(struct ARCHIVE_TIME_SERIES) != test_flag)
	{
		fprintf(stderr, "This compiler doesn't process ");
		fprintf(stderr, "\"#pragma pack\" correctly\n");
		fprintf(stderr, "The ARCHIVE_TIME_SERIES structure ");
		fprintf(stderr, "compiles to a length of %zu octets\n",
				  sizeof(struct ARCHIVE_TIME_SERIES));
		fprintf(stderr, "The ARCHIVE_TIME_SERIES_EXPECTED_SIZE ");
		fprintf(stderr, "is %d octets\n", ARCHIVE_TIME_SERIES_EXPECTED_SIZE);
		exit(0);
	}
	if(argc > 1)
	{
		fprintf(stdout, "Open file:%s\n", argv[argc - 1]);
		list = list_open(argv[argc - 1], 0);
		input = label_open(list, "r");
	}

	 /************************************************************
     *  Check the calibration tables (we have to have them      *
     *      arranged in the right order, or you'll have         *
     *      rather strange calibration results                  *
     ************************************************************/
	if(0)
	{
		fprintf(stdout, "search_coil_calibration_factor\n");
		for(i = 0; i < 16; i++)
		{
			fprintf(stdout, "  %6.3f  %6.3f  %6.3f  %6.3f  \n",
					  search_coil_calibration_factor[i][0],
					  search_coil_calibration_factor[i][1],
					  search_coil_calibration_factor[i][2],
					  search_coil_calibration_factor[i][3]);
		}
		fprintf(stdout, "gain_amp_setting\n");
		for(i = 0; i < 8; i++)
		{
			fprintf(stdout, "  %6.3f  %6.3f  %6.3f  %6.3f  \n",
					  gain_amp_setting[i][0], gain_amp_setting[i][1],
					  gain_amp_setting[i][2], gain_amp_setting[i][3]);
		}
	}

	 /************************************************************
     ************************************************************/

	 /************************************************************
     *    Although we don't need to keep all these steps        *
     *  separate, it makes checking intermediate results a      *
     *  little easier.  Memory is rarely a consideration        *
     *  these days, so away we go...                            *
     ************************************************************/
	archive = malloc(BUFFER_SIZE);
	raw_counts = malloc(BUFFER_SIZE);
	zero_raw_counts = malloc(BUFFER_SIZE);
	calibrated_field_strength = malloc(BUFFER_SIZE);
	calibrated_e_field_strength = malloc(BUFFER_SIZE);
	calibrated_b_field_strength = malloc(BUFFER_SIZE);

	 /************************************************************
     *  This read method will work for WBR, WFR and HSK files.  *
     *  The octet count is contained in the header and will     *
     *  be used to read the remaining portion of the record     *
     *  (i.e. the time-series data).  This method is            *
     *  independant of the label file.                          *
     ************************************************************/
	status = fread(archive, 32, 1, input);

	 /**********************************************************
     *  Sanity checking, set these values for the 1st. record *
     *    so we can perform sanity checks later               *
     *  You will notice that some of the labels are emitted   *
     *  in uppercase if these sanity checks fail.             *
     **********************************************************/
	rec_byt = msb_short(archive->record_bytes);
	samp = msb_short(archive->samples);
	fsw = archive->fsw_ver;
	if(Record_Bytes)
		if(rec_byt != Record_Bytes)
			fprintf(stdout, "Record Length inconsistent with label %d\n",
					  rec_byt);

	/**********************************************************
	 *  All primed and ready to go, we'll read again at the   *
	 *    end of the loop.  Trailing decision loops today...  *
	 **********************************************************/
	while(status)
	{

		/**********************************************************
		 *    Looking for a record with NON-ZERO time field !!!   *
		 **********************************************************/
		if(msb_long(archive->sclk.seconds))
		{

			/****************************************
			 *       This procedure does nothing     *
			 *       if you don not have SPICE       *
			 *****************************************/
			emit_time(msb_long(archive->sclk.seconds), archive->sclk.fine,
						 archive->sclk.partition, newline_flag);

			/* 
			 *      For WBR & WFR, Only 1 of these bits is set..
			 *       (BUT!!!  RAW is 00 and Housekeeping is 11)
			 *      SO we switched to a simple table lookup...
			 */
			inst_id =
				 get_status(&archive->validity_flag,
								ARCH_VALIDITY_FLAG_INST_ID, 0);
			if(header_flag[0])
				fprintf(stdout, "%s ", instrument_id[inst_id]);

			/* 
			 *      WBR timeout (See .FMT file for details)
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
				if(header_flag[0])
					fprintf(stdout, "%s",
							  get_status(&archive->status_flag,
											 ARCH_STATUS_FLAG_TIMEOUT,
											 0) ? "TMO " : " ");
			if(header_flag[0])
				fprintf(stdout, "cnt:%4d  ", count++);

			/* 
			 *      SCLK is 40 bits.  See .FMT files and user guide for
			 *        discussion of the lower 5 bits of sclk.fine.
			 *      NOTE that partition may show up as ZERO, we must take
			 *        care to fix it for SPICE.
			 */
			if(header_flag[0])
				fprintf(stdout, "%d/", archive->sclk.partition);	/* SPICE
																					   partition 
																					 */
			if(header_flag[0])
				fprintf(stdout, "%08X", (int) msb_long(archive->sclk.seconds));	/* 32b 
																										   sec 
																										 */
			if(header_flag[0])
				fprintf(stdout, ".%d", (archive->sclk.fine >> 5) & 0x07);	/* 3 
																								   bits 
																								   RTI 
																								 */
			if(header_flag[0])
				fprintf(stdout, ".%d", (archive->sclk.fine >> 1) & 0x0F);	/* 4 
																								   bits 
																								   sequence 
																								 */
			if(header_flag[0])
				fprintf(stdout, "%s",
						  ((archive->sclk.fine >> 0) & 0x01) ? "*" : " ");

			/* 1 bit time update */
			/* 
			 *      Status flag that indicates fine time (sub-RTI) is available.
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(header_flag[0])
					fprintf(stdout, "%s",
							  get_status(&archive->status_flag,
											 ARCH_STATUS_FLAG_FINE_TIME_QUALITY,
											 0) ? "Q" : " ");
				if(header_flag[0])
					fprintf(stdout, " ");
			}

			/* 
			 *      SCET (from CHDO records, we use same format)
			 */
			seconds = msb_long(archive->scet.msec_of_day) / 1000;
			hour = seconds / 3600;
			min = seconds / 60 - hour * 60;
			sec = seconds % 60;
			msec = msb_long(archive->scet.msec_of_day) % 1000;
			if(header_flag[0])
				fprintf(stdout, "%5d-", msb_short(archive->scet.days));
			if(header_flag[0])
				fprintf(stdout, "%02d:%02d:%02d.%03d ", hour, min, sec, msec);

			/* 
			 *      16 bit RTI (3/RTI 13/seconds)
			 */
			if(header_flag[0])
				fprintf(stdout, "rti:%04X ", msb_short(archive->data_rti));

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
			i = msb_short(archive->record_bytes);
			if(header_flag[0])
				fprintf(stdout, "%s:%d ",
						  (rec_byt == i) ? "rec byt" : "REC BYT", i);
			rec_byt = i;

			/* 
			 *      Number of data samples (8 bits for WBR, 16 bit for WFR)
			 *
			 *      Sanity Check:  We expect most records to be the
			 *        same length.  Instrument operating modes and
			 *        lost CDS records can alter this, but we generally
			 *        should have all records the same sample count.
			 */
			i = msb_short(archive->samples);
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(header_flag[0])
					fprintf(stdout, "%s:%d  ",
							  (samp == i) ? "samples" : "SAMPLES", i);
			}

			else
			{
				if(header_flag[0])
					fprintf(stdout, "%s:%d  ",
							  (samp == i) ? "hsk byt" : "HSK BYT", i);
			}
			samp = i;

			/* 
			 *      Effectively, tells us the sampling rate that the A/D runs at
			 *        and therefore, the filter selected.
			 *      (of the 4 available sample rate, 2 are for the WBR
			 *        and 2 are for the WFR, we don't need to keep track
			 *        of which receiver we're looking at, it's already
			 *        taken care of for us)
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(header_flag[0])
					fprintf(stdout, "%s ",
							  Frequency_Band[archive->frequency_band & 0x03]);
			}

			/* 
			 *      Gain amplifier selection
			 *      (WBR and WFR both map to the same values, WBR
			 *        using 0-70 and WFR onlyusing 0-30)
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(header_flag[0])
					fprintf(stdout, "%d0dB ",
							  get_status(&archive->gain, ARCH_GAIN_ANALOG_GAIN,
											 0));
			}

			/* 
			 *      AGC enabled?
			 *      (This is always ZERO for WFR, it does AGC without
			 *        any hardware assists and we have chosen not to
			 *        tell anyone when the AGC software is or isn't
			 *        running (actually, no status bits left) ).
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(header_flag[0])
					fprintf(stdout, "%s",
							  get_status(&archive->status_flag,
											 ARCH_STATUS_FLAG_AGC_ENABLE,
											 0) ? "AGC " : " ");
			}

			/* 
			 *      Antenna selection
			 *      We map all antenna selections to a field of
			 *        sizteen possible antenna's.  Not all 16
			 *        patterns are meaningful, and not all remaining
			 *        selections are available to the WBR/WFR.  Again,
			 *        this scheme allows us to simply do the table-lookup
			 *        and proceed as the patterns for all receivers are
			 *        mapped to the appropriate bit patterns.
			 *
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(header_flag[0])
					fprintf(stdout, "%s", Antenna[archive->antenna & 0x0F]);
			}

			/* 
			 *      HFR/H2 downconvert enabled ?
			 *      (H1/H2 are mutually exclusive)
			 *      (This is a WBR unique field, it is always
			 *        zero for WFR, RAW, and HSK).
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(header_flag[0])
					fprintf(stdout, "%s",
							  get_status(&archive->status_flag,
											 ARCH_STATUS_FLAG_HFR_H2,
											 0) ? "/H2 " : " ");
			}

			/* 
			 *      HFR/H1 downconvert enabled ?
			 *      (H1/H2 are mutually exclusive)
			 *      (This is a WBR unique field, it is always
			 *        zero for WFR, RAW, and HSK).
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(header_flag[0])
					fprintf(stdout, "%s",
							  get_status(&archive->status_flag,
											 ARCH_STATUS_FLAG_HFR_H1,
											 0) ? "/H1 " : " ");
			}

			/* 
			 *      sub-RTI: WBR can free-run and this field has additional
			 *              timing information.  This shows how to determinbe
			 *              the millisecond offset from the start of RTI when
			 *              this field is marked valid.
			 *      This field is unique to the WBR, all other receivers
			 *        within the instrument are synchrounous with the RTI
			 *        pulse.  This field, therefore, is always ZERO
			 *        for WFR, RAW, and HSK records.
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(get_status
					(&archive->validity_flag, ARCH_VALIDITY_FLAG_SUB_RTI, 0))
				{
					if(header_flag[0])
						fprintf(stdout, "%3dmS ", archive->sub_rti);
					if(!get_status
						(&archive->validity_flag,
						 ARCH_STATUS_FLAG_FINE_TIME_QUALITY, 0))
						if(header_flag[0])
							fprintf(stdout, "+10/-0 mS ");
				}
			}

			/* 
			 *      HFR frequency translation:
			 *              When HFR delivers a down-converted signal to
			 *              WBR, this fragment illustrates how to determine
			 *              the mixing frequency.  Least signifigant bit
			 *              of the frequency holds H1/H2 flag.
			 *        Again, this is one of these WBR only fields that
			 *      is set to ZERO for everything else.
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(get_status
					(&archive->validity_flag, ARCH_VALIDITY_FLAG_HFR_XLATE, 0))
				{
					if(get_status
						(&archive->status_flag, ARCH_STATUS_FLAG_HFR_H1, 0))
						if(header_flag[0])
							fprintf(stdout, "%6.3fMhz ",
									  (float) archive->hfr_xlate * 0.025);
					if(get_status
						(&archive->status_flag, ARCH_STATUS_FLAG_HFR_H2, 0))
						if(header_flag[0])
							fprintf(stdout, "%6.3fMhz ",
									  4.025 + (float) archive->hfr_xlate * 0.050);
				}
			}

			/* 
			 *      L/P bias, when connected to WBR or WFR
			 *      Both WBR and WFR can connect to the L/P sphere
			 *        and measure current with high temporal
			 *        resolution.
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(get_status
					(&archive->validity_flag, ARCH_VALIDITY_FLAG_LP_DAC_0, 0))
					if(header_flag[0])
						fprintf(stdout, "DAC0:%03d ", archive->lp_dac_0);
			}

			/* 
			 *      L/P bias, when connected to WFR
			 *        Only the WFR can connect to the "cylinders" and
			 *      measure current, all other records have the
			 *      validity bits cleard and the DAC-1 field
			 *      cleared to ZERO.
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(get_status
					(&archive->validity_flag, ARCH_VALIDITY_FLAG_LP_DAC_1, 0))
					if(header_flag[0])
						fprintf(stdout, "DAC1:%03d ", archive->lp_dac_1);
			}

			/* 
			 *      MSF bit (More Status Follows).  All we want is to see if it
			 *        is set.  It is used to set some of the previous status bits
			 *        in the archive program.  We're looking at it here as a
			 *        sanity check.
			 *      This bit is used to indicate that the RPWS mini-packet
			 *        (for WBR or WFR) contains 2 extra status octets.  We
			 *        always carry the extra status in this archive record,
			 *        but we save the 2 bytes when sending data to the
			 *        ground.  (Doesn't save much, but it sure does give
			 *        you a warm fuzzy feeling, saving those 2 bytes:-)
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(get_status
					(&archive->validity_flag, ARCH_VALIDITY_FLAG_MSF, 0))
					if(header_flag[0])
						fprintf(stdout, "MSF ");
			}

			/* 
			 *      Software version loaded into the RPWS instrument.
			 *
			 *      More Sanity Checking:
			 *        Software version shouldn't be changing (well,
			 *        at least no too often, and at some very fixed
			 *        points).  If it's flitting around, we need to
			 *        investigate a bit...
			 *      This is of particular interest when analyzing
			 *        the performance of the dust detection algorithm.
			 *        Some of the WBR status bits were implemented in
			 *        later versions of the software (will always be
			 *        zero in earlier data).
			 */
			i = archive->fsw_ver;
			if(i)
				if(header_flag[0])
					fprintf(stdout, "%s V%d.%d ", (fsw == i) ? "fsw" : "FSW",
							  i / 100, i % 100);
			fsw = i;

			/* 
			 *        OK, the Archive software knows about the gritty
			 *      details; what got fixed or broken with each revision
			 *      of the flight software.  When it decides that there
			 *      is data that is probably junk, this bit is set.
			 *        Being the pack-rats that we are, we avoid discarding
			 *      any data (even if it is bad).
			 *
			 *        When this bit is set, just discard the data
			 *      (If this were a data analysis application, we
			 *       would place this check at the begining, and skip
			 *       the data record).
			 */
			if(inst_id != ARCH_VALIDITY_FLAG_INST_ID_HSK)
			{
				if(get_status
					(&archive->validity_flag, ARCH_STATUS_FLAG_SUSPECT, 0))
					if(header_flag[0])
						fprintf(stdout, "Suspect Data");
			}
			if(header_flag[0])
				fprintf(stdout, "\n");

		 /********************************************************************
        *********************************************************************
        **    Now read the remaining portion of the record in              **
        **        NOTE that the record length in the 1st. case is          **
        **      obtained from the label record (assuming we read           **
        **      the label record to obtain the data filename).             **
        **      the 'force_record_length' is a command-line switch         **
        **      that is used to disable the use of the length field        **
        **      contained in the label record (i.e. force use of the       **
        **      length field in the data record).                          **
        **        NOTE that the record length in the 2nd. case is          **
        **      contained within the header that was read first            **
        **      This read method is universal for WBR/WFR/HSK              **
        **      archive data (we do not need to know length in             **
        **      advance nor is there a need to consult the label file).    **
        **                                                                 **
        **      NOTE that we must read the entire data record (even when   **
        **                'ARCH_STATUS_FLAG_SUSPECT' is set!!!             **
        *********************************************************************
        ********************************************************************/
			memset(archive->time_series.byte_sample, 0xFF, 1024);
			if(Record_Bytes && force_record_length)
				remaining_length = Record_Bytes - 32;

			else
				remaining_length = msb_short(archive->record_bytes) - 32;
			status =
				 fread(archive->time_series.byte_sample, remaining_length, 1,
						 input);
			if(!status)				  /* ERROR condition */
				exit(0);				  /* not much grace here, eh? */
			if(header_flag[1])
			{
				fprintf(stdout, "hdr");
				hexdump((char *) archive, 0, 32, 0x15154008);
				fprintf(stdout, "\n");
			 }

			/********************************************************
			 *        Data sanity checks.  Ground processing has,   *
			 *      in earlier versions, has a tendancy to stumble  *
			 *      when telemetry is missing.  This typically      *
			 *      has a result of mixing WBR and WFR data...      *
			 *        Here we will look to see that data looks      *
			 *      appropriate.                                    *
			 ********************************************************/
			sanity_status = 0;
			if(additional_sanity_checks)
			{
				switch (inst_id)
				{
				case ARCH_VALIDITY_FLAG_INST_ID_WFR:
					sanity_status = sanity_WFR(archive);
					break;
				case ARCH_VALIDITY_FLAG_INST_ID_WBR:
					sanity_status = sanity_WBR(archive);
					break;
				}
			}

			/************************************************
			 *      Report results of sanity checks         *
			 ************************************************/
			switch (sanity_status)
			{
			case 1:
				fprintf(stdout, "This WFR data looks a little fishy.\n");
				if(verbose_flag)
					fprintf(stdout,
							  "    In the 16 bit data time-series data words "
							  "(that contain 12 bit samples)\n"
							  "    some of upper bits are set (this does happen "
							  "in the instrument, but is\n"
							  "    the result of how ground processing raects "
							  "to lost telemetry\n");
				break;
			case 2:
				fprintf(stdout, "This WBR data looks a little fishy \n");
				if(verbose_flag)
					fprintf(stdout,
							  "    Looking at every other sample, the upper 4 "
							  "bits are set to zero.\n"
							  "    This is usually an indication that ground "
							  "processing has\n"
							  "    encountered missing telemetry records, "
							  "causing WFR data to be\n"
							  "    inserted into a WBR record\n");
			case 3:
				fprintf(stdout, "This data looks like a DUST record\n");
				if(verbose_flag)
					fprintf(stdout,
							  "    and that's just fine (not an error)\n");
				break;
			}

			/************************************************
			 *      DUMP raw values                         *
			 *      Accomodate differing sample sizes       *
			 ************************************************/
			if(raw_data_flag)
			{
				if(WIDE8 > 10)
				{
					fprintf(stdout, "raw  ");
				}
				if(1)
				{						  /* this is a wide style dump */
					for(i = 0; i < msb_short(archive->samples); i++)
					{
						switch (get_status
								  (&archive->validity_flag,
									ARCH_VALIDITY_FLAG_INST_ID, 0))
						{
						case ARCH_VALIDITY_FLAG_INST_ID_HSK:
							fprintf(stdout, " %02X",
									  archive->time_series.hsk_sample[i]);
							ptmp = WIDE;
							break;
						case ARCH_VALIDITY_FLAG_INST_ID_RAW:
						case ARCH_VALIDITY_FLAG_INST_ID_WBR:
							fprintf(stdout, " %02X",
									  archive->time_series.wbr_sample[i]);
							ptmp = WIDE8;
							break;
						case ARCH_VALIDITY_FLAG_INST_ID_WFR:
							fprintf(stdout, " %03X",
									  msb_short(archive->time_series.
													wfr_sample[i]));
							ptmp = WIDE12;
							break;
						}
						if((i % 8 == 7)
							&& (i < msb_short(archive->samples) - 1))
							fprintf(stdout, " ");
						if((i % ptmp == ptmp - 1)
							&& (i < msb_short(archive->samples) - 1))
						{
							fprintf(stdout, "\n");
							fprintf(stdout, "%4d:", i + 1);
						}
					}					  /* for(i=0; i<msb_short(archive->samples); 
										     i++) */
					fprintf(stdout, "\n");
				}						  /* if(1) */
			}

			/* if(raw_data_flag) */
			/************************************************
			 *      Apply calibration                       *
			 *      Simple full-band cal (band center)      *
			 *                                              *
			 *        See the WFRWBR.TXT file in the        *
			 *      EXTRAS directory for a discussion of    *
			 *      the method used to calibrate the data.  *
			 *        This code uses the steps described    *
			 *      without any shortcuts/optimizations.    *
			 ************************************************/
			/********************************
			 *      Step 5 will do table    *
			 *  	 lookups based on the	 *
			 *  	 following status 		 *
			 ********************************/
			if(calibrated_data_flag)
			{
				j_band = archive->frequency_band;	/* band & receiver */
				j_gain = get_status(&archive->gain, ARCH_GAIN_ANALOG_GAIN, 0);
				j_ant = archive->antenna;

				/********************************
				 *  	 Step 5a Average Value   *
				 ********************************/
				dc = 0.0;
				ymax = 0.0;
				ymin = 0.0;
				switch (get_status
						  (&archive->validity_flag, ARCH_VALIDITY_FLAG_INST_ID,
							0))
				{
				case ARCH_VALIDITY_FLAG_INST_ID_WBR:
					ymax = 255.0;
					ymin = 0.0;
					for(i = 0; i < msb_short(archive->samples); i++)
					{
						raw_counts[i] =
							 (float) archive->time_series.wbr_sample[i];
						dc += raw_counts[i];
					} break;
				case ARCH_VALIDITY_FLAG_INST_ID_WFR:
					ymax = 4095.0;
					ymin = 0.0;
					for(i = 0; i < msb_short(archive->samples); i++)
					{
						raw_counts[i] =
							 (float) msb_short(archive->time_series.
													 wfr_sample[i]);
						dc += raw_counts[i];
					} break;
				}
				DC_value = dc / (float) msb_short(archive->samples);
				maximum_amplitude_sine_wave = (ymax - ymin) / 2.0;
				fprintf(stdout,
						  "DC_value %.3f  " "maximum_amplitude_sine_wave %.3f\n",
						  DC_value, maximum_amplitude_sine_wave);

				/********************************
				 *  	 Step 5b Normalize		 *
				 *  		 about zero 			 *
				 ********************************/
				for(i = 0; i < msb_short(archive->samples); i++)
				{
					zero_raw_counts[i] = raw_counts[i] - DC_value;
				}

				/********************************
				 *  	 Step 5c Apply the		 *
				 *  	 Calibration Factor  	 *
				 ********************************/
				cal_factor = Calibration_Factor[j_band];
				fprintf(stdout, "cal_factor %.3f\n", cal_factor);

				/********************************
				 *  	 Step 5d Gain Amp 		 *
				 *  		  setting				 *
				 ********************************/
				gain_setting = gain_amp_setting[j_gain][j_band];
				fprintf(stdout, "gain_setting %.3f\n", gain_setting);

				/********************************
				 * 	  Step 5e Convert dB 	  *
				 * 			to linear			  *
				 ********************************/
				db_full_scale = cal_factor + gain_setting;
				linear_scale = pow(10., db_full_scale / 20.);
				linear_scale = linear_scale / sqrt(2.);
				fprintf(stdout, "db_full_scale %.3f linear_scale %.3f\n",
						  db_full_scale, linear_scale);

				/********************************
				 *  	 Step 5f Nommalize		 *
				 ********************************/
				for(i = 0; i < msb_short(archive->samples); i++)
				{
					calibrated_field_strength[i] =
						 zero_raw_counts[i] / maximum_amplitude_sine_wave;
					calibrated_field_strength[i] =
						 calibrated_field_strength[i] / linear_scale;
				}

				/********************************
				 * 	  Step 5g convert to 	  *
				 * 	electric field strength   *
				 ********************************/
				for(i = 0; i < msb_short(archive->samples); i++)
				{
					calibrated_e_field_strength[i] =
						 calibrated_field_strength[i] /
						 geometric_antenna_length[j_ant];
				}

				/********************************
				 *  	 Step 5h convert to  	 *
				 *    magnetic field strength   *
				 ********************************/
				for(i = 0; i < msb_short(archive->samples); i++)
				{
					calibrated_b_field_strength[i] =
						 calibrated_field_strength[i] * 24.0;
					calibrated_b_field_strength[i] =
						 calibrated_b_field_strength[i] /
						 search_coil_calibration_factor[j_ant][j_band];
				}

				/************************************************
				 *      DUMP calibrated values                  *
				 ************************************************/
				fprintf(stdout, "calibrated samples\n");
				fprintf(stdout, "   sample # ");
				fprintf(stdout, "   mSec ");
				fprintf(stdout, " raw counts");
				fprintf(stdout, "     %s", measurement[j_ant]);
				fprintf(stdout, "\n");
				time_sample = 0.0;
				for(i = 0; i < msb_short(archive->samples); i++)
				{
					raw_sample = -1;
					switch (get_status
							  (&archive->validity_flag,
								ARCH_VALIDITY_FLAG_INST_ID, 0))
					{
					case ARCH_VALIDITY_FLAG_INST_ID_WBR:
						raw_sample = archive->time_series.wbr_sample[i] & 0xFF;
						break;
					case ARCH_VALIDITY_FLAG_INST_ID_WFR:
						raw_sample =
							 msb_short(archive->time_series.
										  wfr_sample[i]) & 0xFFF;
						break;
					}
					fprintf(stdout, "   %5d ", i);
					fprintf(stdout, "   %7.3f ", time_sample * 1000.);
					time_sample += Sample_Time[archive->frequency_band & 0x03];
					fprintf(stdout, " %4d ", raw_sample);
					fprintf(stdout, " %4.2X ", raw_sample);
					if(measurement[j_ant][0] == 'E')
						fprintf(stdout, "  %11.3e",
								  calibrated_e_field_strength[i]);
					if(measurement[j_ant][0] == 'B')
						fprintf(stdout, "  %11.3e",
								  calibrated_b_field_strength[i]);
					fprintf(stdout, "\n");
				}						   
										  
			}							  
		}
		/* if(msb_long(archive->sclk.seconds)) */
		else{
			/********************************************************
			*      We end up here if sclk.seconds is zero...       *
			*              (This must be a bad thing,              *
			*               as the computer seems to print "BAD"   *
			*               if this occurs :-).                    *
			*      Note that we skipped the read that collects     *
			*      the data portion of the record, so we'll        *
			*      assume that at least that part of the record    *
			*      is correct (archive software seems to get the   *
			*      length correct, even of the time gets royally   *
			*      hosed...                                        *
			********************************************************/
		
			fprintf(stdout, "BAD");
			hexdump((char *) archive, 0, 32, 0);
			status =
				 fread(archive->time_series.byte_sample,
						 msb_short(archive->record_bytes) - 32, 1, input);
			fprintf(stdout, "\n   ");
			hexdump((char *) archive, 32, 64, 0);
		}
		
		if(newline_flag)
			fprintf(stdout, "\n");

		/* 
		 *      Just a few records...
		 *        (for debugging this code)
		 */
		Record_Count++;
		if(limited_dump_flag)
		{
			if(Record_Count >= RECORD_COUNT)
				exit(0);
		}

	/********************************************************
         *      OK, here we are at the bottom of the loop       *
         *      This means it's time for a new record.          *
         *      since a status of ZERO indicates end-of-file    *
         *      we have the makings of out trailing-decision    *
         *      loop all ready to go...                         *
         ********************************************************/
		status = fread(archive, 32, 1, input);	/* next HEADER */

	/********************************************************
         *  We also allow a list of files.   IN the UNIX world  *
         *      we would use something like                     *
         *              'ls -1 *.PKT > xxx.lst'                 *
         *              'rpws_archive_example xxx.lst'          *
         *      to dump a series of files                       *
         ********************************************************/
		if(!status)
		{								  /* EOF occured *//* fetch next record */
			fclose(input);
			list = list_open(NULL, 0);	/* from list file */
			if(list)					  /* */
				input = label_open(list, "r");	/* and try to open */
			else
				input = NULL;
			if(input)
			{							  /* *//* If it's there */
				status = fread(archive,	/* read next record */
									32, 1, input);	/* */
			}
		}

	/********************************************************
         ********************************************************/
	}									  /* while (status) */
	if(File_Records)
		if(!limited_dump_flag)
			if(Record_Count != File_Records)
				fprintf(stdout, "File Records inconsistent with label %d\n",
						  Record_Count);
	return 0;
}
