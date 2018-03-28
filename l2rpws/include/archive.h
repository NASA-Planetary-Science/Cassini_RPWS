
/**********************************************
 ******		#pragma pack(1)		 ******
 **  (it's late Friday, what can I say?)     **
 **   Back in the good old days (back when   **
 ** men were men... you know...) some        **
 ** well-meaning soul added "structures"     **
 ** to the c-language.  And we all saw this  **
 ** was good, or at least made life simple.  **
 ** (of course, we all used FORTRAN in those **
 **  days).  THEN, along comes ALPHA and     **
 ** some fool (at least foolish to us old    **
 ** fortran types...) notices that the       **
 ** performance sucks on un-aligned memory   **
 ** references.  This causes 'boundary       **
 ** alignment' to resurface (from the days   **
 ** of S/360) and be applied to structures   **
 ** (to make the compiler produce fast       **
 ** code).  Now, this compiler doesn't seem  **
 ** to figure out that long-short-short-long **
 ** is OK and we dig deep into the man pages **
 ** to find out if #pragma is supported to   **
 ** allow a structure to be packed into      **
 ** memory.                                  **
 **					     **
 **   So, for this to work, your compiler    **
 ** needs to respect the "#pragma pack(1)"   **
 ** directive to make this structure map     **
 ** into memory without padding.             **
 **   You might note that everything ends    **
 ** up aligned on natural boundaries (i.e.   **
 ** 16 bit short is on 16 bit boundary, 32   **
 ** bit int are on 32 bit boundary).         **
 **   Anyone wishing to enter into a 	     **
 ** philosophical discussion about this will **
 ** need to have worked on a machine that    **
 ** enforces boundary alignment (i.e. s/360),**
 ** one that doesn't (i.e. VAX), and at least**
 ** one machine that has a word size that is **
 ** NOT a multiple of 8 bits (i.e. some      **
 ** Univac, CDC, or GE machine of 18/36 bit  **
 ** word size)				     **
 **					     **
 **   #pragma pack(1) tells the compiler     **
 ** to pack the structure into memory with   **
 ** NO PADDING.  Since we have settled on    **
 ** ARCHIVE_TIME_V1, the structure	     **
 ** ARCHIVE_TIME_SERIES must be exactly 32   **
 ** bytes in length or things won't work     **
 **                                          **
 ** Later, #pragma pack() tells the          **
 ** compiler that it is free to resume       **
 ** performing memory allocation             **
 ** optmizations                             **
 **********************************************/
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
struct SPICE_TIME
{
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
 *****	ARCHIVE_TIME_V1		selected 3/2003			*****
 *****		Expresses time in the format supplied in the	*****
 *****		CHDO headers attached to raw data.  It is	*****
 *****		days/milliseconds(of day) from 1/1/1958		*****
 *****								*****
 *****	ARCHIVE_TIME_V2						*****
 *****		Expresses time in the internal format of the	*****
 *****		SPICE kernel (Barycentric Dynamical Time).	*****
 *****		Stored as a IEEE Double Procision float		*****
 *****								*****
 *****	ARCHIVE_TIME_V3						*****
 *****		Expresses time as an ASCII text string		*****
 *****								*****
 *****	ARCHIVE_TIME_V4						*****
 *****		Expresses time in the internal format of the	*****
 *****		SPICE kernel (Barycentric Dynamical Time), 	*****
 *****		stored as a IEEE Double Procision float		*****
 *****		ALSO						*****
 *****		Expresses time in the format supplied in the	*****
 *****		CHDO headers attached to the raw data.  It is	*****
 *****		days/milliseconds(of day) from 1/1/1958		*****
 *****								*****
 ********************************************************************/
#define ARCHIVE_TIME_V1

#ifdef ARCHIVE_TIME_V1
#define ARCHIVE_TIME_SERIES_EXPECTED_SIZE 224
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
    unsigned char byte_sample[192];     /* 33 */
    unsigned short word_sample[96];     /* 33 */
    unsigned char hsk_sample[192];      /* 33 */
    unsigned char wbr_sample[192];      /* 33 */
    unsigned short wfr_sample[96];      /* 33 */
  } time_series;
};
#endif

#ifdef ARCHIVE_TIME_V2
#define ARCHIVE_TIME_SERIES_EXPECTED_SIZE 224
struct ARCHIVE_TIME_SERIES
{
  double et;                            /*  1 */
  struct archive_event_clock sclk;      /*  9 */
  unsigned short record_bytes;          /* 15 */
  unsigned short samples;               /* 17 */
  unsigned short data_rti;              /* 19 */
  unsigned char validity_flag;          /* 21 */
  unsigned char status_flag;            /* 22 */
  unsigned char frequency_band;         /* 23 */
  unsigned char gain;                   /* 24 */
  unsigned char antenna;                /* 25 */
  unsigned char agc;                    /* 26 */
  unsigned char hfr_xlate;              /* 27 */
  unsigned char sub_rti;                /* 28 */
  unsigned char lp_dac_0;               /* 29 */
  unsigned char lp_dac_1;               /* 30 */
  unsigned char fsw_ver;                /* 31 */
  unsigned char spare[1];               /* 32 */
  union
  {
    unsigned char byte_sample[192];     /* 33 */
    unsigned short word_sample[96];     /* 33 */
    unsigned char hsk_sample[192];      /* 33 */
    unsigned char wbr_sample[192];      /* 33 */
    unsigned short wfr_sample[96];      /* 33 */
  } time_series;
};
#endif

#ifdef ARCHIVE_TIME_V3
#define ARCHIVE_TIME_SERIES_EXPECTED_SIZE 240
struct ARCHIVE_TIME_SERIES
{
  char scet[24];                        /*  1 */
  struct archive_event_clock sclk;      /* 25 */
  unsigned short record_bytes;          /* 31 */
  unsigned short samples;               /* 33 */
  unsigned short data_rti;              /* 35 */
  unsigned char validity_flag;          /* 37 */
  unsigned char status_flag;            /* 38 */
  unsigned char frequency_band;         /* 39 */
  unsigned char gain;                   /* 40 */
  unsigned char antenna;                /* 41 */
  unsigned char agc;                    /* 42 */
  unsigned char hfr_xlate;              /* 43 */
  unsigned char sub_rti;                /* 44 */
  unsigned char lp_dac_0;               /* 45 */
  unsigned char lp_dac_1;               /* 46 */
  unsigned char fsw_ver;                /* 47 */
  unsigned char spare[1];               /* 48 */
  union
  {
    unsigned char byte_sample[192];     /* 33 */
    unsigned short word_sample[96];     /* 33 */
    unsigned char hsk_sample[192];      /* 49 */
    unsigned char wbr_sample[192];      /* 49 */
    unsigned short wfr_sample[96];      /* 49 */
  } time_series;
};
#endif

#ifdef ARCHIVE_TIME_V4
#define ARCHIVE_TIME_SERIES_EXPECTED_SIZE 240
struct ARCHIVE_TIME_SERIES
{
  double et;                            /*  1 */
  struct archive_event_clock sclk;      /*  9 */
  struct archive_event_time scet;       /* 15 */
  unsigned char sp1[4];                 /* 21 */
  unsigned short record_bytes;          /* 25 */
  unsigned short samples;               /* 27 */
  unsigned short data_rti;              /* 29 */
  unsigned char validity_flag;          /* 31 */
  unsigned char status_flag;            /* 32 */
  unsigned char frequency_band;         /* 33 */
  unsigned char gain;                   /* 34 */
  unsigned char antenna;                /* 35 */
  unsigned char agc;                    /* 36 */
  unsigned char hfr_xlate;              /* 37 */
  unsigned char sub_rti;                /* 38 */
  unsigned char lp_dac_0;               /* 39 */
  unsigned char lp_dac_1;               /* 40 */
  unsigned char fsw_ver;                /* 41 */
  unsigned char sp2[3];                 /* 42 */
  unsigned char sp3[4];                 /* 45 */
  union
  {
    unsigned char byte_sample[192];     /* 33 */
    unsigned short word_sample[96];     /* 33 */
    unsigned char hsk_sample[192];      /* 49 */
    unsigned char wbr_sample[192];      /* 49 */
    unsigned short wfr_sample[96];      /* 49 */
  } time_series;
};
#endif

#pragma pack()


/* Inspect a mini-packet and set the status_flag in the above struct */
unsigned long set_status (unsigned char *status, int control, int pattern);



 /*
  * WFR data frame offsets, shifts and mask values 
  */
 /*
  * INDEX values are relative to the minipacket   
  */

 /*
  *     bits 16-23      Index into minipacket
  *     bits 8-15       Shift count
  *     bits 0-7        Mask
  */

/*****************************************************************/

/*   1  |    2  |    3  |    4  |    5  |    6  |    7  |    8   */

/*****************************************************************\
 *   7  |    6  |    5  |    4  |    3  |    2  |    1  |    0  |*
 *	|	|	|	|	|	|	|	|*
 *  MSF |   WBR |   WFR | WALSH |   SUB |   HFR |   LP  |   LP  |*
 *	|	|	|	|   RTI | XLATE | DAC 0	| DAC 1	|*
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
#define ARCH_VALIDITY_FLAG_MSF			0x00000701
#define ARCH_VALIDITY_FLAG_WBR			0x00000601
#define ARCH_VALIDITY_FLAG_WFR			0x00000501
#define ARCH_VALIDITY_FLAG_STIM			0x00000503
#define ARCH_VALIDITY_FLAG_RAW			0x00000503
#define ARCH_VALIDITY_FLAG_INST_ID		0x00000503
#define ARCH_VALIDITY_FLAG_INST_ID_BIT_WBR	0x00000601
#define ARCH_VALIDITY_FLAG_INST_ID_BIT_WFR	0x00000501
#define ARCH_VALIDITY_FLAG_INST_ID_WBR		2
#define ARCH_VALIDITY_FLAG_INST_ID_WFR		1
#define ARCH_VALIDITY_FLAG_INST_ID_HSK		3
#define ARCH_VALIDITY_FLAG_INST_ID_RAW		0

#define ARCH_VALIDITY_FLAG_WALSH_DGF		0x00000401
#define ARCH_VALIDITY_FLAG_SUB_RTI		0x00000301
#define ARCH_VALIDITY_FLAG_HFR_XLATE		0x00000201
#define ARCH_VALIDITY_FLAG_LP_DAC_0		0x00000101
#define ARCH_VALIDITY_FLAG_LP_DAC_1		0x00000001

/*****************************************************************/

/*   1  |    2  |    3  |    4  |    5  |    6  |    7  |    8   */

/*****************************************************************\
 *   7  |    6  |    5  |    4  |    3  |    2  |    1  |    0  |*
 *	|	|	|	|	|	|	|	|*
 *  AGC |  FINE |  WBR  |SUSPECT| HFR/X | HFR/X |   EU  |   EV  |*
 *ENABLE|  TQF	|TIMEOUT| DATA	|  H2   |  H1   |CURRENT|CURRENT|*
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
#define ARCH_STATUS_FLAG_AGC_ENABLE		0x00000701
#define ARCH_STATUS_FLAG_FINE_TIME_QUALITY	0x00000601
#define ARCH_STATUS_FLAG_TIMEOUT		0x00000501
#define ARCH_STATUS_FLAG_SUSPECT		0x00000401
#define ARCH_STATUS_FLAG_HFR_H2			0x00000301
#define ARCH_STATUS_FLAG_HFR_H1			0x00000201
#define ARCH_STATUS_FLAG_EU_CURRENT		0x00000101
#define ARCH_STATUS_FLAG_EV_CURRENT		0x00000001

/*****************************************************************/

/*   1  |    2  |    3  |    4  |    5  |    6  |    7  |    8   */

/*****************************************************************\
 *   7  |    6  |    5  |    4  |    3  |    2  |    1  |    0  |*
 *	 	|		|	|			|*
 *	 	| WALSH DIGITAL	|	|  ANALOG GAIN SETTING	|*
 *	 	|  GAIN FACTOR	|	|			|*
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
