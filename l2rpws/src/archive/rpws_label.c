
/********************************************************************
 ****		rpws_label.c					*****
 ****	Build an appropriate label file for WBR/WFR data	*****
 ****	files.  						*****
 ********************************************************************/
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <fg.h>

/* Cassini Stuff */
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>
#include <mp_status.h>
#include <wbr_status.h>
#include <wfr_status.h>
#include <archive.h>

/* Local Stuff */
#include "rpws_direct.h"
#include "rpws_label.h"
#include "rpws_browse.h"
#include "rpws_label_graphics.h"

#define ORBIT
#define _EPHEMERIS_TIME_
#define _ZULU_
#define _estimated_
#undef  _estimated_

static char Time_File_Name[] = { "HR_TIMES.TXT" };
extern char *Inst_Label[];
extern char *Title;
extern char *Version;
extern char Delimiter;                  /* STIM.TAB delimiter */
extern int Mafi_Flag;
char *Version_rpws_label = { "rpws_label.c 4.8a" };
static struct tm *tm_uttime;

 

/******************************************************************************************************
 ****                                                                                              ****
 ****    Label control:                                                                            ****
 ****      Consists of two items:                                                                  ****
 ****                                                                                              ****
 ****        1st. column is the control field                                                      ****
 ****            This determines which formatting step is taken to emit the line (or it may        ****
 ****            not be emitted at all).  This token determines which data item is used to         ****
 ****            fill in the variable in the label file.                                           ****
 ****            Note that some tokes do not emit data, but merely save a data field for           ****
 ****            later use (like building up filenames)                                            ****
 ****                                                                                              **** 
 ****        2nd. column is the text to be emitted                                                 ****
 ****            This text should contain the appropriate printf format specification.             ****
 ****            There is only 1 format specifier in each line.                                    ****
 ****          Include newline characters where required.                                          ****
 ******************************************************************************************************/
  
  
/* ************************************************************************************************** */
/* WBR and WFR Labels                                                                                 */
/* ************************************************************************************************** */
struct LABEL_RECORD wbr_wfr_label_record[] = {
  LABEL_TEXT_CRLF,     "PDS_VERSION_ID          = PDS3",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "/* File characteristics */",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "RECORD_TYPE             = FIXED_LENGTH",
  LABEL_RECORD_BYTES,  "RECORD_BYTES            = %d",
  LABEL_CRLF,          "%s\r\n",
  LABEL_FILE_RECORDS,  "FILE_RECORDS            = %d",
  LABEL_FILE_RECORDS_1," /* %d */",
  LABEL_CRLF,          "%s\r\n",

#ifdef COLUMNS
  LABEL_TEXT,          "/*  74    1 check   2         3         4         5         6         7     */",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT,          "/*23456789012345678901234567890123456789012345678901234567890123456789012345*/",
  LABEL_CRLF,          "%s\r\n",
#endif

  LABEL_FULLFILENAME,  "DESCRIPTION             = \"%s.DAT contains Cassini",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TYPE_D,        "    Radio and Plasma Wave (RPWS) %s data for the time period between",
  LABEL_CRLF,          "%s\r\n",
  LABEL_SCET_ST,       "    %s and ",
  LABEL_SCET_SP,       "%s that includes the",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TARGET_D_CRLF, "    following targets: %s%s",
  LABEL_IGNORE,        "                       %s%s",

#ifdef ORBIT
  LABEL_ORBIT_D_CRLF,  "    and the following orbits: %s%s",
  LABEL_IGNORE,        "                              %s%s",
#endif

  LABEL_CRLF,          "%s\r\n",

#ifdef _estimated_
  LABEL_ESTIMATED_FILE_SIZE, "ESTIMATED_FILE_SIZE     = %9d = %6d * %5d%",
  LABEL_CRLF,          "%s\r\n",
#endif

  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "/* Data object pointers */",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TYPE,          "^%s",
  LABEL_FULLFILENAME,  "_ROW_PREFIX_TABLE   = (\"%s",
  LABEL_COPYTYPE,      "%s",
  LABEL_TEXT,          ".DAT\", 1)",
  LABEL_CRLF,          "%s\r\n",
  LABEL_FULLFILENAME,  "^TIME_SERIES            = (\"%s",
  LABEL_TEXT,          ".DAT\", 1)",
  LABEL_CRLF,          "%s\r\n",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "/* Identification */",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TYPE,          "DATA_SET_ID             = \"CO-V/E/J/S/SS-RPWS-2-REFDR-%sFULL-",
  LABEL_VERSION,       "%s",
  LABEL_TEXT,          "\"",
  LABEL_CRLF,          "%s\r\n",
  LABEL_CRLF,          "DATA_SET_NAME           = \"%s\r\n",
  LABEL_TYPE,          "               CO V/E/J/S/SS RPWS 2 REFDR %s FULL RESOLUTION ",
  LABEL_VERSION,       "%s",
  LABEL_CRLF,          "\"%s\r\n",
  LABEL_FULLFILENAME,  "PRODUCT_ID              = \"%s_",
  LABEL_VERSION1,      "%s",

/*	LABEL_PAD27,					       "%s",	/**/
  LABEL_TEXT,          "\"",
  LABEL_CRLF,          "%s\r\n",
  LABEL_DATE_TODAY,    "PRODUCT_CREATION_TIME   = %s",
  LABEL_CRLF,          "%s\r\n",
  LABEL_SCET_ST,       "START_TIME              = %s",

#ifdef _ZULU_
  LABEL_TEXT,          "Z",
#endif

  LABEL_CRLF,          "%s\r\n",
  LABEL_SCET_SP,       "STOP_TIME               = %s",

#ifdef _ZULU_
  LABEL_TEXT,          "Z",
#endif

  LABEL_CRLF,          "%s\r\n",
  LABEL_SCLK_ST,       "SPACECRAFT_CLOCK_START_COUNT = \"%s\"",
  LABEL_CRLF,          "%s\r\n",
  LABEL_SCLK_SP,       "SPACECRAFT_CLOCK_STOP_COUNT  = \"%s\"",
  LABEL_CRLF,          "%s\r\n",

#ifdef _EPHEMERIS_TIME_
  LABEL_EPHEM_ST,      "NATIVE_START_TIME            = %s",
  LABEL_CRLF,          "%s\r\n",
  LABEL_EPHEM_SP,      "NATIVE_STOP_TIME             = %s",
  LABEL_CRLF,          "%s\r\n",
  LABEL_EPHEM_TEXT,    "NOTE                         = \"NATIVE_TIME is NAIF 'et' (ephemeris ",
    /**/ LABEL_CRLF,   "%s\r\n",
  LABEL_EPHEM_TEXT,    "  time or barycentric dynamical time) as used in the spice kernel.\"",
  LABEL_CRLF,          "%s\r\n",
#endif

  LABEL_TEXT_CRLF,     "PRODUCT_TYPE                 = DATA",
  LABEL_PRODUCT,       "STANDARD_DATA_PRODUCT_ID     = %s",
  LABEL_CRLF,          "%s\r\n",
  LABEL_PRODUCT_VERSION_ID, "PRODUCT_VERSION_ID           = \"%d\"",
  LABEL_CRLF,          "%s\r\n",
  LABEL_MISSION_PHASE_NAME_0_CRLF, "MISSION_PHASE_NAME           = {%s%c",
  LABEL_MISSION_PHASE_NAME_N_CRLF, "                                %s%c",
  LABEL_TARGET_0_CRLF, "TARGET_NAME                  = {%s%c",
  LABEL_TARGET_N_CRLF, "                                %s%c",
  LABEL_ORBIT_0_CRLF,  "ORBIT_NAME                   = {%s%c",
  LABEL_ORBIT_N_CRLF,  "                                %s%c",
  LABEL_NOTE,          "SOFTWARE_VERSION_ID          = \"%s %s\"",
  LABEL_CRLF,          "%s\r\n",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "/* Instrument description */",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "INSTRUMENT_HOST_NAME    = \"CASSINI ORBITER\"",
  LABEL_TEXT_CRLF,     "INSTRUMENT_HOST_ID      = CO",
  LABEL_TEXT_CRLF,     "INSTRUMENT_NAME         = \"RADIO AND PLASMA WAVE SCIENCE\"",
  LABEL_TEXT_CRLF,     "INSTRUMENT_ID           = RPWS",
  LABEL_TYPE,          "SECTION_ID              = %s",
  LABEL_CRLF,          "%s\r\n",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "/* Data Object Structure */",
  LABEL_TEXT_CRLF,     "/**************************************************************/",

/* #ifdef ARCHIVE_TIME_V1*/
  LABEL_TEXT_CRLF,     "/*      1                  32 33                 x1      x2   */",
/* #endif

#ifdef ARCHIVE_TIME_V2
  LABEL_TEXT_CRLF,     "/ *      1                  32 33                 x1      x2   * /",
#endif

#ifdef ARCHIVE_TIME_V3
  LABEL_TEXT_CRLF,     "/ *      1                  48 49                 x1      x2   * /",
#endif

#ifdef ARCHIVE_TIME_V4
  LABEL_TEXT_CRLF,     "/ *      1                  48 49                 x1      x2   * /",
#endif
*/

  LABEL_TEXT_CRLF,     "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,     "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,     "/*  1  | ROW_PREFIX_TABLE -->| TIME_SERIES ------->| SPARE |  */",
  LABEL_TEXT_CRLF,     "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,     "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,     "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,     "/*     | ROW_PREFIX_TABLE -->| TIME_SERIES ------->| SPARE |  */",
  LABEL_TEXT_CRLF,     "/*  2  |                     |                     |       |  */",
  LABEL_TEXT_CRLF,     "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,     "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,     "/*  3  | ROW_PREFIX_TABLE -->| TIME_SERIES ------->| SPARE |  */",
  LABEL_TEXT_CRLF,     "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,     "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,     "/*  . . .                                                     */",
  LABEL_TEXT_CRLF,     "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,     "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,     "/*  y  | ROW_PREFIX_TABLE -->| TIME_SERIES ------->| SPARE |  */",
  LABEL_TEXT_CRLF,     "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,     "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,     "/*                                                            */",
  LABEL_TEXT_CRLF,     "/*     8-bit octet are numbered across the top.               */",
  LABEL_TEXT_CRLF,     "/*         No header records, all records are in the          */",
  LABEL_TEXT_CRLF,     "/*         same format.                                       */",
  LABEL_TEXT_CRLF,     "/*     Record Number is down the left edge.                   */",
  LABEL_TEXT_CRLF,     "/*                                                            */",

/* #ifdef ARCHIVE_TIME_V1 */
  LABEL_TEXT_CRLF,     "/*  x1 is ROW_BYTES+32 (ITEMS)                                */",
/* #endif
#ifdef ARCHIVE_TIME_V2
  LABEL_TEXT_CRLF,     "/ *  x1 is ROW_BYTES+32 (ITEMS)                                * /",
#endif
#ifdef ARCHIVE_TIME_V3
  LABEL_TEXT_CRLF,     "/ *  x1 is ROW_BYTES+48 (ITEMS)                                * /",
#endif
#ifdef ARCHIVE_TIME_V4
  LABEL_TEXT_CRLF,     "/ *  x1 is ROW_BYTES+48 (ITEMS)                                * /",
#endif
*/

  LABEL_TEXT_CRLF,     "/*  x2 is RECORD_BYTES                                        */",
  LABEL_TEXT_CRLF,     "/*  y  is FILE_RECORDS (ROWS)                                 */",
  LABEL_TEXT_CRLF,     "/*                                                            */",
  LABEL_TEXT_CRLF,     "/**************************************************************/",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "/* Record header */",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TYPE,          "OBJECT                  = %s_ROW_PREFIX_TABLE",
  LABEL_CRLF,          "%s\r\n",

#ifdef NAMEQUOTE
  LABEL_TYPE,          "  NAME                    = \"%s_ROW_PREFIX_TABLE\"",
#else
  LABEL_TYPE,          "  NAME                    = %s_ROW_PREFIX_TABLE",
#endif

  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "  INTERCHANGE_FORMAT      = BINARY",
  LABEL_ROWS,          "  ROWS                    = %d",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "  COLUMNS                 = 19",
  LABEL_ROW_BYTES,     "  ROW_BYTES               = 32",
  LABEL_CRLF,          "%s\r\n",
  LABEL_ROW_SUFFIX_BYTES, "  ROW_SUFFIX_BYTES        = %d",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "  DESCRIPTION             = \"This table describes the structure of the",
  LABEL_TEXT_CRLF,     "    record header attached to each row of time series data.\"",
  LABEL_TEXT_CRLF,     "  ^STRUCTURE              = \"RPWS_WBR_WFR_ROW_PREFIX.FMT\"",
  LABEL_TYPE,          "END_OBJECT              = %s_ROW_PREFIX_TABLE",
  LABEL_CRLF,          "%s\r\n",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "/* Data samples */",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "OBJECT                  = TIME_SERIES",
#ifdef NAMEQUOTE
  LABEL_TYPE,          "  NAME                    = \"%s_TIME_SERIES\"",
#else
  LABEL_TYPE,          "  NAME                    = %s_TIME_SERIES",
#endif
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "  INTERCHANGE_FORMAT      = BINARY",
  LABEL_ROWS,          "  ROWS                    = %d",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "  COLUMNS                 = 1",
  LABEL_ROW_BYTES,     "  ROW_BYTES               = %d",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "  ROW_PREFIX_BYTES        = 32",
  LABEL_TEXT_CRLF,     "  SAMPLING_PARAMETER_NAME = TIME",
  LABEL_TEXT_CRLF,     "  SAMPLING_PARAMETER_UNIT = SECOND",
  LABEL_SAMPLING_PARAMETER_INTERVAL, "  SAMPLING_PARAMETER_INTERVAL = %s",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "  DESCRIPTION             = \"This time series consists of",
  LABEL_SPAN,          "    uncalibrated samples gathered during a %s hour time span from one",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "    or more detectors.  Time interval between TIME_SERIES is",
  LABEL_TEXT_CRLF,     "    variable.\"",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "  OBJECT                  = COLUMN",
#ifdef NAMEQUOTE
  LABEL_TYPE,          "    NAME                    = \"%s_SAMPLE\"",
#else
  LABEL_TYPE,          "    NAME                    = %s_SAMPLE",
#endif
  LABEL_CRLF,          "%s\r\n",
  LABEL_MSB,           "    DATA_TYPE               = %sUNSIGNED_INTEGER",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "    START_BYTE              = 33",
  LABEL_BYTES,         "    BYTES                   = %d",
  LABEL_CRLF,          "%s\r\n",
  LABEL_ITEMS,         "    ITEMS                   = %d",
  LABEL_CRLF,          "%s\r\n",
  LABEL_ITEM_BYTES,    "    ITEM_BYTES              = %d",
  LABEL_CRLF,          "%s\r\n",
  LABEL_OFFSET,        "    OFFSET                  = -%d.5",
  LABEL_CRLF,          "%s\r\n",
  LABEL_VALID_MINIMUM, "    VALID_MINIMUM           = %d",
  LABEL_CRLF,          "%s\r\n",
  LABEL_VALID_MAXIMUM, "    VALID_MAXIMUM           = %d",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT,          "    DESCRIPTION             = \"The ",
  LABEL_SAMPLE_SIZE,   "%d-bit ",
  LABEL_TEXT,          "unsigned uncalibrated",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT,          "      waveform samples range from ",
  LABEL_VALID_MINIMUM, "%d ",
  LABEL_TEXT,          "to ",
  LABEL_VALID_MAXIMUM, "%d",
  LABEL_TEXT,          ".  Zero",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT,          "      amplitude is nominally ",
  LABEL_OFFSET,        "%d.5 ",
  LABEL_TEXT,          "with ",
  LABEL_OFFSET,        "%d ",
  LABEL_TEXT,          "being just",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT,          "      below and ",
  LABEL_OFFSET1,       "%d ",
  LABEL_TEXT,          "just above zero amplitude.\"",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "  END_OBJECT              = COLUMN",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "END_OBJECT              = TIME_SERIES",
  LABEL_CRLF,          "%s\r\n",
  LABEL_TEXT_CRLF,     "END",
  LABEL_END, NULL
};



/* ************************************************************************************************** */
/* RAW                                                                                                */
/* ************************************************************************************************** */
struct LABEL_RECORD raw_label_record[] = {

  LABEL_TEXT_CRLF, "PDS_VERSION_ID          = PDS3",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* File characteristics */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "RECORD_TYPE             = UNDEFINED",
  LABEL_FILE_RECORDS, "FILE_RECORDS            = %d",
  LABEL_FILE_RECORDS_1, " /* %d */",
  LABEL_CRLF, "%s\r\n",

#ifdef COLUMNS
  LABEL_TEXT,
    "/* 326    1 check   2         3         4         5         6         7     */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "/*23456789012345678901234567890123456789012345678901234567890123456789012345*/",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_FULLFILENAME,
    "DESCRIPTION             = \"%s.PKT contains raw Cassini",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "    Radio and Plasma Wave (RPWS) telemetry data for the time period",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "    between %s and ",
  LABEL_SCET_SP, "%s that",
  LABEL_CRLF, "%s\r\n",
  LABEL_TARGET_D_CRLF,
  "    includes the following targets: %s%s",
  LABEL_IGNORE, "                                    %s%s",

#ifdef ORBIT
  LABEL_ORBIT_D_CRLF,
  "    and the following orbits: %s%s",
  LABEL_IGNORE, "                              %s%s",
#endif

  LABEL_CRLF, "%s\r\n",

  LABEL_TEXT_CRLF, "/* Data object pointers */",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "^RPWS_RAW_ROW_PREFIX_TABLE  = (\"%s",
  LABEL_TEXT, ".PKT\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "^RPWS_RAW_PACKET_TABLE      = (\"%s",
  LABEL_TEXT, ".PKT\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Identification */",
  LABEL_CRLF, "%s\r\n",
  LABEL_VERSION,
    "DATA_SET_ID             = \"CO-V/E/J/S/SS-RPWS-2-REFDR-ALL-%s",
  LABEL_TEXT, "\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "DATA_SET_NAME           = \"%s\r\n",
  LABEL_TYPE,
    "              CO V/E/J/S/SS RPWS 2 REFDR %s SCIENCE TELEMETRY ",
  LABEL_VERSION, "%s",
  LABEL_CRLF, "\"%s\r\n",
  LABEL_FULLFILENAME, "PRODUCT_ID              = \"%s_",
  LABEL_VERSION1, "%s",
  LABEL_TEXT, "\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_DATE_TODAY, "PRODUCT_CREATION_TIME   = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "START_TIME              = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_SP, "STOP_TIME               = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_ST, "SPACECRAFT_CLOCK_START_COUNT = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_SP, "SPACECRAFT_CLOCK_STOP_COUNT  = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "PRODUCT_TYPE                 = DATA",
  LABEL_PRODUCT, "STANDARD_DATA_PRODUCT_ID     = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_PRODUCT_VERSION_ID, "PRODUCT_VERSION_ID           = \"%d\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_MISSION_PHASE_NAME_0_CRLF,
  "MISSION_PHASE_NAME           = {%s%c",
  LABEL_MISSION_PHASE_NAME_N_CRLF,
  "                                %s%c",
  LABEL_TARGET_0_CRLF, "TARGET_NAME                  = {%s%c",
  LABEL_TARGET_N_CRLF, "                                %s%c",
  LABEL_ORBIT_0_CRLF, "ORBIT_NAME                   = {%s%c",
  LABEL_ORBIT_N_CRLF, "                                %s%c",
  LABEL_NOTE, "SOFTWARE_VERSION_ID          = \"%s %s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Instrument description */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_NAME    = \"CASSINI ORBITER\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_ID      = CO",
  LABEL_TEXT_CRLF,
    "INSTRUMENT_NAME         = \"RADIO AND PLASMA WAVE SCIENCE\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_ID           = RPWS",

/*	LABEL_TYPE,		"SECTION_ID              = %s",	*/

/*	LABEL_CRLF,		"%s\r\n",			*/
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Record header */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "OBJECT                  = RPWS_RAW_ROW_PREFIX_TABLE",
  LABEL_CRLF, "%s\r\n",

#ifdef NAMEQUOTE
  LABEL_TEXT, "  NAME                    = \"RPWS_RAW_ROW_PREFIX_TABLE\"",
#else
  LABEL_TEXT, "  NAME                    = RPWS_RAW_ROW_PREFIX_TABLE",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = BINARY",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 5",
  LABEL_ROW_BYTES, "  ROW_BYTES               = 268",
  LABEL_CRLF, "%s\r\n",
  LABEL_ROW_BYTES, "  START_BYTE              = 1",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  ROW_SUFFIX_BYTES        = 65536",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  DESCRIPTION             = \"ROW_SUFFIX_BYTES is",
  LABEL_TEXT_CRLF, "    variable length.  See RPWS Users Guide.",
  LABEL_TEXT_CRLF, "    This table describes the structure of the",
  LABEL_TEXT_CRLF, "    record header attached to each row of raw data.\"",
  LABEL_TEXT_CRLF, "  ^STRUCTURE              = \"RPWS_RAW_ROW_PREFIX.FMT\"",
  LABEL_TEXT, "END_OBJECT              = RPWS_RAW_ROW_PREFIX_TABLE",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data samples */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "OBJECT                  = RPWS_RAW_PACKET_TABLE",
  LABEL_CRLF, "%s\r\n",

#ifdef NAMEQUOTE
  LABEL_TEXT, "  NAME                    = \"RPWS_RAW_PACKET_TABLE\"",
#else
  LABEL_TEXT, "  NAME                    = RPWS_RAW_PACKET_TABLE",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = BINARY",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 3",
  LABEL_TEXT_CRLF, "  ROW_BYTES               = 65536",
  LABEL_TEXT_CRLF, "  START_BYTE              = 269",
  LABEL_TEXT_CRLF, "  ROW_PREFIX_BYTES        = 268",
  LABEL_TEXT_CRLF, "  DESCRIPTION             = \"Variable length and",
  LABEL_TEXT_CRLF, "    variable format table.  See RPWS Users Guide",
  LABEL_TEXT_CRLF, "    for details.  This is the RPWS logical",
  LABEL_TEXT_CRLF, "    transport packet.  Each record is the result of a",
  LABEL_TEXT_CRLF, "    complete data collection cycle of one of the ",
  LABEL_TEXT_CRLF, "    receivers within the instrument.\"",
  LABEL_TEXT_CRLF, "  ^STRUCTURE              = \"RPWS_RAW_MINIPACKET.FMT\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END_OBJECT              = RPWS_RAW_PACKET_TABLE",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END",
  LABEL_END, NULL
};


/* ************************************************************************************************** */
/* STIM                                                                                               */
/* ************************************************************************************************** */

struct LABEL_RECORD stim_label_record[] = {
  LABEL_TEXT_CRLF, "PDS_VERSION_ID          = PDS3",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* File characteristics */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "RECORD_TYPE             = FIXED_LENGTH",
  LABEL_RECORD_BYTES, "RECORD_BYTES            = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_FILE_RECORDS, "FILE_RECORDS            = %d",
  LABEL_CRLF, "%s\r\n",

#ifdef COLUMNS
  LABEL_TEXT,
    "/* 466    1 check   2         3         4         5         6         7     */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "/*23456789012345678901234567890123456789012345678901234567890123456789012345*/",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_FULLFILENAME,
    "DESCRIPTION             = \"%s.TAB contains Cassini Radio and Plasma",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "    Wave (RPWS) instrument configuration reports for the time period ",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "    between %s and ",
  LABEL_SCET_SP, "%s.\"",
  LABEL_CRLF, "%s\r\n",

#ifdef _estimated_
  LABEL_ESTIMATED_FILE_SIZE,
  "ESTIMATED_FILE_SIZE     = %9d = %6d * %5d",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data object pointers */",
  LABEL_CRLF, "%s\r\n",

  LABEL_TYPE, "^%s",
  LABEL_FULLFILENAME, "_HEADER_TABLE      = (\"%s",
  LABEL_COPYTYPE, "%s",
  LABEL_TEXT, ".TAB\", 1)",
  LABEL_CRLF, "%s\r\n",

  LABEL_TYPE, "^%s",
  LABEL_FULLFILENAME, "_TABLE             = (\"%s",
  LABEL_COPYTYPE, "%s",
  LABEL_TEXT, ".TAB\", 2)",
  LABEL_CRLF, "%s\r\n",

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Identification */",
  LABEL_CRLF, "%s\r\n",

/*
 *	DATA_SET_ID is used to build the index file.  MAKE SURE that
 *	  the first field is the one that you wist to have appear in
 *	  the index file...
 */
  LABEL_TEXT, "DATA_SET_ID             = {\"CO-V/E/J/S/SS-RPWS-4-SUMM-KEY60S",
  LABEL_VERSION, "-%s",
  LABEL_TEXT, "\",",
  LABEL_CRLF, "%s\r\n",

  LABEL_TEXT, "                           \"CO-V/E/J/S/SS-RPWS-2-REFDR-ALL",
  LABEL_VERSION, "-%s",
  LABEL_TEXT, "\",",
  LABEL_CRLF, "%s\r\n",

  LABEL_TEXT, "                           \"CO-V/E/J/S/SS-RPWS-3-RDR-LRFULL",
  LABEL_VERSION, "-%s",
  LABEL_TEXT, "\",",
  LABEL_CRLF, "%s\r\n",

  LABEL_TEXT,
    "                           \"CO-V/E/J/S/SS-RPWS-2-REFDR-WBRFULL",
  LABEL_VERSION, "-%s",
  LABEL_TEXT, "\",",
  LABEL_CRLF, "%s\r\n",

  LABEL_TEXT,
    "                           \"CO-V/E/J/S/SS-RPWS-2-REFDR-WFRFULL",
  LABEL_VERSION, "-%s",
  LABEL_TEXT, "\"}",
  LABEL_CRLF, "%s\r\n",

  LABEL_FULLFILENAME, "PRODUCT_ID              = \"RPWS_%s_",
  LABEL_VERSION1, "%s",

/*	LABEL_PAD27,					       "%s",	/**/
  LABEL_TEXT, "\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_DATE_TODAY, "PRODUCT_CREATION_TIME   = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "START_TIME              = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_SP, "STOP_TIME               = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_ST, "SPACECRAFT_CLOCK_START_COUNT = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_SP, "SPACECRAFT_CLOCK_STOP_COUNT  = \"%s\"",
  LABEL_CRLF, "%s\r\n",

#ifdef _EPHEMERIS_TIME_
  LABEL_EPHEM_ST, "NATIVE_START_TIME            = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_EPHEM_SP, "NATIVE_STOP_TIME             = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_EPHEM_TEXT,
    "NOTE                         = \"NATIVE_TIME is NAIF 'et' (ephemeris ",
    /**/ LABEL_CRLF, "%s\r\n",
  LABEL_EPHEM_TEXT,
    "  time or barycentric dynamical time) as used in the spice kernel.\"",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_TEXT_CRLF, "PRODUCT_TYPE                 = ANCILLARY",
  LABEL_PRODUCT, "STANDARD_DATA_PRODUCT_ID     = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_PRODUCT_VERSION_ID, "PRODUCT_VERSION_ID           = \"%d\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_MISSION_PHASE_NAME_0_CRLF,
  "MISSION_PHASE_NAME           = {%s%c",
  LABEL_MISSION_PHASE_NAME_N_CRLF,
  "                               %s%c",
  LABEL_TARGET_0_CRLF, "TARGET_NAME                  = {%s%c",
  LABEL_TARGET_N_CRLF, "                                %s%c",
  LABEL_ORBIT_0_CRLF, "ORBIT_NAME                   = {%s%c",
  LABEL_ORBIT_N_CRLF, "                                %s%c",
  LABEL_NOTE, "SOFTWARE_VERSION_ID          = \"%s %s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Instrument description */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_NAME    = \"CASSINI ORBITER\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_ID      = CO",
  LABEL_TEXT_CRLF,
    "INSTRUMENT_NAME         = \"RADIO AND PLASMA WAVE SCIENCE\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_ID           = \"RPWS\"",
  LABEL_TYPE, "SECTION_ID              = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF,    "/* Data Object Structure */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",
  LABEL_DELIM,        "/*     %10s delimited list of 73IEB_TRIGGER,ID events   */",
  LABEL_CRLF,         "%s\r\n",
  LABEL_TEXT_CRLF,    "/*     covering the same period as the WBRFULL/WFRFULL        */",
  LABEL_TEXT_CRLF,    "/*     datasets.  This should be compatible with any of       */",
  LABEL_TEXT_CRLF,    "/*     the commonly available spread-sheet programs.          */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     Spreadsheet column are as follows:                     */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     Column 1:  SCLK/SPICE                                  */",
  LABEL_TEXT_CRLF,    "/*         Decimal representation of Spacecraft Clock (SCLK)  */",
  LABEL_TEXT_CRLF,    "/*         as used by SPICE.  This has a partition number     */",
  LABEL_TEXT_CRLF,    "/*         and a fractional portion as such:                  */",
  LABEL_TEXT_CRLF,    "/*             partition/seconds:fraction                     */",
  LABEL_TEXT_CRLF,    "/*         The partition number starts at one and increments  */",
  LABEL_TEXT_CRLF,    "/*         if there is a discontinuity in SCLK.  The seconds  */",
  LABEL_TEXT_CRLF,    "/*         are seconds from about the 1958 epoch, and the     */",
  LABEL_TEXT_CRLF,    "/*         fraction ranges from 0 through 255.                */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     Column 2:  SCLK/HEX                                    */",
  LABEL_TEXT_CRLF,    "/*         Hexadecimal representation of Spacecraft Clock     */",
  LABEL_TEXT_CRLF,    "/*         (SCLK), c language style (leading 0x).  The        */",
  LABEL_TEXT_CRLF,    "/*         partition is implicit and the fractional           */",
  LABEL_TEXT_CRLF,    "/*         portion of the clock is expressed in RTI's         */",
  LABEL_TEXT_CRLF,    "/*         and therefor runs 0 through 7.                     */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     Column 3:  SCET                                        */",
  LABEL_TEXT_CRLF,    "/*         Spacecraft Event Time in year-day-hour-minute-     */",
  LABEL_TEXT_CRLF,    "/*         second-millisecond.                                */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     Column 4:  FSW VERSION                                 */",
  LABEL_TEXT_CRLF,    "/*         RPWS Flight Software Version Number String.        */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     Column 5:  KEY                                         */",
  LABEL_TEXT_CRLF,    "/*         Content Key, indicates that the remaining data     */",
  LABEL_TEXT_CRLF,    "/*         in the row is valid.                               */",
  LABEL_TEXT_CRLF,    "/*         STIM indicates a valid stim record.  Column 6      */",
  LABEL_TEXT_CRLF,    "/*           will be blank, and columns 7-12 contain data.    */",
  LABEL_TEXT_CRLF,    "/*         XSTM indicates an invalid stim record.             */",
  LABEL_TEXT_CRLF,    "/*           All subsequent columns are blank.                */",
  LABEL_TEXT_CRLF,    "/*         LPSW indicates a Langmuir Probe sweep.             */",
  LABEL_TEXT_CRLF,    "/*           Column 6 will contain an event duration and      */",
  LABEL_TEXT_CRLF,    "/*           columns 7 through 12 will be blank.              */",
  LABEL_TEXT_CRLF,    "/*         HSND indicates an HFR sounder sweep.               */",
  LABEL_TEXT_CRLF,    "/*           Column 6 will contain an event duration and      */",
  LABEL_TEXT_CRLF,    "/*           columns 7 through 12 will be blank.              */",
  LABEL_TEXT_CRLF,    "/*         HCAL indicates an HFR calibration cycle.           */",
  LABEL_TEXT_CRLF,    "/*           Column 6 will contain an event duration and      */",
  LABEL_TEXT_CRLF,    "/*           columns 7 through 12 will be blank.              */",
  LABEL_TEXT_CRLF,    "/*         HDMP indicates an HFR memory dump.                 */",
  LABEL_TEXT_CRLF,    "/*           columns 6 through 12 will be blank.              */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     Column 6:  DUR                                         */",
  LABEL_TEXT_CRLF,    "/*         Event Duration (LPSW, HSBD, HCAL only).  Elapsed   */",
  LABEL_TEXT_CRLF,    "/*         time, in seconds, for a noisy event.  (Following   */",
  LABEL_TEXT_CRLF,    "/*         fields are left blank).                            */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     Column 7:  STIM-SEQ                                    */",
  LABEL_TEXT_CRLF,    "/*         Stim Packet Sequence Number, usually contains 1.   */",
  LABEL_TEXT_CRLF,    "/*         RPWS nominally operates in a manner that has       */",
  LABEL_TEXT_CRLF,    "/*         this field set to 1 although other values are      */",
  LABEL_TEXT_CRLF,    "/*         legitimate.                                        */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     Column 8:  ID-0                                        */",
  LABEL_TEXT_CRLF,    "/*         ID field from STIM packet, usually ZERO.           */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     Column 9:  ID-1                                        */",
  LABEL_TEXT_CRLF,    "/*         ID field from STIM packet, contains the trigger    */",
  LABEL_TEXT_CRLF,    "/*         number that is presently executing in the lower    */",
  LABEL_TEXT_CRLF,    "/*         8 bits.  This field is a hexadecimal number        */",
  LABEL_TEXT_CRLF,    "/*         (has a leading 0x) to match the convention         */",
  LABEL_TEXT_CRLF,    "/*         used in the IEB users guide.                       */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     Column 10:  ID-2                                       */",
  LABEL_TEXT_CRLF,    "/*         ID field from STIM packet, usually ZERO.           */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     Column 11:  DAY                                        */",
  LABEL_TEXT_CRLF,    "/*         Build day.  This field contains the day-of-year    */",
  LABEL_TEXT_CRLF,    "/*         on which the STIM process (that generates these    */",
  LABEL_TEXT_CRLF,    "/*         stim packets) was build.  In other words, this     */",
  LABEL_TEXT_CRLF,    "/*         indicates which FSW version is running in the      */",
  LABEL_TEXT_CRLF,    "/*         RPWS instrument.                                   */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     Column 12:  SEQ                                        */",
  LABEL_TEXT_CRLF,    "/*         Mission Sequence String.  This is the mission      */",
  LABEL_TEXT_CRLF,    "/*         sequence number.  This string is taken from a      */",
  LABEL_TEXT_CRLF,    "/*         table, based on the SCLK from the STIM packet.     */",
  LABEL_TEXT_CRLF,    "/*       This column gives an indication of the IEB image     */",
  LABEL_TEXT_CRLF,    "/*         that is loaded into IEB memory, not the sequence   */",
  LABEL_TEXT_CRLF,    "/*         that is executing on the spacecraft (although      */",
  LABEL_TEXT_CRLF,    "/*         in most cases they are the same).  For a brief     */",
  LABEL_TEXT_CRLF,    "/*         period of time following a software load, while    */",
  LABEL_TEXT_CRLF,    "/*         the instrument is operating with an internal IEB   */",
  LABEL_TEXT_CRLF,    "/*         image, this filed would show \"BASE\" indicating     */",
  LABEL_TEXT_CRLF,    "/*         that it is operating from the internal IEB load.   */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*         rpws_label.c V4.8 20 Jan 2006                      */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Column headers */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TYPE, "OBJECT                  = %s_HEADER_TABLE",
  LABEL_CRLF, "%s\r\n",

#ifdef NAMEQUOTE
  LABEL_TYPE, "  NAME                    = \"%s_HEADER_TABLE\"",
#else
  LABEL_TYPE, "  NAME                    = %s_HEADER_TABLE",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = ASCII",
  LABEL_TEXT_CRLF, "  ROWS                    = 1",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 12",
  LABEL_TEXT_CRLF, "  ROW_BYTES               = 130",
  LABEL_TEXT_CRLF,
    "  DESCRIPTION             = \"This record contains column",
  LABEL_TEXT_CRLF, "    headers.\"",
  LABEL_TEXT_CRLF, "  ^STRUCTURE              = \"RPWS_STIM.FMT\"",
  LABEL_TYPE, "END_OBJECT              = %s_HEADER_TABLE",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* STIM Records */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TYPE, "OBJECT                  = %s_TABLE",
  LABEL_CRLF, "%s\r\n",

#ifdef NAMEQUOTE
  LABEL_TYPE, "  NAME                    = \"%s_TABLE\"",
#else
  LABEL_TYPE, "  NAME                    = %s_TABLE",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = ASCII",
  LABEL_ROWS_1, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 12",
  LABEL_TEXT_CRLF, "  ROW_BYTES               = 130",
  LABEL_TEXT_CRLF, "  DESCRIPTION             = \"STIM records.\"",
  LABEL_TEXT_CRLF, "  ^STRUCTURE              = \"RPWS_STIM.FMT\"",
  LABEL_TYPE, "END_OBJECT              = %s_TABLE",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END",

  LABEL_END, NULL
};



char *instrument[] = { "WBR",           /* 0 */
  "WFR",                                /* 1 */
  "STIM",                               /* 2 */
  "HSK",                                /* 3 */
  "HSK_DUST",                           /* 4 */
  "HSK_BFDL",                           /* 5 */
  "HSK_IPC",                            /* 6 */
  "ENGINEERING",                        /* 7 */
  "ALARM",                              /* 8 */
  "RAW",                                /* 9 */
  "",                                   /* 10 */
  "",                                   /* 11 */
  "",                                   /* 12 */
  "",                                   /* 13 */
  "",                                   /* 14 */
  "",                                   /* 15 */
  ""
};

char *instrument_d[] = { "wideband",    /* 0 */
  "waveform",                           /* 1 */
  "stimulus",                           /* 2 */
  "housekeeping",                       /* 3 */
  "dust housekeeing",                   /* 4 */
  "BFDL housekeeping",                  /* 5 */
  "IPC housekeeping",                   /* 6 */
  "",                                   /* 7 */
  "",                                   /* 8 */
  "raw telemetry",                      /* 9 */
  "",                                   /* 10 */
  "",                                   /* 11 */
  "",                                   /* 12 */
  "",                                   /* 13 */
  "",                                   /* 14 */
  "",                                   /* 15 */
  ""
};                                      /* 16 */


/* ************************************************************************************************** */
/* HOUSEKEEPING                                                                                       */
/* ************************************************************************************************** */
struct LABEL_RECORD housekeeping_label_record[] = {
  LABEL_TEXT_CRLF, "PDS_VERSION_ID          = PDS3",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* File characteristics */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "RECORD_TYPE             = FIXED_LENGTH",
  LABEL_RECORD_BYTES, "RECORD_BYTES            = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_FILE_RECORDS, "FILE_RECORDS            = %d",
  LABEL_FILE_RECORDS_1, " /* %d */",
  LABEL_CRLF, "%s\r\n",

#ifdef COLUMNS
  LABEL_TEXT,
    "/* 770    1 check   2         3         4         5         6         7     */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "/*23456789012345678901234567890123456789012345678901234567890123456789012345*/",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_FULLFILENAME, "DESCRIPTION             = \"%s.PKT contains Cassini",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "    Radio and Plasma Wave (RPWS) houskeeping data for the time period",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "    between %s and ",
  LABEL_SCET_SP, "%s.\"",
  LABEL_CRLF, "%s\r\n",

#ifdef _estimated_
  LABEL_ESTIMATED_FILE_SIZE,
  "ESTIMATED_FILE_SIZE     = %9d = %6d * %5d%",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data object pointers */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "^HSK",
  LABEL_FULLFILENAME, "_ROW_PREFIX_TABLE   = (\"%s",
  LABEL_COPYTYPE, "%s",
  LABEL_TEXT, ".PKT\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "^HOUSEKEEPING_RECORD    = (\"%s",
  LABEL_TEXT, ".PKT\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Identification */",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "PRODUCT_ID              = \"%s_",
  LABEL_VERSION1, "%s",

/*	LABEL_PAD27,					       "%s",	/**/
  LABEL_TEXT, "\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_DATE_TODAY, "PRODUCT_CREATION_TIME   = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "START_TIME              = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_SP, "STOP_TIME               = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_ST, "SPACECRAFT_CLOCK_START_COUNT = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_SP, "SPACECRAFT_CLOCK_STOP_COUNT  = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "PRODUCT_TYPE                 = HOUSEKEEPING",
  LABEL_PRODUCT, "STANDARD_DATA_PRODUCT_ID     = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_PRODUCT_VERSION_ID, "PRODUCT_VERSION_ID           = \"%d\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_MISSION_PHASE_NAME_0_CRLF,
  "MISSION_PHASE_NAME           = {%s%c",
  LABEL_MISSION_PHASE_NAME_N_CRLF,  "                                %s%c",
  LABEL_TARGET_0_CRLF, "TARGET_NAME                  = {%s%c",
  LABEL_TARGET_N_CRLF, "                                %s%c",
  LABEL_ORBIT_0_CRLF, "ORBIT_NAME                   = {%s%c",
  LABEL_ORBIT_N_CRLF, "                                %s%c",
  LABEL_NOTE, "SOFTWARE_VERSION_ID          = \"%s %s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF,         "%s\r\n",
  LABEL_TEXT_CRLF,    "/* Instrument description */",
  LABEL_CRLF,         "%s\r\n",
  LABEL_TEXT_CRLF,    "INSTRUMENT_HOST_NAME    = \"CASSINI ORBITER\"",
  LABEL_TEXT_CRLF,    "INSTRUMENT_HOST_ID      = CO",
  LABEL_TEXT_CRLF,    "INSTRUMENT_NAME         = \"RADIO AND PLASMA WAVE SCIENCE\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_ID           = RPWS",
  LABEL_TYPE, "SECTION_ID              = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF,    "/* Data Object Structure */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",

/* #ifdef ARCHIVE_TIME_V1*/
  LABEL_TEXT_CRLF,    "/*      1                  32 33                      x1/x2   */",
/* #endif
#ifdef ARCHIVE_TIME_V2
  LABEL_TEXT_CRLF,    "/ *      1                  32 33                      x1/x2   * /",
#endif
#ifdef ARCHIVE_TIME_V3
  LABEL_TEXT_CRLF,    "/ *      1                  48 49                      x1/x2   * /",
#endif
#ifdef ARCHIVE_TIME_V4
  LABEL_TEXT_CRLF,    "/ *      1                  48 49                      x1/x2   * /",
#endif
*/

  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------------+    */",
  LABEL_TEXT_CRLF,    "/*     |                     |                           |    */",
  LABEL_TEXT_CRLF,    "/*  1  | ROW_PREFIX_TABLE -->|    HOUSEKEEPING_RECORD    |    */",
  LABEL_TEXT_CRLF,    "/*     |                     |                           |    */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------------+    */",
  LABEL_TEXT_CRLF,    "/*     |                     |                           |    */",
  LABEL_TEXT_CRLF,    "/*     | ROW_PREFIX_TABLE -->|    HOUSEKEEPING_RECORD    |    */",
  LABEL_TEXT_CRLF,    "/*  2  |                     |                           |    */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------------+    */",
  LABEL_TEXT_CRLF,    "/*     |                     |                           |    */",
  LABEL_TEXT_CRLF,    "/*  3  | ROW_PREFIX_TABLE -->|    HOUSEKEEPING_RECORD    |    */",
  LABEL_TEXT_CRLF,    "/*     |                     |                           |    */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------------+    */",
  LABEL_TEXT_CRLF,    "/*  . . .                                                     */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------------+    */",
  LABEL_TEXT_CRLF,    "/*     |                     |                           |    */",
  LABEL_TEXT_CRLF,    "/*  y  | ROW_PREFIX_TABLE -->|    HOUSEKEEPING_RECORD    |    */",
  LABEL_TEXT_CRLF,    "/*     |                     |                           |    */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------------+    */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     8-bit octet are numbered across the top.               */",
  LABEL_TEXT_CRLF,    "/*         No header records, all records are in the          */",
  LABEL_TEXT_CRLF,    "/*         same format.                                       */",
  LABEL_TEXT_CRLF,    "/*     Record Number is down the left edge.                   */",
  LABEL_TEXT_CRLF,    "/*                                                            */",

/* #ifdef ARCHIVE_TIME_V1*/
  LABEL_TEXT_CRLF,    "/*  x1 is ROW_BYTES+32 (ITEMS)                                */",
/* #endif
#ifdef ARCHIVE_TIME_V2
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+32 (ITEMS)                                * /",
#endif
#ifdef ARCHIVE_TIME_V3
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+48 (ITEMS)                                * /",
#endif
#ifdef ARCHIVE_TIME_V4
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+48 (ITEMS)                                * /",
#endif
*/
  LABEL_TEXT_CRLF,    "/*  x2 is RECORD_BYTES                                        */",
  LABEL_TEXT_CRLF,    "/*    (x1/x2 are the same, no trailing fill)                  */",
  LABEL_TEXT_CRLF,    "/*  y  is FILE_RECORDS (ROWS)                                 */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",
  LABEL_CRLF,         "%s\r\n",
  LABEL_TEXT_CRLF,    "/* Record header */",
  LABEL_CRLF,         "%s\r\n",
  LABEL_TEXT,         "OBJECT                  = HSK_ROW_PREFIX_TABLE",
  LABEL_CRLF,         "%s\r\n",
#ifdef NAMEQUOTE
  LABEL_TEXT,         "  NAME                    = \"HSK_ROW_PREFIX_TABLE\"",
#else
  LABEL_TEXT,         "  NAME                    = HSK_ROW_PREFIX_TABLE",
#endif
  LABEL_CRLF,         "%s\r\n",
  LABEL_TEXT_CRLF,    "  INTERCHANGE_FORMAT      = BINARY",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 7",
  LABEL_ROW_BYTES, "  ROW_BYTES               = 32",
  LABEL_CRLF, "%s\r\n",
  LABEL_ROW_SUFFIX_BYTES, "  ROW_SUFFIX_BYTES        = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF,
    "  DESCRIPTION             = \"This table describes the structure of the",
  LABEL_TEXT_CRLF,
    "    record header attached to each row of housekeeping data.\"",
  LABEL_TEXT_CRLF, "  ^STRUCTURE              = \"RPWS_HSK_ROW_PREFIX.FMT\"",
  LABEL_TEXT, "END_OBJECT              = HSK_ROW_PREFIX_TABLE",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data samples */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "OBJECT                  = HOUSEKEEPING_RECORD",
#ifdef NAMEQUOTE
  LABEL_TEXT, "  NAME                    = \"HOUSEKEEPING_RECORD\"",
#else
  LABEL_TEXT, "  NAME                    = HOUSEKEEPING_RECORD",
#endif
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = BINARY",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 24",
  LABEL_ROW_BYTES, "  ROW_BYTES               = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "  ROW_PREFIX_BYTES        = 32",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  DESCRIPTION             = \"See RPWS Users Guide",
  LABEL_TEXT, "    for details of the contents of these records.\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "  ^STRUCTURE              = \"RPWS_HOUSEKEEPING.FMT\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END_OBJECT              = HOUSEKEEPING_RECORD",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END",
  LABEL_END, NULL
};

/* ************************************************************************************************** */
/* DUST                                                                                               */
/* ************************************************************************************************** */
struct LABEL_RECORD dust_label_record[] = {
  LABEL_TEXT_CRLF, "PDS_VERSION_ID          = PDS3",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* File characteristics */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "RECORD_TYPE             = FIXED_LENGTH",
  LABEL_RECORD_BYTES, "RECORD_BYTES            = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_FILE_RECORDS, "FILE_RECORDS            = %d",
  LABEL_FILE_RECORDS_1, " /* %d */",
  LABEL_CRLF, "%s\r\n",

#ifdef COLUMNS
  LABEL_TEXT,
    "/* 961    1         2         3         4         5         6         7     */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "/*23456789012345678901234567890123456789012345678901234567890123456789012345*/",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_FULLFILENAME, "DESCRIPTION             = \"%s.PKT contains Cassini",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "    Radio and Plasma Wave (RPWS) Dust Housekeeping for the time period",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "    between %s and ",
  LABEL_SCET_SP, "%s.\"",
  LABEL_CRLF, "%s\r\n",

#ifdef _estimated_
  LABEL_ESTIMATED_FILE_SIZE,
  "ESTIMATED_FILE_SIZE     = %9d = %6d * %5d%",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data object pointers */",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "^DUST_HSK_ROW_PREFIX_TABLE = (\"%s",
  LABEL_COPYTYPE, "%s",
  LABEL_TEXT, ".PKT\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "^DUST_HSK_RECORD           = (\"%s",
  LABEL_TEXT, ".PKT\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Identification */",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "PRODUCT_ID              = \"%s_",
  LABEL_VERSION1, "%s",

/*	LABEL_PAD27,					       "%s",	/**/
  LABEL_TEXT, "\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_DATE_TODAY, "PRODUCT_CREATION_TIME   = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "START_TIME              = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_SP, "STOP_TIME               = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_ST, "SPACECRAFT_CLOCK_START_COUNT = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_SP, "SPACECRAFT_CLOCK_STOP_COUNT  = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "PRODUCT_TYPE                 = HOUSEKEEPING",
  LABEL_PRODUCT, "STANDARD_DATA_PRODUCT_ID     = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_PRODUCT_VERSION_ID, "PRODUCT_VERSION_ID           = \"%d\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_MISSION_PHASE_NAME_0_CRLF,
  "MISSION_PHASE_NAME           = {%s%c",
  LABEL_MISSION_PHASE_NAME_N_CRLF,
  "                                %s%c",
  LABEL_TARGET_0_CRLF, "TARGET_NAME                  = {%s%c",
  LABEL_TARGET_N_CRLF, "                                %s%c",
  LABEL_ORBIT_0_CRLF, "ORBIT_NAME                   = {%s%c",
  LABEL_ORBIT_N_CRLF, "                                %s%c",
  LABEL_NOTE, "SOFTWARE_VERSION_ID          = \"%s %s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Instrument description */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_NAME    = \"CASSINI ORBITER\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_ID      = CO",
  LABEL_TEXT_CRLF,
    "INSTRUMENT_NAME         = \"RADIO AND PLASMA WAVE SCIENCE\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_ID           = RPWS",
  LABEL_TYPE, "SECTION_ID              = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data Object Structure */",
  LABEL_TEXT_CRLF,
    "/**************************************************************/",

/* #ifdef ARCHIVE_TIME_V1 */
  LABEL_TEXT_CRLF,
    "/*      1                  32 33                 x1      x2   */",
/* #endif

#ifdef ARCHIVE_TIME_V2
  LABEL_TEXT_CRLF,
    "/ *      1                  32 33                 x1      x2   * /",
#endif

#ifdef ARCHIVE_TIME_V3
  LABEL_TEXT_CRLF,
    "/ *      1                  48 49                 x1      x2   * /",
#endif

#ifdef ARCHIVE_TIME_V4
  LABEL_TEXT_CRLF,
    "/ *      1                  48 49                 x1      x2   * /",
#endif
*/

  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*  1  | ROW_PREFIX_TABLE -->| HSK_DUST_RECORD --->| SPARE |  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     | ROW_PREFIX_TABLE -->| HSK_DUST_RECORD --->| SPARE |  */",
  LABEL_TEXT_CRLF,    "/*  2  |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*  3  | ROW_PREFIX_TABLE -->| HSK_DUST_RECORD --->| SPARE |  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*  . . .                                                     */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*  y  | ROW_PREFIX_TABLE -->| HSK_DUST_RECORD --->| SPARE |  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     8-bit octet are numbered across the top.               */",
  LABEL_TEXT_CRLF,    "/*         No header records, all records are in the          */",
  LABEL_TEXT_CRLF,    "/*         same format.                                       */",
  LABEL_TEXT_CRLF,    "/*     Record Number is down the left edge.                   */",
  LABEL_TEXT_CRLF,    "/*                                                            */",

/* #ifdef ARCHIVE_TIME_V1*/
  LABEL_TEXT_CRLF,    "/*  x1 is ROW_BYTES+32 (ITEMS)                                */",
/*#endif

#ifdef ARCHIVE_TIME_V2
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+32 (ITEMS)                                * /",
#endif

#ifdef ARCHIVE_TIME_V3
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+48 (ITEMS)                                * /",
#endif

#ifdef ARCHIVE_TIME_V4
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+48 (ITEMS)                                * /",
#endif
*/

  LABEL_TEXT_CRLF,    "/*  x2 is RECORD_BYTES                                        */",
  LABEL_TEXT_CRLF,    "/*  y  is FILE_RECORDS (ROWS)                                 */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Record header */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "OBJECT                  = DUST_HSK_ROW_PREFIX_TABLE",
  LABEL_CRLF, "%s\r\n",

#ifdef NAMEQUOTE
  LABEL_TEXT, "  NAME                    = \"DUST_HSK_ROW_PREFIX_TABLE\"",
#else
  LABEL_TEXT, "  NAME                    = DUST_HSK_ROW_PREFIX_TABLE",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = BINARY",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 7",
  LABEL_ROW_BYTES, "  ROW_BYTES               = 32",
  LABEL_CRLF, "%s\r\n",
  LABEL_ROW_SUFFIX_BYTES, "  ROW_SUFFIX_BYTES        = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF,
    "  DESCRIPTION             = \"This table describes the structure of the",
  LABEL_TEXT_CRLF,
    "    record header attached to each row of housekeeping data.\"",
  LABEL_TEXT_CRLF, "  ^STRUCTURE              = \"RPWS_HSK_ROW_PREFIX.FMT\"",
  LABEL_TEXT, "END_OBJECT              = DUST_HSK_ROW_PREFIX_TABLE",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data samples */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "OBJECT                  = DUST_HSK_DUST_RECORD",

#ifdef NAMEQUOTE
  LABEL_TEXT, "  NAME                    = \"DUST_HSK_DUST_RECORD\"",
#else
  LABEL_TEXT, "  NAME                    = DUST_HSK_DUST_RECORD",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = BINARY",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 12",
  LABEL_ROW_BYTES, "  ROW_BYTES               = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "  ROW_PREFIX_BYTES        = 32",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  DESCRIPTION             = \"See RPWS Users Guide",
  LABEL_TEXT, "    for details of the contents of these records.\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "  ^STRUCTURE              = \"RPWS_HSK_DUST.FMT\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END_OBJECT              = DUST_HSK_DUST_RECORD",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END",
  LABEL_END, NULL
};


/* ************************************************************************************************** */
/* BFDL HOUSEKEEPING                                                                                  */
/* ************************************************************************************************** */

struct LABEL_RECORD bfdl_label_record[] = {
  LABEL_TEXT_CRLF, "PDS_VERSION_ID          = PDS3",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* File characteristics */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "RECORD_TYPE             = FIXED_LENGTH",
  LABEL_RECORD_BYTES, "RECORD_BYTES            = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_FILE_RECORDS, "FILE_RECORDS            = %d",
  LABEL_FILE_RECORDS_1, " /* %d */",
  LABEL_CRLF, "%s\r\n",

#ifdef COLUMNS
  LABEL_TEXT,
    "/*1151    1         2         3         4         5         6         7     */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "/*23456789012345678901234567890123456789012345678901234567890123456789012345*/",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_FULLFILENAME, "DESCRIPTION             = \"%s.PKT contains Cassini",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "    Radio and Plasma Wave (RPWS) BFDL housekeeping for the time period",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "     between %s and ",
  LABEL_SCET_SP, "%s.\"",

#ifdef _estimated_
  LABEL_ESTIMATED_FILE_SIZE,
  "ESTIMATED_FILE_SIZE     = %9d = %6d * %5d%",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data object pointers */",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "^BFDL_HSK_ROW_PREFIX_TABLE = (\"%s",
  LABEL_COPYTYPE, "%s",
  LABEL_TEXT, ".PKT\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "^BFDL_HSK_RECORD           = (\"%s",
  LABEL_TEXT, ".PKT\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Identification */",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "PRODUCT_ID              = \"%s_",
  LABEL_VERSION1, "%s",

/*	LABEL_PAD27,					       "%s",	/**/
  LABEL_TEXT, "\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_DATE_TODAY, "PRODUCT_CREATION_TIME   = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "START_TIME              = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_SP, "STOP_TIME               = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_ST, "SPACECRAFT_CLOCK_START_COUNT = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_SP, "SPACECRAFT_CLOCK_STOP_COUNT  = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "PRODUCT_TYPE                 = HOUSEKEEPING",
  LABEL_PRODUCT, "STANDARD_DATA_PRODUCT_ID     = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_PRODUCT_VERSION_ID, "PRODUCT_VERSION_ID           = \"%d\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_MISSION_PHASE_NAME_0_CRLF,
  "MISSION_PHASE_NAME           = {%s%c",
  LABEL_MISSION_PHASE_NAME_N_CRLF,
  "                                %s%c",
  LABEL_TARGET_0_CRLF, "TARGET_NAME                  = {%s%c",
  LABEL_TARGET_N_CRLF, "                                %s%c",
  LABEL_ORBIT_0_CRLF, "ORBIT_NAME                   = {%s%c",
  LABEL_ORBIT_N_CRLF, "                                %s%c",
  LABEL_NOTE, "SOFTWARE_VERSION_ID          = \"%s %s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Instrument description */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_NAME    = \"CASSINI ORBITER\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_ID      = CO",
  LABEL_TEXT_CRLF,
    "INSTRUMENT_NAME         = \"RADIO AND PLASMA WAVE SCIENCE\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_ID           = RPWS",
  LABEL_TYPE, "SECTION_ID              = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data Object Structure */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",

/* #ifdef ARCHIVE_TIME_V1*/
  LABEL_TEXT_CRLF,    "/*      1                  32 33                 x1      x2   */",
/*#endif
#ifdef ARCHIVE_TIME_V2
  LABEL_TEXT_CRLF,    "/ *      1                  32 33                 x1      x2   * /",
#endif
#ifdef ARCHIVE_TIME_V3
  LABEL_TEXT_CRLF,    "/ *      1                  48 49                 x1      x2   * /",
#endif
#ifdef ARCHIVE_TIME_V4
  LABEL_TEXT_CRLF,    "/ *      1                  48 49                 x1      x2   * /",
#endif
*/

  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*  1  | ROW_PREFIX_TABLE -->| HSK_BFDL_RECORD --->| SPARE |  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     | ROW_PREFIX_TABLE -->| HSK_BFDL_RECORD --->| SPARE |  */",
  LABEL_TEXT_CRLF,    "/*  2  |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*  3  | ROW_PREFIX_TABLE -->| HSK_BFDL_RECORD --->| SPARE |  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*  . . .                                                     */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*  y  | ROW_PREFIX_TABLE -->| HSK_BFDL_RECORD --->| SPARE |  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     8-bit octet are numbered across the top.               */",
  LABEL_TEXT_CRLF,    "/*         No header records, all records are in the          */",
  LABEL_TEXT_CRLF,    "/*         same format.                                       */",
  LABEL_TEXT_CRLF,    "/*     Record Number is down the left edge.                   */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
/* #ifdef ARCHIVE_TIME_V1*/
  LABEL_TEXT_CRLF,    "/*  x1 is ROW_BYTES+32 (ITEMS)                                */",
/* #endif
#ifdef ARCHIVE_TIME_V2
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+32 (ITEMS)                                * /",
#endif
#ifdef ARCHIVE_TIME_V3
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+48 (ITEMS)                                * /",
#endif
#ifdef ARCHIVE_TIME_V4
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+48 (ITEMS)                                * /",
#endif
*/
  LABEL_TEXT_CRLF,    "/*  x2 is RECORD_BYTES                                        */",
  LABEL_TEXT_CRLF,    "/*  y  is FILE_RECORDS (ROWS)                                 */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Record header */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "OBJECT                  = RPWS_HSK_ROW_PREFIX_TABLE",
  LABEL_CRLF, "%s\r\n",

#ifdef NAMEQUOTE
  LABEL_TEXT, "  NAME                    = \"RPWS_HSK_ROW_PREFIX_TABLE\"",
#else
  LABEL_TEXT, "  NAME                    = RPWS_HSK_ROW_PREFIX_TABLE",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = BINARY",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 7",
  LABEL_ROW_BYTES, "  ROW_BYTES               = 32",
  LABEL_CRLF, "%s\r\n",
  LABEL_ROW_SUFFIX_BYTES, "  ROW_SUFFIX_BYTES        = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF,
    "  DESCRIPTION             = \"This table describes the structure of the",
  LABEL_TEXT_CRLF,
    "    record header attached to each row of housekeeping data.\"",
  LABEL_TEXT_CRLF, "  ^STRUCTURE              = \"RPWS_HSK_ROW_PREFIX.FMT\"",
  LABEL_TEXT, "END_OBJECT              = RPWS_HSK_ROW_PREFIX_TABLE",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data samples */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "OBJECT                  = RPWS_HSK_BFDL_RECORD",

#ifdef NAMEQUOTE
  LABEL_TEXT, "  NAME                    = \"RPWS_HSK_BFDL_RECORD\"",
#else
  LABEL_TEXT, "  NAME                    = RPWS_HSK_BFDL_RECORD",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = BINARY",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 12",
  LABEL_ROW_BYTES, "  ROW_BYTES               = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "  ROW_PREFIX_BYTES        = 32",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  DESCRIPTION             = \"See RPWS Users Guide",
  LABEL_TEXT, "    for details of the contents of these records.\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "  ^STRUCTURE              = \"RPWS_HSK_BFDL.FMT\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END_OBJECT              = RPWS_HSK_BFDL_RECORD",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END",
  LABEL_END, NULL
};

/* ************************************************************************************************** */
/* IPC HOUSEKEEPING                                                                                   */
/* ************************************************************************************************** */

struct LABEL_RECORD ipc_label_record[] = {
  LABEL_TEXT_CRLF, "PDS_VERSION_ID          = PDS3",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* File characteristics */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "RECORD_TYPE             = FIXED_LENGTH",
  LABEL_RECORD_BYTES, "RECORD_BYTES            = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_FILE_RECORDS, "FILE_RECORDS            = %d",
  LABEL_FILE_RECORDS_1, " /* %d */",
  LABEL_CRLF, "%s\r\n",

#ifdef COLUMNS
  LABEL_TEXT,
    "/*1339    1         2         3         4         5         6         7     */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "/*23456789012345678901234567890123456789012345678901234567890123456789012345*/",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_FULLFILENAME, "DESCRIPTION             = \"%s.PKT contains Cassini",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "    Radio and Plasma Wave (RPWS) IPC housekeeping for the time period",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "    between %s and ",
  LABEL_SCET_SP, "%s.\"",
  LABEL_CRLF, "%s\r\n",

#ifdef _estimated_
  LABEL_ESTIMATED_FILE_SIZE,
  "ESTIMATED_FILE_SIZE     = %9d = %6d * %5d%",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data object pointers */",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "^IPC_HSK_ROW_PREFIX_TABLE  = (\"%s",
  LABEL_COPYTYPE, "%s",
  LABEL_TEXT, ".PKT\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "^IPC_HSK_RECORD            = (\"%s",
  LABEL_TEXT, ".PKT\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Identification */",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "PRODUCT_ID              = \"%s_",
  LABEL_VERSION1, "%s",

/*	LABEL_PAD27,					       "%s",	/**/
  LABEL_TEXT, "\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_DATE_TODAY, "PRODUCT_CREATION_TIME   = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "START_TIME              = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_SP, "STOP_TIME               = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_ST, "SPACECRAFT_CLOCK_START_COUNT = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_SP, "SPACECRAFT_CLOCK_STOP_COUNT  = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "PRODUCT_TYPE                 = HOUSEKEEPING",
  LABEL_PRODUCT, "STANDARD_DATA_PRODUCT_ID     = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_PRODUCT_VERSION_ID, "PRODUCT_VERSION_ID           = \"%d\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_MISSION_PHASE_NAME_0_CRLF,
  "MISSION_PHASE_NAME           = {%s%c",
  LABEL_MISSION_PHASE_NAME_N_CRLF,
  "                                %s%c",
  LABEL_TARGET_0_CRLF, "TARGET_NAME                  = {%s%c",
  LABEL_TARGET_N_CRLF, "                                %s%c",
  LABEL_ORBIT_0_CRLF, "ORBIT_NAME                   = {%s%c",
  LABEL_ORBIT_N_CRLF, "                                %s%c",
  LABEL_NOTE, "SOFTWARE_VERSION_ID          = \"%s %s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Instrument description */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_NAME    = \"CASSINI ORBITER\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_ID      = CO",
  LABEL_TEXT_CRLF,
    "INSTRUMENT_NAME         = \"RADIO AND PLASMA WAVE SCIENCE\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_ID           = RPWS",
  LABEL_TYPE, "SECTION_ID              = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data Object Structure */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",

/* #ifdef ARCHIVE_TIME_V1 */
  LABEL_TEXT_CRLF,    "/*      1                  32 33                 x1      x2   */",
/* #endif

#ifdef ARCHIVE_TIME_V2
  LABEL_TEXT_CRLF,    "/ *      1                  32 33                 x1      x2   * /",
#endif

#ifdef ARCHIVE_TIME_V3
  LABEL_TEXT_CRLF,    "/ *      1                  48 49                 x1      x2   * /",
#endif

#ifdef ARCHIVE_TIME_V4
  LABEL_TEXT_CRLF,    "/ *      1                  48 49                 x1      x2   * /",
#endif
*/
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*  1  | ROW_PREFIX_TABLE -->| HSK_IPC_RECORD ---->| SPARE |  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     | ROW_PREFIX_TABLE -->| HSK_IPC_RECORD ---->| SPARE |  */",
  LABEL_TEXT_CRLF,    "/*  2  |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*  3  | ROW_PREFIX_TABLE -->| HSK_IPC_RECORD ---->| SPARE |  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*  . . .                                                     */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*  y  | ROW_PREFIX_TABLE -->| HSK_IPC_RECORD ---->| SPARE |  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     8-bit octet are numbered across the top.               */",
  LABEL_TEXT_CRLF,    "/*         No header records, all records are in the          */",
  LABEL_TEXT_CRLF,    "/*         same format.                                       */",
  LABEL_TEXT_CRLF,    "/*     Record Number is down the left edge.                   */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
/* #ifdef ARCHIVE_TIME_V1 */
  LABEL_TEXT_CRLF,    "/*  x1 is ROW_BYTES+32 (ITEMS)                                */",
/* #endif
#ifdef ARCHIVE_TIME_V2
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+32 (ITEMS)                                * /",
#endif
#ifdef ARCHIVE_TIME_V3
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+48 (ITEMS)                                * /",
#endif
#ifdef ARCHIVE_TIME_V4
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+48 (ITEMS)                                * /",
#endif
*/
  LABEL_TEXT_CRLF,    "/*  x2 is RECORD_BYTES                                        */",
  LABEL_TEXT_CRLF,    "/*  y  is FILE_RECORDS (ROWS)                                 */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Record header */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "OBJECT                  = RPWS_HSK_ROW_PREFIX_TABLE",
  LABEL_CRLF, "%s\r\n",
#ifdef NAMEQUOTE
  LABEL_TEXT, "  NAME                    = \"RPWS_HSK_ROW_PREFIX_TABLE\"",
#else
  LABEL_TEXT, "  NAME                    = RPWS_HSK_ROW_PREFIX_TABLE",
#endif
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = BINARY",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 7",
  LABEL_ROW_BYTES, "  ROW_BYTES               = 32",
  LABEL_CRLF, "%s\r\n",
  LABEL_ROW_SUFFIX_BYTES, "  ROW_SUFFIX_BYTES        = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF,    "  DESCRIPTION             = \"This table describes the structure of the",
  LABEL_TEXT_CRLF,    "    record header attached to each row of housekeeping data.\"",
  LABEL_TEXT_CRLF,    "  ^STRUCTURE              = \"RPWS_HSK_ROW_PREFIX.FMT\"",
  LABEL_TEXT, "END_OBJECT              = RPWS_HSK_ROW_PREFIX_TABLE",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data samples */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "OBJECT                  = RPWS_HSK_IPC_RECORD",
#ifdef NAMEQUOTE
  LABEL_TEXT, "  NAME                    = \"RPWS_HSK_IPC_RECORD\"",
#else
  LABEL_TEXT, "  NAME                    = RPWS_HSK_IPC_RECORD",
#endif
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = BINARY",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 12",
  LABEL_ROW_BYTES, "  ROW_BYTES               = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "  ROW_PREFIX_BYTES        = 32",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  DESCRIPTION             = \"See RPWS Users Guide",
  LABEL_TEXT, "    for details of the contents of these records.\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "  ^STRUCTURE              = \"RPWS_HSK_IPC.FMT\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END_OBJECT              = RPWS_HSK_IPC_RECORD",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END",
  LABEL_END, NULL
};


/* ************************************************************************************************** */
/* ENGINEERING                                                                                        */
/* ************************************************************************************************** */
struct LABEL_RECORD eng_label_record[] = {
  LABEL_TEXT_CRLF, "PDS_VERSION_ID          = PDS3",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* File characteristics */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "RECORD_TYPE             = FIXED_LENGTH",
  LABEL_BYTES, "RECORD_BYTES            = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_FILE_RECORDS, "FILE_RECORDS            = %d",
  LABEL_FILE_RECORDS_1, " /* %d */",
  LABEL_CRLF, "%s\r\n",

#ifdef COLUMNS
  LABEL_TEXT,
    "/*1339    1         2         3         4         5         6         7     */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "/*23456789012345678901234567890123456789012345678901234567890123456789012345*/",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_FULLFILENAME, "DESCRIPTION             = \"%s.TAB contains Cassini",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "    Radio and Plasma Wave (RPWS) Engineering values for the time period",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "    between %s and ",
  LABEL_SCET_SP, "%s.\"",
  LABEL_CRLF, "%s\r\n",

#ifdef _estimated_
  LABEL_ESTIMATED_FILE_SIZE,
  "ESTIMATED_FILE_SIZE     = %9d = %6d * %5d%",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data object pointers */",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "^ENG_TIME                  = (\"%s",
  LABEL_COPYTYPE, "%s",
  LABEL_TEXT, ".TAB\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "^ENG_RECORD                = (\"%s",
  LABEL_TEXT, ".TAB\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Identification */",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "PRODUCT_ID              = \"%s_",
  LABEL_VERSION1, "%s",

/*	LABEL_PAD27,					       "%s",	/**/
  LABEL_TEXT, "\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_DATE_TODAY, "PRODUCT_CREATION_TIME   = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "START_TIME              = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_SP, "STOP_TIME               = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_ST, "SPACECRAFT_CLOCK_START_COUNT = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_SP, "SPACECRAFT_CLOCK_STOP_COUNT  = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "PRODUCT_TYPE                 = HOUSEKEEPING",
  LABEL_PRODUCT, "STANDARD_DATA_PRODUCT_ID     = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_PRODUCT_VERSION_ID, "PRODUCT_VERSION_ID           = \"%d\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_MISSION_PHASE_NAME_0_CRLF,
  "MISSION_PHASE_NAME           = {%s%c",
  LABEL_MISSION_PHASE_NAME_N_CRLF,
  "                                %s%c",
  LABEL_TARGET_0_CRLF, "TARGET_NAME                  = {%s%c",
  LABEL_TARGET_N_CRLF, "                                %s%c",
  LABEL_ORBIT_0_CRLF, "ORBIT_NAME                   = {%s%c",
  LABEL_ORBIT_N_CRLF, "                                %s%c",
  LABEL_NOTE, "SOFTWARE_VERSION_ID          = \"%s %s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Instrument description */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_NAME    = \"CASSINI ORBITER\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_ID      = CO",
  LABEL_TEXT_CRLF,
    "INSTRUMENT_NAME         = \"RADIO AND PLASMA WAVE SCIENCE\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_ID           = RPWS",
  LABEL_TYPE, "SECTION_ID              = %s", /**/ LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data Object Structure */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",
  LABEL_TEXT_CRLF,    "/*      1                  57 59                 x1      x2   */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*  1  | TIME_PREFIX -->     | ENG_RECORD ---->    | SPARE |  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     | TIME_PREFIX -->     | ENG_RECORD ---->    | SPARE |  */",
  LABEL_TEXT_CRLF,    "/*  2  |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*  3  | TIME_PREFIX -->     | ENG_RECORD ---->    | SPARE |  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*  . . .                                                     */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*  y  | TIME_PREFIX -->     | ENG_RECORD ---->    | SPARE |  */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |       |  */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+-------+  */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     8-bit octet are numbered across the top.               */",
  LABEL_TEXT_CRLF,    "/*         No header records, all records are in the          */",
  LABEL_TEXT_CRLF,    "/*         same format.                                       */",
  LABEL_TEXT_CRLF,    "/*     Record Number is down the left edge.                   */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
/* #ifdef ARCHIVE_TIME_V1 */
  LABEL_TEXT_CRLF,    "/*  x1 is ROW_BYTES+32 (ITEMS)                                */",
/* #endif
#ifdef ARCHIVE_TIME_V2
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+32 (ITEMS)                                * /",
#endif
#ifdef ARCHIVE_TIME_V3
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+48 (ITEMS)                                * /",
#endif
#ifdef ARCHIVE_TIME_V4
  LABEL_TEXT_CRLF,    "/ *  x1 is ROW_BYTES+48 (ITEMS)                                * /",
#endif
*/
  LABEL_TEXT_CRLF,    "/*  x2 is RECORD_BYTES                                        */",
  LABEL_TEXT_CRLF,    "/*  y  is FILE_RECORDS (ROWS)                                 */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Record header */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "OBJECT                  = RPWS_ENG_TIME",
  LABEL_CRLF, "%s\r\n",

#ifdef NAMEQUOTE
  LABEL_TEXT, "  NAME                    = \"RPWS_ENG_TIME\"",
#else
  LABEL_TEXT, "  NAME                    = RPWS_ENG_TIME",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = ASCII",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 3",
  LABEL_ROW_PREFIX_BYTES, "  ROW_BYTES               = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_ROW_REMAIN_BYTES, "  ROW_SUFFIX_BYTES        = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  DESCRIPTION             = \"Time tags, 3 formats.",
  LABEL_TEXT, "    PDS SCLK, HEX SCLK, and string\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  ^STRUCTURE              = \"RPWS_ENG_TIME.FMT\"",
  LABEL_TEXT, "END_OBJECT              = RPWS_ENG_TIME",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data samples */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "OBJECT                  = RPWS_ENG_RECORD",

#ifdef NAMEQUOTE
  LABEL_TEXT, "  NAME                    = \"RPWS_ENG_RECORD\"",
#else
  LABEL_TEXT, "  NAME                    = RPWS_ENG_RECORD",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = ASCII",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 22",
  LABEL_ROW_REMAIN_BYTES, "  ROW_BYTES               = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_ROW_PREFIX_BYTES, "  ROW_PREFIX_BYTES        = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF,
    "  DESCRIPTION             = \"Engineering data, converted to",
  LABEL_TEXT, "    Volts, Current, Temperature or Percent\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "  ^STRUCTURE              = \"RPWS_ENG.FMT\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END_OBJECT              = RPWS_ENG_RECORD",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END",
  LABEL_END, NULL
};


/* ************************************************************************************************** */
/* ENGINEERING ALARM EVENTS                                                                           */
/* ************************************************************************************************** */

struct LABEL_RECORD alarm_label_record[] = {
  LABEL_TEXT_CRLF, "PDS_VERSION_ID          = PDS3",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* File characteristics */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "RECORD_TYPE             = STREAM",
  LABEL_CRLF, "%s\r\n",
  LABEL_FILE_RECORDS, "FILE_RECORDS            = %d",
  LABEL_FILE_RECORDS_1, " /* %d */",
  LABEL_CRLF, "%s\r\n",

#ifdef COLUMNS
  LABEL_TEXT,
    "/*1339    1         2         3         4         5         6         7     */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "/*23456789012345678901234567890123456789012345678901234567890123456789012345*/",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_FULLFILENAME, "DESCRIPTION             = \"%s.TXT contains Cassini",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT,
    "    Radio and Plasma Wave (RPWS) Engineering Alarm events for the time",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "    period between %s and ",
  LABEL_SCET_SP, "%s.\"",
  LABEL_CRLF, "%s\r\n",

#ifdef _estimated_
  LABEL_ESTIMATED_FILE_SIZE,
  "ESTIMATED_FILE_SIZE     = %9d = %6d * %5d%",
  LABEL_CRLF, "%s\r\n",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data object pointers */",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "^ALARM_TIME                  = (\"%s",
  LABEL_COPYTYPE, "%s",
  LABEL_TEXT, ".TXT\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "^ALARM_RECORD                = (\"%s",
  LABEL_TEXT, ".TXT\", 1)",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Identification */",
  LABEL_CRLF, "%s\r\n",
  LABEL_FULLFILENAME, "PRODUCT_ID              = \"%s_",
  LABEL_VERSION1, "%s",

/*	LABEL_PAD27,					       "%s",	/**/
  LABEL_TEXT, "\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_DATE_TODAY, "PRODUCT_CREATION_TIME   = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_ST, "START_TIME              = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCET_SP, "STOP_TIME               = %s",

#ifdef _ZULU_
  LABEL_TEXT, "Z",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_ST, "SPACECRAFT_CLOCK_START_COUNT = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_SCLK_SP, "SPACECRAFT_CLOCK_STOP_COUNT  = \"%s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "PRODUCT_TYPE                 = HOUSEKEEPING",
  LABEL_PRODUCT, "STANDARD_DATA_PRODUCT_ID     = %s",
  LABEL_CRLF, "%s\r\n",
  LABEL_PRODUCT_VERSION_ID, "PRODUCT_VERSION_ID           = \"%d\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_MISSION_PHASE_NAME_0_CRLF,
  "MISSION_PHASE_NAME           = {%s%c",
  LABEL_MISSION_PHASE_NAME_N_CRLF,
  "                                %s%c",
  LABEL_TARGET_0_CRLF, "TARGET_NAME                  = {%s%c",
  LABEL_TARGET_N_CRLF, "                                %s%c",
  LABEL_ORBIT_0_CRLF, "ORBIT_NAME                   = {%s%c",
  LABEL_ORBIT_N_CRLF, "                                %s%c",
  LABEL_NOTE, "SOFTWARE_VERSION_ID          = \"%s %s\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Instrument description */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_NAME    = \"CASSINI ORBITER\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_HOST_ID      = CO",
  LABEL_TEXT_CRLF,
    "INSTRUMENT_NAME         = \"RADIO AND PLASMA WAVE SCIENCE\"",
  LABEL_TEXT_CRLF, "INSTRUMENT_ID           = RPWS",
  LABEL_TYPE, "SECTION_ID              = %s", /**/ LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data Object Structure */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",
  LABEL_TEXT_CRLF,    "/*      1                  57 59                              */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+          */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |          */",
  LABEL_TEXT_CRLF,    "/*  1  | TIME_PREFIX -->     | ALARM_RECORD ---->  |          */",
  LABEL_TEXT_CRLF,    "/*     |                     |                     |          */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+---------------------+--+       */",
  LABEL_TEXT_CRLF,    "/*     |                     |                        |       */",
  LABEL_TEXT_CRLF,    "/*     | TIME_PREFIX -->     | ALARM_RECORD ---->     |       */",
  LABEL_TEXT_CRLF,    "/*  2  |                     |                        |       */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+--------------------+---+       */",
  LABEL_TEXT_CRLF,    "/*     |                     |                    |           */",
  LABEL_TEXT_CRLF,    "/*  3  | TIME_PREFIX -->     | ALARM_RECORD ----> |           */",
  LABEL_TEXT_CRLF,    "/*     |                     |                    |           */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+--------------------+           */",
  LABEL_TEXT_CRLF,    "/*  . . .                                                     */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+-----------------------+        */",
  LABEL_TEXT_CRLF,    "/*     |                     |                       |        */",
  LABEL_TEXT_CRLF,    "/*  y  | TIME_PREFIX -->     | ALARM_RECORD ---->    |        */",
  LABEL_TEXT_CRLF,    "/*     |                     |                       |        */",
  LABEL_TEXT_CRLF,    "/*     +---------------------+-----------------------+        */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*     8-bit octet are numbered across the top.               */",
  LABEL_TEXT_CRLF,    "/*         No header records, all records are in the          */",
  LABEL_TEXT_CRLF,    "/*         same format.                                       */",
  LABEL_TEXT_CRLF,    "/*     Record Number is down the left edge.                   */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*  y  is FILE_RECORDS (ROWS)                                 */",
  LABEL_TEXT_CRLF,    "/*  Row terminated with 0x0D 0x0A (carriage return/line feed) */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*    Following the time tag are a set of comma delimited     */",
  LABEL_TEXT_CRLF,    "/*  fields that indicate the channels that are in alarm.      */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*    A sample alarm indicator is provided below:             */",
  LABEL_TEXT_CRLF,    "/*  YEL_HI V-HFR_-6(S1433)-5.698                              */",
  LABEL_TEXT_CRLF,    "/*  ------ -------- ----- ------                              */",
  LABEL_TEXT_CRLF,    "/*     |      |       |     |                                 */",
  LABEL_TEXT_CRLF,    "/*     |      |       |     +----  Voltage/Current/Temp       */",
  LABEL_TEXT_CRLF,    "/*     |      |       |            converted to eng units     */",
  LABEL_TEXT_CRLF,    "/*     |      |       |                                       */",
  LABEL_TEXT_CRLF,    "/*     |      |       +----------  S-channel from DMD         */",
  LABEL_TEXT_CRLF,    "/*     |      |                                               */",
  LABEL_TEXT_CRLF,    "/*     |      +------------------  Mnemonic for the channel   */",
  LABEL_TEXT_CRLF,    "/*     |                           subsystem and voltage      */",
  LABEL_TEXT_CRLF,    "/*     |                                                      */",
  LABEL_TEXT_CRLF,    "/*     +-------------------------  Yellow/Red High/Low        */",
  LABEL_TEXT_CRLF,    "/*                                 alarm condition            */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/*    Timetag is taken from the housekeeping record that      */",
  LABEL_TEXT_CRLF,    "/*  contains the channel that is in alarm.  See the RPWS      */",
  LABEL_TEXT_CRLF,    "/*  USERS GUIDE for additional details that describe          */",
  LABEL_TEXT_CRLF,    "/*  the relationship between the housekeeping time tag        */",
  LABEL_TEXT_CRLF,    "/*  and when that engineering data was sampled.               */",
  LABEL_TEXT_CRLF,    "/*                                                            */",
  LABEL_TEXT_CRLF,    "/**************************************************************/",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Record header */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT, "OBJECT                  = RPWS_ALARM_TIME",
  LABEL_CRLF, "%s\r\n",

#ifdef NAMEQUOTE
  LABEL_TEXT, "  NAME                    = \"RPWS_ALARM_TIME\"",
#else
  LABEL_TEXT, "  NAME                    = RPWS_ALARM_TIME",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = ASCII",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  COLUMNS                 = 3",
  LABEL_ROW_PREFIX_BYTES, "  ROW_BYTES               = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  DESCRIPTION             = \"Time tags, 3 formats.",
  LABEL_TEXT, "    PDS SCLK, HEX SCLK, and string\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  ^STRUCTURE              = \"RPWS_ENG_TIME.FMT\"",
  LABEL_TEXT, "END_OBJECT              = RPWS_ALARM_TIME",
  LABEL_CRLF, "%s\r\n",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "/* Data samples */",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "OBJECT                  = RPWS_ALARM_RECORD",

#ifdef NAMEQUOTE
  LABEL_TEXT, "  NAME                    = \"RPWS_ALARM_RECORD\"",
#else
  LABEL_TEXT, "  NAME                    = RPWS_ALARM_RECORD",
#endif

  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  INTERCHANGE_FORMAT      = ASCII",
  LABEL_ROWS, "  ROWS                    = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_ROW_PREFIX_BYTES, "  ROW_PREFIX_BYTES        = %d",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "  DESCRIPTION             = \"List of channels that",
  LABEL_TEXT, "    are in alarm condition\"",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END_OBJECT              = RPWS_ALARM_RECORD",
  LABEL_CRLF, "%s\r\n",
  LABEL_TEXT_CRLF, "END",
  LABEL_END, NULL
};


/* ************************************************************************* */
/* Label String Processing Utilities */

struct LABEL_TITLE
{
  char *band;
  char *period;
};
static struct LABEL_TITLE label_title[] = { "25HZ", "0.010000",
  "2_5KHZ", "0.000140",
  "10KHZ", "0.000036",
  "75KHZ", "0.0000045",
  "", ""
};

static int crlf_flag = 0;

  /*
   *    ITEM,ITEM_2
   *      changes to
   *    ITEM,ITEM 2
   *
   *    leading quote
   *    change underscore to space
   *    comma change to quote-comma-quote
   */
char *silly_ass_quoted_string (char *text, int sindex)
{
  static char delim[] = { "," };
  static char result[256];
  char *token[36];
  int index = 0;
  int ondex = 0;
  int i;

  memset (result, 0, 256);
  result[ondex++] = '"';                /* leading quote */
  for (index = 0; index < strlen (text); index++) {
    switch (text[index]) {
     default:
       result[ondex++] = text[index];
       break;
     case '_':
       result[ondex++] = ' ';
       break;
    }
  }
  result[ondex++] = '"';                /* trailing quote */
  result[ondex++] = 0;                  /* redundant, CYA */
  /*
   *     sindex>31 indicates they want the entire string
   */
  if (sindex > 31)
    return result;
  /*
   *     sindex>0, return the requested token
   */
  token[0] = strtok (result, delim);
  for (i = 1; i < sindex + 2; i++) {
    token[i] = NULL;
    if (token[i - 1])
      token[i] = strtok (NULL, delim);
  }

  if (token[sindex + 1])
    strcat (token[sindex], ",");

  return token[sindex];
}

  /*
   *    ITEM,ITEM_2
   *      changes to
   *    "ITEM","ITEM 2"
   *
   *    leading quote
   *    change underscore to space
   *    comma change to quote-comma-quote
   */
char *super_silly_ass_quoted_string (char *text, int sindex)
{
  static char delim[] = { "," };
  static char result[256];
  char *token[36];
  int index = 0;
  int ondex = 0;
  int i;

  memset (result, 0, 256);
  result[ondex++] = '"';                /* leading quote */
  for (index = 0; index < strlen (text); index++) {
    switch (text[index]) {
     default:
       result[ondex++] = text[index];
       break;
     case '_':
       result[ondex++] = ' ';
       break;
     case ',':
       result[ondex++] = '"';
       result[ondex++] = text[index];
       result[ondex++] = '"';
       break;
    }
  }
  result[ondex++] = '"';                /* trailing quote */
  result[ondex++] = 0;                  /* redundant, CYA */
  /*
   *     sindex>31 indicates they want the entire string
   */
  if (sindex > 31)
    return result;
  /*
   *     sindex>0, return the requested token
   */
  token[0] = strtok (result, delim);
  for (i = 1; i < sindex + 2; i++) {
    token[i] = NULL;
    if (token[i - 1])
      token[i] = strtok (NULL, delim);
  }

  if (token[sindex + 1])
    strcat (token[sindex], ",");

  return token[sindex];
}
char *super_silly_ass_unquoted_string (char *text, int sindex)
{
  static char delim[] = { "," };
  static char result[256];
  char *token[36];
  int index = 0;
  int ondex = 0;
  int i;

  memset (result, 0, 256);
  for (index = 0; index < strlen (text); index++) {
    switch (text[index]) {
     default:
       result[ondex++] = text[index];
       break;
     case '_':
       result[ondex++] = ' ';
       break;
     case ',':
       result[ondex++] = text[index];
       break;
    }
  }
  result[ondex++] = 0;                  /* redundant, CYA */
  /*
   *     sindex>31 indicates they want the entire string
   */
  if (sindex > 31)
    return result;
  /*
   *     sindex>0, return the requested token
   */
  token[0] = strtok (result, delim);
  for (i = 1; i < sindex + 2; i++) {
    token[i] = NULL;
    if (token[i - 1])
      token[i] = strtok (NULL, delim);
  }

  if (token[sindex + 1])
    strcat (token[sindex], ",");

  return token[sindex];
}

 /*
  * 
  */
char *version1 (char *ver)
{
  char *temp;
  static char result[64];

  strcpy (result, ver);
  temp = strchr (result, '.');
  if (temp)
    temp[0] = 0;
  return result;
}

 
/* ************************************************************************* */
/* Main Label Writer, dosen't actually read the associated data file, just   */
/* uses data already ccmulated in memory.                                    */

int rpws_label_write (struct RPWS_LABEL *data_record, char *directory[])
{
  struct LABEL_RECORD *label_record;
  static FILE *wbr_wfr_raw_label_file = NULL;
  char wbr_wfr_raw_label_filename[256];
  int itemp;
  int i;
  int pad27 = 0;
  int ignore = 0;
  int line_length = 0;
  int fill_length = 0;
  time_t time_t_uttime;
  char temp[128];
  char new_filename[128];
  char full_filename[256];
  char wbr_wfr_label_string[256];
  char *stemp[36];
  char sdelim;
  int sindex;

  switch (data_record->instrument) {
   case RPWS_LABEL_RAW:
     label_record = raw_label_record;
     strcpy (wbr_wfr_raw_label_filename, "raw_label_file.LBL");
     break;
   case RPWS_LABEL_WBR:
     label_record = wbr_wfr_label_record;
     strcpy (wbr_wfr_raw_label_filename, "wbr_label_file.LBL");
     break;
   case RPWS_LABEL_WFR:
     label_record = wbr_wfr_label_record;
     strcpy (wbr_wfr_raw_label_filename, "wfr_label_file.LBL");
     break;
   case RPWS_LABEL_STIM:
     label_record = stim_label_record;
     strcpy (wbr_wfr_raw_label_filename, "stim_label_file.LBL");
     break;
   case RPWS_LABEL_HSK:
     label_record = housekeeping_label_record;
     strcpy (wbr_wfr_raw_label_filename, "hsk_label_file.LBL");
     break;
   case RPWS_LABEL_DUST:
     label_record = dust_label_record;
     strcpy (wbr_wfr_raw_label_filename, "dust_label_file.LBL");
     break;
   case RPWS_LABEL_BFDL:
     label_record = bfdl_label_record;
     strcpy (wbr_wfr_raw_label_filename, "bfdl_label_file.LBL");
     break;
   case RPWS_LABEL_IPC:
     label_record = ipc_label_record;
     strcpy (wbr_wfr_raw_label_filename, "ipc_label_file.LBL");
     break;
   case RPWS_LABEL_ENG:
     label_record = eng_label_record;
     strcpy (wbr_wfr_raw_label_filename, "eng_label_file.LBL");
     break;
   case RPWS_LABEL_ALARM:
     label_record = alarm_label_record;
     strcpy (wbr_wfr_raw_label_filename, "alarm_label_file.LBL");
     break;
  }
  
  if (!wbr_wfr_raw_label_file) {
    wbr_wfr_raw_label_file = fopen (wbr_wfr_raw_label_filename, "w");
    chmod (wbr_wfr_raw_label_filename, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  }

  /**************************************
   *****	Establish local time	****
   **************************************/
  time_t_uttime = time (NULL);
  tm_uttime = gmtime (&time_t_uttime);

  /**************************************
   *****   Build up the label file   ****
   **************************************/
  i = 0;
  while (label_record[i].flag) {
    switch (label_record[i].flag) {
     case LABEL_FULLFILENAME:
       sprintf (temp, "%s", data_record->filename);
       strcpy (new_filename, temp);
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->filename);
       line_length += strlen (wbr_wfr_label_string);
       pad27 = 27 - strlen (data_record->filename);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_COPYNAME:
       sprintf (temp, "T%s_", data_record->filename);
       strcpy (new_filename, temp);
       break;
     case LABEL_FILENAME:
       sprintf (temp, "T%s_", data_record->filename);
       strcpy (new_filename, temp);
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->filename);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;

         /************************************************************\
	    - * We have a field that needs to be padded to 27 characters * -
	      \************************************************************/
     case LABEL_PAD27:
       if (pad27 > 0) {
         memset (temp, ' ', 32);
         temp[pad27] = 0;
         sprintf (wbr_wfr_label_string, label_record[i].label, temp);
         line_length += strlen (wbr_wfr_label_string);
         fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       }
       pad27 = 0;
       break;

     case LABEL_COPYBAND:
       sprintf (temp, "%s", label_title[data_record->fband].band);
       strcat (new_filename, temp);
       break;
     case LABEL_FILEBAND:
       sprintf (temp, "%s", label_title[data_record->fband].band);
       strcat (new_filename, temp);
     case LABEL_BAND:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                label_title[data_record->fband].band);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;

     case LABEL_COPYCLASS:
       sprintf (temp, "%d_", data_record->pad_class);
       strcat (new_filename, temp);
       break;
     case LABEL_FILECLASS:
       sprintf (temp, "%d_", data_record->pad_class);
       strcat (new_filename, temp);
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->pad_class);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;

     case LABEL_COPYTYPE:
       sprintf (temp, "%sFR.LBL", instrument[data_record->instrument]);
       strcat (new_filename, temp);
       break;
     case LABEL_FILETYPE:
       sprintf (temp, "%sFR.LBL", instrument[data_record->instrument]);
       strcat (new_filename, temp);
     case LABEL_TYPE:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                instrument[data_record->instrument]);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_TYPE_D:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                instrument_d[data_record->instrument]);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;

        /**************************************************************/

     case LABEL_TEXT:
       sprintf (wbr_wfr_label_string, label_record[i].label);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_TEXT_WFR:
       if (data_record->instrument) {
         sprintf (wbr_wfr_label_string, label_record[i].label);
         line_length += strlen (wbr_wfr_label_string);
         fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       }
       break;
     case LABEL_TEXT_WBR:
       if (!data_record->instrument) {
         sprintf (wbr_wfr_label_string, label_record[i].label);
         line_length += strlen (wbr_wfr_label_string);
         fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       }
       break;
     case LABEL_ROW_PREFIX_BYTES:
     case LABEL_ROW_SUFFIX_BYTES:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                pad_class[data_record->pad_class]);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_ROW_REMAIN_BYTES:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->sample_count -
                pad_class[data_record->pad_class]);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;

     case LABEL_ITEMS:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->sample_count);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_ROW_BYTES:
     case LABEL_BYTES:
       itemp = data_record->sample_count;
       if (data_record->instrument == RPWS_LABEL_WFR)
         itemp = data_record->sample_count * 2;
       sprintf (wbr_wfr_label_string, label_record[i].label, itemp);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_ITEM_BYTES:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->instrument ? 2 : 1);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_MSB:
       switch (data_record->instrument) {
        case RPWS_LABEL_WBR:
          sprintf (wbr_wfr_label_string, label_record[i].label, "");
          break;
        case RPWS_LABEL_WFR:
          sprintf (wbr_wfr_label_string, label_record[i].label, "MSB_");
          break;
       }
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_ESTIMATED_FILE_SIZE:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                (pad_class[data_record->pad_class] + 32) *
                data_record->record_count[0],
                data_record->record_count[0],
                pad_class[data_record->pad_class] + 32);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_RECORD_BYTES:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                pad_class[data_record->pad_class] + 32);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_FILE_RECORDS:
     case LABEL_ROWS:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->record_count[0]);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_FILE_RECORDS_1:
       if (data_record->record_count[0] != data_record->record_count[1]) {
         sprintf (wbr_wfr_label_string, label_record[i].label,
                  data_record->record_count[1]);
         line_length += strlen (wbr_wfr_label_string);
         fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       }
       break;
     case LABEL_ROWS_1:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->record_count[0] - 1);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_OFFSET:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->instrument ? 2047 : 127);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_OFFSET1:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->instrument ? 2048 : 128);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_SAMPLE_SIZE:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->instrument ? 16 : 8);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_VALID_MINIMUM:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->instrument ? 0 : 0);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_VALID_MAXIMUM:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->instrument ? 4095 : 255);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_SAMPLING_PARAMETER_INTERVAL:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                label_title[data_record->fband].period);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_SPAN:
       if (data_record->span < 6.0)
         sprintf (temp, "%.1f", data_record->span);
       else
         sprintf (temp, "%.0f", data_record->span);

       sprintf (temp, "%.0f", data_record->span);
       sprintf (wbr_wfr_label_string, label_record[i].label, temp);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_DATE:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->utc_date);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_TIME:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->utc_time);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_DATE_TODAY:
       sprintf (temp, "%04d-%02d-%02d",
                tm_uttime->tm_year + 1900,
                tm_uttime->tm_mon + 1, tm_uttime->tm_mday);
       sprintf (wbr_wfr_label_string, label_record[i].label, temp);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_TIME_TODAY:
       sprintf (temp, "%02d:%02d:%02d",
                tm_uttime->tm_hour, tm_uttime->tm_min, tm_uttime->tm_sec);
       sprintf (wbr_wfr_label_string, label_record[i].label, temp);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;

     case LABEL_MISSION_PHASE_NAME_0_CRLF:
       sindex = 0;
       stemp[sindex] =
         super_silly_ass_quoted_string (data_record->Mission_Phase, sindex);
       if (super_silly_ass_quoted_string
           (data_record->Mission_Phase, sindex + 1))
         sdelim = ',';
       else
         sdelim = '}';
       sprintf (wbr_wfr_label_string, label_record[i].label,
                stemp[sindex], sdelim);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       temp[data_record->Label_Line_Pad] = 0;
       fill_length = data_record->Label_Line_Pad - line_length - 2;     /* account for CR/LF pair */
       if (fill_length > 0)
         temp[fill_length] = 0;
       else
         temp[0] = 0;
       sprintf (wbr_wfr_label_string, "%s\r\n", temp);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       line_length = 0;
       break;

     case LABEL_MISSION_PHASE_NAME_N_CRLF:
       sindex = 1;
       while (stemp[sindex] =
              super_silly_ass_quoted_string (data_record->Mission_Phase,
                                             sindex)) {
         if (super_silly_ass_quoted_string
             (data_record->Mission_Phase, sindex + 1))
           sdelim = ',';
         else
           sdelim = '}';

         sprintf (wbr_wfr_label_string, label_record[i].label,
                  stemp[sindex], sdelim);
         line_length += strlen (wbr_wfr_label_string);
         fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
         temp[data_record->Label_Line_Pad] = 0;
         fill_length = data_record->Label_Line_Pad - line_length - 2;   /* account for CR/LF pair */
         if (fill_length > 0)
           temp[fill_length] = 0;
         else
           temp[0] = 0;
         sprintf (wbr_wfr_label_string, "%s\r\n", temp);
         fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
         line_length = 0;
         sindex++;
       }
       break;

     case LABEL_MISSION_PHASE_NAME_D_CRLF:
       sindex = 0;
       ignore = 0;
       while (stemp[sindex] =
              super_silly_ass_unquoted_string (data_record->Mission_Phase,
                                               sindex)) {
         if (super_silly_ass_unquoted_string
             (data_record->Mission_Phase, sindex + 1))
           sdelim = 1;
         else
           sdelim = 0;

         sprintf (wbr_wfr_label_string, label_record[i + ignore].label,
                  stemp[sindex], sdelim ? "," : ".\"");
         line_length += strlen (wbr_wfr_label_string);
         fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
         temp[data_record->Label_Line_Pad] = 0;
         fill_length = data_record->Label_Line_Pad - line_length - 2;   /* account for CR/LF pair */
         if (fill_length > 0)
           temp[fill_length] = 0;
         else
           temp[0] = 0;
         sprintf (wbr_wfr_label_string, "%s\r\n", temp);
         fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
         line_length = 0;
         ignore = 1;
         sindex++;
       }
       break;
     case LABEL_TARGET_0_CRLF:
       sindex = 0;
       stemp[sindex] =
         super_silly_ass_quoted_string (data_record->Target, sindex);
       if (super_silly_ass_quoted_string (data_record->Target, sindex + 1))
         sdelim = ',';
       else
         sdelim = '}';
       sprintf (wbr_wfr_label_string, label_record[i].label,
                stemp[sindex], sdelim);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       temp[data_record->Label_Line_Pad] = 0;
       fill_length = data_record->Label_Line_Pad - line_length - 2;     /* account for CR/LF pair */
       if (fill_length > 0)
         temp[fill_length] = 0;
       else
         temp[0] = 0;
       sprintf (wbr_wfr_label_string, "%s\r\n", temp);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       line_length = 0;
       break;
     case LABEL_TARGET_N_CRLF:
       sindex = 1;
       while (stemp[sindex] =
              super_silly_ass_quoted_string (data_record->Target, sindex)) {
         if (super_silly_ass_quoted_string (data_record->Target, sindex + 1))
           sdelim = ',';
         else
           sdelim = '}';

         sprintf (wbr_wfr_label_string, label_record[i].label,
                  stemp[sindex], sdelim);
         line_length += strlen (wbr_wfr_label_string);
         fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
         temp[data_record->Label_Line_Pad] = 0;
         fill_length = data_record->Label_Line_Pad - line_length - 2;   /* account for CR/LF pair */
         if (fill_length > 0)
           temp[fill_length] = 0;
         else
           temp[0] = 0;
         sprintf (wbr_wfr_label_string, "%s\r\n", temp);
         fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
         line_length = 0;
         sindex++;
       }
       break;
     case LABEL_TARGET_D_CRLF:
       sindex = 0;
       ignore = 0;
       while (stemp[sindex] =
              super_silly_ass_unquoted_string (data_record->Target, sindex)) {
         if (super_silly_ass_unquoted_string
             (data_record->Target, sindex + 1))
           sdelim = 1;
         else
           sdelim = 0;

#ifdef ORBIT
         sprintf (wbr_wfr_label_string, label_record[i + ignore].label,
                  stemp[sindex], sdelim ? "," : " ");
#else
         sprintf (wbr_wfr_label_string, label_record[i + ignore].label,
                  stemp[sindex], sdelim ? "," : ".\"");
#endif

         line_length += strlen (wbr_wfr_label_string);
         fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
         temp[data_record->Label_Line_Pad] = 0;
         fill_length = data_record->Label_Line_Pad - line_length - 2;   /* account for CR/LF pair */
         if (fill_length > 0)
           temp[fill_length] = 0;
         else
           temp[0] = 0;
         sprintf (wbr_wfr_label_string, "%s\r\n", temp);
         fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
         line_length = 0;
         ignore = 1;
         sindex++;
       }
       break;
     case LABEL_ORBIT_0_CRLF:
       sindex = 0;
       if (data_record->Orbit) {
         if (data_record->Orbit[0]) {
           stemp[sindex] =
             super_silly_ass_quoted_string (data_record->Orbit, sindex);
           if (super_silly_ass_quoted_string (data_record->Orbit, sindex + 1))
             sdelim = ',';
           else
             sdelim = '}';
           sprintf (wbr_wfr_label_string, label_record[i].label,
                    stemp[sindex], sdelim);
           line_length += strlen (wbr_wfr_label_string);
           fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
           temp[data_record->Label_Line_Pad] = 0;
           fill_length = data_record->Label_Line_Pad - line_length - 2; /* account for CR/LF pair */
           if (fill_length > 0)
             temp[fill_length] = 0;
           else
             temp[0] = 0;
           sprintf (wbr_wfr_label_string, "%s\r\n", temp);
           fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
           line_length = 0;
         }
       }
       break;
     case LABEL_ORBIT_N_CRLF:
       sindex = 1;
       if (data_record->Orbit) {
         if (data_record->Orbit[0]) {
           while (stemp[sindex] =
                  super_silly_ass_quoted_string (data_record->Orbit,
                                                 sindex)) {
             if (super_silly_ass_quoted_string
                 (data_record->Orbit, sindex + 1))
               sdelim = ',';
             else
               sdelim = '}';

             sprintf (wbr_wfr_label_string, label_record[i].label,
                      stemp[sindex], sdelim);
             line_length += strlen (wbr_wfr_label_string);
             fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
             temp[data_record->Label_Line_Pad] = 0;
             fill_length = data_record->Label_Line_Pad - line_length - 2;       /* account for CR/LF pair */
             if (fill_length > 0)
               temp[fill_length] = 0;
             else
               temp[0] = 0;
             sprintf (wbr_wfr_label_string, "%s\r\n", temp);
             fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
             line_length = 0;
             sindex++;
           }
         }
       }
       break;
     case LABEL_ORBIT_D_CRLF:
       sindex = 0;
       ignore = 0;
       if (data_record->Orbit) {
         if (data_record->Orbit[0]) {
           while (stemp[sindex] =
                  super_silly_ass_unquoted_string (data_record->Orbit,
                                                   sindex)) {
             if (super_silly_ass_unquoted_string
                 (data_record->Orbit, sindex + 1))
               sdelim = 1;
             else
               sdelim = 0;

             sprintf (wbr_wfr_label_string, label_record[i + ignore].label,
                      stemp[sindex], sdelim ? "," : ".\"");
             line_length += strlen (wbr_wfr_label_string);
             fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
             temp[data_record->Label_Line_Pad] = 0;
             fill_length = data_record->Label_Line_Pad - line_length - 2;       /* account for CR/LF pair */
             if (fill_length > 0)
               temp[fill_length] = 0;
             else
               temp[0] = 0;
             sprintf (wbr_wfr_label_string, "%s\r\n", temp);
             fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
             line_length = 0;
             ignore = 1;
             sindex++;
           }
         }
       }
       break;
     case LABEL_NOTE:
       sprintf (wbr_wfr_label_string, label_record[i].label, Title, Version);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
		 
     case LABEL_PRODUCT:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->Product);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
		 
	  case LABEL_PRODUCT_VERSION_ID:
		  sprintf(wbr_wfr_label_string, label_record[i].label, data_record->ProdVerId);
		  line_length += strlen (wbr_wfr_label_string);
        fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
		  break;

     case LABEL_SCLK_ST:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->sclk_start);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;

     case LABEL_SCLK_SP:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->sclk_stop);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;

     case LABEL_SCET_ST:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->scet_start);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_SCET_SP:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->scet_stop);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_EPHEM_ST:
       crlf_flag = 1;
       if (data_record->ephem_start)
         if (data_record->ephem_start[0]) {
           sprintf (wbr_wfr_label_string, label_record[i].label,
                    data_record->ephem_start);
           line_length += strlen (wbr_wfr_label_string);
           fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
           crlf_flag = 0;
         }
       break;
     case LABEL_EPHEM_SP:
       crlf_flag = 1;
       if (data_record->ephem_stop)
         if (data_record->ephem_stop[0]) {
           sprintf (wbr_wfr_label_string, label_record[i].label,
                    data_record->ephem_stop);
           line_length += strlen (wbr_wfr_label_string);
           fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
           crlf_flag = 0;
         }
       break;
     case LABEL_EPHEM_TEXT:
       crlf_flag = 1;
       if (data_record->ephem_start)
         if (data_record->ephem_start[0])
           crlf_flag = 0;
       if (data_record->ephem_stop)
         if (data_record->ephem_stop[0])
           crlf_flag = 0;
       if (!crlf_flag) {
         sprintf (wbr_wfr_label_string, label_record[i].label);
         line_length += strlen (wbr_wfr_label_string);
         fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       }
       break;
     case LABEL_IGNORE:
       break;

        /**************************************************************/
     case LABEL_VERSION:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                data_record->Label_Version);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_VERSION1:
       sprintf (wbr_wfr_label_string, label_record[i].label,
                version1 (data_record->Label_Version));
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;
     case LABEL_CRLF:
       if (crlf_flag) {
         crlf_flag = 0;
         break;
       }
       memset (temp, ' ', data_record->Label_Line_Pad);
       temp[data_record->Label_Line_Pad] = 0;
       fill_length = data_record->Label_Line_Pad - line_length - 2;     /* account for CR/LF pair */
       if (fill_length > 0)
         temp[fill_length] = 0;
       else
         temp[0] = 0;
       sprintf (wbr_wfr_label_string, label_record[i].label, temp);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       line_length = 0;
       break;

     case LABEL_DELIM:

       switch (Delimiter) {
        default:
          sprintf (wbr_wfr_label_string, label_record[i].label, "Unique");
          break;
        case ' ':
          sprintf (wbr_wfr_label_string, label_record[i].label, "Space");
          break;
        case ',':
          sprintf (wbr_wfr_label_string, label_record[i].label, "Comma");
          break;
        case ';':
          sprintf (wbr_wfr_label_string, label_record[i].label, "Semicolon");
          break;
       }
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       break;

     case LABEL_TEXT_CRLF:
       sprintf (wbr_wfr_label_string, label_record[i].label);
       line_length += strlen (wbr_wfr_label_string);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);

       if (crlf_flag) {
         crlf_flag = 0;
         break;
       }
       memset (temp, ' ', data_record->Label_Line_Pad);
       temp[data_record->Label_Line_Pad] = 0;
       fill_length = data_record->Label_Line_Pad - line_length - 2;     /* account for CR/LF pair */
       if (fill_length > 0)
         temp[fill_length] = 0;
       else
         temp[0] = 0;
       sprintf (wbr_wfr_label_string, "%s\r\n", temp);
       fputs (wbr_wfr_label_string, wbr_wfr_raw_label_file);
       line_length = 0;
       break;

             /*******************
	      ** Debugging aid **
	      *******************/
     default:
       fprintf (stderr, "unhandled LABEL_%d\n", label_record[i].flag);
    }
    i++;
  }
  if (wbr_wfr_raw_label_file) {
    fclose (wbr_wfr_raw_label_file);
    wbr_wfr_raw_label_file = NULL;
  }

  switch (data_record->instrument) {
   case RPWS_LABEL_WBR:
     strcpy (full_filename, directory[RPWS_ARCHIVE_DATA_WBRFULL]);
     strcat (full_filename, "/");
     break;
   case RPWS_LABEL_WFR:
     strcpy (full_filename, directory[RPWS_ARCHIVE_DATA_WFRFULL]);
     strcat (full_filename, "/");
     break;
   case RPWS_LABEL_STIM:
     strcpy (full_filename, directory[RPWS_ARCHIVE_DATA_ANCIL]);
     strcat (full_filename, "/");
     break;
   case RPWS_LABEL_RAW:
     strcpy (full_filename, directory[RPWS_ARCHIVE_DATA_RAW]);
     strcat (full_filename, "/");
     break;
   case RPWS_LABEL_HSK:
     strcpy (full_filename, directory[RPWS_ARCHIVE_HOUSEKEEPING]);
     strcat (full_filename, "/");
     break;
   case RPWS_LABEL_DUST:
     strcpy (full_filename, directory[RPWS_ARCHIVE_HOUSEKEEPING]);
     strcat (full_filename, "/");
     break;
   case RPWS_LABEL_BFDL:
     strcpy (full_filename, directory[RPWS_ARCHIVE_HOUSEKEEPING]);
     strcat (full_filename, "/");
     break;
   case RPWS_LABEL_IPC:
     strcpy (full_filename, directory[RPWS_ARCHIVE_HOUSEKEEPING]);
     strcat (full_filename, "/");
     break;
   case RPWS_LABEL_ENG:
     strcpy (full_filename, directory[RPWS_ARCHIVE_HOUSEKEEPING]);
     strcat (full_filename, "/");
     break;
   case RPWS_LABEL_ALARM:
     strcpy (full_filename, directory[RPWS_ARCHIVE_HOUSEKEEPING]);
     strcat (full_filename, "/");
     break;
  }
  if (data_record->filepath1) {
    strcat (full_filename, data_record->filepath1);
    strcat (full_filename, "/");
  }
  if (data_record->filepath2) {
    strcat (full_filename, data_record->filepath2);
    strcat (full_filename, "/");
  }
  strcat (full_filename, new_filename);
  strcat (full_filename, ".LBL");
  remove (full_filename);
  rename (wbr_wfr_raw_label_filename, full_filename);
}

 /*
  * 
  */
int master_detail (struct RPWS_LABEL *data_record, char *directory[])
{
  static int first_time = 1;
  char *write_flag = { "a" };
  FILE *time_file;
  char *lbl[] = { "WBR", "WFR", "STIM", "UNK" };
  char time_file_name[256];
  int path_flag = 0;

  strcpy (time_file_name, Time_File_Name);
  if (directory)
    if (directory[RPWS_ARCHIVE_EXTRAS])
      if (directory[RPWS_ARCHIVE_EXTRAS][0]) {
        strcpy (time_file_name, directory[RPWS_ARCHIVE_EXTRAS]);
        strcat (time_file_name, "/");
        strcat (time_file_name, Time_File_Name);
      }
  time_file = fopen (time_file_name, write_flag);
  chmod (time_file_name, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
  if (first_time) {
    fprintf (time_file, "/**************************************/\r\n");
    fprintf (time_file, "/*****     Datafile  Listing      *****/\r\n");
    fprintf (time_file, "/*****  Details for each of the   *****/\r\n");
    fprintf (time_file, "/*****  files produced during     *****/\r\n");
    fprintf (time_file, "/*****  this archive session      *****/\r\n");
    fprintf (time_file, "/*****                            *****/\r\n");
    fprintf (time_file, "/*****    The keywords here tend  *****/\r\n");
    fprintf (time_file, "/*****  to mimic those in the     *****/\r\n");
    fprintf (time_file, "/*****  label file (WBR/WFR added *****/\r\n");
    fprintf (time_file, "/*****  to make automated decode  *****/\r\n");
    fprintf (time_file, "/*****  a little easier).         *****/\r\n");
    fprintf (time_file, "/*****                            *****/\r\n");
    fprintf (time_file, "/*****  xxx_RAW_DATASET_BYTES     *****/\r\n");
    fprintf (time_file, "/*****    This is a tally of the  *****/\r\n");
    fprintf (time_file, "/*****  total bytes that were     *****/\r\n");
    fprintf (time_file, "/*****  collected from the raw    *****/\r\n");
    fprintf (time_file, "/*****  dataset (i.e. u-files;    *****/\r\n");
    fprintf (time_file, "/*****  without any padding).     *****/\r\n");
    fprintf (time_file, "/*****                            *****/\r\n");
  }
  fprintf (time_file, "/**************************************/\r\n");


  if (data_record->filename)
    if (data_record->filename[0]) {
      fprintf (time_file, "%s_PRODUCT_ID                     = \"%s\"\r\n",
               lbl[data_record->instrument], data_record->filename);

             /*********************************/
      if (data_record->filepath1)
        if (data_record->filepath1[0])
          path_flag |= 1;
      if (data_record->filepath2)
        if (data_record->filepath2[0])
          path_flag |= 2;
      if (data_record->filepath3)
        if (data_record->filepath3[0])
          path_flag |= 4;
      if (path_flag)
        fprintf (time_file,
                 "  %s_DATA_SUBDIRECTORY            = \"",
                 lbl[data_record->instrument]);
      if (path_flag & 1)
        fprintf (time_file, "%s/", data_record->filepath1);
      if (path_flag & 2)
        fprintf (time_file, "%s/", data_record->filepath2);
      if (path_flag & 4)
        fprintf (time_file, "%s/", data_record->filepath3);
      if (path_flag)
        fprintf (time_file, "\"\r\n");

             /*********************************/

      fprintf (time_file,
               "  %s_DATA_SET_ID                  = \"CO-V/E/J/S/SS-RPWS-2-REFDR-%sFULL-%s\"\r\n",
               lbl[data_record->instrument],
               instrument[data_record->instrument],
               data_record->Label_Version);

      fprintf (time_file, "  %s_PRODUCT_ID                   = \"%s\"\r\n",
               lbl[data_record->instrument], data_record->filename);

      fprintf (time_file, "  %s_INSTRUMENT_ID                = \"%s\"\r\n",
               lbl[data_record->instrument], "RPWS");

             /*********************************/

      fprintf (time_file, "  %s_FILE_RECORDS                 = %d\r\n",
               lbl[data_record->instrument], data_record->record_count[0]);
      fprintf (time_file, "  %s_RECORD_BYTES                 = %d\r\n",
               lbl[data_record->instrument],
               pad_class[data_record->pad_class] + 32);
      fprintf (time_file, "  %s_RAW_DATASET_BYTES            = %d\r\n",
               lbl[data_record->instrument], data_record->dataset_size[0]);
      fprintf (time_file, "  %s_ROW_SUFFIX_BYTES             = %d\r\n",
               lbl[data_record->instrument],
               pad_class[data_record->pad_class]);
      fprintf (time_file, "  %s_SPACECRAFT_CLOCK_START_COUNT = %s\r\n",
               lbl[data_record->instrument], data_record->sclk_start);
      fprintf (time_file, "  %s_SPACECRAFT_CLOCK_STOP_COUNT  = %s\r\n",
               lbl[data_record->instrument], data_record->sclk_stop);
      fprintf (time_file, "  %s_START_TIME                   = %s\r\n",
               lbl[data_record->instrument], data_record->scet_start);
      fprintf (time_file, "  %s_STOP_TIME                    = %s\r\n",
               lbl[data_record->instrument], data_record->scet_stop);

      fprintf (time_file, "  %s_WAVEFORM_DURATION            = %.3f\r\n",
               lbl[data_record->instrument], data_record->duration);

#ifdef _EPHEMERIS_TIME_
      fprintf (time_file, "  %s_NATIVE_START_TIME            = %s\r\n",
               lbl[data_record->instrument], data_record->ephem_start);
      fprintf (time_file, "  %s_NATIVE_STOP_TIME             = %s\r\n",
               lbl[data_record->instrument], data_record->ephem_stop);
#endif
    }
  first_time = 0;
  fclose (time_file);
  return 0;
}
int master_label (struct RPWS_LABEL *data_record, char *lbl,
                  char *directory[])
{
  static int first_time = 1;
  char *write_flag[2] = { "a", "a" };
  FILE *time_file;
  time_t time_t_uttime;
  struct tm *tm_uttime;
  char time_file_name[256];

  time_t_uttime = time (NULL);
  tm_uttime = gmtime (&time_t_uttime);

  strcpy (time_file_name, Time_File_Name);
  if (directory)
    if (directory[RPWS_ARCHIVE_EXTRAS])
      if (directory[RPWS_ARCHIVE_EXTRAS][0]) {
        strcpy (time_file_name, directory[RPWS_ARCHIVE_EXTRAS]);
        strcat (time_file_name, "/");
        strcat (time_file_name, Time_File_Name);
      }
  time_file = fopen (time_file_name, write_flag[first_time]);
  if (first_time) {
    fprintf (time_file, "PDS_VERSION_ID       = PDS3\r\n");
    fprintf (time_file, "RECORD_TYPE          = STREAM\r\n");
    fprintf (time_file, "OBJECT               = TEXT\r\n");
    fprintf (time_file, "  PUBLICATION_DATE   = %4d-%02d-%02d\r\n",
             tm_uttime->tm_year + 1900,
             tm_uttime->tm_mon + 1, tm_uttime->tm_mday);
    fprintf (time_file,
             "  NOTE               = \"Start/Stop Log, format description follows.\"\r\n");
    fprintf (time_file, "  SOFTWARE_VERSION_ID= \"%s %s\"\r\n", Title,
             Version);
    fprintf (time_file, "END_OBJECT           = TEXT\r\n");
    fprintf (time_file, "END\r\n");
  }
  chmod (time_file_name, S_IRUSR | S_IWUSR | S_IWUSR | S_IRGRP | S_IROTH);
  fprintf (time_file, "/**************************************/\r\n");
  if (first_time)
    fprintf (time_file, "/***** %16s Filename  *****/\r\n", Time_File_Name);
  fprintf (time_file, "/***** %8s Start/Stop Times  *****/\r\n", lbl);
  if (first_time) {
    fprintf (time_file, "/*****                            *****/\r\n");
    fprintf (time_file, "/*****  This section lists the    *****/\r\n");
    fprintf (time_file, "/*****  requested start and       *****/\r\n");
    fprintf (time_file, "/*****  stop times in several     *****/\r\n");
    fprintf (time_file, "/*****  formats.                  *****/\r\n");
  } else {
    fprintf (time_file, "/*****                            *****/\r\n");
    fprintf (time_file, "/*****  This section lists the    *****/\r\n");
    fprintf (time_file, "/*****  actual start and stop     *****/\r\n");
    fprintf (time_file, "/*****  times in several          *****/\r\n");
    fprintf (time_file, "/*****  formats.                  *****/\r\n");
  }
  fprintf (time_file, "/**************************************/\r\n");

  if (first_time) {
    fprintf (time_file,
             "%s_PRODUCT_CREATION_DATE         = %04d-%02d-%02d\r\n", lbl,
             tm_uttime->tm_year + 1900, tm_uttime->tm_mon + 1,
             tm_uttime->tm_mday);
    fprintf (time_file,
             "%s_PRODUCT_CREATION_TIME         = %02d:%02d:%02d\r\n", lbl,
             tm_uttime->tm_hour, tm_uttime->tm_min, tm_uttime->tm_sec);
  } else {
    if (!strcmp (lbl, "WBR")) {
      fprintf (time_file, "%s_DATA_DIRECTORY                = \"%s/\"\r\n",
               lbl, directory[RPWS_ARCHIVE_DATA_WBRFULL]);
    }
    if (!strcmp (lbl, "WFR")) {
      fprintf (time_file, "%s_DATA_DIRECTORY                = \"%s/\"\r\n",
               lbl, directory[RPWS_ARCHIVE_DATA_WFRFULL]);
    }
  }
  if (data_record->sclk_start)
    if (data_record->sclk_start[0])
      if (strcmp (data_record->sclk_start, "9/999999999:255"))
        fprintf (time_file, "%s_SPACECRAFT_CLOCK_START_COUNT  = \"%s\"\r\n",
                 lbl, data_record->sclk_start);
  if (data_record->sclk_stop)
    if (data_record->sclk_stop[0])
      if (strcmp (data_record->sclk_stop, "0/000000000:000"))
        fprintf (time_file, "%s_SPACECRAFT_CLOCK_STOP_COUNT   = \"%s\"\r\n",
                 lbl, data_record->sclk_stop);
  if (data_record->scet_start)
    if (data_record->scet_start[0])
      fprintf (time_file, "%s_START_TIME                    =  %s\r\n", lbl,
               data_record->scet_start);
  if (data_record->scet_stop)
    if (data_record->scet_stop[0])
      fprintf (time_file, "%s_STOP_TIME                     =  %s\r\n", lbl,
               data_record->scet_stop);
  if (data_record->utc_date)
    if (data_record->utc_date[0])
      fprintf (time_file, "%s_UTC_START                     = \"%s\"\r\n",
               lbl, data_record->utc_date);
  if (data_record->utc_time)
    if (data_record->utc_time[0])
      fprintf (time_file, "%s_UTC_STOP                      = \"%s\"\r\n",
               lbl, data_record->utc_time);
  if (data_record->ephem_start)
    if (data_record->ephem_start[0])
      fprintf (time_file, "%s_NATIVE_START_TIME             =  %s\r\n", lbl,
               data_record->ephem_start);
  if (data_record->ephem_stop)
    if (data_record->ephem_stop[0])
      fprintf (time_file, "%s_NATIVE_STOP_TIME              =  %s\r\n", lbl,
               data_record->ephem_stop);
  fclose (time_file);
  first_time = 0;
  return 0;
}

static char *tohex (char *dec_sclk)
{
  char *next;
  static char result[128];
  int sclk;
  int rti;

  sclk = strtol (&dec_sclk[2], &next, 10);
  rti = strtol (next + 1, NULL, 10);
  rti >>= 5;
  sprintf (result, "0x%08X.%X", sclk, rti);
  return result;
}
static char *seconds_only (char *seconds)
{
  char *temp;
  static char result[128];

  strcpy (result, seconds);
  temp = strstr (result, ".");
  temp[0] = 0;
  return result;
}
int master_cd_label (struct RPWS_LABEL *master_record,
                     struct RPWS_LABEL *wbr_record,
                     struct RPWS_LABEL *wfr_record,
                     char *directory[],
                     char *cd_index,
                     char *cd_count, char *cd_format, double byte_count)
{                                       /* >0 means not FULL (end of period) */
  int i;
  char text_buffer_upper[6][64] = { "", "", "", "", "", "" };
  char text_buffer_lower[6][64] = { "", "", "", "", "", "" };
  char text_buffer_right[12][64] =
    { "", "", "", "", "", "", "", "", "", "", "", "" };
  char text_buffer_left[12][64] =
    { "", "", "", "", "", "", "", "", "", "", "", "" };
  struct GRAPHICS_TEXT text = {
    "CASSINI/RPWS",                     /* upper */
    "Department of Physics & Astronomy",
    "University of Iowa",
    "CORPWS_0000",
    NULL,
    NULL,
    "PDS Archive Dataset",              /* lower */
    NULL,                               /* [1] */
    NULL,                               /* selected start */
    NULL,                               /* selected stop */
    NULL,                               /* date stampe */
    NULL,                               /* version String */
    "WBRFULL",                          /* left */
    "Actual time (from data)",
    NULL,                               /* "Start SCET/SCLK" */
    NULL,                               /* start SCET dd-MMM-yyyy */
    NULL,                               /* start SCET yyy-dddT00:00:00 */
    NULL,                               /* start SCLK */
    NULL,                               /* "Stop SCET/SCLK" */
    NULL,                               /*  */
    NULL,                               /*  */
    NULL,                               /*  */
    NULL,                               /*  */
    NULL,                               /*  */
    "WFRFULL",                          /* right */
    "Actual time (from data)",
    NULL,                               /* same as left */
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL
  };
  time_t time_t_uttime;
  struct tm *tm_uttime;
  int itemp;
  char temp[128];

  time_t_uttime = time (NULL);
  tm_uttime = gmtime (&time_t_uttime);

  sprintf (text_buffer_lower[4], "Processed %04d-%02d-%02d",
           tm_uttime->tm_year + 1900,
           tm_uttime->tm_mon + 1, tm_uttime->tm_mday);

  if (Version)
    if (Version[0])
      sprintf (text_buffer_lower[5], "HR Archive Version %s", Version);
  if (cd_index)
    if (cd_index[0])
      sprintf (text_buffer_upper[5], "CD/DVD %s/%s", cd_index, cd_count);
  if (cd_format)
    if (cd_format[0])
      sprintf (text_buffer_upper[4], "%s", cd_format);


  if (master_record->scet_start)
    if (master_record->scet_start[0]) {
      sprintf (text_buffer_lower[1], "%s", "Start time");
      sprintf (text_buffer_lower[2], "%s", master_record->scet_start);
    }

  if (byte_count > 0.0)                 /* >0 means not FULL (end of period) */
    if (master_record->scet_stop)
      if (master_record->scet_stop[0]) {
        sprintf (text_buffer_lower[1], "%s", "Start/Stop times");
        sprintf (temp, " to %s", master_record->scet_stop);
        strcat (text_buffer_lower[2], temp);
      }

  if (byte_count) {
    itemp = byte_count / 1.E6;
    if (byte_count > 0.0)
      sprintf (text_buffer_lower[3], "Data size approx. %dMB (end)",
               abs (itemp));
    else
      sprintf (text_buffer_lower[3], "Data size approx. %dMB (mid)",
               abs (itemp));
  }

  if (wbr_record->scet_start)
    if (wbr_record->scet_start[0]) {
      sprintf (text_buffer_left[2], "Start SCET/SCLK");
      sprintf (text_buffer_left[3], "%s", wbr_record->scet_start_2);
      sprintf (text_buffer_left[4], "%s",
               seconds_only (wbr_record->scet_start));
    }
  if (wbr_record->sclk_start)
    if (wbr_record->sclk_start[0])
      if (strcmp (wbr_record->sclk_start, "9/999999999:255")) {
        sprintf (text_buffer_left[5], "%s", wbr_record->sclk_start);
        sprintf (text_buffer_left[6], "%s", tohex (wbr_record->sclk_start));
      }
  if (wbr_record->scet_stop)
    if (wbr_record->scet_stop[0]) {
      sprintf (text_buffer_left[7], "Stop  SCET/SCLK");
      sprintf (text_buffer_left[8], "%s",
               seconds_only (wbr_record->scet_stop));
    }
  if (wbr_record->sclk_stop)
    if (wbr_record->sclk_stop[0])
      if (strcmp (wbr_record->sclk_stop, "0/000000000:000")) {
        sprintf (text_buffer_left[9], "%s", wbr_record->sclk_stop);
        sprintf (text_buffer_left[10], "%s", tohex (wbr_record->sclk_stop));
      }

  if (wfr_record->scet_start)
    if (wfr_record->scet_start[0]) {
      sprintf (text_buffer_right[2], "Start SCET/SCLK");
      sprintf (text_buffer_right[3], "%s", wfr_record->scet_start_2);
      sprintf (text_buffer_right[4], "%s",
               seconds_only (wfr_record->scet_start));
    }
  if (wfr_record->sclk_start)
    if (wfr_record->sclk_start[0])
      if (strcmp (wfr_record->sclk_start, "9/999999999:255")) {
        sprintf (text_buffer_right[5], "%s", wfr_record->sclk_start);
        sprintf (text_buffer_right[6], "%s", tohex (wfr_record->sclk_start));
      }
  if (wfr_record->scet_stop)
    if (wfr_record->scet_stop[0]) {
      sprintf (text_buffer_right[7], "Stop  SCET/SCLK");
      sprintf (text_buffer_right[8], "%s",
               seconds_only (wfr_record->scet_stop));
    }
  if (wfr_record->sclk_stop)
    if (wfr_record->sclk_stop[0])
      if (strcmp (wfr_record->sclk_stop, "0/000000000:000")) {
        sprintf (text_buffer_right[9], "%s", wfr_record->sclk_stop);
        sprintf (text_buffer_right[10], "%s", tohex (wfr_record->sclk_stop));
      }

  for (i = 0; i < 6; i++) {
    if (!text.upper_text[i])
      text.upper_text[i] = text_buffer_upper[i];
  }
  for (i = 0; i < 6; i++) {
    if (!text.lower_text[i])
      text.lower_text[i] = text_buffer_lower[i];
  }
  text.lower_text[3][0] = 0;            /* Data size */

  for (i = 0; i < 12; i++) {
    if (!text.left_text[i])
      text.left_text[i] = text_buffer_left[i];
    text.left_text[i][0] = 0;           /* WBR stats */
  }
  for (i = 0; i < 12; i++) {
    if (!text.right_text[i])
      text.right_text[i] = text_buffer_right[i];
    text.right_text[i][0] = 0;          /* WFR stats */
  }


  gr_main (0, NULL, directory[RPWS_ARCHIVE_EXTRAS]);
  gr_main (1, &text, NULL);
  gr_main (1, &text, NULL);
  gr_main (2, NULL, NULL);
}

 /*
  *     Label processing for PNG files
  */
int rpws_label_browse_image (struct RPWS_LABEL *detail_record,
                             char *directory,
                             char *sub_directory, char *thumb_flag, int flag)
{
  static FILE *label_file;
  static char label_filename[128];
  static int index;

  switch (flag) {
   case 0:
   case 1:                             /* OPEN */
     if (sub_directory)
       sprintf (label_filename, "%s/%s/PNGPLOT.LBL", directory,
                sub_directory);
     else
       sprintf (label_filename, "%s/PNGPLOT.LBL", directory);
     fflush (stdout);
     label_file = fopen (label_filename, "w");
     fprintf (label_file, "PDS_VERSION_ID          = PDS3\r\n");
     fprintf (label_file, "RECORD_TYPE             = UNDEFINED\r\n");
     if (Mafi_Flag) {
       fprintf (label_file, "\r\n");
       fprintf (label_file,
                "DATA_SET_ID             = CO-V/E/J/S/SS-RPWS-2-REFDR-%sFULL-%s\r\n",
                instrument[detail_record->instrument],
                detail_record->Label_Version);
       fprintf (label_file, "PRODUCT_ID              = %s_%sPNG_%s\r\n",
                sub_directory, instrument[detail_record->instrument],
                detail_record->Label_Version);
       fprintf (label_file, "PRODUCT_TYPE            = IMAGE\r\n");
       fprintf (label_file, "\r\n");
       fprintf (label_file,
                "INSTRUMENT_HOST_NAME    = \"CASSINI ORBITER\"\r\n");
       fprintf (label_file, "INSTRUMENT_HOST_ID      = CO\r\n");
       fprintf (label_file,
                "INSTRUMENT_NAME         = \"RADIO AND PLASMA WAVE SCIENCE\"\r\n");
       fprintf (label_file, "INSTRUMENT_ID           = RPWS");
       fprintf (label_file, "\r\n");
     }
     fprintf (label_file, "^PNG_DOCUMENT           = (");
     index = 0;
     break;
   case -1:                            /* DETAIL */
     if (index > 0) {
       fprintf (label_file, ",\r\n");
       fprintf (label_file, "                           ");
     }

#ifdef MERGED
     fprintf (label_file, "\"%s.PNG\"", detail_record->filename);
     index++;
     fprintf (label_file, ",\r\n");
     fprintf (label_file, "                           ");
     fprintf (label_file, "\"%s_TN.PNG\"", detail_record->filename);
     index++;
#else
     fprintf (label_file, "\"%s%s.PNG\"", detail_record->filename,
              thumb_flag);
     index++;
#endif

     break;
   case -2:                            /* CLOSE */
     fprintf (label_file, ")\r\n");
     fprintf (label_file, "OBJECT                  = PNG_DOCUMENT\r\n");
     fprintf (label_file,
              "  DOCUMENT_NAME           = \"CASSINI/RPWS FULL RESOLUTION SPECTROGRAM PLOTS\"\r\n");
     fprintf (label_file, "  DOCUMENT_TOPIC_TYPE     = \"BROWSE IMAGE\"\r\n");
     fprintf (label_file, "  INTERCHANGE_FORMAT      = BINARY\r\n");
     fprintf (label_file, "  DOCUMENT_FORMAT         = PNG\r\n");
     fprintf (label_file, "  PUBLICATION_DATE        = %04d-%02d-%02d\r\n",
              tm_uttime->tm_year + 1900,
              tm_uttime->tm_mon + 1, tm_uttime->tm_mday);
     fprintf (label_file, "  FILES                   = %d\r\n", index);
     fprintf (label_file, "  ENCODING_TYPE           = PNG\r\n");
     fprintf (label_file, "  DESCRIPTION             = \"\r\n");
     if (0) {
       fprintf (label_file,
                "    These files are Portable Network Graphics (PNG version 1.0, RFC 2083)\r\n");
       fprintf (label_file,
                "    images of CASSINI/RPWS full resolution WBR and WFR Spectrograms\r\n");
       fprintf (label_file,
                "    (Both full screen and thumbnail images).\r\n");
     } else {
       fprintf (label_file,
                "    These files are Portable Network Graphics (PNG Specification,\r\n");
       fprintf (label_file,
                "    Second Edition, ISO/IEC 15948:2003 E) images of Cassini RPWS\r\n");
       fprintf (label_file,
                "    full resolution Wideband and Waveform Spectrograms.\r\n");
       fprintf (label_file,
                "    (Both full screen and thumbnail images).\r\n");
     }

     fprintf (label_file, "                            \"\r\n");
     fprintf (label_file, "  SOFTWARE_VERSION_ID= \"%s %s\"\r\r\n", Title,
              Version);
     fprintf (label_file, "  END_OBJECT              = PNG_DOCUMENT\r\n");
     fprintf (label_file, "END\r\n");
     /*
      * fprintf(stdout, "%s %d fclose(%p);\r\n", __FILE__, __LINE__, label_file); /*
      */
     /*
      * fflush(stdout); /*
      */
     fclose (label_file);
     index = -1;
     break;
  }
}
