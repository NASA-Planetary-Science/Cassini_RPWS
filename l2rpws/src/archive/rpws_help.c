#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <das2/das1.h>

/* Cassini Stuff */
#include <rtiu.h>
#include <archive.h>

/* Local Stuff */
#include "rpws_direct.h"
#define _rpws_archive_
#include "rpws_archive.h"
#include "bis.h"
#include "rpws_label.h"
#include "rpws_fsw_ver.h"

static char *Version = { "V1.10" };
static char *Version_0 = { VERSION_0 };

/* Copius use of externs (not in a header even) is a bit messy */
extern char* g_sDatabase;
extern char *Title;
extern char *Version;
extern char *w_directory[];
extern int KWBR60;
extern int KWFR60;
extern int KANCIL;
extern int bisflag;
extern struct DATABASE_TIME_TAG database_start_time;
extern char database_sclk_start[];
extern int database_sclk_start_i;
extern struct DATABASE_TIME_TAG database_stop_time;
extern char database_sclk_stop[];
extern int database_sclk_stop_i;
extern char *Label_Ver;
extern char *Mission_Phase;
extern char *Target;
extern char *Coordinate;
extern char *Orbit;
extern char *Product[];
extern int split_stim_flag;
extern int spice_flag;
extern int maximum_difference;
extern int Line_Pad;
extern int ephem_flag;
extern char Delimiter;
extern int debug_flag;
extern int namefile;
extern int maximum_byte_count;

int rpws_help (int argc, char *argv[], FILE * output)
{
  fprintf (output, "\n");
  fprintf (output, "(%s) %s %s   HELP SCREEN\n", argv[0], Title, Version);
  fprintf (output, "\n");
  fprintf (output, "       Record Format:\n");

/* #ifdef ARCHIVE_TIME_V1 */
  fprintf (output, "           struct sclk        4/2 octets MSB\n");
  fprintf (output, "           struct scet        2/4 octets MSB\n");
/* #endif */

/*#ifdef ARCHIVE_TIME_V2
  fprintf (output, "           double et          8 octets IEEE double MSB\n");
  fprintf (output, "           struct sclk        4/2 octets MSB\n");
#endif

#ifdef ARCHIVE_TIME_V3
  fprintf (output, "           char scet[24];     24 octets\n");
  fprintf (output, "           struct sclk        4/2 octets MSB\n");
#endif

#ifdef ARCHIVE_TIME_V4
  fprintf (output, "           double et;	        8 octets IEEE double MSB\n");
  fprintf (output, "           struct sclk        4/2 octets MSB\n");
  fprintf (output, "           struct scet        2/4 octets MSB\n");
  fprintf (output, "           spare[8];          8 octet\n");
#endif
 */

  fprintf (output, "           record_bytes;      2 octets MSB\n");
  fprintf (output, "           samples;           2 octets MSB\n");
  fprintf (output, "           data_rti;          2 octets MSB\n");
  fprintf (output, "           validity_flag;     1 octet\n");
  fprintf (output, "           status_flag;       1 octet\n");
  fprintf (output, "           frequency_band;    1 octet\n");
  fprintf (output, "           gain;              1 octet\n");
  fprintf (output, "           antenna;           1 octet\n");
  fprintf (output, "           agc;               1 octet\n");
  fprintf (output, "           hfr_xlate;         1 octet\n");
  fprintf (output, "           sub_rti;           1 octet\n");
  fprintf (output, "           lp_dac_0;          1 octet\n");
  fprintf (output, "           lp_dac_1;          1 octet\n");
  fprintf (output, "           fsw_ver;           1 octet\n");

#ifdef ARCHIVE_TIME_V1
  fprintf (output, "           spare[3];          3 octet\n");
#endif

#ifdef ARCHIVE_TIME_V2
  fprintf (output, "           spare[1];          1 octet\n");
#endif

#ifdef ARCHIVE_TIME_V3
  fprintf (output, "           spare[1];          1 octet\n");
#endif

#ifdef ARCHIVE_TIME_V4
  fprintf (output, "           spare[7];          7 octet\n");
#endif

  fprintf (output, "           time_series[];     <samples> octet for WBR\n");
  fprintf (output, "           time_series[];     <samples*2> octet for WFR\n");
  
  fprintf (output, "This utility requires CHDO records (i.e. it is intended for use\n");
  fprintf (output, "with flight data only.  EM data will probably not work)\n");
  fprintf (output, "unless SPICE is used to extract spacecraft time (-spice).\n");
  fprintf (output, "    \n");
  fprintf (output, "This utility makes use of R files and processed U files to\n");
  fprintf (output, "produce archive data files as well as archive label files.\n");
  fprintf (output, "We also produce a text file that contains decoded STIM\n");
  fprintf (output, "packets.\n");
  fprintf (output, "\n");
  fprintf (output, "   WBR data and labels are placed in %s\n",
           w_directory[RPWS_ARCHIVE_DATA_WBRFULL]);
  fprintf (output, "   WFR data and labels are placed in %s\n",
           w_directory[RPWS_ARCHIVE_DATA_WFRFULL]);
  fprintf (output, "   BROWSE/WBR data and labels are placed in %s/%s\n",
           w_directory[RPWS_ARCHIVE_BROWSE],
           w_directory[RPWS_ARCHIVE_BROWSE_WBR]);
  fprintf (output, "   BROWSE/WFR data and labels are placed in %s/%s\n",
           w_directory[RPWS_ARCHIVE_BROWSE],
           w_directory[RPWS_ARCHIVE_BROWSE_WFR]);
  fprintf (output, "   STIM data is placed in %s/STIM.TAB\n",
           w_directory[RPWS_ARCHIVE_DATA_ANCIL]);
  fprintf (output, "   HTML files are placed in %s/*.HTM\n",
           w_directory[RPWS_ARCHIVE_EXTRAS]);
  fprintf (output, "   Shell scripts for browse data are placed in %s/*.BAT\n",
           w_directory[RPWS_ARCHIVE_script]);
  fprintf (output, "   Start/Stop times are placed in %s/TIME_FILE.TXT\n",
           w_directory[RPWS_ARCHIVE_EXTRAS]);
  fprintf (output, "   Stop time is the time of the last sample in the \n");
  fprintf (output, "     dataset (not the first sample), so it will probably\n");
  fprintf (output, "     not be on a proper RTI boundary.\n");
  fprintf (output, "   Stop time shown in label files for single record files\n");
  fprintf (output, "    indicate the same point in time for start and stop.\n");
  fprintf (output, "    This is an indication that the dataset is the result of \n");
  fprintf (output, "    a fragmented minipacket resulting from lost data.\n");
  fprintf (output, "    A short record with incorrect status results in a calculated\n");
  fprintf (output, "    stop time that is within a millisecond of the start time.\n");
  fprintf (output, "    \n");
  fprintf (output, "    \n");
  fprintf (output, "  KEEP IN MIND that file split is achieved by\n");
  fprintf (output, "    opening files in append mode (creating a new\n");
  fprintf (output, "    file when necessary).  REMOVE DATA FILES \n");
  fprintf (output, "    PRIOR TO RUNNING THIS ROUTINE.\n");
  fprintf (output, "  We also attemp to minimize file open/close activity\n");
  fprintf (output, "    by keeping many files open at one time.  In the\n");
  fprintf (output, "    event that we exceed the open file limit, all \n");
  fprintf (output, "    open data output files are closed and the pending\n");
  fprintf (output, "    open is re-attempted.\n");
  fprintf (output, "  A postscript label file is produced that may be used\n");
  fprintf (output, "    to label the resulting CD.  This label is placed\n");
  fprintf (output, "    in the EXTRAS directory so that when duplicates\n");
  fprintf (output, "    are made of the archive CD, it may be labeled with\n");
  fprintf (output, "    the same label as the original.\n");
  fprintf (output, "    \n");
  fprintf (output, "\f");
  fprintf (output, "  %s\n", rpws_ver_ver ());
  rpws_ver_help (output, 0);
  fprintf (output, "\f");
  fprintf (output, "    \n");
  fprintf (output, "  -------- Documentation Dump ---------------\n");
  fprintf (output, "    \n");
  fprintf (output, "  -cdrom          CD-ROM directory structure \n");
  fprintf (output, "                  for PDS archives.\n");
  fprintf (output, "  -essay          Extended documentation\n");
  fprintf (output, "    \n");
  fprintf (output, "  -tdump          dump the IEB/FSW/MPN/TGT version table\n");
  fprintf (output, "  -t2dump         dump the IEB/FSW/MPN/TGT version table\n");
  fprintf (output, "                    2 time columns)\n");
  fprintf (output, "                    (-times is used to load this table)\n");
  fprintf (output, "    \n");
  fprintf (output, "  -------- File Selection ---------------\n");
  fprintf (output, "  -files xx       list of files to process\n");
  fprintf (output, "  -database       Use database(%s)\n", g_sDatabase);
  fprintf (output, "  -dbase_st       Data start time (uses file database) (%04d-%03dT%02d:%02d:%02d)\n",
           database_start_time.year, database_start_time.yday,
           database_start_time.hour, database_start_time.minute,
           (int) database_start_time.second);
  fprintf (output,"  -dbase_sp       Data stop time  (uses file database) (%04d-%03dT%02d:%02d:%02d)\n",
           database_stop_time.year, database_stop_time.yday,
           database_stop_time.hour, database_stop_time.minute,
           (int) database_stop_time.second);
  fprintf (output, "  -stsp           Display both start and stop times on CD\n");
  fprintf (output, "                    Normally, the stop time is only shown on the\n");
  fprintf (output, "                    last CD of a set.\n");
  fprintf (output, "  -wbrdir         Directory to stash WBR files & labels (%s)\n",
           w_directory[RPWS_ARCHIVE_DATA_WBRFULL]);
  fprintf (output, "  -wfrdir         Directory to stash WFR files & labels (%s)\n",
           w_directory[RPWS_ARCHIVE_DATA_WFRFULL]);
  fprintf (output, "                      These files are overwritten.  Do not perform\n");
  fprintf (output, "                      a run that processes less than a complete period\n");
  fprintf (output, "                      (%d minutes for WBR and %d minutes for WFR)\n",
           KWBR60, KWFR60);
  fprintf (output, "                      \n");
  fprintf (output, "  -ancildir       Directory to stash Ancillary data(%s)\n",
           w_directory[RPWS_ARCHIVE_DATA_ANCIL]);
  fprintf (output, "                      \n");
  fprintf (output, "  -extradir       Directory to stash Stim, Start/Stop and filename list(%s)\n",
           w_directory[RPWS_ARCHIVE_EXTRAS]);
  fprintf (output, "                      This file is appended to with each run so it is\n");
  fprintf (output, "                      possible to build the archive image with several\n");
  fprintf (output, "                      runs (i.e. day-by-day).  When scanning the file,\n");
  fprintf (output, "                      you would need to accumulate start/sopt times.\n");
  fprintf (output, "  -scriptdir      Directory to stash generated script files(%s)\n",
           w_directory[RPWS_ARCHIVE_script]);
  fprintf (output, "                      This is to allow directory linking\n");
  fprintf (output, "  -splitstim      Splits STIM records into day files\n");
  fprintf (output, "  -splitstim100   Splits STIM records into 100 day files\n");
  fprintf (output, "  -splitstim365   Splits STIM records into year files\n");
  fprintf (output, "  -splitstim1100  Splits STIM records into 100 day files\n");
  fprintf (output, "                    in 100 day subdirectory\n");
  fprintf (output, "  -splitstim1365  Splits STIM records into year files\n");
  fprintf (output, "                    in 1 year subdirectory\n");
  fprintf (output, "                    default: ");
  switch (split_stim_flag) {
   case 0:
     fprintf (output, "a single file.\n");
     break;
   case 1:
     fprintf (output, "one day files.\n");
     break;
   case 100:
     fprintf (output, "100 day files.\n");
     break;
   case 365:
     fprintf (output, "year-long files\n");
     break;
   default:
     fprintf (output, "unknown setting\n");
     break;
  }
  fprintf (output, "  -timeflag xx    Time formatting flag\n");
  fprintf (output, "                      0x01    Start time yyyy-mm-ddT  (browse)\n");
  fprintf (output, "                      0x02    Start time yyyy-mm-ddT  (master browse)\n");
  fprintf (output, "                      \n");
  fprintf (output, "                      \n");
  fprintf (output, "  -times xx       IEB/FSW/MPN/TGT version table\n");
  fprintf (output, "                      (default: %s)\n", namefile);
  fprintf (output, "  -size nn        Data volume, expredded in megabytes\n");
  fprintf (output, "                    data archiving will stop when this much data \n");
  fprintf (output, "                    has accumulated.  This will result partially\n");
  fprintf (output, "                    filled files, but the label file will record the\n");
  fprintf (output, "                    stop time of the last record allowed.\n");
  fprintf (output, "                  Default value is %d\n", (int) (maximum_byte_count / 1000000));
  fprintf (output, "                  0 disables byte counting\n");
  fprintf (output, "  -plot_full      Browse Images on hour/day boundary\n");
  fprintf (output, "  -mask           Attempt to fix record length problems (MSF)\n");
  fprintf (output, "  -zero           Align start & stop times on hour boundary\n");
  fprintf (output, "    \n");
  fprintf (output, "  -------- Time -------------------------\n");
  fprintf (output, "  WBR sub-RTI notes\n");
  fprintf (output, "    The sub-RTI field is flagged as valid only for FSW V2.6 \n");
  fprintf (output, "      (and newer).  Older FSW versions did not correctly \n");
  fprintf (output, "      recover this information from the hardware.\n");
  fprintf (output, "    WBR time-tags are always on the RTI, the finer time is\n");
  fprintf (output, "      not saved in the record time tag.  It will be necessary\n");
  fprintf (output, "      to decode the sub-RTI field (when the valid bit is set)\n");
  fprintf (output, "      to obtain finer time tags for WBR data.  \n");
  fprintf (output, "    sub-RTI decoding:\n");
  fprintf (output, "                     milliseconds_into_RTI = 250-sub_RTI/2\n");
  fprintf (output, "    \n");
  fprintf (output, "  -wbr_size nn    Size (minutes) of archive file (%d)\n", KWBR60);
  fprintf (output, "  -wfr_size nn    Size (minutes) of archive file (%d)\n", KWFR60);
  fprintf (output, "  -spice          Use SPICE kernel to calcualte time\n");
  fprintf (output, "                    report differences, keep u-file\n");
  fprintf (output, "                    file grouping %s\n", spice_flag & 1 ? "(default)" : "");
  fprintf (output, "  -spfix          Use SPICE kernel to calcualte time\n");
  fprintf (output, "                    patch spice time in prior to generating\n");
  fprintf (output, "                    filenames (total fixup), may require\n");
  fprintf (output, "                    sorting following file creation. %s\n", spice_flag & 2 ? "(default)" : "");
  
  if(getenv("CAS_TIME_KERNELS"))
	  fprintf (output, "  -kernel xx        Specify spice metakernel file (%s)\n", getenv("CAS_TIME_KERNELS"));
 else
     fprintf (output, "  -kernel xx        Specify spice metakernel file (no default)\n");
 
  fprintf (output, "  -diff nn        Maximum difference between chdo and spice time (%d)\n", maximum_difference);
  fprintf (output, "                    ZERO suppresses messages.\n");
  fprintf (output, "  -subrti         Dump Sub-RTI translation table\n");
  fprintf (output, "    \n");
  fprintf (output, "  -------- Label File Processing --------\n");
  fprintf (output, "  -lblver xx      Alters the DATA_SET_ID version string (%s)\n", Label_Ver);
  fprintf (output, "  -phase xx       Alters the MISSION_PHASE_NAME string(%s)\n", Mission_Phase);
  fprintf (output, "  -target xx      Alters the TARGET_NAME string(%s)\n", Target);
  fprintf (output, "                    For a single TARGET, EARTH or SOLAR_SYSTEM are OK \n");
  fprintf (output, "                    For multiple TARGET use \"EARTH,VENUS\"\n");
  fprintf (output, "                    and the program will emit the label records with quotes\n");
  fprintf (output, "  -coordinate xx  Alters the coordinate string(%s)\n", Coordinate);
  fprintf (output, "  -orbit xx       Alters the orbit string(%s)\n", Orbit);
  fprintf (output, "  -body xx        String modifies the name of the perl script used to produce\n");
  fprintf (output, "                    browse images.  Intended to allow selection of orbit parameters\n");
  fprintf (output, "                    on thumbnail and browse plots\n");
  fprintf (output, "  -cd_index n     Typically a number, used to sequence CD on the printed label.\n");
  fprintf (output, "  -cd_count n     Typically a number, used to sequence CD on the printed label.\n");
  fprintf (output, "                    This shows up as CD/DVD cd_index/cd_count on the\n");
  fprintf (output, "                    top of the CD/DVD label.\n");
  fprintf (output, "  -cd_format xx   Typically ISO9660 or UDF, identifies the file system\n");
  fprintf (output, "                    that appears on the CD\n");
  fprintf (output, "                    \n");
  fprintf (output, "  +pad            Pads label records to %d bytes %s\n", LINE_PAD, Line_Pad ? "(default)" : "");
  fprintf (output, "  -pad            No additional padding %s\n", Line_Pad ? "" : "(default)");
  fprintf (output, "  +ephem          Add 'ephemeris time' tags to label file %s\n",
           ephem_flag ? "(default)" : "");
  fprintf (output, "  -ephem          Remove 'ephemeris time' tags from label file %s\n",
           ephem_flag ? "" : "(default)");
  fprintf (output, "  -stim_delim x   Column delimiter in STIM.TAB (default \"%c\")\n",
           Delimiter);
  fprintf (output, "    \n");
  fprintf (output, "  -html401        Make HTML files 4.01 compliant\n");
  fprintf (output, "    \n");
  fprintf (output, "  -bisflag nn     Browse Image Selection flags\n");
  fprintf (output, "                    default value: %4.2X\n", bisflag);
  fprintf (output, "                      BISSTART   %4.2X\n", BISSTART);
  fprintf (output, "                              Start time on thumbnail\n");
  fprintf (output, "                      BISSTOP    %4.2X\n", BISSTOP);
  fprintf (output, "                              Stop time on thumbnail\n");
  fprintf (output, "                      BISBAND    %4.2X\n", BISBAND);
  fprintf (output, "                              Band comment on thumbnail\n");
  fprintf (output, "                      BISDETAIL  %4.2X\n", BISDETAIL);
  fprintf (output, "                              Filename shown on thumbnail\n");
  fprintf (output, "                      BISDAY     %4.2X\n", BISDAY);
  fprintf (output, "                              Start time on thumbnail\n");
  fprintf (output, "                      BISPARENT  %4.2X\n", BISPARENT);
  fprintf (output, "                              Link to Parent Directory\n");
  fprintf (output, "                      BISOTHER   %4.2X\n", BISOTHER);
  fprintf (output, "                              Link to other band\n");
  fprintf (output, "                      BISVERSION %4.2X\n", BISVERSION);
  fprintf (output, "                              HTML comment for version string\n");
  fprintf (output, "                      BISFILENAME %4.2X\n", BISFILENAME);
  fprintf (output, "                              Filename on thumbnail\n");
  fprintf (output, "                      BISCEWLLSPACING %4.2X\n",
           BISCELLSPACING);
  fprintf (output, "                              <TABLE cellspacing=10>\n");
  fprintf (output, "    \n");
  fprintf (output, "  -------- Debug Control ----------------\n");
  fprintf (output, "  -debug 0xXX     Debugging (0x%04X)\n", debug_flag);
  fprintf (output, "                  0x0800000         Suppress Archive write\n");
  fprintf (output, "                  0x0400000         Suppress STIM Write\n");
  fprintf (output, "                  0x0200000         Suppress Raw Write\n");
  fprintf (output, "                  0x0100000         HTML 4.01 (0 means HTML3.2\n");
  fprintf (output, "                  0x 080000         suppress data write\n");
  fprintf (output, "                  0x 040000         VERBOSE label file debug\n");
  fprintf (output, "                  0x 020000         STIM file debug\n");
  fprintf (output, "                  0x 010000         master HTML debug\n");
  fprintf (output, "                  0x  08000         Input file messages\n");
  fprintf (output, "                  0x  04000         SC Event message\n");
  fprintf (output, "                  0x  02000         Event message\n");
  fprintf (output, "                  0x  01000         Output file messages\n");
  fprintf (output, "                  0x   0800         write record message\n");
  fprintf (output, "                  0x   0400         label file debug\n");
  fprintf (output, "                  0x   0200         \n");
  fprintf (output, "                  0x   0100         \n");
  fprintf (output, "                  0x    080         wbr file size calculations\n");
  fprintf (output, "                  0x    040         label file progress\n");
  fprintf (output, "                  0x    020         \n");
  fprintf (output, "                  0x    010         \n");
  fprintf (output, "                  0x     08         \n");
  fprintf (output, "                  0x     04         \n");
  fprintf (output, "                  0x     02         \n");
  fprintf (output, "                  0x     01         \n");
  fprintf (output, "\n");
  fprintf (output, "  -prodverid N\n");
  fprintf (output, "     By default the PRODUCT_VERISON_ID field in the PDS labels\n");
  fprintf (output, "     will be \"1\".  Use this to set to a different integer\n");
  fprintf (output, "     value.\n");
  fprintf (output, "    \n");
  return 0;
}
