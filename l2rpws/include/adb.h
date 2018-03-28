#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

/************************************************/

/*	adb		mY dATA bASE		*/

/*			read Cassini database	*/

/************************************************/

#define ADB_VERSION "V0.0"


 /***********************************************/
 /*
  * do NOT define _adb_ in application code !!! 
  */

 /***********************************************/

#ifdef __adb__
#define ADB_NAME_BUFFER 127
#define ADB_LINE_BUFFER 255
#define ADB_DATA_BUFFER 16384
#else

  /*
   *    Library version string
   */
extern char *ADB_Version;
extern int ADB_debug;

  /*
   *    Open database stream:
   *            REQUIRED        start_time      desired start time
   *            may be NULL     stop_time       desired stop time
   *            may be NULL     path            directory where database
   *                                            is located.  Default to
   *                                                    /opt/project/cassini/data/database
   *            may be NULL     name            database filename.  Default
   *                                                    CassiniJPL.db
   *            may be ZERO     adb_packet_type ADB_PACKET_TYPE enum
   *                                            selectes U/R files
   *
   *    returns pointer to database access structure ADB
   *      this structure contains file pointers for the
   *      database index and the individual files, names for
   *      the database and currently active file, requested
   *      start and stop times, and the data.
   *
   *    ADB_open allocates a 65K buffer to read RPWS u-file and
   *      r-file records, so you do not need to allocate this buffer.
   *
   *    TIME LIMITS:
   *        in order to correctly access the database and associated
   *        files, there is a restriction that data prior to 
   *        CASSINI_1999_298_SCLK (as defined in rpws_sclk.h)
   *        which is 1997-298T00:00:00 may not be accessed.
   *        (there are a few WFR records in the file 
   *        t19972972200.u00 that may need to be reprocessed)
   *      Requesting data before this time causes the 
   *        library routines to substitute the SCLK value
   *        CASSINI_1999_298_SCLK.
   *      In other words, you can ask for pre-launch data, but you
   *        aren't going to get it.  FLIGHT DATA ONLY!
   */
struct ADB *ADB_open (char *start_time,
                      char *stop_time, char *path, int packet_type);
  /*
   *    Close stream and release adb structure and buffers.
   *    We should be able to have multiple streams open at the same
   *      time (i.e. multiple handles for the same period).
   */
int ADB_close (struct ADB *adb);

  /*
   *    Return data in selected period.  Data with an event SCLK
   *    outside the selected range is NOT returned to the user.
   *    NOTE NOTE NOTE  this uses start time, it does not figure out
   *    how long each data acquisition takes and (in other words based
   *    strictly on the SCLK that marks the start of a data capture).
   *    We DO, however, add 1 tick to the stop SCLK when calculating 
   *    end times)
   */
unsigned char *ADB_read (struct ADB *adb);

  /*
   *    Uses ADB_read (so above applies with respect to 
   *      time period returned.
   *    Selects STIM data based on flag
   */
unsigned char *ADB_read_stim (struct ADB *adb, int flag);
#endif

#define ADB_TOKENS 6
  /*
   *    Control structure used by most of the ADB
   *    routines.
   */
struct ADB
{
  struct ADB *link;                     /* link reserved for USER !!! */
  char *datafile_path;
  char *datafile_name;                  /* data file name */
  char *datalabel_name;                 /* data label name */
  FILE *datafile_file;                  /* data file */
  FILE *datalabel_file;                 /* data label */
  int data_year;
  int data_day;
  int adb_file_type;                    /*  */
  int adb_file_records;
  int requested_start_sclk;             /* parameter from open */
  int requested_stop_sclk;              /* parameter from open */
  int current_sclk;                     /* SCLK of current record */
  short current_fine;                   /* fine (sub-second, RTI<<5) */
  unsigned char current_part;           /*  partition number (always 1) */
  unsigned char current_RTI;            /*  RTI of current record */
  unsigned char *buffer;                /* data record */
  struct stat status_buffer;            /* file status; see: "man -s2 stat" */
  struct timeval open_time;             /* uSec of when open was called */
  struct timeval read_time;             /* uSec of when 1st. read was called */
  int database_line_count;              /* where we are in the file */
  char adb_file_key[32];
  int spare;                            /* problem??? */
};

  /*
   *    Broken down detail record:  Returned by ADB_detail
   */
struct ADB_DETAIL
{
  double start_et;                      /* seconds from J2000 eopch */
  double stop_et;
  int start_sclk;                       /* starting SCLK (seconds) */
  int stop_sclk;
  int flags;                            /* Which records are present in this file */
  char *pathname;                       /* path to file */
  char *filename;                       /* filename */
  char *detail_line;                    /* Complete detail from database */
};

enum
{
  ADB_UNK_FILE = 0x000000,
  ABD_100_DAY = 0x200000,
  ADB_STRING = 0x100000,
  ADB_DATA = 0x010000,
  ADB_HSK = 0x020000,
  ADB_DATA_STIM = 0x310001,
  ADB_HSK_ENG = 0x320002,
  ADB_HSK_BFDL = 0x020004,
  ADB_HSK_DUST = 0x020008,
  ADB_HSK_HOUSE = 0x020010,
  ADB_HSK_IPC = 0x020020
} ADB_PACKET_TYPE;

enum
{
  ADB_STIM_LPSW = 0x0001,
  ADB_STIM_HSND = 0x0002,
  ADB_STIM_STIM = 0x0004,
  ADB_STIM_HDMP = 0x0008,
  ADB_STIM_HCAL = 0x0010,
  ADB_STIM_20 = 0x0020,
  ADB_STIM_40 = 0x0040,
  ADB_STIM_80 = 0x0080
} ADB_STIM_FLAG;
