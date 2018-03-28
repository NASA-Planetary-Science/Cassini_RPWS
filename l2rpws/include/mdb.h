#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

/************************************************/

/*	mdb		mY dATA bASE		*/

/*			read Cassini database	*/

/************************************************/

#define MDB_VERSION "V3.2r"


 /***********************************************/
 /*
  * do NOT define _mdb_ in application code !!! 
  */

 /***********************************************/

#ifdef __mdb__
#define MDB_NAME_BUFFER 127
#define MDB_LINE_BUFFER 255
#define MDB_DATA_BUFFER 65535
#else

  /*
   *    Library version string
   */
extern char *MDB_Version;
extern int MDB_debug;

  /*
   *    Open database stream:
   *            REQUIRED        start_time      desired start time
   *            may be NULL     stop_time       desired stop time
   *            may be NULL     path            directory where database
   *                                            is located.  Default to
   *                                                    /opt/project/cassini/data/database
   *            may be NULL     name            database filename.  Default
   *                                                    CassiniJPL.db
   *            may be ZERO     mdb_packet_type MDB_PACKET_TYPE enum
   *                                            selectes U/R files
   *
   *    returns pointer to database access structure MDB
   *      this structure contains file pointers for the
   *      database index and the individual files, names for
   *      the database and currently active file, requested
   *      start and stop times, and the data.
   *
   *    MDB_open allocates a 65K buffer to read RPWS u-file and
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
struct MDB *MDB_open (const char *start_time,
                      const char *stop_time,
                      const char *path, const char *name, int packet_type);

  /*
   *    Rewind stream.
   *      Should return the same MDB structure it was passed.
   *      Problem is indicated by NULL return.
   */
struct MDB *MDB_rewind (struct MDB *mdb, int flag);

  /*
   *    Close stream and release mdb structure and buffers.
   *    We should be able to have multiple streams open at the same
   *      time (i.e. multiple handles for the same period).
   */
int MDB_close (struct MDB *mdb);

  /*
   *    Convert SCLK/SCET string to binary SCLK
   *            Partition assumed to be 1
   *            Fine forced to 0
   */
int MDB_time (const char *time_string);

  /*
   *    Convert SCLK/SCET string to proper SCET string.  This takes
   *    any of the acceptable time strings, converts to SCLK and
   *    then back to SCET.  Gets all the trailing zeros stuck in.
   *    Real floating_point-ish, tends to round a bit...
   */
char *MDB_time_SCET (const char *time_string);

  /*
   *    Convert SCLK/SCET string to et.  Use for time
   *    comparison.
   */
double MDB_time_et (const char *time_string);

  /*
   *    Time routines versiuon string
   */
char *MDB_time_ver (void);

  /*
   *    Expanded read, return all data in selected files.
   *    this will return all data in files that cover
   *    the requested period.  Which is to say: you
   *    may get some records at the begining of the
   *    requested hout at the beginging as well as
   *    some of the records at the end of the last 
   *    filea that are after the stop period.
   */
struct RPWS_buffer *MDB_read_all (struct MDB *mdb);

  /*
   *    Return data in selected period.  Data with an event SCLK
   *    outside the selected range is NOT returned to the user.
   *    NOTE NOTE NOTE  this uses start time, it does not figure out
   *    how long each data acquisition takes and (in other words based
   *    strictly on the SCLK that marks the start of a data capture).
   *    We DO, however, add 1 tick to the stop SCLK when calculating 
   *    end times)
   */
struct RPWS_buffer *MDB_read (struct MDB *mdb);

  /*
   *    Return data in selected period from specified stream.
   *            stream is taken from various *_status.h files:
   *
   *    Note that CDS_Packet_ID values are only valid for R files
   *      while the other values appear in the minipacket files.
   *
   *            R-FILES:
   *                    CDS_Packet_ID_UnSegmented_LRS_I
   *                    CDS_Packet_ID_UnSegmented_HRS_I
   *                    CDS_Packet_ID_Segmented_LRS_I
   *                    CDS_Packet_ID_Segmented_HRS_I
   *                    CDS_Packet_ID_Housekeeping_ROM
   *                    CDS_Packet_ID_Housekeeping_Maintenance
   *                    CDS_Packet_ID_Housekeeping_Deploy
   *                    CDS_Packet_ID_Housekeeping_Science
   *            all others:
   *                    DUST_packet_ID_value
   *                    HFR_packet_ID_value
   *                    LFDR_packet_ID_value
   *                    LP_packet_ID_value
   *                    MFR_packet_ID_value
   *                    MRO_packet_ID_value
   *                    WBR_packet_ID_value
   *                    WFR_packet_ID_value
   *                    STIM_packet_ID_value
   *
   *    Data with an event SCLK outside the selected range is 
   *    NOT returned to the user.  NOTE NOTE NOTE  this uses 
   *    start time, it does not figure out how long each data 
   *    acquisition takes and (in other words based strictly on
   *    the SCLK that marks the start of a data capture).  We DO, 
   *    however, add 1 tick to the stop SCLK when calculating 
   *    end times)
   */
struct RPWS_buffer *MDB_read_stream (struct MDB *mdb, int stream);

  /*
   *    Return stat buffer (man -s2 stat)
   *    Call MDB_open with start/stop then call this
   *    guy repeatedly, until a 0 is returned,
   *    to get results.
   */
int MDB_stat (struct MDB *mdb, struct stat *stat_buf, char *filename);

  /*
   *    Return database detail lines (between start/stop)
   *    Call MDB_open with start/stop then call this
   *    guy repeatedly, until a 0 is returned,
   *    to get results.
   */
struct MDB_DETAIL *MDB_detail (struct MDB *mdb);


 /*
  *     Spit detailed help to the given file
  */
int MDB_help (FILE * file);
#endif

#define MDB_TOKENS 6
  /*
   *    Control structure used by most of the MDB
   *    routines.
   */
struct MDB
{
  struct MDB *link;                     /* link reserved for USER !!! */
  char *database_name;                  /* CassiniJPL.db */
  FILE *database_file;                  /* CassiniJPL.db */
  char *database_detail_line;           /* unaduiltered detail line */
  char *database_line;                  /* scet scet sclk sclk filename flags */
  char *database_line_tokens[MDB_TOKENS];
  char *datafile_name;                  /* data file name */
  FILE *datafile_file;                  /* data file */
  int mdb_packet_type;                  /* u* r00 h00 s00 l00 */
  int database_start_sclk;              /* sclk extracted from database_line */
  int database_stop_sclk;               /* sclk extracted from database_line */
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
  int spare;                            /* problem??? */
};

  /*
   *    Broken down detail record:  Returned by MDB_detail
   */
struct MDB_DETAIL
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
  MDB_UNK_FILE = 0x0000,
  MDB_U_FILE = 0x0100,
  MDB_R_FILE = 0x0200,
  MDB_H_FILE = 0x0400,
  MDB_S_FILE = 0x0800,
  MDB_L_FILE = 0x1000
} MDB_PACKET_TYPE;


#define MDB_DEBUG_DATABASE 1
#define MDB_DEBUG_DATAFILE 2

#define MDB_FLAGS_Mask   ((unsigned long)0x0000FFFF)
#define MDB_FLAGS_Stim   ((unsigned long)0x00000001)
#define MDB_FLAGS_MFR    ((unsigned long)0x00000002)
#define MDB_FLAGS_HFR    ((unsigned long)0x00000004)
#define MDB_FLAGS_ID3    ((unsigned long)0x00000008)
#define MDB_FLAGS_LP     ((unsigned long)0x00000010)
#define MDB_FLAGS_ID5    ((unsigned long)0x00000020)
#define MDB_FLAGS_ID6    ((unsigned long)0x00000040)
#define MDB_FLAGS_LFDR   ((unsigned long)0x00000080)
#define MDB_FLAGS_WFR    ((unsigned long)0x00000100)
#define MDB_FLAGS_ID9    ((unsigned long)0x00000200)
#define MDB_FLAGS_ID10   ((unsigned long)0x00000400)
#define MDB_FLAGS_Dust   ((unsigned long)0x00000800)
#define MDB_FLAGS_ID12   ((unsigned long)0x00001000)
#define MDB_FLAGS_MRO    ((unsigned long)0x00002000)
#define MDB_FLAGS_WBR    ((unsigned long)0x00004000)
#define MDB_FLAGS_Fill   ((unsigned long)0x00008000)
