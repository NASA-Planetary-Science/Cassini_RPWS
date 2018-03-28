#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

/************************************************/

/*      PDSDB            PDS dATA bASE          */

/*                      read PDS data (RPWS)    */

/************************************************/

#define PDSDB_VERSION "V0.0"

#define PDSDB_FILE_RPWS_KEY_PARAMETERS "RPWS_KEY_PARAMETERS"
#define PDSDB_FILE_RPWS_LOW_RATE_FULL  "RPWS_LOW_RATE_FULL"
#define PDSDB_FILE_RPWS_RAW_COMPLETE  "RPWS_RAW_COMPLETE"
#define PDSDB_FILE_RPWS_WAVEFORM_FULL  "RPWS_WAVEFORM_FULL"
#define PDSDB_FILE_RPWS_WIDEBAND_FULL  "RPWS_WIDEBAND_FULL"

#define PDSDB_STREAM_RPWS_KEY_PARAMETERS      1
#define PDSDB_STREAM_RPWS_KEY_HEADER          2
#define PDSDB_STREAM_RPWS_LOW_RATE_FULL       3
#define PDSDB_STREAM_RPWS_LOW_RATE_LRFULL     4
#define PDSDB_STREAM_RPWS_LOW_RATE_TIME       5
#define PDSDB_STREAM_RPWS_LOW_RATE_FREQUENCY  6
#define PDSDB_STREAM_RPWS_RAW_COMPLETE        7
#define PDSDB_STREAM_RPWS_WAVEFORM_FULL       8
#define PDSDB_STREAM_RPWS_WIDEBAND_FULL       9

#define PDSDB_RECORDNAME_RPWS_KEY_PARAMETERS     "LRKEY_SPECTRAL_DENSITY_TABLE"
#define PDSDB_RECORDNAME_RPWS_KEY_HEADER         "LRKEY_FREQUENCY_TABLE"
#define PDSDB_RECORDNAME_RPWS_LOW_RATE_FULL      "SPECTRAL_DENSITY_TABLE"
#define PDSDB_RECORDNAME_RPWS_LOW_RATE_LRFULL    "LRFULL_TABLE"
#define PDSDB_RECORDNAME_RPWS_LOW_RATE_TIME      "TIME_TABLE"
#define PDSDB_RECORDNAME_RPWS_LOW_RATE_FREQUENCY "FREQUENCY_TABLE"
#define PDSDB_RECORDNAME_RPWS_WAVEFORM_FULL      "TIME_SERIES"
#define PDSDB_RECORDNAME_RPWS_WIDEBAND_FULL      "TIME_SERIES"

#define PDSDB_DUMP_PDSDB	1
#define PDSDB_DUMP_INDEX	2
#define PDSDB_DUMP_TIME		4

#ifdef __pdsdb__
#define PDSDB_NAME_BUFFER 127
#define PDSDB_LINE_BUFFER 255
#define PDSDB_DATA_BUFFER 65535
#else

extern char *PDSDB_Version;                                                /********************************/
struct PDSDB *PDSDB_open (char *start_time,     /*       desired stat time      */
                          char *stop_time,      /* (opt) desired stop time      */
                          char *path,   /* (opt) path to index/cumindex */
                          char *name,   /* (opt) name of index LABEL    */
                          char *type,   /*       type of file to read   */
                          char *subtype,        /*  xxxxxSUB.LBL                */
                          char *record);        /* which record to read         */

                                                   /********************************/
unsigned char *PDSDB_read (struct PDSDB *pdsdb);        /* database structure           */

                                                   /********************************/
unsigned char *PDSDB_read_stream (struct PDSDB *pdsdb,  /* database structure    */
                                  int stream);  /*        what type of data     */

                                                   /********************************/
unsigned char *PDSDB_close (struct PDSDB *pdsdb);       /* Close open files             */

                                                   /********************************/
int PDSDB_dump (FILE * out, struct PDSDB *pdsdb, int flag);
#endif

/**/

 /***************************************/
 /*
  * PDSDB control structure             
  */

 /***************************************/
#define PDSDB_TOKENS 16
struct PDSDB
{
  struct PDSDB *link;                   /* link reserved for USER               */

        /************************************************* INDEX.LBL/INDEX.TAB			*/
  char *database_path;                  /* path to archive volume               */
  char *database_name;                  /* usually INDEX.TAB                    */
  FILE *database_file;                  /*              INDEX.TAB               */
  int database_rec_num;                 /* current record number                */
  char *database_detail_line;           /* unadultered detail                   */
  int database_seek;                    /* offset to 1st detail line (bytes)    */
  int database_records;                 /* INDEX.TAB record count               */
  int database_bytes;                   /* INDEX.TAB record len                 */
  char *database_fields[PDSDB_TOKENS];  /* db fields                    */
  char *pdsdb_open_type;                /* STANDARD_DATA_PRODUCT_ID               */
  char *pdsdb_open_subtype;             /* FILE_SPECIFICATION_NAME                */
  char *pdsdb_open_record;              /* string in lable for offset           */

        /************************************************* DATA.LBL				*/
  char *datalabel_volume;               /*   Volume name                     */
  char *datalabel_name;                 /* data file name                    */
  FILE *datalabel_file;                 /* data file                         */
  int datalabel_sclk_cnt_start;         /*   start/stop from                 */
  int datalabel_sclk_cnt_stop;          /*   the label file                  */
  int datalabel_records;                /* record count                      */
  int datalabel_bytes;                  /* size of record                    */
  char *datalabel_record_name[PDSDB_TOKENS];    /*                        */
  int datalabel_seek[PDSDB_TOKENS];     /* offset to 1st. rec     */

        /************************************************* DATA.DAT				*/
  char *datafile_name;                  /* data file name                       */
  FILE *datafile_file;                  /* data file                            */
  int datafile_rec_num;                 /* current record number                */
  int datafile_stream;                  /* current stream ID                    */

        /************************************************* Search keys				*/
  int requested_start_sclk;             /* parameter from open                  */
  int requested_stop_sclk;              /* parameter from open                  */

        /************************************************* Data Buffer				*/
  unsigned char *buffer;                /* data record                          */
};
