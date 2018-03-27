 /*
  * rpws_archive.c   
  */
  
#include <SpiceUsr.h>

#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <curses.h>
#include <string.h>
#include <term.h>
#include <time.h>
#include <math.h>
#include <ulimit.h>
#include <sys/types.h>
#include <sys/stat.h>


/* Cas Includes */
#include <rtiu.h>
#include <util.h>
#include <utilf.h>
#include <utilo.h>
#include <utilt.h>
#include <UTIL_status.h>

#include <hsk_micro.h>
#include <archive.h>
#include <mdb.h>
#include <mdb_time.h>


/* Local Includes */
#include "rpws_timing.h"
#include "rpws_label.h"
#include "rpws_browse.h"
#include "rpws_fsw_ver.h"
#include "rpws_archive_cdrom.h"
#include "rpws_hfr_status.h"
#include "rpws_lp_status.h"
#include "rpws_essay.h"
#include "rpws_direct.h"
#include "rpws_sclk.h"
#include "rpws_housekeeping.h"
#include "rpws_time_patch.h"
#include "rpws_housekeeping_help.h"

#define _rpws_archive_
#include "rpws_engineering.h"
#include "rpws_alarm.h"
#include "rpws_archive.h"
#include "rpws_label_graphics.h"


#define SCIOP_TIMEOUT 10
#define BUFFER_SIZE 65536
#define RTI_MASK 0x00E0
#define A_EPOCH 2436204


/*****************************************************************************/


char *Title = { "RPWS_HSK_AR" };
char *Version = { "V6.3" };
static char *Version_0 = { VERSION_0 };
char Delimiter = { ' ' };
int Mafi_Flag = 0;

static int alarm_flag = 0;
static int eng_flag = 0;
static int dust_flag = 0;
static int bfdl_flag = 0;
static int ipc_flag = 0;
static int hsk_flag = 1;
static int force_flag = 1;

static char arglist[] = { "p:n:v:bdihzfmea" };
extern char *MDB_Version;
extern char *optarg;
extern int optind, opterr, optopt;
static int file_type = MDB_R_FILE;
static int Partition = 1;
static int SpaceCraft_ID = -82;
static int Zero_Flag = 0;

static char path[256];
static char *path_list[] = { "DATA", "HOUSEKEEPING", NULL };

#define HSK_RECORD_SIZE 192
#define HSK_DUST_RECORD_SIZE 32
#define HSK_BFDL_RECORD_SIZE 32
#define HSK_IPC_RECORD_SIZE 32

static char Label_version[] = { "V1.0" };
static char Mission_phase[8192];
static char Target[2048];
static char Coordinate[2048];
static char Orbit[2048];
static char *Filetype[] = { "PKT", "TAB", "TXT" };

static struct RPWS_LABEL label = {
  NULL,                                 /* struct RPWS_LABEL *link; */
  RPWS_LABEL_HSK,                       /* int instrument;          0=WBR 1=WFR */
  0,                                    /* int band;                 */
  0,                                    /* int mode_mask;            */

#ifdef HFR_XLATE
  HFR_XLATE * 0,                        /* short hfr_xlate;          */
#else
  0,                                    /* int hfr_xlate;            */
#endif

  0.0,                                  /* double span;              */
  0.0,                                  /* double duration;          */
  HSK_RECORD_SIZE,                      /* int dataset_size[0];      */
  HSK_RECORD_SIZE,                      /* int dataset_size[1];      */
  HSK_RECORD_SIZE,                      /* int sample_count;         */
  RPWS_LABEL_PAD_HSK,                   /* int pad_class;            */
  HSK_RECORD_SIZE,                      /* int record_size;          */
  0, 0,                                 /* int record_count[2];      */
  0,                                    /* int Label_Line_Pad;      Are labels padded to 80 colums ? */
  NULL,                                 /* FILE *filehandle;         */
  label.text_area[0],                   /* char *filename;          RPWS_LABEL_CHAR_FIELD   1 */
  label.text_area[1],                   /* char *filepath1;         RPWS_LABEL_CHAR_FIELD   2 */
  label.text_area[2],                   /* char *filepath2;         RPWS_LABEL_CHAR_FIELD   3 */
  label.text_area[3],                   /* char *filepath3;         RPWS_LABEL_CHAR_FIELD   4 */
  label.text_area[4],                   /* char *thumbname;         RPWS_LABEL_CHAR_FIELD   5 */
  label.text_area[5],                   /* char *utc_date;          RPWS_LABEL_CHAR_FIELD   6 */
  label.text_area[6],                   /* char *utc_time;          RPWS_LABEL_CHAR_FIELD   7 */
  label.text_area[7],                   /* char *sclk_start;        RPWS_LABEL_CHAR_FIELD   8 */
  label.text_area[8],                   /* char *sclk_stop;         RPWS_LABEL_CHAR_FIELD   9 */
  label.text_area[9],                   /* char *scet_start;        RPWS_LABEL_CHAR_FIELD  10 */
  label.text_area[10],                  /* char *scet_start_2;      RPWS_LABEL_CHAR_FIELD  11 */
  label.text_area[11],                  /* char *scet_stop;         RPWS_LABEL_CHAR_FIELD  12 */
  label.text_area[12],                  /* char *ephem_start;       RPWS_LABEL_CHAR_FIELD  13 */
  label.text_area[13],                  /* char *ephem_stop;        RPWS_LABEL_CHAR_FIELD  14 */
  label.text_area[14],                  /* char *plot_start;        RPWS_LABEL_CHAR_FIELD  15 */
  label.text_area[15],                  /* char *plot_start;        RPWS_LABEL_CHAR_FIELD  16 */
  label.text_area[16],                  /* char *scet_start_3;      RPWS_LABEL_CHAR_FIELD  16 */
  Label_version,                        /* char *Label_Version;      */
  Mission_phase,                        /* char *Mission_Phase;      */
  Target,                               /* char *Target;             */
  Coordinate,                           /* char *Coordinate          */
  Orbit,                                /* char *Orbit               */
  "RPWS_HOUSEKEPNG",                    /* char *Product;            */
  1,                                    /* int ProdVerId             */
  RPWS_LABEL_CHAR_FIELDS * RPWS_LABEL_MISC_BUFFER_SIZE * 0      /* char  text_area[RPWS_LABEL_CHAR_FIELDS][RPWS_LABEL_MISC_BUFFER_SIZE] */
};

static struct RPWS_LABEL dust_label = {
  NULL,                                 /* struct RPWS_LABEL *link; */
  RPWS_LABEL_DUST,                      /* int instrument;          0=WBR 1=WFR */
  0,                                    /* int band;                 */
  0,                                    /* int mode_mask;            */

#ifdef HFR_XLATE
  HFR_XLATE * 0,                        /* short hfr_xlate;          */
#else
  0,                                    /* int hfr_xlate;            */
#endif

  0.0,                                  /* double span;              */
  0.0,                                  /* double duration;          */
  HSK_DUST_RECORD_SIZE,                 /* int dataset_size[0];      */
  HSK_DUST_RECORD_SIZE,                 /* int dataset_size[1];      */
  16,                                   /* int sample_count;         */
  RPWS_LABEL_PAD_HSK_DUST,              /* int pad_class;            */
  HSK_DUST_RECORD_SIZE,                 /* int record_size;          */
  0, 0,                                 /* int record_count[2];      */
  0,                                    /* int Label_Line_Pad;      Are labels padded to 80 colums ? */
  NULL,                                 /* FILE *filehandle;         */
  dust_label.text_area[0],              /* char *filename;          RPWS_LABEL_CHAR_FIELD   1 */
  dust_label.text_area[1],              /* char *filepath1;         RPWS_LABEL_CHAR_FIELD   2 */
  dust_label.text_area[2],              /* char *filepath2;         RPWS_LABEL_CHAR_FIELD   3 */
  dust_label.text_area[3],              /* char *filepath3;         RPWS_LABEL_CHAR_FIELD   4 */
  dust_label.text_area[4],              /* char *thumbname;         RPWS_LABEL_CHAR_FIELD   5 */
  label.text_area[5],                   /* char *utc_date;          RPWS_LABEL_CHAR_FIELD   6 */
  label.text_area[6],                   /* char *utc_time;          RPWS_LABEL_CHAR_FIELD   7 */
  label.text_area[7],                   /* char *sclk_start;        RPWS_LABEL_CHAR_FIELD   8 */
  label.text_area[8],                   /* char *sclk_stop;         RPWS_LABEL_CHAR_FIELD   9 */
  label.text_area[9],                   /* char *scet_start;        RPWS_LABEL_CHAR_FIELD  10 */
  label.text_area[10],                  /* char *scet_start_2;      RPWS_LABEL_CHAR_FIELD  11 */
  label.text_area[11],                  /* char *scet_stop;         RPWS_LABEL_CHAR_FIELD  12 */
  label.text_area[12],                  /* char *ephem_start;       RPWS_LABEL_CHAR_FIELD  13 */
  label.text_area[13],                  /* char *ephem_stop;        RPWS_LABEL_CHAR_FIELD  14 */
  label.text_area[14],                  /* char *plot_start;        RPWS_LABEL_CHAR_FIELD  15 */
  label.text_area[15],                  /* char *plot_stop;         RPWS_LABEL_CHAR_FIELD  16 */
  label.text_area[16],                  /* char *scet_start_3;      RPWS_LABEL_CHAR_FIELD  16 */
  Label_version,                        /* char *Label_Version;      */
  Mission_phase,                        /* char *Mission_Phase;      */
  Target,                               /* char *Target;             */
  Coordinate,                           /* char *Coordinate          */
  Orbit,                                /* char *Orbit               */
  "RPWS_DST_HOUSEKEPNG",                /* char *Product;            */
  1,                                    /* int ProdVerId             */
  RPWS_LABEL_CHAR_FIELDS * RPWS_LABEL_MISC_BUFFER_SIZE * 0      /* char  text_area[RPWS_LABEL_CHAR_FIELDS][RPWS_LABEL_MISC_BUFFER_SIZE] */
};
static struct RPWS_LABEL bfdl_label = {
  NULL,                                 /* struct RPWS_LABEL *link; */
  RPWS_LABEL_BFDL,                      /* int instrument;          0=WBR 1=WFR */
  0,                                    /* int band;                 */
  0,                                    /* int mode_mask;            */

#ifdef HFR_XLATE
  HFR_XLATE * 0,                        /* short hfr_xlate;          */
#else
  0,                                    /* int hfr_xlate;            */
#endif

  0.0,                                  /* double span;              */
  0.0,                                  /* double duration;          */
  HSK_BFDL_RECORD_SIZE,                 /* int dataset_size[0];      */
  HSK_BFDL_RECORD_SIZE,                 /* int dataset_size[1];      */
  16,                                   /* int sample_count;         */
  RPWS_LABEL_PAD_HSK_BFDL,              /* int pad_class;            */
  HSK_BFDL_RECORD_SIZE,                 /* int record_size;          */
  0, 0,                                 /* int record_count[2];      */
  0,                                    /* int Label_Line_Pad;      Are labels padded to 80 colums ? */
  NULL,                                 /* FILE *filehandle;         */
  bfdl_label.text_area[0],              /* char *filename;          RPWS_LABEL_CHAR_FIELD   1 */
  bfdl_label.text_area[1],              /* char *filepath1;         RPWS_LABEL_CHAR_FIELD   2 */
  bfdl_label.text_area[2],              /* char *filepath2;         RPWS_LABEL_CHAR_FIELD   3 */
  bfdl_label.text_area[3],              /* char *filepath3;         RPWS_LABEL_CHAR_FIELD   4 */
  bfdl_label.text_area[4],              /* char *thumbname;         RPWS_LABEL_CHAR_FIELD   5 */
  label.text_area[5],                   /* char *utc_date;          RPWS_LABEL_CHAR_FIELD   6 */
  label.text_area[6],                   /* char *utc_time;          RPWS_LABEL_CHAR_FIELD   7 */
  label.text_area[7],                   /* char *sclk_start;        RPWS_LABEL_CHAR_FIELD   8 */
  label.text_area[8],                   /* char *sclk_stop;         RPWS_LABEL_CHAR_FIELD   9 */
  label.text_area[9],                   /* char *scet_start;        RPWS_LABEL_CHAR_FIELD  10 */
  label.text_area[10],                  /* char *scet_start_2;      RPWS_LABEL_CHAR_FIELD  11 */
  label.text_area[11],                  /* char *scet_stop;         RPWS_LABEL_CHAR_FIELD  12 */
  label.text_area[12],                  /* char *ephem_start;       RPWS_LABEL_CHAR_FIELD  13 */
  label.text_area[13],                  /* char *ephem_stop;        RPWS_LABEL_CHAR_FIELD  14 */
  label.text_area[14],                  /* char *plot_start;        RPWS_LABEL_CHAR_FIELD  15 */
  label.text_area[15],                  /* char *plot_start;        RPWS_LABEL_CHAR_FIELD  16 */
  label.text_area[16],                  /* char *scet_start_3;      RPWS_LABEL_CHAR_FIELD  16 */
  Label_version,                        /* char *Label_Version;      */
  Mission_phase,                        /* char *Mission_Phase;      */
  Target,                               /* char *Target;             */
  Coordinate,                           /* char *Coordinate          */
  Orbit,                                /* char *Orbit               */
  "RPWS_BFDL_HOUSEKEPNG",               /* char *Product;            */
  1,                                    /* int ProdVerId             */
  RPWS_LABEL_CHAR_FIELDS * RPWS_LABEL_MISC_BUFFER_SIZE * 0      /* char  text_area[RPWS_LABEL_CHAR_FIELDS][RPWS_LABEL_MISC_BUFFER_SIZE] */
};

static struct RPWS_LABEL ipc_label = {
  NULL,                                 /* struct RPWS_LABEL *link; */
  RPWS_LABEL_IPC,                       /* int instrument;          0=WBR 1=WFR */
  0,                                    /* int band;                 */
  0,                                    /* int mode_mask;            */

#ifdef HFR_XLATE
  HFR_XLATE * 0,                        /* short hfr_xlate;          */
#else
  0,                                    /* int hfr_xlate;            */
#endif

  0.0,                                  /* double span;              */
  0.0,                                  /* double duration;          */
  HSK_IPC_RECORD_SIZE,                  /* int dataset_size[0];      */
  HSK_IPC_RECORD_SIZE,                  /* int dataset_size[1];      */
  18,                                   /* int sample_count;         */
  RPWS_LABEL_PAD_HSK_IPC,               /* int pad_class;            */
  HSK_IPC_RECORD_SIZE,                  /* int record_size;          */
  0, 0,                                 /* int record_count[2];      */
  0,                                    /* int Label_Line_Pad;      Are labels padded to 80 colums ? */
  NULL,                                 /* FILE *filehandle;         */
  ipc_label.text_area[0],               /* char *filename;          RPWS_LABEL_CHAR_FIELD   1 */
  ipc_label.text_area[1],               /* char *filepath1;         RPWS_LABEL_CHAR_FIELD   2 */
  ipc_label.text_area[2],               /* char *filepath2;         RPWS_LABEL_CHAR_FIELD   3 */
  ipc_label.text_area[3],               /* char *filepath3;         RPWS_LABEL_CHAR_FIELD   4 */
  ipc_label.text_area[4],               /* char *thumbname;         RPWS_LABEL_CHAR_FIELD   5 */
  label.text_area[5],                   /* char *utc_date;          RPWS_LABEL_CHAR_FIELD   6 */
  label.text_area[6],                   /* char *utc_time;          RPWS_LABEL_CHAR_FIELD   7 */
  label.text_area[7],                   /* char *sclk_start;        RPWS_LABEL_CHAR_FIELD   8 */
  label.text_area[8],                   /* char *sclk_stop;         RPWS_LABEL_CHAR_FIELD   9 */
  label.text_area[9],                   /* char *scet_start;        RPWS_LABEL_CHAR_FIELD  10 */
  label.text_area[10],                  /* char *scet_start_2;      RPWS_LABEL_CHAR_FIELD  11 */
  label.text_area[11],                  /* char *scet_stop;         RPWS_LABEL_CHAR_FIELD  12 */
  label.text_area[12],                  /* char *ephem_start;       RPWS_LABEL_CHAR_FIELD  13 */
  label.text_area[13],                  /* char *ephem_stop;        RPWS_LABEL_CHAR_FIELD  14 */
  label.text_area[14],                  /* char *plot_start;        RPWS_LABEL_CHAR_FIELD  15 */
  label.text_area[15],                  /* char *plot_start;        RPWS_LABEL_CHAR_FIELD  16 */
  label.text_area[16],                  /* char *scet_start_3;      RPWS_LABEL_CHAR_FIELD  16 */
  Label_version,                        /* char *Label_Version;      */
  Mission_phase,                        /* char *Mission_Phase;      */
  Target,                               /* char *Target;             */
  Coordinate,                           /* char *Coordinate          */
  Orbit,                                /* char *Orbit               */
  "RPWS_IPC_HOUSEKEPNG",                /* char *Product;            */
  1,                                    /* int ProdVerId             */
  RPWS_LABEL_CHAR_FIELDS * RPWS_LABEL_MISC_BUFFER_SIZE * 0      /* char  text_area[RPWS_LABEL_CHAR_FIELDS][RPWS_LABEL_MISC_BUFFER_SIZE] */
};

static struct RPWS_LABEL eng_label = {
  NULL,                                 /* struct RPWS_LABEL *link; */
  RPWS_LABEL_ENG,                       /* int instrument;          0=WBR 1=WFR */
  0,                                    /* int band;                 */
  0,                                    /* int mode_mask;            */

#ifdef HFR_XLATE
  HFR_XLATE * 0,                        /* short hfr_xlate;          */
#else
  0,                                    /* int hfr_xlate;            */
#endif

  0.0,                                  /* double span;              */
  0.0,                                  /* double duration;          */
  -1,                                   /* int dataset_size[0];      */
  -1,                                   /* int dataset_size[1];      */
  -1,                                   /* int sample_count;         */
  RPWS_LABEL_PAD_HSK_ENG,               /* int pad_class;            */
  0,                                    /* int record_size;          */
  0, 0,                                 /* int record_count[2];      */
  0,                                    /* int Label_Line_Pad;      Are labels padded to 80 colums ? */
  NULL,                                 /* FILE *filehandle;         */
  eng_label.text_area[0],               /* char *filename;          RPWS_LABEL_CHAR_FIELD   1 */
  eng_label.text_area[1],               /* char *filepath1;         RPWS_LABEL_CHAR_FIELD   2 */
  eng_label.text_area[2],               /* char *filepath2;         RPWS_LABEL_CHAR_FIELD   3 */
  eng_label.text_area[3],               /* char *filepath3;         RPWS_LABEL_CHAR_FIELD   4 */
  eng_label.text_area[4],               /* char *thumbname;         RPWS_LABEL_CHAR_FIELD   5 */
  label.text_area[5],                   /* char *utc_date;          RPWS_LABEL_CHAR_FIELD   6 */
  label.text_area[6],                   /* char *utc_time;          RPWS_LABEL_CHAR_FIELD   7 */
  label.text_area[7],                   /* char *sclk_start;        RPWS_LABEL_CHAR_FIELD   8 */
  label.text_area[8],                   /* char *sclk_stop;         RPWS_LABEL_CHAR_FIELD   9 */
  label.text_area[9],                   /* char *scet_start;        RPWS_LABEL_CHAR_FIELD  10 */
  label.text_area[10],                  /* char *scet_start_2;      RPWS_LABEL_CHAR_FIELD  11 */
  label.text_area[11],                  /* char *scet_stop;         RPWS_LABEL_CHAR_FIELD  12 */
  label.text_area[12],                  /* char *ephem_start;       RPWS_LABEL_CHAR_FIELD  13 */
  label.text_area[13],                  /* char *ephem_stop;        RPWS_LABEL_CHAR_FIELD  14 */
  label.text_area[14],                  /* char *plot_start;        RPWS_LABEL_CHAR_FIELD  15 */
  label.text_area[15],                  /* char *plot_start;        RPWS_LABEL_CHAR_FIELD  16 */
  label.text_area[16],                  /* char *scet_start_3;      RPWS_LABEL_CHAR_FIELD  16 */
  Label_version,                        /* char *Label_Version;      */
  Mission_phase,                        /* char *Mission_Phase;      */
  Target,                               /* char *Target;             */
  Coordinate,                           /* char *Coordinate          */
  Orbit,                                /* char *Orbit               */
  "RPWS_ENGINEERING",                   /* char *Product;            */
  1,                                    /* int ProdVerId             */
  RPWS_LABEL_CHAR_FIELDS * RPWS_LABEL_MISC_BUFFER_SIZE * 0      /* char  text_area[RPWS_LABEL_CHAR_FIELDS][RPWS_LABEL_MISC_BUFFER_SIZE] */
};

static struct RPWS_LABEL alarm_label = {
  NULL,                                 /* struct RPWS_LABEL *link; */
  RPWS_LABEL_ALARM,                     /* int instrument;          0=WBR 1=WFR */
  0,                                    /* int band;                 */
  0,                                    /* int mode_mask;            */

#ifdef HFR_XLATE
  HFR_XLATE * 0,                        /* short hfr_xlate;          */
#else
  0,                                    /* int hfr_xlate;            */
#endif

  0.0,                                  /* double span;              */
  0.0,                                  /* double duration;          */
  -1,                                   /* int dataset_size[0];      */
  -1,                                   /* int dataset_size[1];      */
  -1,                                   /* int sample_count;         */
  RPWS_LABEL_PAD_HSK_ENG,               /* int pad_class;            */
  0,                                    /* int record_size;          */
  0, 0,                                 /* int record_count[2];      */
  0,                                    /* int Label_Line_Pad;      Are labels padded to 80 colums ? */
  NULL,                                 /* FILE *filehandle;         */
  alarm_label.text_area[0],             /* char *filename;          RPWS_LABEL_CHAR_FIELD   1 */
  alarm_label.text_area[1],             /* char *filepath1;         RPWS_LABEL_CHAR_FIELD   2 */
  alarm_label.text_area[2],             /* char *filepath2;         RPWS_LABEL_CHAR_FIELD   3 */
  alarm_label.text_area[3],             /* char *filepath3;         RPWS_LABEL_CHAR_FIELD   4 */
  alarm_label.text_area[4],             /* char *thumbname;         RPWS_LABEL_CHAR_FIELD   5 */
  label.text_area[5],                   /* char *utc_date;          RPWS_LABEL_CHAR_FIELD   6 */
  label.text_area[6],                   /* char *utc_time;          RPWS_LABEL_CHAR_FIELD   7 */
  label.text_area[7],                   /* char *sclk_start;        RPWS_LABEL_CHAR_FIELD   8 */
  label.text_area[8],                   /* char *sclk_stop;         RPWS_LABEL_CHAR_FIELD   9 */
  label.text_area[9],                   /* char *scet_start;        RPWS_LABEL_CHAR_FIELD  10 */
  label.text_area[10],                  /* char *scet_start_2;      RPWS_LABEL_CHAR_FIELD  11 */
  label.text_area[11],                  /* char *scet_stop;         RPWS_LABEL_CHAR_FIELD  12 */
  label.text_area[12],                  /* char *ephem_start;       RPWS_LABEL_CHAR_FIELD  13 */
  label.text_area[13],                  /* char *ephem_stop;        RPWS_LABEL_CHAR_FIELD  14 */
  label.text_area[14],                  /* char *plot_start;        RPWS_LABEL_CHAR_FIELD  15 */
  label.text_area[15],                  /* char *plot_start;        RPWS_LABEL_CHAR_FIELD  16 */
  label.text_area[16],                  /* char *scet_start_3;      RPWS_LABEL_CHAR_FIELD  16 */
  Label_version,                        /* char *Label_Version;      */
  Mission_phase,                        /* char *Mission_Phase;      */
  Target,                               /* char *Target;             */
  Coordinate,                           /* char *Coordinate          */
  Orbit,                                /* char *Orbit               */
  "RPWS_ENGINEERING",                   /* char *Product;            */
  1,                                    /* int ProdVerId             */
  RPWS_LABEL_CHAR_FIELDS * RPWS_LABEL_MISC_BUFFER_SIZE * 0      /* char  text_area[RPWS_LABEL_CHAR_FIELDS][RPWS_LABEL_MISC_BUFFER_SIZE] */
};



struct ARCHIVE_TIME_SERIES archive_record_hsk;
struct ARCHIVE_TIME_SERIES archive_record_dust;
struct ARCHIVE_TIME_SERIES archive_record_bfdl;
struct ARCHIVE_TIME_SERIES archive_record_ipc;

#define _RPWS_W_DIRECTORY_
#include "rpws_w_directory.h"


int gr_main (int func, struct GRAPHICS_TEXT *text, char directory[])
{
  return 0;
}



  /*********************************************************
   **** If you're familiar with SPICE S- and P-kernels,	**** 
   **** you know that NAIF codes for spacecraft are 	****
   **** negative integers: -31 for Voyager 1, -32 for	****
   **** Voyager 2, -94 for Mars Observer, and so on. We	****
   **** borrow from this convention in defining		****
   **** instrument codes.				****
   ****							****
   **** Well, who'd-a thunk-it... negative numbers...	****
   **** sheesh, I guess that means Cassini is -82, then	****
   **** isn't it ?!?   Doooh				****
   *********************************************************/
static double spice_time_ext (int seconds, int fine, int partition)
{
  double et;
  char sclk_temp[256];

                /************************************************
		 *	Convert SCLK to string (spice format)	*
		 ************************************************/
  sprintf (sclk_temp, "%d/%d:%03d", partition, seconds, fine & RTI_MASK);       /* only valid to RTI */

                /************************************************
		 *	Conversion to SPICE internal	 	*
		 ************************************************/
  scs2e_c(SpaceCraft_ID, sclk_temp, &et);
  return et;
}

  /*
   * 
   */

  /**********************************************************************
   *	
   *	
   *	
   **********************************************************************/
static double spice_time_x (struct archive_event_clock *sclk,
                            struct SPICE_TIME *spice_time_array,
                            char *new_utcout)
{
  double et;
  int prec = 3;
  char *format[2] = { "D", "J" };
  static char utcout[64];
  static char jdout[64];

  memset (utcout, 0, 33);
  memset (jdout, 0, 33);

  /************************************************
   *	Convert SCLK to spice format		*
   ************************************************/
  et = spice_time_ext (sclk->seconds, sclk->fine, Partition);

  /************************************************
   *	Conversion to data string	 	*
   *	Both julian ddd.fff to get days		*
   *	and yyy-ddd // hh:mm:ss.mmm to get	*
   *	  milliseconds of day			*
   ************************************************/
  et2utc_c(et, format[0], prec, 32, utcout);
  et2utc_c(et, format[1], prec, 32, jdout);

      /************************************************
		 *	Goofey-ass, isn't it, BUT...	 	*
		 *	  now we have spice's opinion so	*
		 *	  convert back yo internal		*
		 *	SPICE JULIAN day is epoch 1/1/1 AD	*
		 *	  subtract number of days to 		*
		 *	  CASSINI epoch				*
		 ************************************************/
  spice_time_array->year = strtol (&utcout[0], NULL, 10);
  spice_time_array->doy = strtol (&utcout[5], NULL, 10);
  spice_time_array->d100 = spice_time_array->doy / 100;

  spice_time_array->days = strtol (&jdout[3], NULL, 10) - A_EPOCH;

                /************************************************
		 *	Now use HH:MM:SS.mmm to get 		*
		 *	seconds of day.				*
		 ************************************************/
  spice_time_array->hours = strtol (&utcout[12], NULL, 10);
  spice_time_array->minutes = strtol (&utcout[15], NULL, 10);
  spice_time_array->seconds = strtol (&utcout[18], NULL, 10);
  spice_time_array->milliseconds = strtol (&utcout[21], NULL, 10);

  spice_time_array->msec_of_day = spice_time_array->milliseconds;
  spice_time_array->msec_of_day += spice_time_array->seconds * 1000;
  spice_time_array->msec_of_day += spice_time_array->minutes * 1000 * 60;
  spice_time_array->msec_of_day += spice_time_array->hours * 1000 * 60 * 60;
  if (new_utcout) {
    memset (new_utcout, 0, 32);
    strncat (new_utcout, &utcout[0], 8);
    strncat (new_utcout, "T", 1);
    strncat (new_utcout, &utcout[12], 12);
    if (0) {
      fprintf (stdout, "%5d                1         2         3\n",
               __LINE__);
      fprintf (stdout, "%5d      012345678901234567890123456789012\n",
               __LINE__);
      fprintf (stdout, "%5d UTC >%s<\n", __LINE__, new_utcout);
    }
  }
  return et;
}

 /*
  * 
  */

  /**********************************************************************
   *	
   **********************************************************************/
static int initialize_label (struct RPWS_LABEL *lbl, int flag, int nVerId)
{
  if (flag) {
    strcpy(lbl->sclk_start, "2038-365T23:59:59.999");
    strcpy(lbl->sclk_stop, "1997-200T00:00:00.000");
    strcpy(lbl->scet_start, "2147483647");
    strcpy(lbl->scet_stop, "0000000000");
    strcpy(lbl->plot_start, "2147483647");
    strcpy(lbl->plot_stop, "0000000000");

    Mission_phase[0] = 0;
    Target[0] = 0;
    Coordinate[0] = 0;
    Orbit[0] = 0;
  }
  lbl->ProdVerId = nVerId;
  lbl->record_count[0] = 0;
  lbl->record_count[1] = 0;
  return 1;
}

 /*
  * 
  */

  /**********************************************************************
   *	
   **********************************************************************/
#define LEN 31
static int p_write (char *buffer, int count, int xx, FILE * dst)
{
  int i;

  for (i = 0; i < count; i++) {
    if ((i & LEN) == 0)
      fprintf (stdout, "%04X: ", i);
    fprintf (stdout, " %02X", buffer[i] & 0xFF);
    if ((i & LEN) == (LEN))
      fprintf (stdout, "\n");
  }
  fprintf (stdout, "\n");
}

 /*
  * 
  */

  /**********************************************************************
   *	
   **********************************************************************/
static int new_dir_mkdir (const char *path, mode_t mode)
{
  if (strstr (path, "//"))
    return 0;
  return mkdir (path, mode);
/**/}


/**********************************************************************
 *	08 APR 2005
 *
 *	Look at non-zero fill length and set the ARCH_STATUS_FLAG_SUSPECT bit if
 * the record has zero-fill.
 *
 *	Return a 1 if data is "FAWLTY" (1 means FILL)
 *
 **********************************************************************/
static int misc_status (struct CDS_buffer *buf,
                        struct ARCHIVE_TIME_SERIES *archive_)
{
  int fawlty = 0;

  if (buf->packet.chdo_ancillary.type_94.non_fill_length < 192)
    fawlty = 1;
  if (fawlty)
    set_status (&archive_->status_flag, ARCH_STATUS_FLAG_SUSPECT, 1);
  return fawlty;
}

  /**********************************************************************
   *	
   **********************************************************************/
void dump (char *buf)
{
  int i;

  for (i = 0; i < 16; i++) {
    fprintf (stdout, " %02X", buf[i] & 0xFF);
    if (i % 16 == 15)
      fprintf (stdout, "\n");
  }
  fprintf (stdout, "\n");
}

int format_alarm (struct CDS_buffer *buf, char *txt_buf, char *text)
{
  char *temp;
  int seconds, fine;
  double et;
  int prec = 3;
  char *format[2] = { "D", "J" };
  static char utcout[64];
  static char new_utcout[64];


  temp = rpws_alarm (txt_buf);
  if (temp) {

    seconds = UTIL_extract_TIME (buf);
    fine = UTIL_extract_CDS_RTI (buf);

    et = spice_time_ext (seconds, fine, Partition);
    et2utc_c(et, format[0], prec, 32, utcout);
    memset (new_utcout, 0, 32);
    strncat (new_utcout, &utcout[0], 8);
    strncat (new_utcout, "T", 1);
    strncat (new_utcout, &utcout[12], 12);

    sprintf (&text[0], "\"%1d/%10.10d:%3.3d\", ", Partition, seconds, fine);
    sprintf (&text[19], "\"0x%08X.%1X\", ", seconds, fine >> 5);
    sprintf (&text[34], "\"%s\", ", new_utcout);

    sprintf (&text[58], "%s", temp);

    strcat (text, "\r\n");;
    alarm_label.sample_count = strlen (text);
    alarm_label.dataset_size[1] = alarm_label.sample_count;
    alarm_label.dataset_size[1] = alarm_label.sample_count;
  }
  if (temp)
    return 1;
  else
    return 0;
}

char *format_eng (struct CDS_buffer *buf, char *text)
{
  int seconds, fine;
  double et;
  int prec = 3;
  char *format[2] = { "D", "J" };
  static char utcout[64];
  static char new_utcout[64];


  seconds = UTIL_extract_TIME (buf);
  fine = UTIL_extract_CDS_RTI (buf);

  et = spice_time_ext (seconds, fine, Partition);
  et2utc_c(et, format[0], prec, 32, utcout);
  memset (new_utcout, 0, 32);
  strncat (new_utcout, &utcout[0], 8);
  strncat (new_utcout, "T", 1);
  strncat (new_utcout, &utcout[12], 12);

  sprintf (&text[0], "\"%1d/%10.10d:%3.3d\", ", Partition, seconds, fine);
  sprintf (&text[19], "\"0x%08X.%1X\", ", seconds, fine >> 5);
  sprintf (&text[34], "\"%s\", ", new_utcout);

  sprintf (&text[58], "%s", rpws_engineering (buf));

  strcat (text, "\r\n");;
  eng_label.sample_count = strlen (text);
  eng_label.dataset_size[1] = eng_label.sample_count;
  eng_label.dataset_size[1] = eng_label.sample_count;
  return text;
}

int reformat_dust (struct CDS_buffer *buf,
                   int new, struct DUST_RECORD *dust_new)
{
  int status = 1;
  int i;
  double event_et;
  char new_utc[64];
  struct SPICE_TIME spice_time_array;


  /*
   * move data 
   */
  archive_record_dust.time_series.word_sample[0] =
    get_status ((unsigned char *) &dust_new[new], MHSK_LEN, MHSK_ID);
  if (archive_record_dust.time_series.word_sample[0] != 0xD00D) {
    status = 0;
    return status;
  }

  archive_record_dust.time_series.word_sample[1] =
    get_status ((unsigned char *) &dust_new[new], MHSK_RTI, MHSK_RTI_MSB);

  archive_record_dust.time_series.word_sample[2] =
    get_status ((unsigned char *) &dust_new[new],
                MHSK_DUST_ADDR, MHSK_DUST_ADDR_MSB);
  if (archive_record_dust.time_series.word_sample[2] != 0x27D0) {
    status = 0;
    return status;
  }

  archive_record_dust.time_series.word_sample[3] =
    get_status ((unsigned char *) &dust_new[new],
                MHSK_DUST_COUNT, MHSK_DUST_COUNT_MSB);

  for (i = 0; i < 8; i++)
    archive_record_dust.time_series.byte_sample[i + 8] =
      dust_new[new].count[i];
  /*
   * data moved 
   */

  archive_record_dust.samples = sizeof (struct DUST_RECORD);
  archive_record_dust.data_rti = get_status ((unsigned char *) &dust_new[new],
                                             DUST_minipacket_RTI,
                                             DUST_minipacket_RTI_MSB);
  archive_record_dust.sclk.seconds = UTIL_micro_time (buf, new, 0);
  archive_record_dust.sclk.partition = Partition;
  archive_record_dust.sclk.fine =
    get_status ((unsigned char *) &dust_new[new], MHSK_RTI_ONLY, 0) << 5;
  event_et =
    spice_time_x (&archive_record_dust.sclk, &spice_time_array, new_utc);

  archive_record_dust.scet.days = spice_time_array.days;
  archive_record_dust.scet.msec_of_day = spice_time_array.msec_of_day;

  archive_record_dust.fsw_ver =
    rpws_fsw_ver (&archive_record_dust, NULL, NULL, NULL);

  return status;
}
int reformat_ipc (struct CDS_buffer *buf,
                  int new, struct DUST_RECORD *ipc_new)
{
  int status = 1;
  int i;
  double event_et;
  char new_utc[64];
  struct SPICE_TIME spice_time_array;


  /*
   * move data 
   */
  archive_record_ipc.time_series.word_sample[0] =
    get_status ((unsigned char *) &ipc_new[new], MHSK_LEN, MHSK_ID);
  if (archive_record_ipc.time_series.word_sample[0] != 0xD00D) {
    status = 0;
    return status;
  }

  archive_record_ipc.time_series.word_sample[1] =
    get_status ((unsigned char *) &ipc_new[new], MHSK_RTI, MHSK_RTI_MSB);
  archive_record_ipc.time_series.word_sample[2] =
    get_status ((unsigned char *) &ipc_new[new],
                MHSK_MRO_ADDR, MHSK_MRO_ADDR_MSB);
  status = 0;
  if (archive_record_ipc.time_series.word_sample[2] == 0x1170)
    status = 1;
  if (archive_record_ipc.time_series.word_sample[2] == 0x117A)
    status = 2;
  if (!status) {
    return status;
  }

  switch (status) {
   case 1:
     archive_record_ipc.time_series.word_sample[3] =
       get_status ((unsigned char *) &ipc_new[new],
                   MHSK_WORD_3, MHSK_WORD_3_MSB);
     archive_record_ipc.time_series.word_sample[4] =
       get_status ((unsigned char *) &ipc_new[new],
                   MHSK_WORD_4, MHSK_WORD_4_MSB);
     archive_record_ipc.time_series.word_sample[5] =
       get_status ((unsigned char *) &ipc_new[new],
                   MHSK_WORD_5, MHSK_WORD_5_MSB);
     archive_record_ipc.time_series.word_sample[6] =
       get_status ((unsigned char *) &ipc_new[new],
                   MHSK_WORD_6, MHSK_WORD_6_MSB);
     archive_record_ipc.time_series.word_sample[7] =
       get_status ((unsigned char *) &ipc_new[new],
                   MHSK_WORD_7, MHSK_WORD_7_MSB);
     archive_record_ipc.time_series.word_sample[8] = 0;
     archive_record_ipc.samples = sizeof (struct DUST_RECORD);
     break;
   case 2:
     archive_record_ipc.time_series.word_sample[8] =
       get_status ((unsigned char *) &ipc_new[new],
                   MHSK_WORD_3, MHSK_WORD_3_MSB);
     archive_record_ipc.samples = sizeof (struct DUST_RECORD) + 2;
     break;

  }
  /*
   * data moved 
   */

  archive_record_ipc.samples = sizeof (struct DUST_RECORD);
  archive_record_ipc.data_rti = get_status ((unsigned char *) &ipc_new[new],
                                            DUST_minipacket_RTI,
                                            DUST_minipacket_RTI_MSB);
  archive_record_ipc.sclk.seconds = UTIL_micro_time (buf, new, 0);
  archive_record_ipc.sclk.partition = Partition;
  archive_record_ipc.sclk.fine = get_status ((unsigned char *) &ipc_new[new],
                                             MHSK_RTI_ONLY, 0) << 5;
  event_et = spice_time_x (&archive_record_ipc.sclk,
                           &spice_time_array, new_utc);

  archive_record_ipc.scet.days = spice_time_array.days;
  archive_record_ipc.scet.msec_of_day = spice_time_array.msec_of_day;

  archive_record_ipc.fsw_ver =
    rpws_fsw_ver (&archive_record_ipc, NULL, NULL, NULL);

  return status;
}
int reformat_bfdl (struct CDS_buffer *buf,
                   int new, struct DUST_RECORD *bfdl_new)
{
  int status = 1;
  int i;
  double event_et;
  char new_utc[64];
  struct SPICE_TIME spice_time_array;


  /*
   * move data 
   */
  archive_record_bfdl.time_series.word_sample[0] =
    get_status ((unsigned char *) &bfdl_new[new], MHSK_LEN, MHSK_ID);

  if (archive_record_bfdl.time_series.word_sample[0] != 0xC10D) {
    status = 0;
    return status;
  }

  archive_record_bfdl.time_series.word_sample[1] =
    get_status ((unsigned char *) &bfdl_new[new], MHSK_RTI, MHSK_RTI_MSB);
  archive_record_bfdl.time_series.word_sample[2] =
    get_status ((unsigned char *) &bfdl_new[new],
                MHSK_WORD_2, MHSK_WORD_2_MSB);
  archive_record_bfdl.time_series.word_sample[3] =
    get_status ((unsigned char *) &bfdl_new[new],
                MHSK_WORD_3, MHSK_WORD_3_MSB);
  archive_record_bfdl.time_series.word_sample[4] =
    get_status ((unsigned char *) &bfdl_new[new],
                MHSK_WORD_4, MHSK_WORD_4_MSB);
  archive_record_bfdl.time_series.word_sample[5] =
    get_status ((unsigned char *) &bfdl_new[new],
                MHSK_WORD_5, MHSK_WORD_5_MSB);
  archive_record_bfdl.time_series.word_sample[6] =
    get_status ((unsigned char *) &bfdl_new[new],
                MHSK_WORD_6, MHSK_WORD_6_MSB);
  archive_record_bfdl.time_series.word_sample[7] =
    get_status ((unsigned char *) &bfdl_new[new],
                MHSK_WORD_7, MHSK_WORD_7_MSB);
  /*
   * data moved 
   */

  archive_record_bfdl.samples = sizeof (struct DUST_RECORD);
  archive_record_bfdl.data_rti = get_status ((unsigned char *) &bfdl_new[new],
                                             DUST_minipacket_RTI,
                                             DUST_minipacket_RTI_MSB);
  archive_record_bfdl.sclk.seconds = UTIL_micro_time (buf, new, 0);
  archive_record_bfdl.sclk.partition = Partition;
  archive_record_bfdl.sclk.fine =
    get_status ((unsigned char *) &bfdl_new[new], MHSK_RTI_ONLY, 0) << 5;
  event_et =
    spice_time_x (&archive_record_bfdl.sclk, &spice_time_array, new_utc);

  archive_record_bfdl.scet.days = spice_time_array.days;
  archive_record_bfdl.scet.msec_of_day = spice_time_array.msec_of_day;

  archive_record_bfdl.fsw_ver =
    rpws_fsw_ver (&archive_record_bfdl, NULL, NULL, NULL);

  return status;
}

/*
 *
 */
void dust_extract_write (struct RPWS_LABEL *_label, struct CDS_buffer *buf)
{
  int new, old;
  int result;
  int count;
  int isdust;
  static struct DUST_RECORD dust_old[8];
  struct DUST_RECORD dust_new[8];

  memcpy (dust_new, &buf->packet.mpp.mini_packet[64], 128);
  for (new = 7; new >= 0; new--) {
    count = 0;
    for (old = 7; old >= 0; old--) {
      result =
        memcmp (&dust_new[new], &dust_old[old], sizeof (struct DUST_RECORD));
      if (result)
        count++;
    }
    if (count == 8) {
      isdust = reformat_dust (buf, new, dust_new);
      if (isdust) {
        dust_label.record_count[0]++;
        dust_label.record_count[1]++;
        misc_status (buf, &archive_record_dust);
        fwrite ((char *) &archive_record_dust,
                archive_record_dust.record_bytes, 1, _label->filehandle);
      }
    }
  }

  memcpy (dust_old, dust_new, 128);
}
void bfdl_extract_write (struct RPWS_LABEL *_label, struct CDS_buffer *buf)
{
  int new, old;
  int result;
  int count;
  int isbfdl;
  static struct DUST_RECORD bfdl_old[8];
  struct DUST_RECORD bfdl_new[8];

  memcpy (bfdl_new, &buf->packet.mpp.mini_packet[64], 128);
  for (new = 7; new >= 0; new--) {
    count = 0;
    for (old = 7; old >= 0; old--) {
      result =
        memcmp (&bfdl_new[new], &bfdl_old[old], sizeof (struct DUST_RECORD));
      if (result)
        count++;
    }
    if (count == 8) {
      isbfdl = reformat_bfdl (buf, new, bfdl_new);
      if (isbfdl) {
        bfdl_label.record_count[0]++;
        bfdl_label.record_count[1]++;
        misc_status (buf, &archive_record_bfdl);
        fwrite ((char *) &archive_record_bfdl,
                archive_record_bfdl.record_bytes, 1, _label->filehandle);
        fflush (_label->filehandle);
      }
    }
  }
  memcpy (bfdl_old, bfdl_new, 128);
}
void ipc_extract_write (struct RPWS_LABEL *_label, struct CDS_buffer *buf)
{
  int new, old;
  int result;
  int count;
  int isipc;
  static struct DUST_RECORD ipc_old[8];
  struct DUST_RECORD ipc_new[8];

  memcpy (ipc_new, &buf->packet.mpp.mini_packet[64], 128);
  for (new = 7; new >= 0; new--) {
    count = 0;
    for (old = 7; old >= 0; old--) {
      result =
        memcmp (&ipc_new[new], &ipc_old[old], sizeof (struct DUST_RECORD));
      if (result)
        count++;
    }
    if (count == 8) {
      isipc = reformat_ipc (buf, new, ipc_new);
      if (isipc) {
        ipc_label.record_count[0]++;
        ipc_label.record_count[1]++;
        misc_status (buf, &archive_record_ipc);
        fwrite ((char *) &archive_record_ipc,
                archive_record_ipc.record_bytes, 1, _label->filehandle);
        fflush (_label->filehandle);
      }
    }
  }
  memcpy (ipc_old, ipc_new, 128);
}

/*
 *	Engineering
 */
char *eng_extract_write (struct RPWS_LABEL *_label, struct CDS_buffer *buf)
{
  static char text[1024];

  format_eng (buf, text);
  eng_label.record_count[0]++;
  eng_label.record_count[1]++;
  fwrite (text, _label->sample_count, 1, _label->filehandle);
  return text;
}
void alarm_extract_write (struct RPWS_LABEL *_label, struct CDS_buffer *buf,
                          char *txt_buf)
{
  char text[1024];
  int status;

  status = format_alarm (buf, txt_buf, text);
  if (status) {
    alarm_label.record_count[0]++;
    alarm_label.record_count[1]++;
    fwrite (text, _label->sample_count, 1, _label->filehandle);
  }
}

/**********************************************************************
 *	
 **********************************************************************/
static int main_file (struct CDS_buffer *buf, int nProdVerId)
{
  static char old_filename[256] = { "nil" };
  char new_filename[256];

  char filepath[256];
  char filename[256];
  char dust_filename[256];
  char bfdl_filename[256];
  char ipc_filename[256];
  char eng_filename[256];
  char alarm_filename[256];

  char *temp;
  char new_utc[64];
  double event_et;
  struct SPICE_TIME spice_time_array;

  event_et = spice_time_x ((struct archive_event_clock *)
                           &buf->packet.chdo_tag.sclk.seconds,
                           &spice_time_array, new_utc);
  buf->packet.chdo_tag.scet.days = spice_time_array.days;
  buf->packet.chdo_tag.scet.milliseconds = spice_time_array.msec_of_day;

	/***************************************************
	 * File naming
	 * Keep in mind that this is reading "R" files, so	*
	 *	the records are in time-order, we don't have	*
	 *	to worry about time regressions			*
	 ***************************************************/
  
  sprintf (filepath, "T%04d%01dXX", spice_time_array.year, spice_time_array.d100);
  
  sprintf (filename, "T%04d%03d_HOUSEKEEPING", spice_time_array.year,
           spice_time_array.doy);
  
  sprintf (alarm_filename, "T%04d%03d_ALARM", spice_time_array.year,
           spice_time_array.doy);
  
  sprintf (eng_filename, "T%04d%03d_ENG", spice_time_array.year,
           spice_time_array.doy);
  sprintf (ipc_filename, "T%04d%03d_IPC_HSK", spice_time_array.year,
           spice_time_array.doy);
  sprintf (bfdl_filename, "T%04d%03d_BFDL_HSK", spice_time_array.year,
           spice_time_array.doy);
  sprintf (dust_filename, "T%04d%03d_DUST_HSK", spice_time_array.year,
           spice_time_array.doy);

  sprintf (new_filename, "%s/%s/%s.%s",
           w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
           filepath, filename, Filetype[0]);
  if (strcmp (old_filename, new_filename)) {    /* same file ??? *//* NO, so setup new file... */
    if (label.filehandle) {             /* 1st. time ??? */
      fclose (label.filehandle);        /* NO, close old file */
      /*
       * update label and write it 
       */
      strcpy (Mission_phase,
              rpws_mission_phase_name (label.scet_start, label.scet_stop, 0,
                                       0));
      strcpy (Target,
              rpws_target_name (label.scet_start, label.scet_stop, 0, 0));
      strcpy (Coordinate,
              rpws_coordinate_name (label.scet_start, label.scet_stop, 0, 0));
      strcpy (Orbit,
              rpws_orbit_number (label.scet_start, label.scet_stop, 0));
      if (Zero_Flag)
        rpws_time_patch (&label);
      rpws_label_write (&label, w_directory);
      label.filehandle = NULL;
    }
    if (dust_label.filehandle) {        /* 1st. time ??? */
      fclose (dust_label.filehandle);   /* NO, close old file */
      if (dust_label.record_count[0]) {
        if (Zero_Flag)
          rpws_time_patch (&dust_label);
        rpws_label_write (&dust_label, w_directory);
      } else {
        char temp[256];

        sprintf (temp, "%s/%s/%s.%s",
                 w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
                 dust_label.filepath1, dust_label.filename, Filetype[0]);
        unlink (temp);
      }
      dust_label.filehandle = NULL;
    }
    if (bfdl_label.filehandle) {        /* 1st. time ??? */
      fclose (bfdl_label.filehandle);   /* NO, close old file */
      if (bfdl_label.record_count[0]) {
        if (Zero_Flag)
          rpws_time_patch (&bfdl_label);
        rpws_label_write (&bfdl_label, w_directory);
      } else {
        char temp[256];

        sprintf (temp, "%s/%s/%s.%s",
                 w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
                 bfdl_label.filepath1, bfdl_label.filename, Filetype[0]);
        unlink (temp);
      }
      bfdl_label.filehandle = NULL;
    }
    if (ipc_label.filehandle) {         /* 1st. time ??? */
      fclose (ipc_label.filehandle);    /* NO, close old file */
      if (ipc_label.record_count[0]) {
        if (Zero_Flag)
          rpws_time_patch (&ipc_label);
        rpws_label_write (&ipc_label, w_directory);
      } else {
        char temp[256];

        sprintf (temp, "%s/%s/%s.%s",
                 w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
                 ipc_label.filepath1, ipc_label.filename, Filetype[0]);
        unlink (temp);
      }
      ipc_label.filehandle = NULL;
    }


    if (alarm_label.filehandle) {       /* 1st. time ??? */
      fclose (alarm_label.filehandle);  /* NO, close old file */
      if (alarm_label.record_count[0]) {
        if (Zero_Flag)
          rpws_time_patch (&alarm_label);
        rpws_label_write (&alarm_label, w_directory);
      } else {
        char temp[256];

        sprintf (temp, "%s/%s/%s.%s",
                 w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
                 alarm_label.filepath1, alarm_label.filename, Filetype[2]);
        unlink (temp);
      }
      alarm_label.filehandle = NULL;
    }

    if (eng_label.filehandle) {         /* 1st. time ??? */
      fclose (eng_label.filehandle);    /* NO, close old file */
      if (eng_label.record_count[0]) {
        if (Zero_Flag)
          rpws_time_patch (&eng_label);
        rpws_label_write (&eng_label, w_directory);
      } else {
        char temp[256];

        sprintf (temp, "%s/%s/%s.%s",
                 w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
                 eng_label.filepath1, eng_label.filename, Filetype[1]);
        unlink (temp);
      }
      eng_label.filehandle = NULL;
    }

    if (!label.filehandle)
      initialize_label (&label, 1, nProdVerId);
    if (!dust_label.filehandle)
      initialize_label (&dust_label, 0, nProdVerId);
    if (!bfdl_label.filehandle)
      initialize_label (&bfdl_label, 0, nProdVerId);
    if (!ipc_label.filehandle)
      initialize_label (&ipc_label, 0, nProdVerId);
    if (!eng_label.filehandle)
      initialize_label (&eng_label, 0, nProdVerId);
    if (!alarm_label.filehandle)
      initialize_label (&alarm_label, 0, nProdVerId);
    /*
     *      Build up new filenames, start with
     *      the directory path
     */
    strcpy (label.filename, filename);
    strcpy (label.filepath1, filepath);
    strcpy (dust_label.filename, dust_filename);
    strcpy (dust_label.filepath1, filepath);
    strcpy (bfdl_label.filename, bfdl_filename);
    strcpy (bfdl_label.filepath1, filepath);
    strcpy (ipc_label.filename, ipc_filename);
    strcpy (ipc_label.filepath1, filepath);
    strcpy (eng_label.filename, eng_filename);
    strcpy (eng_label.filepath1, filepath);
    strcpy (alarm_label.filename, alarm_filename);
    strcpy (alarm_label.filepath1, filepath);
    /*
     *      Create directory structure
     */
    sprintf (new_filename, "%s/%s", w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
             filepath);
    new_dir_mkdir (new_filename, S_IRWXU |
                   S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
    /*
     *      Name & Open alarm file  
     */
    sprintf (new_filename, "%s/%s/%s.%s",
             w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
             alarm_label.filepath1, alarm_label.filename, Filetype[2]);
    alarm_label.filehandle = NULL;
    alarm_label.filehandle = fopen (new_filename, "a");
    /**/
      /*
       *      Name & Open eng file    
       */
      sprintf (new_filename, "%s/%s/%s.%s",
               w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
               eng_label.filepath1, eng_label.filename, Filetype[1]);
    eng_label.filehandle = NULL;
    eng_label.filehandle = fopen (new_filename, "a");
    /**/
      /*
       *      Name & Open ipc file    
       */
      sprintf (new_filename, "%s/%s/%s.%s",
               w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
               ipc_label.filepath1, ipc_label.filename, Filetype[0]);
    ipc_label.filehandle = fopen (new_filename, "a");
    /**/
      /*
       *      Name & Open bfdl file   
       */
      sprintf (new_filename, "%s/%s/%s.%s",
               w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
               bfdl_label.filepath1, bfdl_label.filename, Filetype[0]);
    bfdl_label.filehandle = fopen (new_filename, "a");
    /**/
      /*
       *      Name & Open dust file   
       */
      sprintf (new_filename, "%s/%s/%s.%s",
               w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
               dust_label.filepath1, dust_label.filename, Filetype[0]);
    dust_label.filehandle = fopen (new_filename, "a");
    /**/
      /*
       *      Name & open housekeeping
       */
      sprintf (new_filename, "%s/%s/%s.%s",
               w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
               label.filepath1, label.filename, Filetype[0]);
    label.filehandle = fopen (new_filename, "a");
    /**/
      /*
       *      
       */
      strcpy (old_filename, new_filename);
  } else {
    memset (new_filename, ' ', strlen (new_filename));
  }

  if (strcmp (new_utc, label.scet_start) < 0) {
    strcpy (label.scet_start, new_utc);
    sprintf (label.sclk_start, "1/%d:%03d",
             buf->packet.chdo_tag.sclk.seconds, UTIL_extract_CDS_RTI (buf));
    strcpy (label.plot_start, label.sclk_start);
  }
  if (strcmp (new_utc, label.scet_stop) > 0) {
    strcpy (label.scet_stop, new_utc);
    sprintf (label.sclk_stop, "1/%d:%03d",
             buf->packet.chdo_tag.sclk.seconds, UTIL_extract_CDS_RTI (buf));
    strcpy (label.plot_stop, label.scet_stop);
  }

  memcpy (archive_record_hsk.time_series.byte_sample,
          buf->packet.mpp.mini_packet,
          buf->packet.chdo_ancillary.type_94.non_fill_length);

  archive_record_hsk.sclk.seconds = buf->packet.chdo_tag.sclk.seconds;
  archive_record_hsk.sclk.partition = Partition;
  archive_record_hsk.sclk.fine = UTIL_extract_CDS_RTI (buf);

  archive_record_hsk.scet.days = spice_time_array.days;
  archive_record_hsk.scet.msec_of_day = spice_time_array.msec_of_day;

  archive_record_hsk.fsw_ver =
    rpws_fsw_ver (&archive_record_hsk, NULL, NULL, NULL);

  archive_record_hsk.samples =
    buf->packet.chdo_ancillary.type_94.non_fill_length;

  archive_record_hsk.data_rti =
    (buf->packet.chdo_tag.sclk.seconds << 3) & 0xFFF8;
  archive_record_hsk.data_rti |= UTIL_extract_CDS_RTI (buf) >> 5;

  label.record_count[0]++;
  label.record_count[1]++;

  if (force_flag || !misc_status (buf, &archive_record_hsk)) {
    if (hsk_flag) {
      fwrite ((char *) &archive_record_hsk,
              archive_record_hsk.record_bytes, 1, label.filehandle);
    }
    if (dust_flag)
      dust_extract_write (&dust_label, buf);
    /**/ if (bfdl_flag)
      bfdl_extract_write (&bfdl_label, buf);
    /**/ if (ipc_flag)
      ipc_extract_write (&ipc_label, buf);
    /**/ if (eng_flag) {
      char *temp;

      temp = eng_extract_write (&eng_label, buf);
      /**/ if (alarm_flag)
        alarm_extract_write (&alarm_label, buf, temp);
    /**/}
  }
}

int main (int argc, char *argv[])
{
  struct MDB *mdb_file;                 /* pointer to mdb control structure */
  struct CDS_buffer *buf;               /* pointer to data buffer */
  char *database_path = NULL;
  char *database_file = NULL;
  char *start_time = NULL;
  char *stop_time = NULL;
  int nProdVerId   = 1;
  char c;
  int i;
  char* metafile = NULL;

  if((argc < 2)||(strcmp(argv[1], "-h") == 0)){
    rpws_housekeeping_help (argv[0], stdout);
    exit (0);
  }

	if( getenv("CAS_TIME_KERNELS") == NULL){
		fprintf(stderr, "Can't load SCLK-SCET correlation, CAS_TIME_KERNELS "
				  "is not defined.\n");
		return 13;
  }
  metafile = getenv("CAS_TIME_KERNELS");
  
  furnsh_c(metafile);

	while ((c = getopt (argc, argv, arglist)) != EOF) {
		switch (c) {
		case 'f':
			force_flag = 0;
			break;
		case 'h':
			hsk_flag = 0;
			break;
		case 'z':
			Zero_Flag = 1;
			break;
		case 'b':
			bfdl_flag = 1;
			break;
		case 'd':
			dust_flag = 1;
			break;
		case 'i':
			ipc_flag = 1;
			break;
		case 'e':
			eng_flag = 1;
			break;
		case 'a':
			eng_flag = 1;
			alarm_flag = 1;
			break;
		case 'p':
			database_path = optarg;
			break;
		case 'n':
			database_file = optarg;
			break;
		case 'm':
			Mafi_Flag = 1;
			break;
		case 'v':
			nProdVerId = atoi(optarg);
			if((nProdVerId < 1)||(nProdVerId > 99)){
				fprintf(stdout, "Product Version Id numbers must be > 0 and < 99, "
				        "recieved %d", nProdVerId);
				exit(1);
			}
			break;
		case '?':
			fprintf (stdout, "Invalid Command Line Argument, use -h for help.\n");
			exit(1);
		}
	}

	/** Start/Stop time: Accept start only as well as start/stop  */
	switch (argc - optind) {
	case 2:
		start_time = argv[optind + 0];
		stop_time = argv[optind + 1];
		break;
	  
	case 1:
		start_time = argv[optind + 0];
		stop_time = NULL;
		break;
	  
	default:
		rpws_housekeeping_help(argv[0], stdout);
		printf("Invalid Command Line Arguments\n");
		exit (1);
	}
  
	if(start_time){
		i = MDB_SCET_strg(start_time);
		printf("start time %08X=MDB_SCET_strg(\"%s\");\n", i, start_time); 
	}
  
	if(stop_time){
		i = MDB_SCET_strg(stop_time);
		printf("stop time %08X=MDB_SCET_strg(\"%s\");\n", i, stop_time);
	}

	archive_record_hsk.record_bytes = HSK_RECORD_SIZE + 32;
	archive_record_hsk.validity_flag = 0x60;
	archive_record_hsk.status_flag = 0;
	archive_record_hsk.frequency_band = 0;
	archive_record_hsk.gain = 0;
	archive_record_hsk.antenna = 0;
	archive_record_hsk.agc = 0;
	archive_record_hsk.hfr_xlate = 0;
	archive_record_hsk.sub_rti = 0;
	archive_record_hsk.lp_dac_0 = 0;
	archive_record_hsk.lp_dac_1 = 0;
	archive_record_hsk.spare[0] = 0;
	archive_record_hsk.spare[1] = 0;
	archive_record_hsk.spare[2] = 0;
	initialize_label(&label, 1, nProdVerId);

	archive_record_dust.record_bytes = HSK_DUST_RECORD_SIZE + 32;
	archive_record_dust.validity_flag = 0x60;
	archive_record_dust.status_flag = 0;
	archive_record_dust.frequency_band = 0;
	archive_record_dust.gain = 0;
	archive_record_dust.antenna = 0;
	archive_record_dust.agc = 0;
	archive_record_dust.hfr_xlate = 0;
	archive_record_dust.sub_rti = 0;
	archive_record_dust.lp_dac_0 = 0;
	archive_record_dust.lp_dac_1 = 0;
	archive_record_dust.spare[0] = 0;
	archive_record_dust.spare[1] = 0;
	archive_record_dust.spare[2] = 0;
	initialize_label (&dust_label, 0, nProdVerId);

	archive_record_bfdl.record_bytes = HSK_BFDL_RECORD_SIZE + 32;
	archive_record_bfdl.validity_flag = 0x60;
	archive_record_bfdl.status_flag = 0;
	archive_record_bfdl.frequency_band = 0;
	archive_record_bfdl.gain = 0;
	archive_record_bfdl.antenna = 0;
	archive_record_bfdl.agc = 0;
	archive_record_bfdl.hfr_xlate = 0;
	archive_record_bfdl.sub_rti = 0;
	archive_record_bfdl.lp_dac_0 = 0;
	archive_record_bfdl.lp_dac_1 = 0;
	archive_record_bfdl.spare[0] = 0;
	archive_record_bfdl.spare[1] = 0;
	archive_record_bfdl.spare[2] = 0;
	initialize_label (&bfdl_label, 0, nProdVerId);

	archive_record_ipc.record_bytes = HSK_IPC_RECORD_SIZE + 32;
	archive_record_ipc.validity_flag = 0x60;
	archive_record_ipc.status_flag = 0;
	archive_record_ipc.frequency_band = 0;
	archive_record_ipc.gain = 0;
	archive_record_ipc.antenna = 0;
	archive_record_ipc.agc = 0;
	archive_record_ipc.hfr_xlate = 0;
	archive_record_ipc.sub_rti = 0;
	archive_record_ipc.lp_dac_0 = 0;
	archive_record_ipc.lp_dac_1 = 0;
	archive_record_ipc.spare[0] = 0;
	archive_record_ipc.spare[1] = 0;
	archive_record_ipc.spare[2] = 0;
	initialize_label (&ipc_label, 0, nProdVerId);

	initialize_label (&eng_label, 0, nProdVerId);
	initialize_label (&alarm_label, 0, nProdVerId);

	mdb_file = MDB_open(start_time, stop_time, database_path, database_file, file_type);
	
	buf = (struct CDS_buffer *) MDB_read(mdb_file);
	if(buf){
		i = 0;
    	while (path_list[i]) {
      
			if(i) strcat (path, "/");
	      strcat(path, path_list[i]);
	      new_dir_mkdir(path, S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);
			i++;
		}
	    
		while(buf){
	      if(UTIL_extract_HSK (buf)) main_file(buf, nProdVerId);
	      
			buf = (struct CDS_buffer *) MDB_read (mdb_file);
		}
	}
	
	if(label.filehandle){
		
		fclose(label.filehandle);

		strcpy(Mission_phase, rpws_mission_phase_name(label.scet_start, label.scet_stop, 0, 0));
		strcpy(Target, rpws_target_name(label.scet_start, label.scet_stop, 0, 0));	
		strcpy(Coordinate, rpws_coordinate_name (label.scet_start, label.scet_stop, 0, 0));
		strcpy(Orbit, rpws_orbit_number(label.scet_start, label.scet_stop, 0));
	
		if(Zero_Flag) rpws_time_patch (&label);
	
		rpws_label_write(&label, w_directory);
	}

  if (dust_label.filehandle) {
    fclose (dust_label.filehandle);
    /**/ if (dust_label.record_count[0]) {
      if (Zero_Flag)
        rpws_time_patch (&dust_label);
      rpws_label_write (&dust_label, w_directory);
    } else {
      char temp[256];

      sprintf (temp, "%s/%s/%s.%s",
               w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
               dust_label.filepath1, dust_label.filename, Filetype[0]);
      unlink (temp);
    }
  }

  if (bfdl_label.filehandle) {
    fclose (bfdl_label.filehandle);
    /**/ if (bfdl_label.record_count[0]) {
      if (Zero_Flag)
        rpws_time_patch (&bfdl_label);
      rpws_label_write (&bfdl_label, w_directory);
    } else {
      char temp[256];

      sprintf (temp, "%s/%s/%s.%s",
               w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
               bfdl_label.filepath1, bfdl_label.filename, Filetype[0]);
      unlink (temp);
    }
  }

  if (ipc_label.filehandle) {
    fclose (ipc_label.filehandle);
    /**/ if (ipc_label.record_count[0]) {
      if (Zero_Flag)
        rpws_time_patch (&ipc_label);
      rpws_label_write (&ipc_label, w_directory);
    } else {
      char temp[256];

      sprintf (temp, "%s/%s/%s.%s",
               w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
               ipc_label.filepath1, ipc_label.filename, Filetype[0]);
      unlink (temp);
    }
  }

  if (eng_label.filehandle) {
    fclose (eng_label.filehandle);
    /**/ if (eng_label.record_count[0]) {
      if (Zero_Flag)
        rpws_time_patch (&eng_label);
      rpws_label_write (&eng_label, w_directory);
    } else {
      char temp[256];

      sprintf (temp, "%s/%s/%s.%s",
               w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
               eng_label.filepath1, eng_label.filename, Filetype[1]);
      unlink (temp);
    }
  }

  if (alarm_label.filehandle) {
    fclose (alarm_label.filehandle);
    /**/ if (alarm_label.record_count[0]) {
      if (Zero_Flag)
        rpws_time_patch (&alarm_label);
      rpws_label_write (&alarm_label, w_directory);
    } else {
      char temp[256];

      sprintf (temp, "%s/%s/%s.%s",
               w_directory[RPWS_ARCHIVE_HOUSEKEEPING],
               alarm_label.filepath1, alarm_label.filename, Filetype[2]);
      unlink (temp);
    }
  }

  MDB_close(mdb_file);
}
