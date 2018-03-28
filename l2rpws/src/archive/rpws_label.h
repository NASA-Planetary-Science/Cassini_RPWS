enum
{
  RPWS_LABEL_WBR = 0,
  RPWS_LABEL_WFR = 1,
  RPWS_LABEL_STIM = 2,
  RPWS_LABEL_HSK = 3,
  RPWS_LABEL_DUST = 4,
  RPWS_LABEL_BFDL = 5,
  RPWS_LABEL_IPC = 6,
  RPWS_LABEL_ENG = 7,
  RPWS_LABEL_ALARM = 8,
  RPWS_LABEL_RAW = 9,
  RPWS_LABEL_DUPLICATE = 0x7FFFFF00
};
enum
{
  RPWS_LABEL_PAD_1024 = 1,
  RPWS_LABEL_PAD_2048 = 2,
  RPWS_LABEL_PAD_4096 = 4,
  RPWS_LABEL_PAD_6144 = 6,
  RPWS_LABEL_PAD_8192 = 8,
  RPWS_LABEL_PAD_20480 = 9,
  RPWS_LABEL_PAD_STIM = 21,
  RPWS_LABEL_PAD_DUST = 22,
  RPWS_LABEL_PAD_HSK_ENG = 26,
  RPWS_LABEL_PAD_HSK_IPC = 27,
  RPWS_LABEL_PAD_HSK_BFDL = 28,
  RPWS_LABEL_PAD_HSK_DUST = 29,
  RPWS_LABEL_PAD_HSK = 30
};

#define MODE_MASK_WFR		0x01
#define MODE_MASK_25		0x02
#define MODE_MASK_WFR_LO	0x02
#define MODE_MASK_2_5K		0x04
#define MODE_MASK_WFR_HI	0x04
#define MODE_MASK_WBR		0x10
#define MODE_MASK_10K		0x20
#define MODE_MASK_80K		0x40
#define MODE_MASK_WBR_LO	0x20
#define MODE_MASK_WBR_HI	0x40
#define MODE_MASK_HFR		0x80


static int pad_class[] = { 0,           /*  0 */
  1024,                                 /*  1 */
  2048,                                 /*  2 */
  4096,                                 /*  3 */
  4096,                                 /*  4 */
  6144,                                 /*  5 */
  6144,                                 /*  6 */
  8192,                                 /*  7 */
  8192,                                 /*  8 */
  20480,                                /*  9 */
  20480,                                /* 10 */
  20480,                                /* 11 */
  20480,                                /* 12 */
  20480,                                /* 13 */
  20480,                                /* 14 */
  20480,                                /* 15 */
  20480,                                /* 16 */
  20480,                                /* 17 */
  20480,                                /* 18 */
  20480,                                /* 19 */
  20480,                                /* 20 */
  130 - 32,                             /* 21 */
  1024,                                 /* 22 */
  0,                                    /* 23 */
  0,                                    /* 24 */
  0,                                    /* 25 */
  57,                                   /* 26 */
  32,                                   /* 27 */
  32,                                   /* 28 */
  32,                                   /* 29 */
  192,                                  /* 30 */
  0,                                    /* 31 */
};
enum
{ LABEL_END,
  LABEL_TEXT,
  LABEL_TYPE,
  LABEL_TYPE_D,
  LABEL_FULLFILENAME,
  LABEL_FILENAME,
  LABEL_FILEBAND,
  LABEL_FILECLASS,
  LABEL_FILETYPE,
  LABEL_COPYNAME,
  LABEL_COPYBAND,
  LABEL_COPYCLASS,
  LABEL_COPYTYPE,
  LABEL_FILE,
  LABEL_BAND,
  LABEL_TIME,
  LABEL_DATE,
  LABEL_TIME_TODAY,
  LABEL_DATE_TODAY,
  LABEL_OFFSET,
  LABEL_OFFSET1,
  LABEL_SCLK_ST,
  LABEL_SCLK_SP,
  LABEL_SCET_ST,
  LABEL_SCET_SP,
  LABEL_EPHEM_ST,
  LABEL_EPHEM_SP,
  LABEL_EPHEM_TEXT,
  LABEL_SPAN,
  LABEL_TARGET_0_CRLF,
  LABEL_TARGET_N_CRLF,
  LABEL_TARGET_D_CRLF,
  LABEL_ORBIT_0_CRLF,
  LABEL_ORBIT_N_CRLF,
  LABEL_ORBIT_D_CRLF,
  LABEL_MISSION_PHASE_NAME_0_CRLF,
  LABEL_MISSION_PHASE_NAME_N_CRLF,
  LABEL_MISSION_PHASE_NAME_D_CRLF,

  LABEL_RECORD_BYTES,
  LABEL_FILE_RECORDS,
  LABEL_FILE_RECORDS_0,
  LABEL_FILE_RECORDS_1,
  LABEL_BYTES,
  LABEL_ROWS,
  LABEL_ROWS_1,
  LABEL_ROW_BYTES,
  LABEL_ROW_REMAIN_BYTES,
  LABEL_ROW_PREFIX_BYTES,
  LABEL_ROW_SUFFIX_BYTES,
  LABEL_ESTIMATED_FILE_SIZE,
  LABEL_ITEMS,
  LABEL_ITEM_BYTES,
  LABEL_VALID_MINIMUM,
  LABEL_VALID_MAXIMUM,
  LABEL_SAMPLING_PARAMETER_INTERVAL,
  LABEL_SAMPLE_SIZE,
  LABEL_PAD27,
  LABEL_TEXT_CRLF,
  LABEL_TEXT_WFR,
  LABEL_TEXT_WBR,
  LABEL_CRLF,
  LABEL_MSB,
  LABEL_VERSION,
  LABEL_VERSION1,
  LABEL_PRODUCT,
  LABEL_PRODUCT_VERSION_ID,
  LABEL_NOTE,
  LABEL_DELIM,

  LABEL_IGNORE
};
struct LABEL_RECORD
{
  int flag;
  char *label;
};

#define RPWS_LABEL_CHAR_FIELDS 17
#define RPWS_LABEL_MISC_BUFFER_SIZE 64
struct RPWS_LABEL
{
  struct RPWS_LABEL *link;
  int instrument;                       /* 0=WBR 1=WFR */
  short fband;                          /* 25 2.5K 10K 80K */
  short mode_mask;                      /* bits for 5 modes */

#ifdef HFR_XLATE
  short hfr_xlate[HFR_XLATE];           /*  */
#else
  int hfr_xlate;
#endif
  double span;                          /*  */
  double duration;                      /* dataset duration, seconds */
  int dataset_size[2];                  /* byte count */
  int sample_count;                     /*  */
  int pad_class;                        /*  */
  int record_size;                      /*  */
  int record_count[2];                  /*  */
  int Label_Line_Pad;                   /* Are labels padded to 80 colums ? */
  FILE *filehandle;                     /* avoid repeated opens/closes */
  char *filename;                       /* RPWS_LABEL_CHAR_FIELD   1 */
  char *filepath1;                      /* RPWS_LABEL_CHAR_FIELD   2 */
  char *filepath2;                      /* RPWS_LABEL_CHAR_FIELD   3 */
  char *filepath3;                      /* RPWS_LABEL_CHAR_FIELD   4 */
  char *thumbname;                      /* RPWS_LABEL_CHAR_FIELD   5 */
  char *utc_date;                       /* RPWS_LABEL_CHAR_FIELD   6 */
  char *utc_time;                       /* RPWS_LABEL_CHAR_FIELD   7 */
  char *sclk_start;                     /* RPWS_LABEL_CHAR_FIELD   8 */
  char *sclk_stop;                      /* RPWS_LABEL_CHAR_FIELD   9 */
  char *scet_start;                     /* RPWS_LABEL_CHAR_FIELD  10 */
  char *scet_start_2;                   /* RPWS_LABEL_CHAR_FIELD  11 */
  char *scet_stop;                      /* RPWS_LABEL_CHAR_FIELD  12 */
  char *ephem_start;                    /* RPWS_LABEL_CHAR_FIELD  13 */
  char *ephem_stop;                     /* RPWS_LABEL_CHAR_FIELD  14 */
  char *plot_start;                     /* RPWS_LABEL_CHAR_FIELD  15 */
  char *plot_stop;                      /* RPWS_LABEL_CHAR_FIELD  16 */
  char *scet_start_3;                   /* RPWS_LABEL_CHAR_FIELD  17 */
  char *Label_Version;                  /*  */
  char *Mission_Phase;                  /*  */
  char *Target;                         /*  */
  char *Coordinate;                     /*  */
  char *Orbit;                          /*  */
  char *Product;                        /*  */
  int ProdVerId;                        /* Use a look table to pick versions */
  char text_area[RPWS_LABEL_CHAR_FIELDS][RPWS_LABEL_MISC_BUFFER_SIZE];
};

int rpws_label_write (struct RPWS_LABEL *data_record, char *directory[]);
int master_detail (struct RPWS_LABEL *data_record, char *directory[]);
int master_label (struct RPWS_LABEL *data_record, char *lbl,
                  char *directory[]);
int master_cd_label (struct RPWS_LABEL *master_record,
                     struct RPWS_LABEL *wbr_record,
                     struct RPWS_LABEL *wfr_record, char *directory[],
                     char *cd_index, char *cd_count, char *cd_format,
                     double Byte_count);
int rpws_label_browse_image (struct RPWS_LABEL *detail_record,
                             char *directory, char *sub_directory,
                             char *thumb_flag, int flag);
