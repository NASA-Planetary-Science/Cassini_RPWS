struct FSW
{
  int stim_clue;
  int version;
  int patch_level;
  int partition;
  int sclk;
  int sclk_wbr;
  char *version_string;
  char *version_date;
  char *version_date_wbr;
};

#ifndef _RPWS_FSW_VER_
char *rpws_ver_ver (void);
int rpws_ver_load (char *filename);
int rpws_ver_help (FILE * handle, int flag);
int rpws_ver_dump (FILE * handle, int wbr_flag);
int rpws_fsw_ver (struct ARCHIVE_TIME_SERIES *archive,
                  int *partition, int *patch_level, char *version_string[]);
int rpws_ieb_ver (struct ARCHIVE_TIME_SERIES *archive,
                  char *version_string[]);
int rpws_seq_ver (struct ARCHIVE_TIME_SERIES *archive,
                  char *version_string[]);
char *rpws_mpn_ver (struct RPWS_LABEL *label);
char *rpws_target_ver (struct RPWS_LABEL *label);
char *rpws_coordinate_ver (struct RPWS_LABEL *label);
char *rpws_orbit_ver (struct RPWS_LABEL *label);

/************************************************************************/

/*	GENERIC Entry Points						*/

/*	these take start/stop times using either SCET or SCLK		*/

/*----------------------------------------------------------------------*/

/* These calls are for combined stuff.  Using start and stop		*/

/*    time, all the included strings are combined and duplicates	*/

/*    are removed.  The resulting string is returned.			*/

/*  see /opt/project/cassini/src/archive/rpws_fsw.c for example code	*/

/*   quote_flag is non-zero to quote the strings returned by this call  */

/************************************************************************/
char *rpws_target_name (char *start_time, char *stop_time, int quote_flag,
                        int wbr_flag);
char *rpws_coordinate_name (char *start_time, char *stop_time, int quote_flag,
                            int wbr_flag);
char *rpws_orbit_number (char *start_time, char *stop_time, int quote_flag);
char *rpws_mission_phase_name (char *start_time, char *stop_time,
                               int quote_flag, int wbr_flag);
char *rpws_software_version_name (char *start_time, char *stop_time,
                                  int quote_flag);
char *rpws_sequence_name (char *start_time, char *stop_time, int quote_flag);
char *rpws_ieb_load_name (char *start_time, char *stop_time, int quote_flag);
#endif
