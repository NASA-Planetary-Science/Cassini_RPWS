 /*
  * UTILF.H
  * *
  * *     FILENAME UTILITY
  * *     ----------------
  * *     Used to enforce standard file naming conventions
  * *     ------------------------------------------------
  * *     Support libraries:
  * *             fg.o            command line flags
  * *     ------------------
  * *
  * *   "filename" returns the current file name.
  * *   "filenew"  returns a new file name.
  * *     "filelog"  returns todays log file name
  * *   "FILEname" returns the current file handle.
  * *   "FILEnew"  returns a new file handle.
  * *           1st. argument is FILE_ enum
  * *           2nd. argument is FILE_LOCATE_* enum
  * *   "dumprc"   dumps file contents
  * *           1st. argument is FILE_LOCATE_* enum
  * *   "fname"    returns current filename (i.e. after
  * *                   call to FILEname/FILEnew
  * *           1st. argument is FILE_ enum
  * *   "filepipe" returns file pipe
  * *           1st. argument is FILE_ enum
  * *   "fmnemonic returns the test string that is the
  * *                   keyword that identifies the file type
  * *                   used to build get/put flags on command
  * *                   line and as the keyword in the .filrc
  * *           1st. argument is FILE_ enum
  * *   "fentry"   returns the selected filename
  * *           1st. argument is FILE_ enum          
  * *   "fcdname"  returns a name in the correct format
  * *                   for a CDROM ISO9660
  * *           1st. argument is filename (NO path)
  * *   "fcdpath"  returns the next path to the CD drive(s)
  * *           1st. argument is reset flag
  * *                   0 = continue
  * *                   1 = start at begining
  * *
  * *   "find_open" returns an open file containing the desired
  * *                   data.  Looks in local data as well
  * *                   as cd path... (just like fopen)
  * *           1st. argument is the filename
  * *           2nd. argument is flags
  * *   "find_name" returns the name of the just opened file
  * *
  * *
  * *     The following routines MAY RETURN a NULL POINTER
  * *       in the event that the control file (filerc)
  * *       is not writable.
  * *             FILEnew
  * *             filenew
  * *             filelog
  */

extern FILE *UTIL_FILEname (int, int);  /* return handle for existing file    */
extern FILE *UTIL_FILEnew (int, int);   /* return handle for new file         */
extern char *UTIL_filename (int, int);  /* return filename for existing       */
extern char *UTIL_filelog (int, int);   /* return logfile name (append)       */
extern char *UTIL_filenew (int, int);   /* return filename for new file       */
extern void UTIL_dumprc (int);          /* dump filename array                */
extern char *UTIL_fname (int);          /* return current filename 1=in 0=out */
extern const char *UTIL_fmnemonic (int);      /* return file menmonic               */
extern char *UTIL_fentry (int);         /* return file path                   */
extern char *UTIL_fcdname (char *, int);        /* return CD filename                */
extern char *UTIL_fcdpath (int);        /* return next CD path                */
extern FILE *UTIL_find_open (char *, char *);   /* open file anywhere            */
extern char *UTIL_find_name (void);     /* NAME OF FILE                       */
extern char *UTIL_filepipe (int);       /* return named pipe                  */
enum
{ FILE_NEW,                             /* 0                            */
  FILE_OLD,                             /* 1                            */
  FILE_PATH,                            /* 2 Data file path ...         */
  FILE_BIN,                             /* BINARY (JPL)                 */
  FILE_RAW,                             /* RAW TELEMETRY RECORDS        */
  FILE_MP,                              /* MINI PACKETS                 */
  FILE_MPUS,                            /* MINI PACKETS (UNSEGMENTED)   */
  FILE_MFR,                             /* MEDIUM FREQ. RECEIVER        */
  FILE_HFR,                             /* HIGH FREQ. RECEIVER          */
  FILE_WFR,                             /* WAVE FORM RECEIVER           */
  FILE_WBR,                             /* WIDE BAND RECEIVER           */
  FILE_LP,                              /* LANGMUIR PROBE               */
  FILE_LFDR,                            /* LOW FREQ. DIGITAL RECEIVER   */
  FILE_DUST,                            /* DUST DETECTOR                */
  FILE_STIM,                            /* STIMULUS RECORDS             */
  FILE_MRO,                             /* MEMORY READ OUT              */
  FILE_HSK,                             /* HOUSEKEEPING                 */
  FILE_STIM_LOG,                        /* Stimulus control log file    */
  FILE_STIM_PIPE,                       /* Stim control pipe            */
  FILE_COMMAND_PIPE,                    /* Command pipe                 */
  FILE_KRONOS,                          /* KRONOS                       */
  FILE_TYPE_END,
  FILE_CDPATH,                          /* Data file path (on cdrom)    */
  FILE_CDAUTO                           /* Data file path (on cdromchanger) */
};
enum
{ FILE_LOCATE_DEFAULT,                  /* default action               */
  FILE_LOCATE_NORMAL,                   /* honor +local/-local flag     */
  FILE_LOCATE_LOCAL,                    /* ALWAYS local                 */
  FILE_LOCATE_GLOBAL,                   /* ALWAYS global                */
  FILE_LOCATE                           /* get current setting          */
};
