
/* utilfile - Data file routines */
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <stdlib.h>

#include <fg.h>  /* Part of libfg now, see saturn/svn/util/C/trunk/libfg */

#include "utilf.h"

#define SLEN 1024
struct filetype
{
  int type;                             /* enum defined in utilf.h */
  int format_flag;                      /* how name is constructed */
  const char *format;                   /* printf control string */
  const char *mnemonic;                 /* mnenonic in" filerc" */
  char pipe[SLEN];                      /* path to named pipe */
  char entry[SLEN];                     /* file path */
};
enum
{
  FORMAT_NONE,
  FORMAT_DEFAULT,
  FORMAT_KRONOS,
  FORMAT_LOG,
  FORMAT_CD
};

#define BEGIN_INDEX 4
static char ident[] = { "IDENT utilfile.c 16-Aug-2016" };

char g_sUtilFilePath[SLEN] = {'\0'};

/* Okay, getting rid of everything, then adding back in the ones that
   are used... */

static struct filetype ft[] = {
	{0,                 FORMAT_NONE,    "t%2.2d%2.2d%2.2d",       "",        "",                         ""},
	{1,                 FORMAT_DEFAULT, "",                       "",        "",                         ""},

  /* Get the .entry item for this one from the environment variable 
     RPWS_DATA, if present, used to be "/opt /project /cassini /data /temp /" */
  /* FILE_PATH,         FORMAT_DEFAULT, "",                      "PATH",     "",                      "", */

	{FILE_CDPATH,       FORMAT_CD,      "",                      "CD_PATH",  "",                         ""},
	{FILE_CDAUTO,       FORMAT_CD,      "",                      "CD_LUN_0", "",                         ""},
	{FILE_CDAUTO,       FORMAT_CD,      "",                      "CD_LUN_1", "",                         ""},
  {FILE_CDAUTO,       FORMAT_CD,      "",                      "CD_LUN_2", "",                         ""},
  {FILE_CDAUTO,       FORMAT_CD,      "",                      "CD_LUN_3", "",                         ""},
  {FILE_CDAUTO,       FORMAT_CD,      "",                      "CD_LUN_4", "",                         ""},
  {FILE_CDAUTO,       FORMAT_CD,      "",                      "CD_LUN_5", "",                         ""},
  {FILE_CDAUTO,       FORMAT_CD,      "",                      "CD_LUN_6", "",                         ""},
  {FILE_CDAUTO,       FORMAT_CD,      "",                      "CD_LUN_7", "",                         ""},
  {FILE_CDAUTO,       FORMAT_CD,      "",                      "CD_LUN_8", "",                         ""},
  {FILE_CDAUTO,       FORMAT_CD,      "",                      "CD_LUN_9", "",                         ""},
  
  /* Using pipe's for software delvelopment, by default have the named
     pipes appear in RPWS_DATA/dev, only the suff after RPWS_DATA is
	  included here. */
  
  {FILE_BIN,          FORMAT_DEFAULT, "%s.b%2.2d",             "BIN",      "dev/binary_cds",  		   ""},
  {FILE_RAW,          FORMAT_DEFAULT, "%s.r%2.2d",             "RAW",      "dev/raw_cds",  			   ""},
  {FILE_MP,           FORMAT_DEFAULT, "%s.m%2.2d",             "MP",       "dev/segment_minipacket",   ""},
  {FILE_MPUS,         FORMAT_DEFAULT, "%s.u%2.2d",             "MPUS",     "dev/unsegment_minipacket", ""},
  {FILE_MFR,          FORMAT_DEFAULT, "%s.s%2.2d",             "MFR",      "dev/mfr_minipacket", 	   ""},
  {FILE_HFR,          FORMAT_DEFAULT, "%s.h%2.2d",             "HFR",      "dev/hfr_minipacket", 	   ""},
  {FILE_WBR,          FORMAT_DEFAULT, "%s.w%2.2d",             "WBR",      "dev/wbr_minipacket", 	   ""},
  {FILE_WFR,          FORMAT_DEFAULT, "%s.f%2.2d",             "WFR",      "dev/wfr_minipacket", 	   ""},
  {FILE_LP,           FORMAT_DEFAULT, "%s.p%2.2d",             "LP",       "dev/langmuir_minipacket",  ""},
  {FILE_LFDR,         FORMAT_DEFAULT, "%s.l%2.2d",             "LFDR",     "dev/lfdr_minipacket",	   ""},
  {FILE_DUST,         FORMAT_DEFAULT, "%s.d%2.2d",             "DUST",     "dev/dust_minipacket",	   ""},
  {FILE_STIM,         FORMAT_DEFAULT, "%s.t%2.2d",             "STIM",     "dev/stimulus_minipacket",  ""},
  {FILE_MRO,          FORMAT_DEFAULT, "%s.x%2.2d",             "MRO",      "dev/memory_minipacket",    ""},
  {FILE_HSK,          FORMAT_DEFAULT, "%s.k%2.2d",             "HSK",      "dev/raw_housekeeping",     ""},
  {FILE_STIM_LOG,     FORMAT_LOG,     "st%2.2d%2.2d%2.2d.log", "STIMLOG",  "dev/stim_control_log",     ""},
  {FILE_STIM_PIPE,    FORMAT_NONE,    "",                      "STIM",     "dev/stim_control_pipe",    ""},
  {FILE_COMMAND_PIPE, FORMAT_NONE,    "",                      "CMMD",     "dev/command_pipe",		   ""},
  {FILE_KRONOS,       FORMAT_KRONOS,  "kronos.%3.3d",          "KRONOS",   "",                         ""},
  {0,                 1,              "",                      "",         "",                         ""}
};

static const char rclocal[] = { '\0' };

/* #ifdef __sun
static char rcpath[] = { "/opt/ project/ cassini/ data/ temp/" };
#else
static char rcpath[] = { "/usr/cassini/data/" };
#endif
*/
static const char *rcfile = "filerc";
static FILE *FILErc = NULL;
static char iname[SLEN] = {'\0'};
static char oname[SLEN] = {'\0'};
static int local_ = 0;
static char findname[256] = { "" };
static mode_t mode = S_IRUSR | S_IRGRP | S_IROTH | S_IWUSR | S_IWGRP;

static void strip (char *txt)
{
  int act = 1;
  int len;

  while (act) {
    act = 0;
    len = strlen (txt) - 1;
    switch (txt[len]) {
     case 0x01:
     case 0x02:
     case 0x03:
     case 0x04:
     case 0x05:
     case 0x06:
     case 0x07:
     case 0x08:
     case 0x09:
     case 0x0A:
     case 0x0B:
     case 0x0C:
     case 0x0D:
     case 0x0E:
     case 0x0F:
     case 0x7F:
       txt[len] = 0x00;
       act = 1;
       break;
     default:
       break;
    }
  }
}

   /*
    *   local = 0 to honor "+local/-local" command line flag
    *   
    */
static int util_local (int local)
{
  switch (local) {
   case FILE_LOCATE_DEFAULT:
   case FILE_LOCATE_NORMAL:
     local_ = fg_flag ("local");
     break;
   case FILE_LOCATE_LOCAL:
     local_ = 1;
     break;
   case FILE_LOCATE_GLOBAL:
     local_ = 0;
     break;
   default:
   case FILE_LOCATE:
     break;
  }
  return local_;
}
static int readrc (int local)
{
  int index;
  char string[SLEN];
  char str[SLEN];
  char *tok[2];
  char delim[] = { "=" };
  char fname[SLEN];

  static int status = 0;
  static int first = 1;

  if (first)
    util_local (local);
  
  first = 0;
  
  if (local_){
    strcpy (fname, rclocal);
  }
  else{
	  
	 if(getenv("RPWS_DATA")){
	    strcpy(fname, getenv("RPWS_DATA"));
		 strcat(fname, "/temp/");
	 }
	 else{
		 fprintf(stderr, "utilfile(readrc): RPWS_DATA environment variable "
		         "was not defined, local_=%d\n", local_);
		 return -1; 
	 }
 }
  
  strcat (fname, rcfile);

  if (fg_flag ("utilfile"))
    fprintf (stderr,
             "utilfile(readrc): fopen(%s)  local_=%d\n", fname, local_);
  FILErc = fopen (fname, "r");

  if (!FILErc) {
    fprintf (stderr,
             "utilfile(readrc): fopen(%s) failed when local_=%d\n",
             fname, local_);
    return -1;
  }

  while (fgets (string, SLEN - 1, FILErc)) {
    if (fg_flag ("utilfile"))
      fprintf (stderr, "utilfile(readrc): fgets(%s)\n", string);
    strcpy (str, string);
    tok[0] = strtok (str, delim);
    tok[1] = strtok (NULL, delim);
    index = 1;
    while (ft[index].type) {
      if (!strcmp (ft[index].mnemonic, tok[0])) {
        strcpy (ft[index].entry, tok[1]);
        status += 1;
        strip (ft[index].entry);
        break;
      }
      index += 1;
    }
  }
  fclose (FILErc);
  return status;
}
static int writerc (int local)
{
  int index = 1;
  char string[SLEN];
  char fname[SLEN];

  if (local_){
    strcpy (fname, rclocal);
 }
  else{
	 if(!getenv("RPWS_DATA")){
		fprintf(stderr, "writerc, RPWS_DATA env. var. is not defined");
		return 0; 
	 }
    snprintf(fname, SLEN -2, "%s/temp/", getenv("RPWS_DATA"));
 }
  strcat (fname, rcfile);
  if (fg_flag ("utilfile"))
    fprintf (stderr,
             "utilfile(writerc): fopen(%s)  local_=%d\n", fname, local_);
  FILErc = fopen (fname, "w");
  chmod (fname, mode);

  if (!FILErc)
    return EOF;
  while (ft[index].type) {
    if (ft[index].entry[0]) {
      sprintf (string, "%s=%s\n", ft[index].mnemonic, ft[index].entry);
      fputs (string, FILErc);
    }
    index += 1;
  }
  return fclose (FILErc);
}
static char *datestr (const char *format)
{
  const char *format_string;
  static char ds[32] = {'\0'};  /* "tyymmdd        "*/
  struct tm *tim;
  time_t t1;

  time (&t1);
  tim = localtime (&t1);
  if (format)
    format_string = format;
  else
    format_string = ft[0].format;
  sprintf (ds,
           format_string, tim->tm_year % 100, tim->tm_mon + 1, tim->tm_mday);
  return ds;
}

 /*
  *
  */
static int util_index (int flag)
{
  int index = 2;

  while (ft[index].type) {
    if (flag == ft[index].type)
      return index;
    index += 1;
  }
  return 0;
}
static char *util_gp (int flag, int index)
{
  int i;
  static char txt[32];
  char mnelc[32];

  txt[0] = 0;
  switch (flag) {
   case FILE_OLD:
     strcpy (txt, "get");
     break;
   case FILE_NEW:
     strcpy (txt, "put");
     break;
   default:
     break;
  }
  for (i = 0; i < (int) strlen (ft[util_index (index)].mnemonic) + 1; i++)
    mnelc[i] = tolower (ft[util_index (index)].mnemonic[i]);
  strcat (txt, mnelc);
  return txt;
}

 /*
  * Dump the control file
  * * Debugging routine to dump the control structures that are
  * * built up from reading the control file.
  */
void UTIL_dumprc (int local)
{                                       /* (may be NULL) path to local control file */
  int index = 2;

  if (readrc (local) <= 0)
    return;
  fprintf (stderr, "\n");
  while (ft[index].type) {
    fprintf (stderr, "[%3d] %s=%s\n",
             index, ft[index].mnemonic, ft[index].entry);
    index += 1;
  }
}

 /*
  * Determine current filename in use for given data type
  * * returns string, current filename for given type.
  */
char *UTIL_filename (int type /* FILE_ enum in utilf.h */ ,
                     int local)
{                                       /* (may be NULL) path to local control file */
  int index = 1;

  if (readrc (local) <= 0)
    return NULL;
  if ( (index = util_index (type)))
    if (ft[index].entry[0]) {
	  
	  	if(!getenv("RPWS_DATA")){
			fprintf(stderr, "In UTIL_filename, RPWS_DATA is not defined\n");
			return NULL;
		}
	  
		strcpy (iname , getenv("RPWS_DATA"));
      strcat (iname, "/temp/");
      strcat (iname, ft[index].entry);
      return iname;
    }
  return NULL;
}

 /*
  * Build new filename for given type.
  * * Build new filename for given type.  Must take date
  * * and file-sequence-number into account and build a
  * * new filename.
  * * returns NEW filename for given data type
  */
char *UTIL_filenew (int type /* FILE_ enum in utilf.h */ ,
                    int local)
{                                       /* (may be NULL) path to local control file */
  int index = 1;
  int num = 0;
  /* char string[SLEN]; */

  if (readrc (local) <= 0)
    return NULL;
  /*
   * no match, asked for bad file 
   */
  if (!(index = util_index (type)))
    return NULL;
  /*
   * extract current version number 
   */
  if (ft[index].entry)
    switch (ft[index].format_flag) {
     default:
     case FORMAT_DEFAULT:              /* tyymmdd.tnn */
       num = strtol (&ft[index].entry[9], NULL, 10);    /* 01234567890123456789 */
       ft[index].entry[7] = 0;          /* tyymmdd     */
       if (strcmp (ft[index].entry, datestr (NULL)))    /* is it tomorrow yet ? */
         num = 0;                       /* tomorrow means start over */
       break;
     case FORMAT_KRONOS:               /* kronos.nnn */
       num = strtol (&ft[index].entry[7], NULL, 10);    /* 01234567890123456789 */
       break;
    }
  /*
   * build todays date 
   */
  switch (ft[index].format_flag) {
   default:
     break;
   case FORMAT_DEFAULT:
     sprintf (ft[index].entry, ft[index].format, datestr (NULL), ++num);
     break;
   case FORMAT_KRONOS:
     sprintf (ft[index].entry, ft[index].format, ++num);
     break;
  }
  /*
   * save for other to use 
   */
  if (writerc (local) == EOF)
    return NULL;
  /*
   * apply default path information 
   */
  if(!getenv("RPWS_DATA")){
	 fprintf(stderr, "UTIL_filenew, RPWS_DATA is not set.");
	 return NULL;  
  }
  strcpy (oname, getenv("RPWS_DATA"));
  strcat (oname, "/temp/");
  strcat (oname, ft[index].entry);
  return oname;
}

 /*
  * Determine correct filename for today for given filetype.
  * * returns todays filename for given the logfile
  */
char *UTIL_filelog (int type /* FILE_ enum in utilf.h */ ,
                    int local)
{                                       /* (may be NULL) path to local control file */
  int index = 1;
  /* int num = 0; */
  int change = 0;
  /* char string[SLEN]; */
  char entry[128];

  if (readrc (local) <= 0)              /* OK, see whats up */
    return NULL;

  if (!(index = util_index (type)))     /* no match, asked for bad file type */
    return NULL;                        /* recover "index" so we know type */
  /*
   * extract current version number 
   */
  if (ft[index].format_flag != FORMAT_LOG)
    return NULL;                        /* INVALID log file !!! */


  strcpy (entry, datestr (ft[index].format));

  if (ft[index].entry) {
    if (strcmp (ft[index].entry, entry))        /* same ? */
      change = 1;
  } else
    change = 1;

  if (change) {
    strcpy (ft[index].entry, entry);
    if (writerc (local) == EOF)
      return NULL;
  }
  /*
   * apply default path information 
   */
  if(!getenv("RPWS_DATA")){
	  fprintf(stderr, "UTIL_filelog, RPWS_DATA is not set");
	  return NULL;  
  }
  strcpy (oname, getenv("RPWS_DATA"));
  strcat (oname, "/temp/");
  strcat (oname, ft[index].entry);
  return oname;
}

/* Open file handle.  return appropriate file handle */
/* Bug fix -cwp 2012-10-20, all paths now have a return value */
   
FILE *UTIL_FILEname (int flag /* enum FILE_ type */ ,
                     int local)
{                                       /* (may be NULL) path to local control file */
	FILE *h = NULL;
	char gp[32];

	strcpy (iname, "stdin");
	strcpy (gp, util_gp (FILE_OLD, flag));
	if (fg_flag (gp)) {
		
		UTIL_filename (flag, local);
		
		if (fg_flag ("utilfile"))
			fprintf (stderr, "utilfile(UTIL_FILEname): fopen(%s)\n", iname);
		
		h = fopen (iname, "rb");
		if (!h) {
			h = NULL;
			strcpy (iname, "stdin");
		}
		return h;
	}
	return NULL;
}

  /*
   * Open file handle.
   * * return appropriate file handle
   */
FILE *UTIL_FILEnew (int flag, int local)
{                                       /* (may be NULL) path to local control file */
  FILE *h = NULL;
  char gp[32];

  strcpy (oname, "stdout");
  strcpy (gp, util_gp (FILE_NEW, flag));
  if (fg_flag (gp)) {
    UTIL_filenew (flag, local);
    if (oname[0]) {                     /* return NULL if not writable */
      h = fopen (oname, "wb");
      chmod (oname, mode);
      if (!h) {
        h = NULL;
        strcpy (oname, "stdout");
      }
      if (fg_flag ("utilfile"))
        fprintf (stderr, "utilfile(UTIL_FILEnew): fopen(%s)\n", oname);
      return h;
    }
  }
  return NULL;
}

 /*
  *     return filenames in use
  * Return filenames in use.  Should be called 
  * immediatly after UTIL_FILEname/UTIL_FILEnew
  * FLAG_PATH current path to data files
  * 1 indicates input filename
  * 0 indicates output filename
  *returns requested character string
  */
char *UTIL_fname (int flag)
{                                       /* request flag */
  switch (flag) {
   case FILE_PATH:
		if(!getenv("RPWS_DATA")){
			fprintf(stderr, "UTIL_fname, RPWS_DATA is not set");
			return NULL;
		}
		else{
			if(g_sUtilFilePath[0] == '\0')
				 snprintf(g_sUtilFilePath, SLEN - 2, "%s/temp/", getenv("RPWS_DATA"));
			
			return g_sUtilFilePath;
     }
   case FILE_OLD:
     return iname;
   case FILE_NEW:
     return oname;
   default:
     return NULL;
  }
}

   /*
    *   Return the file type mnemonic (its uppercase, so watch out)
    */
const char *UTIL_fmnemonic (int index)
{
  return ft[util_index (index)].mnemonic;
}

   /*
    *   Return the specified actual file name string
    */
char *UTIL_fentry (int index)
{
  return ft[util_index (index)].entry;
}

   /*
    *   Return the specified named pipe path
    */
char *UTIL_filepipe (int index)
{
	char buf[SLEN] = {'\0'};
	int idx; 
	
	idx = util_index (index);
	
	if(ft[idx].pipe[0]){
	  
		/* Get the RPWS_DATA root if needed */
		if(ft[idx].pipe[0] != '/'){
			if(!getenv("RPWS_DATA")){
				fprintf(stderr, "UTIL_filepipe, RPWS_DATA env var is not defined.");
				return NULL;
			}
			strcpy(buf, ft[idx].pipe);
			snprintf(ft[idx].pipe, SLEN - 2, "%s/%s", getenv("RPWS_DATA"), buf);
		}
	  
		return ft[idx].pipe;
	}
  return NULL;
}

   /*
    * Return the filename re-formatted for use with CD file system
    * *         ***CD*****CD*****CD***
    * *   Return the filename re-formatted for use with CD file system
    * * 1st. argument is the filename as it would appear 
    * * on the local file system
    */
char *UTIL_fcdname (char *name, int flag)
{
  static char newname[256];
  static char ver[] = { ";1" };
  int i;
  int max;

  max = strlen (name);
  if (max > 255)
    return NULL;
  newname[0] = 0;
  for (i = 0; i < max; i++) {
    newname[i] = toupper (name[i]);
    newname[i + 1] = 0;
  }
  if (flag)
    strcat (newname, ver);
  return newname;
}

   /*
    *  Returns a list of paths to the CD files.  
    *   This
    *   routine provides support for multiple
    *   CD devices.  They may be implemented as
    *   a CD-changer, or NFS mounts to other
    *   file systems, etc.
    *       "flag" argument is set to NON-ZERO to start
    *           at the begining of the list of paths
    *           and ZERO to return the next in the list.
    *       fcdpath returns a NULL streing when end-of-list
    *           occurs to indicate 'thats all folks' 
    */
char *UTIL_fcdpath (int flag)
{                                       /* position control */
  static int index = 2;

  if (flag) {
    index = 2;
    if (readrc (FILE_LOCATE_DEFAULT) <= 0)      /* OK, see whats up  */
      return NULL;
  }

  while (ft[index].format_flag != FORMAT_CD) {
    if (!ft[index].type) {
      index = 2;
      return NULL;
    }
    index += 1;
  }
  return ft[index++].entry;
}

   /*
    *   find_name may be called after "find_open" has opened 
    *           a file to obtain the full pathname.
    */
char *UTIL_find_name (void)
{
  return findname;
}

   /*
    *   find_open mimics the "fopen" routine.  It returns a file
    *           pointer to an open file if found.  User does NOT
    *           supply path information (path information is obtained
    *           from the filerc file)
    */
FILE *UTIL_find_open (char *name, char *flags)
{
  int flag = 1;
  FILE *fh = NULL;
  char path[256];

  if(!getenv("RPWS_DATA")){
	 fprintf(stderr, "UTIL_find_open, RPWS_DATA environment var is not defined.");
    return NULL;  
  }
  
  strcpy (findname, getenv("RPWS_DATA"));
  strcat (findname, "/temp/");
  strcat (findname, name);
  
  if (fg_flag ("utilfile"))
    fprintf (stderr, "utilfile(UTIL_find_open): fopen(%s)\n", findname);
  if ( (fh = fopen (findname, flags) ) )
    return fh;
  while (1) {
    strcpy (path, UTIL_fcdpath (flag));
    if (!path[0])
      break;
    strcat (path, "/");                 /* PATH */

    strcpy (findname, path);
    strcat (findname, name);
    if (fg_flag ("utilfile"))
      fprintf (stderr, "utilfile(UTIL_find_open): fopen(%s)\n", findname);
    if ( (fh = fopen (findname, flags)) )
      return fh;                        /* unix style */

    strcpy (findname, path);
    strcat (findname, UTIL_fcdname (name, 0));
    if ( (fh = fopen (findname, flags)) )   /* DOS style 8.3 */
      return fh;

    strcpy (findname, path);
    strcat (findname, UTIL_fcdname (name, 1));
    if ( (fh = fopen (findname, flags)))   /* VMS style 8.3;1 */
      return fh;

    flag = 0;
  }
  return NULL;
}
