/*
 * dbupdate.c	For sort/merge stuff
 */
 
/* Version 5.5, cwp 2012-10-24
   * Updated to be able to handle an empty minipacket index file.
   * Set non-zero return values to the calling process if an error is
	  encountered
*/ 

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdbool.h>

#include <time.h>

#include <fg.h>
#include <rpws_sclk.h>

#include "dblock.h"

#define BEFORE 0
#define DURING 2
#define AFTER 4

struct DBASE
{
  struct DBASE *link;
  int sclk_begin;
  int sclk_end;
  char *detail;
};

static char *Title = { "CASSINI database update (dbupdate)" };

static char *Version = { "V5.5" };

static char *flag_text = { "" };

char *cleanup (char *buf)
{
  int len = strlen (buf);
  int i;

  for (i = len - 1; i > 0; i--) {
    if (buf[i] > 0x20 && buf[i] < 0x7F)
      break;
    buf[i] = 0x00;
  }
  return buf;
}

int compare_files (char *filename_1, char *filename_2)
{
  FILE *file_1;
  FILE *file_2;
  char buffer_1[1024];
  char buffer_2[1024];
  int status = 0;
  int eof = 0;
  int count = 0;

  file_1 = fopen (filename_1, "r");
  file_2 = fopen (filename_2, "r");

  /*
   *      1st. check is for zero-length
   */
  if (!file_1) {
    status += 1;
    eof = 1;
  }
  if (!file_2) {
    status -= 1;
    eof = 1;
  }
  /*
   *      If both zero-length, then they match!
   */
  if (eof)
    return status;

  fgets (buffer_1, 1023, file_1);
  fgets (buffer_2, 1023, file_2);

  while (!eof) {                        /* Compare lines */
    status = strcmp (buffer_1, buffer_2);
    count++;
    if (status) {                       /* didn't match */
      eof = 1;                          /* so return */
      status = count;
      break;
    }
    if (!fgets (buffer_1, 1023, file_1)) {      /* EOF */
      eof = 1;
      status += 1;
    }
    if (!fgets (buffer_2, 1023, file_2)) {      /* if both at EOF, then */
      eof = 1;                          /* it's still a match */
      status -= 1;
    }
  }
  fclose (file_1);                      /* leave files closed... */
  fclose (file_2);
  return status;
}

int display (struct DBASE *fred, char *text)
{
  fprintf (stdout, "DISP sclk_%s \n", text);
  fprintf (stdout, "    %08X \n", fred->sclk_begin);
  fprintf (stdout, "    %08X    \n", fred->sclk_end);
  fflush (stdout);
  return 0;
}

/*****************************************************************************
 *   database ---| |--- fred ---| |---- database
 *          end    start   stop   begin
 *                 begin    end
 *
 *     7/2004 WTR: What happens when we're doing
 *             HRS and the SCLK's we're comparing
 *             match???  Change "<" to "<="
 */
int compare (struct DBASE *fred_start,
             struct DBASE *database, struct DBASE *fred_stop)
{
  if (database->sclk_end <= fred_start->sclk_begin)
    return BEFORE;

  if (fred_stop->sclk_end <= database->sclk_begin)
    return AFTER;

  return DURING;
}

/*****************************************************************************/
/* Doesn't affect global items */
struct DBASE *allocate_record (char *buf)
{
  struct DBASE *temp;
  temp = malloc (sizeof (struct DBASE));
  temp->link = NULL;
  temp->detail = malloc (strlen (buf) + 1);
  temp->sclk_begin = strtol (&buf[45], NULL, 16);
  temp->sclk_end = strtol (&buf[54], NULL, 16);
  strcpy (temp->detail, buf);
  return temp;
}

/*****************************************************************************/
/* Doesn't affect global items */
struct DBASE *record_setup (char *buf)
{
  static struct DBASE database;

  database.link = NULL;
  database.sclk_begin = strtol (&buf[45], NULL, 16);
  database.sclk_end = strtol (&buf[54], NULL, 16);
  database.detail = buf;
  return &database;
}

/*****************************************************************************/
/* Saves records to end of the "g_pHead" global linked list,  creates the    */
/* 1st record in head if need be.  Also affects:                             */
/*                                                                           */
/* g_pFredStart - Stores the first item in head fred_start                   */
/*                                                                           */
/* g_pFredStop  - Stores the tail of the list fred_stop                      */
/*                                                                           */
/* g_nFredSequenceErrorFlag - set to 1 if a problem's detected in head list  */

static int g_nFredSequenceErrorFlag = 0;

static struct DBASE* g_pHead = NULL;

static struct DBASE* g_pFredStart = NULL;

static struct DBASE* g_pFredStop = NULL;


void save_record (char *buf)
{
  struct DBASE *temp, *old;

  if(!g_pHead) {                          /* 1st. record ??? */
    g_pHead = allocate_record (buf);
    g_pFredStart = g_pHead;
    return;
  }

  temp = g_pHead;
  while (temp) {
    old = temp;
    temp = temp->link;
  }
  old->link = allocate_record (buf);
  g_pFredStop = old->link;
  if ((g_pFredStop->sclk_begin - old->sclk_begin) <= 0)
    g_nFredSequenceErrorFlag = 1;
  return;
}


/* Dumps all records in the "g_pHead" global to the given file */

void dump_record (FILE * file)
{
  struct DBASE *temp;
  static int first = 1;

  if (!first)
    return;
  first = 0;
  temp = g_pHead;
  while (temp) {
    fprintf (file, "%s %s\n", temp->detail, flag_text);
    temp = temp->link;
  }
}

/*****************************************************************************/
/* Should do more length checking here, but screw it, unless we want to      */
/* bring in a robust string handling lib (like librpwgse) it's not worth     */
/* the huge time investment.  -cwp                                           */

bool getNewName(const char* sDbPath, int nTest, char* sNewPath, int nNewLen){
  
	char sPathCpy[256] = {'\0'};
	char* pSlash = NULL;
	char* pDot = NULL;
	
	if(strlen(sDbPath) > 255){
		fprintf(stderr, "getNewName error: database path is > 255 characters\n");
		return false;
	}
	strcpy(sPathCpy, sDbPath);
	
	
	/* Find out where the database path name starts */
		
	if( (pSlash = strrchr(sPathCpy, '/')) == NULL){
		fprintf(stderr, "Mini-packet index file should be a fully "
			     "qualified path\n");
		return false;
	}
	
	*pSlash = '\0';
	
	pDot = strrchr(sPathCpy, '.');
	
	/* If the last dot is somewhere up the path, just pretend it's not there */
	if(pDot < pSlash)
		pDot = NULL;
	else
		*pDot = '\0';
	
	memset(sNewPath, 0, nNewLen);
		
	char sNewDir[256] = {'\0'};
	int nPrinted = 0;
	if(nTest){
		if( getcwd(sNewDir, 255) == NULL){
			fprintf(stderr, "Couldn't get the current working directory\n");
			return false;
		}
		
		nPrinted = snprintf(sNewPath, nNewLen, "%s/%s.new", sNewDir, pSlash + 1);		
	}
	else{
		nPrinted = snprintf(sNewPath, nNewLen, "%s/%s.new", sPathCpy, pSlash + 1);
	}
	
	if(nPrinted >= nNewLen){
		fprintf(stderr, "buffer too small in getNewName()\n");
		return false;
	}
	
	return true;  
}

/****************************************************************************/
bool getBakName(const char* sDbPath, char* sBakName, int nBakLen){
	
	char sPathCpy[256] = {'\0'};
	char* pDot = NULL;
	int nPrinted = 0;
	time_t nTime;
	struct tm* tmLocal;
	
	nTime = time(NULL);
	tmLocal = localtime(&nTime);

	
	if(strlen(sDbPath) > 255){
		fprintf(stderr, "getNewName error: database path is > 255 characters\n");
		return false;
	}
	strcpy(sPathCpy, sDbPath);
	
	if( (pDot = strrchr(sPathCpy, '.')) != NULL)
		*pDot = '\0';
	
	
	nPrinted = snprintf(sBakName, nBakLen, "%s.%04d_%03dT%02d_%02d_%02d",
            sPathCpy, tmLocal->tm_year + 1900, tmLocal->tm_yday + 1,
	         tmLocal->tm_hour, tmLocal->tm_min, tmLocal->tm_sec);

	if(nPrinted >= nBakLen){
		fprintf(stderr, "buffer too small in getBakName()\n");
		return false;
	}
	return true;
}

/*****************************************************************************/
void prnHelp()
{
	printf(
"Database update program.  This take a list of files that have just been\n"
"through the Sort/Merge step.  These are a combination of fresh and database\n"
"records, properly sorted with no duplicate records.  Works best if they query\n"
"is on 6 hour boundaries (query from about 15 minutes prior to and 15 minutes\n"
"following the period of interest).\n"
"\n"
"We now attempt to gain exclusive access to the database through a lock file\n"
"($RPWS_MPDB.lock).  This file is created and will contain PID, name, and\n"
"time, of the process accessing the database. This file exists only when the\n"
"database is being updated.  If the file already exists when this program\n"
"begins to access the database, it will poll the file, at 1 second intervals,\n"
"until the lock file has been removed, prior to creating a new file and\n"
"accessing the database.  One would, then, expect this file to exist for only\n"
"a few seconds at a time.\n"
"\n"
"V5.4 adds checking to verify that the database and the fred_file are in order.\n"
"Time regressions will cause the database update to be supressed.\n"
"\n"
"This is part of Sort/Merge processing (not reccomeded for other uses).\n"
"\n"
"This update program does NOT delete old database files.  Rather, it renames\n"
"the old file changing the \".db\" to a year/day/hour/minute/second in place\n"
"of the \".db\" forming a running backup (shades of VMS version numbers).  As\n"
"a result, there is little danger of a lost database file, but it becomes\n"
"necessary to remove the old versions occasionally.\n"
"\n"
"OPTIONS:\n"
"-dbase    path to database file (default is: env var $RPWS_MPDB)\n"
"\n"
"-fred_file\n"
"          name of database update file\n"
"\n"
"-starting_point n\n"
"          Skip this many records at the begining\n"
"\n"
"-ending_point n\n"
"          Skip this many records at the end\n"
"\n"
"-flag xx  add \"xx\" to changed records\n"
"\n"
"-test     test run (no update to database) creates a .new file in the local\n"
"          directory\n"
"\n"
		);
}

/*****************************************************************************/
int main (int argc, char *argv[])
{
  FILE *new = NULL;

  /**/ FILE * old = NULL;
  /**/ FILE * fred = NULL;
  struct stat *statbuf;

  int status;
  int test = 0;
  /* int new_pos = 0; */
  /* int last_sclk_b, sclk_b; */
  /* int last_sclk_e, sclk_e; */
  int starting_point = 0;
  int ending_point = 0;
  int fred_count[2] = { 0, 0 };
  int count = 0;
  int delta_t;
  int nOldLine = 0;
  int previous_sclk = 0;

  char buf[1025];
  char fred_name[256] = { "fred" };
  char db_path[256]  = {'\0'};
  char new_path[256] = { '\0' };
  char bak_path[256] = { '\0' };
  char lock_path[256] = {'\0' };

  char lock_text[256];
  
  struct DBASE* database = NULL;  /* Holds single old db record */
  
  int old_sequence_error_flag = 0; /* if problem found in old index file */
	
  /* int new_sequence_error_flag = 0; */  

  
/*  char db_type[32] = { ".db" };
  char db_name[64] = { "CassiniJPL" };
*/

	fg_flags (argc, argv);

	fprintf (stdout, " %s %s(%s)\n", Title, Version, db_Version);
  
	if (fg_flag ("h") || fg_flag ("help")) {
		prnHelp();
		return 0;
	}

	starting_point = fg_int ("starting_point", starting_point);
	ending_point = fg_int ("ending_point", ending_point);

	if(fg_flag("test"))       test = 1;

	if(fg_flag("dbase")) {
		strcpy (db_path, fg_flagc ("dbase"));
		fprintf (stdout, "db_name \"%s\"\n", db_path);
	}
  
	if(fg_flag("flag"))       flag_text = fg_flagc ("flag");
	
	if(fg_flag("fred_file"))  strcpy(fred_name, fg_flagc ("fred_file"));
	
	if(db_path[0] == '\0'){
		if(getenv("RPWS_MPDB") == NULL){
	  		fprintf(stderr, "Mini-packet index not specified, use -dbase or set the "
			        "RPWS_MPDB environment variable.\n");
			return 13;
		}
		strncpy(db_path, getenv("RPWS_MPDB"), 255);
	}
  
	/* Get the new database name */
	if( ! getNewName(db_path, test, new_path, 255) ) return 13;
	if( ! getBakName(db_path, bak_path, 255) ) return 13;
	
	if( snprintf(lock_path, 255, "%s.lock", db_path) > 255){
		fprintf(stderr, "Lock File name buffer too short.\n");
		return 13;
	}

 
	statbuf = malloc (sizeof (struct stat));
	if(!statbuf) {
		fprintf (stderr, "malloc failed %d\n", __LINE__);
		return 23;
	}
	status = stat (fred_name, statbuf);

	fprintf(stdout, "%s %s Configuration:\n", Title, Version);
	fprintf(stdout, "    flag_text   \"%s\"\n", flag_text);
	fprintf(stdout, "     db_path    \"%s\"\n", db_path);
	fprintf(stdout, "    lock_path   \"%s\"\n", lock_path);
	fprintf(stdout, "     new_path   \"%s\"\n", new_path);
	fprintf(stdout, "     bak_path   \"%s\"\n", bak_path);
	fprintf(stdout, "    fred_name   \"%s\"\n", fred_name);
	
	/* Since we always compile with large file support, this is effectively
	   an int64_t */
	fprintf(stdout, "         size    %ld\n", statbuf->st_size);

  if (statbuf->st_size)
    fred = fopen (fred_name, "r");
  else {
    fprintf (stderr, "fred file is empty\n");
    return 14;
  }
  
  /* Count FRED records, REWIND it when done... */
  if(fred){                           
    while (fgets (buf, 255, fred)) {
      fred_count[1]++;
    }
    fprintf (stdout, "    fred_lines      %3d\n", fred_count[1]);
    fprintf (stdout, "      start record  %3d\n", starting_point);
    fprintf (stdout, "      end record    %3d\n",
             fred_count[1] - ending_point - 1);
    rewind (fred);
  }
  
  if(fg_flag ("h") || fg_flag ("help"))
    return 0;

  if(!fred)
    fprintf (stdout, "%d %p=fopen(fred\"%s\",\"%s\");\n", __LINE__, fred,
             fred_name, "r");

  while ((delta_t = db_lock (lock_path, argv[0], lock_text))) {
    fprintf (stdout, "waiting on \"%s\" for Lock File (%d seconds old)\n",
             lock_text, delta_t);
    sleep (1);
  }
  
  /* To allow for testing in an empty area, create a blank database
     if none exists -cwp */
	if( access( db_path, F_OK) != 0 ){
		
		/* TODO: make the directories leading to this database name, if needed.
		 * There's code to handle this in: 
		 *
		 *    util/C/trunk/librpwgse/rpwgse/GseFile.c
		 */
		
		FILE* fTmp = fopen(db_path, "ab");
		if(fTmp == NULL){
			fprintf(stderr, "%s/%d failed to initialize a blank minipacket "
			        "index file at %s", __FILE__, __LINE__, db_path);
		}
		fflush(fTmp);
		fclose(fTmp);
	}
  
	old = fopen(db_path, "r");
	if(!old) 
		fprintf(stdout, "%d %p=fopen(old_\"%s\",\"%s\");\n", __LINE__, old,
		        db_path, "r");
  
	new = fopen(new_path, "w");
	if(!new)
		fprintf(stdout, "%d %p=fopen(new_\"%s\",\"%s\");\n", __LINE__, new,
		        new_path, "w");

  /* If we can't get the old, new and merge files, exit with an error */
  if ((!fred) || (!old) || (!new))
    return 14;
  
  /* Read FRED records, this sets the globals: */
  /* g_pHead -      set to first record in fred file */
  /* g_pFredStart - set to first record in fred file */
  /* g_pFredStop  - set to last record in fred file  */
  
	while (fgets (buf, 255, fred)) {
		int fg = 1;

		if(fred_count[0] < starting_point) fg = 0;
		
		if (fred_count[0] >= (fred_count[1] - ending_point)) fg = 0;
		
		fred_count[0]++;
		
		if(fg){
      	cleanup (buf);
      	save_record (buf);
    	}
	}
	fclose (fred);

	display(g_pFredStart, "start");        /* DISP */
	if(g_pFredStop)
		display(g_pFredStop, "stop");        /* DISP */
	else
		g_pFredStop = g_pFredStart;

	
	/* Read and dump old records unless you're in the time period of the
	   fred records, then dump those instead.  This assumes than fred
		records are continuous */
	
	nOldLine = 0;
	previous_sclk = 0;
	while(fgets (buf, 255, old)) {
	  
		nOldLine++;
		cleanup (buf);
		database = record_setup(buf);
	 
		if ((database->sclk_begin - previous_sclk) <= 0){
			if (database->sclk_begin > CASSINI_STARTING_SCLK) {
				old_sequence_error_flag = 1;
				fprintf (stdout, "sequence error in database file at line %d\n",
				        nOldLine);
			}
		}
    
		switch(compare(g_pFredStart, database, g_pFredStop)) {
		case BEFORE:
			fprintf (new, "%s\n", buf);
			break;
		case DURING:
			dump_record (new);
			break;
		case AFTER:
			dump_record (new);
			fprintf (new, "%s\n", buf);
			break;
		}
		
		cleanup (buf);
		previous_sclk = database->sclk_begin;
  }
  
	/* Handle the case were there were no lines in the old file. -cwp */
	if(nOldLine == 0)
		dump_record(new);
  
	fclose (old);
	fclose (new);
  
	int nErr = 0;
	if(old_sequence_error_flag) {
		nErr += 1;
		fprintf (stdout, "SEQUENCE ERROR in old database UPDATE supressed\n");
	}
  
	if(g_nFredSequenceErrorFlag) {
		nErr += 1;
		fprintf (stdout, "SEQUENCE ERROR in fred UPDATE supressed\n");
	}

	if((count = compare_files (new_path, db_path))){

		fprintf(stdout, "New database differs from old starting at line %d\n", 
		       count);

		if((!test)&&(!nErr)){
			
			if(rename(db_path, bak_path) != 0){
				fprintf(stdout, "ERROR in rename %s -to-> %s\n", db_path, bak_path);
				nErr += 1;
			}
			else{			
				fprintf(stdout, "Renamed %s -to-> %s\n", db_path, bak_path);
			}
			
			if(rename(new_path, db_path) != 0){
				fprintf(stdout, "ERROR in rename %s -to-> %s\n", new_path, db_path);
				nErr += 1;
			}
			else{
				fprintf(stdout, "Renamed %s -to-> %s\n", new_path, db_path);
			}
		}
		
	}
	else{
		fprintf (stdout, "NO change, old %s retained\n", db_path);
	}

  if(db_unlock(lock_path)){
		fprintf(stdout, "Error: Problem with lock file %s\n", lock_path);
		nErr += 1;
	}

  return nErr;
}
