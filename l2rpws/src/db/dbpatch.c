
/*
 * dbpatch.c	For real-time stuff
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* Willy's command line parsing library */
#include <fg.h>

#include "dblock.h"

static char *title = { " CASSINI database update (dbpatch)" };
static char *Version = { "V3.2" };

int compare_files (char *filename_1, char *filename_2)
{
  FILE *file_1;
  FILE *file_2;
  char buffer_1[1024];
  char buffer_2[1024];
  int status = 0;
  int eof = 0;

  file_1 = fopen (filename_1, "r");
  if (!file_1)
    status += 1;
  file_2 = fopen (filename_2, "r");
  if (!file_2)
    status -= 1;

  if (status)
    eof = 1;

  if (eof)
    return status;

  fgets (buffer_1, 1023, file_1);
  fgets (buffer_2, 1023, file_2);
  while (!eof) {
    status = strcmp (buffer_1, buffer_2);
    if (status) {
      break;
    }
    if (!fgets (buffer_1, 1023, file_1)) {
      eof = 1;
      status += 1;
    }
    if (!fgets (buffer_2, 1023, file_2)) {
      eof = 1;
      status -= 1;
    }
  }
  return status;
}

int old_compare_files (char *filename_1, char *filename_2)
{
  FILE *file_1;
  FILE *file_2;
  char buffer_1[1024];
  char buffer_2[1024];
  int status = 0;
  int eof = 0;

  file_1 = fopen (filename_1, "r");
  if (!file_1)
    status += 1;
  file_2 = fopen (filename_2, "r");
  if (!file_2)
    status -= 1;

  if (status)
    eof = 1;

  if ((!file_1) && (!file_2))
    eof = 1;

  if (eof)
    return status;

  fgets (buffer_1, 1023, file_1);
  fprintf (stdout, "buffer_1 %s", buffer_1);
  fgets (buffer_2, 1023, file_2);
  fprintf (stdout, "buffer_2 %s", buffer_2);
  while (!eof) {
    status = strcmp (buffer_1, buffer_2);
    if (status)
      break;
    if (!fgets (buffer_1, 1023, file_1)) {
      fprintf (stdout, "buffer_1 %s", buffer_1);
      eof = 1;
      status += 1;
    }
    if (!fgets (buffer_2, 1023, file_2)) {
      fprintf (stdout, "buffer_2 %s", buffer_2);
      eof = 1;
      status -= 1;
    }
  }
  return status;
}

/*****************************************************************************/
void prnHelp()
{
	printf(
"\n"
"    Database update program.  This take a list of files that (we assume)\n"
"    have been delivered from JPL in real-time and patches the database \n"
"    (list of file start/stop times) with the new files (i.e. keeps files\n"
"    from post-pass queries that have the cover the same time period).\n"
"\n"
"    We now attempt to gain exclusive access to the database through a lock\n"
"    file ($RPWS_MPDB.lock).  This file is created and will contain PID,\n"
"    name, and time, of the process accessing the database.  This file\n"
"    exists only when the database is being updated.  If the file already\n"
"    exists when this program begins to access the database, it will poll\n"
"    the file, at 1 second intervals, until the lock file has been removed,\n"
"    prior to creating a new file and accessing the database.  One would,\n"
"    then, expect this file to exist for only a few seconds at a time.\n"
"\n"
"    V1.0 compares the new database with the existing database and avoids\n"
"    changing the database when not necessary (as this interferes with come\n"
"    post-pass updates and with long-term access).  The database file now\n"
"    changes only when necessary.\n"
"\n"
"OPTIONS:\n"
"       -dbase     name of database file (default: $RPWS_MPDB)\n"
"\n"
"       -fred      name of a database update file (default: $RPWS_DATA/bg_push/fred)\n"
"\n"
"       -sub       The \"database\" doesn't have a field mark records as \n"
"                  originating form a real-time stream.  The only identifer is the\n"
"                  path name.  Use to set the substring that indicates the path to\n"
"                  real-time records. (default: $RPWS_DATA/bg_push)\n"
"\n"
"       -nosmart   supress elimination of duplicate PUSH data\n"
"\n"
"       -time_patch\n"
"                  patch last line of file to end of day\n"
"\n"
"       -verbose n detail lines (status report)\n"
"                      1 add lines\n"
"  						  2 delete lines\n"
"  						  4 keep lines (many lines\n"
"\n"
"       -test      test run (no update to database)\n"
"\n"			
	);
}

/*****************************************************************************/
int main (int argc, char *argv[])
{
	FILE* pNew = NULL;
	FILE* pOld = NULL;
	FILE* pFred = NULL;

	int verbose = 0;
	int time_patch = 0;
	int smart_flag = 1;
	int test = 0;
	int new_pos = 0;
	int last_sclk_b, sclk_b;
	int last_sclk_e, sclk_e;
	int delta_t;

	
	char sDbPath[256]    = {'\0'};
	char sFredPath[256]  = { "fred" };
	
	char sLine[1025]     = {'\0'};
	char sOldPath[256]   = {'\0'};
	char sNewPath[288]   = {'\0'};
	char sBakPath[288]   = {'\0'};
	
	char sLockPath[256]  = {'\0'};
	char sLockText[256]  = {'\0'};
	char sRtPath[256]    = {'\0'};
  	
	/* Compute some file names */
	
	fg_flags (argc, argv);

	fprintf (stdout, " %s %s(%s)\n", title, Version, db_Version);
	if(fg_flag ("h") || fg_flag ("help")) {
		prnHelp();
		return 0;
	}

  	if(fg_flag ("time_patch")) time_patch = 1;

	if(fg_flag ("test")) test = 1;

	if(fg_flag ("nosmart")) smart_flag = 0;

	if(fg_flag ("verbose")) 
		verbose = fg_int("verbose", verbose);

	
	if(fg_flag ("dbase")) {
		strncpy(sDbPath, fg_flagc("dbase"), 255);
	}
	else{
		if(getenv("RPWS_MPDB") == NULL){
			fprintf(stdout, "ERROR: Use -dbase or set the RPWS_MPDB environment "
			        "variable.\n");
			return 13;  
		}
		else{
			strncpy(sDbPath, getenv("RPWS_MPDB"), 255);
		}
	}
	
	if( snprintf(sLockPath, 255, "%s.lock", sDbPath) > 255){
		fprintf(stderr, "Lock File name buffer too short.\n");
		return 13;
	}
	
	if(fg_flag ("fred")) {
		strncpy(sFredPath, fg_flagc("fred"), 255);
	}
	else{
		if(getenv("RPWS_DATA") == NULL){
			fprintf(stdout, "ERROR: Use -fred or set the RPWS_DATA environment "
					  "variable.\n");	
			return 13;
		}
		else{
			snprintf(sFredPath, 255, "%s/bg_push/fred", getenv("RPWS_DATA"));
		}
	}
	
	if(fg_flag("sub")){
		strncpy(sRtPath, fg_flagc("sub"), 255);
	}
	else{
		if(getenv("RPWS_DATA") == NULL){
			fprintf(stdout, "ERROR: Use -sub or set the RPWS_DATA environment "
					  "variable.\n");	
			return 13;
		}
		else{
			snprintf(sRtPath, 255, "%s/bg_push", getenv("RPWS_DATA"));
		}
	}

	strncpy(sOldPath, sDbPath, 255);

	strncpy(sNewPath, sDbPath, 287);
	strncat(sNewPath, ".new", 287);

	strncpy(sBakPath, sDbPath, 287);
	strncat(sBakPath, ".push_bak", 287);

	printf("INFO: Configuration:\n");
	printf("      old_path    %s\n", sOldPath);
	printf("      lock_path   %s\n", sLockPath);
	printf("      new_path    %s\n", sNewPath);
	printf("      bak_path    %s\n", sBakPath);
	printf("      fred_path   %s\n", sFredPath);
	
	if(!test) {
		while( (delta_t = db_lock(sLockPath, argv[0], sLockText)) ){
			fprintf(stderr, "INFO: waiting on \"%s\" for Lock File (%d seconds old)\n",
			        sLockText, delta_t);
			sleep(2);
		}
	}
	
	/* To allow for testing in an empty area, create a blank database
      if none exists -cwp */
	if( access( sDbPath, F_OK) != 0 ){
		
		/* TODO: make the directories leading to this database name, if needed.
		 * There's code to handle this in: 
		 *
		 *    util/C/trunk/librpwgse/rpwgse/GseFile.c
		 */
		
		FILE* fTmp = fopen(sDbPath, "ab");
		if(fTmp == NULL){
			fprintf(stderr, "ERROR: %s/%d failed to initialize a blank minipacket "
			        "index file at %s", __FILE__, __LINE__, sDbPath);
		}
		fflush(fTmp);
		fclose(fTmp);
	}

	pNew = fopen(sNewPath, "w");
	if(!pNew) {
		fprintf(stderr, "ERROR: Couldn't write to '%s'\n", sNewPath);
		return 13;
	}

	pOld = fopen(sOldPath, "r");
	if (!pOld) {
		fprintf(stdout, "ERROR: Couldn't read from '%s'\n", sOldPath);
		return 13;
	}
  
	pFred = fopen(sFredPath, "r");
	if (!pFred) {
		fprintf(stdout, "ERROR: Couldn't read from '%s'\n", sFredPath);
		return 13; 
	}

	/* Copy NON-real-time data records... */
	fprintf(stderr, "INFO: Real-time path: %s\n", sRtPath);
	
	while(fgets(sLine, 255, pOld)){
		if(strstr(sLine, sRtPath)){     /* look for RT directory path */
			if (verbose & 2)
				fprintf(stdout, "INFO: delete %s", sLine);
		}
		else{
			last_sclk_b = strtol (&sLine[45], NULL, 16);
			last_sclk_e = strtol (&sLine[54], NULL, 16);
			if(verbose & 4)
				fprintf(stdout, "INFO: keep (SCLK: %8X-%8X) %s", last_sclk_b, last_sclk_e, sLine);
			fprintf(pNew, "%s", sLine);
		}
	}
	fclose(pOld);

	
	/* Insert REAL-TIME detail lines */
	
	while(fgets(sLine, 255, pFred)){
		sclk_b = strtol(&sLine[45], NULL, 16);
		sclk_e = strtol(&sLine[54], NULL, 16);
		if ((sclk_e > last_sclk_e) || !smart_flag){
			if (verbose & 1)
				fprintf(stdout, "INFO: add    %s", sLine);
			new_pos = ftell(pNew);
			fprintf(pNew, "%s", sLine);
		}
		else {
			if (verbose & 1)
				fprintf(stdout, "INFO: skip   %s", sLine);
		}
	}
	
	/* IF new_pos is zero, we didn't add any real-time records SO don't
	   patch stop time... */
	if(time_patch && new_pos){
		fseek(pNew, new_pos, SEEK_SET);
		sLine[32 - 1] = '2';
		sLine[32] = '3';
		sLine[35 - 1] = '5';
		sLine[35] = '9';
		sLine[38 - 1] = '5';
		sLine[38 - 0] = '9';
		sLine[41 - 1] = '9';
		sLine[41] = '9';
		sLine[41 + 1] = '9';
		sLine[55] = 'F';
		sLine[55 + 1] = 'F';
		sLine[55 + 2] = 'F';
		sLine[55 + 3] = 'F';
		sLine[55 + 4] = 'F';
		sLine[55 + 5] = 'F';
		sLine[55 + 6] = 'F';
		fprintf(pNew, "%s", sLine);
	}

	fclose(pFred);
	fclose(pNew);

	if(compare_files(sNewPath, sOldPath)) {
		if(!test){
			rename(sOldPath, sBakPath);
			rename(sNewPath, sOldPath);
			fprintf(stdout, "INFO: renaming \"%s\" -to-> \"%s\"\n", sOldPath, sBakPath);
      	fprintf(stdout, "INFO: renaming \"%s\" -to-> \"%s\"\n", sNewPath, sOldPath);
		}
		else{
      	fprintf(stdout, "INFO: Test data located in \"%s\"\n", sNewPath);
		}
	}
	else{
		fprintf(stdout, "INFO: NO change between %s and %s\n", sNewPath, sOldPath);
	}

	if(!test){
		if(db_unlock (sLockPath)){
			fprintf(stdout, "ERROR: Problem with lock file %s\n", sLockPath);
			return 17;
		}
	}

  return 0;
}
