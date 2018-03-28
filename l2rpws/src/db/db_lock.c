
/*
 * db_lock.c	CassiniJPL database locking
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include <fg.h>

#include "dblock.h"

/* static char *title = { " CASSINI database lock" }; */
/* static char *Version = { "V1.2" }; */

/* Use $RPWS_DATA/database/CassiniJPL.lock as the default lock file name */
/* static char Lock_file[] = { DB_LOCK_FILENAME }; */

/* ************************************************************************* */
void prnHelp()
{
	fprintf(stdout, 
"rpws_db_lock - RPWS master table co-operative lock program\n"
"\n"
"USAGE\n"
"   rpws_db_lock [ -lock | -rmlock ]\n\n");

	fprintf(stdout, 
"DESCRIPTION\n"
"   In order to allow more and more worker programs to update the database, a\n"
"   locking mechanism has been implemented.  The lock consists of a file that\n"
"   contains the:\n"
"\n"
"      (1) PID \n"
"      (2) hex-time \n"
"      (3) program name\n"
"      (4) text-time \n"
"\n"
"   that the lock was created, and (5)user/host of the process that acquired the\n"
"   lock.\n"
"\n"
"   Worker programs should acquire the lock using the db_lock() routine prior to\n"
"   accessing the database and release it, using db_unlock(), following use of\n"
"   the databse.  The datbase must be locked prior to any use of the database\n"
"   that will write to the database (in other words, you must lock the database\n"
"   prior to reading it when you ultimately intend to write to it.  Failure to\n"
"   lock prior to reading allow another process to potentially modify the\n"
"   database between your read and write.\n"
"\n"
"   Stale locks will prevent any update of the database using rpws_db_patch or\n"
"   rpws_db_update. they may be removed using db_rmlock() via this program\n"
"\n\n");

	fprintf (stdout, 
"OPTIONS\n"
"   By default rpws_db_lock just prints the expected location of the master\n"
"   lock file.  The options below can be use to take other actions.\n"
"\n"
"   -h,-help    Print this help text\n"
"\n"
"   -lock       Lock the database (useful for hand editing).\n"
"\n"
"   -rmlock     Remove lock from database (useful for stale locks or after\n"
"               editing).\n"			
"\n"
"ENVIRONMENT\n"
"   The location of the master database lock file is:\n"
"\n"
"      $RPWS_MPDB.lock\n"
"\n"
"   Where $RPWS_MPDB is the environment variable defining the location of the\n"
"   RPWS master file table.\n"
"\n"
"BUGS\n"
"   The obvious problem with using this program is that a script can die\n"
"   leaving a lock file that will never be removed.  To avoid this problem\n"
"   and many race conditions, modern operating systems have provided much \n"
"   more reliable file locking facility.  See the lockf() system call\n"
"   documentation for more information.\n\n");

}

/* ************************************************************************* */
int main (int argc, char *argv[])
{
  int count = 0;
  int status = 0;
  /* int pid = -1; */
  /* char *temp = NULL; */
  char lock_text[256] = {'\0'};
  char lock_file[512] = {'\0'};

  fg_flags (argc, argv);
  if (fg_flag ("h") || fg_flag ("help")) {
    prnHelp();
    return 0;
  }
  
  if(getenv("RPWS_MPDB") == NULL){
	 fprintf(stderr, "ERROR: RPWS_MPDB environment variable is not set, cannot "
            "determine the location of the master Mini-packet file table\n");
	  return 3;
  }
	
   snprintf(lock_file, 511, "%s.lock", getenv("RPWS_MPDB"));
	 
	/* this is all subrountine documentation and should be in header files, not
	text output */
  /*   
    fprintf (stdout, " %s %s(%s)\n", title, Version, db_Version);
    fprintf (stdout, "       \n");
    fprintf (stdout, "    Database LOCK FILE control program\n");
    fprintf (stdout, "       \n");
    
    fprintf (stdout, "       \n");
    fprintf (stdout, "   synopsis of dblock calls:\n");
    fprintf (stdout, "       \n");
    fprintf (stdout, "     header file definitions:\n");
    fprintf (stdout, "       \n");
    fprintf (stdout, "           These defineitions provide access to the\n");
    fprintf (stdout,
             "         lockfile and to the version of the lock library\n");
    fprintf (stdout, "       \n");
    fprintf (stdout, "         #define DB_LOCK_FILENAME \"%s\"\n",
             DB_LOCK_FILENAME);
    fprintf (stdout, "          extern char *db_Version; \n");
    fprintf (stdout, "       \n");
    fprintf (stdout, "       DB_LOCK_FILENAME is the of the lock file.\n");
    fprintf (stdout, "       db_Version is the text string, stored in\n");
    fprintf (stdout, "         the library object module, that indicates\n");
    fprintf (stdout, "         the library software revision.\n");
    fprintf (stdout, "       \n");
    fprintf (stdout, "     library calls:\n");
    fprintf (stdout, "       \n");
    fprintf (stdout,
             "        int db_lock(char *lock_file_name, char *argv0, char *text);\n");
    fprintf (stdout, "            \n");
    fprintf (stdout,
             "              Attempt to acquire the lock on the database.  If successful\n");
    fprintf (stdout,
             "            you may read/write the database.  If unsuccessful, delay and\n");
    fprintf (stdout, "            try again later.\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "          Arguments:\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "            lock_file_name (const char *)\n");
    fprintf (stdout,
             "                Filename of the lock file, usually the DB_LOCK_FILENAME\n");
    fprintf (stdout,
             "              containing the filename string is used to access the\n");
    fprintf (stdout, "              Cassini database.\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "            argv0 (const char *)\n");
    fprintf (stdout,
             "                Name of the calling process, usually obtained from\n");
    fprintf (stdout, "              argv[0] (from main()).\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "            text (char [256])\n");
    fprintf (stdout,
             "                Contents of the lock file.  If the return value is zero\n");
    fprintf (stdout,
             "              or positive, the contents of the lock file (always 1 line)\n");
    fprintf (stdout,
             "              are returned in this string.  If the lock is already taken, \n");
    fprintf (stdout,
             "              string gives a clue as to who has the lock.\n");
    fprintf (stdout, "              \n");
    fprintf (stdout, "          Return Values:\n");
    fprintf (stdout, "            \n");
    fprintf (stdout,
             "            ZERO indicates success.  You have acquired exclusive write\n");
    fprintf (stdout, "              access to the database.\n");
    fprintf (stdout, "            \n");
    fprintf (stdout,
             "            Positive number:  Another process has the lock, you are NOT\n");
    fprintf (stdout,
             "              permitted to write to the database.  The value returned is\n");
    fprintf (stdout,
             "              an indication of the age of the lock (in seconds).\n");
    fprintf (stdout, "            \n");
    fprintf (stdout,
             "            Negative Number: Another process has the lock, you are NOT\n");
    fprintf (stdout,
             "              permitted to write to the database.  Some problem occurred\n");
    fprintf (stdout,
             "              that prevented acquisition of the lock.\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "        int db_unlock(char *lock_file_name);\n");
    fprintf (stdout, "            \n");
    fprintf (stdout,
             "              Attempt to release the lock on the database.\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "          Arguments:\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "            lock_file_name (const char *)\n");
    fprintf (stdout,
             "                Filename of the lock file, usually the DB_LOCK_FILENAME\n");
    fprintf (stdout,
             "              containing the filename string is used to access the\n");
    fprintf (stdout, "              Cassini database.\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "          Return Values:\n");
    fprintf (stdout, "            \n");
    fprintf (stdout,
             "            ZERO indicates success.  You have released the lock\n");
    fprintf (stdout, "            \n");
    fprintf (stdout,
             "            Positive number:  Your PID does not match that listed\n");
    fprintf (stdout,
             "              in the lock file.  The PID of the owning process is\n");
    fprintf (stdout,
             "              returned.  The lock file has NOT been removed.\n");
    fprintf (stdout,
             "                This should not occur.  The lock should be acquired\n");
    fprintf (stdout,
             "              and released by the same process, so the PID should not\n");
    fprintf (stdout,
             "              change.  If this occurs, investigate the problem.\n");
    fprintf (stdout, "              \n");
    fprintf (stdout,
             "        int db_findlock(char *lock_file_name, char *text);\n");
    fprintf (stdout, "            \n");
    fprintf (stdout,
             "              Attempt to open the lock file and collect it's contents.\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "          Arguments:\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "            lock_file_name (const char *)\n");
    fprintf (stdout,
             "                Filename of the lock file, usually the DB_LOCK_FILENAME\n");
    fprintf (stdout,
             "              containing the filename string is used to access the\n");
    fprintf (stdout, "              Cassini database.\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "            text (char [256])\n");
    fprintf (stdout,
             "                Contents of the lock file.  If the return value is zero\n");
    fprintf (stdout,
             "              the contents of the lock file (always 1 line)\n");
    fprintf (stdout, "              are returned in this string.\n");
    fprintf (stdout, "              \n");
    fprintf (stdout, "          Return Values:\n");
    fprintf (stdout, "            \n");
    fprintf (stdout,
             "            ZERO indicates success.  text variable contains the line\n");
    fprintf (stdout, "              fromt he lock file.\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "            -1: No lock file was found\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "        LOCK FILE MAINTENANCE ONLY\n");
    fprintf (stdout, "        DO NOT USE THIS CALL !!!\n");
    fprintf (stdout,
             "        int db_rmlock(char *lock_file_name, char *text);\n");
    fprintf (stdout, "            \n");
    fprintf (stdout,
             "              Attempt to unconditionally release the lock \n");
    fprintf (stdout,
             "            on the database.  If a program has crashed and left\n");
    fprintf (stdout,
             "            a lock open, the call may be used to clear the lock.\n");
    fprintf (stdout,
             "            This is also used to release a lock that has been\n");
    fprintf (stdout,
             "            manually acquired for the purpose of hand editing the\n");
    fprintf (stdout, "            database file.\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "          Arguments:\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "            lock_file_name (const char *)\n");
    fprintf (stdout,
             "                Filename of the lock file, usually the DB_LOCK_FILENAME\n");
    fprintf (stdout,
             "              containing the filename string is used to access the\n");
    fprintf (stdout, "              Cassini database.\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "            text (char [256])\n");
    fprintf (stdout,
             "                Contents of the lock file.  If the return value is zero\n");
    fprintf (stdout,
             "              the contents of the lock file (always 1 line)\n");
    fprintf (stdout, "              are returned in this string.\n");
    fprintf (stdout, "              \n");
    fprintf (stdout, "          Return Values:\n");
    fprintf (stdout, "            \n");
    fprintf (stdout,
             "            ZERO indicates success.  You have removed a lock file\n");
    fprintf (stdout, "            \n");
    fprintf (stdout, "            -1: No lock file was found\n");
    fprintf (stdout, "            \n");
    return 0;
  }
  */
		  
		  

  if (fg_flag ("lock")) {
    while( (count = db_lock (lock_file, argv[0], lock_text)) ){
      fprintf (stdout,
               "waiting on \"%s\" for Lock File (that is %d seconds old)\r",
               lock_text, count);
      fflush (stdout);
      sleep (1);
      if (count > 30)
        break;
    }
    fprintf (stdout, "\n");
    if (count) {
      fprintf (stdout, "Stale Lock ???\n");
      status = 1;
    } else {
      fprintf (stdout, "Establish lock \"%s\"\n", lock_text);
      status = 0;
    }
  }

  else if (fg_flag ("rmlock")) {
    if (!db_rmlock (lock_file, lock_text)) {
      fprintf (stdout, "   Remove lock \"%s\"\n", lock_text);
      status = 0;
    } else {
      fprintf (stdout, "NO lock found\n");
      status = 2;
    }
  } else {
    db_findlock (lock_file, lock_text);
    if (lock_text[0]) {
      fprintf (stdout, "Find lock      \"%s\"\n", lock_text);
      status = 0;
    } else {
		fprintf (stdout, "Lock file \"%s\" is not present\n", lock_file);
      status = 3;
    }
  }

  return status;
}
