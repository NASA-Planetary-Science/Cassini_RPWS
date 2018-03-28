
/*
 * dblock.c	Exclusive write access to database
 */

#define _POSIX_C_SOURCE 200112L


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

char *db_Version = { "V1.1" };


/* For some reason _POSIX_C_SOURCE is not defining this on linux 
   so define it here */
char *cuserid(char *string);

 /***************************************************************
  *	Create a lock file to control exclusive			*
  *	access to some resource.  				*
  *								*
  *	take a lock out by calling db_lock:			*
  *	lock_file_name:	name of lock file (i.e. control file)	*
  *	argv0:		process name (will be placed in control	*
  *				file).				*
  *	text:		char[256] string that will be used to	*
  *				build the lock file contents	*
  *				(returns current lock holder	*
  *				when lock is NOT acquired).	*
  *	Returns:						*
  *		>0 is the age of the current lock, therefore	*
  *				the lock can not be acquired	*
  *		ZERO when lock has been acquired		*
  *				text contains lock control	*
  *				file contents			*
  *								*
  ***************************************************************/

int db_lock (char *lock_file_name, char *argv0, char *text)
{
  FILE *lock_file;
  time_t time_t_uttime;
  time_t time_old_uttime;
  struct tm *tm_uttime;
  char *ctemp;
  char hostname[128];
  pid_t pid;
  int delta_t;

  pid = getpid ();
  time_t_uttime = time (NULL);
  tm_uttime = gmtime (&time_t_uttime);
  gethostname (hostname, 127);

  sprintf (text, "%d 0x%08lX %s %04d-%03dT%02d:%02d:%02d %s@%s\n", pid,
           time_t_uttime,
           argv0,
           tm_uttime->tm_year + 1900,
           tm_uttime->tm_yday,
           tm_uttime->tm_hour,
           tm_uttime->tm_min, tm_uttime->tm_sec, cuserid (NULL), hostname);

  lock_file = fopen (lock_file_name, "r");
  if (lock_file) {
    fgets (text, 255, lock_file);
    text[strlen (text) - 1] = 0;        /* strip newline */
    fclose (lock_file);                 /* release file */
    strtol (text, &ctemp, 0);           /* conver & discard PID */
    time_old_uttime = strtol (ctemp + 1, NULL, 0);
    delta_t = time_t_uttime - time_old_uttime;
    if (delta_t < 1)
      delta_t = 1;
    return delta_t;
  }
  lock_file = fopen (lock_file_name, "w");
  fputs (text, lock_file);
  fclose (lock_file);
  text[strlen (text) - 1] = 0;
  return 0;
}

 /***************************************************************
  *	Release (delete) a lock file to release exclusive	*
  *	access to some resource.  				*
  *								*
  *	release a lock out by calling db_unlock:		*
  *	lock_file_name:	name of the lock (control) file 	*
  *				(same name as call to db_lock)	*
  *	returns:						*
  *		PID from lock file if it doesn't match the PID	*
  *				of the calling process (i.e.	*
  *				error condition, should NOT	*
  *				occur during normal operations.	*
  *		ZERO if the release was successful		*
  *				File was deleted.		*
  *		-1   lock file not found			*
  *								*
  ***************************************************************/
int db_unlock (char *lock_file_name)
{
  FILE *lock_file;
  pid_t pid;
  int old_pid;
  char text[256];

  lock_file = fopen (lock_file_name, "r");
  if (lock_file) {
    fgets (text, 255, lock_file);
    text[strlen (text) - 1] = 0;
    fclose (lock_file);
  }
  pid = getpid ();
  old_pid = atoi (text);
  if (old_pid != pid) {
    if (old_pid)
      return old_pid;
    else
      return 1;
  }
  unlink (lock_file_name);
  return 0;
}

 /***************************************************************
  *	Remove (delete) a lock file to release exclusive	*
  *	access to some resource.  				*
  *								*
  *	release a lock out by calling db_rmlock:		*
  *	lock_file_name:	name of the lock (control) file 	*
  *				(same name as call to db_lock)	*
  *	returns:						*
  *		ZERO if the release was successful		*
  *				File was deleted.		*
  *		-1   lock file not found			*
  *								*
  ***************************************************************/
int db_rmlock (char *lock_file_name, char *text)
{
  FILE *lock_file;

  lock_file = fopen (lock_file_name, "r");
  text[0] = 0;
  if (lock_file) {
    fgets (text, 255, lock_file);
    text[strlen (text) - 1] = 0;
    fclose (lock_file);
    unlink (lock_file_name);
    return 0;
  }
  return -1;
}

 /***************************************************************
  *	Find a lock file 					*
  *								*
  *	find a lock out by calling db_findlock:			*
  *	lock_file_name:	name of the lock (control) file 	*
  *				(same name as call to db_lock)	*
  *	text:		char[256] string that will be used to	*
  *				build the lock file contents	*
  *				(returns current lock holder	*
  *				when lock is NOT acquired).	*
  *	returns:						*
  *		ZERO if a lock file was found			*
  *				text contains the  contents	*
  *				of the lock file.		*
  *		-1   lock file not found			*
  *								*
  ***************************************************************/
int db_findlock (char *lock_file_name, char *text)
{
  FILE *lock_file;

  lock_file = fopen (lock_file_name, "r");
  text[0] = 0;
  if (lock_file) {
    fgets (text, 255, lock_file);
    text[strlen (text) - 1] = 0;
    fclose (lock_file);
    return 0;
  }
  return -1;
}
