/* #define DB_LOCK_FILENAME "/opt/project/cassini/data/database/CassiniJPL.lock" */

extern char *db_Version;

 /***********************************************************************
  *	Create a lock file to control exclusive				*
  *	access to some resource.  					*
  *									*
  *	db_lock(lock_file_name, argv0, text);				*
  *		lock_file_name:	name of lock file (i.e. control file)	*
  *		argv0:		process name (will be placed in control	*
  *					file).				*
  *		text:		char[256] string that will be used to	*
  *					build the lock file contents	*
  *					(returns current lock holder	*
  *					when lock is NOT acquired).	*
  *	Returns:							*
  *		ZERO when lock has been acquired			*
  *					text contains lock control	*
  *					file contents			*
  *		>0 is the age of the current lock, therefore		*
  *					the lock can not be acquired	*
  *									*
  ***********************************************************************/

int db_lock (char *lock_file_name, char *argv0, char *text);

 /***************************************************************
  *	Release (delete) a lock file to release exclusive	*
  *	access to some resource.  The PID in the lock file	*
  *	must match our PID, otherwise don't try to delete	*
  *	the lock file, it isn't ours (something is screwed up)	*
  *								*
  *	db_unlock(lock_file_name);				*
  *	lock_file_name:	name of the lock (control) file 	*
  *				(same name as call to db_lock)	*
  *	returns:						*
  *		ZERO if the release was successful		*
  *				Lock file was deleted.		*
  *		PID from lock file if it doesn't match the PID	*
  *				of the calling process (i.e.	*
  *				error condition, should NOT	*
  *				occur during normal operations.	*
  *		-1   lock file not found			*
  *								*
  ***************************************************************/
int db_unlock (char *lock_file_name);

 /***************************************************************
  *	Remove (delete) a lock file to release exclusive	*
  *	access to some resource.  				*
  *	MAINTENANCE FUNCTION:  This will kill any existing	*
  *	lock that is outstanding, even if there is a PID	*
  *	mis-match.  From within a program you should acquire	*
  *	and release the lock from the same process, so PID	*
  *	in the lock file should match your own PID.		*
  *								*
  *	db_rmlock(lock_file_name);				*
  *	lock_file_name:	name of the lock (control) file 	*
  *				(same name as call to db_lock)	*
  *	returns:						*
  *		ZERO if the release was successful		*
  *				File was deleted.		*
  *		-1   lock file not found			*
  *								*
  ***************************************************************/
int db_rmlock (char *lock_file_name, char *text);

 /***************************************************************
  *	Find a lock file 					*
  *								*
  *	db_findlock(lock_file_name, text);			*
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
int db_findlock (char *lock_file_name, char *text);
