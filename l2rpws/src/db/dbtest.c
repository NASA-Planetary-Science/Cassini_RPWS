
/*
 * dbtest
 */
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include "dblock.h"

static char Lock_file[] = { DB_LOCK_FILENAME };

int main (int argc, char *argv[])
{
  int pid;
  char lock_text[256];

  double a = .828;
  double b;
  double c = 1.828;

  b = sqrt (c * c - a * a);

  fprintf (stdout, "%f\n", b);
  
  while (pid = db_lock (Lock_file, argv[0], lock_text)) {
    fprintf (stdout, "waiting on \"%s\" for Lock File\n", lock_text);
    sleep (1);
  }
  sleep (15);
  if (db_unlock (Lock_file))
    fprintf (stdout, "     Problem with lock file %s\n", Lock_file);
  return 0;
}
