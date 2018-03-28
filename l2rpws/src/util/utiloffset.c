#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


/****************************************************************************/
/* Compiled in config file directory locations */

#ifndef CFG
#error Compiled in default configuration directory is missing.
#endif


static char ident[] = "IDENT utiloffset.c 21-March-2012" ;

static char rcpath[] =  CFG;

static char findname[256] = { "" };
static char *delim = { "= " };

  /*
   * initialize the file pointer
   * * Estagblish path to and open offset file.  This is
   * * an early attempt to correlate SCLK with SCET and UNIX time
   * * thtough the use of a file containing information about the
   * * difference between SCLK and UNIX time.
   */
void utilo_init (char *path)
{                                       /* path to offset file (may be NULL) */
  if (findname[0])
    return;
  if (path)
    strcpy (findname, path);
  else
    strcpy (findname, rcpath);
  strcat (findname, "/offset");
  return;
}

 /*
  * Obtain an item from the offset control file
  * * The Offset file contains several keys that may be obtained
  * * by supplying the name of the key to recover its value in the
  * * form of a character string
  * * returns the value of the key
  */
char *utilo_char (char *key)
{                                       /* mnemonic in control file */
  FILE *fh;
  static char value[64] = { "" };
  static char result[128];
  char string[128];
  char *tok[4];
  int i;

  utilo_init (NULL);
  fh = fopen (findname, "r");
  if (!fh)
    return NULL;
  result[0] = 0;
  while (fgets (string, 127, fh)) {
    i = strlen (string);
    if (string[i - 1] == '\n')
      string[i - 1] = 0;
    if (!result[0]) {
      if (strchr (string, '=')) {
        tok[0] = strtok (string, delim);
        if (!strcasecmp (tok[0], key)) {
          strcpy (result, strtok (NULL, delim));
          break;
        }
      }
    }
  }
  fclose (fh);
  return result;
}

 /*
  * Obtain an item from the offset control file
  * * The Offset file contains several keys that may be obtained
  * * by supplying the name of the key to recover its value in the
  * * form of a double precision floating point item
  * * returns the value of the key
  */
double utilo_double (char *key)
{                                       /* mnemonic in control file */
  char *temp;

  temp = utilo_char (key);
  if (temp)
    return strtod (temp, NULL);
  else
    return 0.0;
}

 /*
  * Obtain an item from the offset control file
  * * The Offset file contains several keys that may be obtained
  * * by supplying the name of the key to recover its value in the
  * * form of a integer
  * * returns the value of the key
  */
long utilo_int (char *key)
{                                       /* mnemonic in control file */
  char *temp;

  temp = utilo_char (key);
  if (temp)
    return strtol (temp, NULL, 0);
  else
    return 0;
}

/*Time field dump
 *	Time field text form:
 *		YYYY/MM/DD-HH:MM:SS	(6 tokens)
 *		YYYY-DOY-HH:MM:SS	(5 tokens)
 */
void utilo_dump_tm (struct tm *t_m, time_t temp, int offset)
{
  printf
    ("isdst(%d)  yday(%d)  wday(%d)  %d-%d-%d  %2.2d:%2.2d:%2.2d time(%lX) offset(%d)\n",
     t_m->tm_isdst, t_m->tm_yday + 1, t_m->tm_wday + 1, t_m->tm_year,
     t_m->tm_mon + 1, t_m->tm_mday, t_m->tm_hour + 1, t_m->tm_min + 1,
     t_m->tm_sec + 1, temp, offset);
  return;
}

 /*
  * Convert text string to time structure
  * *
  */
struct tm *utilo_time (char *text,      /* text string */
                       int *offset)
{                                       /* offset from UNIX time */
  static struct tm t_m;
  static char *delim[] = { "-/ ",
    "-/ ",
    "-/: ",
    ": ",
    ": ",
    "/ ",
    " ",
    " "
  };
  char stg[128];
  char *tok[8];
  char *ctmp;
  int i;
  int token_count = 0;
  int zone = 0;
  int fudge = 0;

  /*
   * bzero(&t_m, sizeof(struct tm)); /*
   */
  memset (&t_m, 0, sizeof (struct tm));
  strcpy (stg, text);

  ctmp = strchr (stg, '<');
  if (ctmp) {                           /* remove the trailing offset *//*  */
    ctmp[0] = 0;                        /*  */
    fudge = strtol (&ctmp[1], (char **) NULL, 0);
  } else
    fudge = 0;

  ctmp = strchr (stg, '(');
  if (ctmp) {                           /* remove the trailing offset *//*  */
    ctmp[0] = 0;                        /*  */
    offset[0] = strtol (&ctmp[1], (char **) NULL, 0);
  } else
    offset[0] = 0;

  ctmp = strchr (stg, '{');
  if (ctmp) {                           /* remove the trailing time zone *//*  */
    ctmp[0] = 0;                        /*  */
    zone = atoi (&ctmp[1]);
  } else
    zone = -1;

  ctmp = strchr (stg, '[');
  if (ctmp) {                           /* remove the trailing DST *//*  */
    ctmp[0] = 0;                        /*  */
    t_m.tm_isdst = atoi (&ctmp[1]);
  } else
    t_m.tm_isdst = -1;

  for (i = 0; i < 8; i++)
    tok[i] = NULL;
  tok[0] = strtok (stg, delim[0]);
  for (i = 1; i < 8; i++)
    tok[i] = strtok (NULL, delim[i]);
  for (i = 0; i < 8; i++)
    if (tok[i]) {
      token_count += 1;
    }
  memset (&t_m, 0, sizeof (t_m));
  switch (token_count) {
   default:
     return 0;
   case 6:
     t_m.tm_year = atoi (tok[0]) % 100;
     t_m.tm_mon = atoi (tok[1]) - 1;
     t_m.tm_mday = atoi (tok[2]);
     t_m.tm_hour = atoi (tok[3]) - 1;
     t_m.tm_min = atoi (tok[4]) - 1;
     t_m.tm_sec = atoi (tok[5]) - 1;
     break;
   case 5:
     t_m.tm_year = atoi (tok[0]) % 100;
     offset[0] = offset[0] - (atoi (tok[1]) * 86400) + 86400;
     t_m.tm_mon = 0;
     t_m.tm_mday = 1;
     t_m.tm_hour = atoi (tok[2]) - 1;
     t_m.tm_min = atoi (tok[3]) - 1;
     t_m.tm_sec = atoi (tok[4]) - 1;
     break;
  }
  offset[0] = offset[0] + (zone * 3600) + fudge;
  return &t_m;
}

 /*
  *     Convert a numeric or time string into an epoch value
  * returns epoch
  */
long utilo_epoch (char *field)
{
  char *result = NULL;
  struct tm *t_m;
  int epoch = 0;
  int offset;
  long temp;

  if (field)
    result = utilo_char (field);
  else
    result = utilo_char ("EPOCH");
  if (!result)
    return 0;
  if (!result[0])
    return 0;
  if (strstr (result, "0x")) {
    epoch = strtol (result, NULL, 0);
  } else if (strstr (result, "0d")) {
    char *tmp;

    tmp = strstr (result, "0d");
    tmp += 2;
    epoch = strtol (tmp, NULL, 0);
  } else {
    t_m = utilo_time (result, &offset);
    temp = mktime (t_m);
    return temp - offset;
  }
  return epoch;
}

/*dump time field */
void utilo_tm (char *field)
{
  char *result;
  struct tm *t_m;
  int epoch = 0;
  long temp;
  int offset;

  if (field)
    result = utilo_char (field);
  else
    result = utilo_char ("EPOCH");
  if (strstr (result, "0x")) {
    return;
  } else if (strstr (result, "0d")) {
    return;
  } else {
    t_m = utilo_time (result, &offset);
    temp = mktime (t_m);
    utilo_dump_tm (t_m, temp, offset);
  }
}

 /*
  * Update offset file with new value
  * * Allows calling program to update fields in the offset file.
  * * returns (voide)
  */
int utilo_update (char *item,           /* mnemonic */
                  char *value,          /* new value */
                  int version)
{                                       /* version number */
  FILE *cur, *old, *new;
  int i;
  char find_new[256];
  char find_old[256];
  char string[128];
  char str1[128];
  char *tok;

  utilo_init (NULL);
  strcpy (find_new, findname);
  strcpy (find_old, findname);
  strcat (find_old, ".old");
  strcat (find_new, ".new");

  cur = fopen (findname, "r");
  new = fopen (find_new, "w");

  while (fgets (string, 127, cur)) {
    i = strlen (string);
    strcpy (str1, string);
    tok = strtok (str1, delim);
    if (!strcasecmp (tok, item)) {
      sprintf (string, "%s%s%s\n", item, delim, value);
    }
    if (!strcasecmp (tok, "VERSION")) {
      sprintf (string, "%s%s%d\n", tok, delim, version);
    }
    fputs (string, new);
  }
  fclose (cur);
  fclose (new);
  rename (findname, find_old);
  rename (find_new, findname);
}
