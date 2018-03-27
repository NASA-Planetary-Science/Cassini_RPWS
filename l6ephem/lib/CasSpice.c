
/*
#include <stddef.h>
*/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <limits.h>
#include <stdbool.h>

#include <SpiceUsr.h>
#include <rpwstlm/CasSpice.h>


/* extern int getsms_ (char *, int);
extern int getlms_ (char *, int);
extern int qcktrc_ (char *, int); */

char *CAS_NAIF_METAKERNEL;              /* metafiles for kernels to load */

ULONG mk_cas_epoch (void);


/* 
  Stuff naif could of documented, instead you had to read the source code 

NAIF Definition:
int lenout -Length of list for output

What naif really meant:
int lenout - is the number of characters - 1 to be returned when using a
             "GET" operation... otherwise doesn't matter.


*/

bool CasSpice_Init (char *sMetaFile)
{
  bool bStatus = true;
  char s[32], *pStr;
  int lenout = 0;                       /* any guess for a value, seems to be fortran leftovers */

  /*
   * there is no way to redirect errors to stderr 
   */
  strcpy (s, "ALL");
  errprt_c ("SET", lenout, s);
  strcpy (s, "RETURN");
  erract_c ("SET", lenout, s);
  strcpy (s, "NULL");
  errdev_c ("SET", lenout, s);          /* no error messages */

  if (sMetaFile != NULL) {
    CAS_NAIF_METAKERNEL = sMetaFile;
  } 
  else {
	  if ((sMetaFile = getenv ("CAS_TIME_KERNELS")) != NULL) {
	    CAS_NAIF_METAKERNEL = malloc (strlen (sMetaFile) + 1);
	    strcpy (CAS_NAIF_METAKERNEL, sMetaFile);
	  } 
	  else {
	    fprintf(stderr, "CasSpice_Init: sMetaFile == NULL and env. var. "
				   "CAS_TIME_KERNELS is not set.\n");
		 exit(13);
	  }
  }

  furnsh_c (CAS_NAIF_METAKERNEL);

  if ((pStr = CasSpice_sError ()) != NULL) {
    fprintf (stderr, "%s\n", pStr);
    exit (1);
  }

  return bStatus;
}



char *CasSpice_sError (void)
{
  int iNext;
  static char sBuff[1841];

  if (!failed_c ())
    return NULL;                        /* no error */

  getmsg_c("SHORT", 41, sBuff);
  iNext = strlen(sBuff);
  sBuff[iNext] = ' ';
  iNext++;
  getmsg_c("LONG", 1841-iNext , sBuff+iNext);
  
  return sBuff;
}


/* returns the current greenwich time, yyyy-doyThh:mm:ss.msc */
char *CasSpice_GMT (char *pSin)
{
  static int nMsec;
  time_t t;
  struct tm *x;
  static char arStr[32], *pStr;

  t = time (NULL);
  x = gmtime (&t);

  if (pSin == NULL)
    pStr = arStr;
  else
    pStr = pSin;

  sprintf (pStr, "%04d-%03dT%02d:%02d:%02d.%03d", x->tm_year + 1900,
           x->tm_yday + 1, x->tm_hour, x->tm_min, x->tm_sec, nMsec++);

  return pStr;
}

/* returns the current greenwich time, yyyy-mm-ddThh:mm:ss.msc */
char *CasSpice_GMT_yyyymmdd (char *pSin)
{
  static int nMsec;
  time_t t;
  struct tm *x;
  static char arStr[32], *pStr;

  t = time (NULL);
  x = gmtime (&t);

  if (pSin == NULL)
    pStr = arStr;
  else
    pStr = pSin;

  sprintf (pStr, "%04d-%02d-%02dT%02d:%02d:%02d.%03d", x->tm_year + 1900,
           x->tm_mon + 1, x->tm_mday, x->tm_hour, x->tm_min, x->tm_sec,
           nMsec++);

  return pStr;
}



/* 
  Convert a 32bit s/c clock into the standard jpl string ssssssssss.fff
  where s=seconds and f=1/256seconds.
  if pSin==NULL, then use the static internal string buffer. 
*/
char *CasSpice_nSclk_to_sSclk (ULONG nSclk, ULONG nFine, char *pSin)
{
  static char arStr[32], *pStr;

  if (pSin == NULL)
    pStr = arStr;
  else
    pStr = pSin;

  sprintf (pStr, "%10d.%03d", nSclk, nFine);

  return pStr;
}



/* 
  Convert a 32bit s/c clock into the standard jpl string 2002-355T12:32:16.068
  if pSin==NULL, then use the static internal string buffer. 
*/
char *CasSpice_nSclk_to_sScet (ULONG nSclk, ULONG nFine, char *pSin)
{
  char sSclk[32];
  double dEt;
  static char sScet[32];


  sprintf (sSclk, "%10d.%03d", nSclk, nFine);
  scs2e_c (CAS_NAIF_ID_CODE, sSclk, &dEt);      /* sclk string to et (double) */
  et2utc_c (dEt, "ISOD", 3, 32, sScet); /* et to UTC string */

  if (failed_c ()) {                    /* spice choked, use the ansi c routines */
    time_t nTmp;
    ULONG nEpoch;
    struct tm *ptm;

    reset_c ();                         /* reset spice error flags and messages */
    fprintf (stderr, "error, CasSpice_nSclk_to_sScet(0x%08X,0x%08X)\n",
             nSclk, nFine);
    fprintf (stderr, "using workstation approximation\n");

    nEpoch = mk_cas_epoch ();
    nTmp = nSclk - nEpoch;              /* valid workstation time */
    ptm = gmtime (&nTmp);
    sprintf (sScet, "%04d-%03dT%02d:%02d:%02d.%03d", ptm->tm_year + 1900,
             ptm->tm_yday + 1, ptm->tm_hour, ptm->tm_min, ptm->tm_sec,
             nFine / 256);
  }

  if (pSin != NULL)
    strcpy (pSin, sScet);


  return sScet;
}



/* 
  Convert 32bit s/c clock to the jpl standard binary representation of scet
  16bits of days, 32bits of millisecond of days.
*/
ULONG CasSpice_nSclk_to_nScet (ULONG nSclk, ULONG nFine, ULONG * pDays,
                               ULONG * pMsec)
{
  char sSclk[32], sScet[32];
  int nYear, nDoy, nHour, nMin, nSec, nMs, i;
  ULONG nDays, nMsec;
  double dEt;


  sprintf (sSclk, "%10d.%03d", nSclk, nFine);
  scs2e_c (CAS_NAIF_ID_CODE, sSclk, &dEt);      /* sclk string to et (double) */
  et2utc_c (dEt, "ISOD", 3, 32, sScet); /* et to UTC string */

  if (failed_c ()) {                    /* spice choked, use the ansi c routines */
    time_t nTmp;
    ULONG nEpoch;
    struct tm *ptm;

    reset_c ();                         /* reset spice error flags and messages */
    fprintf (stderr, "error, CasSpice_nSclk_to_nScet(0x%08X,0x%08X)\n",
             nSclk, nFine);

    nEpoch = mk_cas_epoch ();
    nTmp = nSclk - nEpoch;              /* valid workstation time */
    ptm = gmtime (&nTmp);
    sprintf (sScet, "%04d-%03dT%02d:%02d:%02d.%03d", ptm->tm_year + 1900,
             ptm->tm_yday + 1, ptm->tm_hour, ptm->tm_min, ptm->tm_sec,
             nFine / 256);
  }

  nYear = strtol (&sScet[0], NULL, 10); /* parse 2002-355T12:32:16.123 */
  nDoy = strtol (&sScet[5], NULL, 10);  /*       0123456789ABCDEF01234 */
  nHour = strtol (&sScet[9], NULL, 10);
  nMin = strtol (&sScet[12], NULL, 10);
  nSec = strtol (&sScet[15], NULL, 10);
  nMs = strtol (&sScet[18], NULL, 10);

  nDays = 0;
  for (i = 1958; i < nYear; i++) {
    if (i % 100)                        /* Year is NOT a century year */
      nDays += (i % 4) ? 365 : 366;     /* if evenly divisible by 4, leap year */
    else                                /* Year is a century year */
      nDays += (i % 400) ? 365 : 366;   /* if evenly divisible by 400, leap year */
  }
  nDays += (nDoy - 1);                  /* doy is number 1-365 (366) */

  nMsec = 0;
  nMsec += nHour * 60 * 60 * 1000;
  nMsec += nMin * 60 * 1000;
  nMsec += nSec * 1000;
  nMsec += nMs;

  if (pDays != NULL)
    *pDays = nDays;
  if (pMsec != NULL)
    *pMsec = nMsec;



  return nDays;
}



/*
   returns the system's approximation in seconds from Jan 1958 to its epoch.
*/
ULONG mk_cas_epoch (void)
{
  time_t t0, t1;
  struct tm tm0, *ptm;

  /*
   * epoch for the Cassini Spacecraft 
   */
  tm0.tm_year = 58;
  tm0.tm_mon = 0;
  tm0.tm_mday = 1;
  tm0.tm_isdst = 0;
  tm0.tm_hour = 0;
  tm0.tm_min = 0;
  tm0.tm_sec = 0;
  t0 = mktime (&tm0);                   /* takes local time and returns gmt of the local time */

  /*
   * find the epoch for the workstation 
   */
  t1 = 0;
  ptm = gmtime (&t1);                   /* returns gmt of time zero */
  t1 = mktime (ptm);                    /* reference gmt date to the local time */

/*
fprintf(stderr,"%04d = 0x%08X (%d)\n",tm0.tm_year+1900,t0,t0);
fprintf(stderr,"%04d = 0x%08X (%d)\n",ptm->tm_year+1900,t1,t1);
*/
  /*
   * subtract the local times (represented by gmt equivalents) to find the
   * offset from time zero 
   */

  return (ULONG) difftime (t1, t0);
}



/* sScet must be = to "2000-153T08:30:36.123" */
ULONG CasSpice_sScet_to_nScet (char *sScet, ULONG * pDays, ULONG * pFine)
{
  int nYear, nDoy, nHour, nMin, nSec, nMs, i;
  ULONG nDays, nFine;


  nYear = strtol (sScet + 0, NULL, 10); /* parse 2002-355T12:32:16.123 */
  nDoy = strtol (sScet + 5, NULL, 10);  /*       0123456789ABCDEF01234 */
  nHour = strtol (sScet + 9, NULL, 10);
  nMin = strtol (sScet + 12, NULL, 10);
  nSec = strtol (sScet + 15, NULL, 10);
  nMs = strtol (sScet + 18, NULL, 10);

  nDays = 0;
  for (i = 1958; i < nYear; i++) {
    if (i % 100)                        /* Year is NOT a century year */
      nDays += (i % 4) ? 365 : 366;     /* if evenly divisible by 4, leap year */
    else                                /* Year is a century year */
      nDays += (i % 400) ? 365 : 366;   /* if evenly divisible by 400, leap year */
  }
  nDays += (nDoy - 1);                  /* doy is number 1-365 (366) */

  nFine = 0;
  nFine += nHour * 60 * 60 * 1000;
  nFine += nMin * 60 * 1000;
  nFine += nSec * 1000;
  nFine += nMs;

  if (pDays != NULL)
    *pDays = nDays;
  if (pFine != NULL)
    *pFine = nFine;

  return nDays;
}



/* sScet must be = to "2000-153T08:30:36.123" */
int CasSpice_sScet_to_nSclk (char *sScet, ULONG * pSclk, ULONG * pFine)
{
  char sSclk[32], *pStr;
  int nPartition = 1;
  double dET;



  utc2et_c (sScet, &dET);
  sce2s_c (CAS_NAIF_ID_CODE, dET, 32, sSclk);   /* lenout=32 ? */

  if ((pStr = CasSpice_sError ()) != NULL) {
    fprintf (stderr, "%s", pStr);
    fprintf (stderr, "spice choked in CasSpice_sScet_to_nSclk(%s)\n\n",
             sScet);
    exit (1);
  }

/*
  if(failed_c()){  
    assert(0);
  }  
*/

/* 
    parse d/ssssssssss.mmm    d=partition number, 10 digits of seconds, &
          0123456789ABCDEF    mmm=1/256 of a second                       
*/
  nPartition = strtoul (sSclk, NULL, 10);
  sSclk[12] = '\0';
  *pSclk = strtoul (sSclk + 2, NULL, 10);
  *pFine = strtoul (sSclk + 13, NULL, 10);



  return nPartition;
}



char *CasSpice_nScet_to_sScet (ULONG nDoy, ULONG nMsec, char *pSin)
{
  int nYear, nHour, nMin, nSec;
  int nDays, nTotal;
  static char arStr[32], *pStr;


  ++nDoy;                               /* convert days since Jan. 1, 1958 [0-356] to day of year [1-366] */

  nDays = 365;
  nYear = 1959;                         /* initial conditions 365 days in 1958 */
  nTotal = nDays;
  while (nDoy > nTotal) {
    if (nYear % 100)                    /* Year is NOT a century year */
      nDays = (nYear % 4) ? 365 : 366;  /* if evenly divisible by 4, leap year */
    else                                /* Year is a century year */
      nDays = (nYear % 400) ? 365 : 366;        /* if evenly divisible by 400, leap year */
    nTotal += nDays;
    ++nYear;
  }
  nYear -= 1;
  nTotal -= nDays;
  nDoy -= nTotal;                       /* days since jan 1 [0-365] */

  nHour = nMsec / (1000 * 60 * 60);
  nMsec -= (nHour * 1000 * 60 * 60);
  nMin = nMsec / (1000 * 60);
  nMsec -= (nMin * 1000 * 60);
  nSec = nMsec / (1000);
  nMsec -= (nSec * 1000);

  if (pSin == NULL)
    pStr = arStr;
  else
    pStr = pSin;

  /*
   * yyyy-doy T hh : mm : ss .mil 
   */
  sprintf (pStr, "%04d-%03dT%02d:%02d:%02d.%03d", nYear, nDoy,
           nHour, nMin, nSec, nMsec);


  return pStr;
}



char *CasSpice_nScet_to_GOOFY_sScet (ULONG nDoy, ULONG nMsec, char *pSin)
{
  bool bLeap;
  int nYear, nMonth, nDom, nHour, nMin, nSec;
  int nDays, nTotal, *p;
  static char arStr[32], *pStr;
  static int arDoy[] =
    { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 };
  static int arLdoy[] =
    { 0, 31, 60, 91, 121, 152, 183, 213, 244, 274, 305, 335, 366 };
  /*
   * J  F  M  Apr May Jun Jul Aug Sep Oct Nov Dec 
   */



  ++nDoy;                               /* convert days since Jan. 1, 1958 [0-356] to day of year [1-366] */

  nDays = 365;
  nYear = 1959;                         /* initial conditions 365 days in 1958 */
  nTotal = nDays;
  while (nDoy > nTotal) {
    if (nYear % 100)                    /* Year is NOT a century year */
      nDays = (nYear % 4) ? 365 : 366;  /* if evenly divisible by 4, leap year */
    else                                /* Year is a century year */
      nDays = (nYear % 400) ? 365 : 366;        /* if evenly divisible by 400, leap year */
    nTotal += nDays;
    ++nYear;
  }
  nYear -= 1;
  nTotal -= nDays;
  nDoy -= nTotal;                       /* days since jan 1 [0-365] */

  if (nYear % 100)                      /* Year is NOT a century year */
    bLeap = (nYear % 4) ? false : true; /* if evenly divisible by 4, leap year */
  else                                  /* Year is a century year */
    bLeap = (nYear % 400) ? false : true;       /* if evenly divisible by 400, leap year */

  if (bLeap == true)
    p = arLdoy;
  else
    p = arDoy;

  for (nMonth = 1; nMonth < 13; nMonth++) {
    if (nDoy <= p[nMonth])              /* nDoy is days since Jan 1. */
      break;
  }
  nDom = nDoy - p[nMonth - 1];

  nHour = nMsec / (1000 * 60 * 60);
  nMsec -= (nHour * 1000 * 60 * 60);
  nMin = nMsec / (1000 * 60);
  nMsec -= (nMin * 1000 * 60);
  nSec = nMsec / (1000);
  nMsec -= (nSec * 1000);

  if (pSin == NULL)
    pStr = arStr;
  else
    pStr = pSin;

  /*
   * yyyy- mm - dd T hh : mm : ss .mil 
   */
  sprintf (pStr, "%04d-%02d-%02dT%02d:%02d:%02d.%03d", nYear, nMonth, nDom,
           nHour, nMin, nSec, nMsec);



  return pStr;
}



/* 
  Convert a 32bit s/c clock into the a perverted version of the standard 
  jpl string ssssssssss:fff
  where s=seconds and f=1/256seconds.
  if pSin==NULL, then use the static internal string buffer. 
*/
char *CasSpice_nSclk_to_GOOFY_sSclk (ULONG nSclk, ULONG nFine, char *pSin)
{
  static char arStr[32], *pStr;

  if (pSin == NULL)
    pStr = arStr;
  else
    pStr = pSin;

  sprintf (pStr, "%10d:%03d", nSclk, nFine);

  return pStr;
}
