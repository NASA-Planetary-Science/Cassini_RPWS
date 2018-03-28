#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <sys/types.h>
#include <dirent.h>
#include <stdbool.h>
#include <errno.h>

#include <das2/das1.h>

#include <rpwstlm/CasType.h>
#include <rpwstlm/CasPds.h>




/* sBeg="2000-365T12:30:59.123"
         123456789ABCDEF012345 */
int CasPds_LRfull_GetFiles (char *sFiles[], char *sBeg, char *sEnd,
                            char *sDir, Ulong nLfdr, Ulong nMfdr, Ulong nMfr,
                            Ulong nHfr, Ulong nMsc)
{
  bool bLeap = false, bLastDay = false;
  char *p = NULL;
  char sPath[1024] = {'\0'};
  int nFiles = 0;
  int nBegYear = 0;
  int nBegDoy = 0;
  int nEndYear = 0;
  int nEndDoy = 0;
  int nQtr = 0;
  int nHour = 0;
  int nMinute = 0;
  int nSecond = 0;
  Ulong tBeg = 0, tEnd = 0;
  struct dirent *dirp = NULL;
  DIR *dp = NULL;


  assert (strlen (sEnd) > 16);
  p = sBeg;
  nBegYear = atoi(p);
  p += 5;
  nBegDoy = atoi(p);
  p = sEnd;
  nEndYear = atoi(p);
  p += 5;
  nEndDoy = atoi(p);
  p += 4;
  nHour = atoi(p);
  p += 3;
	nMinute = atoi(p);
  p += 3;
  nSecond = atoi(p);
  if (nHour == 0 && nMinute == 0 && nSecond == 0)
    bLastDay = false;
  else
    bLastDay = true;
  
  if(((nBegYear * 1000 + nBegDoy) < 1994000) ||
	  ((nEndYear * 1000 + nEndDoy) < 1994000) ){
	  fprintf(stderr, "CasPds_LRfull_GetFiles() error in time range %s to %s\n",
		       sBeg, sEnd);
	  return 0;
  }
  
  tBeg = (Ulong)(nBegYear * 1000 + nBegDoy);
  tEnd = (Ulong)(nEndYear * 1000 + nEndDoy); 
  nQtr = (int)(nBegDoy / 100);

  while (tBeg <= tEnd) {
    sprintf (sPath, "%s/T%d%dXX/T%d%03d", sDir, nBegYear, nQtr, nBegYear,
             nBegDoy);
    if((dp = opendir(sPath)) != NULL){
		 
      /* Making this more verbose to debug a problem on 2013-01-22 -cwp */ 
      while(true){
			errno = 0;
			dirp = readdir (dp);
			if(dirp == NULL){
				if(errno != 0)
					perror(sPath);
				break;
			}
				
        if((strcmp(dirp->d_name, ".")==0) || (strcmp(dirp->d_name, "..")==0))
          continue;
		  
        if (strstr (dirp->d_name, ".DAT")) {
          if ((strstr (dirp->d_name, "LFR") && nLfdr) ||
              (strstr (dirp->d_name, "MFDR") && nMfdr) ||
              (strstr (dirp->d_name, "MFR") && nMfr) ||
              (strstr (dirp->d_name, "HFR") && nHfr) ||
              (strstr (dirp->d_name, "MSC") && nMsc)) {
            sFiles[nFiles] =
              malloc (strlen (sPath) + strlen (dirp->d_name) + 2);
            sprintf (sFiles[nFiles], "%s/%s", sPath, dirp->d_name);
            sFiles[++nFiles] = NULL;
          }
        }
      }
		                                 /* elihw dir */
      closedir (dp);
    }

    /*
     * fi dir 
     */
    if (nBegYear % 100)                 /* Year is NOT a century year */
      bLeap = (nBegYear % 4) ? false : true;    /* if evenly divisible by 4, leap year */
    else                                /* Year is a century year */
      bLeap = (nBegYear % 400) ? false : true;  /*if evenly divisible by 400, leap year */
    if ((bLeap == true) && (nBegDoy >= 366)) {
      ++nBegYear;
      nBegDoy = 1;
    } else if ((bLeap == false) && (nBegDoy >= 365)) {
      ++nBegYear;
      nBegDoy = 1;
    } else {
      ++nBegDoy;
    }
    nQtr = nBegDoy / 100;
    tBeg = (Ulong)(nBegYear * 1000 + nBegDoy);

    /*
     * cure reading extra day for yyyy-100 yyyy-101 
     */
    if ((bLastDay == false) && (tBeg == tEnd))
      break;

  }                                     /* elihw */

  return nFiles;
}


/* ************************************************************************** */
/* dynamically allocate memory if pRec is NULL */

Uchar *CasPds_LRfull_ReadArchive (char *sFilename, Uchar * pRec)
{
  Uchar *pBuf, arHdr[128];
  size_t uRead;
  Ulong nHdrSize = 128;                 /* CORPWS Header Size xxx bytes min. */
  Ulong nRecLen, nNumRec, nType, nFileSize, *pDword;
  FILE *h;



  if ((h = fopen (sFilename, "r")) == NULL) {
    fprintf (stderr, "CasPds_LRfull_ReadArchive(%s,%p) fopen() failed.\n",
             sFilename, pRec);
    exit (1);
  }

  if ((uRead = fread (arHdr, sizeof (Uchar), nHdrSize, h)) != nHdrSize) {
    fprintf (stderr, "CasPds_LRfull_ReadArchive(%s,%p) fread() failed\n",
             sFilename, pRec);
    fprintf (stderr, "  returned %zu of %d\n", uRead, nHdrSize);
    exit (1);
  }

  /*
   * record Length in bytes and number of records 
   */
  swapBufIfHostLE(arHdr + 8, 4, 3);
  
  pDword = (void *) (arHdr + 8);        /* (Ulong*) */
  nRecLen = *pDword++;
  nNumRec = *pDword++;
  nType = *pDword++;
  
  nFileSize = nRecLen * nNumRec;

  if (nFileSize > CAS_PDS_MAX_FILE_SIZE) {
    fprintf (stderr, "CasPds_LRfull_ReadArchive(%s,%p), file too large.\n",
             sFilename, pRec);
    fprintf (stderr, "  file size %ld (bytes) exceeds buffer size %d\n",
             (long)nFileSize, CAS_PDS_MAX_FILE_SIZE);
    exit (1);
  }

  if (pRec == NULL) {
    if ((pBuf = malloc (nFileSize)) == NULL) {
      fprintf (stderr, "file size exceeds buffer size %ld\n", (long)nFileSize);
      exit (1);
    }
  } else {
    pBuf = pRec;
  }

  /*
   * position the pointer to the beginning of the file and read the whole 
   */
  assert (fseek (h, 0, SEEK_SET) == 0);
  if ((uRead = fread (pBuf, sizeof (Uchar), nFileSize, h)) != nFileSize) {
    fprintf (stderr, "fread() failed, returned %zu of %d\n", uRead,
             nFileSize);
    exit (1);
  }

  fclose (h);



  return pBuf;
}
