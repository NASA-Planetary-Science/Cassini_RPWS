#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

#define _Cext_C
#include <Cext.h>
#undef _Cext_C

char *sEquals (const char *pSrc)
{
  char *pDst, *pCh;

  if (pSrc == NULL)
    return NULL;

  if ((pDst = (char *) malloc (sizeof (char) * (strlen (pSrc) + 1))) == NULL) {
    fprintf (stderr, "sEquals(%s), malloc(%d) failed.\n", pSrc,
             strlen (pSrc) + 1);
    return NULL;
  }

  pCh = pDst;
  while (*pSrc != '\0')
    *pDst++ = *pSrc++;
  *pDst = '\0';

  return pCh;
}


/* Cross Between sprintf and strcat */
char *strcatf (char *sBuf, ...)
{
  char *sFormat;
  va_list args;

  va_start (args, sBuf);
  sFormat = va_arg (args, char *);

  while (*sBuf != '\0')
    ++sBuf;                             /* find the end of the string */
  vsprintf (sBuf, sFormat, args);
  va_end (args);

  return sBuf;
}



/* Adds/subtracts forward slashes to generate an appropiate filename */
char *MakeAbsoluteFilename (char *sPath, char *sFile)
{
  char *pChTmp;
  static char sAbsoluteFilename[CEXT_FILENAME_MAX];

  sAbsoluteFilename[0] = '\0';

  if (sPath != NULL && *sPath != '\0') {        /* String length must be atleast one */
    pChTmp = strcpy (sAbsoluteFilename, sPath);
    while (*pChTmp != '\0')
      ++pChTmp;
    if (*--pChTmp != '/') {             /* Include a trailing slash if necessary */
      *++pChTmp = '/';
      *++pChTmp = '\0';
    }
  }

  if (sFile != NULL && *sFile != '\0')
    strcat (sAbsoluteFilename, sFile);

  return sAbsoluteFilename;
}



/*
  returns number parsed
*/
int LineParseNumbers (FILE * hHandle, char **pInLine, int nTokens, ...)
{
  char *pStart, *pEnd;
  int nTotal;
  double **pTokens;
  va_list args;
  static char sInLine[CEXT_LINE_IN_MAX];

  va_start (args, nTokens);
  if ((pTokens =
       (double **) calloc ((size_t) nTokens, sizeof (double *))) == NULL) {
    va_end (args);
    fprintf (stderr, "LineParseNumbers(%p,%p,%n) :: %p=calloc(%d,%d)\n",
             hHandle, pInLine, nTokens, pTokens, nTokens, sizeof (double *));
    return 0;
  }
  for (nTotal = 0; nTotal < nTokens; nTotal++)
    pTokens[nTotal] = va_arg (args, double *);

  va_end (args);


  nTotal = 0;
  while (fgets (sInLine, CEXT_LINE_IN_MAX, hHandle)) {

    /*
     * Nix any commented lines 
     */
    if ((pStart = strstr (sInLine, "#")))
      *pStart = '\0';
    pStart = sInLine;

    while (nTotal < nTokens) {

      /*
       * Find a numeric occurance 
       * 1. Not a  Number
       * 2. Not a floating point number, just punctuation
       * 2. Not end of line 
       */
      while ((*(pStart) < '0' || *(pStart) > '9') &&
             (*pStart != '.' || (*(pStart + 1) < '0' || *(pStart + 1) > '9'))
             && (*pStart != '\0'))
        ++pStart;

      if (*pStart == '\0')              /* End of line */
        break;
      *pTokens[nTotal++] = strtod (pStart, &pEnd);      /* Convert the number */
      pStart = pEnd;
    }                                   /* while getting nTokens */

    if (nTotal > 0)                     /* Have a line with valid input */
      break;

  }                                     /* while get next line */

  if (pInLine != NULL)
    *pInLine = sInLine;

  free (pTokens);

  return nTotal;
}
