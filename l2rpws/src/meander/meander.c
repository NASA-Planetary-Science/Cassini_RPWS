#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>
		 
#include <string.h>
#include <strings.h>

#include <Cext.h>

#include <rpwstlm/RecordFile.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasHfr.h>

/* 

Version 3.2  
  Wednesday, May 5, 2004 recompile with current libraries 

Version 3.3  
  Wednesday, July 7, 2004 nMaxRec was erronously initialized to 65536
  
Version 3.4
  Tuesday, Oct. 10, 2012 Convert to using stdbool.h
*/



extern const char *sCasHfrMeanderVersion;
extern int nMeanderDebug;

void meanderSleep (double dSeconds);
void Help (void);

static const char *sVersion = "meander() ver 3.3";



int main (int argc, char *argv[])
{
  bool bEof = true, bVerbose = false, bSilent = false, bVersion = false;
  bool bNoJunk = false;
  char sTmp[512];
  Ulong nInCnt, nOutCnt, nCmpCnt, nDecCnt, nErrCnt, nWarnCnt;
  Ulong nRecCnt, nRecMax, nRecSkip;
  Ulong nRecLen, nStatus, nTmp;
  RecordFile *hInRec, *hOutRec;         /* Handles to the in & out files */
  CasRecord *pInRec, *pOutRec;



/* Initialize Command Line Variables */
  bEof = true;                          /* Exit on End of File */
  nRecSkip = 0;
  nRecCnt = 0;
  nRecMax = 0xFFFFFFFF;


  while (--argc) {
    ++argv;
    if (!strcmp ("-cnt", *argv)) {
      --argc;
      ++argv;
      nRecMax = strtol (*argv, NULL, 0);
    } else if (!strcmp ("-eof", *argv))
      bEof = false;
    else if (!strcmp ("+eof", *argv))
      bEof = true;
    else if (!strcmp ("-max", *argv)) {
      --argc;
      ++argv;
      nRecMax = strtol (*argv, NULL, 0);
    } else if (!strcmp ("-skip", *argv)) {
      --argc;
      ++argv;
      nRecSkip = strtol (*argv, NULL, 0);
    } else if (!strcmp ("-nojunk", *argv))
      bNoJunk = true;
    else if (!strcmp ("-d", *argv)) {
      --argc;
      ++argv;
      if ((*argv[0] != '-') && (*argv[0] != '+'))
        nMeanderDebug = strtol (*argv, NULL, 0);
      else {
        ++argc;
        --argv;
        nMeanderDebug = 0;
      }
    } else if (!strcmp ("-s", *argv)) {
      bSilent = true;
    } else if (!strcmp ("-v", *argv)) {
      bVerbose = true;
    } else if ((!strcmp ("-version", *argv)) || (!strcmp ("-ver", *argv))) {
      bVersion = true;
    } else if (!strcmp ("-help", *argv)) {
      Help ();
      exit (1);
    } else {
      fprintf (stderr, "Bad Argument:%s\n", *argv);
      exit (1);
    }                                   /* else */
  }

  if (bVerbose == true)
    bSilent = false;

  if (bVersion == true) {
    fprintf (stderr, "%s\n", sVersion);
    fprintf (stderr, " %s\n", sCasHfrMeanderVersion);
    fprintf (stderr, " %s\n", CasTlm_Version ());
  }

  pInRec = CasRecord_Constructor (NULL);
  pOutRec = CasRecord_Constructor (NULL);

  hInRec = RecordFile_Constructor (NULL);
  if (RecordFile_Open (hInRec, "stdin", "rb") == false) {
    fprintf (stderr, "Unable to read stdin\n");
    exit (1);
  }
  hOutRec = RecordFile_Constructor (NULL);
  if (RecordFile_Open (hOutRec, "stdout", "wb") == false) {
    fprintf (stderr, "Unable to write stdout\n");
    exit (1);
  }



  nInCnt = nOutCnt = nCmpCnt = nDecCnt = nErrCnt = nWarnCnt = 0;

  /*
   * Skip the first N records 
   */
  while (nRecSkip-- > 0) {
    nRecLen = RecordFile_ReadNextRecord (hInRec, pInRec);
    ++nInCnt;
  }
  /*
   * Read until MaxRecords or eof 
   */

  while (nRecCnt < nRecMax) {
    nRecLen = RecordFile_ReadNextRecord (hInRec, pInRec);

    if (nRecLen == 0) {
      if (bEof == true)
        break;
      else
        meanderSleep (0.125);
    } else {
      ++nInCnt;
      ++nRecCnt;

      if (CasHfr_bHfrPacket (pInRec->data) == true) {
        if (CasHfr_bMeanderCompressed (pInRec->data) == true)
          ++nCmpCnt;
      }

      /*
       * try to decompress the packet 
       */
      nStatus = CasHfr_Meander (pInRec, pOutRec);
      if (bNoJunk == true) {
        if (!(pOutRec->status.gnd_status & CasMpii_ErrorMask) &&
            !(pOutRec->status.gnd_status & CasMpus_ErrorMask) &&
            !(pOutRec->status.gnd_status & CasMeander_ErrorMask))
          RecordFile_WriteRecord (hOutRec, pOutRec);
        ++nOutCnt;
      } else {
        RecordFile_WriteRecord (hOutRec, pOutRec);
        ++nOutCnt;
      }

      if (nStatus != (pOutRec->status.gnd_status & CasMeander_Mask))
        fprintf (stderr, "returned status and header status differ\n");
      nStatus = pOutRec->status.gnd_status & CasMeander_Mask;

      if (nStatus & CasMeander_Decompressed)
        ++nDecCnt;

      /*
       * meander decompression errors 
       */
      nTmp = CasMeander_HeaderErrors | CasMeander_BufferOverRun;
      nTmp |= CasMeander_MakeClassError | CasMeander_GetBitsError;
      if (nStatus & nTmp)
        ++nErrCnt;

      /*
       * meander decompression warnings 
       */
      nTmp = CasMeander_ZeroFilled | CasMeander_DataTermination;
      if (nStatus & nTmp)
        ++nWarnCnt;

      if (bVerbose == true) {           /* output every packets status */
        sprintf (sTmp, "(%X) 0x%X",
                 (pOutRec->status.cmprs_status >> 24) & 0x00FF,
                 pOutRec->status.cmprs_status & 0x00FFFFFF);
        fprintf (stderr, "%s %-9s %s\n", CasMp_sStdHdr (pOutRec), sTmp,
                 CasHfrMeander_DecodeGndStatus (pOutRec->status.gnd_status));
      } else if (bSilent == false) {    /* only packets with meander errors */
        if (nStatus & CasMeander_ErrorMask) {
          sprintf (sTmp, "(%X) 0x%X",
                   (pOutRec->status.cmprs_status >> 24) & 0x00FF,
                   pOutRec->status.cmprs_status & 0x00FFFFFF);
          fprintf (stderr, "%s %-9s %s\n", CasMp_sStdHdr (pOutRec), sTmp,
                   CasHfrMeander_DecodeGndStatus (pOutRec->status.
                                                  gnd_status));
        }
      } else {
        ;                               /* do nothing */
      }

    }                                   /* esle */

  }                                     /* While nRecCnt < nRecMax */

  fflush (stdout);

/*
  fprintf(stderr,"%s rd/wrt %d, ",sVersion,nRecCnt);
  fprintf(stderr,"%ld comp, %ld decomp. with %ld errors\n",
          nMeanderInCnt,nMeanderOutCnt,nMeanderErrCnt);

  fprintf(stderr,"%s rd/wrt %d/%d,",sVersion,nInCnt,nOutCnt);
  fprintf(stderr,"decomp. %d/%d with %d errors and %d warnings\n",
          nDecCnt,nCmpCnt,nErrCnt,nWarnCnt);
*/
  if (nInCnt != nOutCnt)
    fprintf (stderr, "%s read %d != wrote %d\n", sVersion, nInCnt, nOutCnt);

  fprintf (stderr, "%s i/o %d, ", sVersion, nOutCnt);
  fprintf (stderr, "decomp. %d/%d with %d errors and %d warnings\n",
           nDecCnt, nCmpCnt, nErrCnt, nWarnCnt);



  return 0;
}


void Help (void)
{
  fprintf (stderr, "%s\n", sVersion);
  fprintf (stderr, "   Meander Decompresses Hfr MiniPackets\n");

  fprintf (stderr,
           "-eof    quit or not at end of file\n"
           "-q    quiet\n"
           "-skip NNN  skip the first NNN packets\n"
           "-cnt NNN   read NNN packets and exit\n");

  return;
}



void meanderSleep (double dSeconds)
{
  struct timeval timeout;

  timeout.tv_sec = (unsigned long) dSeconds;
  timeout.tv_usec = (long) ((dSeconds - timeout.tv_sec) * 1000000);
  select (0, 0, 0, 0, &timeout);

  return;
}
