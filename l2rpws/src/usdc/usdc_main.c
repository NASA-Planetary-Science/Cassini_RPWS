#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>
#include <strings.h>

#include <rpwstlm/RecordFile.h>
#include <rpwstlm/CasRecord.h>
#include <rpwstlm/CasMiniPacket.h>
#include <rpwstlm/CasType.h>

#include "usdc.h"
#include "rice.h"



/* no mods since 8.0 -  */
static const char *sVersion = "rpws_usdc() ver 8.4";


void show_help (FILE * h);
void usdcSleep (double dSeconds);
int fg_filter (int argc, char *argv[]);

char IN_FILE_NAME[512] = "stdin";
char OUT_FILE_NAME[512] = "stdout";

int main (int argc, char *argv[])
{
  bool cmdln_eof = false, cmdln_version = false, cmdln_psi = false;
  char infile[512] = "stdin", outfile[512] = "stdout";
  int i;

  bool bVerbose = false, bSilent = false, bStats = false;
  Ulong nInLen, nOutLen;
  Ulong nLength, nInCnt, nOutCnt, nRiceCnt, nErrCnt;
  RecordFile *hInRec, *hOutRec;
  CasRecord *pInRec, *pOutRec[5];


  fg_filter (argc, argv);
  strcpy (infile, IN_FILE_NAME);
  strcpy (outfile, OUT_FILE_NAME);

  fprintf (stderr, "usdc in=%s\n", infile);
  fprintf (stderr, "usdc out=%s\n", outfile);

  nInCnt = nOutCnt = nRiceCnt = nErrCnt = 0;
  while (--argc) {
    ++argv;
    if ((!strcmp (*argv, "-h")) || (!strcmp (*argv, "-help"))) {
      show_help (stdout);
      exit (0);
    } else if (!strcmp (*argv, "-stats")) {
      bStats = true;
    } else if ((!strcmp (*argv, "-ver")) || (!strcmp (*argv, "-version"))) {
      cmdln_version = true;
    } else if (!strcmp (*argv, "-debug")) {
      while (argc) {
        --argc;
        ++argv;
        if (argc == 0) {
          ++argc;
          --argv;
          break;
        } else if ((*argv[0] == '-') || (*argv[0] == '+')) {
          ++argc;
          --argv;
          break;
        } else if (!strcmp (*argv, "all"))
          RICE_DEBUG |= RICEDEBUG_ALL;
        else if (!strcmp (*argv, "map"))
          RICE_DEBUG |= RICEDEBUG_MAPDELTA;
        else if (!strcmp (*argv, "out"))
          RICE_DEBUG |= RICEDEBUG_OUTDATA;
        else if (!strcmp (*argv, "psi"))
          RICE_DEBUG |= RICEDEBUG_PSIHIST;
        else if (!strcmp (*argv, "err"))
          RICE_DEBUG |= RICEDEBUG_ERRORS;
        else
          RICE_DEBUG |= strtoul (*argv, NULL, 0);
      }                                 /* elihw debug */
    } /* esle debug */
    else if (!strcmp (*argv, "-debug_file")) {
      --argc;
      ++argv;
      RICE_FILENAME = *argv;
    } else if (!strcmp (*argv, "+eof")) {
      cmdln_eof = true;
    } else if (!strcmp (*argv, "-psi")) {
      cmdln_psi = true;
    } else if (!strcmp (*argv, "-s")) {
      bSilent = true;
    } else if (!strcmp (*argv, "-v")) {
      bVerbose = true;
    } else {
      fprintf (stderr, "invalid arg(%d)=%s.\n", argc, *argv);
    }
  }                                     /* elihw command line arguments */

  if (cmdln_version == true) {
    fprintf (stderr, "%s\n", sVersion);
    fprintf (stderr, "  %s\n", CasTlm_Version ());
  }

  if (bVerbose == true) {
    bSilent = false;
    fprintf (stderr, "rice debug=%08lX\n", RICE_DEBUG);
  }

  hInRec = RecordFile_Constructor (NULL);
  if (RecordFile_Open (hInRec, infile, "rb") == false) {
    fprintf (stderr, "%s, error - unable to read %s.\n", sVersion, infile);
    exit (1);
  }
  hOutRec = RecordFile_Constructor (NULL);
  if (RecordFile_Open (hOutRec, outfile, "wb") == false) {
    fprintf (stderr, "%s, error - unable to write %s.\n", sVersion, outfile);
    exit (1);
  }

  pInRec = CasRecord_Constructor (NULL);

  for (i = 0; i < 5; i++)
    pOutRec[i] = CasRecord_Constructor (NULL);


  while (1) {

    nLength = RecordFile_ReadNextRecord (hInRec, pInRec);
    if (nLength > 0) {
      ++nInCnt;
      if (bVerbose == true)
        fprintf (stderr, "In: %s\n", CasMp_sStdHdr (pInRec));

      nInLen = pInRec->status.packet_length + 3;
      if (CasUsdc_DccDecompress (pInRec)) {     /* 0=not decompressed, 1=decompressed */
        ++nRiceCnt;
        if (pInRec->status.gnd_status & CasRice_ErrorMask) {
          ++nErrCnt;
          if (bSilent == false) {
            fprintf (stderr, "%s ", CasMp_sStdHdr (pInRec));
            if (pInRec->status.cmprs_status & 0xFF000000)
              fprintf (stderr, "(0x%lX) ",
                       pInRec->status.cmprs_status & 0x00FFFFFF);
            fprintf (stderr, "%s",
                     CasUsdc_DecodeGndStatus (pInRec->status.gnd_status));
            fprintf (stderr, "\n");
            if (cmdln_psi == true) {
              int sum = 0;

              for (i = 0; i < 16; i++)
                sum += arHist_Psi[i];
              fprintf (stderr, "%d blocks decompressed\n", sum);
              for (i = 0; i < 16; i++)
                fprintf (stderr, "%5d", i);
              fprintf (stderr, "\n");
              for (i = 0; i < 16; i++)
                fprintf (stderr, "%5d", arHist_Psi[i]);
              fprintf (stderr, "\n");
            }
          }                             /*fi silent */
        }
      }

      if ((pInRec->data[0] & 0xF0) == 0x80)     /* wfr packet, might be interleaved */
        nLength = CasWfr_Unleave (pInRec, pOutRec);
      else
        nLength = 0;


      if (nLength > 0) {                /* multiple wfr packets */
        for (i = 0; i < nLength; i++) {
          RecordFile_WriteRecord (hOutRec, pOutRec[i]);
          ++nOutCnt;
          if (bVerbose == true)
            fprintf (stderr, "Out: %s\n", CasMp_sStdHdr (pInRec));
        }
      } else {
        RecordFile_WriteRecord (hOutRec, pInRec);
        nOutLen = pInRec->status.packet_length + 3;
        if (bVerbose == true)
          fprintf (stderr, "Out: %s\n", CasMp_sStdHdr (pInRec));
        if (bStats == true)
          fprintf (stderr, "%s %ld/%ld=%.4f\n", CasMp_sStdHdr (pInRec),
                   nInLen, nOutLen, (float) nInLen / (float) nOutLen);
        ++nOutCnt;
      }
    } /* fi */
    else {
      if (cmdln_eof == true)
        break;
      else
        usdcSleep (0.125);
    }                                   /* esle */

  }                                     /* elihw */

  if (bSilent == false) {
    fprintf (stderr, "%s, read/wrote %ld/%ld packet, ", sVersion, nInCnt,
             nOutCnt);
    fprintf (stderr, "decomp. %ld pkts w/ %ld errors\n", nRiceCnt, nErrCnt);
  }


  return 1;
}



void usdcSleep (double dSeconds)
{
  struct timeval timeout;

  timeout.tv_sec = (unsigned long) dSeconds;
  timeout.tv_usec = (long) ((dSeconds - timeout.tv_sec) * 1000000);
  select (0, 0, 0, 0, &timeout);

  return;
}



void show_help (FILE * h)
{
  fprintf (h, "%s\n", sVersion);
  fprintf (h, "  -h | -help     shows help(this page).\n");
  fprintf (h,
           "  +eof           terminate program at eof, default is to sleep for"
           "                 1/8sec\n");

  fprintf (h,
           "  -debug 0xHHH SSS turn on debugging status for rice compression\n");
  fprintf (h,
           "         0x00000001 show reference sample and decompressed bits\n");
  fprintf (h, "         0x00000002 show code ids and decompressed bits\n");
  fprintf (h,
           "         0x00000004 show fundamental sequence decompressed bits\n");
  fprintf (h,
           "         0x00000008 show fundamental sequence decompressed data\n");
  fprintf (h, "         0x00000010 show split sequence decompressed bits\n");
  fprintf (h, "         0x00000020 show split sequence decompressed data\n");
  fprintf (h, "         0x00000040 show backup operator decompressed bits\n");
  fprintf (h, "         0x00000080 show backup operator decompressed data\n");
  fprintf (h, "         0x00000100 show mapped deltas\n");
  fprintf (h, "         0x00000200 show output data\n");
  fprintf (h, "         0x00000400 show psi histogram\n");
  fprintf (h, "         map   show mapped deltas\n");
  fprintf (h, "         out   show output data\n");
  fprintf (h, "         psi   show psi histogram\n");


  fprintf (h, "  -s             be silent, no status output\n");
  fprintf (h, "  -v             be verbose with status\n");
  fprintf (h,
           "  -ver           suppress output of program version information\n");



  return;
}
