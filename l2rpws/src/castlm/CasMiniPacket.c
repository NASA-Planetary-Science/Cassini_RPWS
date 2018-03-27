
/*
Tuesday, August 19, 2003
  Create generic input routines for minipackets
    CasMp_nHeaderLength(CasRecord *p)
    CasMp_nMode(CasRecord *p) 
      - references calls in CasMfr.c CasHfr.c, ...
*/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <strings.h>
#include <stdbool.h>

#include <casephem/CasSpice.h>

#include <rpwstlm/CasMiniPacket.h>
#include <rpwstlm/CasHfr.h>
#include <rpwstlm/CasLp.h>
#include <rpwstlm/CasMfr.h>
#include <rpwstlm/CasWfdr.h>
#include <rpwstlm/CasWfr.h>
#include <rpwstlm/CasWbr.h>


Ulong CasMp_nHeaderLength (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  Ulong nLength;

  switch (pMp[0] & 0xF0) {
   case 0x00:
     nLength = 4;
     break;                             /* CasMp_Stim */
   case 0x10:
     nLength = 5;
     break;                             /* CasMp_Mfr */
   case 0x20:                          /* CasMp_Hfr */
     if (CasHfr_bAnalysis (pMp) == true)
       nLength = 25;
     else if (CasHfr_bSounder (pMp) == true)
       nLength = 21;
     else if (CasHfr_bCalibration (pMp) == true)
       nLength = 12;
     else if (CasHfr_bMillisecond (pMp) == true)
       nLength = 12;
     else
       assert (0);
     break;
   case 0x40:
     nLength = 10;
     break;                             /* CasMp_Lp, used to be 8 ? */
   case 0x70:
     nLength = 6;
     break;                             /* CasMp_Lfdr */
   case 0x80:                          /* CasMp_Wfr */
     if (CasWfr_bMoreShitFollows (pMp) == true)
       nLength = 10;
     else
       nLength = 8;
     break;
   case 0xB0:
     nLength = 15;
     break;                             /* CasMp_Dust */
   case 0xD0:
     nLength = 6;
     break;                             /* CasMp_Mro */
   case 0xE0:
     nLength = 4;                       /* CasMp_Wbr */
     if (CasWbr_bMoreShitFollows (pMp) == true)
       nLength = 10;
     else
       nLength = 8;
     break;
   case 0xF0:
     nLength = 2;                       /* CasMp_Fill */
   default:                            /*CasMp_Id3,CasMp_Id5,CasMp_Id6,CasMp_Id9,CasMp_Id10,CasMp_Id12 */
     nLength = 4;
     assert (0);
     break;
  }

  return nLength;
}



Ulong CasMp_nMode (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  Ulong nMode;

  switch (pMp[0] & 0xF0) {
   case 0x00:
     nMode = CasMp_Stim;
     break;                             /* CasMp_Stim */
   case 0x10:
     nMode = CasMfr_nMode (pRec);
     break;                             /* CasMp_Mfr  */
   case 0x20:
     nMode = CasHfr_nMode (pRec);
     break;                             /* CasMp_Hfr  */
   case 0x30:
     nMode = CasMp_Id3;
     break;                             /* CasMp_Id3  */
   case 0x40:
     nMode = CasLp_nMode (pRec);
     break;                             /* CasMp_Lp   */
   case 0x50:
     nMode = CasMp_Id5;
     break;                             /* CasMp_Id5  */
   case 0x60:
     nMode = CasMp_Id6;
     break;                             /* CasMp_Id6  */
   case 0x70:
     nMode = CasWfdr_nMode (pRec);
     break;                             /* CasMp_Lfdr */
   case 0x80:
     nMode = CasMp_Wfr;
     break;                             /* CasMp_Wfr  */
   case 0x90:
     nMode = CasMp_Id9;
     break;                             /* CasMp_Id9  */
   case 0xA0:
     nMode = CasMp_Id10;
     break;                             /* CasMp_Id10 */
   case 0xB0:
     nMode = CasMp_Dust;
     break;                             /* CasMp_Dust */
   case 0xC0:
     nMode = CasMp_Id12;
     break;                             /* CasMp_Id13 */
   case 0xD0:
     nMode = CasMp_Mro;
     break;                             /* CasMp_Mro  */
   case 0xE0:
     nMode = CasMp_Wbr;
     break;                             /* CasMp_Wbr  */
   case 0xF0:
     nMode = CasMp_Fill;
     break;                             /* CasMp_Fill */
   default:
     assert (0);
     break;
  }

  return nMode;
}



char *CasMp_sReceiver (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  Ulong nSclk, nRti;
  static char sRcv[16];


  nSclk = pRec->status.cds_time;
  nRti = CasMp_nRti (pMp);

  switch (pMp[0] & 0xF0) {
   case 0x00:
     sprintf (sRcv, "STIM");
     break;
   case 0x10:                          /* date when header was redefined */
     if (nSclk < CasMfr_MagicDate)
       sprintf (sRcv, "MFR_nrm");
     else if (CasMfr_bFastToggle (pMp) == true)
       sprintf (sRcv, "MFR_ftg");
     else if (CasMfr_bNormal (pMp) == true)
       sprintf (sRcv, "MFR_nrm");
     else
       sprintf (sRcv, "MFR_unknown");
     break;
   case 0x20:                          /* CasMp_Hfr */
     if (CasHfr_bAnalysis (pMp) == true)
       sprintf (sRcv, "HFR_anl");
     else if (CasHfr_bSounder (pMp) == true)
       sprintf (sRcv, "HFR_snd");
     else if (CasHfr_bCalibration (pMp) == true)
       sprintf (sRcv, "HFR_cal");
     else if (CasHfr_bMillisecond (pMp) == true)
       sprintf (sRcv, "HFR_mil");
     else
       sprintf (sRcv, "HFR_unknown");
     break;
   case 0x30:
     sprintf (sRcv, "ID3");
     break;                             /* CasMp_Id3  */
   case 0x40:
     if (CasLp_bRawSweep (pMp) == true)
       sprintf (sRcv, "LP_rswp");
     else if (CasLp_bRawDensity (pMp) == true)
       sprintf (sRcv, "LP_rden");
     else if (CasLp_bAnalyzedSweep (pMp) == true)
       sprintf (sRcv, "LP_aswp");
     else if (CasLp_bTbdMode (pMp) == true)
       sprintf (sRcv, "LP_tbd");
     else
       sprintf (sRcv, "LP_unknown");
     break;
   case 0x50:
     sprintf (sRcv, "ID5");
     break;                             /* CasMp_Id5  */
   case 0x60:
     sprintf (sRcv, "ID6");
     break;                             /* CasMp_Id6  */
   case 0x70:
     if (CasWfdr_bLfdr (pMp) == true)
       sprintf (sRcv, "LFDR");
     else if (CasWfdr_bMfdr (pMp) == true)
       sprintf (sRcv, "MFDR");
     else
       sprintf (sRcv, "WFDR_unknown");
     break;
   case 0x80:
     if (CasWfr_bLowBand (pMp) == true)
       sprintf (sRcv, "WFR_lb");
     else if (CasWfr_bHighBand (pMp) == true)
       sprintf (sRcv, "WFR_hb");
     else
       sprintf (sRcv, "WFR_unknown");
     break;
   case 0x90:
     sprintf (sRcv, "ID9");
     break;                             /* CasMp_Id9  */
   case 0xA0:
     sprintf (sRcv, "ID10");
     break;                             /* CasMp_Id10 */
   case 0xB0:
     sprintf (sRcv, "DUST");
     break;                             /* CasMp_Dust */
   case 0xC0:
     sprintf (sRcv, "ID13");
     break;                             /* CasMp_Id13 */
   case 0xD0:
     sprintf (sRcv, "MRO");
     break;                             /* CasMp_Mro  */
   case 0xE0:
     if (CasWbr_bLowBand (pMp) == true)
       sprintf (sRcv, "WBR_lb");
     else if (CasWbr_bHighBand (pMp) == true)
       sprintf (sRcv, "WBR_hb");
     else
       sprintf (sRcv, "WBR_unknown");
     break;
   case 0xF0:
     sprintf (sRcv, "FILL");
     break;                             /* CasMp_Fill */
   default:
     sprintf (sRcv, "UNK_unknown");
     break;                             /* unknown mini-packet */
  }

  return sRcv;
}



char *CasMp_sStdHdr (CasRecord * pRec)
{
  char *pMsg, sRcv[16];
  Uchar *pMp = pRec->data;
  Ulong nSclk, nRti;
  static char sMsg[128];

  /*
   * pErrMsg=sErrMsg; 
   */
  nSclk = pRec->status.cds_time;
  nRti = CasMp_nRti (pMp);

  switch (pMp[0] & 0xF0) {
   case 0x00:
     sprintf (sRcv, "STIM");
     break;
   case 0x10:                          /* date when header was redefined */
     if (nSclk < CasMfr_MagicDate)
       sprintf (sRcv, "MFR nrm");
     else {
       if (CasMfr_bNormal (pMp) == true)
         sprintf (sRcv, "MFR nrm");
       else if (CasMfr_bFastToggle (pMp) == true)
         sprintf (sRcv, "MFR ftg");
       else
         sprintf (sRcv, "MFR unknown");
     }
     break;
   case 0x20:                          /* CasMp_Hfr */
     if (CasHfr_bAnalysis (pMp) == true)
       sprintf (sRcv, "HFR anl");
     else if (CasHfr_bSounder (pMp) == true)
       sprintf (sRcv, "HFR snd");
     else if (CasHfr_bCalibration (pMp) == true)
       sprintf (sRcv, "HFR cal");
     else if (CasHfr_bMillisecond (pMp) == true)
       sprintf (sRcv, "HFR mil");
     else
       sprintf (sRcv, "HFR unknown");
     break;
   case 0x30:
     sprintf (sRcv, "ID3");
     break;                             /* CasMp_Id3  */
   case 0x40:
     if (CasLp_bRawSweep (pMp) == true)
       sprintf (sRcv, "LP rswp");
     else if (CasLp_bRawDensity (pMp) == true)
       sprintf (sRcv, "LP rden");
     else if (CasLp_bAnalyzedSweep (pMp) == true)
       sprintf (sRcv, "LP aswp");
     else if (CasLp_bTbdMode (pMp) == true)
       sprintf (sRcv, "LP tbd");
     else
       sprintf (sRcv, "LP unknown");
     break;
   case 0x50:
     sprintf (sRcv, "ID5");
     break;                             /* CasMp_Id5  */
   case 0x60:
     sprintf (sRcv, "ID6");
     break;                             /* CasMp_Id6  */
   case 0x70:
     if (CasWfdr_bLfdr (pMp) == true)
       sprintf (sRcv, "LFDR");
     else if (CasWfdr_bMfdr (pMp) == true)
       sprintf (sRcv, "MFDR");
     else
       sprintf (sRcv, "MFDR unknown");
     break;
   case 0x80:
     if (CasWfr_bLowBand (pMp) == true)
       sprintf (sRcv, "WFR lb");
     else if (CasWfr_bHighBand (pMp) == true)
       sprintf (sRcv, "WFR hb");
     else
       sprintf (sRcv, "WFR unknown");
     break;
   case 0x90:
     sprintf (sRcv, "ID9  ");
     break;                             /* CasMp_Id9  */
   case 0xA0:
     sprintf (sRcv, "ID10 ");
     break;                             /* CasMp_Id10 */
   case 0xB0:
     sprintf (sRcv, "DUST ");
     break;                             /* CasMp_Dust */
   case 0xC0:
     sprintf (sRcv, "ID13 ");
     break;                             /* CasMp_Id13 */
   case 0xD0:
     sprintf (sRcv, "MRO  ");
     break;                             /* CasMp_Mro  */
   case 0xE0:
     if (CasWbr_bLowBand (pMp) == true)
       sprintf (sRcv, "WBR lb");
     else if (CasWbr_bHighBand (pMp) == true)
       sprintf (sRcv, "WBR hb");
     else
       sprintf (sRcv, "WBR unknown");
     break;
   case 0xF0:
     sprintf (sRcv, "FILL ");
     break;                             /* CasMp_Fill */
   default:
     sprintf (sRcv, "UNK unknown");
     break;                             /* unknown mini-packet */
  }
  pMsg = sMsg;
  pMsg += sprintf (pMsg, "%-8s %08X.%04X ", sRcv, nSclk, nRti);
  sprintf (sRcv, "0x%X", pRec->status.packet_length);
  pMsg += sprintf (pMsg, "%6s", sRcv);

  return sMsg;
}



char *CasMp_DumpHeader (CasRecord * pRec, FILE * h)
{
  static char sHdr[128];
  char *p;
  int i;
  Ulong nLen;

  nLen = CasMp_nHeaderLength (pRec);
  assert (nLen < 32);
  p = sHdr;
  for (i = 0; i < nLen; i++)
    p += sprintf (p, "%02X ", pRec->data[i]);
  assert ((strlen (sHdr) + 1) < 128);
  if (h != NULL)
    fprintf (h, "%s\n", sHdr);

  return sHdr;
}



void CasMp_Dump (CasRecord * pRec, FILE * h)
{
  char sRcv[32];
  Ulong i, nHdrLen;
  Ulong nMode, nCdsTime, nRti, nEvtTime, nPktLen;

  nMode = CasMp_nMode (pRec);
  nPktLen = pRec->status.packet_length;
  nCdsTime = pRec->status.cds_time;
  nRti = pRec->data[3];
  nRti <<= 8;
  nRti |= pRec->data[2];
  nEvtTime = GetEventTime (nCdsTime, nRti);
  nHdrLen = CasMp_nHeaderLength (pRec);
  switch (nMode & CasRcv_Mask) {
   case CasLfdr_Normal:
     strcpy (sRcv, "Lfdr");
     break;
   case CasMfdr_Normal:
     strcpy (sRcv, "Mfdr");
     break;
   case CasMfr_Normal:
     strcpy (sRcv, "Mfr");
     break;
   case CasHfr_Analysis:
     strcpy (sRcv, "Hfr Analysis");
     break;
   case CasHfr_Sounder:
     strcpy (sRcv, "Hfr Sounder");
     break;
   case CasHfr_Calibration:
     strcpy (sRcv, "Hfr Calibration");
     break;
   case CasHfr_Millisecond:
     strcpy (sRcv, "Hfr Millisecond");
     break;
   default:
     strcpy (sRcv, "unknown");
     break;
  }

  fprintf (h, "%s mode=%08X, nLen=%d(0x%X)\n", sRcv, nMode, nPktLen, nPktLen);
  fprintf (h, "  cds time=%08X, event time=%08X rti=%04X %s\n  ", nCdsTime,
           nEvtTime, nRti, 
		     CasSpice_nSclk_to_sScet (nEvtTime, (nRti << 5) & 0xFF, NULL));
  
  for (i = 0; i < nHdrLen; i++)
    fprintf (h, "%02X ", pRec->data[i]);
  fprintf (h, "\n");


  return;
}


/*
  char *sDataBase - name of the data base file to parse
  char *tBeg      - start time string "1999-230T00:00:00.000" 
  char *tEnd      - stop  time string "1999-230T23:59:59.999"
  char *sFiles[]  - list of files for the time interval, null terminated list
                     each pointer is allocated in the function and is up to 
                     the caller to free the memory.
  returns         - the number of files in the list
*/
Ulong CasMp_ParseDataBase (char *sDataBase, char *tBeg, char *tEnd,
                           char *sFiles[])
{
  /* Must use the environment varaible so that parallel installs
     on the same system can exist for testing -cwp */
  /* const char *sMpDb = "/opt/ project/ cassini/ data/CassiniJPL.db"; */
	
  char *p, sInLine[1024];
  char *pSclkStart, *pSclkStop, *pFileName;
  Ulong nTvalBeg, nTvalEnd, nTmp;
  Ulong nSclkBeg, nSclkEnd;
  Ulong nFiles = 0;
  FILE *h = NULL;


  if (sDataBase == NULL) {              /* function arg null, try enviroment variable */
    if ((p = getenv ("RPWS_MPDB")) != NULL) {
      sDataBase = malloc (strlen (p) + 1);
      strcpy (sDataBase, p);
    } else {
      /* Must use the env. variable no default value -cwp */
      fprintf(stderr, "Environment variable RPWS_MPDB is not set and no database "
              "file was specifed in the call to CasMp_ParseDataBase.  To "
				  "specifiy the mini-packet database, use a command line option or "
				  "set the enviroment variable RPWS_MPDB\n");
       exit (1);
    }
  }
  if ((h = fopen (sDataBase, "rt")) == NULL) {
    fprintf (stderr, "Unable to open database %s\n", sDataBase);
    fprintf (stderr,
             "To specifiy the mini-packet database,  use a command line option or\n");
    fprintf (stderr, "set the enviroment variable RPWS_MPDB\n");
    exit (1);
  }

  assert (h != NULL);
  CasSpice_sScet_to_nSclk (tBeg, &nTvalBeg, &nTmp);
  CasSpice_sScet_to_nSclk (tEnd, &nTvalEnd, &nTmp);


  /*
   * parse the file which looks like this 
   * 1997-003T03:15:46.000 1997-003T11:52:18.250  495ED880 495F5190  
   * /opt/ project/ cassini/ data/atlo/t19970030000.u00     0000FFFF 
   */

/*  fprintf(stderr,"Using Database %s\n",sDataBase); */

  while ((p = fgets (sInLine, 256, h)) != NULL) {

    /*
     * Scet Start Time 1997-298T00:00:00.000 
     */
    while (*p == ' ' || *p == '\t')
      ++p;
    /*
     * pScetStart=p;
     */
    while (*p != ' ' && *p != '\t' && *p != '\n' && *p != '\0')
      ++p;
    *p++ = '\0';

    /*
     * Scet Stop Time 1997-298T00:00:00.000 
     */
    while (*p == ' ' || *p == '\t')
      ++p;
    /*
     * pScetStop=p;
     */
    while (*p != ' ' && *p != '\t' && *p != '\n' && *p != '\0')
      ++p;
    *p++ = '\0';

    /*
     * Sclk Start 4ae8abcd 
     */
    while (*p == ' ' || *p == '\t')
      ++p;
    pSclkStart = p;
    while (*p != ' ' && *p != '\t' && *p != '\n' && *p != '\0')
      ++p;
    *p++ = '\0';

    /*
     * Sclk Stop 4ae8abcd 
     */
    while (*p == ' ' || *p == '\t')
      ++p;
    pSclkStop = p;
    while (*p != ' ' && *p != '\t' && *p != '\n' && *p != '\0')
      ++p;
    *p++ = '\0';

    /*
     * Filename /opt /project /cassini /data/t9729800_01.u00 
     */
    while (*p == ' ' || *p == '\t')
      ++p;
    pFileName = p;
    while (*p != ' ' && *p != '\t' && *p != '\n' && *p != '\0')
      ++p;
    *p++ = '\0';


    /*
     * Packet Content Bits 80001234 
     */
    while (*p == ' ' || *p == '\t')
      ++p;
    /*
     * pTypeBits=p;
     */
    while (*p != ' ' && *p != '\t' && *p != '\n' && *p != '\0')
      ++p;
    *p++ = '\0';

    nSclkBeg = (Ulong) strtoul (pSclkStart, NULL, 16);
    nSclkEnd = (Ulong) strtoul (pSclkStop, NULL, 16);

    /*
     * Desired time interval [t0,t1) ; File time interval [f0,f1)
     * You need the file if and only if (f0<t1) and (f1>t0)
     */
    if ((nSclkBeg < nTvalEnd) && (nSclkEnd > nTvalBeg)) {
      sFiles[nFiles] = malloc (strlen (pFileName) + 1);
      strcpy (sFiles[nFiles], pFileName);
      ++nFiles;
      sFiles[nFiles] = NULL;
    }

  }                                     /* while */


  return nFiles;
}
