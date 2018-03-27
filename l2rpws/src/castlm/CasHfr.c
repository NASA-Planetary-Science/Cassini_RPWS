
/*
  HFR Calibration Subroutines

Monday, March 24, 2003
  Revamp the whole hfr calibration routines.  The size and number of constants
have changes warrenting a significant rewrite.  Some of the hfr cal changes:
  1. No dBV0 - has been absorbed into the A1 constants.
  2. Addition of Ex+ and Ex- to dbCal_ABC
  3. Attenuator settings for all tables
  4. Phase Information

The calibration routines work as follows:
  I.  First a call to a sort routine - take mini packet data and breaks it
                                       out into the Autos, Agcs, Cross.
      input - standard rpws record
      output - ???
  II. Second a call to the cal routine, which works on the data from sort.

Possible Bugs

03-09-26
int CasHfrAnalysis_XtractAuto(CasRecord *pRec)
  from
    pEz->data[pEz->length+nStep+f]=*pData++;
  to
    pEz->data[pEz->length+nStep*nFilter+f]=*pData++;   raj 03-09-26   

03-09-26
int CasHfrAnalysis_InstrumentTiming(CasRecord *pRec)
  hf1&hf2 instrument timing stored wrong, nFilter*nStep offset problem
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>
#include <stdbool.h>

#include <rpwstlm/CasHfr.h>
#include "cal/CasHfrCal.h"

/*
 =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
                        start of the new stuff 
 =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
*/
#define HFR_MAX_SAMPLES 65536


typedef struct hfr_raw_data0
{
  unsigned char data[HFR_MAX_SAMPLES];
  Ulong length;
} HfrRawData0;

typedef struct hfr_raw_data
{
  HfrRawData0 Eu, Ev, Ex, Ez;
} HfrRawData1;


typedef struct hfr_cal_data0
{
  float data[HFR_MAX_SAMPLES];
  float time[HFR_MAX_SAMPLES];
  float freq[HFR_MAX_SAMPLES];
  Ulong length;
  Ulong nTmLen;
} HfrCalData0;

typedef struct hfr_cal_data
{
  HfrCalData0 Eu, Ev, Ex, Ez;
} HfrCalData1;

typedef struct hfr_analysis
{
  Ulong nFilters, nRepeat;
  HfrRawData1 Agc, Auto, Cross;
  HfrCalData1 dbAuto;                   /* insert Cross Real and Imag */
} HfrAnalysis;

HfrAnalysis A, B, C, HF1, HF2;

/* helper variables */
HfrRawData0 *pAGC[20], *pAUTO[20];
HfrCalData0 *pAUTO_MAG[20];

/* helper functions */
char *CasHfr_Idx2Str (int idx);

/* Program Control */
bool BALESE = false;

/*
 =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
                        end of the new stuff 
 =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
*/

void CasHfr_Init (void)
{
  int i, j, nIdx;


  /*
   * transform (tranpose) the dBcal matrixies 
   */
  for (i = 0; i < 8; i++) {
    for (j = 0; j < 12; j++)
      dBcalABC_08[j][i] = dbcal_08[i][j];
  }
  for (i = 0; i < 16; i++) {
    for (j = 0; j < 12; j++)
      dBcalABC_16[j][i] = dbcal_16[i][j];
  }
  for (i = 0; i < 32; i++) {
    for (j = 0; j < 12; j++)
      dBcalABC_32[j][i] = dbcal_32[i][j];
  }
  for (i = 0; i < 8; i++) {
    for (j = 0; j < 8; j++)
      dBcalHF[j][i] = dbcalhf[i][j];
  }


  /*
   * transform (transpose) the a1 hf matrixies 
   */
  for (i = 0; i < MAX_HF1_A1_CONSTANTS; i++) {
    for (j = 0; j < 8; j++)
      A1HF1[j][i] = A1hf1tmp[i][j];
  }

  /*
   * transform the a1 hf matrixies 
   */
  for (i = 0; i < MAX_HF2_A1_CONSTANTS; i++) {
    for (j = 0; j < 8; j++)
      A1HF2[j][i] = A1hf2tmp[i][j];
  }

  /*
   * Calculate Band ABC filter frequencies 
   */
  for (i = 0; i < 8; i++) {
    CasHfr_BandA_Freq[0][i] = (float)(3.6 * pow (4.5, (0.5 + i) / 8) * 1.0E3);
    CasHfr_BandB_Freq[0][i] = (float)(3.6 * pow (4.5, (0.5 + i + 8) / 8) * 1.0E3);
    CasHfr_BandC_Freq[0][i] = (float)(3.6 * pow (4.5, (0.5 + i + 16) / 8) * 1.0E3);
  }
  for (i = 0; i < 16; i++) {
    CasHfr_BandA_Freq[1][i] = (float)(3.6 * pow (4.5, (0.5 + i) / 16) * 1.0E3);
    CasHfr_BandB_Freq[1][i] = (float)(3.6 * pow (4.5, (0.5 + i + 16) / 16) * 1.0E3);
    CasHfr_BandC_Freq[1][i] = (float)(3.6 * pow (4.5, (0.5 + i + 32) / 16) * 1.0E3);
  }
  for (i = 0; i < 32; i++) {
    CasHfr_BandA_Freq[2][i] = (float)(3.6 * pow (4.5, (0.5 + i) / 32) * 1.0E3);
    CasHfr_BandB_Freq[2][i] = (float)(3.6 * pow (4.5, (0.5 + i + 32) / 32) * 1.0E3);
    CasHfr_BandC_Freq[2][i] = (float)(3.6 * pow (4.5, (0.5 + i + 64) / 32) * 1.0E3);
  }



  nIdx = 0;
  pAGC[nIdx] = &(A.Agc.Eu);
  pAUTO[nIdx] = &(A.Auto.Eu);
  ++nIdx;
  pAGC[nIdx] = &(A.Agc.Ev);
  pAUTO[nIdx] = &(A.Auto.Ev);
  ++nIdx;
  pAGC[nIdx] = &(A.Agc.Ex);
  pAUTO[nIdx] = &(A.Auto.Ex);
  ++nIdx;
  pAGC[nIdx] = &(A.Agc.Ez);
  pAUTO[nIdx] = &(A.Auto.Ez);
  ++nIdx;

  pAGC[nIdx] = &(B.Agc.Eu);
  pAUTO[nIdx] = &(B.Auto.Eu);
  ++nIdx;
  pAGC[nIdx] = &(B.Agc.Ev);
  pAUTO[nIdx] = &(B.Auto.Ev);
  ++nIdx;
  pAGC[nIdx] = &(B.Agc.Ex);
  pAUTO[nIdx] = &(B.Auto.Ex);
  ++nIdx;
  pAGC[nIdx] = &(B.Agc.Ez);
  pAUTO[nIdx] = &(B.Auto.Ez);
  ++nIdx;

  pAGC[nIdx] = &(C.Agc.Eu);
  pAUTO[nIdx] = &(C.Auto.Eu);
  ++nIdx;
  pAGC[nIdx] = &(C.Agc.Ev);
  pAUTO[nIdx] = &(C.Auto.Ev);
  ++nIdx;
  pAGC[nIdx] = &(C.Agc.Ex);
  pAUTO[nIdx] = &(C.Auto.Ex);
  ++nIdx;
  pAGC[nIdx] = &(C.Agc.Ez);
  pAUTO[nIdx] = &(C.Auto.Ez);
  ++nIdx;

  pAGC[nIdx] = &(HF1.Agc.Eu);
  pAUTO[nIdx] = &(HF1.Auto.Eu);
  ++nIdx;
  pAGC[nIdx] = &(HF1.Agc.Ev);
  pAUTO[nIdx] = &(HF1.Auto.Ev);
  ++nIdx;
  pAGC[nIdx] = &(HF1.Agc.Ex);
  pAUTO[nIdx] = &(HF1.Auto.Ex);
  ++nIdx;
  pAGC[nIdx] = &(HF1.Agc.Ez);
  pAUTO[nIdx] = &(HF1.Auto.Ez);
  ++nIdx;

  pAGC[nIdx] = &(HF2.Agc.Eu);
  pAUTO[nIdx] = &(HF2.Auto.Eu);
  ++nIdx;
  pAGC[nIdx] = &(HF2.Agc.Ev);
  pAUTO[nIdx] = &(HF2.Auto.Ev);
  ++nIdx;
  pAGC[nIdx] = &(HF2.Agc.Ex);
  pAUTO[nIdx] = &(HF2.Auto.Ex);
  ++nIdx;
  pAGC[nIdx] = &(HF2.Agc.Ez);
  pAUTO[nIdx] = &(HF2.Auto.Ez);
  ++nIdx;


  nIdx = 0;
  pAUTO_MAG[nIdx] = &(A.dbAuto.Eu);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(A.dbAuto.Ev);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(A.dbAuto.Ex);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(A.dbAuto.Ez);
  ++nIdx;

  pAUTO_MAG[nIdx] = &(B.dbAuto.Eu);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(B.dbAuto.Ev);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(B.dbAuto.Ex);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(B.dbAuto.Ez);
  ++nIdx;

  pAUTO_MAG[nIdx] = &(C.dbAuto.Eu);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(C.dbAuto.Ev);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(C.dbAuto.Ex);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(C.dbAuto.Ez);
  ++nIdx;

  pAUTO_MAG[nIdx] = &(HF1.dbAuto.Eu);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(HF1.dbAuto.Ev);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(HF1.dbAuto.Ex);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(HF1.dbAuto.Ez);
  ++nIdx;

  pAUTO_MAG[nIdx] = &(HF2.dbAuto.Eu);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(HF2.dbAuto.Ev);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(HF2.dbAuto.Ex);
  ++nIdx;
  pAUTO_MAG[nIdx] = &(HF2.dbAuto.Ez);
  ++nIdx;


  return;
}



Ulong CasHfr_nMode (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  Ulong nMode;

  if (CasHfr_bAnalysis (pMp) == true) {
    nMode = CasHfr_Analysis;
    if (CasHfr_bBandASelected (pMp) == true)
      nMode |= CasHfr_BandA;
    if (CasHfr_bBandBSelected (pMp) == true)
      nMode |= CasHfr_BandB;
    if (CasHfr_bBandCSelected (pMp) == true)
      nMode |= CasHfr_BandC;
    if (nMode & CasHfr_BandABC) {
      if (CasHfr_bDirectionFindingABC (pMp) == true) {
        nMode |= (CasAntEu | CasAntEv | CasAntEw);
      } else {
        if (CasHfr_bEuOnABC (pMp) == true)
          nMode |= CasAntEu;
        else if (CasHfr_bEvOnABC (pMp) == true)
          nMode |= CasAntEv;
        else if (CasHfr_bExOnABC (pMp) == true)
          nMode |= CasAntEx;
        else
          nMode |= CasAntNone;
        if (CasHfr_bEwOnABC (pMp) == true)
          nMode |= CasAntEw;
        else
          nMode |= CasAntNone;
      }
    }
    /*
     * hfr band a,b,c 
     */
    if (CasHfr_bBandHF1Selected (pMp) == true) {
      nMode |= CasHfr_BandHF1;
      if (CasHfr_bDirectionFindingHF1 (pMp) == true) {
        nMode |= (CasAntEu | CasAntEv | CasAntEw);
      } else {
        if (CasHfr_bEuOnHF1 (pMp) == true)
          nMode |= CasAntEu;
        else if (CasHfr_bEvOnHF1 (pMp) == true)
          nMode |= CasAntEv;
        else if (CasHfr_bExOnHF1 (pMp) == true)
          nMode |= CasAntEx;
        else
          nMode |= CasAntNone;
        if (CasHfr_bEwOnHF1 (pMp) == true)
          nMode |= CasAntEw;
        else
          nMode |= CasAntNone;
      }
    }
    /*
     * hfr band hf1 
     */
    if (CasHfr_bBandHF2Selected (pMp) == true) {
      nMode |= CasHfr_BandHF2;
      if (CasHfr_bDirectionFindingHF2 (pMp) == true) {
        nMode |= (CasAntEu | CasAntEv | CasAntEw);
      } else {
        if (CasHfr_bEuOnHF2 (pMp) == true)
          nMode |= CasAntEu;
        else if (CasHfr_bEvOnHF2 (pMp) == true)
          nMode |= CasAntEv;
        else if (CasHfr_bExOnHF2 (pMp) == true)
          nMode |= CasAntEx;
        else
          nMode |= CasAntNone;
        if (CasHfr_bEwOnHF2 (pMp) == true)
          nMode |= CasAntEw;
        else
          nMode |= CasAntNone;
      }
    }
    /*
     * hfr band hf2 
     */
    if (pMp[24] != 0xAA)                /* header not terminated with AA */
      nMode |= CasHfr_BadPacket;

  } /* hfr analysis mode */
  else if (CasHfr_bSounder (pMp) == true) {
    nMode = CasHfr_Sounder;

  } else if (CasHfr_bCalibration (pMp) == true) {
    nMode = CasHfr_Calibration;

  } else if (CasHfr_bMillisecond (pMp) == true) {
    nMode = CasHfr_Millisecond;

    if (CasHfrMsc_bHF1 (pMp) == true)
      nMode |= CasHfr_BandHF1;
    else if (CasHfrMsc_bHF2 (pMp) == true)
      nMode |= CasHfr_BandHF2;
    else
      assert (0);

    if (CasHfrMsc_bEwOn (pMp) == true)
      nMode |= CasAntEw;
    else if (CasHfrMsc_bEuOn (pMp) == true)
      nMode |= CasAntEu;
    else if (CasHfrMsc_bEvOn (pMp) == true)
      nMode |= CasAntEv;
    else if (CasHfrMsc_bExOn (pMp) == true)
      nMode |= CasAntEx;
    else
      assert (0);

  } else {
    nMode = 0x00000000;
    assert (0);
  }

  return nMode;
}



char *CasHfrAnalysis_sByteCount (CasRecord * pRec)
{
  unsigned char *pMp = pRec->data;
  char arBandABC[16], arAntABC[16];
  char arAntHF1[16], arAntHF2[16], *pBuf;
  ULONG nAgcABC, nAutoABC, nCrossABC;
  ULONG nAgcHF1, nAutoHF1, nCrossHF1;
  ULONG nAgcHF2, nAutoHF2, nCrossHF2, nTotal;
  static char sBuf[8192];
  const char *sBndFmt = "%-4s  %-8s  %3d  %6d  %8.3f  %3d  %4d  %5d  %5d\n";
  const char *sTotFmt = "%37s  %3d  %4d  %5d  %5d(0x%X)\n";


  sBuf[0] = '\0';
  if (CasHfr_bHfrPacket (pMp) == false)
    return sBuf;
  if (CasHfr_bAnalysis (pMp) == false)
    return sBuf;



  arBandABC[0] = '\0';
  if (CasHfr_bBandASelected (pMp) == true)
    strcat (arBandABC, "A");
  if (CasHfr_bBandBSelected (pMp) == true)
    strcat (arBandABC, "B");
  if (CasHfr_bBandCSelected (pMp) == true)
    strcat (arBandABC, "C");
  if (arBandABC[0] == '\0')
    strcpy (arBandABC, "Off");

  if (CasHfr_bDirectionFindingABC (pMp) == true)
    strcpy (arAntABC, "df");
  else {
    if (CasHfr_bExOnABC (pMp) == true)
      strcpy (arAntABC, "Ex ");
    else if (CasHfr_bEuOnABC (pMp) == true)
      strcpy (arAntABC, "Eu ");
    else if (CasHfr_bEvOnABC (pMp) == true)
      strcpy (arAntABC, "Ev ");
    else
      strcpy (arAntABC, "");
    if (CasHfr_bEzOnABC (pMp) == true)
      strcat (arAntABC, "Ez");
  }

  if (CasHfr_bDirectionFindingHF1 (pMp) == true)
    strcpy (arAntHF1, "df");
  else {
    if (CasHfr_bExOnHF1 (pMp) == true)
      strcpy (arAntHF1, "Ex ");
    else if (CasHfr_bEuOnHF1 (pMp) == true)
      strcpy (arAntHF1, "Eu ");
    else if (CasHfr_bEvOnHF1 (pMp) == true)
      strcpy (arAntHF1, "Ev ");
    else
      strcpy (arAntHF1, "");
    if (CasHfr_bEzOnHF1 (pMp) == true)
      strcat (arAntHF1, "Ez");
  }

  if (CasHfr_bDirectionFindingHF2 (pMp) == true)
    strcpy (arAntHF2, "df");
  else {
    if (CasHfr_bExOnHF2 (pMp) == true)
      strcpy (arAntHF2, "Ex ");
    else if (CasHfr_bEuOnHF2 (pMp) == true)
      strcpy (arAntHF2, "Eu ");
    else if (CasHfr_bEvOnHF2 (pMp) == true)
      strcpy (arAntHF2, "Ev ");
    else
      strcpy (arAntHF2, "");
    if (CasHfr_bEzOnHF2 (pMp) == true)
      strcat (arAntHF2, "Ez");
  }


  nAgcABC = CasHfrAnalysis_AgcByteCount (pRec, CasHfr_BandABC | CasAntAll);
  nAgcHF1 = CasHfrAnalysis_AgcByteCount (pRec, CasHfr_BandHF1 | CasAntAll);
  nAgcHF2 = CasHfrAnalysis_AgcByteCount (pRec, CasHfr_BandHF2 | CasAntAll);

  nAutoABC = CasHfrAnalysis_AutoByteCount (pRec, CasHfr_BandABC | CasAntAll);
  nAutoHF1 = CasHfrAnalysis_AutoByteCount (pRec, CasHfr_BandHF1 | CasAntAll);
  nAutoHF2 = CasHfrAnalysis_AutoByteCount (pRec, CasHfr_BandHF2 | CasAntAll);

  nCrossABC =
    CasHfrAnalysis_CrossByteCount (pRec, CasHfr_BandABC | CasAntAll);
  nCrossHF1 =
    CasHfrAnalysis_CrossByteCount (pRec, CasHfr_BandHF1 | CasAntAll);
  nCrossHF2 =
    CasHfrAnalysis_CrossByteCount (pRec, CasHfr_BandHF2 | CasAntAll);

  nTotal = nAgcABC + nAgcHF1 + nAgcHF2;
  nTotal += nAutoABC + nAutoHF1 + nAutoHF2;
  nTotal += nCrossABC + nCrossHF1 + nCrossHF2;

  pBuf = sBuf;
  pBuf += sprintf (pBuf, "Packet Size=%d(0x%X), RTI=%04X, Meander=%s, "
                   "Ver=%X, Dump=%s ErrFlag=%s\n",
                   CasHfr_nPacketSize (pMp), CasHfr_nPacketSize (pMp),
                   CasHfr_RTI (pMp),
                   CasHfr_bMeanderCompressed (pMp) == true ? "True" : "False",
                   CasHfr_nHeaderVersion (pMp),
                   CasHfr_bDumpMemory (pMp) == true ? "True" : "False",
                   CasHfr_bErrorFlag (pMp) == true ? "True" : "False");

  pBuf +=
    sprintf (pBuf,
             "Band  Antennas  Flt  Repeat  Int(Sec)  Agc  Auto  Cross  Total\n");

  pBuf += sprintf (pBuf, sBndFmt,
                   arBandABC, arAntABC, CasHfr_nFiltersABC (pMp),
                   CasHfr_nRepeatCountABC (pMp) *
                   CasHfr_nRepeatCountAll (pMp),
                   CasHfr_fIntegrationTimeABC (pMp), nAgcABC, nAutoABC,
                   nCrossABC, nAgcABC + nAutoABC + nCrossABC);

  pBuf += sprintf (pBuf, sBndFmt,
                   CasHfr_bBandHF1Selected (pMp) == true ? "HF1" : "Off",
                   arAntHF1, CasHfr_nFiltersHF1 (pMp),
                   CasHfr_nRepeatCountABC (pMp) *
                   CasHfr_nRepeatCountAll (pMp),
                   CasHfr_fIntegrationTimeHF1 (pMp), nAgcHF1, nAutoHF1,
                   nCrossHF1, nAgcHF1 + nAutoHF1 + nCrossHF1);

  pBuf += sprintf (pBuf, sBndFmt,
                   CasHfr_bBandHF2Selected (pMp) == true ? "HF2" : "Off",
                   arAntHF2, CasHfr_nFiltersHF2 (pMp),
                   CasHfr_nRepeatCountHF2 (pMp) *
                   CasHfr_nRepeatCountAll (pMp),
                   CasHfr_fIntegrationTimeHF2 (pMp), nAgcHF2, nAutoHF2,
                   nCrossHF2, nAgcHF2 + nAutoHF2 + nCrossHF2);

  pBuf += sprintf (pBuf, sTotFmt, " ",
                   nAgcABC + nAgcHF1 + nAgcHF2,
                   nAutoABC + nAutoHF1 + nAutoHF2,
                   nCrossABC + nCrossHF1 + nCrossHF2,
                   nAgcABC + nAgcHF1 + nAgcHF2 +
                   nAutoABC + nAutoHF1 + nAutoHF2 +
                   nCrossABC + nCrossHF1 + nCrossHF2,
                   nAgcABC + nAgcHF1 + nAgcHF2 +
                   nAutoABC + nAutoHF1 + nAutoHF2 +
                   nCrossABC + nCrossHF1 + nCrossHF2);

  pBuf +=
    sprintf (pBuf,
             "\tExpected + Header + 5a = %d + %d + %d = %d(0x%X)  "
             "hfr=%d(0x%X)\n", nTotal, 25, 1, nTotal + 25 + 1,
             nTotal + 25 + 1, nTotal + 25 + 1 - 7, nTotal + 25 + 1 - 7);



  return sBuf;
}

Ulong CasHfr_GetAgcByteCount (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  Ulong nRepeat, nStep, nAgc, nBand;
  Ulong nAgcABC, nAgcHF1, nAgcHF2;



  /*
   * Band ABC Agc Count 
   */
  nAgcABC = 0;
  if (CasHfr_bDirectionFindingABC (pMp) == true)
    nAgc = 4;                           /* Eu,Ev,Ez,Ez */
  else {                                /* Eu,Ev,Ex should be mutually exclusive */
    nAgc = 0;
    if (CasHfr_bEuOnABC (pMp) == true)
      ++nAgc;
    if (CasHfr_bEvOnABC (pMp) == true)
      ++nAgc;
    if (CasHfr_bExOnABC (pMp) == true)
      ++nAgc;
    if (CasHfr_bEzOnABC (pMp) == true)
      ++nAgc;
  }
  nBand = 0;
  if (CasHfr_bBandASelected (pMp) == true)
    ++nBand;
  if (CasHfr_bBandBSelected (pMp) == true)
    ++nBand;
  if (CasHfr_bBandCSelected (pMp) == true)
    ++nBand;
  nRepeat = ((Ulong)CasHfr_nRepeatCountABC (pMp)) * ((Ulong)CasHfr_nRepeatCountAll (pMp));
  nAgcABC = nAgc * nBand * nRepeat;

  /*
   * Band HF1 Agc Count 
   */
  nAgcHF1 = 0;
  if (CasHfr_bBandHF1Selected (pMp) == true) {
    if (CasHfr_bDirectionFindingHF1 (pMp) == true)
      nAgc = 4;
    else {
      nAgc = 0;
      if (CasHfr_bEuOnHF1 (pMp) == true)
        ++nAgc;
      if (CasHfr_bEvOnHF1 (pMp) == true)
        ++nAgc;
      if (CasHfr_bExOnHF1 (pMp) == true)
        ++nAgc;
      if (CasHfr_bEzOnHF1 (pMp) == true)
        ++nAgc;
    }
    nStep = CasHfr_nNumberStepsHF1 (pMp);
    nRepeat = CasHfr_nRepeatCountHF1 (pMp) * CasHfr_nRepeatCountAll (pMp);
    nAgcHF1 = nAgc * nStep * nRepeat;
  }

  /*
   * Band HF2 Agc Count 
   */
  nAgcHF2 = 0;
  if (CasHfr_bBandHF2Selected (pMp) == true) {
    if (CasHfr_bDirectionFindingHF2 (pMp) == true)
      nAgc = 4;
    else {
      nAgc = 0;
      if (CasHfr_bEuOnHF2 (pMp) == true)
        ++nAgc;
      if (CasHfr_bEvOnHF2 (pMp) == true)
        ++nAgc;
      if (CasHfr_bExOnHF2 (pMp) == true)
        ++nAgc;
      if (CasHfr_bEzOnHF2 (pMp) == true)
        ++nAgc;
    }
    nStep = CasHfr_nNumberStepsHF2 (pMp);
    nRepeat = CasHfr_nRepeatCountHF2 (pMp) * CasHfr_nRepeatCountAll (pMp);
    nAgcHF2 = nAgc * nStep * nRepeat;
  }

  nAgc = nAgcABC + nAgcHF1 + nAgcHF2;



  return nAgc;
}



void CasHfr_DumpRecord (CasRecord * pRec)
{
  unsigned char *pMp = pRec->data;
  Ulong nType, nMpLen, nPktLen, nRti, nSclk;

  nType = (pMp[0] >> 4) & 0x0F;
  nRti = pMp[3];
  nRti <<= 8;
  nRti |= pMp[2];
  nMpLen = pMp[0];
  nMpLen <<= 8;
  nMpLen |= pMp[1];
  nMpLen &= 0x0FFF;
  nSclk = pRec->status.cds_time;
  nPktLen = pRec->status.packet_length;
  fprintf (stderr, "%X %04X %08X mplen=%d pktlen=%d\n", nType, nRti,
           nSclk, nMpLen, nPktLen);

  return;
}



void CasHfr_DumpCalConstants (FILE * h)
{
  int i;
  float *p;

  fprintf (h, "dBcal ABC 8 filters\n");
  for (i = 0; i < 12; i++) {
    p = dBcalABC_08[i];
    fprintf (h, " %7.2f %7.2f %7.2f %7.2f ", p[0], p[1], p[2], p[3]);
    fprintf (h, "... %7.2f %7.2f %7.2f %7.2f", p[4], p[5], p[6], p[7]);
    fprintf (h, "\n");
  }
  fprintf (h, "dBcal ABC 16 filters\n");
  for (i = 0; i < 12; i++) {
    p = dBcalABC_16[i];
    fprintf (h, " %7.2f %7.2f %7.2f %7.2f ", p[0], p[1], p[2], p[3]);
    fprintf (h, "... %7.2f %7.2f %7.2f %7.2f", p[12], p[13], p[14], p[15]);
    fprintf (h, "\n");
  }
  fprintf (h, "dBcal ABC 32 filters\n");
  for (i = 0; i < 12; i++) {
    p = dBcalABC_32[i];
    fprintf (h, " %7.2f %7.2f %7.2f %7.2f ", p[0], p[1], p[2], p[3]);
    fprintf (h, "... %7.2f %7.2f %7.2f %7.2f", p[28], p[29], p[30], p[31]);
    fprintf (h, "\n");
  }
  fprintf (h, "dBcal HF 1,2,4,8 filters\n");
  for (i = 0; i < 8; i++) {
    p = dBcalHF[i];
    fprintf (h, " %7.2f %7.2f %7.2f %7.2f ", p[0], p[1], p[2], p[3]);
    fprintf (h, "... %7.2f %7.2f %7.2f %7.2f", p[4], p[5], p[6], p[7]);
    fprintf (h, "\n");
  }

  fprintf (h, "A1 HF1\n");
  for (i = 0; i < 8; i++) {
    p = A1HF1[i];
    fprintf (h, " %7.2f %7.2f %7.2f %7.2f ", p[0], p[1], p[2], p[3]);
    fprintf (h, "... %7.2f %7.2f %7.2f %7.2f", p[169], p[170], p[171],
             p[172]);
    fprintf (h, "\n");
  }

  fprintf (h, "A1 HF2\n");
  for (i = 0; i < 8; i++) {
    p = A1HF2[i];
    fprintf (h, " %7.2f %7.2f %7.2f %7.2f ", p[0], p[1], p[2], p[3]);
    fprintf (h, "... %7.2f %7.2f %7.2f %7.2f", p[318], p[319], p[320],
             p[321]);
    fprintf (h, "\n");
  }

  return;
}



void CasHfr_DumpAgc (void)
{
  int i, nIdx;
  HfrRawData0 *pAgc;


  for (nIdx = 0; nIdx < 20; nIdx++) {
    pAgc = pAGC[nIdx];
    if (pAgc->length > 0) {
      fprintf (stderr, "Agc %s (%d)", CasHfr_Idx2Str (nIdx), pAgc->length);
      for (i = 0; i < pAgc->length; i++) {
        if (!(i % 20))
          fprintf (stderr, "\n     ");
        fprintf (stderr, "%02X ", pAgc->data[i]);
        if (!((i + 1) % 5) && (i % 20))
          fprintf (stderr, "  ");
      }
      fprintf (stderr, "\n");
    }
  }



  return;
}


void CasHfr_DumpAuto (void)
{
  int i, nIdx;
  HfrRawData0 *pAuto;


  for (nIdx = 0; nIdx < 20; nIdx++) {
    pAuto = pAUTO[nIdx];
    if (pAuto->length > 0) {
      fprintf (stderr, "Auto %s (%d)", CasHfr_Idx2Str (nIdx), pAuto->length);
      for (i = 0; i < pAuto->length; i++) {
        if (!(i % 20))
          fprintf (stderr, "\n     ");
        fprintf (stderr, "%02X ", pAuto->data[i]);
        if (!((i + 1) % 5) && (i % 20))
          fprintf (stderr, "  ");
      }
      fprintf (stderr, "\n");
    }
  }



  return;
}



void CasHfr_DumpAutoMag (void)
{
  int i, nIdx;
  HfrCalData0 *pAutoMag;



  for (nIdx = 0; nIdx < 20; nIdx++) {
    pAutoMag = pAUTO_MAG[nIdx];
    if (pAutoMag->length > 0) {
      fprintf (stderr, "AutoMag %s (%d)", CasHfr_Idx2Str (nIdx),
               pAutoMag->length);
      for (i = 0; i < pAutoMag->length; i++) {
        if (!(i % 8))
          fprintf (stderr, "\n     ");
        fprintf (stderr, "%8.3f ", pAutoMag->data[i]);
        if (!((i + 1) % 4) && (i % 8))
          fprintf (stderr, "  ");
      }
      fprintf (stderr, "\n");
    }
  }



  return;
}



char *CasHfr_Idx2Str (int idx)
{
  static char str[8];

  switch (idx) {
   case 0:
     strcpy (str, "AEu");
     break;
   case 1:
     strcpy (str, "AEv");
     break;
   case 2:
     strcpy (str, "AEx");
     break;
   case 3:
     strcpy (str, "AEz");
     break;

   case 4:
     strcpy (str, "BEu");
     break;
   case 5:
     strcpy (str, "BEv");
     break;
   case 6:
     strcpy (str, "BEx");
     break;
   case 7:
     strcpy (str, "BEz");
     break;

   case 8:
     strcpy (str, "CEu");
     break;
   case 9:
     strcpy (str, "CEv");
     break;
   case 10:
     strcpy (str, "CEx");
     break;
   case 11:
     strcpy (str, "CEz");
     break;

   case 12:
     strcpy (str, "HF1Eu");
     break;
   case 13:
     strcpy (str, "HF1Ev");
     break;
   case 14:
     strcpy (str, "HF1Ex");
     break;
   case 15:
     strcpy (str, "HF1Ez");
     break;

   case 16:
     strcpy (str, "HF2Eu");
     break;
   case 17:
     strcpy (str, "HF2Ev");
     break;
   case 18:
     strcpy (str, "HF2Ex");
     break;
   case 19:
     strcpy (str, "HF2Ez");
     break;

   default:
     strcpy (str, "none");
     break;

  }

  return str;
}



/* 
  When a new mini packet is read in, the lengths of the calibrated should
get set to zero, implying a no calibration has been performed 
*/
Ulong CasHfrAnalysis_CalibrateAuto (CasRecord * pRec)
{
  unsigned char *pMp = pRec->data;
  bool bAttnOn = false, bDirFnd = false, bExAnt = false, bHf1 = false;
  int nLst, nIdx, r, s, f;
  int nFilter, nStep, nRepeat, nStepBeg, nStepSize;

  Ulong nAuto;
  float A1, A2, A3, fAgc;
  float *pdBcal, *pA1, arA1[1024];      /* A1 constants 322 max */

  float fK, fBw, *pFrq;                 /* pointer to Bands ABC Frequency Table */
  float arStep[1024];                   /* step freq 322 max */

  Ulong nLength = 0;
  HfrRawData0 *pAgc, *pAuto;
  HfrCalData0 *pAutoMag;

  bAttnOn = CasHfr_bAttnOn (pMp);       /* hfr 30dB internal attenuator */

  /*
   * calibrate bands abc 
   */
  nFilter = CasHfr_nFiltersABC (pMp);
  nStep = 0;
  nRepeat = CasHfr_nRepeatCountABC (pMp) * CasHfr_nRepeatCountAll (pMp);
  bDirFnd = CasHfr_bDirectionFindingABC (pMp);  /* special case for Ez */
  for (nLst = 0; nLst < 12; nLst++) {   /* add one to nLst and quit if it's twelve */
    pAgc = pAGC[nLst];
    pAuto = pAUTO[nLst];
    pAutoMag = pAUTO_MAG[nLst];
    if (pAgc->length != 0) {
      /*
       * for dir finding, ez will have 2x as much as eu and ev 
       */
      nRepeat = CasHfr_nRepeatCountABC (pMp) * CasHfr_nRepeatCountAll (pMp);

      /*
       * A1,A2,A3 constants for Bands A,B,C 
       */
      switch (nLst) {
       case 0:
       case 1:
       case 2:                         /* Band A Eu,Ev,Ex */
         A1 = A123[0][0];
         A2 = A123[0][1];
         A3 = A123[0][2];
         if (bAttnOn == true)
           A1 = A123[0][4];
         pFrq = CasHfr_BandA_Freq[0];
         break;
       case 3:                         /* Band A Ez */
         A1 = A123[1][0];
         A2 = A123[1][1];
         A3 = A123[1][2];
         if (bAttnOn == true)
           A1 = A123[1][4];
         if (bDirFnd == true)
           nRepeat *= 2;
         pFrq = CasHfr_BandA_Freq[0];
         break;
       case 4:
       case 5:
       case 6:                         /* Band B Eu,Ev,Ex */
         A1 = A123[2][0];
         A2 = A123[2][1];
         A3 = A123[2][2];
         if (bAttnOn == true)
           A1 = A123[2][4];
         pFrq = CasHfr_BandB_Freq[0];
         break;
       case 7:                         /* Band B Ez */
         A1 = A123[3][0];
         A2 = A123[3][1];
         A3 = A123[3][2];
         if (bAttnOn == true)
           A1 = A123[3][4];
         if (bDirFnd == true)
           nRepeat *= 2;
         pFrq = CasHfr_BandB_Freq[0];
         break;
       case 8:
       case 9:
       case 10:                        /* Band C Eu,Ev,Ex */
         A1 = A123[4][0];
         A2 = A123[4][1];
         A3 = A123[4][2];
         if (bAttnOn == true)
           A1 = A123[4][4];
         pFrq = CasHfr_BandC_Freq[0];
         break;
       case 11:                        /* Band C Ez */
         A1 = A123[5][0];
         A2 = A123[5][1];
         A3 = A123[5][2];
         if (bAttnOn == true)
           A1 = A123[5][4];
         if (bDirFnd == true)
           nRepeat *= 2;
         pFrq = CasHfr_BandC_Freq[0];
         break;
       default:
         assert (0);
         break;
      }                                 /* A1,A2,A3 constants for Bands A,B,C */
      A2 = 1.0 / A2;                    /* invert to save a divide */
      if (nFilter == 8) {
        pdBcal = dBcalABC_08[nLst];
      } else if (nFilter == 16) {
        pdBcal = dBcalABC_16[nLst];
        pFrq += 32;
      } else if (nFilter == 32) {
        pdBcal = dBcalABC_32[nLst];
        pFrq += 64;
      } else
        assert (0);

      /*
       * Sanity check 
       */
      if (pAgc->length != nRepeat) {
        fprintf (stderr, "%s agc len=%d, repeat=%d\n", CasHfr_Idx2Str (nLst),
                 pAgc->length, nRepeat);
        CasHfr_DumpRecord (pRec);
        fprintf (stderr, "agc=%d, auto=%d\n", pAgc->length, pAuto->length);

      }
      assert (pAgc->length == nRepeat);

      nIdx = 0;
      for (r = 0; r < nRepeat; r++) {
        fAgc = (pAgc->data[r] - A3) * A2;
        fAgc = pow (10.0, fAgc);
        fAgc = 40.0 * log10 (fAgc + 1.0);
        fAgc = fAgc - A1;

        if (pAuto->length != 0) {
          for (f = 0; f < nFilter; f++) {
            nAuto = pAuto->data[nIdx];
            nAuto = (1 << (nAuto >> 3)) * ((nAuto & 0x07) + 8);
            pAutoMag->data[nIdx] = 10 * log10 ((double) nAuto);
            pAutoMag->data[nIdx] += fAgc;
            pAutoMag->data[nIdx] -= pdBcal[f];  /* dBVrms/rtHz */
            pAutoMag->freq[nIdx] = pFrq[f];
            ++nIdx;
          }
        } /* if auto data */
        else {
          pAutoMag->data[nIdx] = fAgc;
          /*
           * average n/2 & (n/2)-1 : 8, 16, or 32 filters 
           */
          pAutoMag->freq[nIdx] =
            (pFrq[nFilter / 2] + pFrq[(nFilter / 2) - 1]) / 2.0;
          ++nIdx;
        }                               /* else agc only */

      }                                 /* for repeat cycle */
      pAutoMag->length = nIdx;
      nLength += nIdx;
    }                                   /* if nLst data */
  }                                     /* scan the ABC agc matrixes */


  /*
   * calibrate hf1/hf2 data 
   */
  for (nLst = 12; nLst < 20; nLst++) {  /* add one to nLst and quit if it's twenty */
    pAgc = pAGC[nLst];
    pAuto = pAUTO[nLst];
    pAutoMag = pAUTO_MAG[nLst];
    if (pAgc->length != 0) {

      if (nLst < 16) {                  /* hf1 */
        nFilter = CasHfr_nFiltersHF1 (pMp);
        nStep = CasHfr_nNumberStepsHF1 (pMp);
        nRepeat = CasHfr_nRepeatCountHF1 (pMp) * CasHfr_nRepeatCountAll (pMp);
        bDirFnd = CasHfr_bDirectionFindingHF1 (pMp);    /* special case for Ez */
        nStepBeg = CasHfr_nStartFrequencyHF1 (pMp);
        nStepSize = CasHfr_nFrequencyStepHF1 (pMp);
        fBw = 25.0E3;
        bHf1 = true;
      } else {                          /* hf2 */
        nFilter = CasHfr_nFiltersHF2 (pMp);
        nStep = CasHfr_nNumberStepsHF2 (pMp);
        nRepeat = CasHfr_nRepeatCountHF2 (pMp) * CasHfr_nRepeatCountAll (pMp);
        bDirFnd = CasHfr_bDirectionFindingHF2 (pMp);    /* special case for Ez */
        nStepBeg = CasHfr_nStartFrequencyHF2 (pMp);
        nStepSize = CasHfr_nFrequencyStepHF2 (pMp);
        fBw = 50.0E3;
        bHf1 = false;
      }

      /*
       * A1 constants for Bands HF1,HF2 
       */
      switch (nLst) {
       case 12:                        /* HF1 Eu */
         if (bAttnOn == false)
           pA1 = A1HF1[0];
         else
           pA1 = A1HF1[4];
         bExAnt = true;
         break;
       case 13:                        /* HF1 Ev */
         if (bAttnOn == false)
           pA1 = A1HF1[1];
         else
           pA1 = A1HF1[5];
         bExAnt = true;
         break;
       case 14:                        /* HF1 Ex */
         if (bAttnOn == false)
           pA1 = A1HF1[2];
         else
           pA1 = A1HF1[6];
         bExAnt = true;
         break;
       case 15:                        /* HF1 Ez */
         bExAnt = false;
         if (bAttnOn == false)
           pA1 = A1HF1[3];
         else
           pA1 = A1HF1[7];
         if (bDirFnd == true)
           nRepeat *= 2;
         break;
       case 16:                        /* HF2 Eu */
         if (bAttnOn == false)
           pA1 = A1HF2[0];
         else
           pA1 = A1HF2[4];
         bExAnt = true;
         break;
       case 17:                        /* HF2 Ev */
         if (bAttnOn == false)
           pA1 = A1HF2[1];
         else
           pA1 = A1HF2[5];
         bExAnt = true;
         break;
       case 18:                        /* HF2 Ex */
         if (bAttnOn == false)
           pA1 = A1HF2[2];
         else
           pA1 = A1HF2[6];
         bExAnt = true;
         break;
       case 19:                        /* HF2 Ez */
         bExAnt = false;
         if (bAttnOn == false)
           pA1 = A1HF2[3];
         else
           pA1 = A1HF2[7];
         if (bDirFnd == true)
           nRepeat *= 2;
         break;
       default:
         assert (0);
         break;
      }                                 /* A1 constants for Bands HF */
      assert (nStep < 1024);            /* array overrun, bounds check A1 constants below */
      for (s = 0; s < nStep; s++) {
        arA1[s] = pA1[nStepBeg + s * nStepSize];
        arStep[s] = (nStepBeg + s * nStepSize) * fBw;
      }
      if (bHf1 == false) {              /* hf2 has a 25KHz offset */
        for (s = 0; s < nStep; s++)
          arStep[s] += 25.0E3;
      }
      /*
       * hfr hf bands, from time to time, has undefined freq steps 
       */
      s = nStepBeg + (nStep - 1) * nStepSize;
      if ((bHf1 == true) && (s >= MAX_HF1_A1_CONSTANTS)) {
        static int max_step;

        if (max_step != s) {
          max_step = s;
          fprintf (stderr,
                   "HFR HF1 exceeded A1 table size (%d), max. index=%d\n",
                   MAX_HF1_A1_CONSTANTS, s);
        }
        for (s = 0; s < nStep; s++) {
          if ((nStepBeg + s * nStepSize) >= MAX_HF1_A1_CONSTANTS)
            arA1[s] = pA1[MAX_HF1_A1_CONSTANTS - 1];
        }
      }
      if ((bHf1 == false) && (s >= MAX_HF2_A1_CONSTANTS)) {
        static int max_step;

        if (max_step != s) {
          fprintf (stderr,
                   "HFR HF2 exceeded A1 table size (%d), max. index=%d\n",
                   MAX_HF2_A1_CONSTANTS, s);
          max_step = s;
        }
        for (s = 0; s < nStep; s++) {
          if ((nStepBeg + s * nStepSize) >= MAX_HF2_A1_CONSTANTS)
            arA1[s] = pA1[MAX_HF2_A1_CONSTANTS - 1];
        }
      }


/*
fprintf(stderr,"%d :: nStep=%d, nFilter=%d, nRepeat=%d,"
       "nStepBeg=%d, nStepSize=%d\n",
       nLst,nStep,nFilter,nRepeat,nStepBeg,nStepSize);
fprintf(stderr,"A1[%d]= %7.2f %7.2f %7.2f %7.2f ... "
                       "%7.2f %7.2f %7.2f %7.2f\n",
        s,arA1[0],arA1[1],arA1[2],arA1[3],
          arA1[nStep-4],arA1[nStep-3],arA1[nStep-2],arA1[nStep-1]);
fprintf(stderr,"nStepBeg=%d, nStepSize=%d\n",nStepBeg,nStepSize);
*/



      if (bExAnt == true) {
        A2 = A123[6][1];
        A3 = A123[6][2];
        switch (nFilter) {
         case 1:
           pdBcal = dBcalHF[0];
           break;
         case 2:
           pdBcal = dBcalHF[2];
           break;
         case 4:
           pdBcal = dBcalHF[4];
           break;
         case 8:
           pdBcal = dBcalHF[6];
           break;
         default:
           assert (0);
           break;
        }
      } else {
        A2 = A123[7][1];
        A3 = A123[7][2];
        switch (nFilter) {
         case 1:
           pdBcal = dBcalHF[1];
           break;
         case 2:
           pdBcal = dBcalHF[3];
           break;
         case 4:
           pdBcal = dBcalHF[5];
           break;
         case 8:
           pdBcal = dBcalHF[7];
           break;
         default:
           assert (0);
           break;
        }
      }
      A2 = 1.0 / A2;                    /* invert to save a divide */

      /*
       * Sanity check 
       */
      if (pAgc->length != (nRepeat * nStep)) {
        fprintf (stderr, "%s agc len=%d, repeat=%d, step=%d\n",
                 CasHfr_Idx2Str (nLst), pAgc->length, nRepeat, nStep);
        CasHfr_DumpRecord (pRec);
      }

/* assert(pAgc[nLst]->length==nRepeat);*/


      fK = fBw / (2.0 * nFilter);       /* constant for freq filter offset */
      nIdx = 0;
      for (r = 0; r < nRepeat; r++) {
        for (s = 0; s < nStep; s++) {
          fAgc = (pAgc->data[r * nStep + s] - A3) * A2;
          fAgc = pow (10.0, fAgc);
          fAgc = 40.0 * log10 (fAgc + 1.0);
          fAgc = fAgc - arA1[s];

          if (pAuto->length != 0) {
            for (f = 0; f < nFilter; f++) {
              nAuto = pAuto->data[nIdx];
              nAuto = (1 << (nAuto >> 3)) * ((nAuto & 0x07) + 8);
              pAutoMag->data[nIdx] = 10 * log10 ((double) nAuto);
              pAutoMag->data[nIdx] += fAgc;
              pAutoMag->data[nIdx] -= pdBcal[f];
              pAutoMag->freq[nIdx] =
                arStep[s] + fK * (2.0 * f - nFilter + 1.0);
              ++nIdx;
            }
          } /* if auto data */
          else {
            pAutoMag->data[nIdx] = fAgc;
            pAutoMag->freq[nIdx] = arStep[s];
            ++nIdx;
          }                             /* else agc only */

        }                               /* for step cycle */
      }                                 /* for repeat cycle */
      pAutoMag->length = nIdx;
      nLength += nIdx;
    } /* if agc for the nLst data */
    else {
      pAutoMag->length = 0;             /* no agc => no cal data */
    }
  }                                     /* scan the HF agc matrixes */


  return nLength;
}



Ulong CasHfrAnalysis_GetAgcByteCount (CasRecord * pRec)
{
  unsigned char *pMp = pRec->data;
  Ulong nRepeat, nStep, nBand, nAnt;
  Ulong nAgc, nAgcABC, nAgcHF1, nAgcHF2;


  /*
   * Band ABC Agc Count 
   */
  nAgcABC = 0;
  if (CasHfr_bDirectionFindingABC (pMp) == true)
    nAnt = 4;                           /* Eu,Ev,Ez,Ez */
  else {                                /* Eu,Ev,Ex should be mutually exclusive */
    nAnt = 0;
    if (CasHfr_bEuOnABC (pMp) == true)
      ++nAnt;
    if (CasHfr_bEvOnABC (pMp) == true)
      ++nAnt;
    if (CasHfr_bExOnABC (pMp) == true)
      ++nAnt;
    if (CasHfr_bEzOnABC (pMp) == true)
      ++nAnt;
  }
  nBand = 0;
  if (CasHfr_bBandASelected (pMp) == true)
    ++nBand;
  if (CasHfr_bBandBSelected (pMp) == true)
    ++nBand;
  if (CasHfr_bBandCSelected (pMp) == true)
    ++nBand;
  nRepeat = CasHfr_nRepeatCountABC (pMp) * CasHfr_nRepeatCountAll (pMp);
  nAgcABC = nAnt * nBand * nRepeat;

  /*
   * Band HF1 Agc Count 
   */
  nAgcHF1 = 0;
  if (CasHfr_bBandHF1Selected (pMp) == true) {
    if (CasHfr_bDirectionFindingHF1 (pMp) == true)
      nAnt = 4;
    else {
      nAnt = 0;
      if (CasHfr_bEuOnHF1 (pMp) == true)
        ++nAnt;
      if (CasHfr_bEvOnHF1 (pMp) == true)
        ++nAnt;
      if (CasHfr_bExOnHF1 (pMp) == true)
        ++nAnt;
      if (CasHfr_bEzOnHF1 (pMp) == true)
        ++nAnt;
    }
    nStep = CasHfr_nNumberStepsHF1 (pMp);
    nRepeat = CasHfr_nRepeatCountHF1 (pMp) * CasHfr_nRepeatCountAll (pMp);
    nAgcHF1 = nAnt * nStep * nRepeat;
  }

  /*
   * Band HF2 Agc Count 
   */
  nAgcHF2 = 0;
  if (CasHfr_bBandHF2Selected (pMp) == true) {
    if (CasHfr_bDirectionFindingHF2 (pMp) == true)
      nAnt = 4;
    else {
      nAnt = 0;
      if (CasHfr_bEuOnHF2 (pMp) == true)
        ++nAnt;
      if (CasHfr_bEvOnHF2 (pMp) == true)
        ++nAnt;
      if (CasHfr_bExOnHF2 (pMp) == true)
        ++nAnt;
      if (CasHfr_bEzOnHF2 (pMp) == true)
        ++nAnt;
    }
    nStep = CasHfr_nNumberStepsHF2 (pMp);
    nRepeat = CasHfr_nRepeatCountHF2 (pMp) * CasHfr_nRepeatCountAll (pMp);
    nAgcHF2 = nAnt * nStep * nRepeat;
  }

  nAgc = nAgcABC + nAgcHF1 + nAgcHF2;



  return nAgc;
}                                       /* obsolete */



Ulong CasHfrAnalysis_AgcByteCount (CasRecord * pRec, Ulong nType)
{
  unsigned char *pMp = pRec->data;
  Ulong nHfrBand, nHfrAnt;
  Ulong nBnd, nAnt, nStep, nRepeat;
  Ulong nAgc;

  nAgc = 0;
  if ((nHfrBand = (nType & CasHfr_BandMask)) == 0)
    return nAgc;
  if ((nHfrAnt = (nType & CasAntMask)) == 0)
    return nAgc;

  /*
   * Agc for Bands A,B,C 
   */
  if (nHfrBand & CasHfr_BandABC) {

    nBnd = 0;
    if ((nHfrBand & CasHfr_BandA) && (CasHfr_bBandASelected (pMp) == true))
      ++nBnd;
    if ((nHfrBand & CasHfr_BandB) && (CasHfr_bBandBSelected (pMp) == true))
      ++nBnd;
    if ((nHfrBand & CasHfr_BandC) && (CasHfr_bBandCSelected (pMp) == true))
      ++nBnd;

    nRepeat = (CasHfr_nRepeatCountABC (pMp) * CasHfr_nRepeatCountAll (pMp));

    nAnt = 0;
    if (CasHfr_bDirectionFindingABC (pMp) == true) {
      if (nHfrAnt & CasAntEu)
        nAnt += 1;
      if (nHfrAnt & CasAntEv)
        nAnt += 1;
      if (nHfrAnt & CasAntEz)
        nAnt += 2;
    } /* Direction Finding */
    else {
      if ((nHfrAnt & CasAntEu) && (CasHfr_bEuOnABC (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEv) && (CasHfr_bEvOnABC (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEx) && (CasHfr_bExOnABC (pMp) == true))
        nAnt += 1;
      else
        nAnt = 0;

      if ((nHfrAnt & CasAntEz) && (CasHfr_bEzOnABC (pMp) == true))
        nAnt += 1;
    }                                   /* Regular antenna switches */

    nAgc += (nBnd * nRepeat * nAnt);
  }



  /*
   * Agc for Bands A,B,C 
   */
  /*
   * Agc for Band HF1 
   */
  if ((nHfrBand & CasHfr_BandHF1) && (CasHfr_bBandHF1Selected (pMp) == true)) {
    nStep = CasHfr_nNumberStepsHF1 (pMp);
    nRepeat = (CasHfr_nRepeatCountHF1 (pMp) * CasHfr_nRepeatCountAll (pMp));

    nAnt = 0;
    if (CasHfr_bDirectionFindingHF1 (pMp) == true) {
      if (nHfrAnt & CasAntEu)
        nAnt += 1;
      if (nHfrAnt & CasAntEv)
        nAnt += 1;
      if (nHfrAnt & CasAntEz)
        nAnt += 2;
    } /* Direction Finding */
    else {
      if ((nHfrAnt & CasAntEu) && (CasHfr_bEuOnHF1 (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEv) && (CasHfr_bEvOnHF1 (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEx) && (CasHfr_bExOnHF1 (pMp) == true))
        nAnt += 1;
      else
        nAnt = 0;

      if ((nHfrAnt & CasAntEz) && (CasHfr_bEzOnHF1 (pMp) == true))
        nAnt += 1;
    }                                   /* Regular antenna switches */

    nAgc += (nStep * nRepeat * nAnt);
  }



  /*
   * Agc for Band HF1 
   */
  /*
   * Agc for Band HF2 
   */
  if ((nHfrBand & CasHfr_BandHF2) && (CasHfr_bBandHF2Selected (pMp) == true)) {
    nStep = CasHfr_nNumberStepsHF2 (pMp);
    nRepeat = (CasHfr_nRepeatCountHF2 (pMp) * CasHfr_nRepeatCountAll (pMp));

    nAnt = 0;
    if (CasHfr_bDirectionFindingHF2 (pMp) == true) {
      if (nHfrAnt & CasAntExp)
        nAnt += 1;
      if (nHfrAnt & CasAntExm)
        nAnt += 1;
      if (nHfrAnt & CasAntEz)
        nAnt += 2;
    } /* Direction Finding */
    else {
      if ((nHfrAnt & CasAntEu) && (CasHfr_bEuOnHF2 (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEv) && (CasHfr_bEvOnHF2 (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEx) && (CasHfr_bExOnHF2 (pMp) == true))
        nAnt += 1;
      else
        nAnt = 0;

      if ((nHfrAnt & CasAntEz) && (CasHfr_bEzOnHF2 (pMp) == true))
        nAnt += 1;
    }                                   /* Regular antenna switches */

    nAgc += (nStep * nRepeat * nAnt);
  }
  /*
   * Agc for Band HF2 
   */
  return nAgc;
}



Ulong CasHfrAnalysis_AutoByteCount (CasRecord * pRec, Ulong nType)
{
  unsigned char *pMp = pRec->data;
  Ulong nHfrBand, nHfrAnt;
  Ulong nBnd, nAnt, nStep, nRepeat, nFilter;
  Ulong nAuto;

  nAuto = 0;
  if ((nHfrBand = (nType & CasHfr_BandMask)) == 0)
    return nAuto;
  if ((nHfrAnt = (nType & CasAntMask)) == 0)
    return nAuto;

  /*
   * Auto for Bands A,B,C 
   */
  if ((nHfrBand & CasHfr_BandABC) && (CasHfr_bAutoABC (pMp) == true)) {

    nBnd = 0;
    if ((nHfrBand & CasHfr_BandA) && (CasHfr_bBandASelected (pMp) == true))
      nBnd += 1;
    if ((nHfrBand & CasHfr_BandB) && (CasHfr_bBandBSelected (pMp) == true))
      nBnd += 1;
    if ((nHfrBand & CasHfr_BandC) && (CasHfr_bBandCSelected (pMp) == true))
      nBnd += 1;

    nFilter = CasHfr_nFiltersABC (pMp);
    nRepeat = (CasHfr_nRepeatCountABC (pMp) * CasHfr_nRepeatCountAll (pMp));

    nAnt = 0;
    if (CasHfr_bDirectionFindingABC (pMp) == true) {
      if (nHfrAnt & CasAntEu)
        nAnt += 1;
      if (nHfrAnt & CasAntEv)
        nAnt += 1;
      if (nHfrAnt & CasAntEz)
        nAnt += 2;
    } /* Direction Finding */
    else {
      if ((nHfrAnt & CasAntEu) && (CasHfr_bEuOnABC (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEv) && (CasHfr_bEvOnABC (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEx) && (CasHfr_bExOnABC (pMp) == true))
        nAnt += 1;
      else
        nAnt = 0;

      if ((nHfrAnt & CasAntEz) && (CasHfr_bEzOnABC (pMp) == true))
        nAnt += 1;
    }                                   /* Regular antenna switches */

    nAuto += (nAnt * nBnd * nFilter * nRepeat);
  }



  /*
   * Auto for Bands A,B,C 
   */
  /*
   * Auto for Band HF1 
   */
  if ((nHfrBand & CasHfr_BandHF1) && (CasHfr_bBandHF1Selected (pMp) == true)
      && (CasHfr_bAutoHF1 (pMp) == true)) {

    nStep = CasHfr_nNumberStepsHF1 (pMp);
    nFilter = CasHfr_nFiltersHF1 (pMp);
    nRepeat = (CasHfr_nRepeatCountHF1 (pMp) * CasHfr_nRepeatCountAll (pMp));

    nAnt = 0;
    if (CasHfr_bDirectionFindingHF1 (pMp) == true) {
      if (nHfrAnt & CasAntEu)
        nAnt += 1;
      if (nHfrAnt & CasAntEv)
        nAnt += 1;
      if (nHfrAnt & CasAntEz)
        nAnt += 2;
    } /* Direction Finding */
    else {
      if ((nHfrAnt & CasAntEu) && (CasHfr_bEuOnHF1 (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEv) && (CasHfr_bEvOnHF1 (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEx) && (CasHfr_bExOnHF1 (pMp) == true))
        nAnt += 1;
      else
        nAnt = 0;

      if ((nHfrAnt & CasAntEz) && (CasHfr_bEzOnHF1 (pMp) == true))
        nAnt += 1;
    }                                   /* Regular antenna switches */

    nAuto += (nAnt * nFilter * nStep * nRepeat);
  }



  /*
   * Auto for Band HF1 
   */
  /*
   * Auto for Band HF2 
   */
  if ((nHfrBand & CasHfr_BandHF2) && (CasHfr_bBandHF2Selected (pMp) == true)
      && (CasHfr_bAutoHF2 (pMp) == true)) {

    nStep = CasHfr_nNumberStepsHF2 (pMp);
    nFilter = CasHfr_nFiltersHF2 (pMp);
    nRepeat = (CasHfr_nRepeatCountHF2 (pMp) * CasHfr_nRepeatCountAll (pMp));

    nAnt = 0;
    if (CasHfr_bDirectionFindingHF2 (pMp) == true) {
      if (nHfrAnt & CasAntEu)
        nAnt += 1;
      if (nHfrAnt & CasAntEv)
        nAnt += 1;
      if (nHfrAnt & CasAntEz)
        nAnt += 2;
    } /* Direction Finding */
    else {
      if ((nHfrAnt & CasAntEu) && (CasHfr_bEuOnHF2 (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEv) && (CasHfr_bEvOnHF2 (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEx) && (CasHfr_bExOnHF2 (pMp) == true))
        nAnt += 1;
      else
        nAnt = 0;

      if ((nHfrAnt & CasAntEz) && (CasHfr_bEzOnHF2 (pMp) == true))
        nAnt += 1;
    }                                   /* Regular antenna switches */

    nAuto += (nAnt * nFilter * nStep * nRepeat);
  }

  /*
   * Auto for Band HF2 
   */
  return nAuto;
}



Ulong CasHfrAnalysis_CrossByteCount (CasRecord * pRec, Ulong nType)
{
  unsigned char *pMp = pRec->data;
  Ulong nHfrBand, nHfrAnt;
  Ulong nBnd, nAnt, nStep, nRepeat, nFilter;
  Ulong nCross, nBase;
  float fBase;

  nCross = 0;
  if ((nHfrBand = (nType & CasHfr_BandMask)) == 0)
    return nCross;
  if ((nHfrAnt = (nType & CasAntMask)) == 0)
    return nCross;

  /*
   * Cross for Bands A,B,C : Allways on 8bit boundaries 
   */
  if ((nHfrBand & CasHfr_BandABC) && (CasHfr_bCrossABC (pMp) == true)) {

    nBnd = 0;
    if ((nHfrBand & CasHfr_BandA) && (CasHfr_bBandASelected (pMp) == true))
      nBnd += 1;
    if ((nHfrBand & CasHfr_BandB) && (CasHfr_bBandBSelected (pMp) == true))
      nBnd += 1;
    if ((nHfrBand & CasHfr_BandC) && (CasHfr_bBandCSelected (pMp) == true))
      nBnd += 1;

    nFilter = CasHfr_nFiltersABC (pMp);
    nRepeat = (CasHfr_nRepeatCountABC (pMp) * CasHfr_nRepeatCountAll (pMp));

    nAnt = 0;
    if (CasHfr_bDirectionFindingABC (pMp) == true) {
      if (nHfrAnt & CasAntEu)
        nAnt += 1;
      if (nHfrAnt & CasAntEv)
        nAnt += 1;
      if (nHfrAnt & CasAntEz)
        nAnt += 2;
    } /* Direction Finding */
    else {
      if ((nHfrAnt & CasAntEu) && (CasHfr_bEuOnABC (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEv) && (CasHfr_bEvOnABC (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEx) && (CasHfr_bExOnABC (pMp) == true))
        nAnt += 1;
      else
        nAnt = 0;

      if ((nHfrAnt & CasAntEz) && (CasHfr_bEzOnABC (pMp) == true))
        nAnt += 1;
    }                                   /* Regular antenna switches */

    fBase = (float) nFilter *1.125;

    nBase = (Ulong) fBase;
    nCross += (nBnd * nAnt * nBase * nRepeat);
  }



  /*
   * Cross for Bands A,B,C 
   */
  /*
   * Cross for Band HF1 
   */
  if ((nHfrBand & CasHfr_BandHF1) && (CasHfr_bBandHF1Selected (pMp) == true)
      && (CasHfr_bCrossHF1 (pMp) == true)) {

    nStep = CasHfr_nNumberStepsHF1 (pMp);
    nFilter = CasHfr_nFiltersHF1 (pMp);
    nRepeat = (CasHfr_nRepeatCountHF1 (pMp) * CasHfr_nRepeatCountAll (pMp));

    nAnt = 0;
    if (CasHfr_bDirectionFindingHF1 (pMp) == true) {
      /*
       * antenna measurements are taken in pairs: Ex+ & Ez, Ex- & Ez 
       */
      /*
       * 9bits of data for each measurement 
       */
      if (nHfrAnt & CasAntEu)
        nAnt += 1;
      if (nHfrAnt & CasAntEv)
        nAnt += 1;
      if (nHfrAnt & CasAntEz)
        nAnt += 2;

      if (nAnt == 4) {                  /* Ex+&Ez,Ex-&Ez */
        nAnt = 2;
        nStep *= 2;                     /* Ex+ & Ez are pairs for cross byte counting */
        fBase = (float) (nStep * nFilter) * 1.125;
        nBase = (ULONG) fBase;
        if (fBase != nBase)
          nBase += 1;
        nCross += (nBase * nAnt * nRepeat);
      } else if (nAnt == 3) {           /* (Ex+ | Ex-) & (Ez & Ez) */
        /*
         * Round Ex+ & Ez as a pair 
         */
        fBase = (float) (nStep * nFilter) * 1.125 * 2.0;
        nBase = (ULONG) fBase;
        if (fBase != nBase)
          nBase += 1;
        nCross += (nBase * nRepeat);
        /*
         * Round Ez by itself 
         */
        fBase = (float) (nStep * nFilter) * 1.125;
        nBase = (ULONG) fBase;
        if (fBase != nBase)
          nBase += 1;
        nCross += (nBase * nRepeat);
      } else {                          /* nAnt=2 or nAnt=1 */
        fBase = (float) (nStep * nFilter) * 1.125;
        nBase = (ULONG) fBase;
        if (fBase != nBase)
          nBase += 1;
        nCross += (nBase * nAnt * nRepeat);
      }
    } /* Direction Finding */
    else {
      if ((nHfrAnt & CasAntEu) && (CasHfr_bEuOnHF1 (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEv) && (CasHfr_bEvOnHF1 (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEx) && (CasHfr_bExOnHF1 (pMp) == true))
        nAnt += 1;
      else
        nAnt = 0;

      if ((nHfrAnt & CasAntEz) && (CasHfr_bEzOnHF1 (pMp) == true))
        nAnt += 1;

      fBase = (float) (nStep * nFilter) * 1.125;
      nBase = (ULONG) fBase;
      if (fBase != nBase)
        nBase += 1;
      nCross += (nBase * nAnt * nRepeat);
    }                                   /* Regular antenna switches */

  }



  /*
   * Cross for Band HF1 
   */
  /*
   * Cross for Band HF2 
   */
  if ((nHfrBand & CasHfr_BandHF2) && (CasHfr_bBandHF2Selected (pMp) == true)
      && (CasHfr_bCrossHF2 (pMp) == true)) {

    nStep = CasHfr_nNumberStepsHF2 (pMp);
    nFilter = CasHfr_nFiltersHF2 (pMp);
    nRepeat = (CasHfr_nRepeatCountHF2 (pMp) * CasHfr_nRepeatCountAll (pMp));

    nAnt = 0;
    if (CasHfr_bDirectionFindingHF2 (pMp) == true) {
      if (nHfrAnt & CasAntEu)
        nAnt += 1;
      if (nHfrAnt & CasAntEv)
        nAnt += 1;
      if (nHfrAnt & CasAntEz)
        nAnt += 2;

      if (nAnt == 4) {                  /* Ex+&Ez,Ex-&Ez */
        nAnt = 2;
        nStep *= 2;                     /* Ex+ & Ez are pairs for cross byte counting */
        fBase = (float) (nStep * nFilter) * 1.125;
        nBase = (ULONG) fBase;
        if (fBase != nBase)
          nBase += 1;
        nCross += (nBase * nAnt * nRepeat);
      } else if (nAnt == 3) {           /* (Ex+ | Ex-) & (Ez & Ez) */
        /*
         * Round Ex+ & Ez as a pair 
         */
        fBase = (float) (nStep * nFilter) * 1.125 * 2.0;
        nBase = (ULONG) fBase;
        if (fBase != nBase)
          nBase += 1;
        nCross += (nBase * nRepeat);
        /*
         * Round Ez by itself 
         */
        fBase = (float) (nStep * nFilter) * 1.125;
        nBase = (ULONG) fBase;
        if (fBase != nBase)
          nBase += 1;
        nCross += (nBase * nRepeat);
      } else {                          /* nAnt=2 or nAnt=1 */
        fBase = (float) (nStep * nFilter) * 1.125;
        nBase = (ULONG) fBase;
        if (fBase != nBase)
          nBase += 1;
        nCross += (nBase * nAnt * nRepeat);
      }
    } /* Direction Finding */
    else {
      if ((nHfrAnt & CasAntEu) && (CasHfr_bEuOnHF2 (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEv) && (CasHfr_bEvOnHF2 (pMp) == true))
        nAnt += 1;
      else if ((nHfrAnt & CasAntEx) && (CasHfr_bExOnHF2 (pMp) == true))
        nAnt += 1;
      else
        nAnt = 0;

      if ((nHfrAnt & CasAntEz) && (CasHfr_bEzOnHF2 (pMp) == true))
        nAnt += 1;
      fBase = (float) (nStep * nFilter) * 1.125;
      nBase = (ULONG) fBase;
      if (fBase != nBase)
        nBase += 1;
      nCross += (nBase * nAnt * nRepeat);
    }                                   /* Regular antenna switches */


  }
  /*
   * Cross for Band HF2 
   */
  return nCross;
}



Ulong CasHfrAnalysis_ByteCount (CasRecord * pRec, Ulong nType)
{
  Uchar *pMp = pRec->data;
  Ulong nAgc, nAuto, nCross, nTotal;

  if (CasHfr_bHfrPacket (pMp) == false)
    return 0;                           /* Not a hfr packet */

  /*
   * AGCs,AUTOs,CROSSs 
   */
  nAgc = CasHfrAnalysis_AgcByteCount (pRec, nType);
  nAuto = CasHfrAnalysis_AutoByteCount (pRec, nType);
  nCross = CasHfrAnalysis_CrossByteCount (pRec, nType);

  nTotal = nAgc + nAuto + nCross;

  return nTotal;
}                                       /* Hfr Analysis Byte Count */



Ulong CasHfrCalibration_ByteCount (CasRecord * pRec, Ulong nType)
{
  fprintf (stderr, "CasHfrCalibration_ByteCount()\n");
  assert (0);
}
Ulong CasHfrSounder_ByteCount (CasRecord * pRec, Ulong nType)
{
  fprintf (stderr, "CasHfrSounder_ByteCount()\n");
  assert (0);
}




/*
  Hack some times in for the hfr sounder
  5 Bands, 18 channels / band => 90 total
  Band 0: 0-17, Band 1: 18-35, Band 2: 36-53, Band 3: 54-71, Band 4: 72-89
*/
float CasHfrSounder_fDuration (CasRecord * pRec, Ulong nType)
{
  Uchar *pMp = pRec->data;
  int i, nCycles, nStartFrq, nStopFrq, nSteps[5];
  float fT1, fT2, fT3;
  float fPulse[] =
    { 1.0 / 200.0, 1.0 / 400.0, 1.0 / 800.0, 1.0 / 1600.0, 1.0 / 3200.0 };
  float fDuration;

  if (nType == 0x00)                    /* calculate length for whole packet */
    nType = CasHfr_Sounder | CasHfr_BandAll | CasAntAll;

  assert (CasHfr_bSounder (pMp) == true);

  nCycles = CasHfrSnd_nPassive (pMp);
  nCycles += CasHfrSnd_nActive (pMp);
  nCycles *= CasHfrSnd_nCycles (pMp);

  nSteps[0] = nSteps[1] = nSteps[2] = nSteps[3] = nSteps[4] = 0;
  nStartFrq = CasHfrSnd_nStartFrequency (pMp);
  nStopFrq = CasHfrSnd_nStopFrequency (pMp);
  for (i = nStartFrq; i < (nStopFrq + 1); i++) {
    if (i < 18)
      ++nSteps[0];                      /* Band 0 :  0-17 */
    else if (i < 36)
      ++nSteps[1];                      /* Band 1 : 18-35 */
    else if (i < 54)
      ++nSteps[2];                      /* Band 2 : 36-53 */
    else if (i < 72)
      ++nSteps[3];                      /* Band 3 : 54-71 */
    else if (i < 90)
      ++nSteps[4];                      /* Band 4 : 72-89 */
    else {
      return 0.0;
      /*
       * fprintf(stderr,"bad hfr sounder record\n");
       * CasHfr_DumpRecord(pRec);
       * assert(0); 
       */
    }
  }                                     /* rof */

  fT1 = (float) CasHfrSnd_nT1 (pMp);
  fT1 *= 1.0E-3;                        /* milliseconds */
  fT2 = (float) CasHfrSnd_nT2 (pMp);
  fT2 *= 1.0E-3;                        /* milliseconds */
  fT3 = (float) CasHfrSnd_nT3 (pMp);
  fT3 *= 1.0E-3;                        /* milliseconds */

  fDuration = nSteps[0] * (fPulse[0] + fT1 + 2 * fT2 + fT3 + 3 * 2.5E-3);
  fDuration += nSteps[1] * (fPulse[1] + fT1 + 2 * fT2 + fT3 + 3 * 2.5E-3);
  fDuration += nSteps[2] * (fPulse[2] + fT1 + 2 * fT2 + fT3 + 3 * 2.5E-3);
  fDuration += nSteps[3] * (fPulse[3] + fT1 + 2 * fT2 + fT3 + 3 * 2.5E-3);
  fDuration += nSteps[4] * (fPulse[4] + fT1 + 2 * fT2 + fT3 + 3 * 2.5E-3);

  fDuration *= nCycles;


  return fDuration;
}



Ulong CasHfr_ByteCount (CasRecord * pRec, Ulong nType)
{
  unsigned char *pMp = pRec->data;
  Ulong nLength;

  if (CasHfr_bHfrPacket (pMp) == false)
    nLength = 0;
  else if (CasHfr_bAnalysis (pMp) == true)
    nLength = CasHfrAnalysis_ByteCount (pRec, nType);
  else if (CasHfr_bCalibration (pMp) == true)
    nLength = CasHfrCalibration_ByteCount (pRec, nType);
  else if (CasHfr_bMillisecond (pMp) == true)
    nLength = CasHfrMillisecond_ByteCount (pRec, nType);
  else if (CasHfr_bSounder (pMp) == true)
    nLength = CasHfrSounder_ByteCount (pRec, nType);
  else
    nLength = 0;

  return nLength;
}



Ulong CasHfrAnalysis_XtractAgc (CasRecord * pRec)
{
  unsigned char *pMp = pRec->data;
  unsigned char *pData;

  bool bDirFind = false, bEuOn = false, bEvOn = false, bExOn = false, bEzOn =
    false;
  int nBand;
  int r, s;
  int nRepeat, nStep;

  int nLength;
  HfrRawData1 *pBand;
  HfrRawData0 *pEu, *pEv, *pEx, *pEz;




  if (CasHfr_bAnalysis (pMp) == false)
    return 0;

  /*
   * stop compliler initialization complaints (for loop ensures init) 
   */
  nRepeat = nStep = 0;
  pBand = NULL;
  pEu = pEv = pEx = pEz = NULL;

  pData = pMp + 25;                     /* Analysis Header Offset is 25 bytes (should be a define) */
  for (nBand = 0; nBand < 5; nBand++) {

    if (nBand < 3) {                    /* Bands ABC */
      if (nBand == 0) {
        if (CasHfr_bBandASelected (pMp) == true)
          pBand = &(A.Agc);
        else
          continue;
      } /* Band A */
      else if (nBand == 1) {
        if (CasHfr_bBandBSelected (pMp) == true)
          pBand = &(B.Agc);
        else
          continue;
      } /* Band B */
      else if (nBand == 2) {
        if (CasHfr_bBandCSelected (pMp) == true)
          pBand = &(C.Agc);
        else
          continue;
      } /* Band C */
      else {
        assert (0);
      }
      nStep = 1;
      nRepeat = CasHfr_nRepeatCountABC (pMp) * CasHfr_nRepeatCountAll (pMp);
      bDirFind = CasHfr_bDirectionFindingABC (pMp);
      bEuOn = CasHfr_bEuOnABC (pMp);
      bEvOn = CasHfr_bEvOnABC (pMp);
      bExOn = CasHfr_bExOnABC (pMp);
      bEzOn = CasHfr_bEzOnABC (pMp);
    } /* Bands ABC */
    else if (nBand == 3) {
      if (CasHfr_bBandHF1Selected (pMp) == true)
        pBand = &(HF1.Agc);
      else
        continue;
      nStep = CasHfr_nNumberStepsHF1 (pMp);
      nRepeat = CasHfr_nRepeatCountHF1 (pMp) * CasHfr_nRepeatCountAll (pMp);
      bDirFind = CasHfr_bDirectionFindingHF1 (pMp);
      bEuOn = CasHfr_bEuOnHF1 (pMp);
      bEvOn = CasHfr_bEvOnHF1 (pMp);
      bExOn = CasHfr_bExOnHF1 (pMp);
      bEzOn = CasHfr_bEzOnHF1 (pMp);
    } /* Band HF1 */
    else if (nBand == 4) {
      if (CasHfr_bBandHF2Selected (pMp) == true)
        pBand = &(HF2.Agc);
      else
        continue;
      nStep = CasHfr_nNumberStepsHF2 (pMp);
      nRepeat = CasHfr_nRepeatCountHF2 (pMp) * CasHfr_nRepeatCountAll (pMp);
      bDirFind = CasHfr_bDirectionFindingHF2 (pMp);
      bEuOn = CasHfr_bEuOnHF2 (pMp);
      bEvOn = CasHfr_bEvOnHF2 (pMp);
      bExOn = CasHfr_bExOnHF2 (pMp);
      bEzOn = CasHfr_bEzOnHF2 (pMp);
    } /* Band HF2 */
    else {
      assert (0);
    }

    if (bDirFind == true) {
      pEu = &(pBand->Eu);
      pEv = &(pBand->Ev);
      pEz = &(pBand->Ez);
      for (r = 0; r < nRepeat; r++) {
        for (s = 0; s < nStep; s++) {
          pEu->data[pEu->length++] = *pData++;
          pEv->data[pEv->length++] = *pData++;
        }
      }
      for (r = 0; r < nRepeat; r++) {
        for (s = 0; s < nStep; s++) {
          pEz->data[pEz->length] = *pData++;
          pEz->data[pEz->length + nStep] = *pData++;
          ++pEz->length;
        }
        pEz->length += nStep;
      }
    } /* direction finding */
    else {
      if (bEuOn == true)
        pEx = &(pBand->Eu);
      else if (bEvOn == true)
        pEx = &(pBand->Ev);
      else if (bExOn == true)
        pEx = &(pBand->Ex);
      else
        pEx = NULL;
      if (pEx != NULL) {
        for (r = 0; r < nRepeat; r++) {
          for (s = 0; s < nStep; s++)
            pEx->data[pEx->length++] = *pData++;
        }
      }
      if (bEzOn == true)
        pEz = &(pBand->Ez);
      else
        pEz = NULL;
      if (pEz != NULL) {
        for (r = 0; r < nRepeat; r++)
          for (s = 0; s < nStep; s++) {
            pEz->data[pEz->length++] = *pData++;
          }
      }
    }                                   /* normal */

  }                                     /* for loop */
  /*
   * end of extract agc 
   */
  nLength = pData - (pMp + 25);

  /*
   * Sanity Check for the AGC byte count 
   */

  assert (nLength == CasHfr_GetAgcByteCount (pRec));



  return nLength;
}                                       /* hfr analysis extract agc */



Ulong CasHfrAnalysis_GetAutoByteCount (CasRecord * pRec)
{
  Uchar *pMp = pRec->data;
  Ulong nRepeat, nStep, nFilter, nBand, nAnt;
  Ulong nAuto, nAutoABC, nAutoHF1, nAutoHF2;



  /*
   * Band ABC Agc Count 
   */
  nAutoABC = 0;
  if (CasHfr_bDirectionFindingABC (pMp) == true)
    nAnt = 4;                           /* Eu,Ev,Ez,Ez */
  else {                                /* Eu,Ev,Ex should be mutually exclusive */
    nAnt = 0;
    if (CasHfr_bEuOnABC (pMp) == true)
      ++nAnt;
    if (CasHfr_bEvOnABC (pMp) == true)
      ++nAnt;
    if (CasHfr_bExOnABC (pMp) == true)
      ++nAnt;
    if (CasHfr_bEzOnABC (pMp) == true)
      ++nAnt;
  }
  nBand = 0;
  if (CasHfr_bBandASelected (pMp) == true)
    ++nBand;
  if (CasHfr_bBandBSelected (pMp) == true)
    ++nBand;
  if (CasHfr_bBandCSelected (pMp) == true)
    ++nBand;
  nFilter = CasHfr_nFiltersABC (pMp);
  nRepeat = CasHfr_nRepeatCountABC (pMp) * CasHfr_nRepeatCountAll (pMp);
  nAutoABC = nAnt * nBand * nFilter * nRepeat;

  /*
   * Band HF1 Agc Count 
   */
  nAutoHF1 = 0;
  if (CasHfr_bBandHF1Selected (pMp) == true) {
    if (CasHfr_bDirectionFindingHF1 (pMp) == true)
      nAnt = 4;
    else {
      nAnt = 0;
      if (CasHfr_bEuOnHF1 (pMp) == true)
        ++nAnt;
      if (CasHfr_bEvOnHF1 (pMp) == true)
        ++nAnt;
      if (CasHfr_bExOnHF1 (pMp) == true)
        ++nAnt;
      if (CasHfr_bEzOnHF1 (pMp) == true)
        ++nAnt;
    }
    nFilter = CasHfr_nFiltersHF1 (pMp);
    nStep = CasHfr_nNumberStepsHF1 (pMp);
    nRepeat = CasHfr_nRepeatCountHF1 (pMp) * CasHfr_nRepeatCountAll (pMp);
    nAutoHF1 = nAnt * nFilter * nStep * nRepeat;
  }

  /*
   * Band HF2 Agc Count 
   */
  nAutoHF2 = 0;
  if (CasHfr_bBandHF2Selected (pMp) == true) {
    if (CasHfr_bDirectionFindingHF2 (pMp) == true)
      nAnt = 4;
    else {
      nAnt = 0;
      if (CasHfr_bEuOnHF2 (pMp) == true)
        ++nAnt;
      if (CasHfr_bEvOnHF2 (pMp) == true)
        ++nAnt;
      if (CasHfr_bExOnHF2 (pMp) == true)
        ++nAnt;
      if (CasHfr_bEzOnHF2 (pMp) == true)
        ++nAnt;
    }
    nStep = CasHfr_nNumberStepsHF2 (pMp);
    nRepeat = CasHfr_nRepeatCountHF2 (pMp) * CasHfr_nRepeatCountAll (pMp);
    nAutoHF2 = nAnt * nFilter * nStep * nRepeat;
  }

  nAuto = nAutoABC + nAutoHF1 + nAutoHF2;



  return nAuto;
}



Ulong CasHfrAnalysis_XtractAuto (CasRecord * pRec)
{
  unsigned char *pMp = pRec->data;
  unsigned char *pData;

  bool bDirFind = false, bEuOn = false, bEvOn = false, bExOn = false, bEzOn =
    false;
  int nBand;
  int r, s, f;
  int nRepeat, nStep, nFilter;

  int nLength = 0;
  HfrRawData1 *pBand;
  HfrRawData0 *pEu, *pEv, *pEx, *pEz;



  if (CasHfr_bAnalysis (pMp) == false)
    return 0;

  /*
   * stop compliler initialization complaints (for loop ensures init) 
   */
  nRepeat = nStep = nFilter = 0;
  pBand = NULL;
  pEu = pEv = pEx = pEz = NULL;

  /*
   * Find the start of the Autocorrelation Values 
   */
  pData = (pMp + 25);
  pData += CasHfr_GetAgcByteCount (pRec);


  for (nBand = 0; nBand < 5; nBand++) {

    if (nBand < 3) {                    /* Bands ABC */
      if (CasHfr_bAutoABC (pMp) == false)
        continue;
      if (nBand == 0) {
        if (CasHfr_bBandASelected (pMp) == true)
          pBand = &(A.Auto);
        else
          continue;
      } /* Band A */
      else if (nBand == 1) {
        if (CasHfr_bBandBSelected (pMp) == true)
          pBand = &(B.Auto);
        else
          continue;
      } /* Band B */
      else if (nBand == 2) {
        if (CasHfr_bBandCSelected (pMp) == true)
          pBand = &(C.Auto);
        else
          continue;
      } /* Band C */
      else {
        assert (0);
      }
      nFilter = CasHfr_nFiltersABC (pMp);
      nStep = 1;
      nRepeat = CasHfr_nRepeatCountABC (pMp) * CasHfr_nRepeatCountAll (pMp);
      bDirFind = CasHfr_bDirectionFindingABC (pMp);
      bEuOn = CasHfr_bEuOnABC (pMp);
      bEvOn = CasHfr_bEvOnABC (pMp);
      bExOn = CasHfr_bExOnABC (pMp);
      bEzOn = CasHfr_bEzOnABC (pMp);
    } /* Bands ABC */
    else if (nBand == 3) {
      if (CasHfr_bAutoHF1 (pMp) == false)
        continue;
      if (CasHfr_bBandHF1Selected (pMp) == true)
        pBand = &(HF1.Auto);
      else
        continue;
      nFilter = CasHfr_nFiltersHF1 (pMp);
      nStep = CasHfr_nNumberStepsHF1 (pMp);
      nRepeat = CasHfr_nRepeatCountHF1 (pMp) * CasHfr_nRepeatCountAll (pMp);
      bDirFind = CasHfr_bDirectionFindingHF1 (pMp);
      bEuOn = CasHfr_bEuOnHF1 (pMp);
      bEvOn = CasHfr_bEvOnHF1 (pMp);
      bExOn = CasHfr_bExOnHF1 (pMp);
      bEzOn = CasHfr_bEzOnHF1 (pMp);
    } /* Band HF1 */
    else if (nBand == 4) {
      if (CasHfr_bAutoHF2 (pMp) == false)
        continue;
      if (CasHfr_bBandHF2Selected (pMp) == true)
        pBand = &(HF2.Auto);
      else
        continue;
      nFilter = CasHfr_nFiltersHF2 (pMp);
      nStep = CasHfr_nNumberStepsHF2 (pMp);
      nRepeat = CasHfr_nRepeatCountHF2 (pMp) * CasHfr_nRepeatCountAll (pMp);
      bDirFind = CasHfr_bDirectionFindingHF2 (pMp);
      bEuOn = CasHfr_bEuOnHF2 (pMp);
      bEvOn = CasHfr_bEvOnHF2 (pMp);
      bExOn = CasHfr_bExOnHF2 (pMp);
      bEzOn = CasHfr_bEzOnHF2 (pMp);
    } /* Band HF2 */
    else {
      assert (0);
    }


    if (bDirFind == true) {
      pEu = &(pBand->Eu);
      pEv = &(pBand->Ev);
      pEz = &(pBand->Ez);
      for (r = 0; r < nRepeat; r++) {
        for (s = 0; s < nStep; s++) {
          for (f = 0; f < nFilter; f++)
            pEu->data[pEu->length++] = *pData++;
          for (f = 0; f < nFilter; f++)
            pEv->data[pEv->length++] = *pData++;
        }
      }
      for (r = 0; r < nRepeat; r++) {
        for (s = 0; s < nStep; s++) {
          for (f = 0; f < nFilter; f++)
            pEz->data[pEz->length + f] = *pData++;
          for (f = 0; f < nFilter; f++)
            pEz->data[pEz->length + nStep * nFilter + f] = *pData++;    /* raj 03-09-26 */
          pEz->length += f;             /* so far, only counting first sweep */
        }
        pEz->length += (nStep * nFilter);       /* add in 2nd sweep count */
      }
    } /* direction finding */
    else {
      if (bEuOn == true)
        pEx = &(pBand->Eu);
      else if (bEvOn == true)
        pEx = &(pBand->Ev);
      else if (bExOn == true)
        pEx = &(pBand->Ex);
      else
        pEx = NULL;
      if (pEx != NULL) {
        for (r = 0; r < nRepeat; r++) {
          for (s = 0; s < nStep; s++) {
            for (f = 0; f < nFilter; f++)
              pEx->data[pEx->length++] = *pData++;
          }
        }
      }
      if (bEzOn == true)
        pEz = &(pBand->Ez);
      else
        pEz = NULL;
      if (pEz != NULL) {
        for (r = 0; r < nRepeat; r++) {
          for (s = 0; s < nStep; s++) {
            for (f = 0; f < nFilter; f++)
              pEz->data[pEz->length++] = *pData++;
          }
        }
      }
    }                                   /* normal */

  }                                     /* for loop */
  nLength = pData - (pMp + 25 + CasHfr_GetAgcByteCount (pRec));



  return nLength;
}                                       /* hfr analysis extract auto */



Ulong CasHfrAnalysis_XtractCross (CasRecord * pRec)
{
  return 0;
}                                       /* hfr analysis extract cross */


float CasHfrAnalysis_fDuration (CasRecord * pRec, Ulong nType)
{
  Uchar *pMp = pRec->data;
  Ulong nBnd, nAnt, nMode, nInt, nStep, nFil;
  float fCycleABC, fCycleHF1, fCycleHF2;
  float fDuration;

  if (nType == 0x00)                    /* calculate length for whole packet */
    nType = CasHfr_Analysis | CasHfr_BandAll | CasAntAll;

  assert (CasHfr_bAnalysis (pMp) == true);

  nInt = CasHfr_IntegrationTimeABC (pMp);
  nFil = CasHfr_FiltersABC (pMp);
  nBnd = nAnt = 0;
  if (CasHfr_bBandASelected (pMp) == true)
    ++nBnd;
  if (CasHfr_bBandBSelected (pMp) == true)
    ++nBnd;
  if (CasHfr_bBandCSelected (pMp) == true)
    ++nBnd;
  if (CasHfr_bEwOnABC (pMp) == true)
    ++nAnt;
  if (CasHfr_bEuOnABC (pMp) == true)
    ++nAnt;
  if (CasHfr_bEvOnABC (pMp) == true)
    ++nAnt;
  if (CasHfr_bExOnABC (pMp) == true)
    ++nAnt;

  if (nBnd > 0) {
    if (CasHfr_bDirectionFindingABC (pMp) == true)
      nMode = 3;
    else if (nAnt == 1)
      nMode = 0;
    else if (nAnt == 2) {
      if ((CasHfr_bAutoABC (pMp) == true) && (CasHfr_bCrossABC (pMp) == true))
        nMode = 2;
      else if ((CasHfr_bAutoABC (pMp) == true)
               || (CasHfr_bCrossABC (pMp) == true))
        nMode = 1;
      else {                            /* agc only with two antennas? */
        fprintf (stderr,
                 "bad hfr packet, Band ABC agc only with 2 antennas.  "
                 "cds_time=%08X\n", pRec->status.cds_time);
        nMode = 1;                      /* should be 1E * 20%, rather than 2E a|c */
      }
    } else {
      return 0.0;                       /* bad hfr analysis packet */
      /*
       * fprintf(stderr,"bad hfr packet, Band ABC too few/many antennas.  "
       * "cds_time=%08X\n",pRec->status.cds_time);
       * fprintf(stderr,"  Eu=%s Ev=%s Ex=%s Ew=%s, A=%s B=%s C=%s\n",
       * CasHfr_bEuOnABC(pMp)==true?"on":"off",
       * CasHfr_bEvOnABC(pMp)==true?"on":"off",
       * CasHfr_bExOnABC(pMp)==true?"on":"off",
       * CasHfr_bEwOnABC(pMp)==true?"on":"off", 
       * CasHfr_bBandASelected(pMp)==true?"on":"off",
       * CasHfr_bBandBSelected(pMp)==true?"on":"off",
       * CasHfr_bBandCSelected(pMp)==true?"on":"off");
       * 
       * nMode=1; 
       *//*
       * any ideas as what to put/do here 
       */
    }


    fCycleABC = ABC_CycleTime[nInt][nMode][nFil];
    if (nBnd == 1)
      fCycleABC *= 1.0 / 3.0;
    if (nBnd == 2)
      fCycleABC *= 2.0 / 3.0;
    fCycleABC *= CasHfr_nRepeatCountABC (pMp);
  } else
    fCycleABC = 0.0;


  /*
   * hf1 
   */
  if (CasHfr_bBandHF1Selected (pMp) == true) {
    nAnt = 0;
    if (CasHfr_bEwOnHF1 (pMp) == true)
      ++nAnt;
    if (CasHfr_bEuOnHF1 (pMp) == true)
      ++nAnt;
    if (CasHfr_bEvOnHF1 (pMp) == true)
      ++nAnt;
    if (CasHfr_bExOnHF1 (pMp) == true)
      ++nAnt;
    nInt = CasHfr_IntegrationTimeHF1 (pMp);
    nStep = CasHfr_nNumberStepsHF1 (pMp);
    if (CasHfr_bDirectionFindingHF1 (pMp) == true)
      nMode = 4;
    else if (nAnt == 1)
      nMode = 0;
    else if (nAnt == 2) {
      if ((CasHfr_bAutoHF1 (pMp) == true) && (CasHfr_bCrossHF1 (pMp) == true))
        nMode = 3;
      else if ((CasHfr_bAutoHF1 (pMp) == true)
               || (CasHfr_bCrossHF1 (pMp) == true))
        nMode = 2;
      else
        nMode = 1;
    } /* two antennas */
    else {
      return 0.0;                       /* bad hfr analysis packet */
      /*
       * fprintf(stderr,"bad hfr packet, Band HF1 too few/many antennas.  "
       * "cds_time=%08X\n",pRec->status.cds_time);
       * fprintf(stderr,"  Eu=%s Ev=%s Ex=%s Ew=%s, HF1=%s\n",
       * CasHfr_bEuOnHF1(pMp)==true?"on":"off",
       * CasHfr_bEvOnHF1(pMp)==true?"on":"off",
       * CasHfr_bExOnHF1(pMp)==true?"on":"off",
       * CasHfr_bEwOnHF1(pMp)==true?"on":"off", 
       * CasHfr_bBandHF1Selected(pMp)==true?"on":"off");
       * 
       * nMode=1; 
       *//*
       * any ideas as what to put/do here 
       */
    }

    fCycleHF1 = HF1_InitStepTime[nInt][nMode];
    --nStep;
    if (nStep > 0)
      fCycleHF1 += nStep * HF1_PrecStepTime[nInt][nMode];
    fCycleHF1 *= CasHfr_nRepeatCountHF1 (pMp);
  } else
    fCycleHF1 = 0.0;


  if (CasHfr_bBandHF2Selected (pMp) == true) {
    nAnt = 0;
    if (CasHfr_bEwOnHF2 (pMp) == true)
      ++nAnt;
    if (CasHfr_bEuOnHF2 (pMp) == true)
      ++nAnt;
    if (CasHfr_bEvOnHF2 (pMp) == true)
      ++nAnt;
    if (CasHfr_bExOnHF2 (pMp) == true)
      ++nAnt;
    nInt = CasHfr_IntegrationTimeHF2 (pMp);
    nStep = CasHfr_nNumberStepsHF2 (pMp);
    if (CasHfr_bDirectionFindingHF2 (pMp) == true)
      nMode = 4;
    else if (nAnt == 1)
      nMode = 0;
    else if (nAnt == 2) {
      if ((CasHfr_bAutoHF2 (pMp) == true) && (CasHfr_bCrossHF2 (pMp) == true))
        nMode = 3;
      else if ((CasHfr_bAutoHF2 (pMp) == true)
               || (CasHfr_bCrossHF2 (pMp) == true))
        nMode = 2;
      else
        nMode = 1;
    } /* two antennas */
    else {
      return 0.0;                       /* bad hfr analysis packet */
      /*
       * fprintf(stderr,"bad hfr packet, Band HF2 too few/many antennas.  "
       * "cds_time=%08X\n",pRec->status.cds_time);
       * fprintf(stderr,"  Eu=%s Ev=%s Ex=%s Ew=%s, HF2=%s\n",
       * CasHfr_bEuOnHF2(pMp)==true?"on":"off",
       * CasHfr_bEvOnHF2(pMp)==true?"on":"off",
       * CasHfr_bExOnHF2(pMp)==true?"on":"off",
       * CasHfr_bEwOnHF2(pMp)==true?"on":"off", 
       * CasHfr_bBandHF2Selected(pMp)==true?"on":"off");
       * 
       * nMode=1; 
       *//*
       * any ideas as what to put/do here 
       */
    }
    fCycleHF2 = HF2_InitStepTime[nInt][nMode];
    --nStep;
    if (nStep > 0)
      fCycleHF2 += nStep * HF2_PrecStepTime[nInt][nMode];
    fCycleHF2 *= CasHfr_nRepeatCountHF2 (pMp);
  } else
    fCycleHF2 = 0.0;

  /*
   * fprintf(stderr,"Hfr Cycle Times: ABC=%.4f HF1=%.4f HF2=%.4f\n",
   * fCycleABC,fCycleHF1,fCycleHF2);
   */

  fDuration = fCycleABC + fCycleHF1 + fCycleHF2;
  fDuration *= CasHfr_nRepeatCountAll (pMp);
  fDuration /= 1.0E3;                   /* mSec to Sec */


  return fDuration;
}



/* fix hfr hf1 and hf2 step timing */

/* jan 28, 2004 - possible bug with modes like hf1 auto off cross on */
void CasHfrAnalysis_InstrumentTiming (CasRecord * pRec)
{
  unsigned char *pMp = pRec->data;

  int nFilterABC, nRepeatABC, fabc, rabc;
  int nFilterHF1, nStepHF1, nRepeatHF1, fhf1, shf1, rhf1;
  int nFilterHF2, nStepHF2, nRepeatHF2, fhf2, shf2, rhf2;
  int nRepeatAll, rall;

  float fA, fB, fC, fHF1i, fHF1, fHF2i, fHF2, fOffset;

  HfrCalData0 *pAv, *pAx, *pAz, *pBv, *pBx, *pBz, *pCv, *pCx, *pCz;
  HfrCalData0 *pHF1v, *pHF1x, *pHF1z, *pHF2v, *pHF2x, *pHF2z;



  fA = fB = fC = fHF1i = fHF1 = fHF2i = fHF2 = fOffset = 0.0;
  pAv = pAx = pAz = pBv = pBx = pBz = pCv = pCx = pCz = NULL;
  pHF1v = pHF1x = pHF1z = pHF2v = pHF2x = pHF2z = NULL;

/* 
  decode the mini packet header, for non-direction finding modes use the 
  pointer to the X antenna for Eu,Ev,Ex.  If in direction finding mode,
  use the pointer to the V antenna for Ev and x pointer for Eu. 
*/
  if (CasHfr_bBandASelected (pMp) == true) {
    if (CasHfr_bDirectionFindingABC (pMp) == true) {
      pAx = &(A.dbAuto.Eu);
      pAz = &(A.dbAuto.Ez);
      pAv = &(A.dbAuto.Ev);
    } else {
      if (CasHfr_bEuOnABC (pMp) == true)
        pAx = &(A.dbAuto.Eu);
      if (CasHfr_bEvOnABC (pMp) == true)
        pAx = &(A.dbAuto.Ev);
      if (CasHfr_bExOnABC (pMp) == true)
        pAx = &(A.dbAuto.Ex);
      if (CasHfr_bEzOnABC (pMp) == true)
        pAz = &(A.dbAuto.Ez);
    }
  }
  if (CasHfr_bBandBSelected (pMp) == true) {
    if (CasHfr_bDirectionFindingABC (pMp) == true) {
      pBx = &(B.dbAuto.Eu);
      pBz = &(B.dbAuto.Ez);
      pBv = &(B.dbAuto.Ev);
    } else {
      if (CasHfr_bEuOnABC (pMp) == true)
        pBx = &(B.dbAuto.Eu);
      if (CasHfr_bEvOnABC (pMp) == true)
        pBx = &(B.dbAuto.Ev);
      if (CasHfr_bExOnABC (pMp) == true)
        pBx = &(B.dbAuto.Ex);
      if (CasHfr_bEzOnABC (pMp) == true)
        pBz = &(B.dbAuto.Ez);
    }
  }
  if (CasHfr_bBandCSelected (pMp) == true) {
    if (CasHfr_bDirectionFindingABC (pMp) == true) {
      pCx = &(C.dbAuto.Eu);
      pCz = &(C.dbAuto.Ez);
      pCv = &(C.dbAuto.Ev);
    } else {
      if (CasHfr_bEuOnABC (pMp) == true)
        pCx = &(C.dbAuto.Eu);
      if (CasHfr_bEvOnABC (pMp) == true)
        pCx = &(C.dbAuto.Ev);
      if (CasHfr_bExOnABC (pMp) == true)
        pCx = &(C.dbAuto.Ex);
      if (CasHfr_bEzOnABC (pMp) == true)
        pCz = &(C.dbAuto.Ez);
    }
  }
  if (CasHfr_bBandHF1Selected (pMp) == true) {
    if (CasHfr_bDirectionFindingHF1 (pMp) == true) {
      pHF1x = &(HF1.dbAuto.Eu);
      pHF1z = &(HF1.dbAuto.Ez);
      pHF1v = &(HF1.dbAuto.Ev);
    } else {
      if (CasHfr_bEuOnHF1 (pMp) == true)
        pHF1x = &(HF1.dbAuto.Eu);
      if (CasHfr_bEvOnHF1 (pMp) == true)
        pHF1x = &(HF1.dbAuto.Ev);
      if (CasHfr_bExOnHF1 (pMp) == true)
        pHF1x = &(HF1.dbAuto.Ex);
      if (CasHfr_bEzOnHF1 (pMp) == true)
        pHF1z = &(HF1.dbAuto.Ez);
    }
  }
  if (CasHfr_bBandHF2Selected (pMp) == true) {
    if (CasHfr_bDirectionFindingHF2 (pMp) == true) {
      pHF2x = &(HF2.dbAuto.Eu);
      pHF2z = &(HF2.dbAuto.Ez);
      pHF2v = &(HF2.dbAuto.Ev);
    } else {
      if (CasHfr_bEuOnHF2 (pMp) == true)
        pHF2x = &(HF2.dbAuto.Eu);
      if (CasHfr_bEvOnHF2 (pMp) == true)
        pHF2x = &(HF2.dbAuto.Ev);
      if (CasHfr_bExOnHF2 (pMp) == true)
        pHF2x = &(HF2.dbAuto.Ex);
      if (CasHfr_bEzOnHF2 (pMp) == true)
        pHF2z = &(HF2.dbAuto.Ez);
    }
  }

  nFilterABC = nRepeatABC = 0;
  if ((CasHfr_bBandASelected (pMp) == true) ||
      (CasHfr_bBandBSelected (pMp) == true) ||
      (CasHfr_bBandCSelected (pMp) == true)) {
    int a, b, c;

    if (CasHfr_bAutoABC (pMp) == true)
      nFilterABC = CasHfr_nFiltersABC (pMp);
    else
      nFilterABC = 1;                   /* need autos for filters to have meaning */
    nRepeatABC = CasHfr_nRepeatCountABC (pMp);

    a = CasHfr_IntegrationTimeABC (pMp);
    c = CasHfr_FiltersABC (pMp);
    b = 0;                              /* count the number of antennas */
    if (CasHfr_bEuOnABC (pMp) == true)
      ++b;
    if (CasHfr_bEvOnABC (pMp) == true)
      ++b;
    if (CasHfr_bExOnABC (pMp) == true)
      ++b;
    if (CasHfr_bEzOnABC (pMp) == true)
      ++b;
    assert ((b < 3) && (b != 0));

    if (b == 1)
      b = 0;
    else if (CasHfr_bDirectionFindingABC (pMp) == true)
      b = 3;
    else if ((CasHfr_bAutoABC (pMp) == true)
             && (CasHfr_bCrossABC (pMp) == true))
      b = 2;
    else
      b = 1;
    fOffset = ABC_CycleTime[a][b][c] * 1E-3;    /* for all three receivers */
    fOffset /= 3.0;                     /* for each of three bands (Ex/Ez) */
    if (CasHfr_bDirectionFindingABC (pMp) == true) {
      fOffset /= 2.0;                   /* nRepeatABC*=2; */
    }                                   /* two set of antennas to sample (Eu/Ez & Ev/Ez) */
    fA = fB = fC = 0;
    if (CasHfr_bBandASelected (pMp) == true)
      fA = fOffset;
    if (CasHfr_bBandBSelected (pMp) == true)
      fB = fOffset;
    if (CasHfr_bBandCSelected (pMp) == true)
      fC = fOffset;
  }
  /*
   * abc 
   */
  nFilterHF1 = nStepHF1 = nRepeatHF1 = 0;
  if (CasHfr_bBandHF1Selected (pMp) == true) {
    int a, b;

    if (CasHfr_bAutoHF1 (pMp) == true)
      nFilterHF1 = CasHfr_nFiltersHF1 (pMp);
    else
      nFilterHF1 = 1;
    nStepHF1 = CasHfr_nNumberStepsHF1 (pMp);
    nRepeatHF1 = CasHfr_nRepeatCountHF1 (pMp);

    a = CasHfr_IntegrationTimeHF1 (pMp);
    b = 0;                              /* count the number of antennas */
    if (CasHfr_bEuOnHF1 (pMp) == true)
      ++b;
    if (CasHfr_bEvOnHF1 (pMp) == true)
      ++b;
    if (CasHfr_bExOnHF1 (pMp) == true)
      ++b;
    if (CasHfr_bEzOnHF1 (pMp) == true)
      ++b;
    assert ((b < 3) && (b != 0));

    if (CasHfr_bDirectionFindingHF1 (pMp) == true)
      b = 4;
    else if (b == 1)
      b = 0;
    else if ((CasHfr_bAutoHF1 (pMp) == true)
             && (CasHfr_bCrossHF1 (pMp) == true))
      b = 3;
    else if ((CasHfr_bAutoHF1 (pMp) == true)
             || (CasHfr_bCrossHF1 (pMp) == true))
      b = 2;
    else
      b = 1;
    fHF1i = HF1_InitStepTime[a][b] * 1E-3;
    fHF1 = HF1_PrecStepTime[a][b] * 1E-3;
    if (CasHfr_bDirectionFindingHF1 (pMp) == true) {
      fHF1i /= 2.0;
      fHF1 /= 2.0;                      /* nRepeatHF1*=2; */
    }                                   /* two sets of antennas to sample */
  }
  /*
   * hf1 
   */
  nFilterHF2 = nStepHF2 = nRepeatHF2 = 0;
  if (CasHfr_bBandHF2Selected (pMp) == true) {
    int a, b;

    if (CasHfr_bAutoHF2 (pMp) == true)
      nFilterHF2 = CasHfr_nFiltersHF2 (pMp);
    else
      nFilterHF2 = 1;
    nStepHF2 = CasHfr_nNumberStepsHF2 (pMp);
    nRepeatHF2 = CasHfr_nRepeatCountHF2 (pMp);

    a = CasHfr_IntegrationTimeHF2 (pMp);
    b = 0;                              /* count the number of antennas */
    if (CasHfr_bEuOnHF2 (pMp) == true)
      ++b;
    if (CasHfr_bEvOnHF2 (pMp) == true)
      ++b;
    if (CasHfr_bExOnHF2 (pMp) == true)
      ++b;
    if (CasHfr_bEzOnHF2 (pMp) == true)
      ++b;
    assert ((b < 3) && (b != 0));

    if (CasHfr_bDirectionFindingHF2 (pMp) == true)
      b = 4;
    else if (b == 1)
      b = 0;
    else if ((CasHfr_bAutoHF2 (pMp) == true)
             && (CasHfr_bCrossHF2 (pMp) == true))
      b = 3;
    else if ((CasHfr_bAutoHF2 (pMp) == true)
             || (CasHfr_bCrossHF2 (pMp) == true))
      b = 2;
    else
      b = 1;
    fHF2i = HF2_InitStepTime[a][b] * 1E-3;
    fHF2 = HF2_PrecStepTime[a][b] * 1E-3;
    if (CasHfr_bDirectionFindingHF2 (pMp) == true) {
      fHF2i /= 2.0;
      fHF2 /= 2.0;                      /* nRepeatHF2*=2; */
    }                                   /* two sets of antennas to sample */
  }
  /*
   * hf2 
   */
  nRepeatAll = CasHfr_nRepeatCountAll (pMp);
  fOffset = 0.0;
  for (rall = 0; rall < nRepeatAll; rall++) {

    /*
     * Band A 
     */
    for (rabc = 0; rabc < nRepeatABC; rabc++) {
      if (pAx != NULL) {
        for (fabc = 0; fabc < nFilterABC; fabc++)
          pAx->time[pAx->nTmLen++] = fOffset;
      }
      if (pAz != NULL) {
        for (fabc = 0; fabc < nFilterABC; fabc++)
          pAz->time[pAz->nTmLen++] = fOffset;
      }
      fOffset += fA;
      if (pAv != NULL) {                /* Direction Finding Mode */
        for (fabc = 0; fabc < nFilterABC; fabc++) {
          pAv->time[pAv->nTmLen++] = fOffset;
          pAz->time[pAz->nTmLen++] = fOffset;
        }
        fOffset += fA;
      }
    }                                   /* end band a */
    /*
     * Band B 
     */
    for (rabc = 0; rabc < nRepeatABC; rabc++) {
      if (pBx != NULL) {
        for (fabc = 0; fabc < nFilterABC; fabc++)
          pBx->time[pBx->nTmLen++] = fOffset;
      }
      if (pBz != NULL) {
        for (fabc = 0; fabc < nFilterABC; fabc++)
          pBz->time[pBz->nTmLen++] = fOffset;
      }
      fOffset += fB;
      if (pBv != NULL) {                /* Direction Finding Mode */
        for (fabc = 0; fabc < nFilterABC; fabc++) {
          pBv->time[pBv->nTmLen++] = fOffset;
          pBz->time[pBz->nTmLen++] = fOffset;
        }
        fOffset += fB;
      }
    }                                   /* end band b */
    /*
     * Band C 
     */
    for (rabc = 0; rabc < nRepeatABC; rabc++) {
      if (pCx != NULL) {
        for (fabc = 0; fabc < nFilterABC; fabc++)
          pCx->time[pCx->nTmLen++] = fOffset;
      }
      if (pCz != NULL) {
        for (fabc = 0; fabc < nFilterABC; fabc++)
          pCz->time[pCz->nTmLen++] = fOffset;
      }
      fOffset += fC;
      if (pCv != NULL) {                /* Direction Finding Mode */
        for (fabc = 0; fabc < nFilterABC; fabc++) {
          pCv->time[pCv->nTmLen++] = fOffset;
          pCz->time[pCz->nTmLen++] = fOffset;
        }
        fOffset += fC;
      }
    }                                   /* end band c */

    for (rhf1 = 0; rhf1 < nRepeatHF1; rhf1++) {
      if (pHF1x != NULL) {
        for (fhf1 = 0; fhf1 < nFilterHF1; fhf1++)
          pHF1x->time[pHF1x->nTmLen++] = fOffset;
      }
      if (pHF1z != NULL) {              /* since 1st & 2nd sweeps are stored together, bump */
        for (fhf1 = 0; fhf1 < nFilterHF1; fhf1++)       /* the z length at the end */
          pHF1z->time[pHF1z->nTmLen + fhf1] = fOffset;
      }
      fOffset += fHF1i;                 /* time for the initial hfr step frequency */
      if (pHF1v != NULL) {              /* Direction Finding Mode */
        for (fhf1 = 0; fhf1 < nFilterHF1; fhf1++) {
          pHF1v->time[pHF1v->nTmLen++] = fOffset;
          pHF1z->time[pHF1z->nTmLen + nStepHF1 * nFilterHF1 + fhf1] = fOffset;
        }
        fOffset += fHF1i;               /* time for the initial hfr step frequency */
      }
      if (pHF1z != NULL)                /* raj Jan 30, 2005 filter=filter*auot */
        pHF1z->nTmLen += nFilterHF1;    /* fix the hfr length at the end */

      for (shf1 = 1; shf1 < nStepHF1; shf1++) {
        if (pHF1x != NULL) {
          for (fhf1 = 0; fhf1 < nFilterHF1; fhf1++)
            pHF1x->time[pHF1x->nTmLen++] = fOffset;
        }
        if (pHF1z != NULL) {
          for (fhf1 = 0; fhf1 < nFilterHF1; fhf1++)
            pHF1z->time[pHF1z->nTmLen + fhf1] = fOffset;
        }
        fOffset += fHF1;                /* time for the second plus hfr step frequency */
        if (pHF1v != NULL) {            /* Direction Finding Mode */
          for (fhf1 = 0; fhf1 < nFilterHF1; fhf1++) {
            pHF1v->time[pHF1v->nTmLen++] = fOffset;
            pHF1z->time[pHF1z->nTmLen + nStepHF1 * nFilterHF1 + fhf1] =
              fOffset;
          }
          fOffset += fHF1;              /* time for the second plus hfr step frequency */
        }
        if (pHF1z != NULL)              /* raj Jan 30, 2005 filter=filter*auto */
          pHF1z->nTmLen += nFilterHF1;  /* fix the hfr length at the end */
      }                                 /* hf1 steps */

      if (pHF1v != NULL)                /* Direction Finding Mode */
        pHF1z->nTmLen += nStepHF1 * nFilterHF1;
    }                                   /* Band HF1 */

    for (rhf2 = 0; rhf2 < nRepeatHF2; rhf2++) {
      if (pHF2x != NULL) {
        for (fhf2 = 0; fhf2 < nFilterHF2; fhf2++) {
          pHF2x->time[pHF2x->nTmLen++] = fOffset;
        }
      }
      if (pHF2z != NULL) {              /* since 1st & 2nd sweeps are stored together, bump */
        for (fhf2 = 0; fhf2 < nFilterHF2; fhf2++)       /* the z length at the end */
          pHF2z->time[pHF2z->nTmLen + fhf2] = fOffset;
      }
      fOffset += fHF2i;                 /* time for the initial hfr step frequency */
      if (pHF2v != NULL) {              /* Direction Finding Mode */
        for (fhf2 = 0; fhf2 < nFilterHF2; fhf2++) {
          pHF2v->time[pHF2v->nTmLen++] = fOffset;
          pHF2z->time[pHF2z->nTmLen + nStepHF2 * nFilterHF2 + fhf2] = fOffset;
        }
        fOffset += fHF2i;               /* time for the initial hfr step frequency */
      }
      if (pHF2z != NULL)
        pHF2z->nTmLen += nFilterHF2;    /* raj Jan 30, 2004 filter=filter*auto */


      for (shf2 = 1; shf2 < nStepHF2; shf2++) {
        if (pHF2x != NULL) {
          for (fhf2 = 0; fhf2 < nFilterHF2; fhf2++)
            pHF2x->time[pHF2x->nTmLen++] = fOffset;
        }
        if (pHF2z != NULL) {
          for (fhf2 = 0; fhf2 < nFilterHF2; fhf2++)
            pHF2z->time[pHF2z->nTmLen + fhf2] = fOffset;
        }
        fOffset += fHF2;                /* time for the second plus hfr step frequency */
        if (pHF2v != NULL) {            /* Direction Finding Mode */
          for (fhf2 = 0; fhf2 < nFilterHF2; fhf2++) {
            pHF2v->time[pHF2v->nTmLen++] = fOffset;
            pHF2z->time[pHF2z->nTmLen + nStepHF2 * nFilterHF2 + fhf2] =
              fOffset;
          }
          fOffset += fHF2;              /* time for the second plus hfr step frequency */
        }
        if (pHF2z != NULL)              /* we'll double the length at the end */
          pHF2z->nTmLen += nFilterHF2;  /* raj Jan 30, 2004 filter=filter*auto */
      }                                 /* hf2 steps */

      if (pHF2v != NULL)                /* Direction Finding Mode */
        pHF2z->nTmLen += nStepHF2 * nFilterHF2;
    }                                   /* Band HF2 */

  }                                     /* for repeat count all */

}



int CasHfrAnalysis_GetPhysicalUnits (CasRecord * pRec, Ulong nBndAnt,
                                     float *pTime, float *pFreq, float *pAuto)
{
  bool bNewData = true;
  bool bDipole;
  static unsigned char arMp[32];
  unsigned char *pMp = pRec->data;
  int nIdx, nLength = 0;

  Ulong nBand, nAnt;
  HfrCalData0 *pAnt;
  HfrCalData1 *pBand;
  double dTmp, dX;

  if (CasHfr_bAnalysis (pMp) == false)
    return nLength;
  /*
   * strip any bogus antenna information and receiver mode stuff 
   */
  nBndAnt &= (CasHfr_BandABC12 | CasAntEu | CasAntEv | CasAntEx | CasAntEw);

  bNewData = false;                     /* assume data has been calibrated already */
  for (nIdx = 0; nIdx < 25; nIdx++) {
    if (arMp[nIdx] != pMp[nIdx])
      bNewData = true;
    arMp[nIdx] = pMp[nIdx];
  }

  if (bNewData == true) {

    for (nIdx = 0; nIdx < 20; nIdx++) {
      pAGC[nIdx]->length = 0;
      pAUTO[nIdx]->length = 0;
      pAUTO[nIdx]->length = 0;
      pAUTO_MAG[nIdx]->length = 0;      /* magnitude index */
      pAUTO_MAG[nIdx]->nTmLen = 0;      /* time index */
    }

    CasHfrAnalysis_XtractAgc (pRec);
    CasHfrAnalysis_XtractAuto (pRec);
    CasHfrAnalysis_XtractCross (pRec);  /* empty for now */
    CasHfrAnalysis_CalibrateAuto (pRec);
    CasHfrAnalysis_InstrumentTiming (pRec);
  }


  /*
   * calibrate the new mini-packet 
   */
  /*
   * just fetch the already calibrated data 
   */
  /*
   * Taking into account the base capacitance and antenna length.  
   * Monopole Length=5.00m, Monopole Base Capacitance=8.01dB (2.51478x)
   * Dipole Length=9.26m,   Dipole Base Capacitance=7.94dB (2.49459x)
   */
  nLength = 0;
  pBand = NULL;
  pAnt = NULL;                          /* more erronous complier warnings */
  nBand = nBndAnt & CasHfr_BandMask;
  while (nBand) {
    if (nBand & CasHfr_BandA) {
      pBand = &(A.dbAuto);
      nBand &= ~CasHfr_BandA;
    } else if (nBand & CasHfr_BandB) {
      pBand = &(B.dbAuto);
      nBand &= ~CasHfr_BandB;
    } else if (nBand & CasHfr_BandC) {
      pBand = &(C.dbAuto);
      nBand &= ~CasHfr_BandC;
    } else if (nBand & CasHfr_BandHF1) {
      pBand = &(HF1.dbAuto);
      nBand &= ~CasHfr_BandHF1;
    } else if (nBand & CasHfr_BandHF2) {
      pBand = &(HF2.dbAuto);
      nBand &= ~CasHfr_BandHF2;
    } else {
      nBand &= ~CasHfr_BandABC12;
      continue;
    }
    nAnt = nBndAnt & CasHfr_AntMask;
    while (nAnt) {
      if (nAnt & CasAntExp) {
        pAnt = &(pBand->Eu);
        nAnt &= ~CasAntExp;
        bDipole = false;
        dX = (1.0 / 5.00) * 2.51478;
      } else if (nAnt & CasAntExm) {
        pAnt = &(pBand->Ev);
        nAnt &= ~CasAntExm;
        bDipole = false;
        dX = (1.0 / 5.00) * 2.51478;
      } else if (nAnt & CasAntEx) {
        pAnt = &(pBand->Ex);
        nAnt &= ~CasAntEx;
        bDipole = true;
        dX = (1.0 / 9.26) * 2.49459;
      } else if (nAnt & CasAntEz) {
        pAnt = &(pBand->Ez);
        nAnt &= ~CasAntEz;
        bDipole = false;
        dX = (1.0 / 5.00) * 2.51478;
      } else {
        nAnt &= ~CasAntExEz;
        bDipole = false;
        continue;
      }
      if (pAuto != NULL) {
        for (nIdx = 0; nIdx < pAnt->length; nIdx++) {
          pAuto[nLength + nIdx] = pAnt->data[nIdx];     /* dB(Vrms/rtHz) */
          dTmp = pAnt->data[nIdx];      /* dB(Vrms/rtHz) */
          dTmp = pow (10.0, (dTmp / 20.0));     /* Vrms/rtHz */
          dTmp *= dX;                   /* Vrms/m/rtHz, with base capacitance */
          dTmp *= dTmp;                 /* V^2/m^2/Hz */
          pAuto[nLength + nIdx] = dTmp; /* V^2/m^2/Hz */
        }
      }
      if (pFreq != NULL) {
        for (nIdx = 0; nIdx < pAnt->length; nIdx++)
          pFreq[nLength + nIdx] = pAnt->freq[nIdx];
      }
      if (pTime != NULL) {
        for (nIdx = 0; nIdx < pAnt->length; nIdx++)
          pTime[nLength + nIdx] = pAnt->time[nIdx];
      }

      if (pAuto != NULL) {
        double dResCor, dFreq;

        if (bDipole == true) {
          for (nIdx = 0; nIdx < pAnt->length; nIdx++) {
            dFreq = pFreq[nLength + nIdx];
            dResCor = 1.0 - dFreq * dFreq / ((8.775e6) * (8.775e6));
            dResCor =
              (1.0 + 0.58 / 27.49) / (dResCor * dResCor + (0.58 / 27.49));
            pAuto[nLength + nIdx] /= dResCor;
          }
        } else {
          for (nIdx = 0; nIdx < pAnt->length; nIdx++) {
            dFreq = pFreq[nLength + nIdx];
            dResCor = 1.0 - dFreq * dFreq / ((9.575e6) * (9.575e6));
            dResCor =
              (1.0 + (0.1255 * 0.1255)) / (dResCor * dResCor +
                                           (0.1255 * 0.1255));
            pAuto[nLength + nIdx] /= dResCor;
          }
        }                               /*else */

      }
      /*
       * antenna correction 
       */
      nLength += nIdx;
    }                                   /* Antenna */
  }                                     /* Band */



  return nLength;
}



/* save static string for error messages */
char *CasHfrMillisecond_sValidPacket (CasRecord * pRec)
{
  unsigned char *pMp = pRec->data;
  Ulong nHfrLen, nMpLen, nNumSamples;
  static char sErr[128];

  /*
   * Header Check 
   */
  if (CasHfr_bMillisecond (pMp) == false) {
    sprintf (sErr, "Hfr Ms (cdstime=%08X) - Not Millisecond Mode, %02X",
             pRec->status.cds_time, pMp[7]);
    return sErr;
  }
  if (CasHfr_bMemoryOverrun (pMp) == true) {
    sprintf (sErr, "Hfr Ms (cdstime=%08x) - Memory Overrun, 0x%02X",
             pRec->status.cds_time, pMp[7]);
    return sErr;
  }
  if (CasHfr_nHeaderVersion (pMp) != 0x05) {
    sprintf (sErr, "Hfr Ms (cdstime=%08X) - Invalid Header Version, 0x%02X",
             pRec->status.cds_time, pMp[8]);
    return sErr;
  }
  if ((pMp[11] & 0xFE) != 0x00) {       /* bits 7->1 are spare, 0 - hfr hf1/hf2 */
    sprintf (sErr,
             "Hfr Ms (cdstime=%08X) - Invalid Header Termination, 0x%02X",
             pRec->status.cds_time, pMp[11]);
    return sErr;
  }

  nHfrLen = CasHfr_nPacketSize (pMp);
  nMpLen = pRec->status.packet_length;
  nNumSamples = CasHfrMsc_nNumberOfSamples (pMp);

  if (nNumSamples + 6 != nHfrLen) {
    sprintf (sErr, "Hfr Ms (cdstime=%08X) - Length Check Failed "
             "HfrLen=%d, Samples+6=%d",
             pRec->status.cds_time, nHfrLen, nNumSamples + 6);
    return sErr;
  }
  if ((nMpLen + 3) != (nHfrLen + 7)) {  /* length integrity, total bytes in packet */
    sprintf (sErr, "Hfr Ms (cdstime=%08X) - Length Check Failed "
             "MpLen+3=%d, HfrLen+7=%d",
             pRec->status.cds_time, nMpLen + 3, nHfrLen + 7);
    return sErr;
  }
  if (pMp[nHfrLen + 6] != 0x5A) {
    sprintf (sErr,
             "Hfr Ms (cdstime=%08X) - Invalid Data Termination, 0x%02X",
             pRec->status.cds_time, pMp[nHfrLen + 6]);
    return sErr;
  }

/* raj milli */

  return NULL;
}



Ulong CasHfrMillisecond_ByteCount (CasRecord * pRec, Ulong nType)
{
  unsigned char *pMp = pRec->data;
  int nTotal;
  Ulong nHfrBand, nHfrAnt;

  nTotal = CasHfrMsc_nNumberOfSamples (pMp);
  /*
   * sanity check 
   */
  if (nTotal != (CasHfr_nPacketSize (pMp) - 6)) {
    fprintf (stderr, "bad hfr millisecond mode byte count\n");
    CasHfr_DumpRecord (pRec);
    assert (nTotal == (CasHfr_nPacketSize (pMp) - 6));
  }

  if ((nHfrBand = (nType & CasHfr_BandMask)) == 0)
    return 0;
  if ((nHfrAnt = (nType & CasAntMask)) == 0)
    return 0;

  /*
   * Millisecond mode only contains Agc data for either HF1 or HF2 
   */
  if ((nHfrAnt & CasAntEu) && (CasHfrMsc_bEuOn (pMp) == true))  /* null */
    ;
  else if ((nHfrAnt & CasAntEv) && (CasHfrMsc_bEvOn (pMp) == true))     /* null */
    ;
  else if ((nHfrAnt & CasAntEx) && (CasHfrMsc_bExOn (pMp) == true))     /* null */
    ;
  else if ((nHfrAnt & CasAntEz) && (CasHfrMsc_bEzOn (pMp) == true))     /* null */
    ;
  else
    return 0;

  if ((nHfrBand & CasHfr_BandHF1) && (CasHfrMsc_bHF1 (pMp) == true))    /* null */
    ;
  else if ((nHfrBand & CasHfr_BandHF2) && (CasHfrMsc_bHF2 (pMp) == true))       /* null */
    ;
  else
    return 0;

  return nTotal;
}



float CasHfrMillisecond_fDuration (CasRecord * pRec, Ulong nType)
{
  Uchar *pMp = pRec->data;
  int nSamples;
  float fSampleRate;
  float fDuration;

  if (nType == 0x00)                    /* calculate length for whole packet */
    nType = CasHfr_Millisecond | CasHfr_BandAll | CasAntAll;

  assert (CasHfr_bMillisecond (pMp) == true);

  fSampleRate = CasHfrMsc_fSampleRate (pMp);
  nSamples = CasHfrMsc_nNumberOfSamples (pMp);
  fDuration = fSampleRate * nSamples;

  return fDuration;
}


int CasHfrMillisecond_GetPhysicalUnits (CasRecord * pRec,
                                        Ulong nBndAnt, float *pTime,
                                        float *pFreq, float *pAuto)
{
  unsigned char *pMp = pRec->data;
  static unsigned char arMp[32];

  bool bNewData = true, bAttnOn = false;
  int nIdx, nLength = 0;

  unsigned char *pBuf;
  int nSamples, nStep, nAnt;
  float A1, A2, A3, fAgc, fTime;
  float fSampleRate, fFrq;
  HfrRawData0 *pAgc;
  HfrCalData0 *pAutoMag;



  if (CasHfr_bMillisecond (pMp) == false)
    return 0;

  /*
   * Byte Count is the filter for hfr band and antenna combination 
   */
  if ((nLength = CasHfrMillisecond_ByteCount (pRec, nBndAnt)) == 0)
    return 0;

  bNewData = false;                     /* assume data has been calibrated already */
  for (nIdx = 0; nIdx < CasHfrMsc_HeaderLength; nIdx++) {
    if (arMp[nIdx] != pMp[nIdx])
      bNewData = true;
    arMp[nIdx] = pMp[nIdx];
  }

  if (bNewData == true) {

    for (nIdx = 0; nIdx < 20; nIdx++) {
      pAGC[nIdx]->length = 0;
      pAUTO[nIdx]->length = 0;
      pAUTO[nIdx]->length = 0;
      pAUTO_MAG[nIdx]->length = 0;      /* magnitude index */
      pAUTO_MAG[nIdx]->nTmLen = 0;      /* time index */
    }

    fSampleRate = CasHfrMsc_fSampleRate (pMp);
    nSamples = CasHfrMsc_nNumberOfSamples (pMp);
    nStep = CasHfrMsc_Frequency (pMp);  /* 50KHz steps for HF1, 100KHz for HF2 */
    fFrq = CasHfrMsc_fFrequency (pMp);
    bAttnOn = false;                    /* hfr 30dB internal attenuator, no status in header */

    /*
     * Select the calibration constants 
     */
    nAnt = 0;
    if (CasHfrMsc_bEuOn (pMp) == true)
      nAnt = 0;
    else if (CasHfrMsc_bEvOn (pMp) == true)
      nAnt = 1;
    else if (CasHfrMsc_bExOn (pMp) == true)
      nAnt = 2;
    else if (CasHfrMsc_bEzOn (pMp) == true)
      nAnt = 3;
    else
      assert (0);

    if (CasHfrMsc_bHF1 (pMp) == true) { /* HF1 */
      if (bAttnOn == false)
        A1 = A1HF1[nAnt][nStep];
      else
        A1 = A1HF1[nAnt + 4][nStep];
      assert (nStep < MAX_HF1_A1_CONSTANTS);
      pAgc = pAGC[12 + nAnt];           /* Start of the HF1 data sets */
      pAutoMag = pAUTO_MAG[12 + nAnt];
    } else if (CasHfrMsc_bHF2 (pMp) == true) {  /* HF2 */
      nStep *= 2;                       /* A1 Constant table is indexed at 50KHz steps */
      if (bAttnOn == false)
        A1 = A1HF2[nAnt][nStep];
      else
        A1 = A1HF2[nAnt + 4][nStep];
      assert (nStep < MAX_HF2_A1_CONSTANTS);
      pAgc = pAGC[16 + nAnt];           /* Start of the HF2 data sets */
      pAutoMag = pAUTO_MAG[16 + nAnt];
    } else {
      assert (0);
    }

    if (nAnt < 3) {                     /* Any Ex Antenna */
      A2 = A123[6][1];
      A3 = A123[6][2];
    } else {                            /* Ez Antenna */
      A2 = A123[7][1];
      A3 = A123[7][2];
    }

    /*
     * for consistancy copy the agc values 
     */
    pBuf = pMp + CasHfrMsc_HeaderLength;
    for (nIdx = 0; nIdx < nSamples; nIdx++)
      pAgc->data[nIdx] = *pBuf++;
    pAgc->length = nIdx;

/*
fprintf(stderr,"%7.2f %7.2f %7.2f\n",A1,A2,A3);
*/
    A2 = 1.0 / A2;                      /* invert for multiply */
    fTime = 0.0;
    for (nIdx = 0; nIdx < nSamples; nIdx++) {
      fAgc = (pAgc->data[nIdx] - A3) * A2;
      fAgc = pow (10.0, fAgc);
      fAgc = 40.0 * log10 (fAgc + 1.0);
      fAgc = fAgc - A1;
      pAutoMag->data[nIdx] = fAgc;
      pAutoMag->freq[nIdx] = fFrq;
      pAutoMag->time[nIdx] = fTime;
      fTime += fSampleRate;
    }
    pAutoMag->length = nIdx;
    pAutoMag->nTmLen = nIdx;

    /*
     * convert dBv/rtHz to V^2/m^2/Hz 
     */
    if (BALESE == false) {
      double dTmp, dFreq = fFrq, dResCor, dBasCap;

      /*
       * Base Capacitance V = V * BasCap 
       */
      /*
       * Antenna Resonance Correction V^2 = V^2 / ResCor 
       */
      if (nAnt == 2) {                  /* dipole antenna Ex (Ex+/-) */
        dBasCap = (1.0 / 9.26) * 2.49459;
        dResCor = 1.0 - dFreq * dFreq / ((8.775e6) * (8.775e6));
        dResCor = (1.0 + 0.58 / 27.49) / (dResCor * dResCor + (0.58 / 27.49));
      } else {                          /* monopole antenna Eu,Ev,Ew (Ex+,Ex-,Ez) */
        dBasCap = (1.0 / 5.00) * 2.51478;
        dResCor = 1.0 - dFreq * dFreq / ((9.575e6) * (9.575e6));
        dResCor =
          (1.0 + (0.1255 * 0.1255)) / (dResCor * dResCor + (0.1255 * 0.1255));
      }                                 /*else  magnitude/=dResCor */

      for (nIdx = 0; nIdx < nSamples; nIdx++) {
        dTmp = pAutoMag->data[nIdx];    /* dB(Vrms/rtHz) */
        dTmp = pow (10.0, (dTmp / 20.0));       /* Vrms/rtHz */
        dTmp *= dBasCap;                /* Vrms/m/rtHz, with base capacitance */
        dTmp *= dTmp;                   /* V^2/m^2/Hz */
        dTmp /= dResCor;                /* V^2/m^2/Hz with Ant Res Cor */
        pAutoMag->data[nIdx] = dTmp;    /* V^2/m^2/Hz */
      }
    }                                   /* fi not balese */
  }

  /*
   * calibrate the new data 
   */
  /*
   * find data 
   */
  for (nIdx = 12; nIdx < 20; nIdx++) {  /* A=0-3,B=4-7,C=8-11,HF1=12-15,HF2=16-19 */
    if (pAUTO_MAG[nIdx]->length != 0)
      break;
  }
  pAutoMag = pAUTO_MAG[nIdx];

  nLength = pAutoMag->length;
  if (pTime != NULL) {
    for (nIdx = 0; nIdx < nLength; nIdx++)
      pTime[nIdx] = pAutoMag->time[nIdx];
  }
  if (pFreq != NULL) {
    for (nIdx = 0; nIdx < nLength; nIdx++)
      pFreq[nIdx] = pAutoMag->freq[nIdx];
  }
  if (pAuto != NULL) {
    for (nIdx = 0; nIdx < nLength; nIdx++)
      pAuto[nIdx] = pAutoMag->data[nIdx];
  }


/* raj determine if band and antenna exist */
  return nLength;
}



float CasHfrAnalysis_fSampleWidth (CasRecord * pRec, Ulong nType)
{
  Uchar *pMp = pRec->data;
  Ulong nBnd, nAnt, nMode, nInt, nStep, nFil;
  float fCycleABC, fCycleHF1, fCycleHF2;
  float fDuration;

  assert (CasHfr_bAnalysis (pMp) == true);

  nInt = CasHfr_IntegrationTimeABC (pMp);
  nFil = CasHfr_FiltersABC (pMp);
  nBnd = nAnt = 0;
  if (CasHfr_bBandASelected (pMp) == true)
    ++nBnd;
  if (CasHfr_bBandBSelected (pMp) == true)
    ++nBnd;
  if (CasHfr_bBandCSelected (pMp) == true)
    ++nBnd;
  if (CasHfr_bEwOnABC (pMp) == true)
    ++nAnt;
  if (CasHfr_bEuOnABC (pMp) == true)
    ++nAnt;
  if (CasHfr_bEvOnABC (pMp) == true)
    ++nAnt;
  if (CasHfr_bExOnABC (pMp) == true)
    ++nAnt;

  if (nBnd > 0) {
    if (CasHfr_bDirectionFindingABC (pMp) == true)
      nMode = 3;
    else if (nAnt == 1)
      nMode = 0;
    else if (nAnt == 2) {
      if ((CasHfr_bAutoABC (pMp) == true) && (CasHfr_bCrossABC (pMp) == true))
        nMode = 2;
      else if ((CasHfr_bAutoABC (pMp) == true)
               || (CasHfr_bCrossABC (pMp) == true))
        nMode = 1;
      else {                            /* agc only with two antennas? */
        fprintf (stderr,
                 "bad hfr packet, Band ABC agc only with 2 antennas.  "
                 "cds_time=%08X\n", pRec->status.cds_time);
        nMode = 1;                      /* should be 1E * 20%, rather than 2E a|c */
      }
    } else {
      fprintf (stderr, "bad hfr packet, Band ABC too few/many antennas.  "
               "cds_time=%08X\n", pRec->status.cds_time);
      fprintf (stderr, "  Eu=%s Ev=%s Ex=%s Ew=%s, A=%s B=%s C=%s\n",
               CasHfr_bEuOnABC (pMp) == true ? "on" : "off",
               CasHfr_bEvOnABC (pMp) == true ? "on" : "off",
               CasHfr_bExOnABC (pMp) == true ? "on" : "off",
               CasHfr_bEwOnABC (pMp) == true ? "on" : "off",
               CasHfr_bBandASelected (pMp) == true ? "on" : "off",
               CasHfr_bBandBSelected (pMp) == true ? "on" : "off",
               CasHfr_bBandCSelected (pMp) == true ? "on" : "off");

      nMode = 1;                        /* any ideas as what to put/do here */
    }


    fCycleABC = ABC_CycleTime[nInt][nMode][nFil];
    fCycleABC /= 3.0;
    fCycleABC *= 1.0E-3;
    if (nType & CasHfr_BandABC)
      return fCycleABC;

    fCycleABC *= CasHfr_nRepeatCountABC (pMp);
  } else
    fCycleABC = 0.0;


  /*
   * hf1 
   */
  if (CasHfr_bBandHF1Selected (pMp) == true) {
    nAnt = 0;
    if (CasHfr_bEwOnHF1 (pMp) == true)
      ++nAnt;
    if (CasHfr_bEuOnHF1 (pMp) == true)
      ++nAnt;
    if (CasHfr_bEvOnHF1 (pMp) == true)
      ++nAnt;
    if (CasHfr_bExOnHF1 (pMp) == true)
      ++nAnt;
    nInt = CasHfr_IntegrationTimeHF1 (pMp);
    nStep = CasHfr_nNumberStepsHF1 (pMp);
    if (CasHfr_bDirectionFindingHF1 (pMp) == true)
      nMode = 4;
    else if (nAnt == 1)
      nMode = 0;
    else if (nAnt == 2) {
      if ((CasHfr_bAutoHF1 (pMp) == true) && (CasHfr_bCrossHF1 (pMp) == true))
        nMode = 3;
      else if ((CasHfr_bAutoHF1 (pMp) == true)
               || (CasHfr_bCrossHF1 (pMp) == true))
        nMode = 2;
      else
        nMode = 1;
    } /* two antennas */
    else
      assert (0);

    fCycleHF1 = HF1_InitStepTime[nInt][nMode];
    --nStep;

    if (nType & CasHfr_BandHF1)
      return fCycleHF1 * 1.0E-3;

    if (nStep > 0)
      fCycleHF1 += nStep * HF1_PrecStepTime[nInt][nMode];
    fCycleHF1 *= CasHfr_nRepeatCountHF1 (pMp);
  } else
    fCycleHF1 = 0.0;


  if (CasHfr_bBandHF2Selected (pMp) == true) {
    nAnt = 0;
    if (CasHfr_bEwOnHF2 (pMp) == true)
      ++nAnt;
    if (CasHfr_bEuOnHF2 (pMp) == true)
      ++nAnt;
    if (CasHfr_bEvOnHF2 (pMp) == true)
      ++nAnt;
    if (CasHfr_bExOnHF2 (pMp) == true)
      ++nAnt;
    nInt = CasHfr_IntegrationTimeHF2 (pMp);
    nStep = CasHfr_nNumberStepsHF2 (pMp);
    if (CasHfr_bDirectionFindingHF2 (pMp) == true)
      nMode = 4;
    else if (nAnt == 1)
      nMode = 0;
    else if (nAnt == 2) {
      if ((CasHfr_bAutoHF2 (pMp) == true) && (CasHfr_bCrossHF2 (pMp) == true))
        nMode = 3;
      else if ((CasHfr_bAutoHF2 (pMp) == true)
               || (CasHfr_bCrossHF2 (pMp) == true))
        nMode = 2;
      else
        nMode = 1;
    } /* two antennas */
    else
      assert (0);

    fCycleHF2 = HF2_InitStepTime[nInt][nMode];
    --nStep;

    if (nType & CasHfr_BandHF2)
      return fCycleHF2 * 1.0E-3;

    if (nStep > 0)
      fCycleHF2 += nStep * HF2_PrecStepTime[nInt][nMode];
    fCycleHF2 *= CasHfr_nRepeatCountHF2 (pMp);
  } else
    fCycleHF2 = 0.0;

  /*
   * fprintf(stderr,"Hfr Cycle Times: ABC=%.4f HF1=%.4f HF2=%.4f\n",
   * fCycleABC,fCycleHF1,fCycleHF2);
   */

  fDuration = fCycleABC + fCycleHF1 + fCycleHF2;
  fDuration *= CasHfr_nRepeatCountAll (pMp);
  fDuration /= 1.0E3;                   /* mSec to Sec */


  return fDuration;
}                                       /* fSample width */




/* save static string for error messages */
char *CasHfrAnalysis_sValidPacket (CasRecord * pRec)
{
  unsigned char *pMp = pRec->data;
  Ulong nHfrLen, nMpLen, nNumSamples;
  static char sErr[256], *pErr;

  /*
   * Header Check 
   */
  pErr = sErr;
  pErr += sprintf (pErr, "Hfr Anl (cdstime=%08X) - ", pRec->status.cds_time);
  if (CasHfr_bAnalysis (pMp) == false) {
    sprintf (pErr, "Not Analysis Mode, %02X", pMp[7]);
    return sErr;
  }

  if (CasHfr_bMemoryOverrun (pMp) == true) {
    sprintf (pErr, "Memory Overrun, 0x%02X", pMp[7]);
    return sErr;
  }
  if (CasHfr_nHeaderVersion (pMp) != 0x05) {
    sprintf (pErr, "Invalid Header Version, 0x%02X", pMp[8]);
    return sErr;
  }
  if (CasHfr_bHeaderTerminated (pMp) == false) {
    sprintf (pErr, "Invalid Header Termination, 0x%02X", pMp[24]);
    return sErr;
  }
  if (CasHfr_nRepeatCountAll (pMp) < 1) {
    sprintf (pErr, "Invalid Repeat Count, 0x%02X", pMp[13]);
    return sErr;
  }

  nHfrLen = CasHfr_nPacketSize (pMp);
  nMpLen = pRec->status.packet_length;
  /*
   * only count autocorrelation values 
   */
  nNumSamples =
    CasHfrAnalysis_AgcByteCount (pRec, CasHfr_BandABC | CasAntAll);
  nNumSamples +=
    CasHfrAnalysis_AgcByteCount (pRec, CasHfr_BandHF1 | CasAntAll);
  nNumSamples +=
    CasHfrAnalysis_AgcByteCount (pRec, CasHfr_BandHF2 | CasAntAll);
  nNumSamples +=
    CasHfrAnalysis_AutoByteCount (pRec, CasHfr_BandABC | CasAntAll);
  nNumSamples +=
    CasHfrAnalysis_AutoByteCount (pRec, CasHfr_BandHF1 | CasAntAll);
  nNumSamples +=
    CasHfrAnalysis_AutoByteCount (pRec, CasHfr_BandHF2 | CasAntAll);

  if (nNumSamples + 6 > nHfrLen) {
    sprintf (pErr, "Length Check Failed HfrLen=%d, Auto+Agc+6=%d",
             nHfrLen, nNumSamples + 6);
    return sErr;
  }
  if ((nMpLen + 3) != (nHfrLen + 7)) {  /* length integrity, total bytes in packet */
    sprintf (pErr, "Length Check Failed MpLen+3=%d, HfrLen+7=%d",
             nMpLen + 3, nHfrLen + 7);
    return sErr;
  }
  if (pMp[nHfrLen + 6] != 0x5A) {
    sprintf (pErr, "Invalid Data Termination, 0x%02X", pMp[nHfrLen + 6]);
    return sErr;
  }

  return NULL;
}



/* save static string for error messages */
char *CasHfrSounder_sValidPacket (CasRecord * pRec)
{
  unsigned char *pMp = pRec->data;
  Ulong nHfrLen, nMpLen, nNumSamples;
  static char sErr[256], *pErr;

  /*
   * Header Check 
   */
  pErr = sErr;
  pErr += sprintf (pErr, "Hfr Snd (cdstime=%08X) - ", pRec->status.cds_time);
  if (CasHfr_bSounder (pMp) == false) {
    sprintf (pErr, "Not Sounder Mode, %02X", pMp[7]);
    return sErr;
  }

  if (CasHfr_bMemoryOverrun (pMp) == true) {
    sprintf (pErr, "Memory Overrun, 0x%02X", pMp[7]);
    return sErr;
  }
  if (CasHfr_nHeaderVersion (pMp) != 0x05) {
    sprintf (pErr, "Invalid Header Version, 0x%02X", pMp[8]);
    return sErr;
  }
  if (CasHfrSnd_bHeaderTerminated (pMp) == false) {
    sprintf (pErr, "Invalid Header Termination, 0x%02X", pMp[20]);
    return sErr;
  }

  nHfrLen = CasHfr_nPacketSize (pMp);
  nMpLen = pRec->status.packet_length;

  /*
   * only count autocorrelation values 
   */
  nNumSamples = CasHfrSnd_nPassive (pMp);
  nNumSamples += CasHfrSnd_nActive (pMp);
  nNumSamples *= CasHfrSnd_nCycles (pMp);
  nNumSamples *=
    CasHfrSnd_nStopFrequency (pMp) - CasHfrSnd_nStartFrequency (pMp) + 1;
  nNumSamples *= 2;                     /* for some reason */

  if (nNumSamples + 6 > nHfrLen) {
    sprintf (pErr, "Length Check Failed HfrLen=%d, Samples=%d",
             nHfrLen, nNumSamples + 6);
    return sErr;
  }
  if ((nMpLen + 3) != (nHfrLen + 7)) {  /* length integrity, total bytes in packet */
    sprintf (pErr, "Length Check Failed MpLen+3=%d, HfrLen+7=%d",
             nMpLen + 3, nHfrLen + 7);
    return sErr;
  }
  if (pMp[nHfrLen + 6] != 0x5A) {
    sprintf (pErr, "Invalid Data Termination, 0x%02X", pMp[nHfrLen + 6]);
    return sErr;
  }

  return NULL;
}
